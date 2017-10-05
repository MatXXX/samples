#include <linux/module.h>
MODULE_LICENSE("GPL");

#include <asm/page.h>
#include <asm/uaccess.h>
#include <linux/major.h>
#include <linux/mm.h>
#include <linux/pci.h>
#include <linux/slab.h>

#include "monter_fops.h"

extern struct monter_device *devices[MAX_DEVICES];
uint8_t monter_start_work(struct monter_context *ctx);
void monter_schedule_work(struct list_head *prev_ctx,
                          struct monter_device *monter);
extern struct kmem_cache *monter_cmd_queue_entry_cache;

void monter_context_cleanup(struct monter_context *ctx)
{
    struct list_head *entry;
    struct list_head *temp;
    int i;
    list_for_each_safe(entry, temp, &ctx->cmd_list)
    {
        struct monter_cmd_queue_entry *cmd =
            list_entry(entry, struct monter_cmd_queue_entry, cmd_list);
        if (cmd->type == MONTER_TYPE_COMMANDS) {
            kfree(cmd->cmd_buffer.commands);
        }
        list_del(entry);
        kmem_cache_free(monter_cmd_queue_entry_cache, cmd);
    }

    for (i = 0; i < ctx->allocated_pages; i++) {
        if (ctx->dma_virt_addr[i] != NULL)
            pci_free_consistent(ctx->monter->pci_dev, HOST_PAGE_SIZE,
                                ctx->dma_virt_addr[i], ctx->dma_addr[i]);
    }

    if (ctx->allocated_pages < 16) {
        if (ctx->dma_virt_addr[ctx->allocated_pages] != NULL)
            pci_free_consistent(ctx->monter->pci_dev, HOST_PAGE_SIZE,
                                ctx->dma_virt_addr[ctx->allocated_pages],
                                ctx->dma_addr[ctx->allocated_pages]);
    }
    kfree(ctx);
}

int monter_open(struct inode *inode, struct file *dev_file)
{
    unsigned long flags;
    int minor = MINOR(dev_file->f_inode->i_rdev);
    struct monter_context *ctx =
        kzalloc(sizeof(struct monter_context), GFP_KERNEL);

    if (!ctx) {
        MONTER_ERROR("Cannot allocate ctx");
        return -ENOMEM;
    }

    ctx->monter = devices[minor];
    /* innocent command */
    ctx->last_issued_addr = MONTER_CMD_KIND_ADDR_AB;
    spin_lock_init(&ctx->cmd_list_lock);
    INIT_LIST_HEAD(&ctx->cmd_list);
    INIT_LIST_HEAD(&ctx->ctx_list);
    dev_file->private_data = ctx;

    write_lock_irqsave(&ctx->monter->ctx_list_lock, flags);
    list_add_tail(&ctx->ctx_list, &ctx->monter->ctx_list);
    write_unlock_irqrestore(&ctx->monter->ctx_list_lock, flags);

    init_completion(&ctx->fsync_completion);

    return 0;
}

int monter_release(struct inode *inode, struct file *dev_file)
{
    struct monter_context *ctx = dev_file->private_data;
    struct monter_device *monter = ctx->monter;

    /* check if we are releasing context that is beeing processed by monter
     * right now */
    read_lock_irq(&monter->dev_lock);
    if (monter->working_ctx != ctx) {
        /* ctx is not the one that monter is working on
         * this case is simple, just remove it from the ctx list*/
        write_lock(&monter->ctx_list_lock);
        list_del(&ctx->ctx_list);
        write_unlock(&monter->ctx_list_lock);
        read_unlock_irq(&monter->dev_lock);
    } else {
        read_unlock_irq(&monter->dev_lock);
        write_lock_irq(&monter->dev_lock);
        if (monter->working_ctx != ctx) { /* check once again */
            write_lock(&monter->ctx_list_lock);
            list_del(&ctx->ctx_list);
            write_unlock(&monter->ctx_list_lock);
        } else {
            /* Now we know that device is working on this ctx.
             * We need to stop ongoing work in order to free dma memory
             * and start new work if any */
            struct list_head *prev_ctx;
            MONTER_SET_REGISTER(monter, ENABLE, 0);
            MONTER_SET_REGISTER(monter, INTR, MONTER_INTR_NOTIFY);
            MONTER_SET_REGISTER(monter, RESET,
                                MONTER_RESET_CALC | MONTER_RESET_FIFO);

            write_lock(&monter->ctx_list_lock);
            prev_ctx = ctx->ctx_list.prev;
            list_del(&ctx->ctx_list);
            write_unlock(&monter->ctx_list_lock);

            monter_schedule_work(prev_ctx, monter);
        }
        write_unlock_irq(&monter->dev_lock);
    }

    monter_context_cleanup(ctx);

    dev_file->private_data = NULL;
    return 0;
}

/* Assumes that cmd is correct monter cmd*/
uint32_t user_cmd_to_device(uint32_t cmd)
{
    if (MONTER_IS_USER_CMD_ADDR(cmd))
        return (cmd & 0xfffffff0) | MONTER_CMD_KIND_ADDR_AB;
    if (MONTER_IS_USER_CMD_MULT(cmd))
        return (cmd & 0xfffffff0) | MONTER_CMD_KIND_RUN |
               MONTER_CMD_SUBTYPE_RUN_MULT;
    return (cmd & 0xfffffff0) | MONTER_CMD_KIND_RUN |
           MONTER_CMD_SUBTYPE_RUN_REDC;
}

uint8_t check_command_correctness(uint32_t cmd, uint16_t *last_addr_a,
                                  uint16_t *last_addr_b,
                                  const uint32_t allocated_bytes)
{
    if (MONTER_IS_USER_CMD_ADDR(cmd)) {
        if (MONTER_CMD_ADDR_A(cmd) >= allocated_bytes ||
            MONTER_CMD_ADDR_B(cmd) >= allocated_bytes)
            return 0;
        *last_addr_a = MONTER_CMD_ADDR_A(cmd);
        *last_addr_b = MONTER_CMD_ADDR_B(cmd);
        return 1;
    } else if (MONTER_IS_USER_CMD_MULT(cmd)) {
        uint32_t a_end = *last_addr_a + MONTER_CMD_RUN_SIZE(cmd) * 4;
        uint32_t b_end = *last_addr_b + MONTER_CMD_RUN_SIZE(cmd) * 4;
        uint32_t d_start = MONTER_CMD_ADDR_D(cmd);
        uint32_t d_end = d_start + MONTER_CMD_RUN_SIZE(cmd) * 8;
        return d_start < allocated_bytes && a_end <= allocated_bytes &&
               b_end <= allocated_bytes && d_end <= allocated_bytes;
    } else if (MONTER_IS_USER_CMD_REDC(cmd)) {
        uint32_t a_end = *last_addr_a + 4;
        uint32_t b_end = *last_addr_b + MONTER_CMD_RUN_SIZE(cmd) * 4;
        uint32_t t_start = MONTER_CMD_ADDR_D(cmd);
        uint32_t t_end = MONTER_CMD_ADDR_D(cmd) + MONTER_CMD_RUN_SIZE(cmd) * 8;
        uint32_t d_start =
            MONTER_CMD_ADDR_D(cmd) + MONTER_CMD_RUN_SIZE(cmd) * 4;
        uint32_t d_end = d_start + MONTER_CMD_RUN_SIZE(cmd) * 4;
        return t_start < allocated_bytes && t_end <= allocated_bytes &&
               a_end <= allocated_bytes && b_end <= allocated_bytes &&
               d_start < allocated_bytes && d_end <= allocated_bytes;
    }
    return 0;
}

ssize_t monter_write(struct file *dev_file, const char __user *buf,
                     size_t count, loff_t *pos)
{
    struct monter_context *ctx = dev_file->private_data;
    int i = 0, first_cmd_addr = 0;
    uint16_t last_addr_a, last_addr_b;
    uint32_t *cmd_buffer;
    unsigned long flags;
    struct monter_cmd_queue_entry *cmd;

    if (!ctx->configured) {
        MONTER_DEBUG_PRINT("not configured");
        return -EINVAL;
    }

    if (count == 0)
        return 0;

    if (count % 4 != 0) {
        MONTER_DEBUG_PRINT("COUNT %% 4 != 0");
        return -EINVAL;
    }

    last_addr_a = ctx->last_addr_a;
    last_addr_b = ctx->last_addr_b;

    cmd_buffer = (uint32_t *)kmalloc(count, GFP_KERNEL);
    if (!cmd_buffer) {
        MONTER_ERROR("Cannot alloc buffer.");
        return -ENOMEM;
    }

    if (copy_from_user((void *)cmd_buffer, buf, count)) {
        MONTER_ERROR("Cannot copy data from user.");
        kfree(cmd_buffer);
        return -ENOMEM;
    }

    /* Correctness check */
    for (i = 0; i < count / 4; i++) {
        if (i == 0 && !ctx->was_addr_sent) {
            if (MONTER_IS_USER_CMD_ADDR(cmd_buffer[i]))
                first_cmd_addr = 1;
            else {
                MONTER_DEBUG_PRINT("FIRST CMD IS NOT ADDR");
                kfree(cmd_buffer);
                return -EINVAL;
            }
        }

        if (!check_command_correctness(
                cmd_buffer[i], &last_addr_a, &last_addr_b,
                ctx->allocated_pages * MONTER_PAGE_SIZE)) {
            kfree(cmd_buffer);
            return -EINVAL;
        }
        cmd_buffer[i] = user_cmd_to_device(cmd_buffer[i]);
    }

    if (!ctx->was_addr_sent && first_cmd_addr)
        ctx->was_addr_sent = 1;

    cmd = kmem_cache_alloc(monter_cmd_queue_entry_cache, GFP_KERNEL);
    cmd->type = MONTER_TYPE_COMMANDS;
    cmd->cmd_buffer.commands = cmd_buffer;
    cmd->cmd_buffer.length = count / 4;
    cmd->cmd_buffer.read = 0;

    spin_lock_irqsave(&ctx->cmd_list_lock, flags);
    list_add_tail(&cmd->cmd_list, &ctx->cmd_list);
    spin_unlock_irqrestore(&ctx->cmd_list_lock, flags);

    ctx->last_addr_a = last_addr_a;
    ctx->last_addr_b = last_addr_b;

    read_lock_irqsave(&ctx->monter->dev_lock, flags);

    if (!ctx->monter->computing) {
        /* Monter is idle right now, start it */
        read_unlock_irqrestore(&ctx->monter->dev_lock, flags);
        write_lock_irqsave(&ctx->monter->dev_lock, flags);
        /* Another check, computation might have started while we weren't
         * holding the lock */
        if (!ctx->monter->computing) {
            ctx->monter->computing = 1;
            spin_lock(&ctx->cmd_list_lock);
            monter_start_work(ctx);
            spin_unlock(&ctx->cmd_list_lock);
        }
        write_unlock_irqrestore(&ctx->monter->dev_lock, flags);
    } else {
        read_unlock_irqrestore(&ctx->monter->dev_lock, flags);
    }

    return count;
}

long monter_ioctl(struct file *dev_file, unsigned int cmd, unsigned long arg)
{
    struct monter_context *ctx = dev_file->private_data;
    unsigned long i, monter_pages;

    if (cmd != MONTER_IOCTL_SET_SIZE)
        return -ENOTTY;

    if (ctx->configured) {
        MONTER_DEBUG_PRINT("already configured");
        return -EINVAL;
    }

    if (arg % HOST_PAGE_SIZE != 0 || arg > MONTER_PAGE_NUM * HOST_PAGE_SIZE) {
        MONTER_DEBUG_PRINT("Incorrect size, %lu", arg);
        return -EINVAL;
    }
    /* Allocating one host page per one monter page */
    monter_pages = arg / HOST_PAGE_SIZE;
    for (i = 0; i < monter_pages; i++) {
        ctx->dma_virt_addr[i] = pci_alloc_consistent(
            ctx->monter->pci_dev, HOST_PAGE_SIZE, &ctx->dma_addr[i]);
    }
    ctx->allocated_pages = monter_pages;

    if (monter_pages < MONTER_PAGE_NUM) {
        void *unused_page_virt;
        dma_addr_t unused_page_dma;
        unused_page_virt = pci_alloc_consistent(
            ctx->monter->pci_dev, HOST_PAGE_SIZE, &unused_page_dma);
        for (i = monter_pages; i < MONTER_PAGE_NUM; i++) {
            ctx->dma_virt_addr[i] = unused_page_virt;
            ctx->dma_addr[i] = unused_page_dma;
        }
    }

    ctx->configured = 1;
    return 0;
}

static int monter_vm_ops_fault(struct vm_area_struct *vma, struct vm_fault *vmf)
{
    struct monter_context *ctx = vma->vm_private_data;
    void *addr;
    int pg_num;
    if (!ctx->configured || vmf->pgoff >= ctx->allocated_pages ||
        vma->vm_pgoff >= ctx->allocated_pages - vmf->pgoff)
        return VM_FAULT_SIGBUS;

    pg_num = vma->vm_pgoff + vmf->pgoff;
    addr = ctx->dma_virt_addr[pg_num];
    if (vm_insert_page(vma,
                       (unsigned long)vma->vm_start + pg_num * HOST_PAGE_SIZE,
                       virt_to_page(addr)))
        return VM_FAULT_SIGBUS;
    return VM_FAULT_NOPAGE;
}

static struct vm_operations_struct monter_vm_area_struct_ops = {
    .fault = monter_vm_ops_fault,
};

int monter_mmap(struct file *dev_file, struct vm_area_struct *area_struct)
{
    struct monter_context *ctx = dev_file->private_data;
    unsigned long requested_pages;

    if (!ctx->configured) {
        MONTER_DEBUG_PRINT("not configured\n");
        return -EINVAL;
    }

    if (!(area_struct->vm_flags & VM_SHARED)) {
        MONTER_DEBUG_PRINT("not shared\n");
        return -EINVAL;
    }

    requested_pages = vma_pages(area_struct);
    if (requested_pages > ctx->allocated_pages) {
        MONTER_DEBUG_PRINT("too much pages requested\n");
        return -EINVAL;
    }

    /* Offset + requested_pages > allocated_pages */
    if (area_struct->vm_pgoff > ctx->allocated_pages - requested_pages) {
        MONTER_DEBUG_PRINT("offset is too big\n");
        return -EINVAL;
    }

    area_struct->vm_private_data = ctx;
    area_struct->vm_ops = &monter_vm_area_struct_ops;

    area_struct->vm_page_prot = pgprot_noncached(area_struct->vm_page_prot);
    area_struct->vm_flags |= VM_MIXEDMAP | VM_DONTEXPAND;

    return 0;
}

int monter_fsync(struct file *dev_file, loff_t start, loff_t end, int datasync)
{
    struct monter_context *ctx = dev_file->private_data;
    unsigned long flags;
    struct monter_cmd_queue_entry *cmd;
    cmd = kmem_cache_alloc(monter_cmd_queue_entry_cache, GFP_KERNEL);
    cmd->type = MONTER_TYPE_FSYNC;
    cmd->fsync_completion = &ctx->fsync_completion;
    reinit_completion(&ctx->fsync_completion);

    spin_lock_irqsave(&ctx->cmd_list_lock, flags);
    if (list_empty(&ctx->cmd_list)) {
        spin_unlock_irqrestore(&ctx->cmd_list_lock, flags);
        return 0;
    }
    list_add_tail(&cmd->cmd_list, &ctx->cmd_list);
    spin_unlock_irqrestore(&ctx->cmd_list_lock, flags);

    if (wait_for_completion_interruptible(&ctx->fsync_completion)) {
        return -ERESTARTSYS;
    }
    return 0;
}
