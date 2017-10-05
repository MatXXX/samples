#include <linux/module.h>
#include <linux/pci.h>

#include <asm/atomic.h>
#include <asm/spinlock.h>
#include <asm/uaccess.h>
#include <linux/cdev.h>
#include <linux/err.h>
#include <linux/errno.h>
#include <linux/fs.h>
#include <linux/interrupt.h>
#include <linux/kernel.h>
#include <linux/list.h>
#include <linux/major.h>
#include <linux/slab.h>

MODULE_LICENSE("GPL");

#include "monter_command.h"
#include "monter_defines.h"
#include "monter_fops.h"

struct monter_device *devices[MAX_DEVICES] = {0};
struct kmem_cache *monter_cmd_queue_entry_cache = 0;

static LIST_HEAD(monter_device_list);
static DEFINE_RWLOCK(monter_device_list_lock);

static struct class *monter_class;

static dev_t dev_number = 0;
static struct file_operations fops = {
    .owner = THIS_MODULE,
    .open = monter_open,
    .unlocked_ioctl = monter_ioctl,
    .mmap = monter_mmap,
    .write = monter_write,
    .fsync = monter_fsync,
    .release = monter_release,
};
static atomic_t count_devices = ATOMIC_INIT(0);

irqreturn_t monter_notify_handler(int irq, void *dev);
static int monter_probe(struct pci_dev *dev, const struct pci_device_id *id)
{
    unsigned long err;
    unsigned long flags;
    int device_num = atomic_fetch_add(1, &count_devices);
    struct monter_device *monter =
        kzalloc(sizeof(struct monter_device), GFP_KERNEL);

    if (!monter) {
        MONTER_ERROR("Couldn't allocate monter_device");
        return -ENOMEM;
    }

    devices[device_num] = monter;
    pci_set_drvdata(dev, monter);
    monter->pci_dev = dev;

    /*Add char device*/
    cdev_init(&monter->cdev, &fops);
    monter->cdev.owner = THIS_MODULE;
    monter->devt = dev_number + device_num;
    if ((err = cdev_add(&monter->cdev, monter->devt, 1))) {
        MONTER_ERROR("cdev_add error");
        goto cleanup_cdev;
    }

    /*Create /dev/ entry*/
    monter->dev = device_create(monter_class, &dev->dev, monter->devt, NULL,
                                "monter%d", MINOR(monter->devt));

    if (IS_ERR(monter->dev)) {
        err = PTR_ERR(monter->dev);
        MONTER_ERROR("Couldn't create device");
        goto cleanup_device_create;
    }

    /*Init rest of fields*/
    INIT_LIST_HEAD(&monter->dev_list);
    mutex_init(&monter->mutex);
    init_completion(&monter->event);

    /*Configure PCI bus*/
    err = pci_enable_device(dev);
    if (IS_ERR_VALUE(err)) {
        MONTER_ERROR("pci_enable_device");
        goto cleanup_pci_enable_device;
    }

    err = pci_request_regions(dev, "monter");
    if (IS_ERR_VALUE(err)) {
        MONTER_ERROR("pci_request_regions");
        goto clanup_pci_request_regions;
    }

    monter->bar_addr = pci_iomap(dev, 0, 0);

    if (IS_ERR(monter->bar_addr)) {
        err = PTR_ERR(monter->bar_addr);
        MONTER_ERROR("pci_iomap");
        goto cleanup_pci_iomap;
    }

    /*Reset registers*/
    MONTER_SET_REGISTER(monter, RESET, MONTER_RESET_CALC | MONTER_RESET_FIFO);
    MONTER_SET_REGISTER(monter, ENABLE, 0);
    MONTER_SET_REGISTER(monter, INTR,
                        MONTER_INTR_FIFO_OVERFLOW | MONTER_INTR_INVALID_CMD |
                            MONTER_INTR_NOTIFY);
    MONTER_SET_REGISTER(monter, CMD_READ_PTR, 0);
    MONTER_SET_REGISTER(monter, CMD_WRITE_PTR, 0);
    MONTER_SET_REGISTER(monter, INTR_ENABLE,
                        MONTER_INTR_FIFO_OVERFLOW | MONTER_INTR_INVALID_CMD |
                            MONTER_INTR_NOTIFY);

    /*Add device to list*/
    write_lock_irqsave(&monter_device_list_lock, flags);

    list_add(&monter->dev_list, &monter_device_list);

    write_unlock_irqrestore(&monter_device_list_lock, flags);

    /*Register irqhandler, must be done after adding device to list and
   * initializing locks*/
    err = request_irq(monter->pci_dev->irq, monter_notify_handler, IRQF_SHARED,
                      "monter", monter);
    if (IS_ERR_VALUE(err)) {
        MONTER_ERROR("request_irq");
        goto cleanup_request_irq;
    }

    /*Enable DMA*/
    pci_set_master(dev);
    err = pci_set_dma_mask(dev, DMA_BIT_MASK(32));
    if (IS_ERR_VALUE(err)) {
        MONTER_ERROR("pci_set_dma_mask");
        goto pci_set_dma_mask;
    }

    err = pci_set_consistent_dma_mask(dev, DMA_BIT_MASK(32));
    if (IS_ERR_VALUE(err)) {
        MONTER_ERROR("pci_set_consistent_dma_mask");
        goto pci_set_dma_mask;
    }

    monter->cmd_buffer_virt = pci_alloc_consistent(
        monter->pci_dev, MONTER_CMD_BUFFER_SIZE, &monter->cmd_buffer_dma);

    if (IS_ERR_OR_NULL(monter->cmd_buffer_virt)) {
        err = PTR_ERR(monter->cmd_buffer_virt);
        goto pci_set_dma_mask;
    }

    monter->cmd_buffer_virt[MONTER_CMD_BUFFER_SIZE / MONTER_CMD_SIZE - 1] =
        MONTER_CMD_JUMP(monter->cmd_buffer_dma);

    MONTER_SET_REGISTER(monter, CMD_READ_PTR, monter->cmd_buffer_dma);
    MONTER_SET_REGISTER(monter, CMD_WRITE_PTR, monter->cmd_buffer_dma);

    rwlock_init(&monter->ctx_list_lock);
    rwlock_init(&monter->dev_lock);
    INIT_LIST_HEAD(&monter->ctx_list);
    monter->computing = 0;
    monter->working_ctx = NULL;

    return 0;

pci_set_dma_mask:
    free_irq(monter->pci_dev->irq, monter);
    pci_clear_master(dev);

cleanup_request_irq:
    write_lock_irqsave(&monter_device_list_lock, flags);
    list_del(&monter->dev_list);
    write_unlock_irqrestore(&monter_device_list_lock, flags);
    pci_iounmap(dev, monter->bar_addr);

cleanup_pci_iomap:
    pci_release_regions(dev);

clanup_pci_request_regions:
    pci_disable_device(dev);

cleanup_pci_enable_device:
    device_destroy(monter_class, monter->devt);

cleanup_device_create:
    cdev_del(&monter->cdev);

cleanup_cdev:
    devices[monter->devt - dev_number] = NULL;
    kfree(monter);
    return err;
}

static void monter_remove(struct pci_dev *dev)
{
    unsigned long flags;
    struct monter_device *monter = pci_get_drvdata(dev);
    struct list_head *entry, *temp;

    write_lock_irq(&monter->dev_lock);
    MONTER_SET_REGISTER(monter, ENABLE, 0);
    MONTER_SET_REGISTER(monter, INTR_ENABLE, 0);
    MONTER_SET_REGISTER(monter, INTR,
                        MONTER_INTR_NOTIFY | MONTER_INTR_INVALID_CMD |
                            MONTER_INTR_FIFO_OVERFLOW);
    MONTER_SET_REGISTER(monter, RESET, MONTER_RESET_CALC | MONTER_RESET_FIFO);

    write_lock(&monter->ctx_list_lock);
    list_for_each_safe(entry, temp, &monter->ctx_list)
    {
        struct monter_context *ctx =
            list_entry(entry, struct monter_context, ctx_list);
        list_del(entry);
        monter_context_cleanup(ctx);
    }
    write_unlock(&monter->ctx_list_lock);
    write_unlock_irq(&monter->dev_lock);

    free_irq(monter->pci_dev->irq, monter);
    pci_clear_master(dev);

    write_lock_irqsave(&monter_device_list_lock, flags);
    list_del(&monter->dev_list);
    write_unlock_irqrestore(&monter_device_list_lock, flags);
    pci_iounmap(dev, monter->bar_addr);

    pci_release_regions(dev);

    pci_disable_device(dev);

    device_destroy(monter_class, monter->devt);

    cdev_del(&monter->cdev);

    devices[monter->devt - dev_number] = NULL;
    kfree(monter);
}

static struct pci_device_id monter_device_id[] = {
    {PCI_DEVICE(MONTER_VENDOR_ID, MONTER_DEVICE_ID)}, {0}};

static struct pci_driver monter_driver = {
    name : "monter",
    id_table : monter_device_id,
    probe : monter_probe,
    remove : monter_remove,
};

static void monter_driver_cleanup(void)
{
    kmem_cache_destroy(monter_cmd_queue_entry_cache);

    pci_unregister_driver(&monter_driver);

    class_destroy(monter_class);

    unregister_chrdev_region(dev_number, 256);
}

static char *monter_class_devnode(struct device *dev, umode_t *mode)
{
    if (!mode)
        return NULL;
    if (MAJOR(dev->devt) == MAJOR(dev_number))
        *mode = 0666;
    return NULL;
}

static int monter_driver_init(void)
{
    unsigned long err;
    err = alloc_chrdev_region(&dev_number, 0, 256, "monter");
    if (IS_ERR_VALUE(err)) {
        MONTER_ERROR("Cannot allocate chrdev region");
        return err;
    }

    monter_class = class_create(THIS_MODULE, "monter");
    if (IS_ERR_OR_NULL(monter_class)) {
        MONTER_ERROR("Cannot create class");
        err = PTR_ERR(monter_class);
        goto cleanup_class_create;
    }

    monter_class->devnode = monter_class_devnode;
    err = pci_register_driver(&monter_driver);
    if (IS_ERR_VALUE(err))
        goto cleanup_pci_register_driver;

    monter_cmd_queue_entry_cache =
        kmem_cache_create("monter cmd queue entry",
                          sizeof(struct monter_cmd_queue_entry), 0, 0, NULL);

    if (IS_ERR_OR_NULL(monter_cmd_queue_entry_cache)) {
        MONTER_ERROR("Cannot create monter cmd queue entry cache");
        err = PTR_ERR(monter_cmd_queue_entry_cache);
        goto cleanup_kmem_cache_create;
    }

    return 0;

cleanup_kmem_cache_create:
    pci_unregister_driver(&monter_driver);

cleanup_pci_register_driver:
    class_destroy(monter_class);

cleanup_class_create:
    unregister_chrdev_region(dev_number, 256);

    return (int)err;
}

module_init(monter_driver_init);
module_exit(monter_driver_cleanup);

/*
Expected locks
write_lock(&monter->dev_lock);
spin_lock(&ctx->cmd_list_lock);
*/
uint8_t monter_start_work(struct monter_context *ctx)
{
    struct monter_device *monter = ctx->monter;
    uint32_t *monter_buf = monter->cmd_buffer_virt;
    struct list_head *cmd;
    uint32_t i, index = 0, last_issued_addr;

    if (list_empty(&ctx->cmd_list))
        return 0;

    if (monter->working_ctx != ctx) {
        for (i = 0; i < MONTER_PAGE_NUM; i++)
            monter_buf[index++] =
                MONTER_CMD_PAGE(i, ctx->dma_addr[i], MONTER_DONT_NOTIFY);
        monter->working_ctx = ctx;
        monter_buf[index++] = ctx->last_issued_addr;
    }

    last_issued_addr = ctx->last_issued_addr;

    cmd = ctx->cmd_list.next;
    while (index < MONTER_CMD_BUFFER_CAPACITY - 1 && cmd != &ctx->cmd_list) {
        struct monter_cmd_queue_entry *entry =
            list_entry(cmd, struct monter_cmd_queue_entry, cmd_list);

        /* fsync */
        if (entry->type != MONTER_TYPE_COMMANDS)
            break;

        while (entry->cmd_buffer.read < entry->cmd_buffer.length &&
               index < MONTER_CMD_BUFFER_CAPACITY - 1) {
            /* keep track of last issued addr cmd to restore it later in case of
             * ctx switch*/
            if (MONTER_CMD_KIND(
                    entry->cmd_buffer.commands[entry->cmd_buffer.read]) ==
                MONTER_CMD_KIND_ADDR_AB) {
                last_issued_addr =
                    entry->cmd_buffer.commands[entry->cmd_buffer.read];
            }
            monter_buf[index++] =
                entry->cmd_buffer.commands[entry->cmd_buffer.read++];
        }

        /*if this entry is copied to cmd buffer and isn't the last entry
         * in cmd queue then it can be deleted*/
        if (entry->cmd_buffer.read == entry->cmd_buffer.length &&
            cmd->next != &ctx->cmd_list) {
            struct list_head *this = cmd;
            struct list_head *prev = cmd->prev;
            kfree(entry->cmd_buffer.commands);
            list_del(this);
            kmem_cache_free(monter_cmd_queue_entry_cache, entry);
            cmd = prev;
        }
        cmd = cmd->next;
    }
    ctx->last_issued_addr = last_issued_addr;

    monter_buf[index++] = MONTER_CMD_COUNTER(0, MONTER_NOTIFY);
    MONTER_SET_REGISTER(monter, ENABLE, 0);
    MONTER_SET_REGISTER(monter, CMD_READ_PTR, monter->cmd_buffer_dma);
    MONTER_SET_REGISTER(monter, CMD_WRITE_PTR,
                        monter->cmd_buffer_dma + index * MONTER_CMD_SIZE);
    MONTER_SET_REGISTER(monter, ENABLE,
                        MONTER_ENABLE_CALC | MONTER_ENABLE_FETCH_CMD);

    return 1;
}

uint8_t try_monter_start_work(struct monter_context *ctx)
{
    uint8_t started = 0;
    spin_lock(&ctx->cmd_list_lock);
    if (!list_empty(&ctx->cmd_list)) {
        if (list_entry(ctx->cmd_list.next, struct monter_cmd_queue_entry,
                       cmd_list)
                ->type != MONTER_TYPE_COMMANDS)
            MONTER_ERROR("INCORRECT COMMAND TYPE");
        else
            started = monter_start_work(ctx);
    }
    spin_unlock(&ctx->cmd_list_lock);
    return started;
}

void monter_schedule_work(struct list_head *prev_ctx,
                          struct monter_device *monter)
{
    uint8_t found = 0;
    struct list_head *next_ctx_head;

    /* Take next ctx with some work to do */
    read_lock(&monter->ctx_list_lock);

    list_for_each(next_ctx_head, prev_ctx)
    {
        struct monter_context *next_ctx;
        /*Skip list head*/
        if (next_ctx_head == &monter->ctx_list)
            continue;

        next_ctx = list_entry(next_ctx_head, struct monter_context, ctx_list);
        if (try_monter_start_work(next_ctx)) {
            found = 1;
            break;
        }
    }

    /* Nothing to do in other contexts? */
    /* Check if last working ctx has sth to compute */
    if (!found && prev_ctx != &monter->ctx_list)
        found = try_monter_start_work(
            list_entry(prev_ctx, struct monter_context, ctx_list));

    read_unlock(&monter->ctx_list_lock);

    if (!found) {
        /* No pending work, cleanup */
        MONTER_SET_REGISTER(monter, ENABLE, 0);
        monter->computing = 0;
        monter->working_ctx = NULL;
    }
}

irqreturn_t monter_notify_handler(int irq, void *dev)
{
    uint8_t found = 0;
    struct monter_device *monter;
    unsigned long flags;
    uint32_t regs;

    read_lock_irqsave(&monter_device_list_lock, flags);
    list_for_each_entry(monter, &monter_device_list, dev_list)
    {
        if (dev == monter) {
            found = 1;
            break;
        }
    }
    read_unlock_irqrestore(&monter_device_list_lock, flags);

    if (!found)
        return IRQ_NONE;
    regs = MONTER_GET_REGISTER(monter, INTR);
    MONTER_SET_REGISTER(monter, INTR, regs);

    if (regs & MONTER_INTR_NOTIFY) {
        struct monter_context *ctx;
        write_lock_irqsave(&monter->dev_lock, flags);
        ctx = monter->working_ctx;
        /* No context connected */
        /* Last context was destroyed during last work */
        if (!ctx) {
            monter->computing = 0;
            goto notify_end;
        }

        spin_lock(&ctx->cmd_list_lock);

        /* Remove last cmd if its empty */
        if (!list_empty(&ctx->cmd_list)) {
            struct monter_cmd_queue_entry *entry = list_entry(
                ctx->cmd_list.next, struct monter_cmd_queue_entry, cmd_list);
            if (entry->type == MONTER_TYPE_COMMANDS &&
                entry->cmd_buffer.read == entry->cmd_buffer.length) {
                list_del(&entry->cmd_list);
                kmem_cache_free(monter_cmd_queue_entry_cache, entry);
            }
        }

        /* Notify if next command in queue is fsync */
        if (!list_empty(&ctx->cmd_list)) {
            struct monter_cmd_queue_entry *entry = list_entry(
                ctx->cmd_list.next, struct monter_cmd_queue_entry, cmd_list);
            if (entry->type == MONTER_TYPE_FSYNC) {
                list_del(&entry->cmd_list);
                complete_all(entry->fsync_completion);
                kmem_cache_free(monter_cmd_queue_entry_cache, entry);
            }
        }

        spin_unlock(&ctx->cmd_list_lock);

        monter_schedule_work(&ctx->ctx_list, ctx->monter);

    notify_end:
        write_unlock_irqrestore(&monter->dev_lock, flags);
    }
    if (regs & MONTER_INTR_FIFO_OVERFLOW) {
        MONTER_ERROR("INTR OVERFLOW");
    }
    if (regs & MONTER_INTR_INVALID_CMD) {
        MONTER_ERROR("INTR INVALID_CMD");
    }

    return IRQ_HANDLED;
}
