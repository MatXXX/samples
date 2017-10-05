#ifndef __MONTER_FOPS_H__
#define __MONTER_FOPS_H__

#include <linux/fs.h>

#include "monter_command.h"
#include "monter_defines.h"
#include "monter_ioctl.h"

struct monter_context {
    struct monter_device *monter;
    /*memory*/
    void *dma_virt_addr[16];
    dma_addr_t dma_addr[16];
    uint8_t allocated_pages;
    /*per device list of contexts*/
    struct list_head ctx_list;
    /*comands queue*/
    spinlock_t cmd_list_lock;
    struct list_head cmd_list;
    struct completion fsync_completion;
    uint32_t last_issued_addr;
    /*was ioctl(MONTER_IOCTL_SET_SIZE) called*/
    uint8_t configured;
    uint16_t last_addr_a;
    uint16_t last_addr_b;
    uint8_t was_addr_sent;
};

void monter_context_cleanup(struct monter_context *ctx);

int monter_open(struct inode *inode, struct file *dev_file);

int monter_release(struct inode *inode, struct file *dev_file);

ssize_t monter_write(struct file *dev_file, const char __user *buf,
                     size_t count, loff_t *pos);

long monter_ioctl(struct file *dev_file, unsigned int cmd, unsigned long arg);

int monter_mmap(struct file *dev_file, struct vm_area_struct *area_struct);

int monter_fsync(struct file *dev_file, loff_t start, loff_t end, int datasync);

#endif //__MONTER_FOPS_H__