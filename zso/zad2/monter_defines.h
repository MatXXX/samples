#ifndef __MONTER_DEFINES_H__
#define __MONTER_DEFINES_H__

#include <linux/cdev.h>
#include <linux/completion.h>
#include <linux/mutex.h>

#include "monter.h"
#include "monter_ioctl.h"

/* This struct describes Monter(TM)s BAR0 layout.
   Addresses obtained by pci_iomap() can be casted
   to `struct monter_bar * __iomem` for clarity,
   but do never use fields in it directly. */
struct monter_bar {
    uint32_t ENABLE;        // RW
    uint32_t STATUS;        // R
    uint32_t INTR;          // R
    uint32_t INTR_ENABLE;   // RW
    uint32_t RESET;         // W
    uint32_t COUNTER;       // R
    uint32_t FIFO_SEND;     // W
    uint32_t FIFO_FREE;     // R
    uint32_t CMD_READ_PTR;  // RW
    uint32_t CMD_WRITE_PTR; // RW
};

#define MONTER_IS_USER_CMD_ADDR(cmd) (((cmd)&0xf) == 0)

#define MONTER_IS_USER_CMD_MULT(cmd) (((cmd)&0xf) == 1 && ((cmd)&0x20000) == 0)

#define MONTER_IS_USER_CMD_REDC(cmd) (((cmd)&0xf) == 2 && ((cmd)&0x20000) == 0)

#define MONTER_DONT_NOTIFY 0
#define MONTER_NOTIFY 1

#define HOST_PAGE_SIZE PAGE_SIZE

#define MAX_DEVICES 256

struct monter_context;

#define MONTER_CMD_BUFFER_SIZE HOST_PAGE_SIZE
#define MONTER_CMD_SIZE 4
#define MONTER_CMD_BUFFER_CAPACITY (MONTER_CMD_BUFFER_SIZE / MONTER_CMD_SIZE)
struct monter_device {
    struct cdev cdev;
    struct pci_dev *pci_dev;
    struct device *dev;
    struct monter_bar __iomem *bar_addr;
    dev_t devt;
    struct list_head dev_list;
    struct mutex mutex;
    struct completion event;
    /*per device list of contexts*/
    rwlock_t ctx_list_lock;
    struct list_head ctx_list;
    rwlock_t dev_lock;
    uint8_t computing;
    struct monter_context *working_ctx;

    /*CMD buffer*/
    uint32_t *cmd_buffer_virt;
    dma_addr_t cmd_buffer_dma;
};

#define MONTER_SET_REGISTER(monter, reg_name, val)                             \
    iowrite32((val), (&(monter)->bar_addr->reg_name))

#define MONTER_GET_REGISTER(monter, reg_name)                                  \
    (ioread32(&(monter)->bar_addr->reg_name))

#ifndef DEBUG

#define MONTER_DEBUG_PRINT(fmt, ...)

#else

#define MONTER_DEBUG_PRINT(fmt, ...)                                           \
    printk(KERN_INFO "%s: " fmt "\n", __func__, ##__VA_ARGS__)

#endif

#define MONTER_ERROR(fmt, ...)                                                 \
    printk(KERN_ALERT "%s ERROR: " fmt "\n", __func__, ##__VA_ARGS__)

#endif //__MONTER_DEFINES_H__