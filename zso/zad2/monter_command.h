#ifndef __MONTER_COMMAND_H__
#define __MONTER_COMMAND_H__

enum monter_cmd_type {
    MONTER_TYPE_COMMANDS,
    MONTER_TYPE_FSYNC
};

struct monter_cmd_buffer {
    uint32_t* commands;
    /*Length in 32-bit commands*/
    uint32_t length;
    uint32_t read;
};

struct monter_cmd_queue_entry {
    enum monter_cmd_type type;
    union {
        struct completion* fsync_completion;
        struct monter_cmd_buffer cmd_buffer;
    };
    struct list_head cmd_list;
};

#endif //__MONTER_COMMAND_H__