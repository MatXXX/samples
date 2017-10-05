#ifndef __ELF_SYMBOLS_H__
#define __ELF_SYMBOLS_H__

#include <elf.h>
#include <stddef.h>

struct elf_symbols {
    const Elf64_Sym* symbols;
    const char* strings;
};

static inline const char*
elf_symbols_get_name(const struct elf_symbols* symbols,
                     unsigned int id) {
    if (!symbols->symbols || !symbols->strings)
        return NULL;
    return symbols->strings + symbols->symbols[id].st_name;
}

static inline const Elf64_Sym*
elf_symbols_get_symbol(const struct elf_symbols* symbols,
                       unsigned int id) {
    if (!symbols->symbols || !symbols->strings)
        return NULL;
    return &symbols->symbols[id];
}

#endif //__ELF_SYMBOLS_H__