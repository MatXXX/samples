#ifndef __DYNAMIC_SYMBOLS_DESCRIPTION_H__
#define __DYNAMIC_SYMBOLS_DESCRIPTION_H__

#include "elf_sysv_hashtable.h"
#include "elf_gnu_hashtable.h"

struct tab_info {
    Elf64_Addr addr;
    Elf64_Word size;
    Elf64_Word elem_size;
};

struct dynamic_symbols_description {
    struct elf_sysv_hashtable hash_tab;
    struct elf_gnu_hashtable  gnu_hash_tab;
    struct tab_info           plt_rel;
    Elf64_Word                plt_rel_type;
    struct elf_symbols        symbols;
};

void
dynamic_symbols_description_populate(struct dynamic_symbols_description* out,
                                     const Elf64_Dyn* dynamic);

const Elf64_Sym*
dynamic_symbols_description_symbol_lookup(struct dynamic_symbols_description* dsd,
                                          const char* name);

Elf64_Rela*
dynamic_symbols_description_find_got_reloc(struct dynamic_symbols_description* dsd,
                                           const char* name);

#endif //__DYNAMIC_SYMBOLS_DESCRIPTION_H__