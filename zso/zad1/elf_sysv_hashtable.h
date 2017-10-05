#ifndef __ELF_SYSV_HASHTABLE_H__
#define __ELF_SYSV_HASHTABLE_H__

#include <elf.h>
#include "elf_symbols.h"

struct elf_sysv_hashtable {
    Elf64_Word  nbuckets;
    Elf64_Word  nchains;
    Elf64_Word* buckets;
    Elf64_Word* chains;
};

void
elf_sysv_hashtable_init(struct elf_sysv_hashtable* out,
                   Elf64_Word* hashtable);

const Elf64_Sym*
elf_sysv_hashtable_lookup(struct elf_sysv_hashtable* ht,
                     const char* name,
                     struct elf_symbols* symbols);

#endif //__ELF_SYSV_HASHTABLE_H__