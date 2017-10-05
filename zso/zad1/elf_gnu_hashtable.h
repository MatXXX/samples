#ifndef __ELF_GNU_HASHTABLE__
#define __ELF_GNU_HASHTABLE__

#include <elf.h>

#include "elf_symbols.h"

struct elf_gnu_hashtable {
    Elf32_Word nbuckets;
    Elf32_Word symbase;
    Elf32_Word bitmask_nwords;
    Elf32_Word bitmask_idxbits;
    Elf32_Word shift;
    Elf64_Addr* bitmask;
    Elf32_Word* buckets;
    Elf32_Word* chains;
};

void
elf_gnu_hashtable_init(struct elf_gnu_hashtable* out,
                       Elf32_Word* hashtab);

const Elf64_Sym*
elf_gnu_hashtable_lookup(struct elf_gnu_hashtable* ht,
                         const char* name,
                         struct elf_symbols* symbols);

#endif //__ELF_GNU_HASHTABLE__