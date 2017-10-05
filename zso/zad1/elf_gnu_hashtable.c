#include <link.h>
#include "elf_gnu_hashtable.h"
#include "compare_string.h"
#include "elf_symbols.h"

void
elf_gnu_hashtable_init(struct elf_gnu_hashtable* out,
                       Elf32_Word* hashtab) {
    out->nbuckets = *hashtab++;
    out->symbase = *hashtab++;
    out->bitmask_nwords = *hashtab++;
    out->bitmask_idxbits = out->bitmask_nwords - 1;
    out->shift = *hashtab++;
    out->bitmask = (Elf64_Addr*)hashtab;
    hashtab += sizeof(Elf64_Addr)/sizeof(Elf32_Word) * out->bitmask_nwords;
    out->buckets = hashtab;
    hashtab += out->nbuckets;
    out->chains = hashtab - out->symbase;
}

static unsigned long
elf_gnu_hash(const char *name) {
    const unsigned char *name_alias = (const unsigned char *) name;
    unsigned long h = 5381;
    unsigned char ch;

    while ((ch = *name_alias++) != '\0')
        h = (h << 5) + h + ch;
    return h & 0xffffffff;
}

const Elf64_Sym*
elf_gnu_hashtable_lookup(struct elf_gnu_hashtable* ht,
                         const char* name,
                         struct elf_symbols* symbols) {
    if (ht == NULL || name == NULL || symbols == NULL)
        return NULL;
    
    if (!ht->bitmask || !ht->buckets || !ht->chains)
        return NULL;

    unsigned int index;
    unsigned long hash = elf_gnu_hash(name);

    Elf64_Addr bitmask_word = ht->bitmask[(hash / __ELF_NATIVE_CLASS) & ht->bitmask_idxbits];

    unsigned int hash1 = hash & (__ELF_NATIVE_CLASS - 1);
    unsigned int hash2 = (hash >> ht->shift) & (__ELF_NATIVE_CLASS - 1);

    if (!((bitmask_word >> hash1) & (bitmask_word >> hash2) & 1))
        return NULL;
    
    Elf32_Word bucket = ht->buckets[hash % ht->nbuckets];
    if (bucket == 0)
        return NULL;
    
    const Elf32_Word *hasharr = &ht->chains[bucket];
    do {
        if (((*hasharr ^ hash) >> 1) == 0) {
            index = hasharr - ht->chains;
            const char* found = elf_symbols_get_name(symbols, index);
            if (!found)
                return NULL;

            if (compare_string(name, found))
                return elf_symbols_get_symbol(symbols, index);
        }
    } while ((*hasharr++ & 1u) == 0);
    
    return NULL;
}
