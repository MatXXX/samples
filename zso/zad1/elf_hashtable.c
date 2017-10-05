#include "elf_hashtable.h"
#include "compare_string.h"

void
elf_hashtable_init(struct elf_hashtable* out,
                   Elf64_Word* hashtable) {
    out->nbuckets = *hashtable++;
    out->nchains = *hashtable++;
    out->buckets = hashtable;
    out->chains = out->buckets + out->nbuckets;
}

static unsigned long
elf_sysv_hash(const char* namearg) {
    const unsigned char *name = (const unsigned char *) namearg;
    unsigned long h = 0, g;
    while (*name) {
        h = (h << 4) + *name++;
        if ((g = h & 0xf0000000))
            h ^= g >> 24;
        h &= ~g;
    }
    return h;
}

const Elf64_Sym*
elf_hashtable_lookup(struct elf_hashtable* ht,
                     const char* name,
                     struct elf_symbols* symbols) {
    unsigned long hash = elf_sysv_hash(name);
    unsigned long index = ht->buckets[hash % ht->nbuckets];
    do {
        const char* found = elf_symbols_get_name(symbols, index);
        if (compare_string(name, found))
            return elf_symbols_get_symbol(symbols, index);
        index = ht->chains[index];
    } while (index != STN_UNDEF);

    return NULL;
}