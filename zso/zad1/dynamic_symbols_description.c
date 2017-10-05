#include "dynamic_symbols_description.h"
#include "compare_string.h"

static void 
get_segments_info(struct dynamic_symbols_description* out,
                  const Elf64_Dyn* dynamic) {
    const Elf64_Dyn* entry = dynamic;
    while (entry->d_tag != DT_NULL) {
        switch (entry->d_tag) {
        case DT_STRTAB:
            out->symbols.strings = (void*)entry->d_un.d_ptr;
            break;
        case DT_SYMTAB:
            out->symbols.symbols = (void*)entry->d_un.d_ptr;
            break;
        case DT_PLTREL:
            out->plt_rel_type = entry->d_un.d_val;
            break;
        case DT_PLTRELSZ:
            out->plt_rel.size = entry->d_un.d_val;
            break;
        case DT_JMPREL:
            out->plt_rel.addr = entry->d_un.d_val;
            break;
        case DT_HASH:
            elf_sysv_hashtable_init(&out->hash_tab, (Elf64_Word*)(entry->d_un.d_ptr));
            break;
        case DT_GNU_HASH:
            elf_gnu_hashtable_init(&out->gnu_hash_tab, (Elf64_Word*)(entry->d_un.d_ptr));
            break;
        }
        entry++;
    }
    if (out->plt_rel_type == DT_RELA) //x86_64 uses only RELA relocs type
        out->plt_rel.elem_size = sizeof(Elf64_Rela);
    else //NO PLT
        out->plt_rel.elem_size = 0;
}

void 
dynamic_symbols_description_populate(struct dynamic_symbols_description* out,
                                     const Elf64_Dyn* dynamic) {
    char* out_alias = (char*)out;
    if (!out)
        return;

    //Clear contents
    //Cannot use memset, it might have been replaced.
    for (int i = 0; i < sizeof(struct dynamic_symbols_description); i++)
        out_alias[i] = 0;

    //Collect information contained in dynamic section
    get_segments_info(out, dynamic);
}

int
dynamic_symbols_description_has_hash_tab(struct dynamic_symbols_description* dsd) {
    if (!dsd)
        return 0;
    return dsd->hash_tab.nbuckets != 0;
}

int
dynamic_symbols_description_has_gnu_hash_tab(struct dynamic_symbols_description* dsd) {
    if (!dsd)
        return 0;
    return dsd->gnu_hash_tab.nbuckets != 0;
}

int
dynamic_symbols_description_has_plt_rel_tab(struct dynamic_symbols_description* dsd) {
    if (!dsd)
        return 0;
    return dsd->plt_rel.elem_size != 0;
}

const Elf64_Sym*
dynamic_symbols_description_symbol_lookup(struct dynamic_symbols_description* dsd,
                                          const char* name) {
    if (!dsd)
        return NULL;

    const Elf64_Sym* sym = STN_UNDEF;
    if (dynamic_symbols_description_has_gnu_hash_tab(dsd))
        sym = elf_gnu_hashtable_lookup(&dsd->gnu_hash_tab, name, &dsd->symbols);
    if (sym == STN_UNDEF && dynamic_symbols_description_has_hash_tab(dsd))
        sym = elf_sysv_hashtable_lookup(&dsd->hash_tab, name, &dsd->symbols);

    return sym;
}

Elf64_Rela*
dynamic_symbols_description_find_got_reloc(struct dynamic_symbols_description* dsd,
                                           const char* name) {
    if (!dsd || !dynamic_symbols_description_has_plt_rel_tab(dsd))
        return NULL;

    Elf64_Rela* relocs = (Elf64_Rela*)dsd->plt_rel.addr;

    if (!relocs)
        return NULL;

    int elems = dsd->plt_rel.size / dsd->plt_rel.elem_size;
    for (int i = 0; i < elems; i++) {
        if (ELF64_R_TYPE(relocs[i].r_info) != R_X86_64_JUMP_SLOT)
            continue;
        
        const char* symbol_name = elf_symbols_get_name(&dsd->symbols, ELF64_R_SYM(relocs[i].r_info));
        if (compare_string(symbol_name, name))
            return &relocs[i];
    }

    return NULL;
}