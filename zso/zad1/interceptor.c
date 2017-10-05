#define _GNU_SOURCE
#include <link.h>
#include <sys/auxv.h>
#include <elf.h>
#include <unistd.h>
#include "interceptor.h"
#include "dynamic_symbols_description.h"
#include "compare_string.h"
#include "elf_symbols.h"

static const Elf64_Dyn*
extract_dynamic_address(struct dl_phdr_info *info) {
    for (int i = 0; i < info->dlpi_phnum; i++) {
        const Elf64_Phdr* programHdr = &info->dlpi_phdr[i];
        if (programHdr->p_type == PT_DYNAMIC)
            return (Elf64_Dyn*)(info->dlpi_addr + programHdr->p_vaddr);
    }

    return NULL;
}

struct intercept_iterate_args {
    const char* name;
    void* func;
};

static int
intercept_find_got_iterate(struct dl_phdr_info *info,
                           size_t size,
                           void *data) {
    //vDSO guard, I ignore it.
    if (info->dlpi_addr == getauxval(AT_SYSINFO_EHDR))
        return 0;

    struct intercept_iterate_args* args = data;
    const Elf64_Dyn* dynamic = extract_dynamic_address(info);
    if (!dynamic)
        return 0;

    struct dynamic_symbols_description dsd;
    dynamic_symbols_description_populate(&dsd, dynamic);

    Elf64_Rela* got_entry = dynamic_symbols_description_find_got_reloc(&dsd, args->name);
    if (!got_entry)
        return 0;

    if (ELF32_R_TYPE(got_entry->r_info) == R_X86_64_JUMP_SLOT) {
        void** got_entry_addr = (void**)(info->dlpi_addr + got_entry->r_offset);
        *got_entry_addr = args->func;
    }

    return 0;
}

static int
intercept_symbol_lookup_iterate(struct dl_phdr_info *info,
                                size_t size,
                                void *data) {
    if (info->dlpi_addr == getauxval(AT_SYSINFO_EHDR) ||
        compare_string(info->dlpi_name, "linux-vdso.so.1")) {
        return 0;
    }

    struct intercept_iterate_args* args = data;
    const Elf64_Dyn* dynamic = extract_dynamic_address(info);
    struct dynamic_symbols_description dsd;
    dynamic_symbols_description_populate(&dsd, dynamic);

    const Elf64_Sym* sym = dynamic_symbols_description_symbol_lookup(&dsd, args->name);
    if (sym == NULL || sym->st_shndx == SHN_UNDEF)
        return 0;

    unsigned char bind = ELF64_ST_BIND(sym->st_info);
    unsigned char type = ELF64_ST_TYPE(sym->st_info);

    //If local reference was found, ignore it
    if (bind != STB_GLOBAL && bind != STB_WEAK)
        return 0;

    //Weak reference was already found, don't override it
    if (bind == STB_WEAK && args->func != NULL)
        return 0;

    //IFUNC entry should be evaluated to obtain real address
    if (type == STT_GNU_IFUNC) {
        void* (*p) (void) = (void* (*)())(info->dlpi_addr + sym->st_value);
        args->func = p();
    }
    else if (type == STT_FUNC) {
        args->func = (void*)(info->dlpi_addr + sym->st_value);
    }
    else //This symbol was not a function
        return 0;

    //Found global reference? Finish.
    //Otherwise continue search.
    return bind == STB_GLOBAL;
}

static void*
intercept_symbol_lookup(const char* name) {
    struct intercept_iterate_args args = { name, NULL };
    dl_iterate_phdr(intercept_symbol_lookup_iterate, &args);
    return args.func;
}

void*
intercept_function(const char* name,
                   void* func) {
    void* p = intercept_symbol_lookup(name);
    if (!p)
        return NULL;

    struct intercept_iterate_args args = { name, func };
    dl_iterate_phdr(intercept_find_got_iterate, &args);
    return p;
}

void
unintercept_function(const char* name) {
    void* p = intercept_symbol_lookup(name);
    if (!p)
        return;

    struct intercept_iterate_args args = { name, p };
    dl_iterate_phdr(intercept_find_got_iterate, &args);
}
