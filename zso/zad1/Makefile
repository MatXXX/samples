SOURCES=interceptor.c elf_gnu_hashtable.c elf_sysv_hashtable.c dynamic_symbols_description.c
CFLAGS=-std=c99 -Wall -Wpedantic -shared -fPIC 
LFLAGS=-Wl,-soname=libinterceptor.so,--version-script=version_script.exp

all:
	gcc $(SOURCES) $(CFLAGS) -o libinterceptor.so $(LFLAGS)

clean:
	rm -f *.o libinterceptor.so

