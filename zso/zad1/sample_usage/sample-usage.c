#include <stdio.h>
#include "../interceptor.h"

int (*puts_orig) (const char *);

int my_puts(const char *s) {
    puts_orig("intercepted");
}

int main() {
    puts("before intercept");
    puts_orig = intercept_function("puts", my_puts);
    puts("???");
    unintercept_function("puts");
    puts("after unintercept");
}
