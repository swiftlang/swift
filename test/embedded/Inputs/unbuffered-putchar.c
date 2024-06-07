#include <stdlib.h>
#include <stdio.h>

int putchar(int c) {
    putchar_unlocked(c);
    fflush(stdout);
    return 0;
}
