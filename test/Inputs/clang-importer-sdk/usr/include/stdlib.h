#if defined(_WIN32) || defined(WIN32)
#define SDK_STDLIB_H
#endif

#ifndef SDK_STDLIB_H
#define SDK_STDLIB_H

#include <stdint.h>

typedef long ldiv_t;
typedef long long lldiv_t;

int posix_memalign(void **, size_t, size_t);
void free(void *);

ldiv_t ldiv(long int, long int);
lldiv_t lldiv(long long int, long long int);

_Noreturn void abort(void);

#endif // SDK_STDLIB_H
