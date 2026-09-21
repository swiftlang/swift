#ifndef PASS_OBJECT_SIZE_OVERLOADS_H
#define PASS_OBJECT_SIZE_OVERLOADS_H

#include <stddef.h>

__attribute__((overloadable)) size_t ovl_fp(char *p);
__attribute__((overloadable)) size_t ovl_fp(char *p __attribute__((pass_object_size(0))));

#endif // PASS_OBJECT_SIZE_OVERLOADS_H
