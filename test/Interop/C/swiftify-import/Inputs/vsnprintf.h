#pragma once

#include <stdarg.h>
#include <stddef.h>

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __printf(string_index, first_to_check)                                 \
  __attribute__((__format__(__printf__, string_index, first_to_check)))

#ifndef __cplusplus
#define __restrict restrict
#endif

int vsnprintf(char *__restrict __counted_by(__size) __str, size_t __size,
              const char *__restrict __format, va_list foo_args) __printf(3, 0);
