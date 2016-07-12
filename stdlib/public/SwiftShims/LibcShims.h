//===--- LibcShims.h - Access to POSIX for Swift's core stdlib --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Using the Darwin (or Glibc) module in the core stdlib would create a
//  circular dependency, so instead we import these declarations as part of
//  SwiftShims.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_LIBCSHIMS_H
#define SWIFT_STDLIB_SHIMS_LIBCSHIMS_H

#include "SwiftStdint.h"
#include "SwiftStddef.h"
#include "Visibility.h"

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

// This declaration is not universally correct.  We verify its correctness for
// the current platform in the runtime code.
#if defined(__linux__) && defined (__arm__) && !defined(__android__)
typedef           int __swift_ssize_t;
#elif defined(_MSC_VER)
#if defined(_M_ARM) || defined(_M_IX86)
typedef           int __swift_ssize_t;
#elif defined(_M_X64)
typedef long long int __swift_ssize_t;
#else
#error unsupported machine type
#endif
#else
typedef      long int __swift_ssize_t;
#endif

// General utilities <stdlib.h>
// Memory management functions
SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_stdlib_free(void *ptr);

// Input/output <stdio.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
int _swift_stdlib_putchar_unlocked(int c);
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t _swift_stdlib_fwrite_stdout(const void *ptr, __swift_size_t size,
                                           __swift_size_t nitems);

// String handling <string.h>
__attribute__((__pure__)) SWIFT_RUNTIME_STDLIB_INTERFACE __swift_size_t
_swift_stdlib_strlen(const char *s);

__attribute__((__pure__))
SWIFT_RUNTIME_STDLIB_INTERFACE
int _swift_stdlib_memcmp(const void *s1, const void *s2, __swift_size_t n);

// <unistd.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t _swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte);
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t _swift_stdlib_write(int fd, const void *buf,
                                    __swift_size_t nbyte);
SWIFT_RUNTIME_STDLIB_INTERFACE
int _swift_stdlib_close(int fd);

// Non-standard extensions
__attribute__((__const__)) SWIFT_RUNTIME_STDLIB_INTERFACE __swift_size_t
_swift_stdlib_malloc_size(const void *ptr);

// Random number <random>
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t _swift_stdlib_cxx11_mt19937(void);
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t
_swift_stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound);
  
// Math library functions
static inline float _swift_stdlib_remainderf(float x, float y) {
  return __builtin_remainderf(x, y);
}
  
static inline float _swift_stdlib_sqrtf(float x) {
  return __builtin_sqrtf(x);
}
  
static inline double _swift_stdlib_remainder(double x, double y) {
  return __builtin_remainder(x, y);
}
  
static inline double _swift_stdlib_sqrt(double x) {
  return __builtin_sqrt(x);
}
  
// TODO: Remove horrible workaround when importer does Float80 <-> long double.
#if (defined __i386__ || defined __x86_64__) && !defined _MSC_VER
static inline void _swift_stdlib_remainderl(void *x, const void *y) {
  long double *ptr = (long double *)x;
  *ptr = __builtin_remainderl(*ptr, *(const long double *)y);
}

static inline void _swift_stdlib_sqrtl(void *x) {
  long double *ptr = (long double *)x;
  *ptr = __builtin_sqrtl(*ptr);
}
#endif

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_LIBCSHIMS_H
