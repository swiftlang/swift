//===--- LibcShims.h - Access to POSIX for Swift's core stdlib --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

// This declaration is not universally correct.  We verify its correctness for
// the current platform in the runtime code.
#if defined(__linux__) && defined (__arm__) && !defined(__android__)
typedef           int __swift_ssize_t;
#elif defined(_WIN32)
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

#define SWIFT_HAVE_BUILTIN_REMAINDERF __has_builtin(__builtin_remainderf)
#define SWIFT_HAVE_BUILTIN_SQRTF __has_builtin(__builtin_sqrtf)
#define SWIFT_HAVE_BUILTIN_REMAINDER __has_builtin(__builtin_remainder)
#define SWIFT_HAVE_BUILTIN_SQRT __has_builtin(__builtin_sqrt)

#if !SWIFT_HAVE_BUILTIN_REMAINDERF || !SWIFT_HAVE_BUILTIN_SQRTF || !SWIFT_HAVE_BUILTIN_REMAINDER || !SWIFT_HAVE_BUILTIN_SQRT
#include <math.h>
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
SWIFT_READONLY SWIFT_RUNTIME_STDLIB_INTERFACE __swift_size_t
_swift_stdlib_strlen(const char *s);

SWIFT_READONLY SWIFT_RUNTIME_STDLIB_INTERFACE __swift_size_t
_swift_stdlib_strlen_unsigned(const unsigned char *s);

SWIFT_READONLY
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
SWIFT_READNONE SWIFT_RUNTIME_STDLIB_INTERFACE __swift_size_t
_swift_stdlib_malloc_size(const void *ptr);

// Random number <random>
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t _swift_stdlib_cxx11_mt19937(void);
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t
_swift_stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound);

// Math library functions
static inline SWIFT_ALWAYS_INLINE
float _swift_stdlib_remainderf(float _self, float _other) {
#if SWIFT_HAVE_BUILTIN_REMAINDERF
  return __builtin_remainderf(_self, _other);
#else
  return remainderf(_self, _other);
#endif
}
  
static inline SWIFT_ALWAYS_INLINE
float _swift_stdlib_squareRootf(float _self) {
#if SWIFT_HAVE_BUILTIN_SQRTF
  return __builtin_sqrtf(_self);
#else
  return sqrtf(_self);
#endif
}

static inline SWIFT_ALWAYS_INLINE
double _swift_stdlib_remainder(double _self, double _other) {
#if SWIFT_HAVE_BUILTIN_REMAINDER
  return __builtin_remainder(_self, _other);
#else
  return remainder(_self, _other);
#endif
}

static inline SWIFT_ALWAYS_INLINE
double _swift_stdlib_squareRoot(double _self) {
#if SWIFT_HAVE_BUILTIN_REMAINDERF
  return __builtin_sqrt(_self);
#else
  return sqrt(_self);
#endif
}

// TODO: Remove horrible workaround when importer does Float80 <-> long double.
#if (defined __i386__ || defined __x86_64__) && !defined _MSC_VER
static inline SWIFT_ALWAYS_INLINE
void _swift_stdlib_remainderl(void *_self, const void *_other) {
  long double *_f80self = (long double *)_self;
  *_f80self = __builtin_remainderl(*_f80self, *(const long double *)_other);
}

static inline SWIFT_ALWAYS_INLINE
void _swift_stdlib_squareRootl(void *_self) {
  long double *_f80self = (long double *)_self;
  *_f80self = __builtin_sqrtl(*_f80self);
}
#endif // Have Float80

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_LIBCSHIMS_H
