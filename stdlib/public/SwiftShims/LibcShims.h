//===--- LibcShims.h - Access to POSIX for Swift's core stdlib --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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

// This declaration might not be universally correct.
// We verify its correctness for the current platform in the runtime code.
#if defined(__linux__)
# if defined(__ANDROID__) && !defined(__aarch64__)
typedef __swift_uint16_t __swift_mode_t;
# else
typedef __swift_uint32_t __swift_mode_t;
# endif
#elif defined(__APPLE__)
typedef __swift_uint16_t __swift_mode_t;
#elif defined(_WIN32)
typedef __swift_int32_t __swift_mode_t;
#elif defined(__CYGWIN__)
typedef __swift_uint32_t __swift_mode_t;
#else  // just guessing
typedef __swift_uint16_t __swift_mode_t;
#endif


// Input/output <stdio.h>
SWIFT_RUNTIME_STDLIB_INTERNAL
int _swift_stdlib_putchar_unlocked(int c);
SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_size_t _swift_stdlib_fwrite_stdout(const void *ptr, __swift_size_t size,
                                           __swift_size_t nitems);

// General utilities <stdlib.h>
// Memory management functions
static inline void _swift_stdlib_free(void *ptr) {
  extern void free(void *);
  free(ptr);
}

// <unistd.h>
SWIFT_RUNTIME_STDLIB_SPI
__swift_ssize_t _swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte);
SWIFT_RUNTIME_STDLIB_SPI
__swift_ssize_t _swift_stdlib_write(int fd, const void *buf, __swift_size_t nbyte);
SWIFT_RUNTIME_STDLIB_SPI
int _swift_stdlib_close(int fd);

// String handling <string.h>
SWIFT_READONLY
static inline __swift_size_t _swift_stdlib_strlen(const char *s) {
  extern __swift_size_t strlen(const char *);
  return strlen(s);
}

SWIFT_READONLY
static inline __swift_size_t _swift_stdlib_strlen_unsigned(const unsigned char *s) {
  return _swift_stdlib_strlen((const char *)s);
}

SWIFT_READONLY
static inline int _swift_stdlib_memcmp(const void *s1, const void *s2,
                                       __swift_size_t n) {
  extern int memcmp(const void *, const void *, __swift_size_t);
  return memcmp(s1, s2, n);
}

// Casting helper. This code needs to work when included from C or C++.
// Casting away const with a C-style cast warns in C++. Use a const_cast
// there.
#ifdef __cplusplus
#define CONST_CAST(type, value) const_cast<type>(value)
#else
#define CONST_CAST(type, value) (type)value
#endif

// Non-standard extensions
#if defined(__APPLE__)
static inline __swift_size_t _swift_stdlib_malloc_size(const void *ptr) {
  extern __swift_size_t malloc_size(const void *);
  return malloc_size(ptr);
}
#elif defined(__linux__) || defined(__CYGWIN__) || defined(__ANDROID__) \
   || defined(__HAIKU__) || defined(__FreeBSD__)
static inline __swift_size_t _swift_stdlib_malloc_size(const void *ptr) {
#if defined(__ANDROID__)
#if __ANDROID_API__ >= 17
  extern __swift_size_t malloc_usable_size(const void *ptr);
#endif
#else
  extern __swift_size_t malloc_usable_size(void *ptr);
#endif
  return malloc_usable_size(CONST_CAST(void *, ptr));
}
#elif defined(_WIN32)
static inline __swift_size_t _swift_stdlib_malloc_size(const void *ptr) {
  extern __swift_size_t _msize(void *ptr);
  return _msize(CONST_CAST(void *, ptr));
}
#else
#error No malloc_size analog known for this platform/libc.
#endif

// Math library functions
static inline SWIFT_ALWAYS_INLINE
float _stdlib_remainderf(float _self, float _other) {
  return __builtin_remainderf(_self, _other);
}
  
static inline SWIFT_ALWAYS_INLINE
float _stdlib_squareRootf(float _self) {
  return __builtin_sqrtf(_self);
}

static inline SWIFT_ALWAYS_INLINE
double _stdlib_remainder(double _self, double _other) {
  return __builtin_remainder(_self, _other);
}

static inline SWIFT_ALWAYS_INLINE
double _stdlib_squareRoot(double _self) {
  return __builtin_sqrt(_self);
}

#if !defined _WIN32 && (defined __i386__ || defined __x86_64__)
static inline SWIFT_ALWAYS_INLINE
long double _stdlib_remainderl(long double _self, long double _other) {
  return __builtin_remainderl(_self, _other);
}
  
static inline SWIFT_ALWAYS_INLINE
long double _stdlib_squareRootl(long double _self) {
  return __builtin_sqrtl(_self);
}
#endif

// Apple's math.h does not declare lgamma_r() etc by default, but they're
// unconditionally exported by libsystem_m.dylib in all OS versions that
// support Swift development; we simply need to provide declarations here.
#if defined(__APPLE__)
float lgammaf_r(float x, int *psigngam);
double lgamma_r(double x, int *psigngam);
long double lgammal_r(long double x, int *psigngam);
#endif // defined(__APPLE__)

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_LIBCSHIMS_H
