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

// This declaration is not universally correct.  We verify its correctness for
// the current platform in the runtime code.
#if defined(__linux__) && defined (__arm__)
typedef           int __swift_ssize_t;
#elif defined(_WIN32)
#if defined(_M_ARM) || defined(_M_IX86)
typedef           int __swift_ssize_t;
#elif defined(_M_X64) || defined(_M_ARM64)
typedef long long int __swift_ssize_t;
#else
#error unsupported machine type
#endif
#else
typedef      long int __swift_ssize_t;
#endif

// This declaration might not be universally correct.
// We verify its correctness for the current platform in the runtime code.
#if defined(__linux__)
# if defined(__ANDROID__)
typedef __swift_uint16_t __swift_mode_t;
# else
typedef __swift_uint32_t __swift_mode_t;
# endif
#elif defined(__APPLE__)
typedef __swift_uint16_t __swift_mode_t;
#elif defined(_WIN32)
typedef __swift_int32_t __swift_mode_t;
#else  // just guessing
typedef __swift_uint16_t __swift_mode_t;
#endif


// General utilities <stdlib.h>
// Memory management functions
SWIFT_RUNTIME_STDLIB_INTERNAL
void _stdlib_free(void *ptr);

// Input/output <stdio.h>
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_putchar_unlocked(int c);
SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_size_t _stdlib_fwrite_stdout(const void *ptr, __swift_size_t size,
                                     __swift_size_t nitems);

// String handling <string.h>
SWIFT_READONLY SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_size_t _stdlib_strlen(const char *s);

SWIFT_READONLY SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_size_t _stdlib_strlen_unsigned(const unsigned char *s);

SWIFT_READONLY
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_memcmp(const void *s1, const void *s2, __swift_size_t n);

// <unistd.h>
SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_ssize_t _stdlib_read(int fd, void *buf, __swift_size_t nbyte);
SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_ssize_t _stdlib_write(int fd, const void *buf, __swift_size_t nbyte);
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_close(int fd);

// Semaphores <semaphore.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
// We can't use sem_t itself here, nor is there a platform-consistent
// definition to copy for a __swift_sem_t type. Instead we use
// void* in place of sem_t* and cast it back on the Swift side.
SWIFT_RUNTIME_STDLIB_INTERNAL
void *_stdlib_sem_open2(const char *name, int oflag);
SWIFT_RUNTIME_STDLIB_INTERNAL
void *_stdlib_sem_open4(const char *name, int oflag,
                        __swift_mode_t mode, unsigned int value);
#endif

// File control <fcntl.h>
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_open(const char *path, int oflag, __swift_mode_t mode);
#if !defined(_WIN32) || defined(__CYGWIN__)
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_openat(int fd, const char *path, int oflag, __swift_mode_t mode);
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_fcntl(int fd, int cmd, int value);
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_fcntlPtr(int fd, int cmd, void* ptr);
#endif

// I/O control <ioctl.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_ioctl(int fd, unsigned long int request, int value);
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_ioctlPtr(int fd, unsigned long int request, void* ptr);
#endif

// Environment
#if defined(__APPLE__) || defined(__FreeBSD__)
SWIFT_RUNTIME_STDLIB_INTERNAL
char * _Nullable * _Null_unspecified _stdlib_getEnviron();
#endif

// System error numbers <errno.h>
SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_getErrno();
SWIFT_RUNTIME_STDLIB_INTERNAL
void _stdlib_setErrno(int value);

// Non-standard extensions
SWIFT_READNONE SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_size_t _stdlib_malloc_size(const void *ptr);

// Random number <random>
SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint32_t _stdlib_cxx11_mt19937(void);
SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint32_t _stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound);

// Random number for stdlib
SWIFT_RUNTIME_STDLIB_INTERNAL
void _stdlib_random(void *buf, __swift_size_t nbytes);

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

// TLS - thread local storage

#if defined(__linux__)
# if defined(__ANDROID__)
typedef int __swift_thread_key_t;
# else
typedef unsigned int __swift_thread_key_t;
# endif
#elif defined(__FreeBSD__)
typedef int __swift_thread_key_t;
#elif defined(_WIN32)
typedef unsigned long __swift_thread_key_t;
#elif defined(__HAIKU__)
typedef int __swift_thread_key_t;
#else
typedef unsigned long __swift_thread_key_t;
#endif

SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_thread_key_create(__swift_thread_key_t * _Nonnull key,
                              void (* _Nullable destructor)(void * _Nullable));

SWIFT_RUNTIME_STDLIB_INTERNAL
void * _Nullable _stdlib_thread_getspecific(__swift_thread_key_t key);

SWIFT_RUNTIME_STDLIB_INTERNAL
int _stdlib_thread_setspecific(__swift_thread_key_t key,
                               const void * _Nullable value);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_LIBCSHIMS_H
