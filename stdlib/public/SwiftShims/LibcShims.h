//===--- LibcShims.h - Access to POSIX for Swift's core stdlib -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

// This declaration is not universally correct.  We verify its correctness for
// the current platform in the runtime code.
#if defined(__linux__) && defined (__arm__)
typedef      int __swift_ssize_t;
#else
typedef long int __swift_ssize_t;
#endif

// General utilities <stdlib.h>
// Memory management functions
void _swift_stdlib_free(void *ptr);

// Input/output <stdio.h>
int _swift_stdlib_putchar(int c);

// String handling <string.h>
__attribute__((pure))
__swift_size_t _swift_stdlib_strlen(const char *s);
 __attribute__((pure))
int _swift_stdlib_memcmp(const void *s1, const void *s2, __swift_size_t n);

// <unistd.h>
__swift_ssize_t _swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte);
__swift_ssize_t _swift_stdlib_write(int fd, const void *buf,
                                    __swift_size_t nbyte);
int _swift_stdlib_close(int fd);

// Non-standard extensions
__swift_size_t _swift_stdlib_malloc_size(const void *ptr);
__swift_uint32_t _swift_stdlib_arc4random(void);
__swift_uint32_t _swift_stdlib_arc4random_uniform(__swift_uint32_t upper_bound);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_LIBCSHIMS_H

