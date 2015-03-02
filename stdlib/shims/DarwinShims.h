//===--- DarwinShims.h - Access to POSIX for Swift's core stdlib ----------===//
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
//  Using the Darwin module in the core stdlib would create a circular
//  dependency, so instead we import these declarations as part of
//  SwiftShims
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_DARWINSHIMS_H
#define SWIFT_STDLIB_SHIMS_DARWINSHIMS_H

#include "SwiftStdint.h"
#include "SwiftStddef.h"

// This declaration is not universally correct.  We verify its correctness for
// the current platform in the runtime code.
typedef long int __swift_ssize_t;

__swift_size_t swift_malloc_size(const void *ptr);
void free(void *);
__swift_size_t strlen(const char *s);
char *strcpy(char *__restrict__ dst, const char *__restrict__ src);
int strcmp(const char *s1, const char *s2);
int memcmp(const void *s1, const void *s2, __swift_size_t n);

int putchar(int c);
__swift_ssize_t read(int fildes, void *buf, __swift_size_t nbyte);
__swift_ssize_t write(int fildes, const void *buf, __swift_size_t nbyte);
int close(int fildes);

double strtod(const char *__restrict__ nptr, char **__restrict__ endptr);
float strtof(const char *__restrict__ nptr, char **__restrict__ endptr);

__swift_uint32_t arc4random(void);
__swift_uint32_t arc4random_uniform(__swift_uint32_t upper_bound);

int sysctlbyname(const char *name, void *oldp, __swift_size_t *oldlenp,
                 void *newp, __swift_size_t newlen);

#endif // SWIFT_STDLIB_SHIMS_DARWINSHIMS_H

