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

#include "SwiftStddef.h"

__swift_size_t swift_malloc_size(const void *ptr);
__swift_size_t strlen(const char *s);
char *strcpy(char *restrict dst, const char *restrict src);
int strcmp(const char *s1, const char *s2);
int memcmp(const void *s1, const void *s2, __swift_size_t n);
int putchar(int c);

#endif // SWIFT_STDLIB_SHIMS_DARWINSHIMS_H

