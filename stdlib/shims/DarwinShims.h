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
#ifndef SWIFT_STDLIB_SHIMS_DARWINSHIMS_H_
#define SWIFT_STDLIB_SHIMS_DARWINSHIMS_H_

#include <stddef.h> // for size_t

size_t swift_malloc_size(const void *ptr);
size_t strlen(const char *s);
char *strcpy(char *restrict dst, const char *restrict src);
int strcmp(const char *s1, const char *s2);
int putchar(int c);

#endif
