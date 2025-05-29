//===--- stdlib_shims.h - Swift Concurrency Support -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Forward declarations of <stdlib.h> interfaces so that Swift Concurrency
//  doesn't depend on the C library.
//
//===----------------------------------------------------------------------===//
#ifndef STDLIB_SHIMS_H
#define STDLIB_SHIMS_H

#ifdef __cplusplus
extern "C" [[noreturn]]
#endif
void exit(int);

#define EXIT_SUCCESS 0

#endif // STDLIB_SHIMS_H
