//===--- ThreadLocalStorage.h - Wrapper for thread-local storage. --*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_THREADLOCALSTORAGE_H
#define SWIFT_STDLIB_SHIMS_THREADLOCALSTORAGE_H

#include "Visibility.h"

SWIFT_RUNTIME_STDLIB_INTERNAL
void * _Nonnull _swift_stdlib_threadLocalStorageGet(void);

#endif // SWIFT_STDLIB_SHIMS_THREADLOCALSTORAGE_H
