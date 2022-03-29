//===--- MutexC11.cpp - Supports Mutex.h using C11 threads ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Mutex and Read/Write lock implementations using C11 threads.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/Debug.h"

#if SWIFT_STDLIB_THREADING_C11

using namespace swift;

namespace c11threads {

#ifndef SWIFT_FATAL_ERROR
#define SWIFT_FATAL_ERROR swift::fatalError
#endif

// Triggered if a C11 threads call fails
void fatalError(int errcode) {
  SWIFT_FATAL_ERROR(0, "C11 threads call failed with %d\n", errcode);
}

}

#endif // SWIFT_STDLIB_THREADING_C11
