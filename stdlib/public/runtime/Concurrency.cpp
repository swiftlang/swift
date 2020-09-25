//===------------ Concurrency.cpp - Swift Concurrency Support ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementations of the concurrency runtime functions.
//
// void *swift_taskAlloc(SwiftTask *task, size_t size);
// void swift_taskDealloc(SwiftTask *task, void *ptr);
//
//===----------------------------------------------------------------------===//

#include "../SwiftShims/Visibility.h"
#include "swift/Runtime/Config.h"
#include <cstddef>
#include <stdlib.h>

struct SwiftTask;

SWIFT_RUNTIME_EXPORT
SWIFT_CC(swift)
void *swift_taskAlloc(SwiftTask *task, size_t size) {
  return malloc(size);
}

SWIFT_RUNTIME_EXPORT
SWIFT_CC(swift)
void swift_taskDealloc(SwiftTask *task, void *ptr) {
  free(ptr);
}
