//===--- Heap.cpp - Swift Language Heap Logic -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementations of the Swift heap
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Heap.h"
#include "Private.h"
#include "swift/Runtime/Debug.h"
#include <stdlib.h>

using namespace swift;

SWIFT_RT_ENTRY_VISIBILITY
void *swift::swift_slowAlloc(size_t size, size_t alignMask)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  // FIXME: use posix_memalign if alignMask is larger than the system guarantee.
  void *p = malloc(size);
  if (!p) swift::crash("Could not allocate memory.");
  return p;
}

SWIFT_RT_ENTRY_VISIBILITY
void swift::swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  free(ptr);
}
