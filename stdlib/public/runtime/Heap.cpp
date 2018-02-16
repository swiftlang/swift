//===--- Heap.cpp - Swift Language Heap Logic -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include <algorithm>
#include <stdlib.h>

using namespace swift;

#if defined(__APPLE__)
// Apple malloc is always 16-byte aligned.
#  define MALLOC_ALIGN_MASK 15

#elif defined(__linux__)
// Linux malloc is 16-byte aligned on 64-bit, and 8-byte aligned on 32-bit.
#  if defined(__LP64)
#    define MALLOC_ALIGN_MASK 15
#  else
#    define MALLOC_ALIGN_MASK 7
#  endif

#elif defined(_WIN64)
// Windows malloc is 16-byte aligned on 64-bit and 8-byte aligned on 32-bit.
#  define MALLOC_ALIGN_MASK 15
#elif defined(_WIN32)
#  define MALLOC_ALIGN_MASK 7

#else
// Unknown alignment, but the standard requires alignment suitable for the largest
// standard types.
#  define MALLOC_ALIGN_MASK std::max(alignof(void *), alignof(double))

#endif



void *swift::swift_slowAlloc(size_t size, size_t alignMask)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  void *p;
  if (alignMask <= MALLOC_ALIGN_MASK) {
    p = malloc(size);
  } else {
    p = AlignedAlloc(size, alignMask + 1);
  }
  if (!p) swift::crash("Could not allocate memory.");
  return p;
}

void swift::swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  if (alignMask <= MALLOC_ALIGN_MASK) {
    free(ptr);
  } else {
    AlignedFree(ptr);
  }
}
