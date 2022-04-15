//===--- AutoDiffSupport.cpp ----------------------------------*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "AutoDiffSupport.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/HeapObject.h"

#include <new>

using namespace swift;
using namespace llvm;

SWIFT_CC(swift)
static void destroyLinearMapContext(SWIFT_CONTEXT HeapObject *obj) {
  static_cast<AutoDiffLinearMapContext *>(obj)->~AutoDiffLinearMapContext();
  free(obj);
}

/// Heap metadata for a linear map context.
static FullMetadata<HeapMetadata> linearMapContextHeapMetadata = {
  {
    {
      &destroyLinearMapContext
    },
    {
      /*value witness table*/ nullptr
    }
  },
  {
    MetadataKind::Opaque
  }
};

AutoDiffLinearMapContext::AutoDiffLinearMapContext()
    : HeapObject(&linearMapContextHeapMetadata) {
}

void *AutoDiffLinearMapContext::projectTopLevelSubcontext() const {
  auto offset = alignTo(
      sizeof(AutoDiffLinearMapContext), alignof(AutoDiffLinearMapContext));
  return const_cast<uint8_t *>(
      reinterpret_cast<const uint8_t *>(this) + offset);
}

void *AutoDiffLinearMapContext::allocate(size_t size) {
  return allocator.Allocate(size, alignof(AutoDiffLinearMapContext));
}

AutoDiffLinearMapContext *swift::swift_autoDiffCreateLinearMapContext(
    size_t topLevelLinearMapStructSize) {
  auto allocationSize = alignTo(
      sizeof(AutoDiffLinearMapContext), alignof(AutoDiffLinearMapContext))
      + topLevelLinearMapStructSize;
  auto *buffer = (AutoDiffLinearMapContext *)malloc(allocationSize);
  return ::new (buffer) AutoDiffLinearMapContext;
}

void *swift::swift_autoDiffProjectTopLevelSubcontext(
    AutoDiffLinearMapContext *allocator) {
  return allocator->projectTopLevelSubcontext();
}

void *swift::swift_autoDiffAllocateSubcontext(
    AutoDiffLinearMapContext *allocator, size_t size) {
  return allocator->allocate(size);
}
