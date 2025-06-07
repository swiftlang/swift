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
#include "llvm/ADT/SmallVector.h"
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
      /*type layout*/ nullptr,
    },
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

AutoDiffLinearMapContext::AutoDiffLinearMapContext(
    const Metadata *topLevelLinearMapContextMetadata)
    : HeapObject(&linearMapContextHeapMetadata) {
  allocatedContextObjects.push_back(AllocatedContextObjectRecord{
      topLevelLinearMapContextMetadata, projectTopLevelSubcontext()});
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

void *AutoDiffLinearMapContext::allocateSubcontext(
    const Metadata *contextObjectMetadata) {
  auto size = contextObjectMetadata->vw_size();
  auto align = contextObjectMetadata->vw_alignment();
  auto *contextObjectPtr = allocator.Allocate(size, align);
  allocatedContextObjects.push_back(
      AllocatedContextObjectRecord{contextObjectMetadata, contextObjectPtr});
  return contextObjectPtr;
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
    AutoDiffLinearMapContext *linearMapContext) {
  return static_cast<void *>(linearMapContext->projectTopLevelSubcontext());
}

void *swift::swift_autoDiffAllocateSubcontext(
    AutoDiffLinearMapContext *allocator, size_t size) {
  return allocator->allocate(size);
}

AutoDiffLinearMapContext *swift::swift_autoDiffCreateLinearMapContextWithType(
    const Metadata *topLevelLinearMapContextMetadata) {
  assert(topLevelLinearMapContextMetadata->getValueWitnesses() != nullptr);
  auto topLevelLinearMapContextSize =
      topLevelLinearMapContextMetadata->vw_size();
  auto allocationSize = alignTo(sizeof(AutoDiffLinearMapContext),
                                alignof(AutoDiffLinearMapContext)) +
                        topLevelLinearMapContextSize;
  auto *buffer = (AutoDiffLinearMapContext *)malloc(allocationSize);
  return ::new (buffer)
      AutoDiffLinearMapContext(topLevelLinearMapContextMetadata);
}

void *swift::swift_autoDiffAllocateSubcontextWithType(
    AutoDiffLinearMapContext *linearMapContext,
    const Metadata *linearMapSubcontextMetadata) {
  assert(linearMapSubcontextMetadata->getValueWitnesses() != nullptr);
  return linearMapContext->allocateSubcontext(linearMapSubcontextMetadata);
}
