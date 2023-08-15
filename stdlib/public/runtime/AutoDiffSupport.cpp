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
  auto *linearMapContext = static_cast<AutoDiffLinearMapContext *>(obj);

  for (auto *heapObjectPtr : linearMapContext->getAllocatedHeapObjects()) {
    swift_release(heapObjectPtr);
  }

  linearMapContext->~AutoDiffLinearMapContext();
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

AutoDiffLinearMapContext::AutoDiffLinearMapContext(
    OpaqueValue *const topLevelLinearMapContextProjection)
    : HeapObject(&linearMapContextHeapMetadata) {
  this->topLevelLinearMapContextProjection = topLevelLinearMapContextProjection;
}

AutoDiffLinearMapContext *swift::swift_autoDiffCreateLinearMapContext(
    const Metadata *topLevelLinearMapContextMetadata) {
  // Linear map context metadata must have non-null value witnesses
  assert(topLevelLinearMapContextMetadata->getValueWitnesses());

  // Allocate a box for the top-level linear map context
  auto [topLevelContextHeapObjectPtr, toplevelContextProjection] =
      swift_allocBox(topLevelLinearMapContextMetadata);

  // Create a linear map context object that stores the projection
  // for the top level context
  auto linearMapContext =
      new AutoDiffLinearMapContext(toplevelContextProjection);

  // Stash away the `HeapObject` pointer for the allocated context
  // for proper "release" during clean up.
  linearMapContext->storeAllocatedHeapObjectPtr(topLevelContextHeapObjectPtr);

  // Return the newly created linear map context object
  return linearMapContext;
}

void *swift::swift_autoDiffProjectTopLevelSubcontext(
    AutoDiffLinearMapContext *linearMapContext) {
  return static_cast<void *>(
      linearMapContext->getTopLevelLinearMapContextProjection());
}

void *swift::swift_autoDiffAllocateSubcontext(
    AutoDiffLinearMapContext *linearMapContext,
    const Metadata *linearMapSubcontextMetadata) {
  // Linear map context metadata must have non-null value witnesses
  assert(linearMapSubcontextMetadata->getValueWitnesses());

  // Allocate a box for the linear map subcontext
  auto [subcontextHeapObjectPtr, subcontextProjection] =
      swift_allocBox(linearMapSubcontextMetadata);

  // Stash away the `HeapObject` pointer for the allocated context
  // for proper "release" during clean up.
  linearMapContext->storeAllocatedHeapObjectPtr(subcontextHeapObjectPtr);

  // Return the subcontext projection
  return static_cast<void *>(subcontextProjection);
}
