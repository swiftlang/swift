//===--- AutoDiffSupport.h ------------------------------------*- C++ -*---===//
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

#ifndef SWIFT_RUNTIME_AUTODIFF_SUPPORT_H
#define SWIFT_RUNTIME_AUTODIFF_SUPPORT_H

#include "swift/Runtime/Config.h"
#include "swift/Runtime/HeapObject.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
/// A data structure responsible for efficiently allocating closure contexts for
/// linear maps such as pullbacks, including recursive branching trace enum
/// case payloads.
class AutoDiffLinearMapContext : public HeapObject {
private:
  // TODO: Commenting out BumpPtrAllocator temporarily
  // until we move away from the interim solution of allocating
  // boxes for linear map contexts/subcontexts.
  //
  // /// The underlying allocator.
  // // TODO: Use a custom allocator so that the initial slab can be
  // // tail-allocated.
  // llvm::BumpPtrAllocator allocator;

  /// A projection/pointer to the memory storing the
  /// top-level linear map context object.
  OpaqueValue *topLevelLinearMapContextProjection;

  /// Storage for `HeapObject` pointers to the linear map
  /// context and subcontexts allocated for derivatives with
  /// loops.
  llvm::SmallVector<HeapObject *, 4> allocatedHeapObjects;

public:
  /// Creates a linear map context.
  AutoDiffLinearMapContext(OpaqueValue *const);

  // TODO: Commenting out BumpPtrAllocator temporarily
  // until we move away from the interim solution of allocating
  // boxes for linear map contexts/subcontexts.
  //
  // llvm::BumpPtrAllocator& getAllocator() const {
  //   return const_cast<llvm::BumpPtrAllocator&>(this->allocator);
  // }

  OpaqueValue *getTopLevelLinearMapContextProjection() const {
    return this->topLevelLinearMapContextProjection;
  }

  llvm::ArrayRef<HeapObject *> getAllocatedHeapObjects() const {
    return this->allocatedHeapObjects;
  }

  void storeAllocatedHeapObjectPtr(HeapObject *allocatedHeapObjectPtr) {
    this->allocatedHeapObjects.push_back(allocatedHeapObjectPtr);
  }
};

/// Creates a linear map context with a tail-allocated top-level subcontext.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
    AutoDiffLinearMapContext *swift_autoDiffCreateLinearMapContext(
        const Metadata *topLevelLinearMapContextMetadata);

/// Returns the address of the tail-allocated top-level subcontext.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void *swift_autoDiffProjectTopLevelSubcontext(AutoDiffLinearMapContext *);

/// Allocates memory for a new subcontext.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift) void *swift_autoDiffAllocateSubcontext(
    AutoDiffLinearMapContext *, const Metadata *linearMapSubcontextMetadata);
} // namespace swift
#endif /* SWIFT_RUNTIME_AUTODIFF_SUPPORT_H */
