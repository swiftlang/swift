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

#include "swift/ABI/Metadata.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/HeapObject.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"

namespace swift {
/// A data structure responsible for efficiently allocating closure contexts for
/// linear maps such as pullbacks, including recursive branching trace enum
/// case payloads.
class AutoDiffLinearMapContext : public HeapObject {
  /// A simple wrapper around a context object allocated by the
  /// `AutoDiffLinearMapContext` type. This type knows all the "physical"
  /// properties and behavior of the allocated context object by way of
  /// storing the allocated type's `TypeMetadata`. It uses this information
  /// to ensure that the allocated context object is destroyed/deinitialized
  /// properly, upon its own destruction.
  class [[nodiscard]] AllocatedContextObjectRecord final {
    const Metadata *contextObjectMetadata;
    OpaqueValue *contextObjectPtr;

  public:
    AllocatedContextObjectRecord(const Metadata *contextObjectMetadata,
                                 OpaqueValue *contextObjectPtr)
        : contextObjectMetadata(contextObjectMetadata),
          contextObjectPtr(contextObjectPtr) {}

    AllocatedContextObjectRecord(const Metadata *contextObjectMetadata,
                                 void *contextObjectPtr)
        : AllocatedContextObjectRecord(
              contextObjectMetadata,
              static_cast<OpaqueValue *>(contextObjectPtr)) {}

    ~AllocatedContextObjectRecord() {
      contextObjectMetadata->vw_destroy(contextObjectPtr);
    }

    size_t size() const { return contextObjectMetadata->vw_size(); }

    size_t align() const { return contextObjectMetadata->vw_alignment(); }
  };

private:
  /// The underlying allocator.
  // TODO: Use a custom allocator so that the initial slab can be
  // tail-allocated.
  llvm::BumpPtrAllocator allocator;

  /// Storage for `AllocatedContextObjectRecord`s, corresponding to the
  /// subcontext allocations performed by the type.
  llvm::SmallVector<AllocatedContextObjectRecord, 4> allocatedContextObjects;

public:
  /// Creates a linear map context.
  AutoDiffLinearMapContext(const Metadata *topLevelLinearMapContextMetadata);

  /// Returns the address of the tail-allocated top-level subcontext.
  void *projectTopLevelSubcontext() const;

  /// Allocates memory for a new subcontext.
  void *allocateSubcontext(const Metadata *contextObjectMetadata);
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
