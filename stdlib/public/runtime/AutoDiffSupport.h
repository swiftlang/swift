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

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Config.h"
#include "llvm/Support/Allocator.h"

namespace swift {

/// A data structure responsible for efficiently allocating closure contexts for
/// linear maps such as pullbacks, including rescursive branching trace enum
/// case payloads.
class AutoDiffLinearMapContext : public HeapObject {
private:
  /// The underlying allocator.
  // TODO: Use a custom allocator so that the initial slab can be
  // tail-allocated.
  llvm::BumpPtrAllocator allocator;

public:
  /// Creates a linear map context.
  AutoDiffLinearMapContext();
  /// Returns the address of the tail-allocated top-level subcontext.
  void *projectTopLevelSubcontext() const;
  /// Allocates memory for a new subcontext.
  void *allocate(size_t size);
};

/// Creates a linear map context with a tail-allocated top-level subcontext.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
AutoDiffLinearMapContext *swift_autoDiffCreateLinearMapContext(
    size_t topLevelSubcontextSize);

/// Returns the address of the tail-allocated top-level subcontext.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void *swift_autoDiffProjectTopLevelSubcontext(AutoDiffLinearMapContext *);

/// Allocates memory for a new subcontext.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void *swift_autoDiffAllocateSubcontext(AutoDiffLinearMapContext *, size_t size);

}

#endif /* SWIFT_RUNTIME_AUTODIFF_SUPPORT_H */
