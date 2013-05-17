//===--- SILAllocated.h - Defines the SILAllocated class --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILALLOCATED_H
#define SWIFT_SIL_SILALLOCATED_H

#include "swift/Basic/LLVM.h"

namespace swift {
  class SILModule;

/// SILAllocated - This class enforces that derived classes are allocated out of
/// the SILModule bump pointer allocator.
template <typename DERIVED>
class SILAllocated {
public:
  /// Forward to ordinary 'new'.
  void *operator new(size_t Bytes, size_t Alignment = alignof(DERIVED)) {
    return ::operator new(Bytes);
  }

  /// Forward to ordinary 'delete'.
  void operator delete(void *Ptr, size_t) {
    ::operator delete(Ptr);
  }

  /// Custom version of 'new' that uses the SILModule's BumpPtrAllocator with
  /// precise alignment knowledge.  This is templated on the allocator type so
  /// that this doesn't require including SILModule.h.
  template <typename SizeTy>
  void *operator new(size_t Bytes, const SizeTy &C,
                     size_t Alignment = alignof(DERIVED)) {
    return C.allocate(Bytes, Alignment);
  }
};

} // end swift namespace

#endif
