//===--- SILBase.h - Defines some core details of SIL code ------*- C++ -*-===//
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
//
// This file defines the SILBase class, which provides some basic functionality
// for clients of SIL and serves as a baseclass for SIL types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBASE_H
#define SWIFT_SIL_SILBASE_H

#include "swift/Basic/LLVM.h"

namespace swift {
  class SILModule;

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
  /// precise alignment knowledge.
  template <typename SizeTy>
  void *operator new(size_t Bytes, const SizeTy &C,
                     size_t Alignment = alignof(DERIVED)) {
    return C.allocate(Bytes, Alignment);
  }
};

} // end swift namespace

#endif
