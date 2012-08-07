//===--- CFGBase.h - Defines some core details of CFGS  ----------*- C++ -*-==//
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
// This file defines the CFBase class, which provides some basic functionality
// for clients of CFGs and serves as a baseclass for CFGs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CFGBASE_H
#define SWIFT_CFGBASE_H

#include "llvm/Support/Allocator.h"
#include "llvm/Support/AlignOf.h"

namespace swift {
class CFGBase {
  /// Allocator that manages the memory of all the pieces of the CFG.
  mutable llvm::BumpPtrAllocator BPA;
public:
  /// Allocate memory using CFG's internal allocator.
  void *allocate(unsigned Size, unsigned Align) const {
    return BPA.Allocate(Size, Align);
  }
};

template <typename DERIVED>
class CFGAllocated {
public:
  /// Forward to ordinary 'new'.
  void *operator new(size_t Bytes,
                     size_t Alignment = llvm::AlignOf<DERIVED>::Alignment) {
    return ::operator new(Bytes);
  }

  /// Forward to ordinary 'delete'.
  void operator delete(void *Ptr, size_t) {
    ::operator delete(Ptr);
  }

  /// Custom version of 'new' that uses the CFG's BumpPtrAllocator with
  /// precise alignment knowledge.
  void *operator new(size_t Bytes, const swift::CFGBase &C,
                     size_t Alignment = llvm::AlignOf<DERIVED>::Alignment) {
    return C.allocate(Bytes, Alignment);
  }
};
} // end swift namespace

#endif
