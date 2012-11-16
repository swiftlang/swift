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

#ifndef SWIFT_SIL_SILBase_H
#define SWIFT_SIL_SILBase_H

#include "swift/Basic/LLVM.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/AlignOf.h"

namespace swift {
class SILTypeList;
class Type;
class SILBase {
  /// Allocator that manages the memory of all the pieces of the Function.
  mutable llvm::BumpPtrAllocator BPA;
  void *TypeListUniquing;
  SILBase(const SILBase&) = delete;
  void operator=(const SILBase&) = delete;
public:
  SILBase();
  ~SILBase();

  /// Allocate memory using Function's internal allocator.
  void *allocate(unsigned Size, unsigned Align) const {
    return BPA.Allocate(Size, Align);
  }
  /// getSILTypeList - Get a uniqued pointer to a SIL type list.  This can only
  /// be used by Value.
  SILTypeList *getSILTypeList(llvm::ArrayRef<Type> Types);
};

template <typename DERIVED>
class SILAllocated {
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

  /// Custom version of 'new' that uses the Function's BumpPtrAllocator with
  /// precise alignment knowledge.
  void *operator new(size_t Bytes, const SILBase &C,
                     size_t Alignment = llvm::AlignOf<DERIVED>::Alignment) {
    return C.allocate(Bytes, Alignment);
  }
};

} // end swift namespace

#endif
