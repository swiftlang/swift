//===--- GenHeap.h - Heap-object layout and management ----------*- C++ -*-===//
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
// This file defines some routines that are useful for emitting
// operations on heap objects and their metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENHEAP_H
#define SWIFT_IRGEN_GENHEAP_H

#include "NecessaryBindings.h"
#include "StructLayout.h"

namespace llvm {
  class Constant;
  template <class T> class SmallVectorImpl;
}

namespace swift {
namespace irgen {
  class Address;
  class ManagedValue;

/// A heap layout is the result of laying out a complete structure for
/// heap-allocation.
class HeapLayout : public StructLayout {
public:
  HeapLayout(IRGenModule &IGM, LayoutStrategy strategy,
             llvm::ArrayRef<const TypeInfo *> fields,
             llvm::StructType *typeToFill = 0);

  /// Build a size function for this layout.
  llvm::Constant *createSizeFn(IRGenModule &IGM) const;

  /// As a convenience, build a metadata object with internal linkage
  /// consisting solely of the standard heap metadata.
  llvm::Constant *getPrivateMetadata(IRGenModule &IGM) const;
};

/// The heap-layout of an array.
class ArrayHeapLayout {
  const TypeInfo &ElementTI;
  Size HeaderSize;
  Alignment Align;
  NecessaryBindings Bindings;

public:
  ArrayHeapLayout(IRGenFunction &IGF, CanType T);

  const TypeInfo &getElementTypeInfo() const { return ElementTI; }

  /// Returns the size required by this array.
  Size getHeaderSize() const { return HeaderSize; }

  /// Returns the alignment required by this array.
  Alignment getAlignment() const { return Align; }

  /// Returns the size required by the given length.  If 'canOverflow',
  /// perform overflow checks and produce (size_t) -1 on overflow.
  llvm::Value *getAllocationSize(IRGenFunction &IGF, llvm::Value *&length,
                                 bool canOverflow, bool updateLength) const;

  /// Derive a pointer to the length field of the given allocation.
  Address getLengthPointer(IRGenFunction &IGF, llvm::Value *alloc) const;

  /// Derive a pointer to the first element of the given allocation.
  llvm::Value *getBeginPointer(IRGenFunction &IGF, llvm::Value *alloc) const;

  /// Allocate the array without a cleanup.
  llvm::Value *emitUnmanagedAlloc(IRGenFunction &IGF,
                                  llvm::Value *length,
                                  Address &beginPtr,
                                  Expr *init,
                                  const llvm::Twine &name) const;

  /// Allocate the array with a release cleanup.
  ManagedValue emitAlloc(IRGenFunction &IGF, llvm::Value *length,
                         Address &beginPtr,
                         Expr *init, const llvm::Twine &name) const;

private:
  llvm::Constant *getPrivateMetadata(IRGenModule &IGM) const;
};

} // end namespace irgen
} // end namespace swift

#endif
