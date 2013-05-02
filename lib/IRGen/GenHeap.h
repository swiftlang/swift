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

/// A class to manage allocating a reference-counted array on the heap.
class HeapArrayInfo {
  const TypeInfo &ElementTI;
  NecessaryBindings Bindings;

public:
  HeapArrayInfo(IRGenFunction &IGF, CanType T);

  const TypeInfo &getElementTypeInfo() const { return ElementTI; }

  struct Layout {
    /// The offset from the allocation to the first element.
    llvm::Value *HeaderSize;

    /// The alignment requirement for the array allocation.
    llvm::Value *AllocAlign;

    /// The most aggressive statically-known alignment
    /// requirement for the total allocation.
    Alignment BestStaticAlignment;
  };

  /// Compute layout for this heap array.  The result is local to a
  /// particular IGF.
  Layout getLayout(IRGenFunction &IGF) const;

  /// Returns the size required by the given length.  If 'canOverflow',
  /// perform overflow checks and produce (size_t) -1 on overflow.
  llvm::Value *getAllocationSize(IRGenFunction &IGF, const Layout &layout,
                                 llvm::Value *&length,
                                 bool canOverflow, bool updateLength) const;

  /// Derive a pointer to the length field of the given allocation.
  Address getLengthPointer(IRGenFunction &IGF, const Layout &layout,
                           llvm::Value *alloc) const;

  /// Derive a pointer to the first element of the given allocation.
  llvm::Value *getBeginPointer(IRGenFunction &IGF, const Layout &layout,
                               llvm::Value *alloc) const;

  /// Allocate the array without a cleanup.
  llvm::Value *emitUnmanagedAlloc(IRGenFunction &IGF,
                                  llvm::Value *length,
                                  Address &beginPtr,
                                  const llvm::Twine &name) const;

private:
  llvm::Constant *getPrivateMetadata(IRGenModule &IGM) const;
};

/// Emit a heap object deallocation.
void emitDeallocateHeapObject(IRGenFunction &IGF,
                              llvm::Value *object,
                              llvm::Value *size);

} // end namespace irgen
} // end namespace swift

#endif
