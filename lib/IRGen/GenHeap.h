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
             llvm::ArrayRef<const TypeInfo *> fields);

  /// Add the layout's required metadata fields to the given collection.
  /// The address point of the metadata should be at the first field added.
  ///
  /// It may be wasteful to call this multiple times.
  void buildMetadataInto(IRGenModule &IGM,
                   llvm::SmallVectorImpl<llvm::Constant*> &metadata) const;

  /// As a convenience, build a metadata object with internal linkage
  /// consisting solely of the standard heap metadata.
  llvm::Constant *getPrivateMetadata(IRGenModule &IGM) const;

  /// Given that an object has been allocated, cast the result to the
  /// appropriate type.
  Address emitCastOfAlloc(IRGenFunction &IGF, llvm::Value *alloc,
                          const llvm::Twine &name = "") const;
};

/// The heap-layout of an array.
class ArrayHeapLayout {
  const TypeInfo &ElementTI;
  Size HeaderSize;
  Alignment Align;
public:
  ArrayHeapLayout(IRGenModule &IGM, const TypeInfo &elementTI);

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

  /// Allocate the array.
  ManagedValue emitAlloc(IRGenFunction &IGF, llvm::Value *length,
                         Address &beginPtr,
                         Expr *init, const llvm::Twine &name) const;

private:
  llvm::Constant *getPrivateMetadata(IRGenModule &IGM) const;
  void buildMetadataInto(IRGenModule &IGM,
                   llvm::SmallVectorImpl<llvm::Constant*> &fields) const;
};

} // end namespace irgen
} // end namespace swift

#endif
