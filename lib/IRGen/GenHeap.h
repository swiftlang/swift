//===--- GenHeap.h - Heap-object layout and management ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
  class OwnedAddress;

/// A heap layout is the result of laying out a complete structure for
/// heap-allocation.
class HeapLayout : public StructLayout {
  SmallVector<SILType, 8> ElementTypes;
  NecessaryBindings Bindings;
  mutable llvm::Constant *privateMetadata = nullptr;
  
public:
  HeapLayout(IRGenModule &IGM, LayoutStrategy strategy,
             ArrayRef<SILType> elementTypes,
             ArrayRef<const TypeInfo *> elementTypeInfos,
             llvm::StructType *typeToFill = 0,
             NecessaryBindings &&bindings = {});
  
  /// True if the heap object carries type bindings.
  ///
  /// If true, the first element of the heap layout will be the type metadata
  /// buffer.
  bool hasBindings() const {
    return !Bindings.empty();
  }
  
  const NecessaryBindings &getBindings() const {
    return Bindings;
  }

  /// Get the types of the elements.
  ArrayRef<SILType> getElementTypes() const {
    return ElementTypes;
  }
  
  /// Build a size function for this layout.
  llvm::Constant *createSizeFn(IRGenModule &IGM) const;

  /// As a convenience, build a metadata object with internal linkage
  /// consisting solely of the standard heap metadata.
  llvm::Constant *getPrivateMetadata(IRGenModule &IGM,
                                     llvm::Constant *captureDescriptor) const;
};

class HeapNonFixedOffsets : public NonFixedOffsetsImpl {
  SmallVector<llvm::Value *, 1> Offsets;
  llvm::Value *TotalSize;
  llvm::Value *TotalAlignMask;
public:
  HeapNonFixedOffsets(IRGenFunction &IGF, const HeapLayout &layout);
  
  llvm::Value *getOffsetForIndex(IRGenFunction &IGF, unsigned index) override {
    auto result = Offsets[index];
    assert(result != nullptr
           && "fixed-layout field doesn't need NonFixedOffsets");
    return result;
  }
  
  // The total size of the heap object.
  llvm::Value *getSize() const {
    return TotalSize;
  }
  
  // The total alignment of the heap object.
  llvm::Value *getAlignMask() const {
    return TotalAlignMask;
  }
};

/// Emit a heap object deallocation.
void emitDeallocateHeapObject(IRGenFunction &IGF,
                              llvm::Value *object,
                              llvm::Value *size,
                              llvm::Value *alignMask);

/// Emit a class instance deallocation.
void emitDeallocateClassInstance(IRGenFunction &IGF,
                                 llvm::Value *object,
                                 llvm::Value *size,
                                 llvm::Value *alignMask);
  
/// Emit a partial class instance deallocation from a failing constructor.
void emitDeallocatePartialClassInstance(IRGenFunction &IGF,
                                        llvm::Value *object,
                                        llvm::Value *metadata,
                                        llvm::Value *size,
                                        llvm::Value *alignMask);

/// Allocate a boxed value.
///
/// The interface type is required for emitting reflection metadata.
OwnedAddress
emitAllocateBox(IRGenFunction &IGF,
                CanSILBoxType boxType,
                GenericEnvironment *env,
                const llvm::Twine &name);

/// Deallocate a box whose value is uninitialized.
void emitDeallocateBox(IRGenFunction &IGF, llvm::Value *box,
                       CanSILBoxType boxType);

/// Project the address of the value inside a box.
Address emitProjectBox(IRGenFunction &IGF, llvm::Value *box,
                       CanSILBoxType boxType);

} // end namespace irgen
} // end namespace swift

#endif
