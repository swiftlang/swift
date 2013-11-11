//===--- HeapTypeInfo.h - Utilities for reference-counted types -*- C++ -*-===//
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
// types that are single, reference-counted pointers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_HEAPTYPEINFO_H
#define SWIFT_IRGEN_HEAPTYPEINFO_H

#include "llvm/IR/DerivedTypes.h"
#include "ReferenceTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "SwiftTargetInfo.h"

namespace swift {
namespace irgen {

/// \group Extra inhabitants of heap object pointers.

/// Return the number of extra inhabitant representations for heap objects,
/// that is, the number of invalid heap object pointer values that can be used
/// to represent enum tags for enums involving a reference type as a payload.
unsigned getHeapObjectExtraInhabitantCount(IRGenModule &IGM);
  
/// Return an indexed extra inhabitant constant for a heap object pointer.
///
/// If the pointer appears within a larger aggregate, the 'bits' and 'offset'
/// arguments can be used to position the inhabitant within the larger integer
/// constant.
llvm::ConstantInt *getHeapObjectFixedExtraInhabitantValue(IRGenModule &IGM,
                                                          unsigned bits,
                                                          unsigned index,
                                                          unsigned offset);
  
/// Calculate the index of a heap object extra inhabitant representation stored
/// in memory.
llvm::Value *getHeapObjectExtraInhabitantIndex(IRGenFunction &IGF,
                                               Address src);

/// Calculate an extra inhabitant representation from an index and store it to
/// memory.
void storeHeapObjectExtraInhabitant(IRGenFunction &IGF,
                                    llvm::Value *index,
                                    Address dest);

/// \group HeapTypeInfo
  
/// HeapTypeInfo - A type designed for use implementing a type
/// which consists solely of something reference-counted.
///
/// Subclasses should implement the following method, returning true
/// if it's known to be OK to use Swift reference-counting on values
/// of this type:
///   bool hasSwiftRefcount() const;
template <class Impl>
class HeapTypeInfo : public SingleScalarTypeInfo<Impl, ReferenceTypeInfo> {
  typedef SingleScalarTypeInfo<Impl, ReferenceTypeInfo> super;
protected:
  using super::asDerived;
public:
  HeapTypeInfo(llvm::PointerType *storage, Size size, llvm::BitVector spareBits,
               Alignment align)
    : super(storage, size, spareBits, align) {}

  bool isSingleRetainablePointer(ResilienceScope scope) const {
    return asDerived().hasSwiftRefcount();
  }

  static const bool IsScalarPOD = false;

  void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const {
    if (asDerived().hasSwiftRefcount()) {
      IGF.emitRelease(value);
    } else {
      IGF.emitObjCRelease(value);
    }
  }

  void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const {
    if (asDerived().hasSwiftRefcount()) {
      IGF.emitRetainCall(value);
    } else {
      IGF.emitObjCRetainCall(value);
    }
  }

  void emitScalarRetainUnowned(IRGenFunction &IGF, llvm::Value *value) const {
    if (asDerived().hasSwiftRefcount()) {
      IGF.emitRetainUnowned(value);
    } else {
      IGF.emitUnknownRetainUnowned(value);
    }
  }

  void emitScalarUnownedRelease(IRGenFunction &IGF, llvm::Value *value) const {
    if (asDerived().hasSwiftRefcount()) {
      IGF.emitUnownedRelease(value);
    } else {
      IGF.emitUnknownUnownedRelease(value);
    }
  }

  void emitScalarUnownedRetain(IRGenFunction &IGF, llvm::Value *value) const {
    if (asDerived().hasSwiftRefcount()) {
      IGF.emitUnownedRetain(value);
    } else {
      IGF.emitUnknownUnownedRetain(value);
    }
  }

  void retain(IRGenFunction &IGF, Explosion &e) const override {
    llvm::Value *value = e.claimNext();
    asDerived().emitScalarRetain(IGF, value);
  }

  void release(IRGenFunction &IGF, Explosion &e) const override {
    llvm::Value *value = e.claimNext();
    asDerived().emitScalarRelease(IGF, value);
  }

  void retainUnowned(IRGenFunction &IGF, Explosion &e) const override {
    llvm::Value *value = e.claimNext();
    asDerived().emitScalarRetainUnowned(IGF, value);
  }

  void unownedRetain(IRGenFunction &IGF, Explosion &e) const override {
    llvm::Value *value = e.claimNext();
    asDerived().emitScalarUnownedRetain(IGF, value);
  }

  void unownedRelease(IRGenFunction &IGF, Explosion &e) const override {
    llvm::Value *value = e.claimNext();
    asDerived().emitScalarUnownedRelease(IGF, value);
  }

  const WeakTypeInfo *createWeakStorageType(TypeConverter &TC) const {
    if (asDerived().hasSwiftRefcount()) {
      return TC.createSwiftWeakStorageType(this->getStorageType());
    } else {
      return TC.createUnknownWeakStorageType(this->getStorageType());
    }
  }

  const UnownedTypeInfo *createUnownedStorageType(TypeConverter &TC) const {
    if (asDerived().hasSwiftRefcount()) {
      return TC.createSwiftUnownedStorageType(this->getStorageType());
    } else {
      return TC.createUnknownUnownedStorageType(this->getStorageType());
    }
  }

  // Extra inhabitants of heap object pointers.

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return true;
  }

  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return getHeapObjectExtraInhabitantCount(IGM);
  }

  llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                unsigned bits,
                                                unsigned index) const override {
    return getHeapObjectFixedExtraInhabitantValue(IGM, bits, index, 0);
  }

  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src)
  const override {
    return getHeapObjectExtraInhabitantIndex(IGF, src);
  }

  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest) const override {
    return storeHeapObjectExtraInhabitant(IGF, index, dest);
  }
};

}
}

#endif


