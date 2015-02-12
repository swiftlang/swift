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
#include "ExtraInhabitants.h"
#include "ReferenceTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "SwiftTargetInfo.h"
#include "GenType.h"

namespace swift {
namespace irgen {

/// \group HeapTypeInfo
  
/// The kind of reference counting implementation a heap object uses.
enum class ReferenceCounting : unsigned char {
  /// The object uses native Swift reference counting.
  Native,
  
  /// The object uses ObjC reference counting.
  ///
  /// When ObjC interop is enabled, native Swift class objects are also ObjC
  /// reference counting compatible. Swift non-class heap objects are never
  /// ObjC reference counting compatible.
  ///
  /// Blocks are always ObjC reference counting compatible.
  ObjC,
  
  /// The object uses _Block_copy/_Block_release reference counting.
  ///
  /// This is a strict subset of ObjC; all blocks are also ObjC reference
  /// counting compatible. The block is assumed to have already been moved to
  /// the heap so that _Block_copy returns the same object back.
  Block,
  
  /// The object has an unknown reference counting implementation.
  ///
  /// This uses maximally-compatible reference counting entry points in the
  /// runtime.
  ///
  /// FIXME: Those entry points are currently objc_retain/objc_release, which
  /// are not compatible with non-class heap objects.
  Unknown,

  /// The object has an unknown reference counting implementation and
  /// the reference value may contain extra bits that need to be masked.
  ///
  /// This uses maximally-compatible reference counting entry points in the
  /// runtime, with a masking layer on top. A bit inside the pointer is used
  /// to signal native Swift refcounting.
  Bridge,
};
  
/// The kind of 'isa' encoding a heap object uses to reference its heap
/// metadata.
enum class IsaEncoding : unsigned char {
  /// The object stores a plain pointer to its heap metadata as its first word.
  Pointer,
  /// The object's isa is managed by the Objective-C runtime and must be
  /// accessed with object_getClass. This is a superset of "Pointer" because
  /// object_getClass is compatible with pointer isas.
  ObjC,
  /// The isa encoding is unknown and must be accessed in a maximally-compatible
  /// way.
  Unknown = ObjC,
};
  
/// HeapTypeInfo - A type designed for use implementing a type
/// which consists solely of something reference-counted.
///
/// Subclasses should implement the following method, returning true
/// if it's known to be OK to use Swift reference-counting on values
/// of this type:
///   ReferenceCounting getReferenceCounting() const;
template <class Impl>
class HeapTypeInfo : public SingleScalarTypeInfo<Impl, ReferenceTypeInfo> {
  typedef SingleScalarTypeInfo<Impl, ReferenceTypeInfo> super;
protected:
  using super::asDerived;
public:
  HeapTypeInfo(llvm::PointerType *storage, Size size, SpareBitVector spareBits,
               Alignment align)
    : super(storage, size, spareBits, align) {}

  bool isSingleSwiftRetainablePointer(ResilienceScope scope) const override {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return true;
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Unknown:
    case ReferenceCounting::Bridge:
      return false;
    }
  }
  
  IsaEncoding getIsaEncoding(ResilienceScope scope) const {
    switch (asDerived().getReferenceCounting()) {
    // We can access the isa of pure Swift heap objects directly.
    case ReferenceCounting::Native:
      return IsaEncoding::Pointer;
    // Use the ObjC runtime to access ObjC or mixed-heritage isas.
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
      return IsaEncoding::ObjC;
    case ReferenceCounting::Unknown:
      return IsaEncoding::Unknown;
    }
  }
  
  bool isSingleUnknownRetainablePointer(ResilienceScope scope) const override {
    return true;
  }

  static const bool IsScalarPOD = false;

  void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return IGF.emitRelease(value);
    case ReferenceCounting::ObjC:
      return IGF.emitObjCRelease(value);
    case ReferenceCounting::Block:
      return IGF.emitBlockRelease(value);
    case ReferenceCounting::Unknown:
      return IGF.emitUnknownRelease(value);
    case ReferenceCounting::Bridge:
      return IGF.emitBridgeRelease(value);
    }
  }

  void emitScalarFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
    return IGF.emitFixLifetime(value);
  }

  void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      IGF.emitRetainCall(value);
      return;
    case ReferenceCounting::Bridge:
      IGF.emitBridgeRetainCall(value);
      return;
    case ReferenceCounting::ObjC:
      IGF.emitObjCRetainCall(value);
      return;
    case ReferenceCounting::Block:
      IGF.emitBlockCopyCall(value);
      return;
    case ReferenceCounting::Unknown:
      IGF.emitUnknownRetainCall(value);
      return;
    }
  }

  void emitScalarRetainUnowned(IRGenFunction &IGF, llvm::Value *value) const {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return IGF.emitRetainUnowned(value);
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Unknown:
      return IGF.emitUnknownRetainUnowned(value);
    case ReferenceCounting::Bridge:
      llvm_unreachable("not supported!");
    }
  }

  void emitScalarUnownedRelease(IRGenFunction &IGF, llvm::Value *value) const {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return IGF.emitUnownedRelease(value);
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Unknown:
      return IGF.emitUnknownUnownedRelease(value);
    case ReferenceCounting::Bridge:
      llvm_unreachable("not supported!");
    }
  }

  void emitScalarUnownedRetain(IRGenFunction &IGF, llvm::Value *value) const {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return IGF.emitUnownedRetain(value);
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Unknown:
      return IGF.emitUnknownUnownedRetain(value);
    case ReferenceCounting::Bridge:
      llvm_unreachable("not supported!");
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

  const WeakTypeInfo *createWeakStorageType(TypeConverter &TC) const override {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return TC.createSwiftWeakStorageType(this->getStorageType());
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Bridge:
    case ReferenceCounting::Unknown:
      return TC.createUnknownWeakStorageType(this->getStorageType());
    }
  }

  const UnownedTypeInfo *
  createUnownedStorageType(TypeConverter &TC) const override {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return TC.createSwiftUnownedStorageType(this->getStorageType());
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Bridge:
    case ReferenceCounting::Unknown:
      return TC.createUnknownUnownedStorageType(this->getStorageType());
    }
  }

  const TypeInfo *createUnmanagedStorageType(TypeConverter &TC) const override {
    return TC.createUnmanagedStorageType(this->getStorageType());
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

  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                       SILType T)
  const override {
    return getHeapObjectExtraInhabitantIndex(IGF, src);
  }

  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T) const override {
    return storeHeapObjectExtraInhabitant(IGF, index, dest);
  }
};

}
}

#endif


