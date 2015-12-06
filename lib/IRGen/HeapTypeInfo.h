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

  bool isSingleRetainablePointer(ResilienceScope scope,
                                 ReferenceCounting *refcounting) const override {
    if(refcounting)
      *refcounting = asDerived().getReferenceCounting();
    return true;
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
    case ReferenceCounting::Error:
      llvm_unreachable("errortype doesn't have an isa");
    }
  }

  static const bool IsScalarPOD = false;

  void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const {
    IGF.emitStrongRelease(value, asDerived().getReferenceCounting());
  }

  void emitScalarFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
    return IGF.emitFixLifetime(value);
  }

  void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const {
    IGF.emitStrongRetain(value, asDerived().getReferenceCounting());
  }

  void emitScalarRetainUnowned(IRGenFunction &IGF, llvm::Value *value) const {
    IGF.emitStrongRetainUnowned(value, asDerived().getReferenceCounting());
  }

  void emitScalarUnownedRelease(IRGenFunction &IGF, llvm::Value *value) const {
    IGF.emitUnownedRelease(value, asDerived().getReferenceCounting());
  }

  void emitScalarUnownedRetain(IRGenFunction &IGF, llvm::Value *value) const {
    IGF.emitUnownedRetain(value, asDerived().getReferenceCounting());
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

  LoadedRef loadRefcountedPtr(IRGenFunction &IGF, SourceLoc loc,
                              Address addr) const override {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return LoadedRef(IGF.emitLoadNativeRefcountedPtr(addr), true);
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Unknown:
      return LoadedRef(IGF.emitLoadUnknownRefcountedPtr(addr), true);
    case ReferenceCounting::Bridge:
      return LoadedRef(IGF.emitLoadBridgeRefcountedPtr(addr), true);
    case ReferenceCounting::Error:
      llvm_unreachable("not supported!");
    }
  }

  const WeakTypeInfo *createWeakStorageType(TypeConverter &TC) const override {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return TC.createSwiftWeakStorageType(this->getStorageType());
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Unknown:
      return TC.createUnknownWeakStorageType(this->getStorageType());
    case ReferenceCounting::Bridge:
    case ReferenceCounting::Error:
      llvm_unreachable("not supported!");
    }
  }

  const UnownedTypeInfo *
  createUnownedStorageType(TypeConverter &TC) const override {
    switch (asDerived().getReferenceCounting()) {
    case ReferenceCounting::Native:
      return TC.createSwiftUnownedStorageType(this->getStorageType());
    case ReferenceCounting::ObjC:
    case ReferenceCounting::Block:
    case ReferenceCounting::Unknown:
      return TC.createUnknownUnownedStorageType(this->getStorageType());
    case ReferenceCounting::Bridge:
    case ReferenceCounting::Error:
      llvm_unreachable("not supported!");
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

  APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
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


