//===--- HeapTypeInfo.h - Utilities for reference-counted types -*- C++ -*-===//
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

  bool isSingleRetainablePointer(ResilienceExpansion expansion,
                                 ReferenceCounting *refcounting) const override {
    if (refcounting)
      *refcounting = asDerived().getReferenceCounting();
    return true;
  }
  
  IsaEncoding getIsaEncoding(ResilienceExpansion expansion) const {
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

  // Emit the copy/destroy operations required by SingleScalarTypeInfo
  // using strong reference counting.
  void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value,
                         Atomicity atomicity) const {
    IGF.emitStrongRelease(value, asDerived().getReferenceCounting(), atomicity);
  }

  void emitScalarFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
    return IGF.emitFixLifetime(value);
  }

  void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value,
                        Atomicity atomicity) const {
    IGF.emitStrongRetain(value, asDerived().getReferenceCounting(), atomicity);
  }

  // Implement the primary retain/release operations of ReferenceTypeInfo
  // using basic reference counting.
  void strongRetain(IRGenFunction &IGF, Explosion &e,
                    Atomicity atomicity) const override {
    llvm::Value *value = e.claimNext();
    asDerived().emitScalarRetain(IGF, value, atomicity);
  }

  void strongRelease(IRGenFunction &IGF, Explosion &e,
                     Atomicity atomicity) const override {
    llvm::Value *value = e.claimNext();
    asDerived().emitScalarRelease(IGF, value, atomicity);
  }

  void strongRetainUnowned(IRGenFunction &IGF, Explosion &e,
                           Atomicity atomicity) const override {
    llvm::Value *value = e.claimNext();
    IGF.emitStrongRetainUnowned(value, asDerived().getReferenceCounting(),
                                atomicity);
  }

  void strongRetainUnownedRelease(IRGenFunction &IGF,
                                  Explosion &e,
                                  Atomicity atomicity) const override {
    llvm::Value *value = e.claimNext();
    IGF.emitStrongRetainAndUnownedRelease(value,
                                          asDerived().getReferenceCounting(),
                                          atomicity);
  }

  void unownedRetain(IRGenFunction &IGF, Explosion &e,
                     Atomicity atomicity) const override {
    llvm::Value *value = e.claimNext();
    IGF.emitUnownedRetain(value, asDerived().getReferenceCounting(), atomicity);
  }

  void unownedRelease(IRGenFunction &IGF, Explosion &e,
                      Atomicity atomicity) const override {
    llvm::Value *value = e.claimNext();
    IGF.emitUnownedRelease(value, asDerived().getReferenceCounting(), atomicity);
  }

  void unownedLoadStrong(IRGenFunction &IGF, Address src,
                         Explosion &out) const override {
    llvm::Value *value = IGF.emitUnownedLoadStrong(src, this->getStorageType(),
                                           asDerived().getReferenceCounting());
    out.add(value);
  }

  void unownedTakeStrong(IRGenFunction &IGF, Address src,
                         Explosion &out) const override {
    llvm::Value *value = IGF.emitUnownedTakeStrong(src, this->getStorageType(),
                                           asDerived().getReferenceCounting());
    out.add(value);
  }

  void unownedInit(IRGenFunction &IGF, Explosion &in,
                   Address dest) const override {
    IGF.emitUnownedInit(in.claimNext(), dest,
                        asDerived().getReferenceCounting());
  }

  void unownedAssign(IRGenFunction &IGF, Explosion &in,
                     Address dest) const override {
    IGF.emitUnownedAssign(in.claimNext(), dest,
                          asDerived().getReferenceCounting());
  }

  LoadedRef loadRefcountedPtr(IRGenFunction &IGF, SourceLoc loc,
                              Address addr) const override {
    llvm::Value *ptr =
      IGF.emitLoadRefcountedPtr(addr, asDerived().getReferenceCounting());
    return LoadedRef(ptr, true);
  }

  const WeakTypeInfo *createWeakStorageType(TypeConverter &TC) const override {
    return TC.createWeakStorageType(this->getStorageType(),
                                    asDerived().getReferenceCounting());
  }

  const TypeInfo *
  createUnownedStorageType(TypeConverter &TC) const override {
    return TC.createUnownedStorageType(this->getStorageType(),
                                       asDerived().getReferenceCounting());
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
