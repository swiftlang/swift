//===--- ScalarTypeInfo.h - Convenience class for scalar types --*- C++ -*-===//
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
// This file defines ScalarTypeInfo, which is a convenient abstract
// implementation of TypeInfo for working with types that are
// efficiently scalarizable.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_SCALARTYPEINFO_H
#define SWIFT_IRGEN_SCALARTYPEINFO_H

#include "EnumPayload.h"
#include "Explosion.h"
#include "TypeInfo.h"
#include "IRGenFunction.h"
#include "GenEnum.h"

namespace swift {
namespace irgen {

/// ScalarTypeInfo - An abstract class designed for use when
/// implementing a type which can be efficiently exploded and
/// unexploded.
template <class Derived, class Base>
class ScalarTypeInfo : public Base {
protected:
  template <class... T> ScalarTypeInfo(T &&...args)
    : Base(::std::forward<T>(args)...) {}

  const Derived &asDerived() const {
    return static_cast<const Derived &>(*this);
  }

public:
  void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                            Address dest, SILType T) const override {
    asDerived().Derived::initialize(IGF, params, dest);
  }

  void initializeWithCopy(IRGenFunction &IGF, Address dest, Address src,
                          SILType T) const override {
    Explosion temp;
    asDerived().Derived::loadAsCopy(IGF, src, temp);
    asDerived().Derived::initialize(IGF, temp, dest);
  }

  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const override {
    Explosion temp;
    asDerived().Derived::loadAsCopy(IGF, src, temp);
    asDerived().Derived::assign(IGF, temp, dest);
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const override {
    Explosion temp;
    asDerived().Derived::loadAsTake(IGF, src, temp);
    asDerived().Derived::assign(IGF, temp, dest);
  }

  void reexplode(IRGenFunction &IGF, Explosion &in,
                 Explosion &out) const override {
    unsigned size = asDerived().Derived::getExplosionSize();
    in.transferInto(out, size);
  }
};

/// SingleScalarTypeInfo - A further specialization of
/// ScalarTypeInfo for types which consist of a single scalar
/// which equals the storage type.
template <class Derived, class Base>
class SingleScalarTypeInfo : public ScalarTypeInfo<Derived, Base> {
protected:
  template <class... T> SingleScalarTypeInfo(T &&...args)
    : ScalarTypeInfo<Derived,Base>(::std::forward<T>(args)...) {}

  const Derived &asDerived() const {
    return static_cast<const Derived &>(*this);
  }

public:
  /// Return the type of the scalar.  Override this if it's not
  /// just the storage type.
  llvm::Type *getScalarType() const { return this->getStorageType(); }

  /// Project to the address of the scalar.  Override this if it's not
  /// just the storage type.
  Address projectScalar(IRGenFunction &IGF, Address addr) const { return addr; }

  // Subclasses must implement the following four operations:

  // Is the scalar POD?
  // static const bool IsScalarPOD;

  // Make the scalar +1.
  // void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const;

  // Make the scalar -1.
  // void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const;

  unsigned getExplosionSize() const override {
    return 1;
  }

  void getSchema(ExplosionSchema &schema) const override {
    llvm::Type *ty = asDerived().getScalarType();
    schema.add(ExplosionSchema::Element::forScalar(ty));
  }

  void initialize(IRGenFunction &IGF, Explosion &src,
                  Address addr) const override {
    addr = asDerived().projectScalar(IGF, addr);
    IGF.Builder.CreateStore(src.claimNext(), addr);
  }

  void loadAsCopy(IRGenFunction &IGF, Address addr,
                  Explosion &out) const override {
    addr = asDerived().projectScalar(IGF, addr);
    llvm::Value *value = IGF.Builder.CreateLoad(addr);
    asDerived().emitScalarRetain(IGF, value, IGF.getDefaultAtomicity());
    out.add(value);
  }

  void loadAsTake(IRGenFunction &IGF, Address addr,
                  Explosion &out) const override {
    addr = asDerived().projectScalar(IGF, addr);
    out.add(IGF.Builder.CreateLoad(addr));
  }

  void assign(IRGenFunction &IGF, Explosion &src, Address dest) const override {
    // Project down.
    dest = asDerived().projectScalar(IGF, dest);

    // Grab the old value if we need to.
    llvm::Value *oldValue = nullptr;
    if (!Derived::IsScalarPOD) {
      oldValue = IGF.Builder.CreateLoad(dest, "oldValue");
    }

    // Store.
    llvm::Value *newValue = src.claimNext();
    IGF.Builder.CreateStore(newValue, dest);

    // Release the old value if we need to.
    if (!Derived::IsScalarPOD) {
      asDerived().emitScalarRelease(IGF, oldValue, IGF.getDefaultAtomicity());
    }
  }

  void copy(IRGenFunction &IGF, Explosion &in, Explosion &out,
            Atomicity atomicity) const override {
    llvm::Value *value = in.claimNext();
    asDerived().emitScalarRetain(IGF, value, atomicity);
    out.add(value);
  }

  void consume(IRGenFunction &IGF, Explosion &in,
               Atomicity atomicity) const override {
    llvm::Value *value = in.claimNext();
    asDerived().emitScalarRelease(IGF, value, atomicity);
  }

  void fixLifetime(IRGenFunction &IGF, Explosion &in) const override {
    llvm::Value *value = in.claimNext();
    asDerived().emitScalarFixLifetime(IGF, value);
  }

  void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
    if (!Derived::IsScalarPOD) {
      addr = asDerived().projectScalar(IGF, addr);
      llvm::Value *value = IGF.Builder.CreateLoad(addr, "toDestroy");
      asDerived().emitScalarRelease(IGF, value, IGF.getDefaultAtomicity());
    }
  }
  
  void packIntoEnumPayload(IRGenFunction &IGF,
                           EnumPayload &payload,
                           Explosion &src,
                           unsigned offset) const override {
    payload.insertValue(IGF, src.claimNext(), offset);
  }
  
  void unpackFromEnumPayload(IRGenFunction &IGF,
                             const EnumPayload &payload,
                             Explosion &dest,
                             unsigned offset) const override {
    dest.add(payload.extractValue(IGF, asDerived().getScalarType(), offset));
  }

  void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                        Size offset) const override {
    // Can't use getFixedSize because it returns the alloc size not the store
    // size.
    LoadableTypeInfo::addScalarToAggLowering(
        IGM, lowering, asDerived().getScalarType(), offset,
        Size(IGM.DataLayout.getTypeStoreSize(asDerived().getScalarType())));
  }
};

/// PODSingleScalarTypeInfo - A further specialization of
/// SingleScalarTypeInfo for types which consist of a single POD
/// scalar.  This is a complete implementation.
template <class Derived, class Base>
class PODSingleScalarTypeInfo : public SingleScalarTypeInfo<Derived, Base> {
protected:
  template <class StorageType, class... T> 
  PODSingleScalarTypeInfo(StorageType *storage, Size size,
                          SpareBitVector spareBits,
                          Alignment align, T &&...args)
    : SingleScalarTypeInfo<Derived, Base>(storage, size, spareBits, align,
                                          IsPOD, IsFixedSize,
                                          ::std::forward<T>(args)...) {}

private:
  friend class SingleScalarTypeInfo<Derived, Base>;
  static const bool IsScalarPOD = true;

  void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value,
                        Atomicity atomicity) const {}

  void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value,
                         Atomicity atomicity) const {}

  void emitScalarFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
  }
};

}
}

#endif
