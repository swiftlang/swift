//===--- ScalarTypeInfo.h - Convenience class for scalar types --*- C++ -*-===//
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
// This file defines ScalarTypeInfo, which is a convenient abstract
// implementation of TypeInfo for working with types that are
// efficiently scalarizable.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_SCALARTYPEINFO_H
#define SWIFT_IRGEN_SCALARTYPEINFO_H

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
  /// Efficiently-scalarizable types are always passed directly.
  bool isIndirectArgument() const override { return false; }

  void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                            Address dest, SILType T) const override {
    asDerived().Derived::initialize(IGF, params, dest);
  }

  void initializeWithCopy(IRGenFunction &IGF, Address dest, Address src,
                          SILType T) const {
    Explosion temp;
    asDerived().Derived::loadAsCopy(IGF, src, temp);
    asDerived().Derived::initialize(IGF, temp, dest);
  }

  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const {
    Explosion temp;
    asDerived().Derived::loadAsCopy(IGF, src, temp);
    asDerived().Derived::assign(IGF, temp, dest);
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const {
    Explosion temp;
    asDerived().Derived::loadAsTake(IGF, src, temp);
    asDerived().Derived::assign(IGF, temp, dest);
  }

  void reexplode(IRGenFunction &IGF, Explosion &in, Explosion &out) const {
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

  unsigned getExplosionSize() const {
    return 1;
  }

  void getSchema(ExplosionSchema &schema) const {
    llvm::Type *ty = asDerived().getScalarType();
    schema.add(ExplosionSchema::Element::forScalar(ty));
  }

  void initialize(IRGenFunction &IGF, Explosion &src, Address addr) const {
    addr = asDerived().projectScalar(IGF, addr);
    IGF.Builder.CreateStore(src.claimNext(), addr);
  }

  void loadAsCopy(IRGenFunction &IGF, Address addr, Explosion &out) const {
    addr = asDerived().projectScalar(IGF, addr);
    llvm::Value *value = IGF.Builder.CreateLoad(addr);
    asDerived().emitScalarRetain(IGF, value);
    out.add(value);
  }

  void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &out) const {
    addr = asDerived().projectScalar(IGF, addr);
    out.add(IGF.Builder.CreateLoad(addr));
  }

  void assign(IRGenFunction &IGF, Explosion &src, Address dest) const {
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
      asDerived().emitScalarRelease(IGF, oldValue);
    }
  }

  void copy(IRGenFunction &IGF, Explosion &in, Explosion &out) const {
    llvm::Value *value = in.claimNext();
    asDerived().emitScalarRetain(IGF, value);
    out.add(value);
  }
  
  void consume(IRGenFunction &IGF, Explosion &in) const {
    llvm::Value *value = in.claimNext();
    asDerived().emitScalarRelease(IGF, value);
  }

  void fixLifetime(IRGenFunction &IGF, Explosion &in) const {
    llvm::Value *value = in.claimNext();
    asDerived().emitScalarFixLifetime(IGF, value);
  }
  
  void destroy(IRGenFunction &IGF, Address addr, SILType T) const {
    if (!Derived::IsScalarPOD) {
      addr = asDerived().projectScalar(IGF, addr);
      llvm::Value *value = IGF.Builder.CreateLoad(addr, "toDestroy");
      asDerived().emitScalarRelease(IGF, value);
    }
  }
  
  llvm::Value *packEnumPayload(IRGenFunction &IGF,
                                Explosion &src,
                                unsigned bitWidth,
                                unsigned offset) const override {
    PackEnumPayload pack(IGF, bitWidth);
    pack.addAtOffset(src.claimNext(), offset);
    return pack.get();
  }
  
  void unpackEnumPayload(IRGenFunction &IGF,
                          llvm::Value *payload,
                          Explosion &dest,
                          unsigned offset) const override {
    UnpackEnumPayload unpack(IGF, payload);
    dest.add(unpack.claimAtOffset(asDerived().getScalarType(), offset));
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
                          llvm::BitVector spareBits,
                          Alignment align, T &&...args)
    : SingleScalarTypeInfo<Derived, Base>(storage, size, spareBits, align, IsPOD,
                                          ::std::forward<T>(args)...) {}

private:
  friend class SingleScalarTypeInfo<Derived, Base>;
  static const bool IsScalarPOD = true;

  void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const {
  }

  void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const {
  }

  void emitScalarFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
  }
};

}
}

#endif
