//===--- TypeInfo.h - Abstract primitive operations on values ---*- C++ -*-===//
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
// This file defines the interface used to perform primitive
// operations on swift values and objects.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_TYPEINFO_H
#define SWIFT_IRGEN_TYPEINFO_H

#include "IRGen.h"

namespace llvm {
  class Constant;
  class Twine;
  class Type;
}

namespace swift {
namespace irgen {
  class Address;
  class IRGenFunction;
  class IRGenModule;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class ExplosionSchema;
  enum OnHeap_t : unsigned char;
  class OwnedAddress;
  class RValue;
  class RValueSchema;

/// Information about the IR representation and generation of the
/// given type.
class TypeInfo {
  TypeInfo(const TypeInfo &) = delete;
  TypeInfo &operator=(const TypeInfo &) = delete;

  friend class TypeConverter;
  mutable const TypeInfo *NextConverted;

protected:
  TypeInfo(llvm::Type *Type, Alignment A, IsPOD_t pod, IsFixedSize_t fixed)
    : NextConverted(0), StorageType(Type), StorageAlignment(A),
      POD(pod), Fixed(fixed) {}

  /// Change the minimum alignment of a stored value of this type.
  void setStorageAlignment(Alignment alignment) {
    StorageAlignment = alignment;
  }

public:
  virtual ~TypeInfo() = default;

  /// Unsafely cast this to the given subtype.
  template <class T> const T &as() const {
    // FIXME: maybe do an assert somehow if we have RTTI enabled.
    return static_cast<const T &>(*this);
  }

  /// The LLVM representation of a stored value of this type.  For
  /// non-fixed types, this is really useful only for forming pointers to it.
  llvm::Type *StorageType;

private:
  /// The storage alignment of this type in bytes.  This is never zero
  /// for a completely-converted type.
  Alignment StorageAlignment;

  /// Whether this type is known to be POD.
  unsigned POD : 1;

  /// Whether this type is known to be fixed in size.
  unsigned Fixed : 1;

public:
  /// Sets whether this type is POD.  Should only be called during
  /// completion of a forward-declaration.
  void setPOD(IsPOD_t isPOD) { POD = unsigned(isPOD); }

  /// Whether this type info has been completely converted.
  bool isComplete() const { return !StorageAlignment.isZero(); }

  /// Whether this type is known to be empty.
  bool isKnownEmpty() const;

  /// Whether this type is known to be POD, i.e. to not require any
  /// particular action on copy or destroy.
  IsPOD_t isPOD(ResilienceScope scope) const { return IsPOD_t(POD); }

  /// Whether this type is known to be fixed-size in the local
  /// resilience domain.  If true, this TypeInfo can be cast to
  /// FixedTypeInfo.
  IsFixedSize_t isFixedSize() const {
    return IsFixedSize_t(Fixed);
  }

  llvm::Type *getStorageType() const { return StorageType; }

  Alignment getBestKnownAlignment() const {
    return StorageAlignment;
  }

  /// Given a generic pointer to this type, produce an Address for it.
  Address getAddressForPointer(llvm::Value *ptr) const;

  /// Produce an undefined pointer to an object of this type.
  Address getUndefAddress() const;
    
  /// Return the size and alignment of this type.
  virtual std::pair<llvm::Value*,llvm::Value*>
    getSizeAndAlignment(IRGenFunction &IGF) const = 0;
  virtual llvm::Value *getSize(IRGenFunction &IGF) const = 0;
  virtual llvm::Value *getAlignment(IRGenFunction &IGF) const = 0;
  virtual llvm::Value *getStride(IRGenFunction &IGF) const = 0;

  /// Return the statically-known size of this type, or null if it is
  /// not known.
  virtual llvm::Constant *getStaticSize(IRGenModule &IGM) const = 0;

  /// Return the statically-known alignment of this type, or null if
  /// it is not known.
  virtual llvm::Constant *getStaticAlignment(IRGenModule &IGM) const = 0;

  /// Return the statically-known stride size of this type, or null if
  /// it is not known.
  virtual llvm::Constant *getStaticStride(IRGenModule &IGM) const = 0;

  /// Add the information for exploding values of this type to the
  /// given schema.
  virtual void getSchema(ExplosionSchema &schema) const = 0;

  /// A convenience for getting the schema of a single type.
  ExplosionSchema getSchema(ExplosionKind kind) const;

  /// Return the number of elements in an explosion of this type.
  virtual unsigned getExplosionSize(ExplosionKind kind) const = 0;

  /// Allocate a variable of this type.
  virtual OwnedAddress allocate(IRGenFunction &IGF, OnHeap_t onHeap,
                                const llvm::Twine &name) const = 0;

  /// Load an explosion of values from an address.
  virtual void load(IRGenFunction &IGF, Address addr,
                    Explosion &explosion) const = 0;

  /// Perform a 'take' load of the given address.  This is like a C++
  /// move-initialization, except that the object at the old address
  /// will not be destroyed.
  virtual void loadAsTake(IRGenFunction &IGF, Address addr,
                          Explosion &explosion) const = 0;

  /// Assign a set of exploded values into an address.  The values are
  /// consumed out of the explosion.
  virtual void assign(IRGenFunction &IGF, Explosion &explosion,
                      Address addr) const = 0;

  /// Copy a value out of an object and into another, destroying the
  /// old value in the destination.
  virtual void assignWithCopy(IRGenFunction &IGF, Address dest,
                              Address src) const = 0;

  /// Move a value out of an object and into another, destroying the
  /// old value there and leaving the source object in an invalid state.
  virtual void assignWithTake(IRGenFunction &IGF, Address dest,
                              Address src) const = 0;

  /// Initialize an address by consuming values out of an explosion.
  virtual void initialize(IRGenFunction &IGF, Explosion &explosion,
                          Address addr) const = 0;

  /// Perform a "take-initialization" from the given object.  A
  /// take-initialization is like a C++ move-initialization, except that
  /// the old object is actually no longer permitted to be destroyed.
  virtual void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                                  Address srcAddr) const = 0;

  /// Perform a copy-initialization from the given object.
  virtual void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                                  Address srcAddr) const = 0;

  /// Consume a bunch of values which have exploded at one explosion
  /// level and produce them at another.
  virtual void reexplode(IRGenFunction &IGF, Explosion &sourceExplosion,
                         Explosion &targetExplosion) const = 0;

  /// Copy a value into a new explosion without touching any of the
  /// existing cleanups.  This operation may also shift explosion levels.
  ///
  /// This operation is useful when an explosion is being used
  /// multiple times, for example in when doing load-modify-store
  /// l-value operations.
  virtual void copy(IRGenFunction &IGF, Explosion &sourceExplosion,
                    Explosion &targetExplosion) const = 0;

  /// Destroy an object of this type in memory.
  virtual void destroy(IRGenFunction &IGF, Address address) const = 0;
  
  /// Retains a value. Does not affect cleanups.
  virtual void retain(IRGenFunction &IGF, Explosion &explosion) const = 0;
  
  /// Releases a value. Does not affect cleanups.
  virtual void release(IRGenFunction &IGF, Explosion &explosion) const = 0;

  /// Should optimizations be enabled which rely on the representation
  /// for this type being a single retainable object pointer?
  ///
  /// \return false by default
  virtual bool isSingleRetainablePointer(ResilienceScope scope) const;
};

} // end namespace irgen
} // end namespace swift

#endif
