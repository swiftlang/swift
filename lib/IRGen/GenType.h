//===--- GenType.h - Auxiliary Interface for Type IR Generation -*- C++ -*-===//
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
// This file defines the interface used 
// the AST into LLVM IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENTYPE_H
#define SWIFT_IRGEN_GENTYPE_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"

#include "IRGen.h"

namespace llvm {
  class Type;
}

namespace swift {
  class ArrayType;
  class FunctionType;
  class LValueType;
  class OneOfType;
  class TupleType;
  class Type;
  class TypeBase;

namespace irgen {
  class IRGenFunction;
  class IRGenModule;
  class Address;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class ExplosionSchema;
  class RValue;
  class RValueSchema;

/// Information about the IR representation and generation of the
/// given type.
class TypeInfo {
  TypeInfo(const TypeInfo &) = delete;
  TypeInfo &operator=(const TypeInfo &) = delete;

  friend class TypeConverter;
  mutable const TypeInfo *NextConverted;

public:
  TypeInfo(llvm::Type *Type, Size S, Alignment A)
    : NextConverted(0), StorageType(Type), StorageSize(S),
      StorageAlignment(A) {}

  virtual ~TypeInfo() {}

  /// Unsafely cast this to the given subtype.
  template <class T> const T &as() const {
    // FIXME: maybe do an assert somehow if we have RTTI enabled.
    return static_cast<const T &>(*this);
  }

  /// The LLVM representation of a stored value of this type.
  llvm::Type *StorageType;

  /// The storage size of this type in bytes.  This may be zero even
  /// for well-formed and complete types, such as a trivial oneof or
  /// tuple.
  Size StorageSize;

  /// The storage alignment of this type in bytes.  This is never zero
  /// for a completely-converted type.
  Alignment StorageAlignment;

  /// Whether this type info has been completely converted.
  bool isComplete() const { return !StorageAlignment.isZero(); }

  /// Whether this type is known to be empty within the given
  /// resilience scope.
  bool isEmpty(ResilienceScope Scope) const { return StorageSize.isZero(); }

  llvm::Type *getStorageType() const { return StorageType; }

  /// Add the information for exploding values of this type to the
  /// given schema.
  virtual void getSchema(ExplosionSchema &schema) const = 0;

  /// A convenience for getting the schema of a single type.
  ExplosionSchema getSchema(ExplosionKind kind) const;

  /// Return the number of elements in an explosion of this type.
  virtual unsigned getExplosionSize(ExplosionKind kind) const = 0;

  /// Load a list of exploded values from an address.
  virtual void load(IRGenFunction &IGF, Address addr,
                    Explosion &explosion) const = 0;

  /// Assign a set of exploded values into an address.  The values are
  /// consumed out of the explosion.
  virtual void assign(IRGenFunction &IGF, Explosion &explosion,
                      Address addr) const = 0;

  /// Initialize an address by consuming values out of an explosion.
  virtual void initialize(IRGenFunction &IGF, Explosion &explosion,
                          Address addr) const = 0;

  /// Perform a "take-initialization" from the given object.  A
  /// take-initialization is like a C++ move-initialization, except that
  /// the old object is actually no longer permitted to be destroyed.
  void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr) const;

  /// Consume a bunch of values which have exploded at one explosion
  /// level and produce them at another.
  virtual void reexplode(IRGenFunction &IGF, Explosion &sourceExplosion,
                         Explosion &targetExplosion) const = 0;

  /// Should optimizations be enabled which rely on the representation
  /// for this type being a single retainable object pointer?
  ///
  /// \return false by default
  virtual bool isSingleRetainablePointer(ResilienceScope scope) const;
};

/// The helper class for generating types.
class TypeConverter {
  llvm::DenseMap<TypeBase*, const TypeInfo*> Converted;
  const TypeInfo *FirstConverted;

  static const TypeInfo *createPrimitive(llvm::Type *T,
                                         Size size, Alignment align);
    
  static const TypeInfo *convertType(IRGenModule &IGM, Type T);
  static const TypeInfo *convertTupleType(IRGenModule &IGM, TupleType *T);
  static const TypeInfo *convertOneOfType(IRGenModule &IGM, OneOfType *T);
  static const TypeInfo *convertFunctionType(IRGenModule &IGM, FunctionType *T);
  static const TypeInfo *convertArrayType(IRGenModule &IGM, ArrayType *T);
  static const TypeInfo *convertLValueType(IRGenModule &IGM, LValueType *T);
  static const TypeInfo *convertBuiltinObjectPointer(IRGenModule &IGM);

 public:
  TypeConverter();
  ~TypeConverter();
  static const TypeInfo &getFragileTypeInfo(IRGenModule &IGM, Type T);
};

} // end namespace irgen
} // end namespace swift

#endif
