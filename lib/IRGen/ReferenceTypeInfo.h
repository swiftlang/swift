//===--- ReferenceTypeInfo.h - Supplement for reference types ---*- C++ -*-===//
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
// This file defines ReferenceTypeInfo, which supplements the
// FixedTypeInfo interface for types with reference semantics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_REFERENCETYPEINFO_H
#define SWIFT_IRGEN_REFERENCETYPEINFO_H

#include "LoadableTypeInfo.h"

namespace swift {
namespace irgen {

class WeakTypeInfo;
class UnownedTypeInfo;
class TypeConverter;
  
/// \brief An abstract class designed for use when implementing a type
/// that has reference semantics.
class ReferenceTypeInfo : public LoadableTypeInfo {
protected:
  // FIXME: Get spare bits for pointers from a TargetInfo-like structure.
  ReferenceTypeInfo(llvm::Type *type, Size size, SpareBitVector spareBits,
                    Alignment align, IsPOD_t pod = IsNotPOD)
    : LoadableTypeInfo(type, size, spareBits, align, pod,
                       IsFixedSize, STIK_Reference)
  {}

public:
  /// Strongly retains a value.
  virtual void strongRetain(IRGenFunction &IGF, Explosion &in,
                            Atomicity atomicity) const = 0;

  /// Strongly releases a value.
  virtual void strongRelease(IRGenFunction &IGF, Explosion &in,
                             Atomicity atomicity) const = 0;

  /// Strongly retains a value that has come from a safe [unowned] reference.
  /// This operation is not supported for all reference types.
  virtual void strongRetainUnowned(IRGenFunction &IGF, Explosion &in) const = 0;

  /// Strongly retains a value that has come from a safe [unowned] reference.
  /// This operation is not supported for all reference types.
  virtual void strongRetainUnownedRelease(IRGenFunction &IGF,
                                          Explosion &in) const = 0;

  /// Weakly retains a value in the manner of a safe [unowned] reference.
  /// This operation is not supported for all reference types.
  virtual void unownedRetain(IRGenFunction &IGF, Explosion &in) const = 0;

  /// Weakly releases a value in the manner of a safe [unowned] reference.
  /// This operation is not supported for all reference types.
  virtual void unownedRelease(IRGenFunction &IGF, Explosion &in) const = 0;

  /// Load a reference from a safe [unowned] reference in memory and
  /// destroy the [unowned] location.
  virtual void unownedTakeStrong(IRGenFunction &IGF, Address addr,
                                 Explosion &out) const = 0;

  /// Load a reference from a safe [unowned] reference in memory.
  virtual void unownedLoadStrong(IRGenFunction &IGF, Address addr,
                                 Explosion &out) const = 0;

  /// Initialize a safe [unowned] reference in memory.
  virtual void unownedInit(IRGenFunction &IGF, Explosion &in,
                           Address dest) const = 0;

  /// Assign to an initialized safe [unowned] reference in memory.
  virtual void unownedAssign(IRGenFunction &IGF, Explosion &in,
                             Address dest) const = 0;

  /// Produce the storage information for [weak] storage.
  virtual const WeakTypeInfo *createWeakStorageType(TypeConverter &TC) const = 0;

  /// Produce the storage information for [unowned] storage.
  ///
  /// The reference-counting operations done by the value operations
  /// on the [unowned] storage type are assumed to be basically the
  /// same operations as weakRetain and weakRelease.
  virtual const TypeInfo *createUnownedStorageType(TypeConverter &TC)
    const = 0;

  /// Produce the storage information for @unowned(unsafe) storage.
  virtual const TypeInfo *createUnmanagedStorageType(TypeConverter &TC)
    const = 0;

  static bool classof(const ReferenceTypeInfo *type) { return true; }
  static bool classof(const TypeInfo *type) {
    return type->getSpecialTypeInfoKind() == STIK_Reference;
  }
};

}
}

#endif
