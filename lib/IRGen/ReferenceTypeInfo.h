//===--- ReferenceTypeInfo.h - Supplement for reference types ---*- C++ -*-===//
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
                    Alignment align)
    : LoadableTypeInfo(type, size, spareBits, align, IsNotPOD, STIK_Reference)
  {}

public:
  /// Strongly retains a value.
  virtual void retain(IRGenFunction &IGF, Explosion &explosion) const = 0;
  
  /// Strongly releases a value.
  virtual void release(IRGenFunction &IGF, Explosion &explosion) const = 0;

  /// Strongly retains a value that has come from a safe [unowned] reference.
  virtual void retainUnowned(IRGenFunction &IGF, Explosion &in) const = 0;

  /// Weakly retains a value in the manner of a safe [unowned] reference.
  virtual void unownedRetain(IRGenFunction &IGF, Explosion &in) const = 0;

  /// Weakly releases a value in the manner of a safe [unowned] reference.
  virtual void unownedRelease(IRGenFunction &IGF, Explosion &in) const = 0;

  /// Produce the storage information for [weak] storage.
  virtual const WeakTypeInfo *createWeakStorageType(TypeConverter &TC) const = 0;

  /// Produce the storage information for [unowned] storage.
  ///
  /// The reference-counting operations done by the value operations
  /// on the [unowned] storage type are assumed to be basically the
  /// same operations as weakRetain and weakRelease.
  virtual const UnownedTypeInfo *createUnownedStorageType(TypeConverter &TC)
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
