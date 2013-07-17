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

#include "FixedTypeInfo.h"

namespace swift {
namespace irgen {

/// \brief An abstract class designed for use when implementing a type
/// that has reference semantics.
class ReferenceTypeInfo : public FixedTypeInfo {
protected:
  ReferenceTypeInfo(llvm::Type *type, Size size, Alignment align)
    : FixedTypeInfo(type, size, align, IsNotPOD, IsReference) {}

public:
  /// Retains a value.
  virtual void retain(IRGenFunction &IGF, Explosion &explosion) const = 0;
  
  /// Releases a value.
  virtual void release(IRGenFunction &IGF, Explosion &explosion) const = 0;

  /// Produce the storage information for [weak] storage.
  virtual const TypeInfo *createWeakStorageType(TypeConverter &TC) const = 0;

  /// Produce the storage information for [unowned] storage.
  virtual const TypeInfo *createUnownedStorageType(TypeConverter &TC) const = 0;

  static bool classof(const ReferenceTypeInfo *type) { return true; }
  static bool classof(const TypeInfo *type) {
    return type->hasReferenceSemantics();
  }
};

}
}

#endif
