//===--- WeakTypeInfo.h - Supplemental API for [weak] types -----*- C++ -*-===//
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
// This file defines WeakTypeInfo, which supplements the FixedTypeInfo
// interface for types that implement [weak] references.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_WEAKTYPEINFO_H
#define SWIFT_IRGEN_WEAKTYPEINFO_H

#include "FixedTypeInfo.h"

namespace swift {
namespace irgen {

/// \brief An abstract class designed for use when implementing a
/// ReferenceStorageType with [weak] ownership.
class WeakTypeInfo : public FixedTypeInfo {
protected:
  WeakTypeInfo(llvm::Type *type, Size size, Alignment align,
               const SpareBitVector &spareBits)
    : FixedTypeInfo(type, size, spareBits, align, IsNotPOD,
                    IsNotBitwiseTakable, STIK_Weak) {}

  WeakTypeInfo(llvm::Type *type, Size size, Alignment align,
               SpareBitVector &&spareBits)
    : FixedTypeInfo(type, size, std::move(spareBits), align, IsNotPOD,
                    IsNotBitwiseTakable, STIK_Weak) {}

public:
  virtual void weakLoadStrong(IRGenFunction &IGF, Address addr,
                              Explosion &out) const = 0;
  virtual void weakTakeStrong(IRGenFunction &IGF, Address addr,
                              Explosion &out) const = 0;
  virtual void weakInit(IRGenFunction &IGF, Explosion &src,
                        Address dest) const = 0;
  virtual void weakAssign(IRGenFunction &IGF, Explosion &src,
                          Address dest) const = 0;

  static bool classof(const WeakTypeInfo *type) { return true; }
  static bool classof(const TypeInfo *type) {
    return type->getSpecialTypeInfoKind() == STIK_Weak;
  }
};

}
}

#endif
