//===--- UnownedTypeInfo.h - Supplemental API for [unowned] -----*- C++ -*-===//
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
// This file defines UnownedTypeInfo, which supplements the FixedTypeInfo
// interface for types that implement [unowned] references.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_UNOWNEDTYPEINFO_H
#define SWIFT_IRGEN_UNOWNEDTYPEINFO_H

#include "LoadableTypeInfo.h"

namespace swift {
namespace irgen {

/// \brief An abstract class designed for use when implementing a
/// ReferenceStorageType with [unowned] ownership.
class UnownedTypeInfo : public LoadableTypeInfo {
protected:
  UnownedTypeInfo(llvm::Type *type, Size size,
                  const SpareBitVector &spareBits, Alignment align)
    : LoadableTypeInfo(type, size, spareBits, align, IsNotPOD, STIK_Unowned) {}

  UnownedTypeInfo(llvm::Type *type, Size size,
                  SpareBitVector &&spareBits, Alignment align)
    : LoadableTypeInfo(type, size, std::move(spareBits), align,
                       IsNotPOD, STIK_Unowned) {}

public:
  // No API yet.

  static bool classof(const UnownedTypeInfo *type) { return true; }
  static bool classof(const TypeInfo *type) {
    return type->getSpecialTypeInfoKind() == STIK_Unowned;
  }
};

}
}

#endif
