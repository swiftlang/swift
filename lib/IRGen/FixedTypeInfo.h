//===--- FixedTypeInfo.h - Convenience for fixed-layout types ---*- C++ -*-===//
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
// This file defines FixedTypeInfo, which is a convenient abstract
// implementation of TypeInfo for working with types that can be laid
// out statically.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FIXEDTYPEINFO_H
#define SWIFT_IRGEN_FIXEDTYPEINFO_H

#include "TypeInfo.h"

namespace swift {
namespace irgen {

/// FixedTypeInfo - An abstract class designed for use when
/// implementing a type that has a statically known layout.
class FixedTypeInfo : public TypeInfo {
protected:
  FixedTypeInfo(llvm::Type *type, Size size, Alignment align, IsPOD_t pod)
    : TypeInfo(type, size, align, pod) {}

public:
  OwnedAddress allocate(IRGenFunction &IGF, Initialization &init,
                        InitializedObject object,
                        OnHeap_t onHeap,
                        const llvm::Twine &name) const;

  // We can give these reasonable default implementations.

  void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr) const;

  void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr) const;

  // TODO: move the StorageSize etc. members here.
};

}
}

#endif
