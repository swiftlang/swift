//===--- HeapTypeInfo.h - Utilities for reference-counted types -*- C++ -*-===//
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
// This file defines some routines that are useful for emitting
// types that are single, reference-counted pointers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_HEAPTYPEINFO_H
#define SWIFT_IRGEN_HEAPTYPEINFO_H

#include "llvm/DerivedTypes.h"
#include "FixedTypeInfo.h"
#include "ScalarTypeInfo.h"

namespace swift {
namespace irgen {

/// HeapTypeInfo - A type designed for use implementing a type
/// which consists solely of something reference-counted.
template <class Impl>
class HeapTypeInfo : public SingleScalarTypeInfo<Impl, FixedTypeInfo> {
  typedef SingleScalarTypeInfo<Impl, FixedTypeInfo> super;
protected:
  using super::asDerived;
public:
  HeapTypeInfo(llvm::PointerType *storage, Size size, Alignment align)
    : super(storage, size, align, IsNotPOD) {}

  bool isSingleRetainablePointer(ResilienceScope scope) const {
    return asDerived().isKnownSwift();
  }

  static const bool IsScalarPOD = false;

  void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const {
    if (asDerived().isKnownSwift()) {
      IGF.emitRelease(value);
    } else {
      IGF.emitObjCRelease(value);
    }
  }

  void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const {
    if (asDerived().isKnownSwift()) {
      IGF.emitRetainCall(value);
    } else {
      IGF.emitObjCRetainCall(value);
    }
  }

  void enterScalarCleanup(IRGenFunction &IGF, llvm::Value *value,
                          Explosion &out) const {
    if (asDerived().isKnownSwift()) {
      out.add(IGF.enterReleaseCleanup(value));
    } else {
      out.add(IGF.enterObjCReleaseCleanup(value));
    }
  }
};

}
}

#endif


