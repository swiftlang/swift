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
#include "GenType.h"

namespace swift {
namespace irgen {

/// HeapTypeInfo - A type designed for use implementing a type
/// which consists solely of something reference-counted.
class HeapTypeInfo : public TypeInfo {
public:
  HeapTypeInfo(llvm::PointerType *storage, Size size, Alignment align)
    : TypeInfo(storage, size, align, IsNotPOD) {}

  bool isSingleRetainablePointer(ResilienceScope scope) const;
  unsigned getExplosionSize(ExplosionKind kind) const;
  void getSchema(ExplosionSchema &schema) const;
  void load(IRGenFunction &IGF, Address addr, Explosion &e) const;
  void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const;
  void assign(IRGenFunction &IGF, Explosion &e, Address addr) const;
  void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const;
  void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const;
  void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const;
  void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const;
  void destroy(IRGenFunction &IGF, Address addr) const;
};

}
}

#endif


