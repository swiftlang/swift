//===--- GenValueWitness.h - Swift IRGen for value witnesses ----*- C++ -*-===//
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
//  This file provides the private interface to the value witness emission
//  code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENVALUEWITNESS_H
#define SWIFT_IRGEN_GENVALUEWITNESS_H

#include "swift/Basic/LLVM.h"

namespace llvm {
  class Constant;
  class Type;
}

namespace swift {
  class CanType;

namespace irgen {
  class IRGenModule;

  /// True if a type has a generic-parameter-dependent value witness table.
  /// Currently, this is true if the size and/or alignment of the type is
  /// dependent on its generic parameters.
  bool hasDependentValueWitnessTable(IRGenModule &IGM, CanType ty);

  /// Emit a value-witness table for the given type, which is assumed
  /// to be non-dependent.
  llvm::Constant *emitValueWitnessTable(IRGenModule &IGM, CanType type);

  /// Emit the elements of a dependent value witness table template into a
  /// vector.
  void emitDependentValueWitnessTablePattern(IRGenModule &IGM,
                                    CanType abstractType,
                                    SmallVectorImpl<llvm::Constant*> &fields);

  /// Build a value witness that initializes an array front-to-back.
  void emitInitializeArrayFrontToBack(IRGenFunction &IGF,
                                      const TypeInfo &type,
                                      Address destArray,
                                      Address srcArray,
                                      llvm::Value *count,
                                      SILType T,
                                      IsTake_t take);

  /// Build a value witness that initializes an array back-to-front.
  void emitInitializeArrayBackToFront(IRGenFunction &IGF,
                                      const TypeInfo &type,
                                      Address destArray,
                                      Address srcArray,
                                      llvm::Value *count,
                                      SILType T,
                                      IsTake_t take);

}
}

#endif
