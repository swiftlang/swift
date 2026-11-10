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
  class ClassDecl;

namespace irgen {
  class ConstantStructBuilder;
  class IRGenModule;
  class ConstantReference;

  /// True if a type has a generic-parameter-dependent value witness table.
  /// Currently, this is true if the size and/or alignment of the type is
  /// dependent on its generic parameters.
  bool hasDependentValueWitnessTable(IRGenModule &IGM, NominalTypeDecl *decl);

  /// Emit a value-witness table for the given type.
  ///
  /// \param isPattern - true if the table just serves as an instantiation
  ///   pattern and does not need to be modifiable in-place (if the type
  ///   does not have static layout for some reason)
  ConstantReference emitValueWitnessTable(IRGenModule &IGM, CanType type,
                                          bool isPattern,
                                          bool relativeReference);

  SILType getLoweredTypeInPrimaryContext(IRGenModule &IGM,
                                         NominalTypeDecl *type);

  /// The value witness table, with the class allocator witness (see
  /// TargetClassAllocatorValueWitnessTable), of \p cls, a class with a class
  /// allocator whose metadata this module emits; or null if \p cls has no
  /// class allocator, and its value witness table is Builtin.NativeObject's
  /// or Builtin.UnknownObject's. Classes with the same allocator share a
  /// table, which each image has one copy of.
  llvm::Constant *getClassAllocatorValueWitnessTable(IRGenModule &IGM,
                                                     ClassDecl *cls);

  bool layoutStringsEnabled(IRGenModule &IGM, bool diagnose = false);
}
}

#endif
