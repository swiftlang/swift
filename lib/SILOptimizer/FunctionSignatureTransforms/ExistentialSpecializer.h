//===--- ExistentialSpecializer.h - Existential Specializer ----*- C++ -*-===//
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
// This contains utilities for transforming existentials.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_EXISTENTIALSPECIALIZER_H
#define SWIFT_SIL_EXISTENTIALSPECIALIZER_H
#include "FunctionSignatureOpts.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/Existential.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// A descriptor to carry information from ExistentialTransform analysis
/// to transformation.
struct ExistentialTransformArgumentDescriptor {
  OpenedExistentialAccess AccessType;
  bool DestroyAddrUse;
};

/// ExistentialSpecializer transformation class that creates a protocol
/// constrained generic and a thunk.
class ExistentialSpecializerTransform {
  /// The original function to analyze and transform.
  SILFunction *F;

  /// The newly created inner function.
  SILFunction *NewF;

  /// The function signature mangler we are using.
  Mangle::FunctionSignatureSpecializationMangler &Mangler;

  /// List of arguments and their descriptors to specialize
  llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
      &ExistentialArgDescriptor;

  /// Argument to Generic Type Map for NewF.
  llvm::SmallDenseMap<int, GenericTypeParamType *> Arg2GenericTypeMap;

  // Allocate the argument descriptors.
  llvm::SmallVector<ArgumentDescriptor, 4> &ArgumentDescList;

  /// Create the Devirtualized Inner Function.
  void createExistentialSpecializedFunction();

  /// Create a name for the inner function.
  std::string createExistentialSpecializedFunctionName();

  /// Create the new devirtualized protocol function signature.
  CanSILFunctionType createExistentialSpecializedFunctionType();

  /// Populate the body of NewF.
  void populateSpecializedGenericFunction();

  /// Create the thunk.
  void populateThunkBody();

public:
  /// Constructor.
  ExistentialSpecializerTransform(
      SILFunction *F, Mangle::FunctionSignatureSpecializationMangler &Mangler,
      llvm::SmallVector<ArgumentDescriptor, 4> &ADL,
      llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
          &ExistentialArgDescriptor)
      : F(F), NewF(nullptr), Mangler(Mangler),
        ExistentialArgDescriptor(ExistentialArgDescriptor),
        ArgumentDescList(ADL) {}

  /// Return the optimized iner function.
  SILFunction *getExistentialSpecializedFunction() { return NewF; }

  /// External function for the optimization.
  bool run() {
    createExistentialSpecializedFunction();
    return true;
  }
};

} // end namespace swift

#endif
