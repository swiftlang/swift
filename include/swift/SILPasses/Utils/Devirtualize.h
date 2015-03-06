//===-- Devirtualize.h - Helper for devirtualizing apply --------*- C++ -*-===//
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
// This contains helper functions that perform the work of devirtualizing a
// given apply when possible.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DEVIRTUALIZE_H
#define SWIFT_SIL_DEVIRTUALIZE_H

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {

/// A helper struct to keep information collected by
/// the analysis which checks if it is possible to
/// devirtualize a given class_method.
struct DevirtClassMethodInfo {
  SILFunctionType::ParameterSILTypeArrayRef ParamTypes;
  SILFunction *F;
  CanSILFunctionType SubstCalleeType;
  ArrayRef<Substitution> Substitutions;

  DevirtClassMethodInfo() : ParamTypes({}), F(nullptr) {}
};

ApplyInst *devirtualizeApply(ApplyInst *AI);
bool isClassWithUnboundGenericParameters(SILType C, SILModule &M);
bool canDevirtualizeClassMethod(ApplyInst *AI,
                                SILType ClassInstanceType, ClassDecl *CD,
                                DevirtClassMethodInfo& DCMI);
ApplyInst *devirtualizeClassMethod(ApplyInst *AI,
                                   SILValue ClassInstance,
                                   DevirtClassMethodInfo& DCMI);
ApplyInst *devirtualizeClassMethod(ApplyInst *AI,
                                   SILValue ClassInstance, ClassDecl *CD);

}

#endif
