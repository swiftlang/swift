//===- Generics.cpp ---- Utilities for transforming generics ----*- C++ -*-===//
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

#define DEBUG_TYPE "generic-specializer-utility"

#include "swift/SILPasses/Utils/Generics.h"

#include "swift/SILPasses/Utils/GenericCloner.h"

using namespace swift;

bool swift::trySpecializeApplyOfGeneric(ApplySite Apply,
                                        SILFunction **NewFunction) {
  if (NewFunction)
    *NewFunction = nullptr;

  assert(Apply.hasSubstitutions() && "Expected an apply with substitutions!");

  auto *F = cast<FunctionRefInst>(Apply.getCallee())->getReferencedFunction();
  assert(F->isDefinition() && "Expected definition to specialize!");

  DEBUG(llvm::dbgs() << "        ApplyInst: " << *Apply.getInstruction());

  // Create the substitution maps.
  TypeSubstitutionMap InterfaceSubs
    = F->getLoweredFunctionType()->getGenericSignature()
    ->getSubstitutionMap(Apply.getSubstitutions());

  TypeSubstitutionMap ContextSubs
    = F->getContextGenericParams()
    ->getSubstitutionMap(Apply.getSubstitutions());

  // We do not support partial specialization.
  if (hasUnboundGenericTypes(InterfaceSubs)) {
    DEBUG(llvm::dbgs() << "    Can not specialize with interface subs.\n");
    return false;
  }

  llvm::SmallString<64> ClonedName;
  {
    llvm::raw_svector_ostream buffer(ClonedName);
    ArrayRef<Substitution> Subs = Apply.getSubstitutions();
    Mangle::Mangler M(buffer);
    Mangle::GenericSpecializationMangler Mangler(M, F, Subs);
    Mangler.mangle();
  }

  SILFunction *NewF;
  auto &M = Apply.getInstruction()->getModule();
  // If we already have this specialization, reuse it.
  if (auto PrevF = M.lookUpFunction(ClonedName)) {
    NewF = PrevF;

#ifndef NDEBUG
    // Make sure that NewF's subst type matches the expected type.
    auto Subs = Apply.getSubstitutions();
    auto FTy =
      F->getLoweredFunctionType()->substGenericArgs(M,
                                                    M.getSwiftModule(),
                                                    Subs);
    assert(FTy == NewF->getLoweredFunctionType() &&
           "Previously specialized function does not match expected type.");
#endif
  } else {
    // Create a new function.
    NewF = GenericCloner::cloneFunction(F, InterfaceSubs, ContextSubs,
                                        ClonedName, Apply);
    if (NewFunction)
      *NewFunction = NewF;
  }

  // Replace all of the Apply functions with the new function.
  replaceWithSpecializedFunction(Apply, NewF);
  return true;
}
