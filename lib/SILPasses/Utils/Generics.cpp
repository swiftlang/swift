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

// Create a new apply based on an old one, but with a different
// function being applied.
static ApplySite replaceWithSpecializedFunction(ApplySite AI,
                                                SILFunction *NewF) {
  SILLocation Loc = AI.getLoc();
  ArrayRef<Substitution> Subst;

  SmallVector<SILValue, 4> Arguments;
  for (auto &Op : AI.getArgumentOperands()) {
    Arguments.push_back(Op.get());
  }

  SILBuilderWithScope<2> Builder(AI.getInstruction());
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    return Builder.createTryApply(Loc, FRI, TAI->getSubstCalleeSILType(),
                                  {}, Arguments, TAI->getNormalBB(),
                                  TAI->getErrorBB());

  if (isa<ApplyInst>(AI))
    return Builder.createApply(Loc, FRI, Arguments);

  if (auto *PAI = dyn_cast<PartialApplyInst>(AI))
      return Builder.createPartialApply(Loc, FRI,
                                        PAI->getSubstCalleeSILType(),
                                        {},
                                        Arguments,
                                        PAI->getType());

  llvm_unreachable("unhandled kind of apply");
}

ApplySite swift::trySpecializeApplyOfGeneric(ApplySite Apply,
                                             SILFunction *&NewFunction,
         llvm::SmallVectorImpl<FullApplyCollector::value_type> &NewApplyPairs) {
  NewFunction = nullptr;

  assert(NewApplyPairs.empty() && "Expected no new applies in vector yet!");
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
    return ApplySite();
  }

  // If there are any incomplete conformances, we cannot specialize.
  // FIXME: Semantic analysis should ensure that this never happens.
  for (auto sub : Apply.getSubstitutions()) {
    for (auto conformance : sub.getConformances()) {
      if (conformance && conformance->isIncomplete())
        return ApplySite();
    }
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
    FullApplyCollector Collector;

    // Create a new function.
    NewF = GenericCloner::cloneFunction(F, InterfaceSubs, ContextSubs,
                                        ClonedName, Apply,
                                        Collector.getCallback());
    for (auto &P : Collector.getApplyPairs())
      NewApplyPairs.push_back(P);

    NewFunction = NewF;
  }

  return replaceWithSpecializedFunction(Apply, NewF);
}
