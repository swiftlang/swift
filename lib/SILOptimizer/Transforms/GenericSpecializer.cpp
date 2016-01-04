//===--- GenericSpecializer.cpp - Specialization of generic functions -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Specialize calls to generic functions by substituting static type
// information.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-generic-specializer"

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

namespace {

class GenericSpecializer : public SILFunctionTransform {

  bool specializeAppliesInFunction(SILFunction &F);

  /// The entry point to the transformation.
  void run() override {
    SILFunction &F = *getFunction();
    DEBUG(llvm::dbgs() << "***** GenericSpecializer on function:" << F.getName()
                       << " *****\n");

    if (specializeAppliesInFunction(F))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
  }

  StringRef getName() override { return "Generic Specializer"; }
};

} // end anonymous namespace

bool GenericSpecializer::specializeAppliesInFunction(SILFunction &F) {
  bool Changed = false;
  llvm::SmallVector<SILInstruction *, 8> DeadApplies;

  for (auto &BB : F) {
    for (auto It = BB.begin(), End = BB.end(); It != End;) {
      auto &I = *It++;

      // Skip non-apply instructions, apply instructions with no
      // substitutions, apply instructions where we do not statically
      // know the called function, and apply instructions where we do
      // not have the body of the called function.

      ApplySite Apply = ApplySite::isa(&I);
      if (!Apply || !Apply.hasSubstitutions())
        continue;

      auto *Callee = Apply.getCalleeFunction();
      if (!Callee || !Callee->isDefinition())
        continue;

      // We have a call that can potentially be specialized, so
      // attempt to do so.

      // The specializer helper function currently expects a collector
      // argument, but we aren't going to make use of the results so
      // we'll have our filter always return false;
      auto Filter = [](SILInstruction *I) -> bool { return false; };
      CloneCollector Collector(Filter);

      SILFunction *SpecializedFunction;

      auto Specialized =
          trySpecializeApplyOfGeneric(Apply, SpecializedFunction, Collector);

      if (Specialized) {
        Changed = true;

        // If calling the specialization utility resulted in a new
        // function (as opposed to returning a previous
        // specialization), we need to notify the pass manager so that
        // the new function gets optimized.
        if (SpecializedFunction)
          notifyPassManagerOfFunction(SpecializedFunction);

        auto *AI = Apply.getInstruction();

        if (!isa<TryApplyInst>(AI))
          AI->replaceAllUsesWith(Specialized.getInstruction());

        DeadApplies.push_back(AI);
      }
    }
  }

  // Remove all the now-dead applies.
  while (!DeadApplies.empty()) {
    auto *AI = DeadApplies.pop_back_val();
    recursivelyDeleteTriviallyDeadInstructions(AI, true);
  }

  return Changed;
}

SILTransform *swift::createGenericSpecializer() {
  return new GenericSpecializer();
}
