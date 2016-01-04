//===--- Devirtualizer.cpp - Devirtualize indirect calls  -----------------===//
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
// Devirtualize indirect calls to functions, turning them into direct function
// references.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-devirtualizer"

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

namespace {

class Devirtualizer : public SILFunctionTransform {

  bool devirtualizeAppliesInFunction(SILFunction &F,
                                     ClassHierarchyAnalysis *CHA);

  /// The entry point to the transformation.
  void run() override {
    SILFunction &F = *getFunction();
    ClassHierarchyAnalysis *CHA = PM->getAnalysis<ClassHierarchyAnalysis>();
    DEBUG(llvm::dbgs() << "***** Devirtualizer on function:" << F.getName()
                       << " *****\n");

    if (devirtualizeAppliesInFunction(F, CHA))
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
  }

  StringRef getName() override { return "Devirtualizer"; }
};

} // end anonymous namespace

bool Devirtualizer::devirtualizeAppliesInFunction(SILFunction &F,
                                                  ClassHierarchyAnalysis *CHA) {
  bool Changed = false;
  llvm::SmallVector<SILInstruction *, 8> DeadApplies;

  for (auto &BB : F) {
    for (auto It = BB.begin(), End = BB.end(); It != End;) {
      auto &I = *It++;

      // Skip non-apply instructions.

      auto Apply = FullApplySite::isa(&I);
      if (!Apply)
        continue;

      auto NewInstPair = tryDevirtualizeApply(Apply, CHA);
      if (!NewInstPair.second)
        continue;

      Changed = true;

      auto *CalleeFn = NewInstPair.second.getCalleeFunction();
      assert(CalleeFn && "Expected devirtualized callee!");

      // We may not have optimized these functions yet, and it could
      // be beneficial to rerun some earlier passes on the current
      // function now that we've made these direct references visible.
      if (CalleeFn->isDefinition() && CalleeFn->shouldOptimize())
        notifyPassManagerOfFunction(CalleeFn);

      auto *AI = Apply.getInstruction();
      if (!isa<TryApplyInst>(AI))
        AI->replaceAllUsesWith(NewInstPair.first);

      DeadApplies.push_back(AI);
    }
  }

  // Remove all the now-dead applies.
  while (!DeadApplies.empty()) {
    auto *AI = DeadApplies.pop_back_val();
    recursivelyDeleteTriviallyDeadInstructions(AI, true);
  }

  return Changed;
}

SILTransform *swift::createDevirtualizer() { return new Devirtualizer(); }
