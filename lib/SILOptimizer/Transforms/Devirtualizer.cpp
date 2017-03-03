//===--- Devirtualizer.cpp - Devirtualize indirect calls  -----------------===//
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
  llvm::SmallVector<ApplySite, 8> NewApplies;

  SmallVector<ApplySite, 16> Applies;
  for (auto &BB : F) {
    for (auto It = BB.begin(), End = BB.end(); It != End;) {
      auto &I = *It++;

      // Skip non-apply instructions.

      auto Apply = ApplySite::isa(&I);
      if (!Apply)
        continue;
      Applies.push_back(Apply);
   }
  }
  for (auto Apply : Applies) {
    auto NewInstPair = tryDevirtualizeApply(Apply, CHA);
    if (!NewInstPair.second)
      continue;

    Changed = true;

    auto *AI = Apply.getInstruction();
    if (!isa<TryApplyInst>(AI))
      AI->replaceAllUsesWith(NewInstPair.first);

    DeadApplies.push_back(AI);
    NewApplies.push_back(NewInstPair.second);
  }

  // Remove all the now-dead applies.
  while (!DeadApplies.empty()) {
    auto *AI = DeadApplies.pop_back_val();
    recursivelyDeleteTriviallyDeadInstructions(AI, true);
  }

  // For each new apply, attempt to link in function bodies if we do
  // not already have them, then notify the pass manager of the new
  // functions.
  //
  // We do this after deleting the old applies because otherwise we
  // hit verification errors in the linking code due to having
  // non-cond_br critical edges.
  while (!NewApplies.empty()) {
    auto Apply = NewApplies.pop_back_val();

    auto *CalleeFn = Apply.getReferencedFunction();
    assert(CalleeFn && "Expected devirtualized callee!");

    // We need to ensure that we link after devirtualizing in order to pull in
    // everything we reference from another module. This is especially important
    // for transparent functions, because if transparent functions are not
    // inlined for some reason, we need to generate code for them.
    // Note that functions, which are only referenced from witness/vtables, are
    // not linked upfront by the SILLinker.
    if (!CalleeFn->isDefinition())
      F.getModule().linkFunction(CalleeFn, SILModule::LinkingMode::LinkAll);

    // We may not have optimized these functions yet, and it could
    // be beneficial to rerun some earlier passes on the current
    // function now that we've made these direct references visible.
    if (CalleeFn->isDefinition() && CalleeFn->shouldOptimize())
      notifyPassManagerOfFunction(CalleeFn, nullptr);
  }

  return Changed;
}

SILTransform *swift::createDevirtualizer() { return new Devirtualizer(); }
