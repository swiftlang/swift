//===--- DifferentiabilityWitnessInliner.cpp ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// "Inlines" differentiability witnesses whose bodies are availabe, by turning
// "differentiability_witness_function" instructions into "function_ref"
// instructions referencing the appropriate function.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {
class DifferentiabilityWitnessInliner : public SILFunctionTransform {

  /// Returns true if and changes were made.
  bool inlineDifferentiabilityWitnessesInFunction(SILFunction &F);

  /// The entry point to the transformation.
  void run() override {
    if (inlineDifferentiabilityWitnessesInFunction(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
  }
};
} // end anonymous namespace

bool DifferentiabilityWitnessInliner::
    inlineDifferentiabilityWitnessesInFunction(SILFunction &F) {
  bool Changed = false;
  llvm::SmallVector<DifferentiabilityWitnessFunctionInst *, 8> Insts;
  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *DFWI = dyn_cast<DifferentiabilityWitnessFunctionInst>(&I);
      if (!DFWI)
        continue;
      Insts.push_back(DFWI);
    }
  }
  for (auto *I : Insts) {
    auto *W = I->getWitness();
    if (W->isDeclaration() && !F.getModule().loadDifferentiabilityWitness(W))
      continue;
    assert(W->isDefinition());
    SILBuilderWithScope B(I);
    auto Kind = I->getWitnessKind().getAsDerivativeFunctionKind();
    assert(Kind.hasValue());
    auto *NewI = B.createFunctionRefFor(I->getLoc(), W->getDerivative(*Kind));
    I->replaceAllUsesWith(NewI);
    I->getParent()->erase(I);
  }
  return Changed;
}

SILTransform *swift::createDifferentiabilityWitnessInliner() {
  return new DifferentiabilityWitnessInliner();
}
