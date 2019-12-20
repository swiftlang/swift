//===--- DifferentiabilityWitnessDevirtualizer.cpp ------------------------===//
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
// Devirtualized differentiability witnesses whose bodies are availabe, by
// turning "differentiability_witness_function" instructions into "function_ref"
// instructions referencing the appropriate function.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {
class DifferentiabilityWitnessDevirtualizer : public SILFunctionTransform {

  /// Returns true if and changes were made.
  bool devirtualizeDifferentiabilityWitnessesInFunction(SILFunction &f);

  /// The entry point to the transformation.
  void run() override {
    if (devirtualizeDifferentiabilityWitnessesInFunction(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
  }
};
} // end anonymous namespace

bool DifferentiabilityWitnessDevirtualizer::
    devirtualizeDifferentiabilityWitnessesInFunction(SILFunction &f) {
  bool changed = false;
  llvm::SmallVector<DifferentiabilityWitnessFunctionInst *, 8> insts;
  for (auto &bb : f) {
    for (auto &inst : bb) {
      auto *dfwi = dyn_cast<DifferentiabilityWitnessFunctionInst>(&inst);
      if (!dfwi)
        continue;
      insts.push_back(dfwi);
    }
  }
  for (auto *inst : insts) {
    auto *wit = inst->getWitness();
    if (wit->isDeclaration())
      f.getModule().loadDifferentiabilityWitness(wit);
    if (wit->isDeclaration())
      continue;
    changed = true;
    SILBuilderWithScope builder(inst);
    auto kind = inst->getWitnessKind().getAsDerivativeFunctionKind();
    assert(kind.hasValue());
    auto *newInst = builder.createFunctionRefFor(inst->getLoc(),
                                                 wit->getDerivative(*kind));
    inst->replaceAllUsesWith(newInst);
    inst->getParent()->erase(inst);
  }
  return changed;
}

SILTransform *swift::createDifferentiabilityWitnessDevirtualizer() {
  return new DifferentiabilityWitnessDevirtualizer();
}
