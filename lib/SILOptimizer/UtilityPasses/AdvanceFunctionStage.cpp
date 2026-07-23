//===--- AdvanceFunctionStage.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Test-only utility: advances the per-function SIL stage of the functions it
/// visits to Canonical, WITHOUT touching the module stage floor and WITHOUT
/// setting wasDeserializedCanonical. This lets .sil tests manufacture a local
/// function that is Canonical-ahead-of-a-Raw-module-floor -- a state the normal
/// pipeline never produces (the Raw->Canonical sweep advances every local
/// function in lockstep with the module floor) -- to validate per-function
/// stage readers and skip-checks before a demand-driven per-function driver
/// exists. sil-opt's explicit-tag path never raises the module stage, so running
/// this pass on a `sil_stage raw` module leaves the floor at Raw.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<std::string> AdvanceStageOnlyFun(
    "advance-function-stage-only-fun",
    llvm::cl::desc("If set, only advance the per-function stage of the function "
                   "with this exact name; leave all other functions untouched."));

namespace {

class AdvanceFunctionStage : public SILFunctionTransform {
  void run() override {
    SILFunction *f = getFunction();
    if (!AdvanceStageOnlyFun.empty() && f->getName() != AdvanceStageOnlyFun)
      return;
    // Guard keeps setFunctionStage()'s monotonic assert satisfied. Do not run
    // this on a `sil_stage lowered` module.
    if (f->getFunctionStage() < SILStage::Canonical)
      f->setFunctionStage(SILStage::Canonical);
  }
};

} // end anonymous namespace

SILTransform *swift::createAdvanceFunctionStage() {
  return new AdvanceFunctionStage();
}
