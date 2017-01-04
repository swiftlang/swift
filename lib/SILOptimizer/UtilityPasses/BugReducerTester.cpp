//===--- BugReducerTester.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This pass is a testing pass for sil-bug-reducer. It asserts when it visits a
/// function that calls a function specified by an llvm::cl::opt.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<std::string> FunctionTarget(
    "bug-reducer-tester-target-func",
    llvm::cl::desc("Function that when called by an apply should cause "
                   "BugReducerTester to blow up or miscompile if the pass "
                   "visits the apply"));

namespace {
enum class FailureKind { OptimizerCrasher, Miscompile, None };
} // end anonymous namespace

static llvm::cl::opt<FailureKind> TargetFailureKind(
    "bug-reducer-tester-failure-kind",
    llvm::cl::desc("The type of failure to perform"),
    llvm::cl::values(
        clEnumValN(FailureKind::OptimizerCrasher, "opt-crasher",
                   "Crash the optimizer when we see the specified apply"),
        clEnumValN(FailureKind::Miscompile, "runtime-miscompile",
                   "Delete the target function call to cause a miscompile"),
        clEnumValEnd),
    llvm::cl::init(FailureKind::None));

namespace {

class BugReducerTester : public SILFunctionTransform {

  // We only want to cause 1 miscompile.
  bool CausedMiscompile = false;

  void run() override {
    // If we don't have a target function or we already caused a miscompile,
    // just return.
    if (FunctionTarget.empty() || CausedMiscompile)
      return;
    assert(TargetFailureKind != FailureKind::None);
    SILModule &M = getFunction()->getModule();
    for (auto &BB : *getFunction()) {
      for (auto &II : BB) {
        auto *Apply = dyn_cast<ApplyInst>(&II);
        if (!Apply)
          continue;
        auto *FRI = dyn_cast<FunctionRefInst>(Apply->getCallee());
        if (!FRI ||
            !FRI->getReferencedFunction()->getName().equals(FunctionTarget))
          continue;

        // Ok, we found the Apply that we want! If we are asked to crash, crash
        // here.
        if (TargetFailureKind == FailureKind::OptimizerCrasher)
          llvm_unreachable("Found the target!");

        // Otherwise, delete the apply to cause a miscompile.
        Apply->replaceAllUsesWith(SILUndef::get(Apply->getType(), M));
        Apply->eraseFromParent();

        // Mark that we found the miscompile and return so we do not try to
        // visit any more instructions in this function.
        CausedMiscompile = true;
        return;
      }
    }
  }

  StringRef getName() override { return "Bug Reducer Tester"; }
};

} // end anonymous namespace

SILTransform *swift::createBugReducerTester() { return new BugReducerTester(); }
