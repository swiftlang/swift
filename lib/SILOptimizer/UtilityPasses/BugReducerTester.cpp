//===--- BugReducerTester.cpp ---------------------------------------------===//
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
///
/// \file
///
/// This pass is a testing pass for sil-bug-reducer. It asserts when it visits a
/// function that calls a function specified by an llvm::cl::opt.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<std::string> FunctionTarget(
    "bug-reducer-tester-target-func",
    llvm::cl::desc("Function that when called by an apply should cause "
                   "BugReducerTester to blow up if the pass visits the apply"));

namespace {

class BugReducerTester : public SILFunctionTransform {

  void run() override {
    if (FunctionTarget.empty())
      return;
    for (auto &BB : *getFunction()) {
      for (auto &II : BB) {
        auto *Apply = dyn_cast<ApplyInst>(&II);
        if (!Apply)
          continue;
        auto *FRI = dyn_cast<FunctionRefInst>(Apply->getCallee());
        if (!FRI ||
            !FRI->getReferencedFunction()->getName().equals(FunctionTarget))
          continue;
        llvm_unreachable("Found the target!");
      }
    }
  }

  StringRef getName() override { return "Bug Reducer Tester"; }
};

} // end anonymous namespace

SILTransform *swift::createBugReducerTester() { return new BugReducerTester(); }
