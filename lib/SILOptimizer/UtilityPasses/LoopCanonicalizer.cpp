//===--- LoopCanonicalizer.cpp --------------------------------------------===//
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
/// This is a simple pass that can be used to apply loop canonicalizations to a
/// cfg. It also enables loop canonicalizations to be tested via FileCheck.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-loop-canonicalizer"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"

using namespace swift;

namespace {

class LoopCanonicalizer : public SILFunctionTransform {

  void run() override {
    SILFunction *F = getFunction();

    LLVM_DEBUG(llvm::dbgs() << "Attempt to canonicalize loops in "
                            << F->getName() << "\n");

    auto *LA = PM->getAnalysis<SILLoopAnalysis>();
    auto *LI = LA->get(F);

    if (LI->empty()) {
      LLVM_DEBUG(llvm::dbgs() << "    No loops to canonicalize!\n");
      return;
    }

    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    auto *DI = DA->get(F);
    if (canonicalizeAllLoops(DI, LI)) {
      // We preserve loop info and the dominator tree.
      DA->lockInvalidation();
      LA->lockInvalidation();
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::FunctionBody);
      DA->unlockInvalidation();
      LA->unlockInvalidation();
    }
  }

};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

SILTransform *swift::createLoopCanonicalizer() {
  return new LoopCanonicalizer();
}
