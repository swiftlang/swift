//===--- LoopCanonicalizer.cpp --------------------------------------------===//
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
///
/// \file
/// This is a simple pass that can be used to apply loop canonicalizations to a
/// cfg. It also enables loop canonicalizations to be tested via FileCheck.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-loop-canonicalizer"
#include "swift/SILPasses/Passes.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/LoopUtils.h"

using namespace swift;

namespace {

class LoopCanonicalizer : public SILFunctionTransform {

  void run() override {
    SILFunction *F = getFunction();

    DEBUG(llvm::dbgs() << "Attempt to canonicalize loops in " << F->getName()
                       << "\n");

    auto *LA = PM->getAnalysis<SILLoopAnalysis>();
    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    auto *LI = LA->get(F);
    auto *DT = DA->get(F);

    if (LI->empty()) {
      DEBUG(llvm::dbgs() << "    No loops to canonicalize!\n");
      return;
    }

    bool Changed = false;
    std::vector<SILLoop *> Worklist;

    for (auto *L : LI->getTopLevelLoops())
      Worklist.push_back(L);

    while (Worklist.size()) {
      auto *L = Worklist.back();
      Worklist.pop_back();

      // We currently do not perform loop canonicalizations that introduce new
      // loops, so this will be safe. When that occurs, this pass will need to
      // be updated.
      for (auto *SubLoop : L->getSubLoops())
        Worklist.push_back(SubLoop);

      if (canonicalizeLoop(L, DT, LI)) {
        DEBUG(llvm::dbgs() << "    Canonicalized: " << *L);
        Changed = true;
      }
    }

    if (Changed) {
      // We preserve loop info and the dominator tree.
      DA->lockInvalidation();
      LA->lockInvalidation();
      PM->invalidateAnalysis(F, SILAnalysis::PreserveKind::Nothing);
      DA->unlockInvalidation();
      LA->unlockInvalidation();
    }
  }

  StringRef getName() override { return "Loop Canonicalizer"; }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

SILTransform *swift::createLoopCanonicalizer() {
  return new LoopCanonicalizer();
}
