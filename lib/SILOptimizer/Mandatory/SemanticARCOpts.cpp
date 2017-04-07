//===--- SemanticARCOpts.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "sil-semantic-arc-opts"
#include "swift/SIL/OwnershipChecker.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumEliminatedInsts, "number of removed instructions");

static bool optimizeGuaranteedArgument(SILArgument *Arg) {
  bool MadeChange = false;

  // Gather all copy_value users of Arg.
  llvm::SmallVector<CopyValueInst *, 4> Copies;
  for (auto *Op : Arg->getUses()) {
    if (auto *CVI = dyn_cast<CopyValueInst>(Op->getUser())) {
      Copies.push_back(CVI);
    }
  }

  // Then until we run out of copies...
  while (!Copies.empty()) {
    auto *CVI = Copies.pop_back_val();

    // Quickly see if copy has only one use and that use is a destroy_value. In
    // such a case, we can always eliminate both the copy and the destroy.
    if (auto *Op = CVI->getSingleUse()) {
      if (auto *DVI = dyn_cast<DestroyValueInst>(Op->getUser())) {
        DVI->eraseFromParent();
        CVI->eraseFromParent();
        NumEliminatedInsts += 2;
        continue;
      }
    }
  }

  return MadeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

struct SemanticARCOpts : SILFunctionTransform {
  void run() override {
    bool MadeChange = false;
    SILFunction *F = getFunction();

    // First as a special case, handle guaranteed SIL function arguments.
    //
    // The reason that this is special is that we do not need to consider the
    // end of the borrow scope since the end of the function is the end of the
    // borrow scope.
    for (auto *Arg : F->getArguments()) {
      if (Arg->getOwnershipKind() != ValueOwnershipKind::Guaranteed)
        continue;
      MadeChange |= optimizeGuaranteedArgument(Arg);
    }

    if (MadeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

  StringRef getName() override { return "Semantic ARC Opts"; }
};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() { return new SemanticARCOpts(); }
