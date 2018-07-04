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
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumEliminatedInsts, "number of removed instructions");

static bool optimizeGuaranteedArgument(SILArgument *Arg,
                                       OwnershipChecker &Checker) {
  bool MadeChange = false;

  // Gather all copy_value users of Arg.
  llvm::SmallVector<CopyValueInst *, 4> Worklist;
  for (auto *Op : Arg->getUses()) {
    if (auto *CVI = dyn_cast<CopyValueInst>(Op->getUser())) {
      Worklist.push_back(CVI);
    }
  }

  // Then until we run out of copies...
  while (!Worklist.empty()) {
    auto *CVI = Worklist.pop_back_val();

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

    // Ok, now run the checker on the copy value. If it fails, then we just
    // continue.
    if (!Checker.checkValue(CVI))
      continue;

    // Otherwise, lets do a quick check on what the checker thinks the lifetime
    // ending and non-lifetime ending users. To be conservative, we bail unless
    // each lifetime ending use is a destroy_value and if each non-lifetime
    // ending use is one of the following instructions:
    //
    // 1. copy_value.
    // 2. begin_borrow.
    // 3. end_borrow.
    if (!all_of(Checker.LifetimeEndingUsers, [](SILInstruction *I) -> bool {
          return isa<DestroyValueInst>(I);
        }))
      continue;

    // Extra copy values that we should visit recursively.
    llvm::SmallVector<CopyValueInst *, 8> NewCopyInsts;
    llvm::SmallVector<SILInstruction *, 8> NewBorrowInsts;
    if (!all_of(Checker.RegularUsers, [&](SILInstruction *I) -> bool {
          if (auto *CVI = dyn_cast<CopyValueInst>(I)) {
            NewCopyInsts.push_back(CVI);
            return true;
          }

          if (!isa<BeginBorrowInst>(I) && !isa<EndBorrowInst>(I))
            return false;

          NewBorrowInsts.push_back(I);
          return true;
        }))
      continue;

    // Ok! we can remove the copy_value, destroy_values!
    MadeChange = true;
    CVI->replaceAllUsesWith(CVI->getOperand());
    CVI->eraseFromParent();
    ++NumEliminatedInsts;

    while (!Checker.LifetimeEndingUsers.empty()) {
      Checker.LifetimeEndingUsers.pop_back_val()->eraseFromParent();
      ++NumEliminatedInsts;
    }

    // Then add the copy_values that were users of our original copy value to
    // the worklist.
    while (!NewCopyInsts.empty()) {
      Worklist.push_back(NewCopyInsts.pop_back_val());
    }

    // Then remove any begin/end borrow that we found. These are unneeded since
    // the lifetime guarantee from the argument exists above and beyond said
    // scope.
    while (!NewBorrowInsts.empty()) {
      SILInstruction *I = NewBorrowInsts.pop_back_val();
      if (auto *BBI = dyn_cast<BeginBorrowInst>(I)) {
        // Any copy_value that is used by the begin borrow is added to the
        // worklist.
        for (auto *BBIUse : BBI->getUses()) {
          if (auto *BBIUseCopyValue =
                  dyn_cast<CopyValueInst>(BBIUse->getUser())) {
            Worklist.push_back(BBIUseCopyValue);
          }
        }
        BBI->replaceAllUsesWith(BBI->getOperand());
        BBI->eraseFromParent();
        ++NumEliminatedInsts;
        continue;
      }

      // This is not necessary, but it does add a check.
      auto *EBI = cast<EndBorrowInst>(I);
      EBI->eraseFromParent();
      ++NumEliminatedInsts;
    }
  }

  return MadeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

// Even though this is a mandatory pass, it is rerun after deserialization in
// case DiagnosticConstantPropagation exposed anything new in this assert
// configuration.
struct SemanticARCOpts : SILFunctionTransform {
  void run() override {
    bool MadeChange = false;
    SILFunction *F = getFunction();

    DeadEndBlocks DEBlocks(F);
    OwnershipChecker Checker{F->getModule(), DEBlocks, {}, {}, {}};

    // First as a special case, handle guaranteed SIL function arguments.
    //
    // The reason that this is special is that we do not need to consider the
    // end of the borrow scope since the end of the function is the end of the
    // borrow scope.
    for (auto *Arg : F->getArguments()) {
      if (Arg->getOwnershipKind() != ValueOwnershipKind::Guaranteed)
        continue;
      MadeChange |= optimizeGuaranteedArgument(Arg, Checker);
    }

    if (MadeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() { return new SemanticARCOpts(); }
