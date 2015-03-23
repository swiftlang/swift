//===-- MergeCondFail.cpp -  Merge cond_fail instructions -*- C++ -*-------===//
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

#define DEBUG_TYPE "merge-cond_fail"

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/Support/Debug.h"

using namespace swift;


/// \brief Return true if the operand of the cond_fail instruction looks like
/// the overflow bit of an arithmetic instruction.
static bool hasOverflowConditionOperand(CondFailInst *CFI) {
  if (auto *TEI = dyn_cast<TupleExtractInst>(CFI->getOperand()))
    if (isa<BuiltinInst>(TEI->getOperand()))
      return true;
  return false;
}

namespace {
/// Merge cond_fail instructions.
///
/// We can merge cond_fail instructions if there is no side-effect or memory
/// write in between them.
/// This pass merges cond_fail instructions by building the disconjunction of
/// their operands.
class MergeCondFailInsts : public SILFunctionTransform {
public:
  MergeCondFailInsts() {}

  StringRef getName() override { return "Merge cond_fail instructions"; }

  void run() override {
    bool Changed = false;

    // Merge cond_fail instructions if there is no side-effect or read in
    // between them.
    for (auto &BB : *getFunction()) {
      // Per basic block list of cond_fails to merge.
      SmallVector<CondFailInst *, 16> CondFailToMerge;
      for (auto InstIt = BB.begin(), End = BB.end(); InstIt != End;) {
        auto *CurInst = &*InstIt;
        ++InstIt;
        CondFailInst *CFI = dyn_cast<CondFailInst>(CurInst);

        // Stop merging at side-effects or reads from memory.
        if (!CFI && (CurInst->mayHaveSideEffects() ||
                     CurInst->mayReadFromMemory())) {
          // Merge cond_fail.
          if (CondFailToMerge.size() > 1) {
            Changed |= mergeCondFails(CondFailToMerge);
            CondFailToMerge.clear();
            continue;
          }
        }

        if (CFI)
          CondFailToMerge.push_back(CFI);

      }
      // Process any remaining cond_fail instructions in the current basic
      // block.
      if (CondFailToMerge.size() > 1)
        Changed |= mergeCondFails(CondFailToMerge);
    }
    if (Changed)
      PM->invalidateAnalysis(getFunction(),
                             SILAnalysis::PreserveKind::ProgramFlow);
  }

  /// \brief Try to merge the cond_fail instructions. Returns true if any could
  /// be merge.
  bool mergeCondFails(SmallVectorImpl<CondFailInst *> &CondFailToMerge) {
    assert(CondFailToMerge.size() > 1 &&
           "Need at least two cond_fail instructions");

    // Remove overflow operands from the list. We can typically generate more
    // efficient code without merging them: jump-on-overflow.
    CondFailToMerge.erase(std::remove_if(CondFailToMerge.begin(),
                                         CondFailToMerge.end(),
                                         hasOverflowConditionOperand),
                          CondFailToMerge.end());

    if (CondFailToMerge.size() < 2)
      return false;

    SILValue MergedCond;
    auto *LastCFI = CondFailToMerge.back();
    auto InsertPt = ++SILBasicBlock::iterator(LastCFI);
    SILBuilderWithScope<4> Builder(InsertPt);
    SILLocation Loc = LastCFI->getLoc();

    // Merge conditions and remove the merged cond_fail instructions.
    for (unsigned I = 0, E = CondFailToMerge.size(); I != E; ++I) {
      auto CurCond = CondFailToMerge[I]->getOperand();
      if (MergedCond)
        CurCond = Builder.createBuiltinBinaryFunction(
            Loc, "or", CurCond.getType(), CurCond.getType(),
            {MergedCond, CurCond});

      CondFailToMerge[I]->eraseFromParent();
      MergedCond = CurCond;
    }

    Builder.createCondFail(Loc, MergedCond);
    // Create a new cond_fail using the merged condition.
    return true;
  }
};
}

SILTransform *swift::createMergeCondFails() {
  return new MergeCondFailInsts();
}
