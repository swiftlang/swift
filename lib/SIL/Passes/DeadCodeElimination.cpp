//===--- DeadCodeElimination.cpp - Promote alloc_box to alloc_stack ------===//
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
#define DEBUG_TYPE "dead-code-elimination"
#include "swift/Subsystems.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumBlocksRemoved, "Number of unreachable basic blocks removed");


static void constantFoldTerminator(SILBasicBlock &BB) {
  TermInst *TI = BB.getTerminator();

  // Process conditional branches with constant conditions.
  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(TI)) {
    SILValue V = CBI->getCondition();
    SILInstruction *CondI = dyn_cast<SILInstruction>(V.getDef());

    if (IntegerLiteralInst *ConstCond = dyn_cast<IntegerLiteralInst>(CondI)) {
      SILBuilder B(&BB);

      // Determine which of the successors is unreachable.
      // TODO: propagate the BB arguments.
      BranchInst *BI = 0;
      if (ConstCond->getValue() == APInt(1, /*value*/ 0, false)) {
        BI = B.createBranch(CBI->getLoc(),
                            CBI->getFalseBB()/*, CBI->getFalseArgs()*/);
      } else {
        assert(ConstCond->getValue() == APInt(1, /*value*/ 1, false) &&
               "Our representation of true/false does not match.");
        BI = B.createBranch(CBI->getLoc(),
                            CBI->getTrueBB()/*, CBI->getTrueArgs()*/);
      }

      // TODO: Produce an unreachable code warning here if the basic block
      // contains user code.
      
      CBI->eraseFromParent();

      // TODO: Some of the instructions the terminator was using are now dead.
      // We need to remove them as well.
    }
  }
}

static bool removeUnreachableBlocks(SILFunction &F) {
  if (F.empty())
    return false;

  llvm::SmallPtrSet<SILBasicBlock*, 16> Reachable;
  llvm::SmallVector<SILBasicBlock*, 128> Worklist;
  Worklist.push_back(&F.front());
  Reachable.insert(&F.front());

  // Collect all reachable blocks by walking the successors.
  do {
    SILBasicBlock *BB = Worklist.pop_back_val();
    for (auto SI = BB->succ_begin(), SE = BB->succ_end(); SI != SE; ++SI) {
      if (Reachable.insert(*SI))
        Worklist.push_back(*SI);
    }
  } while (!Worklist.empty());
  assert(Reachable.size() <= F.size());

  // If everything is reachable, we are done.
  if (Reachable.size() == F.size())
    return false;

  // Remove references from the dead blocks.
  for (auto I = F.begin(), E = F.end(); I != E; ++I) {
    SILBasicBlock *BB = I;
    if (Reachable.count(BB))
      continue;

    // TODO: We would need to specially handle the args for the blocks that
    // are going to stay around.
    // for (auto SI = BB->succ_begin(), SE = BB->succ_end(); SI != SE; ++SI)
    //   if (Reachable.count(*SI))
    //     (*SI)->removePredecessor(BB);

    // Drop references to other blocks.
    BB->getTerminator()->eraseFromParent();
  }

  // Delete the dead blocks.
  for (auto I = F.begin(), E = F.end(); I != E;)
    if (!Reachable.count(I)) {
      I = F.getBlocks().erase(I);
      NumBlocksRemoved++;
    } else
      ++I;

  return true;
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILDeadCodeElimination(SILModule *M) {

  for (auto &Fn : *M) {
    for (auto &BB : Fn) {
      constantFoldTerminator(BB);
    }
    removeUnreachableBlocks(Fn);
  }
}
