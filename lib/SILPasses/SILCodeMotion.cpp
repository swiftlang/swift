//===- SILCodeMotion.cpp - Code Motion Optimizations ----------------------===//
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

#define DEBUG_TYPE "codemotion"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/RecyclingAllocator.h"

STATISTIC(NumCSESunk,   "Number of instructions sunk");
STATISTIC(NumCSEDeadStores, "Number of dead stores removed");
STATISTIC(NumCSEDupLoads,   "Number of dup loads removed");

using namespace swift;

static const int SinkSearchWindow = 6;

/// \brief Promote stored values to loads, remove dead stores and merge
/// duplicated loads.
void promoteMemoryOperationsInBlock(SILBasicBlock*BB) {
  StoreInst  *PrevStore = 0;
  llvm::SmallVector<LoadInst*, 8> Loads;

  auto II = BB->begin(), E = BB->end();
  while (II != E) {
    SILInstruction *Inst = II++;

    // This is a StoreInst. Let's see if we can remove the previous stores.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      // Invalidate all previous loads.
      Loads.clear();

      // If we are storing to the previously stored address then delete the old
      // store.
      if (PrevStore && PrevStore->getDest() == SI->getDest()) {
        recursivelyDeleteTriviallyDeadInstructions(PrevStore, true);
        PrevStore = SI;
        NumCSEDeadStores++;
        continue;
      }
      PrevStore = SI;
      continue;
    }

    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      // If we are loading a value that we just saved then use the saved value.
      if (PrevStore && PrevStore->getDest() == LI->getOperand()) {
        SILValue(LI, 0).replaceAllUsesWith(PrevStore->getSrc());
        recursivelyDeleteTriviallyDeadInstructions(LI, true);
        NumCSEDupLoads++;
        continue;
      }

      // Search the previous loads and replace the current load with one of the
      // previous loads.
      for (auto PrevLd : Loads) {
        if (PrevLd->getOperand() == LI->getOperand()) {
          SILValue(LI, 0).replaceAllUsesWith(PrevLd);
          recursivelyDeleteTriviallyDeadInstructions(LI, true);
          LI = 0;
          NumCSEDupLoads++;
          break;
        }
      }

      if (LI)
        Loads.push_back(LI);
      continue;
    }

    // Retains write to memory but they don't affect loads and stores.
    if (isa<StrongRetainInst>(Inst))
      continue;

    // All other instructions that read from memory invalidate the store.
    if (Inst->mayReadFromMemory())
      PrevStore = 0;

    // All other instructions that write to memory invalidate the loads.
    if (Inst->mayWriteToMemory())
      Loads.clear();
  }
}

/// \brief Returns True if we can sink this instruction to another basic block.
static bool canSinkInstruction(SILInstruction *Inst) {
  return Inst->use_empty() && !isa<TermInst>(Inst);
}

/// \brief Returns true if this instruction is a skip barrier, which means that
/// we can't sink other instructions past it.
static bool isSinkBarrier(SILInstruction *Inst) {
  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(Inst)) {
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef())) {
      return !isSideEffectFree(FR);
    }
  }

  if (isa<TermInst>(Inst))
    return false;

  if (Inst->mayHaveSideEffects())
    return true;

  return false;
}

/// \brief Search for an instruction that is identical to \p Iden by scanning
/// \p BB starting at the end of the block, stopping on sink barriers.
SILInstruction *findIdenticalInBlock(SILBasicBlock *BB, SILInstruction *Iden) {
  int SkipBudget = SinkSearchWindow;

  SILBasicBlock::iterator InstToSink = BB->getTerminator();

  while (SkipBudget) {
    // If we found a sinkable instruction that is identical to our goal
    // then return it.
    if (canSinkInstruction(InstToSink) && Iden->isIdenticalTo(InstToSink)) {
      DEBUG(llvm::dbgs() << "Found an identical instruction.");
      return InstToSink;
    }

    // If this instruction is a skip-barrier end the scan.
    if (isSinkBarrier(InstToSink))
      return nullptr;

    // If this is the first instruction in the block then we are done.
    if (InstToSink == BB->begin())
      return nullptr;

    SkipBudget--;
    InstToSink = std::prev(InstToSink);
    DEBUG(llvm::dbgs() << "Continuing scan. Next inst: " << *InstToSink);
  }

  return nullptr;
}

static void sinkCodeFromPredecessors(SILBasicBlock*BB) {
  if (BB->pred_empty())
    return;

  // This block must be the only successor of all the predecessors.
  for (auto P : BB->getPreds())
    if (P->getSingleSuccessor() != BB)
      return;

  SILBasicBlock *FirstPred = *BB->pred_begin();
  // The first Pred must have at least one non-terminator.
  if (FirstPred->getTerminator() == FirstPred->begin())
    return;

  DEBUG(llvm::dbgs() << " Sinking values from predecessors.\n");

  unsigned SkipBudget = SinkSearchWindow;

  // Start scanning backwards from the terminator.
  SILBasicBlock::iterator InstToSink = FirstPred->getTerminator();

  while (SkipBudget) {
    DEBUG(llvm::dbgs() << "Processing: " << *InstToSink);

    // Save the duplicated instructions in case we need to remove them.
    SmallVector<SILInstruction*, 4> Dups;

    if (canSinkInstruction(InstToSink)) {
      // For all preds:
      for (auto P : BB->getPreds()) {
        if (P == FirstPred)
          continue;

        // Search the duplicated instruction in the predecessor.
        if (SILInstruction *DupInst = findIdenticalInBlock(P, InstToSink)) {
          Dups.push_back(DupInst);
        } else {
          DEBUG(llvm::dbgs() << "Instruction mismatch.\n");
          Dups.clear();
          break;
        }
      }

      // If we found duplicated instructions, sink one of the copies and delete
      // the rest.
      if (Dups.size()) {
        DEBUG(llvm::dbgs() << "Moving: " << *InstToSink);
        InstToSink->moveBefore(BB->begin());
        for (auto I : Dups) {
          I->replaceAllUsesWith(InstToSink);
          I->eraseFromParent();
          NumCSESunk++;
        }

        // Restart the scan.
        InstToSink = FirstPred->getTerminator();
        DEBUG(llvm::dbgs() << "Restarting scan. Next inst: " << *InstToSink);
        continue;
      }
    }

    // If this instruction was a barrier then we can't sink anything else.
    if (isSinkBarrier(InstToSink)) {
      DEBUG(llvm::dbgs() << "Aborting on barrier: " << *InstToSink);
      return;
    }

    // This is the first instruction, we are done.
    if (InstToSink == FirstPred->begin()) {
      DEBUG(llvm::dbgs() << "Reached the first instruction.");
      return;
    }

    SkipBudget--;
    InstToSink = std::prev(InstToSink);
    DEBUG(llvm::dbgs() << "Continuing scan. Next inst: " << *InstToSink);

  }
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILCodeMotion(SILModule *M) {
  for (SILFunction &F : *M) {
    // Remove dead stores and merge duplicate loads.
    for (auto &BB : F)
      promoteMemoryOperationsInBlock(&BB);

    // Sink duplicated code from predecessors.
    for (auto &BB : F)
      sinkCodeFromPredecessors(&BB);
  }
}
