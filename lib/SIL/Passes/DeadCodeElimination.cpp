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
STATISTIC(NumInstructionsRemoved, "Number of unreachable instructions removed");

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This rutine only examines the state of the instruction at hand.
/// \param checkUses - The flag that allows not to check if the instruction is
///        used by others. This should be used when we already know that the
//         instruction is not used or is only used by dead instructions.
static bool isInstructionTriviallyDead(SILInstruction *I,
                                       bool checkUses = true) {
  if ((!I->use_empty() && checkUses) || isa<TermInst>(I))
    return false;

  if (!I->mayHaveSideEffects())
    return true;

  return false;
}

/// \brief Check if the operand is only used by the given user.
static bool isTheOnlyUser(SILValue Op, SILValue User) {
  for (auto UseI = Op.use_begin(), UseE = Op.use_end(); UseI != UseE; ++UseI) {
    if (UseI.getUser() != User.getDef())
      return false;
  }
  return true;
}

/// \brief If the given instruction is dead, delete it along with its dead
/// operands.
///
/// \return Returns true if any instructions were deleted.
static bool recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I) {
  // If the instruction is not dead, there is nothing to do.
  if (!I || !isInstructionTriviallyDead(I))
    return false;

  // Delete this instruction and others that become dead after it's deleted.
  SmallVector<SILInstruction*, 16> DeadInsts;
  DeadInsts.push_back(I);
  do {
    I = DeadInsts.pop_back_val();

    // Check if any of the operands will become dead as well.
    ArrayRef<Operand> Ops = I->getAllOperands();
    for (auto OpI = Ops.begin(), OpE = Ops.end(); OpI != OpE; ++OpI) {

      // If the operand is an instruction that is only used by the instruction
      // being deleted, delete it in a future loop iteration.
      SILValue OpVal = (*OpI).get();
      if (!isTheOnlyUser(OpVal, I))
        continue;
      if (SILInstruction *OpI = dyn_cast<SILInstruction>(OpVal))
        if (isInstructionTriviallyDead(OpI, false))
          DeadInsts.push_back(OpI);
    }

    // This will remove this instruction and all its uses.
    I->eraseFromParent();
  } while (!DeadInsts.empty());
  
  return true;
}

static void constantFoldTerminator(SILBasicBlock &BB) {
  TermInst *TI = BB.getTerminator();

  // Process conditional branches with constant conditions.
  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(TI)) {
    SILValue V = CBI->getCondition();
    SILInstruction *CondI = dyn_cast<SILInstruction>(V.getDef());

    if (IntegerLiteralInst *ConstCond =
          dyn_cast_or_null<IntegerLiteralInst>(CondI)) {
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
      // Note that some of the instructions the terminator was using are now
      // dead. We rely on later stages of the pass to remove them.
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
      // Simplify the blocks with terminators that rely on constant conditions.
      constantFoldTerminator(BB);
    }

    // Remove unreachable blocks.
    removeUnreachableBlocks(Fn);

    // Remove dead instructions.
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        auto CurrentInst = I;
        // Move the iterator before we remove instructions to avoid iterator
        // invalidation issues.
        ++I;
        if (recursivelyDeleteTriviallyDeadInstructions(CurrentInst))
          NumInstructionsRemoved++;
      }
    }
  }
}
