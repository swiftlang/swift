//===--- ValueLifetime.cpp - ValueLifetimeAnalysis ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"

using namespace swift;

void ValueLifetimeAnalysis::propagateLiveness() {
  assert(LiveBlocks.empty() && "frontier computed twice");

  auto DefBB = DefValue->getParentBlock();
  llvm::SmallVector<SILBasicBlock *, 64> Worklist;
  int NumUsersBeforeDef = 0;

  // Find the initial set of blocks where the value is live, because
  // it is used in those blocks.
  for (SILInstruction *User : UserSet) {
    SILBasicBlock *UserBlock = User->getParent();
    if (LiveBlocks.insert(UserBlock))
      Worklist.push_back(UserBlock);

    // A user in the DefBB could potentially be located before the DefValue.
    if (UserBlock == DefBB)
      NumUsersBeforeDef++;
  }
  // Don't count any users in the DefBB which are actually located _after_
  // the DefValue.
  auto InstIter = DefValue->getIterator();
  while (NumUsersBeforeDef > 0 && ++InstIter != DefBB->end()) {
    if (UserSet.count(&*InstIter))
      NumUsersBeforeDef--;
  }

  // Now propagate liveness backwards until we hit the block that defines the
  // value.
  while (!Worklist.empty()) {
    auto *BB = Worklist.pop_back_val();

    // Don't go beyond the definition.
    if (BB == DefBB && NumUsersBeforeDef == 0)
      continue;

    for (SILBasicBlock *Pred : BB->getPredecessorBlocks()) {
      // If it's already in the set, then we've already queued and/or
      // processed the predecessors.
      if (LiveBlocks.insert(Pred))
        Worklist.push_back(Pred);
    }
  }
}

SILInstruction *ValueLifetimeAnalysis:: findLastUserInBlock(SILBasicBlock *BB) {
  // Walk backwards in BB looking for last use of the value.
  for (auto II = BB->rbegin(); II != BB->rend(); ++II) {
    assert(DefValue != &*II && "Found def before finding use!");

    if (UserSet.count(&*II))
      return &*II;
  }
  llvm_unreachable("Expected to find use of value in block!");
}

bool ValueLifetimeAnalysis::computeFrontier(Frontier &Fr, Mode mode,
                                            DeadEndBlocks *DEBlocks) {
  assert(!isAliveAtBeginOfBlock(DefValue->getFunction()->getEntryBlock()) &&
         "Can't compute frontier for def which does not dominate all uses");

  bool NoCriticalEdges = true;

  // Exit-blocks from the lifetime region. The value is live at the end of
  // a predecessor block but not in the frontier block itself.
  llvm::SmallSetVector<SILBasicBlock *, 16> FrontierBlocks;

  // Blocks where the value is live at the end of the block and which have
  // a frontier block as successor.
  llvm::SmallSetVector<SILBasicBlock *, 16> LiveOutBlocks;

  /// The lifetime ends if we have a live block and a not-live successor.
  for (SILBasicBlock *BB : LiveBlocks) {
    if (DEBlocks && DEBlocks->isDeadEnd(BB))
      continue;

    bool LiveInSucc = false;
    bool DeadInSucc = false;
    for (const SILSuccessor &Succ : BB->getSuccessors()) {
      if (isAliveAtBeginOfBlock(Succ)) {
        LiveInSucc = true;
      } else if (!DEBlocks || !DEBlocks->isDeadEnd(Succ)) {
        DeadInSucc = true;
      }
    }
    if (!LiveInSucc) {
      // The value is not live in any of the successor blocks. This means the
      // block contains a last use of the value. The next instruction after
      // the last use is part of the frontier.
      SILInstruction *LastUser = findLastUserInBlock(BB);
      if (!isa<TermInst>(LastUser)) {
        Fr.push_back(&*std::next(LastUser->getIterator()));
        continue;
      }
      // In case the last user is a TermInst we add all successor blocks to the
      // frontier (see below).
      assert(DeadInSucc && "The final using TermInst must have successors");
    }
    if (DeadInSucc) {
      if (mode == UsersMustPostDomDef)
        return false;

      // The value is not live in some of the successor blocks.
      LiveOutBlocks.insert(BB);
      for (const SILSuccessor &Succ : BB->getSuccessors()) {
        if (!isAliveAtBeginOfBlock(Succ)) {
          // It's an "exit" edge from the lifetime region.
          FrontierBlocks.insert(Succ);
        }
      }
    }
  }
  // Handle "exit" edges from the lifetime region.
  llvm::SmallPtrSet<SILBasicBlock *, 16> UnhandledFrontierBlocks;
  for (SILBasicBlock *FrontierBB: FrontierBlocks) {
    assert(mode != UsersMustPostDomDef);
    bool needSplit = false;
    // If the value is live only in part of the predecessor blocks we have to
    // split those predecessor edges.
    for (SILBasicBlock *Pred : FrontierBB->getPredecessorBlocks()) {
      if (!LiveOutBlocks.count(Pred)) {
        needSplit = true;
        break;
      }
    }
    if (needSplit) {
      if (mode == DontModifyCFG)
        return false;
      // We need to split the critical edge to create a frontier instruction.
      UnhandledFrontierBlocks.insert(FrontierBB);
    } else {
      // The first instruction of the exit-block is part of the frontier.
      Fr.push_back(&*FrontierBB->begin());
    }
  }
  // Split critical edges from the lifetime region to not yet handled frontier
  // blocks.
  for (SILBasicBlock *FrontierPred : LiveOutBlocks) {
    assert(mode != UsersMustPostDomDef);
    auto *T = FrontierPred->getTerminator();
    // Cache the successor blocks because splitting critical edges invalidates
    // the successor list iterator of T.
    llvm::SmallVector<SILBasicBlock *, 4> SuccBlocks;
    for (const SILSuccessor &Succ : T->getSuccessors())
      SuccBlocks.push_back(Succ);

    for (unsigned i = 0, e = SuccBlocks.size(); i != e; ++i) {
      if (UnhandledFrontierBlocks.count(SuccBlocks[i])) {
        assert(mode == AllowToModifyCFG);
        assert(isCriticalEdge(T, i) && "actually not a critical edge?");
        SILBasicBlock *NewBlock = splitEdge(T, i);
        // The single terminator instruction is part of the frontier.
        Fr.push_back(&*NewBlock->begin());
        NoCriticalEdges = false;
      }
    }
  }
  return NoCriticalEdges;
}

bool ValueLifetimeAnalysis::isWithinLifetime(SILInstruction *Inst) {
  SILBasicBlock *BB = Inst->getParent();
  // Check if the value is not live anywhere in Inst's block.
  if (!LiveBlocks.count(BB))
    return false;
  for (const SILSuccessor &Succ : BB->getSuccessors()) {
    // If the value is live at the beginning of any successor block it is also
    // live at the end of BB and therefore Inst is definitely in the lifetime
    // region (Note that we don't check in upward direction against the value's
    // definition).
    if (isAliveAtBeginOfBlock(Succ))
      return true;
  }
  // The value is live in the block but not at the end of the block. Check if
  // Inst is located before (or at) the last use.
  for (auto II = BB->rbegin(); II != BB->rend(); ++II) {
    if (UserSet.count(&*II)) {
      return true;
    }
    if (Inst == &*II)
      return false;
  }
  llvm_unreachable("Expected to find use of value in block!");
}

// Searches \p BB backwards from the instruction before \p FrontierInst
// to the beginning of the list and returns true if we find a dealloc_ref
// /before/ we find \p DefValue (the instruction that defines our target value).
static bool blockContainsDeallocRef(SILBasicBlock *BB, SILInstruction *DefValue,
                                    SILInstruction *FrontierInst) {
  SILBasicBlock::reverse_iterator End = BB->rend();
  SILBasicBlock::reverse_iterator Iter = FrontierInst->getReverseIterator();
  for (++Iter; Iter != End; ++Iter) {
    SILInstruction *I = &*Iter;
    if (isa<DeallocRefInst>(I))
      return true;
    if (I == DefValue)
      return false;
  }
  return false;
}

bool ValueLifetimeAnalysis::containsDeallocRef(const Frontier &Frontier) {
  SmallPtrSet<SILBasicBlock *, 8> FrontierBlocks;
  // Search in live blocks where the value is not alive until the end of the
  // block, i.e. the live range is terminated by a frontier instruction.
  for (SILInstruction *FrontierInst : Frontier) {
    SILBasicBlock *BB = FrontierInst->getParent();
    if (blockContainsDeallocRef(BB, DefValue, FrontierInst))
      return true;
    FrontierBlocks.insert(BB);
  }
  // Search in all other live blocks where the value is alive until the end of
  // the block.
  for (SILBasicBlock *BB : LiveBlocks) {
    if (FrontierBlocks.count(BB) == 0) {
      if (blockContainsDeallocRef(BB, DefValue, BB->getTerminator()))
        return true;
    }
  }
  return false;
}

void ValueLifetimeAnalysis::dump() const {
  llvm::errs() << "lifetime of def: " << *DefValue;
  for (SILInstruction *Use : UserSet) {
    llvm::errs() << "  use: " << *Use;
  }
  llvm::errs() << "  live blocks:";
  for (SILBasicBlock *BB : LiveBlocks) {
    llvm::errs() << ' ' << BB->getDebugID();
  }
  llvm::errs() << '\n';
}
