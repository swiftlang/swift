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
  assert(liveBlocks.empty() && "frontier computed twice");
  assert(!userSet.count(defValue) && "definition cannot be its own use");

  auto defBB = defValue->getParentBlock();
  llvm::SmallVector<SILBasicBlock *, 64> worklist;
  int numUsersBeforeDef = 0;

  // Find the initial set of blocks where the value is live, because
  // it is used in those blocks.
  for (SILInstruction *user : userSet) {
    SILBasicBlock *userBlock = user->getParent();
    if (liveBlocks.insert(userBlock))
      worklist.push_back(userBlock);

    // A user in the defBB could potentially be located before the defValue.
    if (userBlock == defBB)
      numUsersBeforeDef++;
  }
  // Don't count any users in the defBB which are actually located _after_
  // the defValue.
  auto instIter = defValue->getIterator();
  while (numUsersBeforeDef > 0 && ++instIter != defBB->end()) {
    if (userSet.count(&*instIter))
      numUsersBeforeDef--;
  }

  // Initialize the hasUsersBeforeDef field.
  hasUsersBeforeDef = numUsersBeforeDef > 0;

  // Now propagate liveness backwards until we hit the block that defines the
  // value.
  while (!worklist.empty()) {
    auto *bb = worklist.pop_back_val();

    // Don't go beyond the definition.
    if (bb == defBB && !hasUsersBeforeDef)
      continue;

    for (SILBasicBlock *Pred : bb->getPredecessorBlocks()) {
      // If it's already in the set, then we've already queued and/or
      // processed the predecessors.
      if (liveBlocks.insert(Pred))
        worklist.push_back(Pred);
    }
  }
}

SILInstruction *ValueLifetimeAnalysis::findLastUserInBlock(SILBasicBlock *bb) {
  // Walk backwards in bb looking for last use of the value.
  for (auto ii = bb->rbegin(); ii != bb->rend(); ++ii) {
    assert(defValue != &*ii && "Found def before finding use!");

    if (userSet.count(&*ii))
      return &*ii;
  }
  llvm_unreachable("Expected to find use of value in block!");
}

bool ValueLifetimeAnalysis::computeFrontier(Frontier &frontier, Mode mode,
                                            DeadEndBlocks *deBlocks) {
  assert(!isAliveAtBeginOfBlock(defValue->getFunction()->getEntryBlock())
         && "Can't compute frontier for def which does not dominate all uses");

  bool noCriticalEdges = true;

  // Exit-blocks from the lifetime region. The value is live at the end of
  // a predecessor block but not in the frontier block itself.
  llvm::SmallSetVector<SILBasicBlock *, 16> frontierBlocks;

  // Blocks where the value is live at the end of the block and which have
  // a frontier block as successor.
  llvm::SmallSetVector<SILBasicBlock *, 16> liveOutBlocks;

  /// The lifetime ends if we have a live block and a not-live successor.
  for (SILBasicBlock *bb : liveBlocks) {
    if (deBlocks && deBlocks->isDeadEnd(bb))
      continue;

    bool liveInSucc = false;
    bool deadInSucc = false;
    bool usedAndRedefinedInSucc = false;
    for (const SILSuccessor &succ : bb->getSuccessors()) {
      if (isAliveAtBeginOfBlock(succ)) {
        liveInSucc = true;
        if (succ == defValue->getParent()) {
          // Here, the basic block bb uses the value but also redefines the
          // value inside bb. The new value could be used by the successors
          // of succ and therefore could be live at the end of succ as well.
          usedAndRedefinedInSucc = true;
        }
      } else if (!deBlocks || !deBlocks->isDeadEnd(succ)) {
        deadInSucc = true;
      }
    }
    if (usedAndRedefinedInSucc) {
      // Here, the basic block bb uses the value and later redefines the value.
      // Therefore, this value's lifetime ends after its last use preceding the
      // re-definition of the value.
      auto ii = defValue->getReverseIterator();
      for (; ii != bb->rend(); ++ii) {
        if (userSet.count(&*ii)) {
          frontier.push_back(&*std::next(ii));
          break;
        }
      }
      assert(ii != bb->rend() &&
             "There must be a user in bb before definition");
    }
    if (!liveInSucc) {
      // The value is not live in any of the successor blocks. This means the
      // block contains a last use of the value. The next instruction after
      // the last use is part of the frontier.
      SILInstruction *lastUser = findLastUserInBlock(bb);
      if (!isa<TermInst>(lastUser)) {
        frontier.push_back(&*std::next(lastUser->getIterator()));
        continue;
      }
      // In case the last user is a TermInst we add all successor blocks to the
      // frontier (see below).
      assert(deadInSucc && "The final using TermInst must have successors");
    }
    if (deadInSucc) {
      if (mode == UsersMustPostDomDef)
        return false;

      // The value is not live in some of the successor blocks.
      liveOutBlocks.insert(bb);
      for (const SILSuccessor &succ : bb->getSuccessors()) {
        if (!isAliveAtBeginOfBlock(succ)) {
          // It's an "exit" edge from the lifetime region.
          frontierBlocks.insert(succ);
        }
      }
    }
  }
  // Handle "exit" edges from the lifetime region.
  llvm::SmallPtrSet<SILBasicBlock *, 16> unhandledFrontierBlocks;
  for (SILBasicBlock *frontierBB : frontierBlocks) {
    assert(mode != UsersMustPostDomDef);
    bool needSplit = false;
    // If the value is live only in part of the predecessor blocks we have to
    // split those predecessor edges.
    for (SILBasicBlock *Pred : frontierBB->getPredecessorBlocks()) {
      if (!liveOutBlocks.count(Pred)) {
        needSplit = true;
        break;
      }
    }
    if (needSplit) {
      // We need to split the critical edge to create a frontier instruction.
      unhandledFrontierBlocks.insert(frontierBB);
    } else {
      // The first instruction of the exit-block is part of the frontier.
      frontier.push_back(&*frontierBB->begin());
    }
  }
  if (unhandledFrontierBlocks.size() == 0) {
    return true;
  }

  // Split critical edges from the lifetime region to not yet handled frontier
  // blocks.
  for (SILBasicBlock *frontierPred : liveOutBlocks) {
    assert(mode != UsersMustPostDomDef);
    auto *term = frontierPred->getTerminator();
    // Cache the successor blocks because splitting critical edges invalidates
    // the successor list iterator of T.
    llvm::SmallVector<SILBasicBlock *, 4> succBlocks;
    for (const SILSuccessor &succ : term->getSuccessors())
      succBlocks.push_back(succ);

    for (unsigned i = 0, e = succBlocks.size(); i != e; ++i) {
      if (unhandledFrontierBlocks.count(succBlocks[i])) {
        assert(isCriticalEdge(term, i) && "actually not a critical edge?");
        noCriticalEdges = false;
        if (mode != AllowToModifyCFG) {
          // If the CFG need not be modified, just record the critical edge and
          // continue.
          this->criticalEdges.push_back({term, i});
          continue;
        }
        SILBasicBlock *newBlock = splitEdge(term, i);
        // The single terminator instruction is part of the frontier.
        frontier.push_back(&*newBlock->begin());
      }
    }
  }
  return noCriticalEdges;
}

bool ValueLifetimeAnalysis::isWithinLifetime(SILInstruction *inst) {
  SILBasicBlock *bb = inst->getParent();
  // Check if the value is not live anywhere in inst's block.
  if (!liveBlocks.count(bb))
    return false;
  for (const SILSuccessor &succ : bb->getSuccessors()) {
    // If the value is live at the beginning of any successor block it is also
    // live at the end of bb and therefore inst is definitely in the lifetime
    // region (Note that we don't check in upward direction against the value's
    // definition).
    if (isAliveAtBeginOfBlock(succ))
      return true;
  }
  // The value is live in the block but not at the end of the block. Check if
  // inst is located before (or at) the last use.
  for (auto ii = bb->rbegin(); ii != bb->rend(); ++ii) {
    if (userSet.count(&*ii)) {
      return true;
    }
    if (inst == &*ii)
      return false;
  }
  llvm_unreachable("Expected to find use of value in block!");
}

// Searches \p bb backwards from the instruction before \p frontierInst
// to the beginning of the list and returns true if we find a dealloc_ref
// /before/ we find \p defValue (the instruction that defines our target value).
static bool blockContainsDeallocRef(SILBasicBlock *bb, SILInstruction *defValue,
                                    SILInstruction *frontierInst) {
  SILBasicBlock::reverse_iterator End = bb->rend();
  SILBasicBlock::reverse_iterator iter = frontierInst->getReverseIterator();
  for (++iter; iter != End; ++iter) {
    SILInstruction *inst = &*iter;
    if (isa<DeallocRefInst>(inst))
      return true;
    if (inst == defValue)
      return false;
  }
  return false;
}

bool ValueLifetimeAnalysis::containsDeallocRef(const Frontier &frontier) {
  SmallPtrSet<SILBasicBlock *, 8> frontierBlocks;
  // Search in live blocks where the value is not alive until the end of the
  // block, i.e. the live range is terminated by a frontier instruction.
  for (SILInstruction *frontierInst : frontier) {
    SILBasicBlock *bb = frontierInst->getParent();
    if (blockContainsDeallocRef(bb, defValue, frontierInst))
      return true;
    frontierBlocks.insert(bb);
  }
  // Search in all other live blocks where the value is alive until the end of
  // the block.
  for (SILBasicBlock *bb : liveBlocks) {
    if (frontierBlocks.count(bb) == 0) {
      if (blockContainsDeallocRef(bb, defValue, bb->getTerminator()))
        return true;
    }
  }
  return false;
}

void ValueLifetimeAnalysis::dump() const {
  llvm::errs() << "lifetime of def: " << *defValue;
  for (SILInstruction *Use : userSet) {
    llvm::errs() << "  use: " << *Use;
  }
  llvm::errs() << "  live blocks:";
  for (SILBasicBlock *bb : liveBlocks) {
    llvm::errs() << ' ' << bb->getDebugID();
  }
  llvm::errs() << '\n';
}
