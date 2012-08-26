//===--- Condition.cpp - Implements the CFGLowering Condition class --------==//
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

#include "Condition.h"
#include "swift/CFG/CFG.h"
#include "swift/CFG/CFGBuilder.h"
using namespace swift;
using namespace CFGLowering;

/// emitBlock - Each basic block is individually new'd, then them emitted with
/// this function.  Since each block is implicitly added to the CFG's list of
/// blocks when created, the construction order is not particularly useful.
///
/// Instead, we want blocks to end up in the order that they are *emitted*.  The
/// cheapest way to ensure this is to just move each block to the end of the
/// block list when emitted: as later blocks are emitted, they'll be moved after
/// this, giving us a block list order that matches emission order when the
/// function is done.
///
/// This function also sets the insertion point of the builder to be the newly
/// emitted block.
static void emitBlock(CFGBuilder &B, BasicBlock *BB) {
  CFG *C = BB->getParent();
  // If this is a fall through into BB, emit the fall through branch.
  if (B.hasValidInsertionPoint())
    B.createBranch(BB);

  // Start inserting into that block.
  B.setInsertionPoint(BB);
  
  // Move block to the end of the list.
  if (&C->getBlocks().back() != BB)
    C->getBlocks().splice(C->end(), C->getBlocks(), BB);
}


void Condition::enterTrue(CFGBuilder &B) {
  assert(TrueBB && "Cannot call enterTrue without a True block!");
  assert(B.hasValidInsertionPoint());
  
  // TrueBB has already been inserted somewhere unless there's a
  // continuation block.
  if (!ContBB) return;
  
  emitBlock(B, TrueBB);
}

void Condition::exitTrue(CFGBuilder &B) {
  // If there's no continuation block, it's because the condition was
  // folded to true.  In that case, we just continue emitting code as
  // if we were still in the true case, and we're unreachable iff the
  // end of the true case is unreachable.  In other words, there's
  // nothing to do.
  if (!ContBB) {
    assert(!FalseBB && "no continuation");
    return;
  }
  
  // If there is a continuation block, we should branch to it if the
  // current point is reachable.
  if (!B.hasValidInsertionPoint()) {
    // If there is no false code, the continuation block has a use
    // because the main condition jumps directly to it.
    assert(ContBB->pred_empty() || !FalseBB);
    return;
  }
  
  // Otherwise, resume into the continuation block.  This branch might
  // be folded by exitFalse if it turns out that that point is
  // unreachable.
  B.createBranch(ContBB);
  
  // Coming out of exitTrue, we can be in one of three states:
  //   - a valid non-terminal IP, but only if there is no continuation
  //     block, which is only possible if there is no false block;
  //   - a valid terminal IP, if the end of the true block was reachable; or
  //   - a cleared IP, if the end of the true block was not reachable.
}

void Condition::enterFalse(CFGBuilder &B) {
  assert(FalseBB && "entering the false branch when it was not valid");
  
  // FalseBB has already been inserted somewhere unless there's a
  // continuation block.
  if (!ContBB) return;
  
  // It's possible to have no insertion point here if the end of the
  // true case was unreachable.
  emitBlock(B, FalseBB);
}

void Condition::exitFalse(CFGBuilder &B) {
  // If there's no continuation block, it's because the condition was
  // folded to false.  In that case, we just continue emitting code as
  // if we were still in the false case, and we're unreachable iff the
  // end of the false case is unreachable.  In other words, there's
  // nothing to do.
  if (!ContBB) return;
  
  if (ContBB->pred_empty()) {
    // If the true case didn't need the continuation block, then
    // we don't either, regardless of whether the current location
    // is reachable.  Just keep inserting / being unreachable
    // right where we are.
  } else if (!B.hasValidInsertionPoint()) {
    // If the true case did need the continuation block, but the false
    // case doesn't, just merge the continuation block back into its
    // single predecessor and move the IP there.
    //
    // Note that doing this tends to strand the false code after
    // everything else in the function, so maybe it's not a great idea.
    auto PI = ContBB->pred_begin();
    BasicBlock *ContPred = *PI;

    // Verify there was only a single predecessor to ContBB.
    ++PI;
    assert(PI == ContBB->pred_end() && "Only expect one branch to the ContBB");
    
    // Insert before the uncond branch and zap it.
    auto *Br = cast<BranchInst>(ContPred->getTerminator());
    B.setInsertionPoint(Br->getParent());
    Br->eraseFromParent();
    assert(ContBB->pred_empty() &&"Zapping the branch should make ContBB dead");
    
    // Otherwise, branch to the continuation block and start inserting there.
  } else {
    B.createBranch(ContBB);
  }
}

void Condition::complete(CFGBuilder &B) {
  // If there is no continuation block, it's because we
  // constant-folded the branch.  The case-exit will have left us in a
  // normal insertion state (i.e. not a post-terminator IP) with
  // nothing to clean up after.
  if (!ContBB) {
    assert(B.hasValidInsertionPoint());
    return;
  }
  
  // Kill the continuation block if it's not being used.  Case-exits
  // only leave themselves post-terminator if they use the
  // continuation block, so we're in an acceptable insertion state.
  if (ContBB->pred_empty()) {
    assert(B.hasValidInsertionPoint());
    ContBB->eraseFromParent();
    return;
  }
  
  // Okay, we need to insert the continuation block.
  emitBlock(B, ContBB);
}


