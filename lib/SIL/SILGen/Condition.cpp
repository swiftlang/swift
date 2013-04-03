//===--- Condition.cpp - Implements the SILGen Condition class ------------===//
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
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBuilder.h"
using namespace swift;
using namespace Lowering;

void Condition::enterTrue(SILBuilder &B) {
  assert(TrueBB && "Cannot call enterTrue without a True block!");
  
  // TrueBB has already been inserted somewhere unless there's a
  // continuation block.
  if (!ContBB) return;
  
  B.emitBlock(TrueBB);
}

void Condition::exitTrue(SILBuilder &B, ArrayRef<SILValue> Args) {
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
  B.createBranch(SILLocation(), ContBB, Args);
  
  // Coming out of exitTrue, we can be in one of three states:
  //   - a valid non-terminal IP, but only if there is no continuation
  //     block, which is only possible if there is no false block;
  //   - a valid terminal IP, if the end of the true block was reachable; or
  //   - a cleared IP, if the end of the true block was not reachable.
}

void Condition::enterFalse(SILBuilder &B) {
  assert(FalseBB && "entering the false branch when it was not valid");
  
  // FalseBB has already been inserted somewhere unless there's a
  // continuation block.
  if (!ContBB) return;
  
  // It's possible to have no insertion point here if the end of the
  // true case was unreachable.
  B.emitBlock(FalseBB);
}

void Condition::exitFalse(SILBuilder &B, ArrayRef<SILValue> Args) {
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
    SILBasicBlock *ContPred = *PI;

    // Verify there was only a single predecessor to ContBB.
    ++PI;
    assert(PI == ContBB->pred_end() && "Only expect one branch to the ContBB");
    
    // Insert before the uncond branch and zap it.
    auto *Br = cast<BranchInst>(ContPred->getTerminator());
    B.setInsertionPoint(Br->getParent());

    Br->eraseFromParent();
    assert(ContBB->pred_empty() &&
           "Zapping the branch should make ContBB dead");
  } else {
    // Otherwise, branch to the continuation block and start inserting there.
    B.createBranch(SILLocation(), ContBB, Args);
  }
}

SILBasicBlock *Condition::complete(SILBuilder &B) {
  // If there is no continuation block, it's because we
  // constant-folded the branch.  The case-exit will have left us in a
  // normal insertion state (i.e. not a post-terminator IP) with
  // nothing to clean up after.
  if (!ContBB) {
    return B.getInsertionBB();
  }
  
  // Kill the continuation block if it's not being used.  Case-exits
  // only leave themselves post-terminator if they use the
  // continuation block, so we're in an acceptable insertion state.
  if (ContBB->pred_empty()) {
    ContBB->eraseFromParent();
    return B.hasValidInsertionPoint() ? B.getInsertionBB() : nullptr;
  }
  
  // Okay, we need to insert the continuation block.
  B.emitBlock(ContBB);
  return ContBB;
}


