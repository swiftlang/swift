//===--- PrunedLiveness.cpp - Compute liveness from selected uses ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/PrunedLiveness.h"
#include "swift/SIL/OwnershipUtils.h"

using namespace swift;

/// Mark blocks live during a reverse CFG traversal from one specific block
/// containing a user.
void PrunedLiveBlocks::computeUseBlockLiveness(SILBasicBlock *userBB) {
  // If we are visiting this block, then it is not already LiveOut. Mark it
  // LiveWithin to indicate a liveness boundary within the block.
  markBlockLive(userBB, LiveWithin);

  SmallVector<SILBasicBlock *, 8> predBBWorklist({userBB});
  while (!predBBWorklist.empty()) {
    SILBasicBlock *bb = predBBWorklist.pop_back_val();

    // The popped `bb` is live; now mark all its predecessors LiveOut.
    //
    // Traversal terminates at any previously visited block, including the
    // blocks initialized as definition blocks.
    for (auto *predBB : bb->getPredecessorBlocks()) {
      switch (getBlockLiveness(predBB)) {
      case Dead:
        predBBWorklist.push_back(predBB);
        LLVM_FALLTHROUGH;
      case LiveWithin:
        markBlockLive(predBB, LiveOut);
        break;
      case LiveOut:
        break;
      }
    }
  }
}

/// Update the current def's liveness based on one specific use instruction.
///
/// Return the updated liveness of the \p use block (LiveOut or LiveWithin).
///
/// Terminators are not live out of the block.
PrunedLiveBlocks::IsLive PrunedLiveBlocks::updateForUse(SILInstruction *user) {
  SWIFT_ASSERT_ONLY(seenUse = true);

  auto *bb = user->getParent();
  switch (getBlockLiveness(bb)) {
  case LiveOut:
    return LiveOut;
  case LiveWithin:
    return LiveWithin;
  case Dead: {
    // This use block has not yet been marked live. Mark it and its predecessor
    // blocks live.
    computeUseBlockLiveness(bb);
    return getBlockLiveness(bb);
  }
  }
  llvm_unreachable("covered switch");
}

//===----------------------------------------------------------------------===//
//                            MARK: PrunedLiveness
//===----------------------------------------------------------------------===//

void PrunedLiveness::updateForUse(SILInstruction *user, bool lifetimeEnding) {
  auto useBlockLive = liveBlocks.updateForUse(user);
  // Record all uses of blocks on the liveness boundary. For blocks marked
  // LiveWithin, the boundary is considered to be the last use in the block.
  if (!lifetimeEnding && useBlockLive == PrunedLiveBlocks::LiveOut) {
    return;
  }
  // Note that a user may use the current value from multiple operands. If any
  // of the uses are non-lifetime-ending, then we must consider the user
  // itself non-lifetime-ending; it cannot be a final destroy point because
  // the value of the non-lifetime-ending operand must be kept alive until the
  // end of the user. Consider a call that takes the same value using
  // different conventions:
  //
  //   apply %f(%val, %val) : $(@guaranteed, @owned) -> ()
  //
  // This call is not considered the end of %val's lifetime. The @owned
  // argument must be copied.
  auto iterAndSuccess = users.try_emplace(user, lifetimeEnding);
  if (!iterAndSuccess.second)
    iterAndSuccess.first->second &= lifetimeEnding;
}

bool PrunedLiveness::updateForBorrowingOperand(Operand *op) {
  assert(op->getOperandOwnership() == OperandOwnership::Borrow);

  // A nested borrow scope is considered a use-point at each scope ending
  // instruction.
  //
  // TODO: Handle reborrowed copies by considering the extended borrow
  // scope. Temporarily bail-out on reborrows because we can't handle uses
  // that aren't dominated by currentDef.
  if (!BorrowingOperand(op).visitScopeEndingUses(
        [this](Operand *end) {
          if (end->getOperandOwnership() == OperandOwnership::Reborrow) {
            return false;
          }
          updateForUse(end->getUser(), /*lifetimeEnding*/ false);
          return true;
        })) {
    return false;
  }
  return true;
}
