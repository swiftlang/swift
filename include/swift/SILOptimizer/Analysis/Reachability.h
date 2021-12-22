//===--- Reachability.h ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Reachability data flow analysis using a path-discovery worklist. For
/// efficient data flow propagation based on a single SSA value and its uses.
///
/// TODO: Add an optimistic data flow for more aggresive optimization:
/// - Add another set for blocks reachable by barriers
/// - Change the meet operation to a union
/// - Propagate past barriers up to the SSA def
/// - Iterate to a fix-point.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_REACHABILITY_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_REACHABILITY_H

#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/SILBasicBlock.h"

namespace swift {

/// Pessimistic, non-iterative data flow for analyzing backward reachability
/// from a set of last uses to their dominating def or nearest barrier.
///
/// Intended for frequently called utilities where minimizing the cost of data
/// flow is more important than analyzing reachability across loops. Expected to
/// visit very few blocks because barriers often occur close to a last use.
///
/// BlockReachability {
///   // True if the beginning of \p block is reachable.
///   // Typically a BasicBlockSet wrapper.
///   bool hasReachableBegin(SILBasicBlock *block);
///
///   // Mark the beginning of a block reachable. Only called once per block.
///   // Typically a BasicBlockSet wrapper.
///   boid markReachableBegin(SILBasicBlock *block);
/// 
///   // Mark the end of a block reachable. Only called once per block.
///   // Typically a BasicBlockSet wrapper.
///   void markReachableEnd(SILBasicBlock *block);
///
///   // Return true if \p inst is a barrier. Called once for each reachable
///   // instruction, assuming that each lastUse is itself a barrier.
///   // Used by the data flow client to perform additional book-keeping,
///   // such as recording debug_value instructions.
///   bool checkReachableBarrier(SILInstruction *inst);
/// };
template <typename BlockReachability>
class BackwardReachability {
  SILFunction *function;
  BlockReachability &reachableBlocks;
  BasicBlockWorklist cfgWorklist;

public:
  BackwardReachability(SILFunction *function,
                       BlockReachability &reachableBlocks)
      : function(function), reachableBlocks(reachableBlocks),
        cfgWorklist(function) {}

  // Initialize data flow starting points before running solveBackward.
  void initLastUse(SILInstruction *lastUsePoint) {
    auto *lastUseBlock = lastUsePoint->getParent();
    if (canReachBlockBegin(lastUsePoint)) {
      pushPreds(lastUseBlock);
    }
  }

  // Data flow "meet": interesection of successor reachability.
  void solveBackward() {
    while (SILBasicBlock *block = cfgWorklist.popAndForget()) {
      if (!meetOverSuccessors(block))
        continue;

      reachableBlocks.markReachableEnd(block);

      if (canReachBlockBegin(block->getTerminator())) {
        pushPreds(block);
      }
    }
  }

protected:
  BackwardReachability(BackwardReachability const &) = delete;
  BackwardReachability &operator=(BackwardReachability const &) = delete;

  // Perform a "meet" over successor begin reachability.
  // Return true if \p predecessor's end is pessimistically reachable.
  //
  // Meet:
  // ReachableEnd(predecessor) := intersection(ReachableBegin, successors)
  bool meetOverSuccessors(SILBasicBlock *block) {
    return llvm::all_of(block->getSuccessorBlocks(), [this](auto *successor) {
      return reachableBlocks.hasReachableBegin(successor);
    });
  }

  // Local data flow. Computes the block's flow function.
  bool canReachBlockBegin(SILInstruction *lastReachablePoint) {
    do {
      if (reachableBlocks.checkReachableBarrier(lastReachablePoint))
        return false;
      lastReachablePoint = lastReachablePoint->getPreviousInstruction();
    } while (lastReachablePoint);
    return true;
  }

  // Propagate global data flow from \p succBB to its predecessors.
  void pushPreds(SILBasicBlock *succBB) {
    reachableBlocks.markReachableBegin(succBB);

    for (SILBasicBlock *predBB : succBB->getPredecessorBlocks()) {
      cfgWorklist.pushIfNotVisited(predBB);
    }
  }
};

} // end namespace swift

#endif
