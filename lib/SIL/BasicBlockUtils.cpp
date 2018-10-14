//===--- BasicBlockUtils.cpp - Utilities for SILBasicBlock ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;

/// Merge the basic block with its successor if possible.
void swift::mergeBasicBlockWithSingleSuccessor(SILBasicBlock *BB,
                                               SILBasicBlock *succBB) {
  auto *BI = cast<BranchInst>(BB->getTerminator());
  assert(succBB->getSinglePredecessorBlock());

  // If there are any BB arguments in the destination, replace them with the
  // branch operands, since they must dominate the dest block.
  for (unsigned i = 0, e = BI->getArgs().size(); i != e; ++i)
    succBB->getArgument(i)->replaceAllUsesWith(BI->getArg(i));

  BI->eraseFromParent();

  // Move the instruction from the successor block to the current block.
  BB->spliceAtEnd(succBB);

  succBB->eraseFromParent();
}

//===----------------------------------------------------------------------===//
//                              DeadEndBlocks
//===----------------------------------------------------------------------===//

void DeadEndBlocks::compute() {
  assert(ReachableBlocks.empty() && "Computed twice");

  // First step: find blocks which end up in a no-return block (terminated by
  // an unreachable instruction).
  // Search for function-exiting blocks, i.e. return and throw.
  for (const SILBasicBlock &BB : *F) {
    const TermInst *TI = BB.getTerminator();
    if (TI->isFunctionExiting())
      ReachableBlocks.insert(&BB);
  }
  // Propagate the reachability up the control flow graph.
  unsigned Idx = 0;
  while (Idx < ReachableBlocks.size()) {
    const SILBasicBlock *BB = ReachableBlocks[Idx++];
    for (SILBasicBlock *Pred : BB->getPredecessorBlocks())
      ReachableBlocks.insert(Pred);
  }
}
