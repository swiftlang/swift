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
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBasicBlock.h"

using namespace swift;

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
