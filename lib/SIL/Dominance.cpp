//===--- Dominance.cpp - SIL basic block dominance analysis ---------------===//
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

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/Dominance.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/DominatorInternals.h"

using namespace swift;

template class llvm::DominatorTreeBase<SILBasicBlock>;
template class llvm::DominatorBase<SILBasicBlock>;
template class llvm::DomTreeNodeBase<SILBasicBlock>;

/// Compute the immmediate-dominators map.
DominanceInfo::DominanceInfo(SILFunction *F)
    : DominatorTreeBase(/*isPostDom*/ false) {
  recalculate(*F);
}

bool DominanceInfo::properlyDominates(SILInstruction *a, SILInstruction *b) {
  auto aBlock = a->getParent(), bBlock = b->getParent();

  // If the blocks are different, it's as easy as whether A's block
  // dominates B's block.
  if (aBlock != bBlock)
    return properlyDominates(a->getParent(), b->getParent());

  // Otherwise, they're in the same block, and we just need to check
  // whether B comes after A.  This is a non-strict computation.
  do {
    b = b->getPrevNode();
    if (a == b) return true;
  } while (b != nullptr);

  return false;
}
