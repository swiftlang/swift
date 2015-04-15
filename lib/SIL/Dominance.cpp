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
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/GenericDomTreeConstruction.h"

using namespace swift;

template class llvm::DominatorTreeBase<SILBasicBlock>;
template class llvm::DominatorBase<SILBasicBlock>;
template class llvm::DomTreeNodeBase<SILBasicBlock>;

/// Compute the immmediate-dominators map.
DominanceInfo::DominanceInfo(SILFunction *F)
    : DominatorTreeBase(/*isPostDom*/ false) {
      assert(!F->isExternalDeclaration() &&
             "Make sure the function is a definition and not a declaration.");
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
  SILInstruction *f = aBlock->begin();
  while (b != f) {
    b = b->getPrevNode();
    if (a == b) return true;
  }

  return false;
}

void DominanceInfo::verify() const {
  // Recompute.
  auto *F = getRoot()->getParent();
  DominanceInfo OtherDT(F);

  // And compare.
  if (errorOccuredOnComparison(OtherDT)) {
    llvm::errs() << "DominatorTree is not up to date!\nComputed:\n";
    print(llvm::errs());
    llvm::errs() << "\nActual:\n";
    OtherDT.print(llvm::errs());
    abort();
  }
}

/// Compute the immmediate-post-dominators map.
PostDominanceInfo::PostDominanceInfo(SILFunction *F)
  : DominatorTreeBase(/*isPostDom*/ true) {
  recalculate(*F);
}

bool
PostDominanceInfo::
properlyDominates(SILInstruction *I1, SILInstruction *I2) {
  SILBasicBlock *BB1 = I1->getParent(), *BB2 = I2->getParent();

  // If the blocks are different, it's as easy as whether BB1 post dominates
  // BB2.
  if (BB1 != BB2)
    return properlyDominates(BB1, BB2);

  // Otherwise, they're in the same block, and we just need to check
  // whether A comes after B.
  for (SILBasicBlock::iterator II = I1, IE = BB1->end(); II != IE; ++II) {
    if (&*II == I2) {
      return false;
    }
  }

  return true;
}
