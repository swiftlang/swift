//===--- SILBasicBlock.cpp - Basic blocks for high-level SIL code ----------==//
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
//
// This file defines the high-level BasicBlocks used for Swift SIL code.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// SILBasicBlock Implementation
//===----------------------------------------------------------------------===//

SILBasicBlock::SILBasicBlock(SILFunction *parent, SILBasicBlock *afterBB)
  : Parent(parent), PredList(0) {
  if (afterBB) {
    parent->getBlocks().insertAfter(afterBB, this);
  } else {
    parent->getBlocks().push_back(this);
  }
}
SILBasicBlock::~SILBasicBlock() {
  // iplist's destructor is going to destroy the InstList.
}

int SILBasicBlock::getDebugID() {
  if (!getParent())
    return -1;
  int idx = 0;
  for (const SILBasicBlock &B : *getParent()) {
    if (&B == this)
      return idx;
    idx++;
  }
  llvm_unreachable("block not in function's block list");
}

SILModule &SILBasicBlock::getModule() const {
  return getParent()->getModule();
}

/// This method unlinks 'self' from the containing SILFunction and deletes it.
void SILBasicBlock::eraseFromParent() {
  getParent()->getBlocks().erase(this);
}

/// This method unlinks 'self' from the containing SILFunction.
void SILBasicBlock::removeFromParent() {
  getParent()->getBlocks().remove(this);
}


/// Replace the ith BB argument with a new one with type Ty (and optional
/// ValueDecl D).
SILArgument *SILBasicBlock::replaceBBArg(unsigned i, SILType Ty,
                                         const ValueDecl *D) {
  SILModule &M = getParent()->getModule();
  assert(BBArgList[i]->use_empty() && "Expected no uses of the old BB arg!");

  auto *NewArg = new (M) SILArgument(Ty, D);
  NewArg->setParent(this);
  BBArgList[i] = NewArg;

  return NewArg;
}

SILArgument *SILBasicBlock::createBBArg(SILType Ty, const ValueDecl *D) {
  return new (getModule()) SILArgument(this, Ty, D);
}

SILArgument *SILBasicBlock::insertBBArg(bbarg_iterator Iter, SILType Ty,
                                        const ValueDecl *D) {
  return new (getModule()) SILArgument(this, Iter, Ty, D);
}

/// \brief Splits a basic block into two at the specified instruction.
///
/// Note that all the instructions BEFORE the specified iterator
/// stay as part of the original basic block. The old basic block is left
/// without a terminator.
SILBasicBlock *SILBasicBlock::splitBasicBlock(iterator I) {
  SILBasicBlock *New = new (Parent->getModule()) SILBasicBlock(Parent);
  SILFunction::iterator Where = std::next(SILFunction::iterator(this));
  SILFunction::iterator First = SILFunction::iterator(New);
  if (Where != First)
    Parent->getBlocks().splice(Where, Parent->getBlocks(), First);
  // Move all of the specified instructions from the original basic block into
  // the new basic block.
  New->getInstList().splice(New->end(), this->getInstList(), I, end());
  return New;
}

/// \brief Splits a basic block into two at the specified instruction and
/// inserts an unconditional branch from the old basic block to the new basic
/// block.
SILBasicBlock *SILBasicBlock::splitBasicBlockAndBranch(iterator I,
                                                       SILLocation BranchLoc) {
  SILBasicBlock *New = splitBasicBlock(I);
  getInstList().insert(getInstList().end(),
                       BranchInst::create(BranchLoc, New, *getParent()));
  return New;
}

/// \brief Move the basic block to after the specified basic block in the IR.
void SILBasicBlock::moveAfter(SILBasicBlock *After) {
  assert(getParent() && getParent() == After->getParent() &&
         "Blocks must be in the same function");
  auto InsertPt = std::next(SILFunction::iterator(After));
  auto &BlkList = getParent()->getBlocks();
  BlkList.splice(InsertPt, BlkList, this);
}

void
llvm::ilist_traits<swift::SILBasicBlock>::
transferNodesFromList(llvm::ilist_traits<SILBasicBlock> &SrcTraits,
                      llvm::ilist_iterator<SILBasicBlock> First,
                      llvm::ilist_iterator<SILBasicBlock> Last) {
  assert(&Parent->getModule() == &SrcTraits.Parent->getModule() &&
         "Module mismatch!");

  // If we are asked to splice into the same function, don't update parent
  // pointers.
  if (Parent == SrcTraits.Parent) return;

  // If splicing blocks not in the same function, update the parent pointers.
  for (; First != Last; ++First) {
    First->Parent = Parent;
    for (auto &II : *First) {
      II.setDebugScope(Parent->getDebugScope());
    }
  }
}
