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
// SILArgument Implementation
//===----------------------------------------------------------------------===//

SILArgument::SILArgument(SILType Ty, SILBasicBlock *ParentBB, ValueDecl *D)
  : ValueBase(ValueKind::SILArgument, Ty), ParentBB(ParentBB), Decl(D) {
  ParentBB->addArgument(this);
}


SILFunction *SILArgument::getFunction() {
  return getParent()->getParent();
}
const SILFunction *SILArgument::getFunction() const {
  return getParent()->getParent();
}

SILModule &SILArgument::getModule() const {
  return getFunction()->getModule();
}

//===----------------------------------------------------------------------===//
// SILBasicBlock Implementation
//===----------------------------------------------------------------------===//

SILBasicBlock::SILBasicBlock(SILFunction *Parent)
  : Parent(Parent), PredList(0) {
  Parent->getBlocks().push_back(this);
}
SILBasicBlock::~SILBasicBlock() {
  // iplist's destructor is going to destroy the InstList.
}

SILModule &SILBasicBlock::getModule() const {
  return getParent()->getModule();
}

/// eraseFromParent - This method unlinks 'self' from the containing SIL and
/// deletes it.
///
void SILBasicBlock::eraseFromParent() {
  getParent()->getBlocks().erase(this);
}

/// \brief Splits a basic block into two at the specified instruction.
///
/// Note that all the instructions BEFORE the specified iterator
/// stay as part of the original basic block. The old basic block is left
/// without a terminator.
SILBasicBlock *SILBasicBlock::splitBasicBlock(iterator I) {
  SILBasicBlock *New = new (Parent->getModule()) SILBasicBlock(Parent);
  SILFunction::iterator Where = llvm::next(SILFunction::iterator(this));
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
