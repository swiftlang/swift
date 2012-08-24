//===--- Verifier - Verification of Swift CFGs -------------------*- C++ -*-==//
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
// This file defines the high-level Instruction classes used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#include "swift/CFG/CFG.h"
using namespace swift;

static void verifyInst(const Instruction *I) {
  // Check that non-terminators look ok.
  if (!isa<TermInst>(I)) {
    assert(!I->basicBlock->instructions.empty() &&
           "Can't be in a parent block if it is empty");
    assert(&*I->basicBlock->instructions.rbegin() != I &&
           "Non-terminators cannot be the last in a block");
  } else {
    assert(&*I->basicBlock->instructions.rbegin() == I &&
           "Terminator must be the last in block");
  }
  
  // FIXME: This should switch to an InstVisitor.
  switch (I->kind) {
  case Instruction::Call:
  case Instruction::DeclRef:
  case Instruction::IntegerLit:
  case Instruction::Load:
  case Instruction::Return:
  case Instruction::ThisApply:
  case Instruction::Tuple:
  case Instruction::TypeOf:
    return;
  case Instruction::CondBranch:
    break;
  case Instruction::UncondBranch: {
    // FIXME: Generalize this to support all terminators in the generic
    // terminator case.  Just diff "successors()" of the TermInst vs the block.
    // why redundantly store block terminators in the first place?
    
    const UncondBranchInst &UBI = *cast<UncondBranchInst>(I);
    const BasicBlock &targetBlock = *UBI.targetBlock();
    assert(std::find(targetBlock.preds().begin(), targetBlock.preds().end(),
                     I->basicBlock) &&
           "BasicBlock of UncondBranchInst must be a predecessor of target");
    (void)targetBlock;
  }
  }
}





/// verify - Run the IR verifier to make sure that the CFG follows invariants.
void CFG::verify() const {
  for (auto &BB : blocks)
    for (auto &I : BB.instructions)
      verifyInst(&I);
  
}
