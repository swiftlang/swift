//===--- Instruction.cpp - Instructions for high-level CFGs ------*- C++ -*-==//
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

#include "swift/AST/AST.h"
#include "swift/CFG/Instruction.h"
#include "swift/CFG/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/APInt.h"
#include <algorithm>

using namespace swift;

void Instruction::print(raw_ostream &OS) const {
  switch (kind) {
    case Invalid:
      OS << "InvalidInstruction";
      break;
    case IntegerLit: {
      const IntegerLiteralInst &ILE = *cast<IntegerLiteralInst>(this);
      OS << "IntegerLiteralInst " << ILE.literal->getValue();
      break;
    }
    case UncondBranch: {
      const UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      OS << "br " << UBI.targetBlock();
      const UncondBranchInst::ArgsTy Args = UBI.blockArgs();
      if (!Args.empty()) {
        OS << '(';
        for (auto Arg : Args) { OS << "%" << Arg; }
        OS << ')';
      }
      break;
    }
  }
  OS << '\n';
}

void Instruction::dump() const { print(llvm::errs()); }

void Instruction::validateNonTerm() const {
  assert(basicBlock->instructions.size() > 1);
  assert(&*basicBlock->instructions.rbegin() != this &&
         "Non-terminator Instructions cannot be the last in a block");
}

void Instruction::validate() const {
  if (kind > Invalid && kind < TERM_INST_BEGIN)
    validateNonTerm();

  switch (kind) {
    case Invalid:
    case IntegerLit:
      return;
    case UncondBranch: {
      const UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      assert(!basicBlock->instructions.empty() &&
             &*basicBlock->instructions.rbegin() == this &&
             "UncondBranchInst must appear at end of BasicBlock");
      const BasicBlock &targetBlock = UBI.targetBlock();
      assert(std::find(targetBlock.preds().begin(), targetBlock.preds().end(),
                       basicBlock) &&
             "BasicBlock of UncondBranchInst must be a predecessor of target");
      (void)targetBlock;
    }
  }
}

TermInst::Successors TermInst::successors() {
  switch (kind) {
    case Invalid:
    case IntegerLit:
      llvm_unreachable("Only TermInst's are allowed");
    case UncondBranch: {
      UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      return Successors(&UBI.targetBlock());
    }
  }
}

void UncondBranchInst::unregisterTarget() {
  if (!TargetBlock)
    return;

}

void UncondBranchInst::setTarget(BasicBlock *NewTarget, const ArgsTy BlockArgs){
  if (TargetBlock != NewTarget) {
    unregisterTarget();
    TargetBlock = NewTarget;
    TargetBlock->addPred(basicBlock);
  }

  // FIXME: check that TargetBlock's # args agrees with BlockArgs.

  if (BlockArgs.empty())
    return;

  // Copy the arguments over to our holding buffer.
  NumArgs = BlockArgs.size();
  Args = new (basicBlock->cfg) unsigned[NumArgs];
  ArgsTy(Args, NumArgs) = BlockArgs;
}
