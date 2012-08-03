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

#include "swift/CFG/Instruction.h"
#include "swift/CFG/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include <algorithm>

using namespace swift;

void Instruction::print(llvm::raw_ostream &OS) const {
  switch (kind) {
    case Invalid:
      OS << "InvalidInstruction";
      return;
    case UncondBranch: {
      const UncondBranchInst &UBI = *llvm::cast<UncondBranchInst>(this);
      OS << "br " << UBI.targetBlock();
      const UncondBranchInst::ArgsTy Args = UBI.blockArgs();
      if (!Args.empty()) {
        OS << '(';
        for (auto Arg : Args) { OS << "%" << Arg; }
        OS << ')';
      }
      return;
    }
  }
}

void Instruction::dump() const { print(llvm::errs()); }

void Instruction::validate() const {
  switch (kind) {
    case Invalid:
      return;
    case UncondBranch: {
      const UncondBranchInst &UBI = *llvm::cast<UncondBranchInst>(this);
      assert(!basicBlock->instructions.empty() &&
             &*basicBlock->instructions.rbegin() == this &&
             "UncondBranchInst must appear at end of BasicBlock");
      const BasicBlock &targetBlock = UBI.targetBlock();
      assert(std::find(targetBlock.preds().begin(), targetBlock.preds().end(),
                       basicBlock) &&
             "BasicBlock of UncondBranchInst must be a predecessor of target");
    }
  }
}

TermInst::Successors TermInst::successors() {
  switch (kind) {
    case Invalid:
      llvm_unreachable("Only TermInst's are allowed");
    case UncondBranch: {
      UncondBranchInst &UBI = *llvm::cast<UncondBranchInst>(this);
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
