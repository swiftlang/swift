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

UncondBranchInst::UncondBranchInst(BasicBlock &SrcBlk,
                                   BasicBlock &DstBlk,
                                   llvm::ArrayRef<unsigned> BArgs)
  : TermInst(&SrcBlk, UncondBranch),
    Args(nullptr), NumArgs(0),
    targetBlock(DstBlk)
{
  DstBlk.addPred(&SrcBlk);

  if (BArgs.empty())
    return;
  NumArgs = BArgs.size();
  Args = new (targetBlock.cfg) unsigned[NumArgs];
  llvm::ArrayRef<unsigned>(Args, NumArgs) = BArgs;
}

void Instruction::print(llvm::raw_ostream &OS) const {
  switch (kind) {
    case Invalid:
      OS << "InvalidInstruction";
      return;
    case UncondBranch: {
      const UncondBranchInst &UBI = *llvm::cast<UncondBranchInst>(this);
      OS << "br " << UBI.targetBlock;
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
      const BasicBlock &targetBlock = UBI.targetBlock;
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
      const UncondBranchInst &UBI = *llvm::cast<UncondBranchInst>(this);
      return Successors(&UBI.targetBlock);
    }
  }
}
