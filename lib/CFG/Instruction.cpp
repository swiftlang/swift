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
#include <algorithm>

using namespace swift;

Instruction::~Instruction() {}
void Instruction::print(llvm::raw_ostream &OS) const {}
void Instruction::dump() const { print(llvm::errs()); }
void Instruction::validate() const {}

TermInst::~TermInst() {}

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

UncondBranchInst::~UncondBranchInst() {}

void UncondBranchInst::print(llvm::raw_ostream &OS) const {
  OS << "br " << *basicBlock;
  const ArgsTy Args = blockArgs();
  if (!Args.empty()) {
    OS << '(';
    for (auto Arg : Args) { OS << "%" << Arg; }
    OS << ')';
  }
}

void UncondBranchInst::validate() const {
  assert(!basicBlock->instructions.empty() &&
         &*basicBlock->instructions.rbegin() == this &&
         "UncondBranchInst must appear at end of BasicBlock");
  assert(std::find(targetBlock.preds().begin(), targetBlock.preds().end(),
                   basicBlock) &&
         "BasicBlock of UncondBranchInst must be a predecessor of target");
}