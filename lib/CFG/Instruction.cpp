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
#include "swift/CFG/CFG.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/APInt.h"

using namespace swift;

Instruction::Instruction(BasicBlock *B, Kind K)
  : kind(K), basicBlock(B) {
  B->instructions.push_back(this);
}

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
    case Call:
    case DeclRef:
    case IntegerLit:
    case ThisApply:
    case TypeOf:
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
    case Call:
    case DeclRef:
    case IntegerLit:
    case ThisApply:
    case TypeOf:
      llvm_unreachable("Only TermInst's are allowed");
    case UncondBranch: {
      UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      return Successors(&UBI.targetBlock());
    }
  }
}

CallInst *CallInst::create(CallExpr *expr,
                           BasicBlock *B,
                           CFGValue function,
                           ArrayRef<CFGValue> args) {
  CFG &cfg = *B->cfg;
  void *Buffer = cfg.allocate(sizeof(CallInst) +
                              args.size() * sizeof(CFGValue),
                              llvm::AlignOf<CallInst>::Alignment);
  return ::new(Buffer) CallInst(expr, B, function, args);
}

CallInst::CallInst(CallExpr *expr, BasicBlock *B,
                   CFGValue function,
                   ArrayRef<CFGValue> args)
  : Instruction(B, Call), NumArgs(args.size()), expr(expr),
    function(function) {
  memcpy(getArgsStorage(), args.data(), args.size() * sizeof(CFGValue));
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
