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
#include "swift/AST/AST.h"
#include "swift/CFG/CFG.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/APInt.h"
using namespace swift;

Instruction::Instruction(BasicBlock *B, InstKind Kind)
  : Kind(Kind), ParentBB(B) {
  B->instructions.push_back(this);
}


TermInst::Successors TermInst::successors() {
  switch (getKind()) {
  case InstKind::Call:
  case InstKind::DeclRef:
  case InstKind::IntegerLit:
  case InstKind::Load:
  case InstKind::ThisApply:
  case InstKind::Tuple:
  case InstKind::TypeOf:
    llvm_unreachable("Only TermInst's are allowed");
  case InstKind::Return:
    return Successors();
  case InstKind::CondBranch: {
    CondBranchInst &CBI = *cast<CondBranchInst>(this);
    return Successors(CBI.branches());
  }
  case InstKind::UncondBranch: {
    UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
    return Successors(UBI.targetBlock());
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
  : Instruction(B, InstKind::Call), NumArgs(args.size()), expr(expr),
    function(function) {
  memcpy(getArgsStorage(), args.data(), args.size() * sizeof(CFGValue));
}

CondBranchInst::CondBranchInst(Stmt *BranchStmt,
                               CFGValue condition,
                               BasicBlock *Target1,
                               BasicBlock *Target2,
                               BasicBlock *B)
  : TermInst(B, InstKind::CondBranch),
    branchStmt(BranchStmt), condition(condition) {
  Branches[0] = Target1;
  Branches[1] = Target2;
  memset(&Args, sizeof(Args), 0);
  for (auto branch : branches()) { if (branch) branch->addPred(B); }
}

TupleInst *TupleInst::create(TupleExpr *Expr,
                             ArrayRef<CFGValue> Elements,
                             BasicBlock *B) {
  CFG &cfg = *B->cfg;
  void *Buffer = cfg.allocate(sizeof(TupleInst) +
                              Elements.size() * sizeof(CFGValue),
                              llvm::AlignOf<TupleInst>::Alignment);
  return ::new(Buffer) TupleInst(Expr, Elements, B);
}

TupleInst::TupleInst(TupleExpr *Expr, ArrayRef<CFGValue> Elems, BasicBlock *B)
  : Instruction(B, InstKind::Tuple),  NumArgs(Elems.size()), expr(Expr) {
  memcpy(getElementsStorage(), Elems.data(), Elems.size() * sizeof(CFGValue));
}

void UncondBranchInst::unregisterTarget() {
  if (!TargetBlock)
    return;

  // ?
}

void UncondBranchInst::setTarget(BasicBlock *NewTarget, ArgsTy BlockArgs) {
  if (TargetBlock != NewTarget) {
    unregisterTarget();
    TargetBlock = NewTarget;
    TargetBlock->addPred(getParent());
  }

  // FIXME: check that TargetBlock's # args agrees with BlockArgs.

  if (BlockArgs.empty())
    return;

  // Copy the arguments over to our holding buffer.
  NumArgs = BlockArgs.size();
  Args = new (getParent()->cfg) CFGValue[NumArgs];
  ArgsTy(Args, NumArgs) = BlockArgs;
}
