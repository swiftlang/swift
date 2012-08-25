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

//===----------------------------------------------------------------------===//
// ilist_traits<Instruction> Implementation
//===----------------------------------------------------------------------===//

// The trait object is embedded into a basic block.  Use dirty hacks to
// reconstruct the BB from the 'this' pointer of the trait.
BasicBlock *llvm::ilist_traits<Instruction>::getContainingBlock() {
  typedef iplist<Instruction> BasicBlock::*Sublist;
 size_t Offset(size_t(&((BasicBlock*)0->*BasicBlock::getSublistAccess())));
  iplist<Instruction>* Anchor(static_cast<iplist<Instruction>*>(this));
  return reinterpret_cast<BasicBlock*>(reinterpret_cast<char*>(Anchor)-Offset);
}


void llvm::ilist_traits<Instruction>::addNodeToList(Instruction *I) {
  assert(I->ParentBB == 0 && "Already in a list!");
  I->ParentBB = getContainingBlock();
}

void llvm::ilist_traits<Instruction>::removeNodeFromList(Instruction *I) {
  // When an instruction is removed from a BB, clear the parent pointer.
  assert(I->ParentBB && "Not in a list!");
  I->ParentBB = 0;
}

void llvm::ilist_traits<Instruction>::
transferNodesFromList(llvm::ilist_traits<Instruction> &L2,
                      llvm::ilist_iterator<Instruction> first,
                      llvm::ilist_iterator<Instruction> last) {
  // If transfering instructions within the same basic block, no reason to
  // update their parent pointers.
  BasicBlock *ThisParent = getContainingBlock();
  if (ThisParent == L2.getContainingBlock()) return;

  // Update the parent fields in the instructions.
  for (; first != last; ++first)
    first->ParentBB = ThisParent;
}


//===----------------------------------------------------------------------===//
// Instruction Implementation
//===----------------------------------------------------------------------===//


Instruction::Instruction(BasicBlock *B, InstKind Kind)
  : Kind(Kind), ParentBB(0) {
  B->getInsts().push_back(this);
}


TermInst::Successors TermInst::successors() {
  switch (getKind()) {
  case InstKind::Call:
  case InstKind::DeclRef:
  case InstKind::IntegerLiteral:
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

CallInst *CallInst::create(CallExpr *expr, CFGValue function,
                           ArrayRef<CFGValue> args, CFG &C) {
  void *Buffer = C.allocate(sizeof(CallInst) +
                            args.size() * sizeof(CFGValue),
                            llvm::AlignOf<CallInst>::Alignment);
  return ::new(Buffer) CallInst(expr, function, args);
}

CallInst::CallInst(CallExpr *expr, CFGValue function, ArrayRef<CFGValue> args)
  : Instruction(InstKind::Call), NumArgs(args.size()), expr(expr),
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

CondBranchInst::CondBranchInst(Stmt *BranchStmt,
                               CFGValue condition,
                               BasicBlock *Target1,
                               BasicBlock *Target2)
  : TermInst(InstKind::CondBranch), branchStmt(BranchStmt),
    condition(condition) {
  Branches[0] = Target1;
  Branches[1] = Target2;
  memset(&Args, sizeof(Args), 0);
}


TupleInst *TupleInst::create(TupleExpr *Expr, ArrayRef<CFGValue> Elements,
                             CFG &C) {
  void *Buffer = C.allocate(sizeof(TupleInst) +
                            Elements.size() * sizeof(CFGValue),
                            llvm::AlignOf<TupleInst>::Alignment);
  return ::new(Buffer) TupleInst(Expr, Elements);
}

TupleInst::TupleInst(TupleExpr *Expr, ArrayRef<CFGValue> Elems)
  : Instruction(InstKind::Tuple),  NumArgs(Elems.size()), expr(Expr) {
  memcpy(getElementsStorage(), Elems.data(), Elems.size() * sizeof(CFGValue));
}

void UncondBranchInst::unregisterTarget() {
  if (!TargetBlock)
    return;

  // ?
}

void UncondBranchInst::setTarget(BasicBlock *NewTarget, ArgsTy BlockArgs,
                                 CFG &C) {
  // FIXME: This isn't right if the args are changing.
  if (TargetBlock != NewTarget) {
    unregisterTarget();
    TargetBlock = NewTarget;
    TargetBlock->addPred(getParent());
  }

  // FIXME: check that TargetBlock's # args agrees with BlockArgs.

  if (BlockArgs.empty())
    return;

  assert(0 && "FIXME: Unimplemented");
  
#if 0
  // Copy the arguments over to our holding buffer.
  NumArgs = BlockArgs.size();
  // FIXME: Allocate memory from CFG.
  Args = new (getParent()->getParent()) CFGValue[NumArgs];
  ArgsTy(Args, NumArgs) = BlockArgs;
#endif
}
