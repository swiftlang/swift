//===--- Instruction.cpp - Instructions for high-level CFGs ----------------==//
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

/// removeFromParent - This method unlinks 'this' from the containing basic
/// block, but does not delete it.
///
void Instruction::removeFromParent() {
  getParent()->getInsts().remove(this);
}

/// eraseFromParent - This method unlinks 'this' from the containing basic
/// block and deletes it.
///
void Instruction::eraseFromParent() {
  getParent()->getInsts().erase(this);
}


TermInst::SuccessorListTy TermInst::getSuccessors() {
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
    return cast<ReturnInst>(this)->getSuccessors();
  case InstKind::CondBranch:
    return cast<CondBranchInst>(this)->getSuccessors();
  case InstKind::UncondBranch:
    return cast<UncondBranchInst>(this)->getSuccessors();
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

CondBranchInst::CondBranchInst(Stmt *BranchStmt, CFGValue condition,
                               BasicBlock *TrueBB, BasicBlock *FalseBB)
  : TermInst(InstKind::CondBranch), branchStmt(BranchStmt),
    condition(condition) {
  DestBBs[0].init(this);
  DestBBs[1].init(this);
  DestBBs[0] = TrueBB;
  DestBBs[1] = FalseBB;
      
  // FIXME: Args?
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

