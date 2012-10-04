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
// CFGValue Implementation
//===----------------------------------------------------------------------===//

//CFGType CFGValue::getType() const {
//  assert(isInstruction() && "getType() not implemented for BBArgs");
//  return getInst()->getType();
//}

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

DeclRefExpr *DeclRefInst::getExpr() const {
  return getLocExpr<DeclRefExpr>();
}

/// getDecl - Return the underlying declaration.
ValueDecl *DeclRefInst::getDecl() const {
  return getExpr()->getDecl();
}

IntegerLiteralExpr *IntegerLiteralInst::getExpr() const {
  return getLocExpr<IntegerLiteralExpr>();
}

/// getValue - Return the APInt for the underlying integer literal.
APInt IntegerLiteralInst::getValue() const {
  return getExpr()->getValue();
}


TypeOfExpr *TypeOfInst::getExpr() const {
  return getLocExpr<TypeOfExpr>();
}

/// getMetaType - Return the type of the metatype that this instruction
/// returns.
Type TypeOfInst::getMetaType() const {
  return getExpr()->getType();
}


TermInst::SuccessorListTy TermInst::getSuccessors() {
  switch (getKind()) {
  case InstKind::AllocVar:
  case InstKind::AllocTmp:
  case InstKind::Apply:
  case InstKind::DeclRef:
  case InstKind::IntegerLiteral:
  case InstKind::Load:
  case InstKind::Store:
  case InstKind::Requalify:
  case InstKind::ScalarToTuple:
  case InstKind::Tuple:
  case InstKind::TypeOf:
    llvm_unreachable("Only TermInst's are allowed");
  case InstKind::Unreachable:
    return cast<UnreachableInst>(this)->getSuccessors();
  case InstKind::Return:
    return cast<ReturnInst>(this)->getSuccessors();
  case InstKind::CondBranch:
    return cast<CondBranchInst>(this)->getSuccessors();
  case InstKind::Branch:
    return cast<BranchInst>(this)->getSuccessors();
  }
}

ApplyInst *ApplyInst::create(ApplyExpr *Expr, CFGValue Callee,
                             ArrayRef<CFGValue> Args, CFG &C) {
  void *Buffer = C.allocate(sizeof(ApplyInst) +
                            Args.size() * sizeof(CFGValue),
                            llvm::AlignOf<ApplyInst>::Alignment);
  return ::new(Buffer) ApplyInst(Expr, Callee, Args);
}

ApplyInst::ApplyInst(ApplyExpr *Expr, CFGValue Callee, ArrayRef<CFGValue> Args)
  : Instruction(InstKind::Apply, Expr), Callee(Callee), NumArgs(Args.size()) {
  memcpy(getArgsStorage(), Args.data(), Args.size() * sizeof(CFGValue));
}

CondBranchInst::CondBranchInst(Stmt *TheStmt, CFGValue Condition,
                               BasicBlock *TrueBB, BasicBlock *FalseBB)
  : TermInst(InstKind::CondBranch, TheStmt), Condition(Condition) {
  DestBBs[0].init(this);
  DestBBs[1].init(this);
  DestBBs[0] = TrueBB;
  DestBBs[1] = FalseBB;
}


TupleInst *TupleInst::create(TupleExpr *Expr, ArrayRef<CFGValue> Elements,
                             CFG &C) {
  void *Buffer = C.allocate(sizeof(TupleInst) +
                            Elements.size() * sizeof(CFGValue),
                            llvm::AlignOf<TupleInst>::Alignment);
  return ::new(Buffer) TupleInst(Expr, Elements);
}

TupleInst::TupleInst(TupleExpr *Expr, ArrayRef<CFGValue> Elems)
  : Instruction(InstKind::Tuple, Expr), NumArgs(Elems.size()) {
  memcpy(getElementsStorage(), Elems.data(), Elems.size() * sizeof(CFGValue));
}

