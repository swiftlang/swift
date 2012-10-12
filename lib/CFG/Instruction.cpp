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
#include "llvm/ADT/APFloat.h"
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

//===----------------------------------------------------------------------===//
// Instruction Subclasses
//===----------------------------------------------------------------------===//

static Type getVoidType(Type T) {
  return T->getASTContext().TheEmptyTupleType;
}

AllocVarInst::AllocVarInst(VarDecl *VD)
  : AllocInst(ValueKind::AllocVar, VD, VD->getTypeOfReference()) {
}

/// getDecl - Return the underlying declaration.
VarDecl *AllocVarInst::getDecl() const {
  return getLocDecl<VarDecl>();
}


AllocTmpInst::AllocTmpInst(MaterializeExpr *E)
  : AllocInst(ValueKind::AllocTmp, E, E->getType()) {}

// AllocArray always returns a tuple (Builtin.ObjectPointer, lvalue[EltTy])
static Type getAllocArrayType(Type EltTy) {
  ASTContext &Ctx = EltTy->getASTContext();;

  TupleTypeElt Fields[] = {
    TupleTypeElt(Ctx.TheObjectPointerType, Identifier()),
    TupleTypeElt(LValueType::get(EltTy, LValueType::Qual::DefaultForType, Ctx),
                 Identifier())
  };

  return TupleType::get(Fields, Ctx);
}

AllocArrayInst::AllocArrayInst(TupleShuffleExpr *E, Type ElementType,
                               unsigned NumElements)
  : Instruction(ValueKind::AllocArray, E, getAllocArrayType(ElementType)),
    ElementType(ElementType), NumElements(NumElements) {
}


ApplyInst *ApplyInst::create(ApplyExpr *Expr, Value *Callee,
                             ArrayRef<Value*> Args, CFG &C) {
  void *Buffer = C.allocate(sizeof(ApplyInst) +
                            Args.size() * sizeof(Value*),
                            llvm::AlignOf<ApplyInst>::Alignment);
  return ::new(Buffer) ApplyInst(Expr, Callee, Args);
}

ApplyInst::ApplyInst(ApplyExpr *Expr, Value *Callee, ArrayRef<Value*> Args)
  : Instruction(ValueKind::Apply, Expr, Expr->getType()),
    Callee(Callee), NumArgs(Args.size()) {
  memcpy(getArgsStorage(), Args.data(), Args.size() * sizeof(Value*));
}

ConstantRefInst::ConstantRefInst(DeclRefExpr *E)
  : Instruction(ValueKind::ConstantRef, E, E->getType()) {}

DeclRefExpr *ConstantRefInst::getExpr() const {
  return getLocExpr<DeclRefExpr>();
}

/// getDecl - Return the underlying declaration.
ValueDecl *ConstantRefInst::getDecl() const {
  return getExpr()->getDecl();
}

ZeroValueInst::ZeroValueInst(VarDecl *D)
  : Instruction(ValueKind::ZeroValue, D, D->getType()) {
}

IntegerLiteralInst::IntegerLiteralInst(IntegerLiteralExpr *E)
  : Instruction(ValueKind::IntegerLiteral, E, E->getType()) {
}

IntegerLiteralExpr *IntegerLiteralInst::getExpr() const {
  return getLocExpr<IntegerLiteralExpr>();
}

/// getValue - Return the APInt for the underlying integer literal.
APInt IntegerLiteralInst::getValue() const {
  return getExpr()->getValue();
}

FloatLiteralInst::FloatLiteralInst(FloatLiteralExpr *E)
  : Instruction(ValueKind::FloatLiteral, E, E->getType()) {
}

FloatLiteralExpr *FloatLiteralInst::getExpr() const {
  return getLocExpr<FloatLiteralExpr>();
}

APFloat FloatLiteralInst::getValue() const {
  return getExpr()->getValue();
}

CharacterLiteralInst::CharacterLiteralInst(CharacterLiteralExpr *E)
  : Instruction(ValueKind::CharacterLiteral, E, E->getType()) {
}

CharacterLiteralExpr *CharacterLiteralInst::getExpr() const {
  return getLocExpr<CharacterLiteralExpr>();
}

uint32_t CharacterLiteralInst::getValue() const {
  return getExpr()->getValue();
}

StringLiteralInst::StringLiteralInst(StringLiteralExpr *E)
  : Instruction(ValueKind::StringLiteral, E, E->getType()) {
}

StringLiteralExpr *StringLiteralInst::getExpr() const {
  return getLocExpr<StringLiteralExpr>();
}

StringRef StringLiteralInst::getValue() const {
  return getExpr()->getValue();
}


LoadInst::LoadInst(LoadExpr *E, Value *LValue)
  : Instruction(ValueKind::Load, E, E->getType()), LValue(LValue) {
}


StoreInst::StoreInst(AssignStmt *S, Value *Src, Value *Dest)
  : Instruction(ValueKind::Store, S, getVoidType(Src->getType())),
    Src(Src), Dest(Dest), IsInitialization(false) {
}

StoreInst::StoreInst(VarDecl *VD, Value *Src, Value *Dest)
  : Instruction(ValueKind::Store, VD, getVoidType(Src->getType())),
    Src(Src), Dest(Dest), IsInitialization(true) {
}


StoreInst::StoreInst(MaterializeExpr *E, Value *Src, Value *Dest)
  : Instruction(ValueKind::Store, E, getVoidType(Src->getType())),
    Src(Src), Dest(Dest), IsInitialization(true) {
}

StoreInst::StoreInst(TupleShuffleExpr *E, Value *Src, Value *Dest)
  : Instruction(ValueKind::Store, E, getVoidType(Src->getType())),
    Src(Src), Dest(Dest), IsInitialization(true) {
  // This happens in a store to an array initializer for varargs tuple shuffle.
}




TypeConversionInst::TypeConversionInst(ImplicitConversionExpr *E,
                                       Value *Operand)
  : Instruction(ValueKind::TypeConversion, E, E->getType()), Operand(Operand) {}


TupleInst *TupleInst::createImpl(Expr *E, ArrayRef<Value*> Elements,
                                 CFG &C) {
  void *Buffer = C.allocate(sizeof(TupleInst) +
                            Elements.size() * sizeof(Value*),
                            llvm::AlignOf<TupleInst>::Alignment);
  return ::new(Buffer) TupleInst(E, Elements);
}

TupleInst::TupleInst(Expr *E, ArrayRef<Value*> Elems)
  : Instruction(ValueKind::Tuple, E, E->getType()), NumArgs(Elems.size()) {
  memcpy(getElementsStorage(), Elems.data(), Elems.size() * sizeof(Value*));
}

TypeOfInst::TypeOfInst(TypeOfExpr *E)
  : Instruction(ValueKind::TypeOf, E, E->getType()) {}

TypeOfExpr *TypeOfInst::getExpr() const {
  return getLocExpr<TypeOfExpr>();
}

/// getMetaType - Return the type of the metatype that this instruction
/// returns.
Type TypeOfInst::getMetaType() const {
  return getExpr()->getType();
}

ScalarToTupleInst::ScalarToTupleInst(ScalarToTupleExpr *E, Value *Operand)
  : Instruction(ValueKind::ScalarToTuple, E, E->getType()), Operand(Operand) {
}

TupleElementInst::TupleElementInst(TupleElementExpr *E, Value *Operand,
                                   unsigned FieldNo)
  : Instruction(ValueKind::TupleElement, E, E->getType()),
    Operand(Operand), FieldNo(FieldNo) {
}

TupleElementInst::TupleElementInst(Type ResultTy, Value *Operand,
                                   unsigned FieldNo)
  : Instruction(ValueKind::TupleElement, (Expr*)nullptr, ResultTy),
    Operand(Operand), FieldNo(FieldNo) {
  
}


//===----------------------------------------------------------------------===//
// CFG-only instructions that don't have an AST analog
//===----------------------------------------------------------------------===//

IndexLValueInst::IndexLValueInst(TupleShuffleExpr *E, Value *Operand,
                                 unsigned Index)
  : Instruction(ValueKind::IndexLValue, E, Operand->getType()),
    Operand(Operand), Index(Index) {
}

//===----------------------------------------------------------------------===//
// Instructions representing terminators
//===----------------------------------------------------------------------===//


TermInst::SuccessorListTy TermInst::getSuccessors() {
  assert(isa<TermInst>(this) && "Only TermInst's are allowed");
  if (auto I = dyn_cast<UnreachableInst>(this))
    return I->getSuccessors();
  if (auto I = dyn_cast<ReturnInst>(this))
    return I->getSuccessors();
  if (auto I = dyn_cast<CondBranchInst>(this))
    return I->getSuccessors();
  return cast<BranchInst>(this)->getSuccessors();
}

UnreachableInst::UnreachableInst(CFG &C)
  : TermInst(ValueKind::Unreachable, CFGLocation(),
             C.getContext().TheEmptyTupleType) {
}

ReturnInst::ReturnInst(ReturnStmt *S, Value *ReturnValue)
  : TermInst(ValueKind::Return, S, getVoidType(ReturnValue->getType())),
    ReturnValue(ReturnValue) {
}

BranchInst::BranchInst(BasicBlock *DestBB, CFG &C)
  : TermInst(ValueKind::Branch, CFGLocation(), C.getContext().TheEmptyTupleType),
    DestBB(this, DestBB) {
}


CondBranchInst::CondBranchInst(Stmt *TheStmt, Value *Condition,
                               BasicBlock *TrueBB, BasicBlock *FalseBB)
  : TermInst(ValueKind::CondBranch, TheStmt, getVoidType(Condition->getType())),
    Condition(Condition) {
  DestBBs[0].init(this);
  DestBBs[1].init(this);
  DestBBs[0] = TrueBB;
  DestBBs[1] = FalseBB;
}

