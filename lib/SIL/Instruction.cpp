//===--- Instruction.cpp - Instructions for SIL code ----------------------===//
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
// This file defines the high-level Instruction classes used for Swift SIL code.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/Instruction.h"
#include "swift/AST/AST.h"
#include "swift/SIL/Function.h"
#include "llvm/Support/ErrorHandling.h"
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

AllocVarInst::AllocVarInst(VarDecl *VD)
  : AllocInst(ValueKind::AllocVarInst, VD, VD->getTypeOfReference()) {
}

/// getDecl - Return the underlying declaration.
VarDecl *AllocVarInst::getDecl() const {
  return getLocDecl<VarDecl>();
}


AllocTmpInst::AllocTmpInst(MaterializeExpr *E)
  : AllocInst(ValueKind::AllocTmpInst, E, E->getType()) {}

// Allocations always returns two results: Builtin.ObjectPointer & LValue[EltTy]
static SILTypeList *getAllocType(Type EltTy, SILBase &B) {
  ASTContext &Ctx = EltTy->getASTContext();

  Type ResTys[] = {
    Ctx.TheObjectPointerType,
    LValueType::get(EltTy, LValueType::Qual::DefaultForType, Ctx)
  };

  return B.getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(Expr *E, Type ElementType, SILBase &B)
  : Instruction(ValueKind::AllocBoxInst, E, getAllocType(ElementType, B)),
    ElementType(ElementType) {
}


AllocArrayInst::AllocArrayInst(Expr *E, Type ElementType,
                               Value NumElements, SILBase &B)
  : Instruction(ValueKind::AllocArrayInst, E, getAllocType(ElementType, B)),
    ElementType(ElementType), NumElements(NumElements) {
}

ApplyInst::ApplyInst(SILLocation Loc, Type Ty, Value Callee,
                     ArrayRef<Value> Args)
  : Instruction(ValueKind::ApplyInst, Loc, Ty), Callee(Callee),
    NumArgs(Args.size()) {
  memcpy(getArgsStorage(), Args.data(), Args.size() * sizeof(Value));
}

ApplyInst *ApplyInst::create(ApplyExpr *Expr, Value Callee,
                             ArrayRef<Value> Args, Function &F) {
  void *Buffer = F.allocate(sizeof(ApplyInst) + Args.size() * sizeof(Value),
                            llvm::AlignOf<ApplyInst>::Alignment);
  Type ResTy = Callee.getType()->castTo<FunctionType>()->getResult();
  assert(ResTy->isEqual(Expr->getType()));
  return ::new(Buffer) ApplyInst(Expr, ResTy, Callee, Args);
}

ApplyInst *ApplyInst::create(Value Callee, ArrayRef<Value> Args, Function &F) {
  void *Buffer = F.allocate(sizeof(ApplyInst) + Args.size() * sizeof(Value),
                            llvm::AlignOf<ApplyInst>::Alignment);
  Type ResTy = Callee.getType()->castTo<FunctionType>()->getResult();
  return ::new(Buffer) ApplyInst(SILLocation(), ResTy, Callee, Args);
}


ConstantRefInst::ConstantRefInst(DeclRefExpr *E)
  : Instruction(ValueKind::ConstantRefInst, E, E->getType()) {}

DeclRefExpr *ConstantRefInst::getExpr() const {
  return getLocExpr<DeclRefExpr>();
}

/// getDecl - Return the underlying declaration.
ValueDecl *ConstantRefInst::getDecl() const {
  return getExpr()->getDecl();
}

ZeroValueInst::ZeroValueInst(VarDecl *D)
  : Instruction(ValueKind::ZeroValueInst, D, D->getType()) {
}

IntegerLiteralInst::IntegerLiteralInst(IntegerLiteralExpr *E)
  : Instruction(ValueKind::IntegerLiteralInst, E, E->getType()),
    value(E->getValue()) {
}

IntegerLiteralInst::IntegerLiteralInst(CharacterLiteralExpr *E)
  : Instruction(ValueKind::IntegerLiteralInst, E, E->getType()),
    value(32, E->getValue()) {
}

Expr *IntegerLiteralInst::getExpr() const {
  return getLocExpr<Expr>();
}

/// getValue - Return the APInt for the underlying integer literal.
APInt IntegerLiteralInst::getValue() const {
  return value;
}

FloatLiteralInst::FloatLiteralInst(FloatLiteralExpr *E)
  : Instruction(ValueKind::FloatLiteralInst, E, E->getType()),
    value(E->getValue()) {
}

FloatLiteralExpr *FloatLiteralInst::getExpr() const {
  return getLocExpr<FloatLiteralExpr>();
}

APFloat FloatLiteralInst::getValue() const {
  return value;
}

StringLiteralInst::StringLiteralInst(StringLiteralExpr *E)
  : Instruction(ValueKind::StringLiteralInst, E, E->getType()) {
}

StringLiteralExpr *StringLiteralInst::getExpr() const {
  return getLocExpr<StringLiteralExpr>();
}

StringRef StringLiteralInst::getValue() const {
  return getExpr()->getValue();
}


LoadInst::LoadInst(LoadExpr *E, Value LValue)
  : Instruction(ValueKind::LoadInst, E, E->getType()), LValue(LValue) {
}


StoreInst::StoreInst(AssignStmt *S, Value Src, Value Dest)
  : Instruction(ValueKind::StoreInst, S),
    Src(Src), Dest(Dest), IsInitialization(false) {
}

StoreInst::StoreInst(VarDecl *VD, Value Src, Value Dest)
  : Instruction(ValueKind::StoreInst, VD),
    Src(Src), Dest(Dest), IsInitialization(true) {
}


StoreInst::StoreInst(MaterializeExpr *E, Value Src, Value Dest)
  : Instruction(ValueKind::StoreInst, E),
    Src(Src), Dest(Dest), IsInitialization(true) {
}

StoreInst::StoreInst(Expr *E, bool IsInitialization, Value Src, Value Dest)
  : Instruction(ValueKind::StoreInst, E),
    Src(Src), Dest(Dest), IsInitialization(IsInitialization) {
  // This happens in a store to an array initializer for varargs tuple shuffle.
}


CopyInst::CopyInst(Expr *E, Value SrcLValue, Value DestLValue,
                   bool IsTakeOfSrc, bool IsInitializationOfDest)
  : Instruction(ValueKind::CopyInst, E), Src(SrcLValue), Dest(DestLValue),
    IsTakeOfSrc(IsTakeOfSrc), IsInitializationOfDest(IsInitializationOfDest) {
}


SpecializeInst::SpecializeInst(SpecializeExpr *SE, Value Operand, Type DestTy)
  : Instruction(ValueKind::SpecializeInst, SE, DestTy), Operand(Operand) {
}


ConvertInst::ConvertInst(ImplicitConversionExpr *E, Value Operand)
  : Instruction(ValueKind::ConvertInst, E, E->getType()), Operand(Operand) {
}

ConvertInst::ConvertInst(Type Ty, Value Operand)
  : Instruction(ValueKind::ConvertInst, (Expr*)nullptr, Ty), Operand(Operand) {
}


TupleInst *TupleInst::createImpl(Expr *E, Type Ty, ArrayRef<Value> Elements,
                                 Function &F) {
  if (E) Ty = E->getType();

  void *Buffer = F.allocate(sizeof(TupleInst) + Elements.size() * sizeof(Value),
                            llvm::AlignOf<TupleInst>::Alignment);
  return ::new(Buffer) TupleInst(E, Ty, Elements);
}

TupleInst::TupleInst(Expr *E, Type Ty, ArrayRef<Value> Elems)
  : Instruction(ValueKind::TupleInst, E, Ty), NumArgs(Elems.size()) {
  memcpy(getElementsStorage(), Elems.data(), Elems.size() * sizeof(Value));
}

MetatypeInst::MetatypeInst(MetatypeExpr *E)
  : Instruction(ValueKind::MetatypeInst, E, E->getType()) {}

MetatypeExpr *MetatypeInst::getExpr() const {
  return getLocExpr<MetatypeExpr>();
}

/// getMetaType - Return the type of the metatype that this instruction
/// returns.
Type MetatypeInst::getMetaType() const {
  return getExpr()->getType();
}

TupleElementInst::TupleElementInst(TupleElementExpr *E, Value Operand,
                                   unsigned FieldNo)
  : Instruction(ValueKind::TupleElementInst, E, E->getType()),
    Operand(Operand), FieldNo(FieldNo) {
}

TupleElementInst::TupleElementInst(Type ResultTy, Value Operand,
                                   unsigned FieldNo)
  : Instruction(ValueKind::TupleElementInst, (Expr*)nullptr, ResultTy),
    Operand(Operand), FieldNo(FieldNo) {
  
}

RetainInst::RetainInst(Expr *E, Value Operand)
  : Instruction(ValueKind::RetainInst, E, Operand.getType()), Operand(Operand){
}

ReleaseInst::ReleaseInst(Expr *E, Value Operand)
  : Instruction(ValueKind::ReleaseInst, E), Operand(Operand) {
}

DeallocInst::DeallocInst(Expr *E, Value Operand)
  : Instruction(ValueKind::DeallocInst, E), Operand(Operand) {
}

DestroyInst::DestroyInst(Expr *E, Value Operand)
  : Instruction(ValueKind::DestroyInst, E), Operand(Operand) {
}

//===----------------------------------------------------------------------===//
// SIL-only instructions that don't have an AST analog
//===----------------------------------------------------------------------===//

IndexAddrInst::IndexAddrInst(Expr *E, Value Operand, unsigned Index)
  : Instruction(ValueKind::IndexAddrInst, E, Operand.getType()),
    Operand(Operand), Index(Index) {
}

IntegerValueInst::IntegerValueInst(uint64_t Val, Type Ty)
  : Instruction(ValueKind::IntegerValueInst, SILLocation(), Ty), Val(Val) {}


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

UnreachableInst::UnreachableInst(Function &F)
  : TermInst(ValueKind::UnreachableInst, SILLocation(),
             F.getContext().TheEmptyTupleType) {
}

ReturnInst::ReturnInst(ReturnStmt *S, Value ReturnValue)
  : TermInst(ValueKind::ReturnInst, S),
    ReturnValue(ReturnValue) {
}

BranchInst::BranchInst(BasicBlock *DestBB, Function &F)
  : TermInst(ValueKind::BranchInst, SILLocation(),
             F.getContext().TheEmptyTupleType),
    DestBB(this, DestBB) {
}


CondBranchInst::CondBranchInst(Stmt *TheStmt, Value Condition,
                               BasicBlock *TrueBB, BasicBlock *FalseBB)
  : TermInst(ValueKind::CondBranchInst, TheStmt), Condition(Condition) {
  DestBBs[0].init(this);
  DestBBs[1].init(this);
  DestBBs[0] = TrueBB;
  DestBBs[1] = FalseBB;
}

