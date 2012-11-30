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
  : AllocInst(ValueKind::AllocVarInst, VD, VD->getTypeOfReference(),
              AllocKind::Heap) {
}

AllocVarInst::AllocVarInst(SILLocation loc, AllocKind allocKind,
                           Type elementType)
  // FIXME: LValue qualifiers being wrong can break the verifier
  : AllocInst(ValueKind::AllocVarInst, loc,
              LValueType::get(elementType, LValueType::Qual::DefaultForType,
                              elementType->getASTContext()),
              allocKind) {
}

/// getDecl - Return the underlying variable declaration associated with this
/// allocation, or null if this is a temporary allocation.
VarDecl *AllocVarInst::getDecl() const {
  if (Decl *d = getLoc().dyn_cast<Decl*>()) {
    return dyn_cast<VarDecl>(d);
  } else {
    return nullptr;
  }
}

/// getElementType - Get the type of the allocated memory (as opposed to the
/// type of the instruction itself, which will be an address type).
Type AllocVarInst::getElementType() const {
  return getType()->castTo<LValueType>()->getObjectType();
}

// Allocations always return two results: Builtin.ObjectPointer & LValue[EltTy]
static SILTypeList *getAllocType(Type EltTy, SILBase &B) {
  ASTContext &Ctx = EltTy->getASTContext();

  Type ResTys[] = {
    Ctx.TheObjectPointerType,
    LValueType::get(EltTy, LValueType::Qual::DefaultForType, Ctx)
  };

  return B.getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(SILLocation Loc, Type ElementType, SILBase &B)
  : Instruction(ValueKind::AllocBoxInst, Loc, getAllocType(ElementType, B)),
    ElementType(ElementType) {
}


AllocArrayInst::AllocArrayInst(SILLocation Loc, Type ElementType,
                               Value NumElements, SILBase &B)
  : Instruction(ValueKind::AllocArrayInst, Loc, getAllocType(ElementType, B)),
    ElementType(ElementType), NumElements(NumElements) {
}

ApplyInst::ApplyInst(SILLocation Loc, Type Ty, Value Callee,
                     ArrayRef<Value> Args)
  : Instruction(ValueKind::ApplyInst, Loc, Ty), Callee(Callee),
    NumArgs(Args.size()) {
  memcpy(getArgsStorage(), Args.data(), Args.size() * sizeof(Value));
}

ApplyInst *ApplyInst::create(SILLocation Loc, Value Callee,
                             ArrayRef<Value> Args, Function &F) {
  void *Buffer = F.allocate(sizeof(ApplyInst) + Args.size() * sizeof(Value),
                            llvm::AlignOf<ApplyInst>::Alignment);
  Type ResTy = Callee.getType()->castTo<FunctionType>()->getResult();
  return ::new(Buffer) ApplyInst(Loc, ResTy, Callee, Args);
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

ZeroValueInst::ZeroValueInst(SILLocation Loc, Type Ty)
  : Instruction(ValueKind::ZeroValueInst, Loc, Ty) {
}

IntegerLiteralInst::IntegerLiteralInst(IntegerLiteralExpr *E)
  : Instruction(ValueKind::IntegerLiteralInst, E, E->getType()) {
}

IntegerLiteralInst::IntegerLiteralInst(CharacterLiteralExpr *E)
  : Instruction(ValueKind::IntegerLiteralInst, E, E->getType()) {
}

Expr *IntegerLiteralInst::getExpr() const {
  return getLocExpr<Expr>();
}

/// getValue - Return the APInt for the underlying integer literal.
APInt IntegerLiteralInst::getValue() const {
  auto expr = getExpr();
  if (auto intExpr = dyn_cast<IntegerLiteralExpr>(expr)) {
    return intExpr->getValue();
  } else if (auto charExpr = dyn_cast<CharacterLiteralExpr>(expr)) {
    return APInt(32, charExpr->getValue());
  }
  llvm_unreachable("int_literal instruction associated with unexpected "
                   "ast node!");
}

FloatLiteralInst::FloatLiteralInst(FloatLiteralExpr *E)
  : Instruction(ValueKind::FloatLiteralInst, E, E->getType()) {
}

FloatLiteralExpr *FloatLiteralInst::getExpr() const {
  return getLocExpr<FloatLiteralExpr>();
}

APFloat FloatLiteralInst::getValue() const {
  return getExpr()->getValue();
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


LoadInst::LoadInst(SILLocation Loc, Value LValue)
  : Instruction(ValueKind::LoadInst, Loc, LValue.getType()->getRValueType()),
    LValue(LValue) {
}


StoreInst::StoreInst(SILLocation Loc, Value Src, Value Dest)
  : Instruction(ValueKind::StoreInst, Loc),
    Src(Src), Dest(Dest) {
}


CopyAddrInst::CopyAddrInst(SILLocation Loc, Value SrcLValue, Value DestLValue,
                           bool IsTakeOfSrc, bool IsInitializationOfDest)
  : Instruction(ValueKind::CopyAddrInst, Loc), Src(SrcLValue), Dest(DestLValue),
    IsTakeOfSrc(IsTakeOfSrc), IsInitializationOfDest(IsInitializationOfDest) {
}


SpecializeInst::SpecializeInst(SILLocation Loc, Value Operand, Type DestTy)
  : Instruction(ValueKind::SpecializeInst, Loc, DestTy), Operand(Operand) {
}


ConvertInst::ConvertInst(SILLocation Loc, Value Operand, Type Ty)
  : Instruction(ValueKind::ConvertInst, Loc, Ty), Operand(Operand) {
}


TupleInst *TupleInst::createImpl(SILLocation Loc, Type Ty,
                                 ArrayRef<Value> Elements, Function &F) {
  void *Buffer = F.allocate(sizeof(TupleInst) + Elements.size() * sizeof(Value),
                            llvm::AlignOf<TupleInst>::Alignment);
  return ::new(Buffer) TupleInst(Loc, Ty, Elements);
}

TupleInst::TupleInst(SILLocation Loc, Type Ty, ArrayRef<Value> Elems)
  : Instruction(ValueKind::TupleInst, Loc, Ty), NumArgs(Elems.size()) {
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

ExtractInst::ExtractInst(SILLocation Loc, Value Operand,
                         unsigned FieldNo, Type ResultTy)
  : Instruction(ValueKind::ExtractInst, Loc, ResultTy),
    Operand(Operand), FieldNo(FieldNo) {
}


RetainInst::RetainInst(SILLocation Loc, Value Operand)
  : Instruction(ValueKind::RetainInst, Loc, Operand.getType()),
    Operand(Operand) {
}

ReleaseInst::ReleaseInst(SILLocation Loc, Value Operand)
  : Instruction(ValueKind::ReleaseInst, Loc), Operand(Operand) {
}

DeallocVarInst::DeallocVarInst(SILLocation loc, AllocKind allocKind,
                               Value operand)
  : Instruction(ValueKind::DeallocVarInst, loc), allocKind(allocKind),
    Operand(operand) {
}

DestroyAddrInst::DestroyAddrInst(SILLocation Loc, Value Operand)
  : Instruction(ValueKind::DestroyAddrInst, Loc), Operand(Operand) {
}

//===----------------------------------------------------------------------===//
// SIL-only instructions that don't have an AST analog
//===----------------------------------------------------------------------===//

IndexAddrInst::IndexAddrInst(SILLocation Loc, Value Operand, unsigned Index)
  : Instruction(ValueKind::IndexAddrInst, Loc, Operand.getType()),
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

ReturnInst::ReturnInst(SILLocation Loc, Value ReturnValue)
  : TermInst(ValueKind::ReturnInst, Loc),
    ReturnValue(ReturnValue) {
}

BranchInst::BranchInst(BasicBlock *DestBB, Function &F)
  : TermInst(ValueKind::BranchInst, SILLocation(),
             F.getContext().TheEmptyTupleType),
    DestBB(this, DestBB) {
}


CondBranchInst::CondBranchInst(SILLocation Loc, Value Condition,
                               BasicBlock *TrueBB, BasicBlock *FalseBB)
  : TermInst(ValueKind::CondBranchInst, Loc), Condition(Condition) {
  DestBBs[0].init(this);
  DestBBs[1].init(this);
  DestBBs[0] = TrueBB;
  DestBBs[1] = FalseBB;
}

