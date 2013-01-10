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
#include "swift/SIL/SILVisitor.h"
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

namespace {
  class InstructionDestroyer : public SILVisitor<InstructionDestroyer> {
  public:
#define VALUE(CLASS, PARENT) void visit##CLASS(CLASS *I) { I->~CLASS(); }
#include "swift/SIL/SILNodes.def"
  };
} // end anonymous namespace

void Instruction::destroy(Instruction *I) {
  InstructionDestroyer().visit(I);
}

//===----------------------------------------------------------------------===//
// Instruction Subclasses
//===----------------------------------------------------------------------===//

AllocVarInst::AllocVarInst(SILLocation loc, AllocKind allocKind,
                           SILType elementType,
                           Function &F)
  : AllocInst(ValueKind::AllocVarInst, loc,
              elementType.getAddressType(),
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

AllocRefInst::AllocRefInst(SILLocation loc, AllocKind allocKind,
                           SILType elementType,
                           Function &F)
: AllocInst(ValueKind::AllocRefInst, loc,
            elementType,
            allocKind) {
}


/// getElementType - Get the type of the allocated memory (as opposed to the
/// type of the instruction itself, which will be an address type).
Type AllocVarInst::getElementType() const {
  return getType().getSwiftRValueType();
}

// Allocations always return two results: Builtin.ObjectPointer & LValue[EltTy]
static SILTypeList *getAllocType(SILType EltTy, Function &F) {
  ASTContext &Ctx = EltTy.getASTContext();

  SILType ResTys[] = {
    SILType::getObjectPointerType(Ctx),
    EltTy.getAddressType()
  };

  return F.getModule().getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(SILLocation Loc, SILType ElementType, Function &F)
  : Instruction(ValueKind::AllocBoxInst, Loc, getAllocType(ElementType, F)) {
}

Type AllocBoxInst::getElementType() const {
  return getType(1).getSwiftRValueType();
}

AllocArrayInst::AllocArrayInst(SILLocation Loc, SILType ElementType,
                               Value NumElements, Function &F)
  : Instruction(ValueKind::AllocArrayInst, Loc, getAllocType(ElementType, F)),
    NumElements(NumElements) {
}

Type AllocArrayInst::getElementType() const {
  return getType(1).getSwiftRValueType();
}

FunctionInst::FunctionInst(ValueKind kind,
                           SILLocation Loc, SILType Ty, Value Callee,
                           ArrayRef<Value> Args)
  : Instruction(kind, Loc, Ty), Callee(Callee),
    NumArgs(Args.size()) {
  memcpy(getArgsStorage(), Args.data(), Args.size() * sizeof(Value));
}

template<typename DERIVED, typename...T>
DERIVED *FunctionInst::create(Function &F, ArrayRef<Value> Args,
                              T &&...ConstructorArgs) {
  void *Buffer = F.allocate(sizeof(DERIVED) + Args.size() * sizeof(Value),
                            llvm::AlignOf<DERIVED>::Alignment);
  return ::new(Buffer) DERIVED(::std::forward<T>(ConstructorArgs)...);
}

ApplyInst::ApplyInst(SILLocation Loc, Value Callee,
                     SILType Result, ArrayRef<Value> Args)
  : FunctionInst(ValueKind::ApplyInst, Loc, Result,
                 Callee, Args) {
  
}

ApplyInst *ApplyInst::create(SILLocation Loc, Value Callee,
                             SILType Result, ArrayRef<Value> Args,
                             Function &F) {
  return FunctionInst::create<ApplyInst>(F, Args,
                                         Loc, Callee, Result, Args);
}

ClosureInst::ClosureInst(SILLocation Loc, Value Callee, ArrayRef<Value> Args)
// FIXME: the callee should have a lowered SIL function type, and ClosureInst
// should derive the type of its result by partially applying the callee's type.
  : FunctionInst(ValueKind::ClosureInst, Loc,
                 Callee.getType(),
                 Callee, Args) {
  
}

ClosureInst *ClosureInst::create(SILLocation Loc, Value Callee,
                                 ArrayRef<Value> Args, Function &F) {
  return FunctionInst::create<ClosureInst>(F, Args,
                                           Loc, Callee, Args);
}

ConstantRefInst::ConstantRefInst(SILLocation Loc, SILConstant C, SILType Ty)
  : Instruction(ValueKind::ConstantRefInst, Loc, Ty),
    Constant(C) {
}

SILConstant ConstantRefInst::getConstant() const {
  return Constant;
}

ZeroValueInst::ZeroValueInst(SILLocation Loc, SILType Ty)
  : Instruction(ValueKind::ZeroValueInst, Loc, Ty) {
}

IntegerLiteralInst::IntegerLiteralInst(IntegerLiteralExpr *E)
  : Instruction(ValueKind::IntegerLiteralInst, E,
                // Builtin integer types are always valid SIL types.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                       /*address=*/false, /*loadable=*/true)) {
}

IntegerLiteralInst::IntegerLiteralInst(CharacterLiteralExpr *E)
  : Instruction(ValueKind::IntegerLiteralInst, E,
                // Builtin integer types are always valid SIL types.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                       /*address=*/false, /*loadable=*/true)) {
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
  : Instruction(ValueKind::FloatLiteralInst, E,
                // Builtin floating-point types are always valid SIL types.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                       /*address=*/false, /*loadable=*/true)) {
}

FloatLiteralExpr *FloatLiteralInst::getExpr() const {
  return getLocExpr<FloatLiteralExpr>();
}

APFloat FloatLiteralInst::getValue() const {
  return getExpr()->getValue();
}

StringLiteralInst::StringLiteralInst(StringLiteralExpr *E)
  : Instruction(ValueKind::StringLiteralInst, E,
                // The string literal tuple type is always a valid SIL type.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                       /*address=*/false, /*loadable=*/true)) {
}

StringLiteralExpr *StringLiteralInst::getExpr() const {
  return getLocExpr<StringLiteralExpr>();
}

StringRef StringLiteralInst::getValue() const {
  return getExpr()->getValue();
}


LoadInst::LoadInst(SILLocation Loc, Value LValue)
  : Instruction(ValueKind::LoadInst, Loc, LValue.getType().getObjectType()),
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

ZeroAddrInst::ZeroAddrInst(SILLocation Loc, Value Dest)
  : Instruction(ValueKind::ZeroAddrInst, Loc), Dest(Dest) {
}

SpecializeInst *SpecializeInst::create(SILLocation Loc, Value Operand,
                                       ArrayRef<Substitution> Substitutions,
                                       SILType DestTy, Function &F) {
 void *Buffer = F.allocate(
           sizeof(SpecializeInst) + Substitutions.size() * sizeof(Substitution),
           llvm::AlignOf<SpecializeInst>::Alignment);
  return ::new(Buffer) SpecializeInst(Loc, Operand, Substitutions, DestTy);
}

SpecializeInst::SpecializeInst(SILLocation Loc, Value Operand,
                               ArrayRef<Substitution> Substitutions,
                               SILType DestTy)
  : Instruction(ValueKind::SpecializeInst, Loc, DestTy), Operand(Operand),
    NumSubstitutions(Substitutions.size())
{
  memcpy(getSubstitutionsStorage(), Substitutions.data(),
         Substitutions.size() * sizeof(Substitution));
}

ConversionInst::ConversionInst(ValueKind Kind,
                               SILLocation Loc, Value Operand, SILType Ty)
  : Instruction(Kind, Loc, Ty), Operand(Operand) {
}

ImplicitConvertInst::ImplicitConvertInst(SILLocation Loc, Value Operand,
                                         SILType Ty)
  : ConversionInst(ValueKind::ImplicitConvertInst, Loc, Operand, Ty) {
}

CoerceInst::CoerceInst(SILLocation Loc, Value Operand, SILType Ty)
  : ConversionInst(ValueKind::CoerceInst, Loc, Operand, Ty) {
}

DowncastInst::DowncastInst(SILLocation Loc, Value Operand, SILType Ty)
  : ConversionInst(ValueKind::DowncastInst, Loc, Operand, Ty) {
}

ArchetypeToSuperInst::ArchetypeToSuperInst(SILLocation Loc,
                                           Value Operand, SILType Ty)
  : ConversionInst(ValueKind::ArchetypeToSuperInst, Loc, Operand, Ty) {
}

SuperToArchetypeInst::SuperToArchetypeInst(SILLocation Loc,
                                           Value SrcBase,
                                           Value DestArchetypeAddress)
  : Instruction(ValueKind::SuperToArchetypeInst, Loc),
    SrcBase(SrcBase), DestArchetypeAddress(DestArchetypeAddress) {
}

TupleInst *TupleInst::createImpl(SILLocation Loc, SILType Ty,
                                 ArrayRef<Value> Elements, Function &F) {
  void *Buffer = F.allocate(sizeof(TupleInst) + Elements.size() * sizeof(Value),
                            llvm::AlignOf<TupleInst>::Alignment);
  return ::new(Buffer) TupleInst(Loc, Ty, Elements);
}

TupleInst::TupleInst(SILLocation Loc, SILType Ty, ArrayRef<Value> Elems)
  : Instruction(ValueKind::TupleInst, Loc, Ty), NumArgs(Elems.size()) {
  memcpy(getElementsStorage(), Elems.data(), Elems.size() * sizeof(Value));
}

MetatypeInst::MetatypeInst(SILLocation Loc, SILType Metatype)
  : Instruction(ValueKind::MetatypeInst, Loc, Metatype) {}

AssociatedMetatypeInst::AssociatedMetatypeInst(SILLocation Loc,
                                               Value MetatypeSrc,
                                               SILType MetatypeDest)
  : Instruction(ValueKind::AssociatedMetatypeInst, Loc, MetatypeDest),
    SourceMetatype(MetatypeSrc) {}

ExtractInst::ExtractInst(SILLocation Loc, Value Operand,
                         unsigned FieldNo, SILType ResultTy)
  : Instruction(ValueKind::ExtractInst, Loc, ResultTy),
    Operand(Operand), FieldNo(FieldNo) {
}

ElementAddrInst::ElementAddrInst(SILLocation Loc, Value Operand,
                                 unsigned FieldNo, SILType ResultTy)
  : Instruction(ValueKind::ElementAddrInst, Loc, ResultTy),
    Operand(Operand), FieldNo(FieldNo) {
}

RefElementAddrInst::RefElementAddrInst(SILLocation Loc, Value Operand,
                                       unsigned FieldNo, SILType ResultTy)
  : Instruction(ValueKind::RefElementAddrInst, Loc, ResultTy),
    Operand(Operand), FieldNo(FieldNo) {
}

WitnessTableMethodInst::WitnessTableMethodInst(ValueKind Kind,
                                               SILLocation Loc, Value Operand,
                                               SILConstant Member,
                                               SILType Ty,
                                               Function &F)
  : Instruction(Kind, Loc, Ty),
    Operand(Operand), Member(Member) {
}

ArchetypeMethodInst::ArchetypeMethodInst(SILLocation Loc, Value Operand,
                                         SILConstant Member,
                                         SILType Ty,
                                         Function &F)
  : WitnessTableMethodInst(ValueKind::ArchetypeMethodInst,
                           Loc, Operand, Member,
                           Ty, F) {
}

ProtocolMethodInst::ProtocolMethodInst(SILLocation Loc, Value Operand,
                                             SILConstant Member,
                                             SILType Ty,
                                             Function &F)
  : WitnessTableMethodInst(ValueKind::ProtocolMethodInst,
                           Loc, Operand, Member,
                           Ty, F) {
}

ProjectExistentialInst::ProjectExistentialInst(SILLocation Loc, Value Operand,
                                               Function &F)
  : Instruction(ValueKind::ProjectExistentialInst, Loc,
                SILType::getRawPointerType(F.getContext())),
    Operand(Operand) {
}

InitExistentialInst::InitExistentialInst(SILLocation Loc, Value Existential,
                                           SILType ConcreteType,
                                           Function &F)
  : Instruction(ValueKind::InitExistentialInst, Loc,
                ConcreteType.getAddressType()),
    Existential(Existential) {
  
}

Type InitExistentialInst::getConcreteType() const {
  return getType(0).getSwiftRValueType();
}

DeinitExistentialInst::DeinitExistentialInst(SILLocation Loc,
                                               Value Existential)
  : Instruction(ValueKind::DeinitExistentialInst, Loc,
            SILType::getEmptyTupleType(Existential.getType().getASTContext())) {
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

DeallocRefInst::DeallocRefInst(SILLocation loc, Value operand)
  : Instruction(ValueKind::DeallocRefInst, loc),
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

IntegerValueInst::IntegerValueInst(uint64_t Val, SILType Ty)
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
             SILType::getEmptyTupleType(F.getContext())) {
}

ReturnInst::ReturnInst(SILLocation Loc, Value ReturnValue)
  : TermInst(ValueKind::ReturnInst, Loc),
    ReturnValue(ReturnValue) {
}

BranchInst::BranchInst(BasicBlock *DestBB, Function &F)
  : TermInst(ValueKind::BranchInst, SILLocation(),
             SILType::getEmptyTupleType(F.getContext())),
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

