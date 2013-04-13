//===--- SILInstruction.cpp - Instructions for SIL code -------------------===//
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
// This file defines the high-level SILInstruction classes used for  SIL code.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/AST/AST.h"
#include "swift/Basic/AssertImplements.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// ilist_traits<SILInstruction> Implementation
//===----------------------------------------------------------------------===//

// The trait object is embedded into a basic block.  Use dirty hacks to
// reconstruct the BB from the 'this' pointer of the trait.
SILBasicBlock *llvm::ilist_traits<SILInstruction>::getContainingBlock() {
  typedef iplist<SILInstruction> SILBasicBlock::*Sublist;
size_t Offset(size_t(&((SILBasicBlock*)0->*SILBasicBlock::getSublistAccess())));
  iplist<SILInstruction>* Anchor(static_cast<iplist<SILInstruction>*>(this));
return reinterpret_cast<SILBasicBlock*>(reinterpret_cast<char*>(Anchor)-Offset);
}


void llvm::ilist_traits<SILInstruction>::addNodeToList(SILInstruction *I) {
  assert(I->ParentBB == 0 && "Already in a list!");
  I->ParentBB = getContainingBlock();
}

void llvm::ilist_traits<SILInstruction>::removeNodeFromList(SILInstruction *I) {
  // When an instruction is removed from a BB, clear the parent pointer.
  assert(I->ParentBB && "Not in a list!");
  I->ParentBB = 0;
}

void llvm::ilist_traits<SILInstruction>::
transferNodesFromList(llvm::ilist_traits<SILInstruction> &L2,
                      llvm::ilist_iterator<SILInstruction> first,
                      llvm::ilist_iterator<SILInstruction> last) {
  // If transfering instructions within the same basic block, no reason to
  // update their parent pointers.
  SILBasicBlock *ThisParent = getContainingBlock();
  if (ThisParent == L2.getContainingBlock()) return;

  // Update the parent fields in the instructions.
  for (; first != last; ++first)
    first->ParentBB = ThisParent;
}


//===----------------------------------------------------------------------===//
// SILInstruction Implementation
//===----------------------------------------------------------------------===//

// Assert that all subclasses of ValueBase implement classof.
#define VALUE(CLASS, PARENT) \
  ASSERT_IMPLEMENTS_STATIC(CLASS, PARENT, classof, bool(const ValueBase*));
#include "swift/SIL/SILNodes.def"

/// removeFromParent - This method unlinks 'this' from the containing basic
/// block, but does not delete it.
///
void SILInstruction::removeFromParent() {
  getParent()->getInsts().remove(this);
}

/// eraseFromParent - This method unlinks 'this' from the containing basic
/// block and deletes it.
///
void SILInstruction::eraseFromParent() {
  getParent()->getInsts().erase(this);
}

namespace {
  class InstructionDestroyer : public SILVisitor<InstructionDestroyer> {
  public:
#define VALUE(CLASS, PARENT) void visit##CLASS(CLASS *I) { I->~CLASS(); }
#include "swift/SIL/SILNodes.def"
  };
} // end anonymous namespace

void SILInstruction::destroy(SILInstruction *I) {
  InstructionDestroyer().visit(I);
}

namespace {
  class AllOperandsAccessor : public SILVisitor<AllOperandsAccessor,
                                                ArrayRef<Operand> > {
  public:
#define VALUE(CLASS, PARENT) \
    ArrayRef<Operand> visit##CLASS(const CLASS *I) {                    \
      llvm_unreachable("accessing non-instruction " #CLASS);            \
    }
#define INST(CLASS, PARENT) \
    ArrayRef<Operand> visit##CLASS(const CLASS *I) {                    \
      ASSERT_IMPLEMENTS(CLASS, SILInstruction, getAllOperands,          \
                        ArrayRef<Operand>() const);                     \
      return I->getAllOperands();                                       \
    }
#include "swift/SIL/SILNodes.def"
  };
} // end anonymous namespace

ArrayRef<Operand> SILInstruction::getAllOperands() const {
  return AllOperandsAccessor().visit(const_cast<SILInstruction*>(this));
}

//===----------------------------------------------------------------------===//
// SILInstruction Subclasses
//===----------------------------------------------------------------------===//

AllocVarInst::AllocVarInst(SILLocation loc, AllocKind allocKind,
                           SILType elementType,
                           SILFunction &F)
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
                           SILFunction &F)
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
static SILTypeList *getAllocType(SILType EltTy, SILFunction &F) {
  ASTContext &Ctx = EltTy.getASTContext();

  SILType ResTys[] = {
    SILType::getObjectPointerType(Ctx),
    EltTy.getAddressType()
  };

  return F.getModule().getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(SILLocation Loc, SILType ElementType, SILFunction &F)
  : SILInstruction(ValueKind::AllocBoxInst, Loc, getAllocType(ElementType, F)) {
}

Type AllocBoxInst::getElementType() const {
  return getType(1).getSwiftRValueType();
}

AllocArrayInst::AllocArrayInst(SILLocation Loc, SILType ElementType,
                               SILValue NumElements, SILFunction &F)
  : SILInstruction(ValueKind::AllocArrayInst, Loc, getAllocType(ElementType, F)),
    Operands(this, NumElements) {
}

Type AllocArrayInst::getElementType() const {
  return getType(1).getSwiftRValueType();
}

FunctionInst::FunctionInst(ValueKind kind,
                           SILLocation Loc, SILType Ty, SILValue Callee,
                           ArrayRef<SILValue> Args)
  : SILInstruction(kind, Loc, Ty), Operands(this, Args, Callee) {
}

template<typename DERIVED, typename...T>
DERIVED *FunctionInst::create(SILFunction &F, ArrayRef<SILValue> Args,
                              T &&...ConstructorArgs) {
  // The way we store operands requires this.
  static_assert(sizeof(DERIVED) == sizeof(FunctionInst),
                "can't have extra storage in a FunctionInst subclass");

  void *Buffer = F.allocate(sizeof(DERIVED) +
                            decltype(Operands)::getExtraSize(Args.size()),
                            alignof(DERIVED));
  return ::new(Buffer) DERIVED(::std::forward<T>(ConstructorArgs)...);
}

ApplyInst::ApplyInst(SILLocation Loc, SILValue Callee,
                     SILType Result, ArrayRef<SILValue> Args)
  : FunctionInst(ValueKind::ApplyInst, Loc, Result,
                 Callee, Args) {
  
}

ApplyInst *ApplyInst::create(SILLocation Loc, SILValue Callee,
                             SILType Result, ArrayRef<SILValue> Args,
                             SILFunction &F) {
  return FunctionInst::create<ApplyInst>(F, Args,
                                         Loc, Callee, Result, Args);
}

PartialApplyInst::PartialApplyInst(SILLocation Loc, SILValue Callee,
                                   ArrayRef<SILValue> Args, SILType ClosureType)
// FIXME: the callee should have a lowered SIL function type, and PartialApplyInst
// should derive the type of its result by partially applying the callee's type.
  : FunctionInst(ValueKind::PartialApplyInst, Loc,
                 ClosureType,
                 Callee, Args) {
  
}

PartialApplyInst *PartialApplyInst::create(SILLocation Loc, SILValue Callee,
                                           ArrayRef<SILValue> Args,
                                           SILType ClosureType,
                                           SILFunction &F) {
  return FunctionInst::create<PartialApplyInst>(F, Args, Loc, Callee,
                                                Args, ClosureType);
}

ConstantRefInst::ConstantRefInst(SILLocation Loc, SILConstant C, SILType Ty)
  : SILInstruction(ValueKind::ConstantRefInst, Loc, Ty),
    Constant(C) {
}

SILConstant ConstantRefInst::getConstant() const {
  return Constant;
}

IntegerLiteralInst::IntegerLiteralInst(IntegerLiteralExpr *E)
  : SILInstruction(ValueKind::IntegerLiteralInst, E,
                // Builtin integer types are always valid SIL types.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                     /*address=*/false, /*loadable=*/true)) {
}

IntegerLiteralInst::IntegerLiteralInst(CharacterLiteralExpr *E)
  : SILInstruction(ValueKind::IntegerLiteralInst, E,
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
  : SILInstruction(ValueKind::FloatLiteralInst, E,
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
  : SILInstruction(ValueKind::StringLiteralInst, E,
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


LoadInst::LoadInst(SILLocation Loc, SILValue LValue)
  : SILInstruction(ValueKind::LoadInst, Loc, LValue.getType().getObjectType()),
    Operands(this, LValue) {
}


StoreInst::StoreInst(SILLocation Loc, SILValue Src, SILValue Dest)
  : SILInstruction(ValueKind::StoreInst, Loc),
    Operands(this, Src, Dest) {
}


CopyAddrInst::CopyAddrInst(SILLocation Loc, SILValue SrcLValue, SILValue DestLValue,
                           bool IsTakeOfSrc, bool IsInitializationOfDest)
  : SILInstruction(ValueKind::CopyAddrInst, Loc),
    IsTakeOfSrc(IsTakeOfSrc), IsInitializationOfDest(IsInitializationOfDest),
    Operands(this, SrcLValue, DestLValue)
{
}

InitializeVarInst::InitializeVarInst(SILLocation Loc, SILValue Dest)
  : SILInstruction(ValueKind::InitializeVarInst, Loc), Operands(this, Dest) {
}

SpecializeInst *SpecializeInst::create(SILLocation Loc, SILValue Operand,
                                       ArrayRef<Substitution> Substitutions,
                                       SILType DestTy, SILFunction &F) {
 void *Buffer = F.allocate(
           sizeof(SpecializeInst) + Substitutions.size() * sizeof(Substitution),
           alignof(SpecializeInst));
  return ::new(Buffer) SpecializeInst(Loc, Operand, Substitutions, DestTy);
}

SpecializeInst::SpecializeInst(SILLocation Loc, SILValue Operand,
                               ArrayRef<Substitution> Substitutions,
                               SILType DestTy)
  : SILInstruction(ValueKind::SpecializeInst, Loc, DestTy),
    Operands(this, Operand), NumSubstitutions(Substitutions.size())
{
  memcpy(getSubstitutionsStorage(), Substitutions.data(),
         Substitutions.size() * sizeof(Substitution));
}

ConversionInst::ConversionInst(ValueKind Kind,
                               SILLocation Loc, SILValue Operand, SILType Ty)
  : SILInstruction(Kind, Loc, Ty), Operands(this, Operand) {
}

ConvertFunctionInst::ConvertFunctionInst(SILLocation Loc, SILValue Operand,
                                         SILType Ty)
  : ConversionInst(ValueKind::ConvertFunctionInst, Loc, Operand, Ty) {
}

CoerceInst::CoerceInst(SILLocation Loc, SILValue Operand, SILType Ty)
  : ConversionInst(ValueKind::CoerceInst, Loc, Operand, Ty) {
}

UpcastInst::UpcastInst(SILLocation Loc, SILValue Operand, SILType Ty)
  : ConversionInst(ValueKind::UpcastInst, Loc, Operand, Ty) {
}

DowncastInst::DowncastInst(SILLocation Loc, SILValue Operand, SILType Ty)
  : ConversionInst(ValueKind::DowncastInst, Loc, Operand, Ty) {
}

AddressToPointerInst::AddressToPointerInst(SILLocation Loc, SILValue Operand,
                                           SILType Ty)
  : ConversionInst(ValueKind::AddressToPointerInst, Loc, Operand, Ty) {
}

BridgeToBlockInst::BridgeToBlockInst(SILLocation Loc, SILValue Operand,
                                     SILType Ty)
  : ConversionInst(ValueKind::BridgeToBlockInst, Loc, Operand, Ty) {
}

ThinToThickFunctionInst::ThinToThickFunctionInst(SILLocation Loc, SILValue Operand,
                                                 SILType Ty)
  : ConversionInst(ValueKind::ThinToThickFunctionInst, Loc, Operand, Ty) {
}

ArchetypeToSuperInst::ArchetypeToSuperInst(SILLocation Loc,
                                           SILValue Operand, SILType Ty)
  : ConversionInst(ValueKind::ArchetypeToSuperInst, Loc, Operand, Ty) {
}

SuperToArchetypeInst::SuperToArchetypeInst(SILLocation Loc,
                                           SILValue SrcBase,
                                           SILValue DestArchetypeAddress)
  : SILInstruction(ValueKind::SuperToArchetypeInst, Loc),
    Operands(this, SrcBase, DestArchetypeAddress) {
}

TupleInst *TupleInst::createImpl(SILLocation Loc, SILType Ty,
                                 ArrayRef<SILValue> Elements, SILFunction &F) {
  void *Buffer = F.allocate(sizeof(TupleInst) +
                            decltype(Operands)::getExtraSize(Elements.size()),
                            alignof(TupleInst));
  return ::new(Buffer) TupleInst(Loc, Ty, Elements);
}

TupleInst::TupleInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elems)
  : SILInstruction(ValueKind::TupleInst, Loc, Ty), Operands(this, Elems) {
}

MetatypeInst::MetatypeInst(SILLocation Loc, SILType Metatype)
  : SILInstruction(ValueKind::MetatypeInst, Loc, Metatype) {}

ClassMetatypeInst::ClassMetatypeInst(SILLocation Loc, SILType Metatype,
                                     SILValue Base)
  : SILInstruction(ValueKind::ClassMetatypeInst, Loc, Metatype),
    Operands(this, Base) {}

ArchetypeMetatypeInst::ArchetypeMetatypeInst(SILLocation Loc, SILType Metatype,
                                             SILValue Base)
  : SILInstruction(ValueKind::ArchetypeMetatypeInst, Loc, Metatype),
    Operands(this, Base) {}

ProtocolMetatypeInst::ProtocolMetatypeInst(SILLocation Loc, SILType Metatype,
                                           SILValue Base)
  : SILInstruction(ValueKind::ProtocolMetatypeInst, Loc, Metatype),
    Operands(this, Base) {}

ModuleInst::ModuleInst(SILLocation Loc, SILType ModuleType)
  : SILInstruction(ValueKind::ModuleInst, Loc, ModuleType) {}

AssociatedMetatypeInst::AssociatedMetatypeInst(SILLocation Loc,
                                               SILValue MetatypeSrc,
                                               SILType MetatypeDest)
  : SILInstruction(ValueKind::AssociatedMetatypeInst, Loc, MetatypeDest),
    Operands(this, MetatypeSrc) {}

ExtractInst::ExtractInst(SILLocation Loc, SILValue Operand,
                         unsigned FieldNo, SILType ResultTy)
  : SILInstruction(ValueKind::ExtractInst, Loc, ResultTy),
    Operands(this, Operand), FieldNo(FieldNo) {
}

ElementAddrInst::ElementAddrInst(SILLocation Loc, SILValue Operand,
                                 unsigned FieldNo, SILType ResultTy)
  : SILInstruction(ValueKind::ElementAddrInst, Loc, ResultTy),
    Operands(this, Operand), FieldNo(FieldNo) {
}

RefElementAddrInst::RefElementAddrInst(SILLocation Loc, SILValue Operand,
                                       VarDecl *Field, SILType ResultTy)
  : SILInstruction(ValueKind::RefElementAddrInst, Loc, ResultTy),
    Operands(this, Operand), Field(Field) {
}

DynamicMethodInst::DynamicMethodInst(ValueKind Kind,
                                               SILLocation Loc, SILValue Operand,
                                               SILConstant Member,
                                               SILType Ty,
                                               SILFunction &F)
  : SILInstruction(Kind, Loc, Ty),
    Operands(this, Operand), Member(Member) {
}

ClassMethodInst::ClassMethodInst(SILLocation Loc, SILValue Operand,
                                 SILConstant Member,
                                 SILType Ty,
                                 SILFunction &F)
  : DynamicMethodInst(ValueKind::ClassMethodInst,
                      Loc, Operand, Member,
                      Ty, F) {
}

SuperMethodInst::SuperMethodInst(SILLocation Loc, SILValue Operand,
                                 SILConstant Member,
                                 SILType Ty,
                                 SILFunction &F)
  : DynamicMethodInst(ValueKind::SuperMethodInst,
                      Loc, Operand, Member,
                      Ty, F) {
}

ArchetypeMethodInst::ArchetypeMethodInst(SILLocation Loc, SILValue Operand,
                                         SILConstant Member,
                                         SILType Ty,
                                         SILFunction &F)
: DynamicMethodInst(ValueKind::ArchetypeMethodInst,
                    Loc, Operand, Member,
                    Ty, F) {
}

ProtocolMethodInst::ProtocolMethodInst(SILLocation Loc, SILValue Operand,
                                             SILConstant Member,
                                             SILType Ty,
                                             SILFunction &F)
  : DynamicMethodInst(ValueKind::ProtocolMethodInst,
                           Loc, Operand, Member,
                           Ty, F) {
}

ProjectExistentialInst::ProjectExistentialInst(SILLocation Loc, SILValue Operand,
                                               SILFunction &F)
  : SILInstruction(ValueKind::ProjectExistentialInst, Loc,
                SILType::getOpaquePointerType(F.getContext())),
    Operands(this, Operand) {
}

InitExistentialInst::InitExistentialInst(SILLocation Loc,
                                   SILValue Existential,
                                   SILType ConcreteType,
                                   ArrayRef<ProtocolConformance*> Conformances)
  : SILInstruction(ValueKind::InitExistentialInst, Loc,
                ConcreteType.getAddressType()),
    Operands(this, Existential),
    Conformances(Conformances) {
}

Type InitExistentialInst::getConcreteType() const {
  return getType(0).getSwiftRValueType();
}

UpcastExistentialInst::UpcastExistentialInst(SILLocation Loc,
                                 SILValue SrcExistential,
                                 SILValue DestExistential,
                                 bool isTakeOfSrc,
                                 ArrayRef<ProtocolConformance*> Conformances)
  : SILInstruction(ValueKind::UpcastExistentialInst, Loc),
    IsTakeOfSrc(isTakeOfSrc),
    Operands(this, SrcExistential, DestExistential),
    Conformances(Conformances)
{
}

DeinitExistentialInst::DeinitExistentialInst(SILLocation Loc,
                                             SILValue Existential)
  : SILInstruction(ValueKind::DeinitExistentialInst, Loc),
    Operands(this, Existential) {
}

RetainInst::RetainInst(SILLocation Loc, SILValue Operand)
  : SILInstruction(ValueKind::RetainInst, Loc, Operand.getType()),
    Operands(this, Operand) {
}

ReleaseInst::ReleaseInst(SILLocation Loc, SILValue Operand)
  : SILInstruction(ValueKind::ReleaseInst, Loc), Operands(this, Operand) {
}

DeallocVarInst::DeallocVarInst(SILLocation loc, AllocKind allocKind,
                               SILValue operand)
  : SILInstruction(ValueKind::DeallocVarInst, loc), allocKind(allocKind),
    Operands(this, operand) {
}

DeallocRefInst::DeallocRefInst(SILLocation loc, SILValue operand)
  : SILInstruction(ValueKind::DeallocRefInst, loc),
    Operands(this, operand) {
}

DestroyAddrInst::DestroyAddrInst(SILLocation Loc, SILValue Operand)
  : SILInstruction(ValueKind::DestroyAddrInst, Loc), Operands(this, Operand) {
}

//===----------------------------------------------------------------------===//
// SIL-only instructions that don't have an AST analog
//===----------------------------------------------------------------------===//

IndexAddrInst::IndexAddrInst(SILLocation Loc, SILValue Operand, unsigned Index)
  : SILInstruction(ValueKind::IndexAddrInst, Loc, Operand.getType()),
    Operands(this, Operand), Index(Index) {
}

IntegerValueInst::IntegerValueInst(uint64_t Val, SILType Ty)
  : SILInstruction(ValueKind::IntegerValueInst, SILLocation(), Ty), Val(Val) {}


//===----------------------------------------------------------------------===//
// Instructions representing terminators
//===----------------------------------------------------------------------===//


TermInst::SuccessorListTy TermInst::getSuccessors() {
  assert(isa<TermInst>(this) && "Only TermInsts are allowed");
  if (auto I = dyn_cast<UnreachableInst>(this))
    return I->getSuccessors();
  if (auto I = dyn_cast<ReturnInst>(this))
    return I->getSuccessors();
  if (auto I = dyn_cast<CondBranchInst>(this))
    return I->getSuccessors();
  return cast<BranchInst>(this)->getSuccessors();
}

UnreachableInst::UnreachableInst(SILFunction &F)
  : TermInst(ValueKind::UnreachableInst, SILLocation()) {
}

ReturnInst::ReturnInst(SILLocation Loc, SILValue ReturnValue)
  : TermInst(ValueKind::ReturnInst, Loc),
    Operands(this, ReturnValue) {
}

BranchInst::BranchInst(SILLocation Loc,
                       SILBasicBlock *DestBB,
                       ArrayRef<SILValue> Args)
  : TermInst(ValueKind::BranchInst, Loc),
    DestBB(this, DestBB), Operands(this, Args)
{
  assert(Args.size() == DestBB->bbarg_size() &&
         "branch argument count does not match target bb");
}

BranchInst *BranchInst::create(SILLocation Loc,
                               SILBasicBlock *DestBB,
                               SILFunction &F) {
  return create(Loc, DestBB, {}, F);
}

BranchInst *BranchInst::create(SILLocation Loc,
                               SILBasicBlock *DestBB, ArrayRef<SILValue> Args,
                               SILFunction &F) {
  void *Buffer = F.allocate(sizeof(BranchInst) +
                              decltype(Operands)::getExtraSize(Args.size()),
                            alignof(BranchInst));
  return ::new (Buffer) BranchInst(Loc, DestBB, Args);
}

CondBranchInst::CondBranchInst(SILLocation Loc, SILValue Condition,
                               SILBasicBlock *TrueBB, SILBasicBlock *FalseBB,
                               ArrayRef<SILValue> Args)
  : TermInst(ValueKind::CondBranchInst, Loc),
    DestBBs{{this, TrueBB}, {this, FalseBB}},
    Operands(this, Args, Condition)
{
}

CondBranchInst *CondBranchInst::create(SILLocation Loc, SILValue Condition,
                                       SILBasicBlock *TrueBB, SILBasicBlock *FalseBB,
                                       SILFunction &F) {
  return create(Loc, Condition, TrueBB, {}, FalseBB, {}, F);
}

CondBranchInst *CondBranchInst::create(SILLocation Loc, SILValue Condition,
                               SILBasicBlock *TrueBB, ArrayRef<SILValue> TrueArgs,
                               SILBasicBlock *FalseBB, ArrayRef<SILValue> FalseArgs,
                               SILFunction &F) {
  assert(TrueArgs.size() == TrueBB->bbarg_size() &&
         FalseArgs.size() == FalseBB->bbarg_size() &&
         "branch argument counts do not match target bbs");

  SmallVector<SILValue, 4> Args;
  Args.append(TrueArgs.begin(), TrueArgs.end());
  Args.append(FalseArgs.begin(), FalseArgs.end());

  void *Buffer = F.allocate(sizeof(CondBranchInst) +
                              decltype(Operands)::getExtraSize(Args.size()),
                            alignof(CondBranchInst));
  return ::new (Buffer) CondBranchInst(Loc, Condition, TrueBB, FalseBB, Args);
}

OperandValueArrayRef CondBranchInst::getTrueArgs() const {
  return Operands.asValueArray().slice(1, getTrueBB()->bbarg_size());
}

OperandValueArrayRef CondBranchInst::getFalseArgs() const {
  return Operands.asValueArray().slice(1 + getTrueBB()->bbarg_size(),
                                       getFalseBB()->bbarg_size());
}
