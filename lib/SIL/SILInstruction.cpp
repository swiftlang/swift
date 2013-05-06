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
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/SmallString.h"
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

FunctionRefInst::FunctionRefInst(SILLocation Loc, SILFunction *F)
  : SILInstruction(ValueKind::FunctionRefInst, Loc, F->getLoweredType()),
    Function(F) {
}

template<typename INST>
static void *allocateLiteralInstWithTextSize(SILFunction &B, unsigned length) {
  return B.allocate(sizeof(INST) + length, alignof(INST));
}

IntegerLiteralInst::IntegerLiteralInst(SILLocation Loc, SILType Ty,
                                       StringRef Text)
  : SILInstruction(ValueKind::IntegerLiteralInst, Loc, Ty),
    length(Text.size())
{
  memcpy(this + 1, Text.data(), Text.size());
}

IntegerLiteralInst *
IntegerLiteralInst::create(SILLocation Loc, SILType Ty,
                           StringRef Text, SILFunction &B) {
  void *buf = allocateLiteralInstWithTextSize<IntegerLiteralInst>(B,
                                                                  Text.size());
  return ::new (buf) IntegerLiteralInst(Loc, Ty, Text);
}

IntegerLiteralInst *
IntegerLiteralInst::create(SILLocation Loc, SILType Ty, intmax_t Value,
                           SILFunction &B) {
  llvm::SmallString<12> s;
  llvm::raw_svector_ostream ss(s);
  ss << Value;
  ss.flush();
  
  return create(Loc, Ty, s, B);
}

IntegerLiteralInst *
IntegerLiteralInst::create(swift::IntegerLiteralExpr *E, SILFunction &B) {
  return create(E,
                // Builtin integer types are always valid SIL types.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                           /*address=*/false,
                                           /*loadable=*/true),
                E->getText(), B);
}

IntegerLiteralInst *
IntegerLiteralInst::create(CharacterLiteralExpr *E, SILFunction &B) {
  return create(E,
                // Builtin integer types are always valid SIL types.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                           /*address=*/false,
                                           /*loadable=*/true),
                E->getValue(), B);
}

/// getValue - Return the APInt for the underlying integer literal.
APInt IntegerLiteralInst::getValue() const {
  return IntegerLiteralExpr::getValue(getText(),
                        getType().castTo<BuiltinIntegerType>()->getBitWidth());
}

FloatLiteralInst::FloatLiteralInst(SILLocation Loc, SILType Ty, StringRef Text)
  : SILInstruction(ValueKind::FloatLiteralInst, Loc, Ty),
    length(Text.size())
{
  memcpy(this + 1, Text.data(), Text.size());
}

FloatLiteralInst *
FloatLiteralInst::create(SILLocation Loc, SILType Ty, StringRef Text,
                         SILFunction &B) {
  void *buf = allocateLiteralInstWithTextSize<FloatLiteralInst>(B, Text.size());
  return ::new (buf) FloatLiteralInst(Loc, Ty, Text);
}

FloatLiteralInst *
FloatLiteralInst::create(FloatLiteralExpr *E, SILFunction &B) {
  return create(E,
                // Builtin floating-point types are always valid SIL types.
                SILType::getPreLoweredType(E->getType()->getCanonicalType(),
                                           /*address=*/false,
                                           /*loadable=*/true),
                E->getText(), B);
}

APFloat FloatLiteralInst::getValue() const {
  return FloatLiteralExpr::getValue(getText(),
                  getType().castTo<BuiltinFloatType>()->getAPFloatSemantics());
}

StringLiteralInst::StringLiteralInst(SILLocation Loc, SILType Ty,
                                     StringRef Text)
  : SILInstruction(ValueKind::StringLiteralInst, Loc, Ty),
    length(Text.size())
{
  memcpy(this + 1, Text.data(), Text.size());
}

StringLiteralInst *
StringLiteralInst::create(SILLocation Loc, SILType Ty, StringRef Text,
                          SILFunction &B) {
  void *buf
    = allocateLiteralInstWithTextSize<StringLiteralInst>(B, Text.size());
  return ::new (buf) StringLiteralInst(Loc, Ty, Text);
}

StringLiteralInst *
StringLiteralInst::create(StringLiteralExpr *E, SILType ty, SILFunction &B) {
  return create(E, ty, E->getValue(), B);
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

SuperToArchetypeInst::SuperToArchetypeInst(SILLocation Loc,
                                           SILValue SrcBase,
                                           SILValue DestArchetypeAddress)
  : SILInstruction(ValueKind::SuperToArchetypeInst, Loc),
    Operands(this, SrcBase, DestArchetypeAddress) {
}

StructInst *StructInst::createImpl(SILLocation Loc, SILType Ty,
                                 ArrayRef<SILValue> Elements, SILFunction &F) {
  void *Buffer = F.allocate(sizeof(StructInst) +
                            decltype(Operands)::getExtraSize(Elements.size()),
                            alignof(StructInst));
  return ::new(Buffer) StructInst(Loc, Ty, Elements);
}

StructInst::StructInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elems)
  : SILInstruction(ValueKind::StructInst, Loc, Ty), Operands(this, Elems) {
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

ModuleInst::ModuleInst(SILLocation Loc, SILType ModuleType)
  : SILInstruction(ValueKind::ModuleInst, Loc, ModuleType) {}

ProjectExistentialInst::ProjectExistentialInst(SILLocation Loc, SILValue Operand,
                                               SILFunction &F)
  : UnaryInstructionBase(Loc, Operand,
                         SILType::getOpaquePointerType(F.getContext()))
{}

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


//===----------------------------------------------------------------------===//
// Instructions representing terminators
//===----------------------------------------------------------------------===//


TermInst::SuccessorListTy TermInst::getSuccessors() {
  assert(isa<TermInst>(this) && "Only TermInsts are allowed");
  if (auto I = dyn_cast<UnreachableInst>(this))
    return I->getSuccessors();
  if (auto I = dyn_cast<ReturnInst>(this))
    return I->getSuccessors();
  if (auto I = dyn_cast<AutoreleaseReturnInst>(this))
    return I->getSuccessors();
  if (auto I = dyn_cast<CondBranchInst>(this))
    return I->getSuccessors();
  return cast<BranchInst>(this)->getSuccessors();
}

UnreachableInst::UnreachableInst(SILFunction &F)
  : TermInst(ValueKind::UnreachableInst, SILLocation()) {
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
