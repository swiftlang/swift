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
#include "llvm/ADT/APInt.h"
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


SILFunction *SILInstruction::getFunction() {
  return getParent()->getParent();
}
const SILFunction *SILInstruction::getFunction() const {
  return getParent()->getParent();
}

SILModule *SILInstruction::getModule() {
  return getFunction()->getParent();
}
const SILModule *SILInstruction::getModule() const {
  return getFunction()->getParent();
}


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
#define INST(CLASS, PARENT, MEMBEHAVIOR) \
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

/// getOperandNumber - Return which operand this is in the operand list of the
/// using instruction.
unsigned Operand::getOperandNumber() const {
  return this - &cast<SILInstruction>(getUser())->getAllOperands()[0];
}

SILInstructionMemoryBehavior SILInstruction::getMemoryBehavior() const {
  switch (getKind()) {
#define INST(CLASS, PARENT, MEMBEHAVIOR) \
  case ValueKind::CLASS: return SILInstructionMemoryBehavior::MEMBEHAVIOR;
#include "swift/SIL/SILNodes.def"
  case ValueKind::SILArgument:
    llvm_unreachable("Non-instructions are unreachable.");
  }
  llvm_unreachable("We've just exhausted the switch.");
}

bool SILInstruction::mayHaveSideEffects() const {
  SILInstructionMemoryBehavior B = getMemoryBehavior();
  if (B == SILInstructionMemoryBehavior::MayWrite ||
      B == SILInstructionMemoryBehavior::MayHaveSideEffects ||
      B == SILInstructionMemoryBehavior::MayWriteAndHaveSideEffects)
    return true;
  return false;
}

//===----------------------------------------------------------------------===//
// SILInstruction Subclasses
//===----------------------------------------------------------------------===//

AllocStackInst::AllocStackInst(SILLocation loc, SILType elementType, SILFunction &F)
  : SILInstruction(ValueKind::AllocStackInst, loc, elementType.getAddressType()) {
}

/// getDecl - Return the underlying variable declaration associated with this
/// allocation, or null if this is a temporary allocation.
VarDecl *AllocStackInst::getDecl() const {
  return getLoc().getAs<VarDecl>();
}

AllocRefInst::AllocRefInst(SILLocation loc, SILType elementType, SILFunction &F)
  : SILInstruction(ValueKind::AllocRefInst, loc, elementType) {
}


// Allocations always return two results: Builtin.ObjectPointer & LValue[EltTy]
static SILTypeList *getAllocType(SILType EltTy, SILFunction &F) {
  const ASTContext &Ctx = EltTy.getASTContext();

  SILType ResTys[] = {
    SILType::getObjectPointerType(Ctx),
    EltTy.getAddressType()
  };

  return F.getModule().getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(SILLocation Loc, SILType ElementType, SILFunction &F)
  : SILInstruction(ValueKind::AllocBoxInst, Loc, getAllocType(ElementType, F)) {
}

AllocArrayInst::AllocArrayInst(SILLocation Loc, SILType ElementType,
                               SILValue NumElements, SILFunction &F)
  : SILInstruction(ValueKind::AllocArrayInst, Loc, getAllocType(ElementType, F)),
    Operands(this, NumElements) {
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

  void *Buffer = F.getModule().allocate(sizeof(DERIVED) +
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

static unsigned getWordsForBitWidth(unsigned bits) {
  return (bits + llvm::integerPartWidth - 1)/llvm::integerPartWidth;
}

template<typename INST>
static void *allocateLiteralInstWithTextSize(SILFunction &F, unsigned length) {
  return F.getModule().allocate(sizeof(INST) + length, alignof(INST));
}

template<typename INST>
static void *allocateLiteralInstWithBitSize(SILFunction &F, unsigned bits) {
  unsigned words = getWordsForBitWidth(bits);
  return F.getModule().allocate(sizeof(INST) + sizeof(llvm::integerPart)*words,
                                alignof(INST));
}

IntegerLiteralInst::IntegerLiteralInst(SILLocation Loc, SILType Ty,
                                       const llvm::APInt &Value)
  : SILInstruction(ValueKind::IntegerLiteralInst, Loc, Ty),
    numBits(Value.getBitWidth())
{
  memcpy(this + 1, Value.getRawData(),
         Value.getNumWords() * sizeof(llvm::integerPart));
}

IntegerLiteralInst *
IntegerLiteralInst::create(SILLocation Loc, SILType Ty, const APInt &Value,
                           SILFunction &B) {
  auto *intTy = Ty.castTo<BuiltinIntegerType>();
  assert(intTy->getBitWidth() == Value.getBitWidth() &&
         "IntegerLiteralInst APInt value's bit width doesn't match type");
  
  void *buf = allocateLiteralInstWithBitSize<IntegerLiteralInst>(B,
                                                          Value.getBitWidth());
  return ::new (buf) IntegerLiteralInst(Loc, Ty, Value);
}

IntegerLiteralInst *
IntegerLiteralInst::create(SILLocation Loc, SILType Ty,
                           intmax_t Value, SILFunction &B) {
  auto *intTy = Ty.castTo<BuiltinIntegerType>();
  return create(Loc, Ty,
                APInt(intTy->getBitWidth(), Value), B);
}

IntegerLiteralInst *
IntegerLiteralInst::create(IntegerLiteralExpr *E, SILFunction &F) {
  return create(E,
                SILType::getBuiltinIntegerType(
                     E->getType()->castTo<BuiltinIntegerType>()->getBitWidth(),
                     F.getASTContext()),
                E->getValue(), F);
}

IntegerLiteralInst *
IntegerLiteralInst::create(CharacterLiteralExpr *E, SILFunction &F) {
  return create(E,
                SILType::getBuiltinIntegerType(
                     E->getType()->castTo<BuiltinIntegerType>()->getBitWidth(),
                     F.getASTContext()),
                E->getValue(), F);
}

/// getValue - Return the APInt for the underlying integer literal.
APInt IntegerLiteralInst::getValue() const {
  return APInt(numBits,
               {reinterpret_cast<const llvm::integerPart *>(this + 1),
                 getWordsForBitWidth(numBits)});
}

FloatLiteralInst::FloatLiteralInst(SILLocation Loc, SILType Ty,
                                   const APInt &Bits)
  : SILInstruction(ValueKind::FloatLiteralInst, Loc, Ty),
    numBits(Bits.getBitWidth())
{
  memcpy(this + 1, Bits.getRawData(),
         Bits.getNumWords() * sizeof(llvm::integerPart));
}

FloatLiteralInst *
FloatLiteralInst::create(SILLocation Loc, SILType Ty, const APFloat &Value,
                         SILFunction &B) {
  auto *floatTy = Ty.castTo<BuiltinFloatType>();
  assert(&floatTy->getAPFloatSemantics() == &Value.getSemantics() &&
         "FloatLiteralInst value's APFloat semantics do not match type");
  
  APInt Bits = Value.bitcastToAPInt();
  
  void *buf = allocateLiteralInstWithBitSize<FloatLiteralInst>(B,
                                                            Bits.getBitWidth());
  return ::new (buf) FloatLiteralInst(Loc, Ty, Bits);
}

FloatLiteralInst *
FloatLiteralInst::create(FloatLiteralExpr *E, SILFunction &F) {
  return create(E,
                // Builtin floating-point types are always valid SIL types.
                SILType::getBuiltinFloatType(
                         E->getType()->castTo<BuiltinFloatType>()->getFPKind(),
                         F.getASTContext()),
                E->getValue(), F);
}

APInt FloatLiteralInst::getBits() const {
  return APInt(numBits,
               {reinterpret_cast<const llvm::integerPart *>(this + 1),
                 getWordsForBitWidth(numBits)});
}

APFloat FloatLiteralInst::getValue() const {
  return APFloat(getType().castTo<BuiltinFloatType>()->getAPFloatSemantics(),
                 getBits());
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
                           IsTake_t isTakeOfSrc,
                           IsInitialization_t isInitializationOfDest)
  : SILInstruction(ValueKind::CopyAddrInst, Loc),
    IsTakeOfSrc(isTakeOfSrc), IsInitializationOfDest(isInitializationOfDest),
    Operands(this, SrcLValue, DestLValue)
{
}

SpecializeInst *SpecializeInst::create(SILLocation Loc, SILValue Operand,
                                       ArrayRef<Substitution> Substitutions,
                                       SILType DestTy, SILFunction &F) {
 void *Buffer = F.getModule().allocate(
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

StructInst *StructInst::createImpl(SILLocation Loc, SILType Ty,
                                 ArrayRef<SILValue> Elements, SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(StructInst) +
                            decltype(Operands)::getExtraSize(Elements.size()),
                            alignof(StructInst));
  return ::new(Buffer) StructInst(Loc, Ty, Elements);
}

StructInst::StructInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elems)
  : SILInstruction(ValueKind::StructInst, Loc, Ty), Operands(this, Elems) {
}

TupleInst *TupleInst::createImpl(SILLocation Loc, SILType Ty,
                                 ArrayRef<SILValue> Elements, SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(TupleInst) +
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

ProjectExistentialInst::ProjectExistentialInst(SILLocation Loc,
                                               SILValue Operand,
                                               SILType ThisTy)
  : UnaryInstructionBase(Loc, Operand, ThisTy)
{}

ProjectExistentialRefInst::ProjectExistentialRefInst(SILLocation Loc,
                                                     SILValue Operand,
                                                     SILFunction &F)
  : UnaryInstructionBase(Loc, Operand,
                         SILType::getObjCPointerType(F.getASTContext()))
{}


UpcastExistentialInst::UpcastExistentialInst(SILLocation Loc,
                                 SILValue SrcExistential,
                                 SILValue DestExistential,
                                 IsTake_t isTakeOfSrc)
  : SILInstruction(ValueKind::UpcastExistentialInst, Loc),
    IsTakeOfSrc(isTakeOfSrc),
    Operands(this, SrcExistential, DestExistential)
{
}


//===----------------------------------------------------------------------===//
// Instructions representing terminators
//===----------------------------------------------------------------------===//


TermInst::SuccessorListTy TermInst::getSuccessors() {
  #define TERMINATOR(TYPE, PARENT, EFFECT) \
    if (auto I = dyn_cast<TYPE>(this)) \
      return I->getSuccessors();
  #include "swift/SIL/SILNodes.def"
  
  llvm_unreachable("not a terminator?!");
}

BranchInst::BranchInst(SILLocation Loc,
                       SILBasicBlock *DestBB,
                       ArrayRef<SILValue> Args)
  : TermInst(ValueKind::BranchInst, Loc),
    DestBB(this, DestBB), Operands(this, Args) {}

BranchInst *BranchInst::create(SILLocation Loc,
                               SILBasicBlock *DestBB,
                               SILFunction &F) {
  return create(Loc, DestBB, {}, F);
}

BranchInst *BranchInst::create(SILLocation Loc,
                               SILBasicBlock *DestBB, ArrayRef<SILValue> Args,
                               SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(BranchInst) +
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
                                       SILBasicBlock *TrueBB,
                                       SILBasicBlock *FalseBB,
                                       SILFunction &F) {
  return create(Loc, Condition, TrueBB, {}, FalseBB, {}, F);
}

CondBranchInst *CondBranchInst::create(SILLocation Loc, SILValue Condition,
                          SILBasicBlock *TrueBB, ArrayRef<SILValue> TrueArgs,
                          SILBasicBlock *FalseBB, ArrayRef<SILValue> FalseArgs,
                          SILFunction &F) {
  SmallVector<SILValue, 4> Args;
  Args.append(TrueArgs.begin(), TrueArgs.end());
  Args.append(FalseArgs.begin(), FalseArgs.end());

  void *Buffer = F.getModule().allocate(sizeof(CondBranchInst) +
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

SwitchIntInst::SwitchIntInst(SILLocation Loc, SILValue Operand,
                             SILBasicBlock *DefaultBB,
                             ArrayRef<std::pair<APInt, SILBasicBlock*>> CaseBBs)
  : TermInst(ValueKind::SwitchIntInst, Loc),
    Operands(this, Operand),
    NumCases(CaseBBs.size()),
    HasDefault(bool(DefaultBB))
{
  // Initialize the case and successor arrays.
  auto *cases = getCaseBuf();
  auto *succs = getSuccessorBuf();
  
  unsigned words = getNumWordsForCase();
  
  for (unsigned i = 0, size = CaseBBs.size(); i < size; ++i) {
    assert(CaseBBs[i].first.getBitWidth() == getBitWidthForCase() &&
           "switch_int case value is not same bit width as operand");
    memcpy(cases + i*words, CaseBBs[i].first.getRawData(),
           words * sizeof(llvm::integerPart));
    ::new (succs + i) SILSuccessor(this, CaseBBs[i].second);
  }
  
  if (HasDefault)
    ::new (succs + NumCases) SILSuccessor(this, DefaultBB);
}

SwitchIntInst::~SwitchIntInst() {
  // Destroy the successor records to keep the CFG up to date.
  auto *succs = getSuccessorBuf();
  for (unsigned i = 0, end = NumCases + HasDefault; i < end; ++i) {
    succs[i].~SILSuccessor();
  }
}

SwitchIntInst *SwitchIntInst::create(SILLocation Loc, SILValue Operand,
                           SILBasicBlock *DefaultBB,
                           ArrayRef<std::pair<APInt, SILBasicBlock *>> CaseBBs,
                           SILFunction &F) {
  // Allocate enough room for the instruction with tail-allocated data for all
  // the APInt values and the SILSuccessor arrays. There are `CaseBBs.size()`
  // APInts (each needing `getNumWords()` `llvm::integerPart`s of storage) and
  // `CaseBBs.size() + (DefaultBB ? 1 : 0)` successors.
  unsigned numCases = CaseBBs.size();
  unsigned numSuccessors = numCases + (DefaultBB ? 1 : 0);
  
  unsigned bits = Operand.getType().castTo<BuiltinIntegerType>()->getBitWidth();
  unsigned words = (bits + llvm::integerPartWidth - 1) / llvm::integerPartWidth;
  
  void *buf = F.getModule().allocate(sizeof(SwitchIntInst)
                                      + sizeof(llvm::integerPart) * numCases
                                                                  * words
                                      + sizeof(SILSuccessor) * numSuccessors,
                                     alignof(SwitchIntInst));
  return ::new (buf) SwitchIntInst(Loc, Operand, DefaultBB, CaseBBs);
}

SwitchUnionInst::SwitchUnionInst(SILLocation Loc, SILValue Operand,
                SILBasicBlock *DefaultBB,
                ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs)
  : TermInst(ValueKind::SwitchUnionInst, Loc),
    Operands(this, Operand),
    NumCases(CaseBBs.size()),
    HasDefault(bool(DefaultBB))
{
  // Initialize the case and successor arrays.
  auto *cases = getCaseBuf();
  auto *succs = getSuccessorBuf();
  for (unsigned i = 0, size = CaseBBs.size(); i < size; ++i) {
    cases[i] = CaseBBs[i].first;
    ::new (succs + i) SILSuccessor(this, CaseBBs[i].second);
  }
  
  if (HasDefault)
    ::new (succs + NumCases) SILSuccessor(this, DefaultBB);
}

SwitchUnionInst::~SwitchUnionInst() {
  // Destroy the successor records to keep the CFG up to date.
  auto *succs = getSuccessorBuf();
  for (unsigned i = 0, end = NumCases + HasDefault; i < end; ++i) {
    succs[i].~SILSuccessor();
  }
}

SwitchUnionInst *SwitchUnionInst::create(SILLocation Loc, SILValue Operand,
                SILBasicBlock *DefaultBB,
                ArrayRef<std::pair<UnionElementDecl*, SILBasicBlock*>> CaseBBs,
                SILFunction &F) {
  // Allocate enough room for the instruction with tail-allocated
  // UnionElementDecl and SILSuccessor arrays. There are `CaseBBs.size()` decls
  // and `CaseBBs.size() + (DefaultBB ? 1 : 0)` successors.
  unsigned numCases = CaseBBs.size();
  unsigned numSuccessors = numCases + (DefaultBB ? 1 : 0);
  
  void *buf = F.getModule().allocate(sizeof(SwitchUnionInst)
                                       + sizeof(UnionElementDecl*) * numCases
                                       + sizeof(SILSuccessor) * numSuccessors,
                                     alignof(SwitchUnionInst));
  return ::new (buf) SwitchUnionInst(Loc, Operand, DefaultBB, CaseBBs);
}
