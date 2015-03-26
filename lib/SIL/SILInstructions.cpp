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
#include "swift/Basic/type_traits.h"
#include "swift/Basic/Unicode.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/AST/AST.h"
#include "swift/Basic/AssertImplements.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
// SILInstruction Subclasses
//===----------------------------------------------------------------------===//

// alloc_stack always returns two results: Builtin.RawPointer & LValue[EltTy]
static SILTypeList *getAllocStackType(SILType eltTy, SILFunction &F) {
  SILType resTys[] = {
    eltTy.getLocalStorageType(),
    eltTy.getAddressType()
  };

  return F.getModule().getSILTypeList(resTys);
}

AllocStackInst::AllocStackInst(SILLocation loc, SILType elementType, SILFunction &F)
  : AllocationInst(ValueKind::AllocStackInst, loc,
                   getAllocStackType(elementType, F)) {
}

/// getDecl - Return the underlying variable declaration associated with this
/// allocation, or null if this is a temporary allocation.
VarDecl *AllocStackInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}

AllocRefInst::AllocRefInst(SILLocation loc, SILType elementType, SILFunction &F,
                           bool objc)
  : AllocationInst(ValueKind::AllocRefInst, loc, elementType), ObjC(objc) {
}


// alloc_box returns two results: Builtin.NativeObject & LValue[EltTy]
static SILTypeList *getAllocBoxType(SILType EltTy, SILFunction &F) {
  const ASTContext &Ctx = F.getModule().getASTContext();

  SILType ResTys[] = {
    SILType::getNativeObjectType(Ctx),
    EltTy.getAddressType()
  };

  return F.getModule().getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(SILLocation Loc, SILType ElementType, SILFunction &F)
  : AllocationInst(ValueKind::AllocBoxInst, Loc,
                   getAllocBoxType(ElementType, F)) {
}

/// getDecl - Return the underlying variable declaration associated with this
/// allocation, or null if this is a temporary allocation.
VarDecl *AllocBoxInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}

VarDecl *DebugValueInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}
VarDecl *DebugValueAddrInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}

static SILTypeList *getAllocExistentialBoxType(SILType ExistTy,
                                               SILType ConcreteTy,
                                               SILFunction &F) {
  SILType Tys[] = {
    ExistTy.getObjectType(),
    ConcreteTy.getAddressType(),
  };
  return F.getModule().getSILTypeList(Tys);
}

AllocExistentialBoxInst::AllocExistentialBoxInst(SILLocation Loc,
                                 SILType ExistentialType,
                                 CanType ConcreteType,
                                 SILType ConcreteLoweredType,
                                 ArrayRef<ProtocolConformance *> Conformances,
                                 SILFunction *Parent)
  : AllocationInst(ValueKind::AllocExistentialBoxInst, Loc,
                   getAllocExistentialBoxType(ExistentialType,
                                              ConcreteLoweredType, *Parent)),
    ConcreteType(ConcreteType),
    Conformances(Conformances)
{
}

static void declareWitnessTable(SILModule &Mod,
                                ProtocolConformance *C) {
  if (!C) return;
  if (!Mod.lookUpWitnessTable(C, false).first)
    Mod.createWitnessTableDeclaration(C,
        TypeConverter::getLinkageForProtocolConformance(
                                                  C->getRootNormalConformance(),
                                                  NotForDefinition));
}

AllocExistentialBoxInst *
AllocExistentialBoxInst::create(SILLocation Loc,
                                SILType ExistentialType,
                                CanType ConcreteType,
                                SILType ConcreteLoweredType,
                                ArrayRef<ProtocolConformance *> Conformances,
                                SILFunction *F) {
  SILModule &Mod = F->getModule();
  void *Buffer = Mod.allocate(sizeof(AllocExistentialBoxInst),
                              alignof(AllocExistentialBoxInst));
  for (ProtocolConformance *C : Conformances)
    declareWitnessTable(Mod, C);
  return ::new (Buffer) AllocExistentialBoxInst(Loc,
                                                ExistentialType,
                                                ConcreteType,
                                                ConcreteLoweredType,
                                                Conformances, F);
}

BuiltinInst *BuiltinInst::create(SILLocation Loc, Identifier Name,
                                 SILType ReturnType,
                                 ArrayRef<Substitution> Substitutions,
                                 ArrayRef<SILValue> Args,
                                 SILFunction &F) {
  void *Buffer = F.getModule().allocate(
                              sizeof(BuiltinInst)
                                + decltype(Operands)::getExtraSize(Args.size())
                                + sizeof(Substitution) * Substitutions.size(),
                              alignof(BuiltinInst));
  return ::new (Buffer) BuiltinInst(Loc, Name, ReturnType, Substitutions,
                                    Args);
}

BuiltinInst::BuiltinInst(SILLocation Loc,
                         Identifier Name,
                         SILType ReturnType,
                         ArrayRef<Substitution> Subs,
                         ArrayRef<SILValue> Args)
  : SILInstruction(ValueKind::BuiltinInst, Loc, ReturnType),
    Name(Name),
    NumSubstitutions(Subs.size()),
    Operands(this, Args)
{
  static_assert(IsTriviallyCopyable<Substitution>::value,
                "assuming Substitution is trivially copyable");
  memcpy(getSubstitutionsStorage(), Subs.begin(),
         sizeof(Substitution) * Subs.size());
}

ApplyInstBase::ApplyInstBase(ValueKind Kind, SILLocation Loc, SILValue Callee,
    SILType SubstCalleeType,
    ArrayRef<Substitution> Subs,
    ArrayRef<SILValue> Args,
    SILType Ty)
  : SILInstruction(Kind, Loc, Ty),
    SubstCalleeType(SubstCalleeType),
    NumSubstitutions(Subs.size()),
    Operands(this, Args, Callee)
{
  static_assert(IsTriviallyCopyable<Substitution>::value,
                "assuming Substitution is trivial");
  memcpy(getSubstitutionsStorage(), Subs.begin(),
         sizeof(Substitution) * Subs.size());
}


ApplyInst::ApplyInst(SILLocation Loc, SILValue Callee,
                     SILType SubstCalleeTy,
                     SILType Result,
                     ArrayRef<Substitution> Subs,
                     ArrayRef<SILValue> Args)
  : ApplyInstBase(ValueKind::ApplyInst, Loc, Callee, SubstCalleeTy,
                  Subs, Args, Result)
{
}

ApplyInst *ApplyInst::create(SILLocation Loc, SILValue Callee,
                             SILType SubstCalleeTy,
                             SILType Result,
                             ArrayRef<Substitution> Subs,
                             ArrayRef<SILValue> Args,
                             SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(ApplyInst)
                              + decltype(Operands)::getExtraSize(Args.size())
                              + sizeof(Substitution) * Subs.size(),
                            alignof(ApplyInst));
  return ::new(Buffer) ApplyInst(Loc, Callee, SubstCalleeTy,
                                 Result, Subs, Args);
}

bool ApplyInst::hasSemantics(StringRef SemanticsString) const {
  if (auto *FRI = dyn_cast<FunctionRefInst>(getCallee()))
    if (auto *F = FRI->getReferencedFunction())
      return F->hasSemanticsString(SemanticsString);

  return false;
}

PartialApplyInst::PartialApplyInst(SILLocation Loc, SILValue Callee,
                                   SILType SubstCalleeTy,
                                   ArrayRef<Substitution> Subs,
                                   ArrayRef<SILValue> Args, SILType ClosureType)
// FIXME: the callee should have a lowered SIL function type, and PartialApplyInst
// should derive the type of its result by partially applying the callee's type.
  : ApplyInstBase(ValueKind::PartialApplyInst, Loc, Callee, SubstCalleeTy,
                  Subs, Args, ClosureType)
{
}

PartialApplyInst *PartialApplyInst::create(SILLocation Loc, SILValue Callee,
                                           SILType SubstCalleeTy,
                                           ArrayRef<Substitution> Subs,
                                           ArrayRef<SILValue> Args,
                                           SILType ClosureType,
                                           SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(PartialApplyInst)
                              + decltype(Operands)::getExtraSize(Args.size())
                              + sizeof(Substitution) * Subs.size(),
                            alignof(PartialApplyInst));
  return ::new(Buffer) PartialApplyInst(Loc, Callee, SubstCalleeTy,
                                        Subs, Args, ClosureType);
}

FunctionRefInst::FunctionRefInst(SILLocation Loc, SILFunction *F)
  : LiteralInst(ValueKind::FunctionRefInst, Loc, F->getLoweredType()),
    Function(F) {
  F->incrementRefCount();
}

FunctionRefInst::~FunctionRefInst() {
  if (Function)
    Function->decrementRefCount();
}

void FunctionRefInst::dropReferencedFunction() {
  if (Function)
    Function->decrementRefCount();
  Function = nullptr;
}

GlobalAddrInst::GlobalAddrInst(SILLocation Loc, SILGlobalVariable *Global)
  : LiteralInst(ValueKind::GlobalAddrInst, Loc,
                Global->getLoweredType().getAddressType()),
    Global(Global)
{}

GlobalAddrInst::GlobalAddrInst(SILLocation Loc, SILType Ty)
  : LiteralInst(ValueKind::GlobalAddrInst, Loc, Ty),
    Global(nullptr)
{}

const IntrinsicInfo &BuiltinInst::getIntrinsicInfo() const {
  return getModule().getIntrinsicInfo(getName());
}

const BuiltinInfo &BuiltinInst::getBuiltinInfo() const {
  return getModule().getBuiltinInfo(getName());
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
  : LiteralInst(ValueKind::IntegerLiteralInst, Loc, Ty),
    numBits(Value.getBitWidth())
{
  memcpy(this + 1, Value.getRawData(),
         Value.getNumWords() * sizeof(llvm::integerPart));
}

IntegerLiteralInst *
IntegerLiteralInst::create(SILLocation Loc, SILType Ty, const APInt &Value,
                           SILFunction &B) {
  auto intTy = Ty.castTo<BuiltinIntegerType>();
  assert(intTy->getGreatestWidth() == Value.getBitWidth() &&
         "IntegerLiteralInst APInt value's bit width doesn't match type");
  (void)intTy;

  void *buf = allocateLiteralInstWithBitSize<IntegerLiteralInst>(B,
                                                          Value.getBitWidth());
  return ::new (buf) IntegerLiteralInst(Loc, Ty, Value);
}

IntegerLiteralInst *
IntegerLiteralInst::create(SILLocation Loc, SILType Ty,
                           intmax_t Value, SILFunction &B) {
  auto intTy = Ty.castTo<BuiltinIntegerType>();
  return create(Loc, Ty,
                APInt(intTy->getGreatestWidth(), Value), B);
}

IntegerLiteralInst *
IntegerLiteralInst::create(IntegerLiteralExpr *E, SILFunction &F) {
  return create(E,
                SILType::getBuiltinIntegerType(
                     E->getType()->castTo<BuiltinIntegerType>()
                      ->getGreatestWidth(),
                     F.getASTContext()),
                E->getValue(), F);
}

IntegerLiteralInst *
IntegerLiteralInst::create(CharacterLiteralExpr *E, SILFunction &F) {
  return create(E,
              SILType::getPrimitiveObjectType(E->getType()->getCanonicalType()),
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
  : LiteralInst(ValueKind::FloatLiteralInst, Loc, Ty),
    numBits(Bits.getBitWidth())
{
  memcpy(this + 1, Bits.getRawData(),
         Bits.getNumWords() * sizeof(llvm::integerPart));
}

FloatLiteralInst *
FloatLiteralInst::create(SILLocation Loc, SILType Ty, const APFloat &Value,
                         SILFunction &B) {
  auto floatTy = Ty.castTo<BuiltinFloatType>();
  assert(&floatTy->getAPFloatSemantics() == &Value.getSemantics() &&
         "FloatLiteralInst value's APFloat semantics do not match type");
  (void)floatTy;

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

StringLiteralInst::StringLiteralInst(SILLocation Loc, StringRef Text,
                                     Encoding encoding, SILType Ty)
  : LiteralInst(ValueKind::StringLiteralInst, Loc, Ty),
    Length(Text.size()), TheEncoding(encoding)
{
  memcpy(this + 1, Text.data(), Text.size());
}

StringLiteralInst *
StringLiteralInst::create(SILLocation loc, StringRef text, Encoding encoding,
                          SILFunction &F) {
  void *buf
    = allocateLiteralInstWithTextSize<StringLiteralInst>(F, text.size());

  auto Ty = SILType::getRawPointerType(F.getModule().getASTContext());
  return ::new (buf) StringLiteralInst(loc, text, encoding, Ty);
}

uint64_t StringLiteralInst::getCodeUnitCount() {
  if (TheEncoding == Encoding::UTF16)
    return unicode::getUTF16Length(getValue());
  return Length;
}

StoreInst::StoreInst(SILLocation Loc, SILValue Src, SILValue Dest)
  : SILInstruction(ValueKind::StoreInst, Loc),
    Operands(this, Src, Dest) {
}

AssignInst::AssignInst(SILLocation Loc, SILValue Src, SILValue Dest)
  : SILInstruction(ValueKind::AssignInst, Loc),
    Operands(this, Src, Dest) {
}

MarkFunctionEscapeInst *
MarkFunctionEscapeInst::create(SILLocation Loc,
                               ArrayRef<SILValue> Elements, SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(MarkFunctionEscapeInst) +
                              decltype(Operands)::getExtraSize(Elements.size()),
                                        alignof(MarkFunctionEscapeInst));
  return ::new(Buffer) MarkFunctionEscapeInst(Loc, Elements);
}

MarkFunctionEscapeInst::MarkFunctionEscapeInst(SILLocation Loc,
                                               ArrayRef<SILValue> Elems)
  : SILInstruction(ValueKind::MarkFunctionEscapeInst, Loc),
    Operands(this, Elems) {
}

static SILType getPinResultType(SILType operandType) {
  return SILType::getPrimitiveObjectType(
    OptionalType::get(operandType.getSwiftRValueType())->getCanonicalType());
}

StrongPinInst::StrongPinInst(SILLocation loc, SILValue operand)
  : UnaryInstructionBase(loc, operand, getPinResultType(operand.getType())) {
}

StoreWeakInst::StoreWeakInst(SILLocation loc, SILValue value, SILValue dest,
                             IsInitialization_t isInit)
  : SILInstruction(ValueKind::StoreWeakInst, loc),
    Operands(this, value, dest), IsInitializationOfDest(isInit) {
}

CopyAddrInst::CopyAddrInst(SILLocation Loc, SILValue SrcLValue, SILValue DestLValue,
                           IsTake_t isTakeOfSrc,
                           IsInitialization_t isInitializationOfDest)
  : SILInstruction(ValueKind::CopyAddrInst, Loc),
    IsTakeOfSrc(isTakeOfSrc), IsInitializationOfDest(isInitializationOfDest),
    Operands(this, SrcLValue, DestLValue)
{
}

UnconditionalCheckedCastAddrInst::
UnconditionalCheckedCastAddrInst(SILLocation loc,
                                 CastConsumptionKind consumption,
                                 SILValue src, CanType srcType,
                                 SILValue dest, CanType targetType)
  : SILInstruction(ValueKind::UnconditionalCheckedCastAddrInst, loc),
    Operands(this, src, dest), ConsumptionKind(consumption),
    SourceType(srcType), TargetType(targetType) {
}

StructInst *StructInst::create(SILLocation Loc, SILType Ty,
                               ArrayRef<SILValue> Elements, SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(StructInst) +
                            decltype(Operands)::getExtraSize(Elements.size()),
                            alignof(StructInst));
  return ::new(Buffer) StructInst(Loc, Ty, Elements);
}

StructInst::StructInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elems)
  : SILInstruction(ValueKind::StructInst, Loc, Ty), Operands(this, Elems) {
  assert(!Ty.getStructOrBoundGenericStruct()->hasUnreferenceableStorage());
}

TupleInst *TupleInst::create(SILLocation Loc, SILType Ty,
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
                               ArrayRef<SILValue> Args, unsigned NumTrue,
                               unsigned NumFalse)
  : TermInst(ValueKind::CondBranchInst, Loc),
    DestBBs{{this, TrueBB}, {this, FalseBB}},
    NumTrueArgs(NumTrue), NumFalseArgs(NumFalse),
    Operands(this, Args, Condition)
{
  assert(Args.size() == (NumTrueArgs + NumFalseArgs) &&
         "Invalid number of args");
  assert(TrueBB != FalseBB && "Identical destinations");
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
  return ::new (Buffer) CondBranchInst(Loc, Condition, TrueBB, FalseBB, Args,
                                       TrueArgs.size(), FalseArgs.size());
}

OperandValueArrayRef CondBranchInst::getTrueArgs() const {
  return Operands.asValueArray().slice(1, NumTrueArgs);
}

OperandValueArrayRef CondBranchInst::getFalseArgs() const {
  return Operands.asValueArray().slice(1 + NumTrueArgs, NumFalseArgs);
}

SILValue
CondBranchInst::getArgForDestBB(SILBasicBlock *DestBB, SILArgument *A) {
  // If TrueBB and FalseBB equal, we can not find an arg for this DestBB so
  // return an empty SILValue.
  if (getTrueBB() == getFalseBB()) {
    assert(DestBB == getTrueBB() && "DestBB is not a target of this cond_br");
    return SILValue();
  }

  unsigned i = A->getIndex();

  if (DestBB == getTrueBB())
    return Operands[1 + i].get();

  assert(DestBB == getFalseBB()
         && "By process of elimination BB must be false BB");
  return Operands[1 + NumTrueArgs + i].get();
}

ArrayRef<Operand> CondBranchInst::getTrueOperands() const {
  return ArrayRef<Operand>(&Operands[1], NumTrueArgs);
}

MutableArrayRef<Operand> CondBranchInst::getTrueOperands() {
  return MutableArrayRef<Operand>(&Operands[1], NumTrueArgs);
}

ArrayRef<Operand> CondBranchInst::getFalseOperands() const {
  return ArrayRef<Operand>(&Operands[1+NumTrueArgs], NumFalseArgs);
}

MutableArrayRef<Operand> CondBranchInst::getFalseOperands() {
  return MutableArrayRef<Operand>(&Operands[1+NumTrueArgs], NumFalseArgs);
}

void CondBranchInst::swapSuccessors() {
  // Swap our destinations.
  SILBasicBlock *First = DestBBs[0].getBB();
  DestBBs[0] = DestBBs[1].getBB();
  DestBBs[1] = First;

  // If we don't have any arguments return.
  if (!NumTrueArgs && !NumFalseArgs)
    return;

  // Otherwise swap our true and false arguments.
  MutableArrayRef<Operand> Ops = getAllOperands();
  llvm::SmallVector<SILValue, 4> TrueOps;
  for (SILValue V : getTrueArgs())
    TrueOps.push_back(V);

  auto FalseArgs = getFalseArgs();
  for (unsigned i = 0, e = NumFalseArgs; i < e; ++i) {
    Ops[1+i].set(FalseArgs[i]);
  }

  for (unsigned i = 0, e = NumTrueArgs; i < e; ++i) {
    Ops[1+i+NumFalseArgs].set(TrueOps[i]);
  }

  // Finally swap the number of arguments that we have.
  std::swap(NumTrueArgs, NumFalseArgs);
}

SwitchValueInst::SwitchValueInst(SILLocation Loc, SILValue Operand,
                                 SILBasicBlock *DefaultBB,
                                 ArrayRef<SILValue> Cases,
                                 ArrayRef<SILBasicBlock*> BBs)
  : TermInst(ValueKind::SwitchValueInst, Loc),
    NumCases(Cases.size()),
    HasDefault(bool(DefaultBB)),
    Operands(this, Cases, Operand)
{

  // Initialize the successor array.
  auto *succs = getSuccessorBuf();
  unsigned OperandBitWidth = 0;

  if (auto OperandTy = Operand.getType().getAs<BuiltinIntegerType>()) {
    OperandBitWidth = OperandTy->getGreatestWidth();
  }

  for (unsigned i = 0, size = Cases.size(); i < size; ++i) {
    if (OperandBitWidth) {
      auto *IL = dyn_cast<IntegerLiteralInst>(Cases[i]);
      assert(IL && "switch_value case value should be of an integer type");
      assert(IL->getValue().getBitWidth() == OperandBitWidth &&
             "switch_value case value is not same bit width as operand");
      (void)IL;
    } else {
      auto *FR = dyn_cast<FunctionRefInst>(Cases[i]);
      if (!FR) {
        if (auto *CF = dyn_cast<ConvertFunctionInst>(Cases[i])) {
          FR = dyn_cast<FunctionRefInst>(CF->getOperand());
        }
      }
      assert(FR && "switch_value case value should be a function reference");
    }
    ::new (succs + i) SILSuccessor(this, BBs[i]);
  }

  if (HasDefault)
    ::new (succs + NumCases) SILSuccessor(this, DefaultBB);
}

SwitchValueInst::~SwitchValueInst() {
  // Destroy the successor records to keep the CFG up to date.
  auto *succs = getSuccessorBuf();
  for (unsigned i = 0, end = NumCases + HasDefault; i < end; ++i) {
    succs[i].~SILSuccessor();
  }
}

SwitchValueInst *SwitchValueInst::create(
    SILLocation Loc, SILValue Operand, SILBasicBlock *DefaultBB,
    ArrayRef<std::pair<SILValue, SILBasicBlock *>> CaseBBs, SILFunction &F) {
  // Allocate enough room for the instruction with tail-allocated data for all
  // the case values and the SILSuccessor arrays. There are `CaseBBs.size()`
  // SILValues and `CaseBBs.size() + (DefaultBB ? 1 : 0)` successors.
  SmallVector<SILValue, 8> Cases;
  SmallVector<SILBasicBlock *, 8> BBs;
  unsigned numCases = CaseBBs.size();
  unsigned numSuccessors = numCases + (DefaultBB ? 1 : 0);
  for(auto pair: CaseBBs) {
    Cases.push_back(pair.first);
    BBs.push_back(pair.second);
  }
  size_t bufSize = sizeof(SwitchValueInst) +
                   decltype(Operands)::getExtraSize(Cases.size()) +
                   sizeof(SILSuccessor) * numSuccessors;
  void *buf = F.getModule().allocate(bufSize, alignof(SwitchValueInst));
  return ::new (buf) SwitchValueInst(Loc, Operand, DefaultBB, Cases, BBs);
}

SelectValueInst::SelectValueInst(SILLocation Loc, SILValue Operand, SILType Type,
                                 SILValue DefaultResult,
                                 ArrayRef<SILValue> CaseValuesAndResults)
    : SelectInstBase(ValueKind::SelectValueInst,
                     Loc,
                     Type,
                     CaseValuesAndResults.size() / 2,
                     bool(DefaultResult),
                     CaseValuesAndResults, Operand) {

  unsigned OperandBitWidth = 0;

  if (auto OperandTy = Operand.getType().getAs<BuiltinIntegerType>()) {
    OperandBitWidth = OperandTy->getGreatestWidth();
  }

  for (unsigned i = 0; i < NumCases; ++i) {
    auto *IL = dyn_cast<IntegerLiteralInst>(CaseValuesAndResults[i * 2]);
    assert(IL && "select_value case value should be of an integer type");
    assert(IL->getValue().getBitWidth() == OperandBitWidth &&
           "select_value case value is not same bit width as operand");
    (void)IL;
  }
}

SelectValueInst::~SelectValueInst() {
}

SelectValueInst *
SelectValueInst::create(SILLocation Loc, SILValue Operand, SILType Type,
                        SILValue DefaultResult,
                        ArrayRef<std::pair<SILValue, SILValue>> CaseValues,
                        SILFunction &F) {
  // Allocate enough room for the instruction with tail-allocated data for all
  // the case values and the SILSuccessor arrays. There are `CaseBBs.size()`
  // SILValuues and `CaseBBs.size() + (DefaultBB ? 1 : 0)` successors.
  SmallVector<SILValue, 8> CaseValuesAndResults;
  for (auto pair : CaseValues) {
    CaseValuesAndResults.push_back(pair.first);
    CaseValuesAndResults.push_back(pair.second);
  }

  if ((bool)DefaultResult)
    CaseValuesAndResults.push_back(DefaultResult);

  size_t bufSize = sizeof(SelectValueInst) + decltype(Operands)::getExtraSize(
                                               CaseValuesAndResults.size());
  void *buf = F.getModule().allocate(bufSize, alignof(SelectValueInst));
  return ::new (buf)
      SelectValueInst(Loc, Operand, Type, DefaultResult, CaseValuesAndResults);
}

static SmallVector<SILValue, 4>
getCaseOperands(ArrayRef<std::pair<EnumElementDecl*, SILValue>> CaseValues,
                SILValue DefaultValue) {
  SmallVector<SILValue, 4> result;

  for (auto &pair : CaseValues)
    result.push_back(pair.second);
  if (DefaultValue)
    result.push_back(DefaultValue);

  return result;
}

SelectEnumInstBase::SelectEnumInstBase(
    ValueKind Kind, SILLocation Loc, SILValue Operand, SILType Ty,
    SILValue DefaultValue,
    ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues)
    : SelectInstBase(Kind, Loc, Ty, CaseValues.size(), bool(DefaultValue),
                     getCaseOperands(CaseValues, DefaultValue), Operand) {
  // Initialize the case and successor arrays.
  auto *cases = getCaseBuf();
  for (unsigned i = 0, size = CaseValues.size(); i < size; ++i) {
    cases[i] = CaseValues[i].first;
  }
}

template<typename SELECT_ENUM_INST>
SELECT_ENUM_INST *
SelectEnumInstBase::createSelectEnum(SILLocation Loc, SILValue Operand,
                 SILType Ty,
                 SILValue DefaultValue,
                 ArrayRef<std::pair<EnumElementDecl*, SILValue>> CaseValues,
                 SILFunction &F) {
  // Allocate enough room for the instruction with tail-allocated
  // EnumElementDecl and operand arrays. There are `CaseBBs.size()` decls
  // and `CaseBBs.size() + (DefaultBB ? 1 : 0)` values.
  unsigned numCases = CaseValues.size();

  void *buf = F.getModule().allocate(
    sizeof(SELECT_ENUM_INST) + sizeof(EnumElementDecl*) * numCases
     + TailAllocatedOperandList<1>::getExtraSize(numCases + (bool)DefaultValue),
    alignof(SELECT_ENUM_INST));
  return ::new (buf) SELECT_ENUM_INST(Loc,Operand,Ty,DefaultValue,CaseValues);
}

SelectEnumInst *
SelectEnumInst::create(SILLocation Loc, SILValue Operand, SILType Type,
                    SILValue DefaultValue,
                    ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues,
                    SILFunction &F) {
  return createSelectEnum<SelectEnumInst>(Loc, Operand, Type, DefaultValue,
                                          CaseValues, F);
}

SelectEnumAddrInst *
SelectEnumAddrInst::create(SILLocation Loc, SILValue Operand, SILType Type,
                    SILValue DefaultValue,
                    ArrayRef<std::pair<EnumElementDecl *, SILValue>> CaseValues,
                    SILFunction &F) {
  return createSelectEnum<SelectEnumAddrInst>(Loc, Operand, Type, DefaultValue,
                                              CaseValues, F);
}

SwitchEnumInstBase::SwitchEnumInstBase(
                ValueKind Kind,
                SILLocation Loc, SILValue Operand,
                SILBasicBlock *DefaultBB,
                ArrayRef<std::pair<EnumElementDecl*, SILBasicBlock*>> CaseBBs)
  : TermInst(Kind, Loc),
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

namespace {
  template <class Inst> EnumElementDecl *
  getUniqueCaseForDefaultValue(Inst *inst, SILValue enumValue) {
    assert(inst->hasDefault() && "doesn't have a default");
    SILType enumType = enumValue.getType();

    if (enumType.isResilient(inst->getModule()))
      return nullptr;

    EnumDecl *decl = enumType.getEnumOrBoundGenericEnum();
    assert(decl && "switch_enum operand is not an enum");

    llvm::SmallPtrSet<EnumElementDecl *, 4> unswitchedElts;
    for (auto elt : decl->getAllElements())
      unswitchedElts.insert(elt);

    for (unsigned i = 0, e = inst->getNumCases(); i != e; ++i) {
      auto Entry = inst->getCase(i);
      unswitchedElts.erase(Entry.first);
    }

    if (unswitchedElts.size() == 1)
      return *unswitchedElts.begin();

    return nullptr;
  }
}

NullablePtr<EnumElementDecl> SelectEnumInstBase::getUniqueCaseForDefault() {
  return getUniqueCaseForDefaultValue(this, getEnumOperand());
}

NullablePtr<EnumElementDecl> SelectEnumInstBase::getSingleTrueElement() const {
  auto SEIType = getType().getAs<BuiltinIntegerType>();
  if (!SEIType)
    return nullptr;
  if (SEIType->getWidth() != BuiltinIntegerWidth::fixed(1))
    return nullptr;

  // Try to find a single literal "true" case.
  Optional<EnumElementDecl*> TrueElement;
  for (unsigned i = 0, e = getNumCases(); i < e; ++i) {
    auto casePair = getCase(i);
    if (auto intLit = dyn_cast<IntegerLiteralInst>(casePair.second)) {
      if (intLit->getValue() == APInt(1, 1)) {
        if (!TrueElement)
          TrueElement = casePair.first;
        else
          // Use Optional(nullptr) to represent more than one.
          TrueElement = Optional<EnumElementDecl*>(nullptr);
      }
    }
  }

  if (!TrueElement || !*TrueElement)
    return nullptr;
  return *TrueElement;
}

SwitchEnumInstBase::~SwitchEnumInstBase() {
  // Destroy the successor records to keep the CFG up to date.
  auto *succs = getSuccessorBuf();
  for (unsigned i = 0, end = NumCases + HasDefault; i < end; ++i) {
    succs[i].~SILSuccessor();
  }
}

template<typename SWITCH_ENUM_INST>
SWITCH_ENUM_INST *
SwitchEnumInstBase::createSwitchEnum(SILLocation Loc, SILValue Operand,
                 SILBasicBlock *DefaultBB,
                 ArrayRef<std::pair<EnumElementDecl*, SILBasicBlock*>> CaseBBs,
                 SILFunction &F) {
  // Allocate enough room for the instruction with tail-allocated
  // EnumElementDecl and SILSuccessor arrays. There are `CaseBBs.size()` decls
  // and `CaseBBs.size() + (DefaultBB ? 1 : 0)` successors.
  unsigned numCases = CaseBBs.size();
  unsigned numSuccessors = numCases + (DefaultBB ? 1 : 0);

  void *buf = F.getModule().allocate(sizeof(SWITCH_ENUM_INST)
                                       + sizeof(EnumElementDecl*) * numCases
                                       + sizeof(SILSuccessor) * numSuccessors,
                                     alignof(SWITCH_ENUM_INST));
  return ::new (buf) SWITCH_ENUM_INST(Loc, Operand, DefaultBB, CaseBBs);
}

NullablePtr<EnumElementDecl> SwitchEnumInstBase::getUniqueCaseForDefault() {
  return getUniqueCaseForDefaultValue(this, getOperand());
}

NullablePtr<EnumElementDecl>
SwitchEnumInstBase::getUniqueCaseForDestination(SILBasicBlock *BB) {
  SILValue value = getOperand();
  SILType enumType = value.getType();
  EnumDecl *decl = enumType.getEnumOrBoundGenericEnum();
  assert(decl && "switch_enum operand is not an enum");
  (void)decl;

  EnumElementDecl *D = nullptr;
  for (unsigned i = 0, e = getNumCases(); i != e; ++i) {
    auto Entry = getCase(i);
    if (Entry.second == BB) {
      if (D != nullptr)
        return nullptr;
      D = Entry.first;
    }
  }
  if (!D && hasDefault() && getDefaultBB() == BB) {
    return getUniqueCaseForDefault();
  }
  return D;
}

SwitchEnumInst *SwitchEnumInst::create(SILLocation Loc, SILValue Operand,
                SILBasicBlock *DefaultBB,
                ArrayRef<std::pair<EnumElementDecl*, SILBasicBlock*>> CaseBBs,
                SILFunction &F) {
  return
    createSwitchEnum<SwitchEnumInst>(Loc, Operand, DefaultBB, CaseBBs, F);
}

SwitchEnumAddrInst *
SwitchEnumAddrInst::create(SILLocation Loc, SILValue Operand,
               SILBasicBlock *DefaultBB,
               ArrayRef<std::pair<EnumElementDecl*, SILBasicBlock*>> CaseBBs,
               SILFunction &F) {
  return createSwitchEnum<SwitchEnumAddrInst>
    (Loc, Operand, DefaultBB, CaseBBs, F);
}

DynamicMethodBranchInst::DynamicMethodBranchInst(SILLocation Loc,
                                                 SILValue Operand,
                                                 SILDeclRef Member,
                                                 SILBasicBlock *HasMethodBB,
                                                 SILBasicBlock *NoMethodBB)
  : TermInst(ValueKind::DynamicMethodBranchInst, Loc),
    Member(Member),
    DestBBs{{this, HasMethodBB}, {this, NoMethodBB}},
    Operands(this, Operand)
{
}

DynamicMethodBranchInst *DynamicMethodBranchInst::create(
                                                    SILLocation Loc,
                                                    SILValue Operand,
                                                    SILDeclRef Member,
                                                    SILBasicBlock *HasMethodBB,
                                                    SILBasicBlock *NoMethodBB,
                                                    SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(DynamicMethodBranchInst),
                                        alignof(DynamicMethodBranchInst));
  return ::new (Buffer) DynamicMethodBranchInst(Loc, Operand, Member,
                                                HasMethodBB, NoMethodBB);
}

SILLinkage
TypeConverter::getLinkageForProtocolConformance(const NormalProtocolConformance *C,
                                                ForDefinition_t definition) {
  // If the conformance is imported from Clang, give it shared linkage.
  auto typeDecl = C->getType()->getNominalOrBoundGenericNominal();
  auto typeUnit = typeDecl->getModuleScopeContext();
  if (isa<ClangModuleUnit>(typeUnit)
      && C->getDeclContext()->getParentModule() == typeUnit->getParentModule())
    return SILLinkage::Shared;

  // FIXME: This should be using std::min(protocol's access, type's access).
  switch (C->getProtocol()->getEffectiveAccess()) {
    case Accessibility::Private:
      return (definition ? SILLinkage::Private : SILLinkage::PrivateExternal);

    case Accessibility::Internal:
      return (definition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

    default:
      return (definition ? SILLinkage::Public : SILLinkage::PublicExternal);
  }
}

/// Create a witness method, creating a witness table declaration if we don't
/// have a witness table for it. Later on if someone wants the real definition,
/// lookUpWitnessTable will deserialize it for us if we can.
///
/// This is following the same model of how we deal with SILFunctions in
/// function_ref. There we always just create a declaration and then later
/// deserialize the actual function definition if we need to.
WitnessMethodInst *
WitnessMethodInst::create(SILLocation Loc, CanType LookupType,
                          ProtocolConformance *Conformance, SILDeclRef Member,
                          SILType Ty, SILFunction *F,
                          SILValue OpenedExistential, bool Volatile) {
  SILModule &Mod = F->getModule();
  void *Buffer =
      Mod.allocate(sizeof(WitnessMethodInst), alignof(WitnessMethodInst));

  declareWitnessTable(Mod, Conformance);
  return ::new (Buffer) WitnessMethodInst(Loc, LookupType, Conformance, Member,
                                          Ty, OpenedExistential, Volatile);
}

InitExistentialAddrInst *
InitExistentialAddrInst::create(SILLocation Loc, SILValue Existential,
                            CanType ConcreteType,
                            SILType ConcreteLoweredType,
                            ArrayRef<ProtocolConformance *> Conformances,
                            SILFunction *F) {
  SILModule &Mod = F->getModule();
  void *Buffer = Mod.allocate(sizeof(InitExistentialAddrInst),
                              alignof(InitExistentialAddrInst));
  for (ProtocolConformance *C : Conformances)
    declareWitnessTable(Mod, C);
  return ::new (Buffer) InitExistentialAddrInst(Loc, Existential,
                                            ConcreteType,
                                            ConcreteLoweredType,
                                            Conformances);
}

InitExistentialRefInst *
InitExistentialRefInst::create(SILLocation Loc, SILType ExistentialType,
                               CanType ConcreteType,
                               SILValue Instance,
                               ArrayRef<ProtocolConformance *> Conformances,
                               SILFunction *F) {
  SILModule &Mod = F->getModule();
  void *Buffer = Mod.allocate(sizeof(InitExistentialRefInst),
                              alignof(InitExistentialRefInst));
  for (ProtocolConformance *C : Conformances) {
    if (!C)
      continue;
    if (!Mod.lookUpWitnessTable(C, false).first)
      declareWitnessTable(Mod, C);
  }

  return ::new (Buffer) InitExistentialRefInst(Loc, ExistentialType,
                                               ConcreteType,
                                               Instance,
                                               Conformances);
}

InitExistentialMetatypeInst *
InitExistentialMetatypeInst::create(SILLocation loc,
                                    SILType existentialMetatypeType,
                                    SILValue metatype,
                               ArrayRef<ProtocolConformance *> conformances,
                                    SILFunction *F) {
  SILModule &M = F->getModule();
  void *buffer = M.allocate(sizeof(InitExistentialMetatypeInst),
                            alignof(InitExistentialMetatypeInst));
  for (ProtocolConformance *conformance : conformances)
    if (!M.lookUpWitnessTable(conformance, false).first)
      declareWitnessTable(M, conformance);

  return ::new (buffer) InitExistentialMetatypeInst(loc, existentialMetatypeType,
                                                    metatype, conformances);
}
