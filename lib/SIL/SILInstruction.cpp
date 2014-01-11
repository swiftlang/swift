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
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
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
// reconstruct the BB from the 'self' pointer of the trait.
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

SILModule &SILInstruction::getModule() const {
  return getFunction()->getModule();
}

/// removeFromParent - This method unlinks 'self' from the containing basic
/// block, but does not delete it.
///
void SILInstruction::removeFromParent() {
  getParent()->getInstList().remove(this);
}

/// eraseFromParent - This method unlinks 'self' from the containing basic
/// block and deletes it.
///
void SILInstruction::eraseFromParent() {
  assert(use_empty() && "There are uses of instruction being deleted.");
  getParent()->getInstList().erase(this);
}

/// Unlink this instruction from its current basic block and insert it into
/// the basic block that MovePos lives in, right before MovePos.
void SILInstruction::moveBefore(SILInstruction *MovePos) {
  MovePos->getParent()->getInstList().splice(MovePos,
                                             getParent()->getInstList(), this);
}

void SILInstruction::dropAllReferences() {
  MutableArrayRef<Operand> PossiblyDeadOps = getAllOperands();
  for (auto OpI = PossiblyDeadOps.begin(),
            OpE = PossiblyDeadOps.end(); OpI != OpE; ++OpI) {
    OpI->drop();
  }
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
  /// Given a pair of instructions that are already known to have the same kind,
  /// type, and operands check any special state in the two instructions that
  /// could disrupt equality.
  class InstructionIdentityComparer :
    public SILInstructionVisitor<InstructionIdentityComparer, bool> {
  public:

    InstructionIdentityComparer(const SILInstruction *L) : LHS(L) { }

    /// Make sure we only process instructions we know how to process.
    bool visitValueBase(const ValueBase *RHS) {
      llvm_unreachable("Unimplemented comparison.");
    }

    bool visitFunctionRefInst(const FunctionRefInst *RHS) {
      auto *X = cast<FunctionRefInst>(LHS);
      return X->getReferencedFunction() == RHS->getReferencedFunction();
    }

    bool visitBuiltinFunctionRefInst(const BuiltinFunctionRefInst *RHS) {
      auto *X = cast<BuiltinFunctionRefInst>(LHS);
      return X->getName() == RHS->getName();
    }

    bool visitGlobalAddrInst(const GlobalAddrInst *RHS) {
      auto *X = cast<GlobalAddrInst>(LHS);
      return X->getGlobal() == RHS->getGlobal();
    }

    bool visitIntegerLiteralInst(const IntegerLiteralInst *RHS) {
      APInt X = cast<IntegerLiteralInst>(LHS)->getValue();
      APInt Y = RHS->getValue();
      return X.getBitWidth() == Y.getBitWidth() &&
        X == Y;
    }

    bool visitFloatLiteralInst(const FloatLiteralInst *RHS) {
      // Avoid floating point comparison issues by doing a bitwise comparison.
      APInt X = cast<FloatLiteralInst>(LHS)->getBits();
      APInt Y = RHS->getBits();
      return X.getBitWidth() == Y.getBitWidth() &&
        X == Y;
    }

    bool visitStringLiteralInst(const StringLiteralInst *RHS) {
      StringRef X = cast<StringLiteralInst>(LHS)->getValue();
      return X.equals(RHS->getValue());
    }

    bool visitStructInst(const StructInst *RHS) {
      // We have already checked the operands. Make sure that the StructDecls
      // match up.
      StructDecl *S1 = cast<StructInst>(LHS)->getStructDecl();
      return S1 == RHS->getStructDecl();
    }

    bool visitStructExtractInst(const StructExtractInst *RHS) {
      // We have already checked that the operands of our struct_extracts
      // match. Thus we need to check the field/struct decl which are not
      // operands.
      auto *X = cast<StructExtractInst>(LHS);
      if (X->getStructDecl() != RHS->getStructDecl())
        return false;
      if (X->getField() != RHS->getField())
        return false;
      return true;
    }

    bool visitStructElementAddrInst(const StructElementAddrInst *RHS) {
      // We have already checked that the operands of our struct_element_addrs
      // match. Thus we only need to check the field/struct decl which are not
      // operands.
      auto *X = cast<StructElementAddrInst>(LHS);
      if (X->getStructDecl() != RHS->getStructDecl())
        return false;
      if (X->getField() != RHS->getField())
        return false;
      return true;
    }

    bool visitTupleInst(const TupleInst *RHS) {
      // We have already checked the operands. Make sure that the tuple types
      // match up.
      TupleType *TT1 = cast<TupleInst>(LHS)->getTupleType();
      return TT1 == RHS->getTupleType();;
    }

    bool visitTupleExtractInst(const TupleExtractInst *RHS) {
      // We have already checked that the operands match. Thus we only need to
      // check the field no and tuple type which are not represented as operands.
      auto *X = cast<TupleExtractInst>(LHS);
      if (X->getTupleType() != RHS->getTupleType())
        return false;
      if (X->getFieldNo() != RHS->getFieldNo())
        return false;
      return true;
    }

    bool visitTupleElementAddrInst(const TupleElementAddrInst *RHS) {
      // We have already checked that the operands match. Thus we only need to
      // check the field no and tuple type which are not represented as operands.
      auto *X = cast<TupleElementAddrInst>(LHS);
      if (X->getTupleType() != RHS->getTupleType())
        return false;
      if (X->getFieldNo() != RHS->getFieldNo())
        return false;
      return true;
    }

    bool visitMetatypeInst(const MetatypeInst *RHS) {
      // We have already compared the operands/types, so we should have equality
      // at this point.
      return true;
    }

  private:
    const SILInstruction *LHS;
  };
}

bool SILInstruction::isIdenticalTo(const SILInstruction *RHS) const {
  // Quick check if both instructions have the same kind, number of operands,
  // and number of types. This should filter out most cases.
  if (getKind() != RHS->getKind() ||
      getNumOperands() != RHS->getNumOperands() ||
      getNumTypes() != RHS->getNumTypes()) {
    return false;
  }

  // Check types.
  //
  // Many instructions have only 1 type so it makes sense to check it first.
  for (unsigned i = 0, e = getNumTypes(); i != e; ++i)
    if (getType(i) != RHS->getType(i))
      return false;

  // Check operands.
  for (unsigned i = 0, e = getNumOperands(); i != e; ++i)
    if (getOperand(i) != RHS->getOperand(i))
      return false;

  // Check any special state of instructions that are not represented in the
  // instructions operands/type. We whitelist instructions that we handle so
  // that we can ensure that every instruction in this switch statement has been
  // audited and more importantly that as this method is used on more
  // instructions, it is updated appropriately.
  SILInstruction *UnconstRHS = const_cast<SILInstruction *>(RHS);
  return InstructionIdentityComparer(this).visit(UnconstRHS);
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

  class AllOperandsMutableAccessor
    : public SILVisitor<AllOperandsMutableAccessor,
                        MutableArrayRef<Operand> > {
  public:
#define VALUE(CLASS, PARENT) \
    MutableArrayRef<Operand> visit##CLASS(const CLASS *I) {             \
      llvm_unreachable("accessing non-instruction " #CLASS);            \
    }
#define INST(CLASS, PARENT, MEMBEHAVIOR) \
    MutableArrayRef<Operand> visit##CLASS(CLASS *I) {                   \
      ASSERT_IMPLEMENTS(CLASS, SILInstruction, getAllOperands,          \
                        MutableArrayRef<Operand>());                    \
      return I->getAllOperands();                                       \
    }
#include "swift/SIL/SILNodes.def"
  };
} // end anonymous namespace

ArrayRef<Operand> SILInstruction::getAllOperands() const {
  return AllOperandsAccessor().visit(const_cast<SILInstruction*>(this));
}

MutableArrayRef<Operand> SILInstruction::getAllOperands() {
  return AllOperandsMutableAccessor().visit(this);
}

/// getOperandNumber - Return which operand this is in the operand list of the
/// using instruction.
unsigned Operand::getOperandNumber() const {
  return this - &cast<SILInstruction>(getUser())->getAllOperands()[0];
}

SILInstruction::MemoryBehavior SILInstruction::getMemoryBehavior() const {
  switch (getKind()) {
#define INST(CLASS, PARENT, MEMBEHAVIOR) \
  case ValueKind::CLASS: return MemoryBehavior::MEMBEHAVIOR;
#include "swift/SIL/SILNodes.def"
  case ValueKind::SILArgument:
  case ValueKind::SILUndef:
    llvm_unreachable("Non-instructions are unreachable.");
  }
  llvm_unreachable("We've just exhausted the switch.");
}

bool SILInstruction::mayHaveSideEffects() const {
  MemoryBehavior B = getMemoryBehavior();
  return B == MemoryBehavior::MayWrite ||
    B == MemoryBehavior::MayReadWrite ||
    B == MemoryBehavior::MayHaveSideEffects;
}

namespace {
  class TrivialCloner : public SILCloner<TrivialCloner> {
    friend class SILCloner<TrivialCloner>;
    friend class SILVisitor<TrivialCloner>;
    SILInstruction *Result = nullptr;
    TrivialCloner(SILFunction *F) : SILCloner(*F) {}
  public:

    static SILInstruction *doIt(SILInstruction *I) {
      TrivialCloner TC(I->getFunction());
      TC.visit(I);
      return TC.Result;
    }

    void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
      Result = Cloned;
    }
    SILValue remapValue(SILValue Value) {
      return Value;
    }
  };
}

/// Create a new copy of this instruction, which retains all of the operands
/// and other information of this one.  If an insertion point is specified,
/// then the new instruction is inserted before the specified point, otherwise
/// the new instruction is returned without a parent.
SILInstruction *SILInstruction::clone(SILInstruction *InsertPt) {
  SILInstruction *NewInst = TrivialCloner::doIt(this);

  if (InsertPt)
    InsertPt->getParent()->getInstList().insert(InsertPt, NewInst);
  return NewInst;
}


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
  : SILInstruction(ValueKind::AllocStackInst, loc,
                   getAllocStackType(elementType, F)) {
}

/// getDecl - Return the underlying variable declaration associated with this
/// allocation, or null if this is a temporary allocation.
VarDecl *AllocStackInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}

AllocRefInst::AllocRefInst(SILLocation loc, SILType elementType, SILFunction &F,
                           bool objc)
  : SILInstruction(ValueKind::AllocRefInst, loc, elementType), ObjC(objc) {
}


// Allocations always return two results: Builtin.ObjectPointer & LValue[EltTy]
static SILTypeList *getAllocType(SILType EltTy, SILFunction &F) {
  const ASTContext &Ctx = F.getModule().getASTContext();

  SILType ResTys[] = {
    SILType::getObjectPointerType(Ctx),
    EltTy.getAddressType()
  };

  return F.getModule().getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(SILLocation Loc, SILType ElementType, SILFunction &F)
  : SILInstruction(ValueKind::AllocBoxInst, Loc, getAllocType(ElementType, F)) {
}

/// getDecl - Return the underlying variable declaration associated with this
/// allocation, or null if this is a temporary allocation.
VarDecl *AllocBoxInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}



AllocArrayInst::AllocArrayInst(SILLocation Loc, SILType ElementType,
                               SILValue NumElements, SILFunction &F)
  : SILInstruction(ValueKind::AllocArrayInst, Loc, getAllocType(ElementType, F)),
    Operands(this, NumElements) {
}

VarDecl *DebugValueInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}
VarDecl *DebugValueAddrInst::getDecl() const {
  return getLoc().getAsASTNode<VarDecl>();
}


ApplyInst::ApplyInst(SILLocation Loc, SILValue Callee,
                     SILType SubstCalleeTy,
                     SILType Result,
                     ArrayRef<Substitution> Subs,
                     ArrayRef<SILValue> Args,
                     bool Transparent)
  : SILInstruction(ValueKind::ApplyInst, Loc, Result),
    NumSubstitutions(Subs.size()), Transparent(Transparent),
    SubstCalleeType(SubstCalleeTy),
    Operands(this, Args, Callee)
{
  static_assert(IsTriviallyCopyable<Substitution>::value,
                "assuming Substitution is trivially copyable");
  memcpy(getSubstitutionsStorage(), Subs.begin(),
         sizeof(Substitution) * Subs.size());
}

ApplyInst *ApplyInst::create(SILLocation Loc, SILValue Callee,
                             SILType SubstCalleeTy,
                             SILType Result,
                             ArrayRef<Substitution> Subs,
                             ArrayRef<SILValue> Args,
                             bool Transparent, SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(ApplyInst)
                              + decltype(Operands)::getExtraSize(Args.size())
                              + sizeof(Substitution) * Subs.size(),
                            alignof(ApplyInst));
  return ::new(Buffer) ApplyInst(Loc, Callee, SubstCalleeTy,
                                 Result, Subs, Args, Transparent);
}

PartialApplyInst::PartialApplyInst(SILLocation Loc, SILValue Callee,
                                   SILType SubstCalleeTy,
                                   ArrayRef<Substitution> Subs,
                                   ArrayRef<SILValue> Args, SILType ClosureType)
// FIXME: the callee should have a lowered SIL function type, and PartialApplyInst
// should derive the type of its result by partially applying the callee's type.
  : SILInstruction(ValueKind::PartialApplyInst, Loc, ClosureType),
    SubstCalleeType(SubstCalleeTy),
    NumSubstitutions(Subs.size()),
    Operands(this, Args, Callee)
{
  static_assert(IsTriviallyCopyable<Substitution>::value,
                "assuming Substitution is trivial");
  memcpy(getSubstitutionsStorage(), Subs.begin(),
         sizeof(Substitution) * Subs.size());
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
  : SILInstruction(ValueKind::FunctionRefInst, Loc, F->getLoweredType()),
    Function(F) {

      F->RefCount++;
}

FunctionRefInst::~FunctionRefInst() {
  Function->RefCount--;
}

SILGlobalAddrInst::SILGlobalAddrInst(SILLocation Loc, SILGlobalVariable *Global)
  : SILInstruction(ValueKind::SILGlobalAddrInst, Loc,
                   Global->getLoweredType().getAddressType()),
    Global(Global)
{}

const IntrinsicInfo &BuiltinFunctionRefInst::getIntrinsicInfo() const {
  return getModule().getIntrinsicInfo(getName());
}

const BuiltinInfo &BuiltinFunctionRefInst::getBuiltinInfo() const {
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
  : SILInstruction(ValueKind::IntegerLiteralInst, Loc, Ty),
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
  : SILInstruction(ValueKind::FloatLiteralInst, Loc, Ty),
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
                                     Encoding encoding, SILTypeList *Ty)
  : SILInstruction(ValueKind::StringLiteralInst, Loc, Ty),
    Length(Text.size()), TheEncoding(encoding)
{
  memcpy(this + 1, Text.data(), Text.size());
}

StringLiteralInst *
StringLiteralInst::create(SILLocation loc, StringRef text, Encoding encoding,
                          SILFunction &F) {
  void *buf
    = allocateLiteralInstWithTextSize<StringLiteralInst>(F, text.size());

  const ASTContext &Ctx = F.getModule().getASTContext();
  SILType ResTys[] = {
    SILType::getRawPointerType(Ctx),
    SILType::getBuiltinWordType(Ctx),
    SILType::getBuiltinIntegerType(1, Ctx)
  };

  SILTypeList *Ty = F.getModule().getSILTypeList(ResTys);

  return ::new (buf) StringLiteralInst(loc, text, encoding, Ty);
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

StructInst *StructInst::create(SILLocation Loc, SILType Ty,
                               ArrayRef<SILValue> Elements, SILFunction &F) {
  void *Buffer = F.getModule().allocate(sizeof(StructInst) +
                            decltype(Operands)::getExtraSize(Elements.size()),
                            alignof(StructInst));
  return ::new(Buffer) StructInst(Loc, Ty, Elements);
}

StructInst::StructInst(SILLocation Loc, SILType Ty, ArrayRef<SILValue> Elems)
  : SILInstruction(ValueKind::StructInst, Loc, Ty), Operands(this, Elems) {
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

ProjectExistentialInst::ProjectExistentialInst(SILLocation Loc,
                                               SILValue Operand,
                                               SILType SelfTy)
  : UnaryInstructionBase(Loc, Operand, SelfTy)
{}

ProjectExistentialRefInst::ProjectExistentialRefInst(SILLocation Loc,
                                                     SILValue Operand,
                                                     SILType Ty)
  : UnaryInstructionBase(Loc, Operand, Ty)
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
                               ArrayRef<SILValue> Args, unsigned NumTrue,
                               unsigned NumFalse)
  : TermInst(ValueKind::CondBranchInst, Loc),
    DestBBs{{this, TrueBB}, {this, FalseBB}},
    NumTrueArgs(NumTrue), NumFalseArgs(NumFalse),
    Operands(this, Args, Condition)
{
  assert(Args.size() == (NumTrueArgs + NumFalseArgs) &&
         "Invalid number of args");
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

SwitchIntInst::SwitchIntInst(SILLocation Loc, SILValue Operand,
                             SILBasicBlock *DefaultBB,
                             ArrayRef<std::pair<APInt, SILBasicBlock*>> CaseBBs)
  : TermInst(ValueKind::SwitchIntInst, Loc),
    Operands(this, Operand),
    NumCases(CaseBBs.size()),
    HasDefault(bool(DefaultBB)),
    BitWidthForCase(Operand.getType().castTo<BuiltinIntegerType>()
                                                           ->getGreatestWidth())
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

  unsigned bits = Operand.getType().castTo<BuiltinIntegerType>()
    ->getGreatestWidth();
  unsigned words = (bits + llvm::integerPartWidth - 1) / llvm::integerPartWidth;

  void *buf = F.getModule().allocate(sizeof(SwitchIntInst)
                                      + sizeof(llvm::integerPart) * numCases
                                                                  * words
                                      + sizeof(SILSuccessor) * numSuccessors,
                                     alignof(SwitchIntInst));
  return ::new (buf) SwitchIntInst(Loc, Operand, DefaultBB, CaseBBs);
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
