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
// Instruction-specific properties on SILValue
//===----------------------------------------------------------------------===//

Optional<SILLocation> SILValue::getLoc() const {
  if (auto I = dyn_cast<SILInstruction>(*this)) {
    return I->getLoc();
  }
  return None;
}

//===----------------------------------------------------------------------===//
// ilist_traits<SILInstruction> Implementation
//===----------------------------------------------------------------------===//

// The trait object is embedded into a basic block.  Use dirty hacks to
// reconstruct the BB from the 'self' pointer of the trait.
SILBasicBlock *llvm::ilist_traits<SILInstruction>::getContainingBlock() {
  size_t Offset(
      size_t(&((SILBasicBlock *)0->*SILBasicBlock::getSublistAccess())));
  iplist<SILInstruction> *Anchor(static_cast<iplist<SILInstruction> *>(this));
  return reinterpret_cast<SILBasicBlock *>(reinterpret_cast<char *>(Anchor) -
                                           Offset);
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

  // If we have a function ref inst, we need to especially drop its function
  // argument so that it gets a proper ref decement.
  auto *FRI = dyn_cast<FunctionRefInst>(this);
  if (!FRI || !FRI->getReferencedFunction())
    return;

  FRI->dropReferencedFunction();
}

void SILInstruction::replaceAllUsesWithUndef() {
  SILModule &Mod = getModule();
  while (!use_empty()) {
    Operand *Op = *use_begin();
    Op->set(SILUndef::get(Op->get().getType(), Mod));
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
      return false;
    }

    bool visitInjectEnumAddrInst(const InjectEnumAddrInst *RHS) {
        auto *X = cast<InjectEnumAddrInst>(LHS);
        return X->getElement() == RHS->getElement();
    }

    bool visitDestroyAddrInst(const DestroyAddrInst *RHS) {
      return true;
    }

    bool visitReleaseValueInst(const ReleaseValueInst *RHS) {
      return true;
    }

    bool visitRetainValueInst(const RetainValueInst *RHS) {
      return true;
    }

    bool visitDeallocStackInst(const DeallocStackInst *RHS) {
      return true;
    }

    bool visitAllocStackInst(const AllocStackInst *RHS) {
      return false;
    }

    bool visitDeallocBoxInst(const DeallocBoxInst *RHS) {
      return true;
    }

    bool visitAllocBoxInst(const AllocBoxInst *RHS) {
      return false;
    }

    bool visitDeallocRefInst(const DeallocRefInst *RHS) {
      return true;
    }

    bool visitAllocRefInst(const AllocRefInst *RHS) {
      return false;
    }

    bool visitAllocRefDynamicInst(const AllocRefDynamicInst *RHS) {
      return false;
    }

    bool visitProjectValueBufferInst(const ProjectValueBufferInst *RHS) {
      auto *X = cast<ProjectValueBufferInst>(LHS);
      return X->getValueType() == RHS->getValueType();
    }

    bool visitStrongReleaseInst(const StrongReleaseInst *RHS) {
      return true;
    }

    bool visitStrongRetainInst(const StrongRetainInst *RHS) {
      return true;
    }

    bool visitLoadInst(const LoadInst *RHS) {
      return true;
    }

    bool visitStoreInst(const StoreInst *RHS) {
      auto *X = cast<StoreInst>(LHS);
      return (X->getSrc() == RHS->getSrc() && X->getDest() == RHS->getDest());
    }

    bool visitFunctionRefInst(const FunctionRefInst *RHS) {
      auto *X = cast<FunctionRefInst>(LHS);
      return X->getReferencedFunction() == RHS->getReferencedFunction();
    }

    bool visitGlobalAddrInst(const GlobalAddrInst *RHS) {
      auto *X = cast<GlobalAddrInst>(LHS);
      return X->getReferencedGlobal() == RHS->getReferencedGlobal();
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
      auto LHS_ = cast<StringLiteralInst>(LHS);
      return LHS_->getEncoding() == RHS->getEncoding()
        && LHS_->getValue().equals(RHS->getValue());
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

    bool visitRefElementAddrInst(RefElementAddrInst *RHS) {
      auto *X = cast<RefElementAddrInst>(LHS);
      if (X->getField() != RHS->getField())
        return false;
      if (X->getOperand() != RHS->getOperand())
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

    bool visitValueMetatypeInst(const ValueMetatypeInst *RHS) {
      // We have already compared the operands/types, so we should have equality
      // at this point.
      return true;
    }

    bool visitExistentialMetatypeInst(const ExistentialMetatypeInst *RHS) {
      // We have already compared the operands/types, so we should have equality
      // at this point.
      return true;
    }

    bool visitIndexRawPointerInst(IndexRawPointerInst *RHS) {
      // We have already compared the operands/types, so we should have equality
      // at this point.
      return true;
    }

    bool visitIndexAddrInst(IndexAddrInst *RHS) {
      // We have already compared the operands/types, so we should have equality
      // at this point.
      return true;
    }

    bool visitCondFailInst(CondFailInst *RHS) {
      // We have already compared the operands/types, so we should have equality
      // at this point.
      return true;
    }

    bool visitApplyInst(ApplyInst *RHS) {
      auto *X = cast<ApplyInst>(LHS);
      return X->getSubstitutions() == RHS->getSubstitutions();
    }

    bool visitBuiltinInst(BuiltinInst *RHS) {
      auto *X = cast<BuiltinInst>(LHS);
      if (X->getName() != RHS->getName())
        return false;
      return X->getSubstitutions() == RHS->getSubstitutions();
    }

    bool visitEnumInst(EnumInst *RHS) {
      // We already checked operands and types. Only thing we need to check is
      // that the element is the same.
      auto *X = cast<EnumInst>(LHS);
      return X->getElement() == RHS->getElement();
    }

    bool visitUncheckedEnumDataInst(UncheckedEnumDataInst *RHS) {
      // We already checked operands and types. Only thing we need to check is
      // that the element is the same.
      auto *X = cast<UncheckedEnumDataInst>(LHS);
      return X->getElement() == RHS->getElement();
    }

    bool visitSelectEnumInstBase(const SelectEnumInstBase *RHS) {
      // Check that the instructions match cases in the same order.
      auto *X = cast<SelectEnumInstBase>(LHS);

      if (X->getNumCases() != RHS->getNumCases())
        return false;
      if (X->hasDefault() != RHS->hasDefault())
        return false;

      for (unsigned i = 0, e = X->getNumCases(); i < e; ++i) {
        if (X->getCase(i).first != RHS->getCase(i).first)
          return false;
      }

      return true;
    }

    bool visitSelectEnumInst(const SelectEnumInst *RHS) {
      return visitSelectEnumInstBase(RHS);
    }
    bool visitSelectEnumAddrInst(const SelectEnumAddrInst *RHS) {
      return visitSelectEnumInstBase(RHS);
    }

    bool visitSelectValueInst(const SelectValueInst *RHS) {
      // Check that the instructions match cases in the same order.
      auto *X = cast<SelectValueInst>(LHS);

      if (X->getNumCases() != RHS->getNumCases())
        return false;
      if (X->hasDefault() != RHS->hasDefault())
        return false;

      for (unsigned i = 0, e = X->getNumCases(); i < e; ++i) {
        if (X->getCase(i).first != RHS->getCase(i).first)
          return false;
        if (X->getCase(i).second != RHS->getCase(i).second)
          return false;
      }

      return true;
    }

    // Conversion instructions.
    // All of these just return true as they have already had their
    // operands and types checked
    bool visitUncheckedRefCastInst(UncheckedRefCastInst *RHS) {
      return true;
    }

    bool visitUncheckedAddrCastInst(UncheckedAddrCastInst *RHS) {
      return true;
    }

    bool visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *RHS) {
      return true;
    }

    bool visitUncheckedRefBitCastInst(UncheckedRefBitCastInst *RHS) {
      return true;
    }

    bool visitUpcastInst(UpcastInst *RHS) {
      return true;
    }

    bool visitAddressToPointerInst(AddressToPointerInst *RHS) {
      return true;
    }

    bool visitPointerToAddressInst(PointerToAddressInst *RHS) {
      return true;
    }

    bool visitRefToRawPointerInst(RefToRawPointerInst *RHS) {
      return true;
    }

    bool visitRawPointerToRefInst(RawPointerToRefInst *RHS) {
      return true;
    }

    bool visitRefToUnownedInst(RefToUnownedInst *RHS) {
      return true;
    }

    bool visitUnownedToRefInst(UnownedToRefInst *RHS) {
      return true;
    }

    bool visitRefToUnmanagedInst(RefToUnmanagedInst *RHS) {
      return true;
    }

    bool visitUnmanagedToRefInst(UnmanagedToRefInst *RHS) {
      return true;
    }

    bool visitThinToThickFunctionInst(ThinToThickFunctionInst *RHS) {
      return true;
    }

    bool visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *RHS) {
      return true;
    }

    bool visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *RHS) {
      return true;
    }

    bool visitConvertFunctionInst(ConvertFunctionInst *RHS) {
      return true;
    }

    bool visitObjCMetatypeToObjectInst(ObjCMetatypeToObjectInst *RHS) {
      return true;
    }

    bool visitObjCExistentialMetatypeToObjectInst(ObjCExistentialMetatypeToObjectInst *RHS) {
      return true;
    }

    bool visitProjectBlockStorageInst(ProjectBlockStorageInst *RHS) {
      return true;
    }

    bool visitIsNonnullInst(IsNonnullInst *RHS) {
      return true;
    }

    bool visitBridgeObjectToRefInst(BridgeObjectToRefInst *X) {
      return true;
    }

    bool visitBridgeObjectToWordInst(BridgeObjectToWordInst *X) {
      return true;
    }
      
    bool visitRefToBridgeObjectInst(RefToBridgeObjectInst *X) {
      return true;
    }
    bool visitThinFunctionToPointerInst(ThinFunctionToPointerInst *X) {
      return true;
    }
    bool visitPointerToThinFunctionInst(PointerToThinFunctionInst *X) {
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

  if (auto *BI = dyn_cast<BuiltinInst>(this)) {
    // Handle Swift builtin functions.
    const BuiltinInfo &BInfo = BI->getBuiltinInfo();
    if (BInfo.ID != BuiltinValueKind::None)
      return BInfo.isReadNone() ? MemoryBehavior::None
                                : MemoryBehavior::MayHaveSideEffects;

    // Handle LLVM intrinsic functions.
    const IntrinsicInfo & IInfo = BI->getIntrinsicInfo();
    if (IInfo.ID != llvm::Intrinsic::not_intrinsic) {
      // Read-only.
      if (IInfo.hasAttribute(llvm::Attribute::ReadOnly) &&
          IInfo.hasAttribute(llvm::Attribute::NoUnwind))
        return MemoryBehavior::MayRead;
      // Read-none?
      return IInfo.hasAttribute(llvm::Attribute::ReadNone) &&
                     IInfo.hasAttribute(llvm::Attribute::NoUnwind)
                 ? MemoryBehavior::None
                 : MemoryBehavior::MayHaveSideEffects;
    }
  }

  // Handle functions that have an effects attribute.
  if (auto *AI = dyn_cast<ApplyInst>(this))
    if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
      if (auto *F = FRI->getReferencedFunction())
        return F->getEffectsKind() == EffectsKind::ReadNone
                   ? MemoryBehavior::None
                   : MemoryBehavior::MayHaveSideEffects;

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
  // If this instruction traps then it must have side effects.
  if (mayTrap())
    return true;

  MemoryBehavior B = getMemoryBehavior();
  return B == MemoryBehavior::MayWrite ||
    B == MemoryBehavior::MayReadWrite ||
    B == MemoryBehavior::MayHaveSideEffects;
}

namespace {
  class TrivialCloner : public SILClonerWithScopes<TrivialCloner> {
    friend class SILCloner<TrivialCloner>;
    friend class SILVisitor<TrivialCloner>;
    SILInstruction *Result = nullptr;
    TrivialCloner(SILFunction *F) : SILClonerWithScopes(*F) {}
  public:

    static SILInstruction *doIt(SILInstruction *I) {
      TrivialCloner TC(I->getFunction());
      TC.visit(I);
      return TC.Result;
    }

    void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
      Result = Cloned;
      SILClonerWithScopes<TrivialCloner>::postProcess(Orig, Cloned);
    }
    SILValue remapValue(SILValue Value) {
      return Value;
    }
    SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }
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

/// Returns true if the instruction can be duplicated without any special
/// additional handling. It is important to know this information when
/// you perform such optimizations like e.g. jump-threading.
bool SILInstruction::isTriviallyDuplicatable() const {
  if (isa<AllocStackInst>(this) || isa<DeallocStackInst>(this)) {
    return false;
  }

  if (isa<OpenExistentialAddrInst>(this) ||
      isa<OpenExistentialRefInst>(this) ||
      isa<OpenExistentialMetatypeInst>(this)) {
    // Don't know how to duplicate these properly yet. Inst.clone() per
    // instruction does not work. Because the follow-up instructions need to
    // reuse the same archetype uuid which would only work if we used a
    // cloner.
    return false;
  }

  if (auto *MI = dyn_cast<MethodInst>(this)) {
    // We can't build SSA for method values that lower to objc methods.
    if (MI->getMember().isForeign)
      return false;
  }

  return true;
}

bool SILInstruction::mayTrap() const {
  switch(getKind()) {
  case ValueKind::CondFailInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UnconditionalCheckedCastAddrInst:
    return true;
  default:
    return false;
  }
}
