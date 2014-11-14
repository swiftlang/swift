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
      if (X->isTransparent() != RHS->isTransparent())
        return false;
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

  if (isa<OpenExistentialInst>(this) ||
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


// Allocations always return two results: Builtin.NativeObject & LValue[EltTy]
static SILTypeList *getAllocType(SILType EltTy, SILFunction &F) {
  const ASTContext &Ctx = F.getModule().getASTContext();

  SILType ResTys[] = {
    SILType::getNativeObjectType(Ctx),
    EltTy.getAddressType()
  };

  return F.getModule().getSILTypeList(ResTys);
}

AllocBoxInst::AllocBoxInst(SILLocation Loc, SILType ElementType, SILFunction &F)
  : AllocationInst(ValueKind::AllocBoxInst, Loc, getAllocType(ElementType, F)) {
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

OpenExistentialInst::OpenExistentialInst(SILLocation Loc,
                                         SILValue Operand,
                                         SILType SelfTy)
  : UnaryInstructionBase(Loc, Operand, SelfTy)
{}

OpenExistentialRefInst::OpenExistentialRefInst(SILLocation Loc,
                                               SILValue Operand,
                                               SILType Ty)
  : UnaryInstructionBase(Loc, Operand, Ty)
{}

OpenExistentialMetatypeInst::OpenExistentialMetatypeInst(SILLocation loc,
                                                         SILValue operand,
                                                         SILType ty)
  : UnaryInstructionBase(loc, operand, ty)
{}

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

    if (enumType.isResilient(inst->getParent()->getParent()->getModule()))
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

EnumElementDecl *SelectEnumInstBase::getUniqueCaseForDefault() {
  return getUniqueCaseForDefaultValue(this, getEnumOperand());
}

EnumElementDecl *
SelectEnumInstBase::getSingleTrueElement() const {
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

EnumElementDecl *SwitchEnumInstBase::getUniqueCaseForDefault() {
  return getUniqueCaseForDefaultValue(this, getOperand());
}

EnumElementDecl *
SwitchEnumInstBase::getUniqueCaseForDestination(SILBasicBlock *BB) {
  SILValue value = getOperand();
  SILType enumType = value.getType();
  EnumDecl *decl = enumType.getEnumOrBoundGenericEnum();
  assert(decl && "switch_enum operand is not an enum");

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

  switch (C->getProtocol()->getAccessibility()) {
    case Accessibility::Private:
      return (definition ? SILLinkage::Private : SILLinkage::PrivateExternal);

    case Accessibility::Internal:
      return (definition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

    default:
      return (definition ? SILLinkage::Public : SILLinkage::PublicExternal);
  }
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
                          SILType Ty, SILFunction *F, bool Volatile) {
  SILModule &Mod = F->getModule();
  void *Buffer = Mod.allocate(sizeof(WitnessMethodInst),
                              alignof(WitnessMethodInst));

  declareWitnessTable(Mod, Conformance);
  return ::new (Buffer) WitnessMethodInst(Loc, LookupType, Conformance, Member,
                                          Ty, Volatile);
}

InitExistentialInst *
InitExistentialInst::create(SILLocation Loc, SILValue Existential,
                            CanType ConcreteType,
                            SILType ConcreteLoweredType,
                            ArrayRef<ProtocolConformance *> Conformances,
                            SILFunction *F) {
  SILModule &Mod = F->getModule();
  void *Buffer = Mod.allocate(sizeof(InitExistentialInst),
                              alignof(InitExistentialInst));
  for (ProtocolConformance *C : Conformances)
    declareWitnessTable(Mod, C);
  return ::new (Buffer) InitExistentialInst(Loc, Existential,
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
  for (ProtocolConformance *C : Conformances)
    if (!Mod.lookUpWitnessTable(C, false).first)
      declareWitnessTable(Mod, C);

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
