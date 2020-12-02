//===--- OperandOwnership.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/ApplySite.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                      OwnershipConstraintClassifier
//===----------------------------------------------------------------------===//

namespace {

class OwnershipConstraintClassifier
    : public SILInstructionVisitor<OwnershipConstraintClassifier,
                                   OwnershipConstraint> {
  LLVM_ATTRIBUTE_UNUSED SILModule &mod;

  const Operand &op;

public:
  /// Create a new OwnershipConstraintClassifier.
  ///
  /// In most cases, one should only pass in \p Op and \p BaseValue will be set
  /// to Op.get(). In cases where one is trying to verify subobjects, Op.get()
  /// should be the subobject and Value should be the parent object. An example
  /// of where one would want to do this is in the case of value projections
  /// like struct_extract.
  OwnershipConstraintClassifier(SILModule &mod, const Operand &op)
      : mod(mod), op(op) {}

  SILValue getValue() const { return op.get(); }

  ValueOwnershipKind getOwnershipKind() const {
    assert(getValue().getOwnershipKind() == op.get().getOwnershipKind() &&
           "Expected ownership kind of parent value and operand");
    return getValue().getOwnershipKind();
  }

  unsigned getOperandIndex() const { return op.getOperandNumber(); }

  SILType getType() const { return op.get()->getType(); }

  bool compatibleWithOwnership(ValueOwnershipKind kind) const {
    return getOwnershipKind().isCompatibleWith(kind);
  }

  bool hasExactOwnership(ValueOwnershipKind kind) const {
    return getOwnershipKind() == kind;
  }

  bool isAddressOrTrivialType() const {
    if (getType().isAddress())
      return true;
    return getOwnershipKind() == OwnershipKind::None;
  }

  OwnershipConstraint visitApplyParameter(ValueOwnershipKind requiredConvention,
                                          UseLifetimeConstraint requirement);
  OwnershipConstraint visitFullApply(FullApplySite apply);

  OwnershipConstraint visitCallee(CanSILFunctionType substCalleeType);
  OwnershipConstraint
  checkTerminatorArgumentMatchesDestBB(SILBasicBlock *destBB, unsigned opIndex);

// Create declarations for all instructions, so we get a warning at compile
// time if any instructions do not have an implementation.
#define INST(Id, Parent) OwnershipConstraint visit##Id(Id *);
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

/// Implementation for instructions that we should never visit since they are
/// not valid in ossa or do not have operands. Since we should never visit
/// these, we just assert.
#define SHOULD_NEVER_VISIT_INST(INST)                                          \
  OwnershipConstraint OwnershipConstraintClassifier::visit##INST##Inst(        \
      INST##Inst *i) {                                                         \
    llvm::errs() << "Unhandled inst: " << *i;                                  \
    llvm::report_fatal_error(                                                  \
        "Visited instruction that should never be visited?!");                 \
  }
SHOULD_NEVER_VISIT_INST(AllocBox)
SHOULD_NEVER_VISIT_INST(AllocExistentialBox)
SHOULD_NEVER_VISIT_INST(AllocGlobal)
SHOULD_NEVER_VISIT_INST(AllocStack)
SHOULD_NEVER_VISIT_INST(DifferentiabilityWitnessFunction)
SHOULD_NEVER_VISIT_INST(FloatLiteral)
SHOULD_NEVER_VISIT_INST(FunctionRef)
SHOULD_NEVER_VISIT_INST(DynamicFunctionRef)
SHOULD_NEVER_VISIT_INST(PreviousDynamicFunctionRef)
SHOULD_NEVER_VISIT_INST(GlobalAddr)
SHOULD_NEVER_VISIT_INST(GlobalValue)
SHOULD_NEVER_VISIT_INST(BaseAddrForOffset)
SHOULD_NEVER_VISIT_INST(IntegerLiteral)
SHOULD_NEVER_VISIT_INST(Metatype)
SHOULD_NEVER_VISIT_INST(ObjCProtocol)
SHOULD_NEVER_VISIT_INST(RetainValue)
SHOULD_NEVER_VISIT_INST(RetainValueAddr)
SHOULD_NEVER_VISIT_INST(StringLiteral)
SHOULD_NEVER_VISIT_INST(StrongRetain)
SHOULD_NEVER_VISIT_INST(Unreachable)
SHOULD_NEVER_VISIT_INST(Unwind)
SHOULD_NEVER_VISIT_INST(ReleaseValue)
SHOULD_NEVER_VISIT_INST(ReleaseValueAddr)
SHOULD_NEVER_VISIT_INST(StrongRelease)
SHOULD_NEVER_VISIT_INST(GetAsyncContinuation)

#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  SHOULD_NEVER_VISIT_INST(StrongRetain##Name)                                  \
  SHOULD_NEVER_VISIT_INST(Name##Retain)
#include "swift/AST/ReferenceStorage.def"
#undef SHOULD_NEVER_VISIT_INST

/// Instructions that are interior pointers into a guaranteed value.
#define INTERIOR_POINTER_PROJECTION(INST)                                      \
  OwnershipConstraint OwnershipConstraintClassifier::visit##INST##Inst(        \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return {OwnershipKind::Guaranteed,                                         \
            UseLifetimeConstraint::NonLifetimeEnding};                         \
  }
INTERIOR_POINTER_PROJECTION(RefElementAddr)
INTERIOR_POINTER_PROJECTION(RefTailAddr)
#undef INTERIOR_POINTER_PROJECTION

/// Instructions whose arguments are always compatible with one convention.
#define CONSTANT_OWNERSHIP_INST(OWNERSHIP, USE_LIFETIME_CONSTRAINT, INST)      \
  OwnershipConstraint OwnershipConstraintClassifier::visit##INST##Inst(        \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return {OwnershipKind::OWNERSHIP,                                          \
            UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT};                   \
  }
CONSTANT_OWNERSHIP_INST(Guaranteed, NonLifetimeEnding, OpenExistentialValue)
CONSTANT_OWNERSHIP_INST(Guaranteed, NonLifetimeEnding, OpenExistentialBox)
CONSTANT_OWNERSHIP_INST(Guaranteed, NonLifetimeEnding, HopToExecutor)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, AutoreleaseValue)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, DeallocBox)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, DeallocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, DeallocRef)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, DestroyValue)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, EndLifetime)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, BeginCOWMutation)
CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, EndCOWMutation)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, AwaitAsyncContinuation)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, AbortApply)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, AddressToPointer)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, BeginAccess)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, BeginUnpairedAccess)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, BindMemory)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, CheckedCastAddrBranch)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, CondFail)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, CopyAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, DeallocStack)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, DebugValueAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, DeinitExistentialAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, DestroyAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, EndAccess)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, EndApply)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, EndUnpairedAccess)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, GetAsyncContinuationAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, IndexAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, IndexRawPointer)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, InitBlockStorageHeader)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, InitEnumDataAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, InitExistentialAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, InitExistentialMetatype)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, InjectEnumAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, IsUnique)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, Load)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, LoadBorrow)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, MarkFunctionEscape)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding,
                        ObjCExistentialMetatypeToObject)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, ObjCMetatypeToObject)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, ObjCToThickMetatype)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, OpenExistentialAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, OpenExistentialMetatype)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, PointerToAddress)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, PointerToThinFunction)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, ProjectBlockStorage)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, ProjectValueBuffer)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, RawPointerToRef)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, SelectEnumAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, SelectValue)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, StructElementAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, SwitchEnumAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, SwitchValue)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, TailAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, ThickToObjCMetatype)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, ThinFunctionToPointer)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, ThinToThickFunction)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, UncheckedRefCastAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, UnconditionalCheckedCastAddr)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, DeallocValueBuffer)

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, Load##Name)
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                         \
  CONSTANT_OWNERSHIP_INST(Owned, LifetimeEnding, Name##Release)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                      \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")                              \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  CONSTANT_OWNERSHIP_INST(None, NonLifetimeEnding, Name##ToRef)
#include "swift/AST/ReferenceStorage.def"
#undef CONSTANT_OWNERSHIP_INST

/// Instructions whose arguments are always compatible with one convention.
#define CONSTANT_OR_NONE_OWNERSHIP_INST(OWNERSHIP, USE_LIFETIME_CONSTRAINT,    \
                                        INST)                                  \
  OwnershipConstraint OwnershipConstraintClassifier::visit##INST##Inst(        \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return {OwnershipKind::OWNERSHIP,                                          \
            UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT};                   \
  }
CONSTANT_OR_NONE_OWNERSHIP_INST(Owned, LifetimeEnding, CheckedCastValueBranch)
CONSTANT_OR_NONE_OWNERSHIP_INST(Owned, LifetimeEnding,
                                UnconditionalCheckedCastValue)
CONSTANT_OR_NONE_OWNERSHIP_INST(Owned, LifetimeEnding, InitExistentialValue)
CONSTANT_OR_NONE_OWNERSHIP_INST(Owned, LifetimeEnding, DeinitExistentialValue)
#undef CONSTANT_OR_NONE_OWNERSHIP_INST

#define ACCEPTS_ANY_OWNERSHIP_INST(INST)                                       \
  OwnershipConstraint OwnershipConstraintClassifier::visit##INST##Inst(        \
      INST##Inst *i) {                                                         \
    return OwnershipConstraint::any();                                         \
  }
ACCEPTS_ANY_OWNERSHIP_INST(BeginBorrow)
ACCEPTS_ANY_OWNERSHIP_INST(CopyValue)
ACCEPTS_ANY_OWNERSHIP_INST(DebugValue)
ACCEPTS_ANY_OWNERSHIP_INST(FixLifetime)
ACCEPTS_ANY_OWNERSHIP_INST(UncheckedBitwiseCast) // Is this right?
ACCEPTS_ANY_OWNERSHIP_INST(WitnessMethod)        // Is this right?
ACCEPTS_ANY_OWNERSHIP_INST(ProjectBox)           // The result is a T*.
ACCEPTS_ANY_OWNERSHIP_INST(DynamicMethodBranch)
ACCEPTS_ANY_OWNERSHIP_INST(UncheckedTrivialBitCast)
ACCEPTS_ANY_OWNERSHIP_INST(ExistentialMetatype)
ACCEPTS_ANY_OWNERSHIP_INST(ValueMetatype)
ACCEPTS_ANY_OWNERSHIP_INST(UncheckedOwnershipConversion)
ACCEPTS_ANY_OWNERSHIP_INST(ValueToBridgeObject)
ACCEPTS_ANY_OWNERSHIP_INST(IsEscapingClosure)
ACCEPTS_ANY_OWNERSHIP_INST(ClassMethod)
ACCEPTS_ANY_OWNERSHIP_INST(ObjCMethod)
ACCEPTS_ANY_OWNERSHIP_INST(ObjCSuperMethod)
ACCEPTS_ANY_OWNERSHIP_INST(SuperMethod)
ACCEPTS_ANY_OWNERSHIP_INST(BridgeObjectToWord)
ACCEPTS_ANY_OWNERSHIP_INST(ClassifyBridgeObject)
ACCEPTS_ANY_OWNERSHIP_INST(CopyBlock)
ACCEPTS_ANY_OWNERSHIP_INST(RefToRawPointer)
ACCEPTS_ANY_OWNERSHIP_INST(SetDeallocating)
ACCEPTS_ANY_OWNERSHIP_INST(ProjectExistentialBox)
ACCEPTS_ANY_OWNERSHIP_INST(UnmanagedRetainValue)
ACCEPTS_ANY_OWNERSHIP_INST(UnmanagedReleaseValue)
ACCEPTS_ANY_OWNERSHIP_INST(UnmanagedAutoreleaseValue)
ACCEPTS_ANY_OWNERSHIP_INST(ConvertEscapeToNoEscape)
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  ACCEPTS_ANY_OWNERSHIP_INST(RefTo##Name)                                      \
  ACCEPTS_ANY_OWNERSHIP_INST(Name##ToRef)                                      \
  ACCEPTS_ANY_OWNERSHIP_INST(StrongCopy##Name##Value)
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  ACCEPTS_ANY_OWNERSHIP_INST(RefTo##Name)                                      \
  ACCEPTS_ANY_OWNERSHIP_INST(StrongCopy##Name##Value)
#include "swift/AST/ReferenceStorage.def"
#undef ACCEPTS_ANY_OWNERSHIP_INST

#define FORWARD_ANY_OWNERSHIP_INST(INST)                                       \
  OwnershipConstraint OwnershipConstraintClassifier::visit##INST##Inst(        \
      INST##Inst *i) {                                                         \
    assert(isa<OwnershipForwardingInst>(i));                                   \
    auto kind = i->getOwnershipKind();                                         \
    auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();          \
    return {kind, lifetimeConstraint};                                         \
  }
FORWARD_ANY_OWNERSHIP_INST(Tuple)
FORWARD_ANY_OWNERSHIP_INST(Struct)
FORWARD_ANY_OWNERSHIP_INST(Object)
FORWARD_ANY_OWNERSHIP_INST(Enum)
FORWARD_ANY_OWNERSHIP_INST(OpenExistentialRef)
FORWARD_ANY_OWNERSHIP_INST(Upcast)
FORWARD_ANY_OWNERSHIP_INST(UncheckedRefCast)
FORWARD_ANY_OWNERSHIP_INST(ConvertFunction)
FORWARD_ANY_OWNERSHIP_INST(RefToBridgeObject)
FORWARD_ANY_OWNERSHIP_INST(BridgeObjectToRef)
FORWARD_ANY_OWNERSHIP_INST(UnconditionalCheckedCast)
FORWARD_ANY_OWNERSHIP_INST(UncheckedEnumData)
FORWARD_ANY_OWNERSHIP_INST(InitExistentialRef)
FORWARD_ANY_OWNERSHIP_INST(DifferentiableFunction)
FORWARD_ANY_OWNERSHIP_INST(LinearFunction)
FORWARD_ANY_OWNERSHIP_INST(UncheckedValueCast)
FORWARD_ANY_OWNERSHIP_INST(DestructureStruct)
FORWARD_ANY_OWNERSHIP_INST(DestructureTuple)
#undef FORWARD_ANY_OWNERSHIP_INST

// An instruction that forwards a constant ownership or trivial ownership.
#define FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST(OWNERSHIP, INST)               \
  OwnershipConstraint OwnershipConstraintClassifier::visit##INST##Inst(        \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    assert(isa<OwnershipForwardingInst>(i));                                   \
    ValueOwnershipKind kind = OwnershipKind::OWNERSHIP;                        \
    return {kind, kind.getForwardingLifetimeConstraint()};                     \
  }
FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST(Guaranteed, TupleExtract)
FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST(Guaranteed, StructExtract)
FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST(Guaranteed,
                                        DifferentiableFunctionExtract)
FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST(Guaranteed, LinearFunctionExtract)
FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST(Guaranteed, OpenExistentialBoxValue)
FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST(Owned, MarkUninitialized)
#undef FORWARD_CONSTANT_OR_NONE_OWNERSHIP_INST

OwnershipConstraint OwnershipConstraintClassifier::visitDeallocPartialRefInst(
    DeallocPartialRefInst *i) {
  if (getValue() == i->getInstance()) {
    return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
  }

  return OwnershipConstraint::any();
}

OwnershipConstraint
OwnershipConstraintClassifier::visitSelectEnumInst(SelectEnumInst *i) {
  if (getValue() == i->getEnumOperand()) {
    return OwnershipConstraint::any();
  }

  auto kind = i->getOwnershipKind();
  auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();
  return {kind, lifetimeConstraint};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitAllocRefInst(AllocRefInst *i) {
  assert(i->getNumOperands() != 0 &&
         "If we reach this point, we must have a tail operand");
  return OwnershipConstraint::any();
}

OwnershipConstraint OwnershipConstraintClassifier::visitAllocRefDynamicInst(
    AllocRefDynamicInst *i) {
  assert(i->getNumOperands() != 0 &&
         "If we reach this point, we must have a tail operand");
  return OwnershipConstraint::any();
}

OwnershipConstraint
OwnershipConstraintClassifier::checkTerminatorArgumentMatchesDestBB(
    SILBasicBlock *destBB, unsigned opIndex) {
  // Grab the ownership kind of the destination block.
  ValueOwnershipKind destBlockArgOwnershipKind =
      destBB->getArgument(opIndex)->getOwnershipKind();
  auto lifetimeConstraint =
      destBlockArgOwnershipKind.getForwardingLifetimeConstraint();
  return {destBlockArgOwnershipKind, lifetimeConstraint};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitBranchInst(BranchInst *bi) {
  ValueOwnershipKind destBlockArgOwnershipKind =
      bi->getDestBB()->getArgument(getOperandIndex())->getOwnershipKind();

  // If we have a guaranteed parameter, treat this as consuming.
  if (destBlockArgOwnershipKind == OwnershipKind::Guaranteed) {
    return {destBlockArgOwnershipKind, UseLifetimeConstraint::LifetimeEnding};
  }

  // Otherwise, defer to defaults.
  auto lifetimeConstraint =
      destBlockArgOwnershipKind.getForwardingLifetimeConstraint();
  return {destBlockArgOwnershipKind, lifetimeConstraint};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitCondBranchInst(CondBranchInst *cbi) {
  // In ossa, cond_br insts are not allowed to take non-trivial values. Thus, we
  // just accept anything since we know all of our operands will be trivial.
  return OwnershipConstraint::any();
}

OwnershipConstraint
OwnershipConstraintClassifier::visitSwitchEnumInst(SwitchEnumInst *sei) {
  auto kind = getOwnershipKind();
  auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();
  return {kind, lifetimeConstraint};
}

OwnershipConstraint OwnershipConstraintClassifier::visitCheckedCastBranchInst(
    CheckedCastBranchInst *ccbi) {
  auto kind = getOwnershipKind();
  auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();
  return {kind, lifetimeConstraint};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitReturnInst(ReturnInst *ri) {
  auto kind = ri->getOwnershipKind();
  auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();
  return {kind, lifetimeConstraint};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitEndBorrowInst(EndBorrowInst *i) {
  /// An end_borrow is modeled as invalidating the guaranteed value preventing
  /// any further uses of the value.
  return {OwnershipKind::Guaranteed, UseLifetimeConstraint::LifetimeEnding};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitThrowInst(ThrowInst *i) {
  return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
}

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  OwnershipConstraint OwnershipConstraintClassifier::visitStore##Name##Inst(   \
      Store##Name##Inst *i) {                                                  \
    /* A store instruction implies that the value to be stored to be live, */  \
    /* but it does not touch the strong reference count of the value. We */    \
    /* also just care about liveness for the dest. So just match everything */ \
    /* as must be live. */                                                     \
    return OwnershipConstraint::any();                                         \
  }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                      \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#include "swift/AST/ReferenceStorage.def"

OwnershipConstraint
OwnershipConstraintClassifier::visitStoreBorrowInst(StoreBorrowInst *i) {
  if (getValue() == i->getSrc()) {
    return {OwnershipKind::Guaranteed,
            UseLifetimeConstraint::NonLifetimeEnding};
  }
  return OwnershipConstraint::any();
}

// FIXME: Why not use SILArgumentConvention here?
OwnershipConstraint
OwnershipConstraintClassifier::visitCallee(CanSILFunctionType substCalleeType) {
  ParameterConvention conv = substCalleeType->getCalleeConvention();
  switch (conv) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Constant:
    assert(!SILModuleConventions(mod).isSILIndirect(
                                      SILParameterInfo(substCalleeType, conv)));
    return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
  case ParameterConvention::Indirect_In_Guaranteed:
    assert(!SILModuleConventions(mod).isSILIndirect(
                                      SILParameterInfo(substCalleeType, conv)));
    return {OwnershipKind::Guaranteed,
            UseLifetimeConstraint::NonLifetimeEnding};
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("Illegal convention for callee");
  case ParameterConvention::Direct_Unowned:
    return OwnershipConstraint::any();
  case ParameterConvention::Direct_Owned:
    return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
  case ParameterConvention::Direct_Guaranteed:
    if (substCalleeType->isNoEscape())
      return OwnershipConstraint::any();
    return {OwnershipKind::Guaranteed,
            UseLifetimeConstraint::NonLifetimeEnding};
  }

  llvm_unreachable("Unhandled ParameterConvention in switch.");
}

// We allow for trivial cases of enums with non-trivial cases to be passed in
// non-trivial argument positions. This fits with modeling of a
// SILFunctionArgument as a phi in a global program graph.
OwnershipConstraint OwnershipConstraintClassifier::visitApplyParameter(
    ValueOwnershipKind kind, UseLifetimeConstraint requirement) {
  return {kind, requirement};
}

// Handle Apply and TryApply.
OwnershipConstraint
OwnershipConstraintClassifier::visitFullApply(FullApplySite apply) {
  // If we are visiting the callee operand, handle it specially.
  if (apply.isCalleeOperand(op)) {
    return visitCallee(apply.getSubstCalleeType());
  }

  // Indirect return arguments are address types.
  if (apply.isIndirectResultOperand(op)) {
    return OwnershipConstraint::any();
  }

  // We should have early exited if we saw a type dependent operand, so we
  // should never hit this.
  //
  // Lets just assert to be careful though.
  assert(!apply.getInstruction()->isTypeDependentOperand(op));

  unsigned argIndex = apply.getCalleeArgIndex(op);
  auto conv = apply.getSubstCalleeConv();
  SILParameterInfo paramInfo = conv.getParamInfoForSILArg(argIndex);

  switch (paramInfo.getConvention()) {
  case ParameterConvention::Direct_Owned:
    return visitApplyParameter(OwnershipKind::Owned,
                               UseLifetimeConstraint::LifetimeEnding);
  case ParameterConvention::Direct_Unowned:
    return OwnershipConstraint::any();

  case ParameterConvention::Indirect_In: {
    // This expects an @trivial if we have lowered addresses and @
    if (conv.useLoweredAddresses()) {
      return OwnershipConstraint::any();
    }
    // TODO: Once trivial is subsumed in any, this goes away.
    auto map = visitApplyParameter(OwnershipKind::Owned,
                                   UseLifetimeConstraint::LifetimeEnding);
    return map;
  }

  case ParameterConvention::Indirect_In_Guaranteed: {
    // This expects an @trivial if we have lowered addresses and @
    if (conv.useLoweredAddresses()) {
      return OwnershipConstraint::any();
    }
    return visitApplyParameter(OwnershipKind::Guaranteed,
                               UseLifetimeConstraint::NonLifetimeEnding);
  }

  // The following conventions should take address types and thus be
  // trivial.
  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    return OwnershipConstraint::any();

  case ParameterConvention::Direct_Guaranteed:
    // A +1 value may be passed to a guaranteed argument. From the caller's
    // point of view, this is just like a normal non-consuming use.
    // Direct_Guaranteed only accepts non-trivial types, but trivial types are
    // already handled above.
    return visitApplyParameter(OwnershipKind::Guaranteed,
                               UseLifetimeConstraint::NonLifetimeEnding);
  }
  llvm_unreachable("unhandled convension");
}

OwnershipConstraint
OwnershipConstraintClassifier::visitBeginApplyInst(BeginApplyInst *i) {
  return visitFullApply(i);
}

OwnershipConstraint
OwnershipConstraintClassifier::visitApplyInst(ApplyInst *i) {
  return visitFullApply(i);
}

OwnershipConstraint
OwnershipConstraintClassifier::visitTryApplyInst(TryApplyInst *i) {
  return visitFullApply(i);
}

OwnershipConstraint
OwnershipConstraintClassifier::visitPartialApplyInst(PartialApplyInst *i) {
  // partial_apply [stack] does not take ownership of its operands.
  if (i->isOnStack())
    return OwnershipConstraint::any();

  // All non-trivial types should be captured.
  return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
}

// TODO: FIX THIS
OwnershipConstraint
OwnershipConstraintClassifier::visitYieldInst(YieldInst *i) {
  // Indirect return arguments are address types.
  //
  // TODO: Change this to check if this operand is an indirect result
  if (isAddressOrTrivialType())
    return OwnershipConstraint::any();

  auto fnType = i->getFunction()->getLoweredFunctionType();
  auto yieldInfo = fnType->getYields()[getOperandIndex()];
  switch (yieldInfo.getConvention()) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Direct_Owned:
    return visitApplyParameter(OwnershipKind::Owned,
                               UseLifetimeConstraint::LifetimeEnding);
  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Direct_Unowned:
    // We accept unowned, owned, and guaranteed in unowned positions.
    return OwnershipConstraint::any();
  case ParameterConvention::Indirect_In_Guaranteed:
  case ParameterConvention::Direct_Guaranteed:
    return visitApplyParameter(OwnershipKind::Guaranteed,
                               UseLifetimeConstraint::NonLifetimeEnding);
  // The following conventions should take address types.
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("Unexpected non-trivial parameter convention.");
  }
  llvm_unreachable("unhandled convension");
}

OwnershipConstraint
OwnershipConstraintClassifier::visitAssignInst(AssignInst *i) {
  if (getValue() != i->getSrc()) {
    return OwnershipConstraint::any();
  }

  return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
}

OwnershipConstraint OwnershipConstraintClassifier::visitAssignByWrapperInst(
    AssignByWrapperInst *i) {
  if (getValue() != i->getSrc()) {
    return OwnershipConstraint::any();
  }

  return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitStoreInst(StoreInst *i) {
  if (getValue() != i->getSrc()) {
    return OwnershipConstraint::any();
  }

  return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
}

OwnershipConstraint
OwnershipConstraintClassifier::visitCopyBlockWithoutEscapingInst(
    CopyBlockWithoutEscapingInst *i) {
  // Consumes the closure parameter.
  if (getValue() == i->getClosure()) {
    return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
  }

  return OwnershipConstraint::any();
}

OwnershipConstraint OwnershipConstraintClassifier::visitMarkDependenceInst(
    MarkDependenceInst *mdi) {
  // If we are analyzing "the value", we forward ownership.
  if (getValue() == mdi->getValue()) {
    auto kind = mdi->getOwnershipKind();
    if (kind == OwnershipKind::None)
      return OwnershipConstraint::any();
    auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();
    return {kind, lifetimeConstraint};
  }

  // If we are not the "value" of the mark_dependence, then we must be the
  // "base". This means that any use that would destroy "value" can not be moved
  // before any uses of "base". We treat this as non-consuming and rely on the
  // rest of the optimizer to respect the movement restrictions.
  return OwnershipConstraint::any();
}

OwnershipConstraint
OwnershipConstraintClassifier::visitKeyPathInst(KeyPathInst *I) {
  // KeyPath moves the value in memory out of address operands, but the
  // ownership checker doesn't reason about that yet.
  return {OwnershipKind::Owned, UseLifetimeConstraint::LifetimeEnding};
}

//===----------------------------------------------------------------------===//
//                            Builtin Use Checker
//===----------------------------------------------------------------------===//

namespace {

struct OperandOwnershipKindBuiltinClassifier
    : SILBuiltinVisitor<OperandOwnershipKindBuiltinClassifier,
                        OwnershipConstraint> {
  using Map = OwnershipConstraint;

  OwnershipConstraint visitLLVMIntrinsic(BuiltinInst *bi,
                                         llvm::Intrinsic::ID id) {
    // LLVM intrinsics do not traffic in ownership, so if we have a result, it
    // must be trivial.
    return OwnershipConstraint::any();
  }

  // BUILTIN_TYPE_CHECKER_OPERATION does not live past the type checker.
#define BUILTIN_TYPE_CHECKER_OPERATION(ID, NAME)

#define BUILTIN(ID, NAME, ATTRS)                                               \
  OwnershipConstraint visit##ID(BuiltinInst *bi, StringRef attr);
#include "swift/AST/Builtins.def"

  OwnershipConstraint check(BuiltinInst *bi) { return visit(bi); }
};

} // end anonymous namespace

#define ANY_OWNERSHIP_BUILTIN(ID)                                              \
  OwnershipConstraint OperandOwnershipKindBuiltinClassifier::visit##ID(        \
      BuiltinInst *, StringRef) {                                              \
    return OwnershipConstraint::any();                                         \
  }
ANY_OWNERSHIP_BUILTIN(ErrorInMain)
ANY_OWNERSHIP_BUILTIN(UnexpectedError)
ANY_OWNERSHIP_BUILTIN(WillThrow)
ANY_OWNERSHIP_BUILTIN(AShr)
ANY_OWNERSHIP_BUILTIN(GenericAShr)
ANY_OWNERSHIP_BUILTIN(Add)
ANY_OWNERSHIP_BUILTIN(GenericAdd)
ANY_OWNERSHIP_BUILTIN(Alignof)
ANY_OWNERSHIP_BUILTIN(AllocRaw)
ANY_OWNERSHIP_BUILTIN(And)
ANY_OWNERSHIP_BUILTIN(GenericAnd)
ANY_OWNERSHIP_BUILTIN(AssertConf)
ANY_OWNERSHIP_BUILTIN(AssignCopyArrayNoAlias)
ANY_OWNERSHIP_BUILTIN(AssignCopyArrayFrontToBack)
ANY_OWNERSHIP_BUILTIN(AssignCopyArrayBackToFront)
ANY_OWNERSHIP_BUILTIN(AssignTakeArray)
ANY_OWNERSHIP_BUILTIN(AssumeNonNegative)
ANY_OWNERSHIP_BUILTIN(AssumeTrue)
ANY_OWNERSHIP_BUILTIN(AtomicLoad)
ANY_OWNERSHIP_BUILTIN(AtomicRMW)
ANY_OWNERSHIP_BUILTIN(AtomicStore)
ANY_OWNERSHIP_BUILTIN(BitCast)
ANY_OWNERSHIP_BUILTIN(CanBeObjCClass)
ANY_OWNERSHIP_BUILTIN(CondFailMessage)
ANY_OWNERSHIP_BUILTIN(CmpXChg)
ANY_OWNERSHIP_BUILTIN(CondUnreachable)
ANY_OWNERSHIP_BUILTIN(CopyArray)
ANY_OWNERSHIP_BUILTIN(DeallocRaw)
ANY_OWNERSHIP_BUILTIN(DestroyArray)
ANY_OWNERSHIP_BUILTIN(ExactSDiv)
ANY_OWNERSHIP_BUILTIN(GenericExactSDiv)
ANY_OWNERSHIP_BUILTIN(ExactUDiv)
ANY_OWNERSHIP_BUILTIN(GenericExactUDiv)
ANY_OWNERSHIP_BUILTIN(ExtractElement)
ANY_OWNERSHIP_BUILTIN(FAdd)
ANY_OWNERSHIP_BUILTIN(GenericFAdd)
ANY_OWNERSHIP_BUILTIN(FCMP_OEQ)
ANY_OWNERSHIP_BUILTIN(FCMP_OGE)
ANY_OWNERSHIP_BUILTIN(FCMP_OGT)
ANY_OWNERSHIP_BUILTIN(FCMP_OLE)
ANY_OWNERSHIP_BUILTIN(FCMP_OLT)
ANY_OWNERSHIP_BUILTIN(FCMP_ONE)
ANY_OWNERSHIP_BUILTIN(FCMP_ORD)
ANY_OWNERSHIP_BUILTIN(FCMP_UEQ)
ANY_OWNERSHIP_BUILTIN(FCMP_UGE)
ANY_OWNERSHIP_BUILTIN(FCMP_UGT)
ANY_OWNERSHIP_BUILTIN(FCMP_ULE)
ANY_OWNERSHIP_BUILTIN(FCMP_ULT)
ANY_OWNERSHIP_BUILTIN(FCMP_UNE)
ANY_OWNERSHIP_BUILTIN(FCMP_UNO)
ANY_OWNERSHIP_BUILTIN(FDiv)
ANY_OWNERSHIP_BUILTIN(GenericFDiv)
ANY_OWNERSHIP_BUILTIN(FMul)
ANY_OWNERSHIP_BUILTIN(GenericFMul)
ANY_OWNERSHIP_BUILTIN(FNeg)
ANY_OWNERSHIP_BUILTIN(FPExt)
ANY_OWNERSHIP_BUILTIN(FPToSI)
ANY_OWNERSHIP_BUILTIN(FPToUI)
ANY_OWNERSHIP_BUILTIN(FPTrunc)
ANY_OWNERSHIP_BUILTIN(FRem)
ANY_OWNERSHIP_BUILTIN(GenericFRem)
ANY_OWNERSHIP_BUILTIN(FSub)
ANY_OWNERSHIP_BUILTIN(GenericFSub)
ANY_OWNERSHIP_BUILTIN(Fence)
ANY_OWNERSHIP_BUILTIN(GetObjCTypeEncoding)
ANY_OWNERSHIP_BUILTIN(ICMP_EQ)
ANY_OWNERSHIP_BUILTIN(ICMP_NE)
ANY_OWNERSHIP_BUILTIN(ICMP_SGE)
ANY_OWNERSHIP_BUILTIN(ICMP_SGT)
ANY_OWNERSHIP_BUILTIN(ICMP_SLE)
ANY_OWNERSHIP_BUILTIN(ICMP_SLT)
ANY_OWNERSHIP_BUILTIN(ICMP_UGE)
ANY_OWNERSHIP_BUILTIN(ICMP_UGT)
ANY_OWNERSHIP_BUILTIN(ICMP_ULE)
ANY_OWNERSHIP_BUILTIN(ICMP_ULT)
ANY_OWNERSHIP_BUILTIN(InsertElement)
ANY_OWNERSHIP_BUILTIN(IntToFPWithOverflow)
ANY_OWNERSHIP_BUILTIN(IntToPtr)
ANY_OWNERSHIP_BUILTIN(IsOptionalType)
ANY_OWNERSHIP_BUILTIN(IsPOD)
ANY_OWNERSHIP_BUILTIN(IsConcrete)
ANY_OWNERSHIP_BUILTIN(IsBitwiseTakable)
ANY_OWNERSHIP_BUILTIN(IsSameMetatype)
ANY_OWNERSHIP_BUILTIN(LShr)
ANY_OWNERSHIP_BUILTIN(GenericLShr)
ANY_OWNERSHIP_BUILTIN(Mul)
ANY_OWNERSHIP_BUILTIN(GenericMul)
ANY_OWNERSHIP_BUILTIN(OnFastPath)
ANY_OWNERSHIP_BUILTIN(Once)
ANY_OWNERSHIP_BUILTIN(OnceWithContext)
ANY_OWNERSHIP_BUILTIN(Or)
ANY_OWNERSHIP_BUILTIN(GenericOr)
ANY_OWNERSHIP_BUILTIN(PtrToInt)
ANY_OWNERSHIP_BUILTIN(SAddOver)
ANY_OWNERSHIP_BUILTIN(SDiv)
ANY_OWNERSHIP_BUILTIN(GenericSDiv)
ANY_OWNERSHIP_BUILTIN(SExt)
ANY_OWNERSHIP_BUILTIN(SExtOrBitCast)
ANY_OWNERSHIP_BUILTIN(SIToFP)
ANY_OWNERSHIP_BUILTIN(SMulOver)
ANY_OWNERSHIP_BUILTIN(SRem)
ANY_OWNERSHIP_BUILTIN(GenericSRem)
ANY_OWNERSHIP_BUILTIN(SSubOver)
ANY_OWNERSHIP_BUILTIN(SToSCheckedTrunc)
ANY_OWNERSHIP_BUILTIN(SToUCheckedTrunc)
ANY_OWNERSHIP_BUILTIN(Expect)
ANY_OWNERSHIP_BUILTIN(Shl)
ANY_OWNERSHIP_BUILTIN(GenericShl)
ANY_OWNERSHIP_BUILTIN(Sizeof)
ANY_OWNERSHIP_BUILTIN(StaticReport)
ANY_OWNERSHIP_BUILTIN(Strideof)
ANY_OWNERSHIP_BUILTIN(StringObjectOr)
ANY_OWNERSHIP_BUILTIN(Sub)
ANY_OWNERSHIP_BUILTIN(GenericSub)
ANY_OWNERSHIP_BUILTIN(TakeArrayNoAlias)
ANY_OWNERSHIP_BUILTIN(TakeArrayBackToFront)
ANY_OWNERSHIP_BUILTIN(TakeArrayFrontToBack)
ANY_OWNERSHIP_BUILTIN(Trunc)
ANY_OWNERSHIP_BUILTIN(TruncOrBitCast)
ANY_OWNERSHIP_BUILTIN(TSanInoutAccess)
ANY_OWNERSHIP_BUILTIN(UAddOver)
ANY_OWNERSHIP_BUILTIN(UDiv)
ANY_OWNERSHIP_BUILTIN(GenericUDiv)
ANY_OWNERSHIP_BUILTIN(UIToFP)
ANY_OWNERSHIP_BUILTIN(UMulOver)
ANY_OWNERSHIP_BUILTIN(URem)
ANY_OWNERSHIP_BUILTIN(GenericURem)
ANY_OWNERSHIP_BUILTIN(USubOver)
ANY_OWNERSHIP_BUILTIN(UToSCheckedTrunc)
ANY_OWNERSHIP_BUILTIN(UToUCheckedTrunc)
ANY_OWNERSHIP_BUILTIN(Unreachable)
ANY_OWNERSHIP_BUILTIN(UnsafeGuaranteedEnd)
ANY_OWNERSHIP_BUILTIN(Xor)
ANY_OWNERSHIP_BUILTIN(GenericXor)
ANY_OWNERSHIP_BUILTIN(ZExt)
ANY_OWNERSHIP_BUILTIN(ZExtOrBitCast)
ANY_OWNERSHIP_BUILTIN(ZeroInitializer)
ANY_OWNERSHIP_BUILTIN(Swift3ImplicitObjCEntrypoint)
ANY_OWNERSHIP_BUILTIN(PoundAssert)
ANY_OWNERSHIP_BUILTIN(GlobalStringTablePointer)
ANY_OWNERSHIP_BUILTIN(TypePtrAuthDiscriminator)
ANY_OWNERSHIP_BUILTIN(IntInstrprofIncrement)
#undef ANY_OWNERSHIP_BUILTIN

// This is correct today since we do not have any builtins which return
// @guaranteed parameters. This means that we can only have a lifetime ending
// use with our builtins if it is owned.
#define CONSTANT_OWNERSHIP_BUILTIN(OWNERSHIP, USE_LIFETIME_CONSTRAINT, ID)     \
  OwnershipConstraint OperandOwnershipKindBuiltinClassifier::visit##ID(        \
      BuiltinInst *, StringRef) {                                              \
    return {OwnershipKind::OWNERSHIP,                                          \
            UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT};                   \
  }
CONSTANT_OWNERSHIP_BUILTIN(Owned, LifetimeEnding, COWBufferForReading)
CONSTANT_OWNERSHIP_BUILTIN(Owned, LifetimeEnding, UnsafeGuaranteed)
CONSTANT_OWNERSHIP_BUILTIN(Guaranteed, NonLifetimeEnding, CancelAsyncTask)
CONSTANT_OWNERSHIP_BUILTIN(Guaranteed, NonLifetimeEnding, CreateAsyncTask)
CONSTANT_OWNERSHIP_BUILTIN(Guaranteed, NonLifetimeEnding, CreateAsyncTaskFuture)
CONSTANT_OWNERSHIP_BUILTIN(None, NonLifetimeEnding, AutoDiffCreateLinearMapContext)
CONSTANT_OWNERSHIP_BUILTIN(Guaranteed, NonLifetimeEnding, AutoDiffAllocateSubcontext)
CONSTANT_OWNERSHIP_BUILTIN(Guaranteed, NonLifetimeEnding, AutoDiffProjectTopLevelSubcontext)

#undef CONSTANT_OWNERSHIP_BUILTIN

#define SHOULD_NEVER_VISIT_BUILTIN(ID)                                         \
  OwnershipConstraint OperandOwnershipKindBuiltinClassifier::visit##ID(        \
      BuiltinInst *, StringRef) {                                              \
    llvm_unreachable(                                                          \
        "Builtin should never be visited! E.x.: It may not have arguments");   \
  }
SHOULD_NEVER_VISIT_BUILTIN(GetCurrentAsyncTask)
#undef SHOULD_NEVER_VISIT_BUILTIN

// Builtins that should be lowered to SIL instructions so we should never see
// them.
#define BUILTIN_SIL_OPERATION(ID, NAME, CATEGORY)                              \
  OwnershipConstraint OperandOwnershipKindBuiltinClassifier::visit##ID(        \
      BuiltinInst *, StringRef) {                                              \
    llvm_unreachable("Builtin should have been lowered to SIL instruction?!"); \
  }
#define BUILTIN(X, Y, Z)
#include "swift/AST/Builtins.def"

OwnershipConstraint
OwnershipConstraintClassifier::visitBuiltinInst(BuiltinInst *bi) {
  return OperandOwnershipKindBuiltinClassifier().check(bi);
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

Optional<OwnershipConstraint> Operand::getOwnershipConstraint() const {
  if (isTypeDependent())
    return None;

  // If we do not have ownership enabled, just return any. This ensures that we
  // do not have any consuming uses and everything from an ownership perspective
  // is just a liveness use short-circuiting many of the optimizations.
  //
  // We do not ever call this function when an instruction isn't in a block.
  assert(getUser()->getParent() &&
         "Can not lookup ownership constraint unless inserted into block");
  if (auto *block = getUser()->getParent()) {
    auto *func = block->getParent();
    if (!func) {
      // If we don't have a function, then we must have a SILGlobalVariable. In
      // that case, we act as if we aren't in ownership.
      return {{OwnershipKind::Any, UseLifetimeConstraint::NonLifetimeEnding}};
    }

    if (!func->hasOwnership())
      return {{OwnershipKind::Any, UseLifetimeConstraint::NonLifetimeEnding}};
  }

  OwnershipConstraintClassifier classifier(getUser()->getModule(), *this);
  return classifier.visit(const_cast<SILInstruction *>(getUser()));
}
