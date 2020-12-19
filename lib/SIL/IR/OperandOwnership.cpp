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
//                         OperandOwnershipClassifier
//===----------------------------------------------------------------------===//

namespace {

class OperandOwnershipClassifier
  : public SILInstructionVisitor<OperandOwnershipClassifier, OperandOwnership> {
  LLVM_ATTRIBUTE_UNUSED SILModule &mod;

  const Operand &op;

public:
  /// Create a new OperandOwnershipClassifier.
  ///
  /// In most cases, one should only pass in \p Op and \p BaseValue will be set
  /// to Op.get(). In cases where one is trying to verify subobjects, Op.get()
  /// should be the subobject and Value should be the parent object. An example
  /// of where one would want to do this is in the case of value projections
  /// like struct_extract.
  OperandOwnershipClassifier(SILModule &mod, const Operand &op)
      : mod(mod), op(op) {}

  SILValue getValue() const { return op.get(); }

  ValueOwnershipKind getOwnershipKind() const {
    return op.get().getOwnershipKind();
  }

  unsigned getOperandIndex() const { return op.getOperandNumber(); }

  SILType getType() const { return op.get()->getType(); }

  bool compatibleWithOwnership(ValueOwnershipKind kind) const {
    return getOwnershipKind().isCompatibleWith(kind);
  }

  bool hasExactOwnership(ValueOwnershipKind kind) const {
    return getOwnershipKind() == kind;
  }

  OperandOwnership visitFullApply(FullApplySite apply);

// Create declarations for all instructions, so we get a warning at compile
// time if any instructions do not have an implementation.
#define INST(Id, Parent) OperandOwnership visit##Id(Id *);
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

/// Implementation for instructions that we should never visit since they are
/// not valid in ossa or do not have operands. Since we should never visit
/// these, we just assert.
#define SHOULD_NEVER_VISIT_INST(INST)                                          \
  OperandOwnership OperandOwnershipClassifier::visit##INST##Inst(              \
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

// FIXME: The assert should be moved to the verifier.
#define OPERAND_OWNERSHIP(OWNERSHIP, INST)                                     \
  OperandOwnership                                                             \
  OperandOwnershipClassifier::visit##INST##Inst(INST##Inst *i) {               \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return OperandOwnership::OWNERSHIP;                                        \
  }

// Instructions that require trivial operands.
OPERAND_OWNERSHIP(None, AwaitAsyncContinuation)
OPERAND_OWNERSHIP(None, AbortApply)
OPERAND_OWNERSHIP(None, AddressToPointer)
OPERAND_OWNERSHIP(None, AllocRef)        // with tail operand
OPERAND_OWNERSHIP(None, AllocRefDynamic) // with tail operand
OPERAND_OWNERSHIP(None, BeginAccess)
OPERAND_OWNERSHIP(None, BeginUnpairedAccess)
OPERAND_OWNERSHIP(None, BindMemory)
OPERAND_OWNERSHIP(None, CheckedCastAddrBranch)
OPERAND_OWNERSHIP(None, CondBranch)
OPERAND_OWNERSHIP(None, CondFail)
OPERAND_OWNERSHIP(None, CopyAddr)
OPERAND_OWNERSHIP(None, DeallocStack)
OPERAND_OWNERSHIP(None, DebugValueAddr)
OPERAND_OWNERSHIP(None, DeinitExistentialAddr)
OPERAND_OWNERSHIP(None, DestroyAddr)
OPERAND_OWNERSHIP(None, EndAccess)
OPERAND_OWNERSHIP(None, EndApply)
OPERAND_OWNERSHIP(None, EndUnpairedAccess)
OPERAND_OWNERSHIP(None, GetAsyncContinuationAddr)
OPERAND_OWNERSHIP(None, IndexAddr)
OPERAND_OWNERSHIP(None, IndexRawPointer)
OPERAND_OWNERSHIP(None, InitBlockStorageHeader)
OPERAND_OWNERSHIP(None, InitEnumDataAddr)
OPERAND_OWNERSHIP(None, InitExistentialAddr)
OPERAND_OWNERSHIP(None, InitExistentialMetatype)
OPERAND_OWNERSHIP(None, InjectEnumAddr)
OPERAND_OWNERSHIP(None, IsUnique)
OPERAND_OWNERSHIP(None, Load)
OPERAND_OWNERSHIP(None, LoadBorrow)
OPERAND_OWNERSHIP(None, MarkFunctionEscape)
OPERAND_OWNERSHIP(None, ObjCExistentialMetatypeToObject)
OPERAND_OWNERSHIP(None, ObjCMetatypeToObject)
OPERAND_OWNERSHIP(None, ObjCToThickMetatype)
OPERAND_OWNERSHIP(None, OpenExistentialAddr)
OPERAND_OWNERSHIP(None, OpenExistentialMetatype)
OPERAND_OWNERSHIP(None, PointerToAddress)
OPERAND_OWNERSHIP(None, PointerToThinFunction)
OPERAND_OWNERSHIP(None, ProjectBlockStorage)
OPERAND_OWNERSHIP(None, ProjectValueBuffer)
OPERAND_OWNERSHIP(None, RawPointerToRef)
OPERAND_OWNERSHIP(None, SelectEnumAddr)
OPERAND_OWNERSHIP(None, SelectValue)
OPERAND_OWNERSHIP(None, StructElementAddr)
OPERAND_OWNERSHIP(None, SwitchEnumAddr)
OPERAND_OWNERSHIP(None, SwitchValue)
OPERAND_OWNERSHIP(None, TailAddr)
OPERAND_OWNERSHIP(None, ThickToObjCMetatype)
OPERAND_OWNERSHIP(None, ThinFunctionToPointer)
OPERAND_OWNERSHIP(None, ThinToThickFunction)
OPERAND_OWNERSHIP(None, TupleElementAddr)
OPERAND_OWNERSHIP(None, UncheckedAddrCast)
OPERAND_OWNERSHIP(None, UncheckedRefCastAddr)
OPERAND_OWNERSHIP(None, UncheckedTakeEnumDataAddr)
OPERAND_OWNERSHIP(None, UnconditionalCheckedCastAddr)
OPERAND_OWNERSHIP(None, AllocValueBuffer)
OPERAND_OWNERSHIP(None, DeallocValueBuffer)

// Use an owned or guaranteed value only for the duration of the operation.
OPERAND_OWNERSHIP(InstantaneousUse, ExistentialMetatype)
OPERAND_OWNERSHIP(InstantaneousUse, FixLifetime)
OPERAND_OWNERSHIP(InstantaneousUse, WitnessMethod)
OPERAND_OWNERSHIP(InstantaneousUse, DynamicMethodBranch)
OPERAND_OWNERSHIP(InstantaneousUse, ValueMetatype)
OPERAND_OWNERSHIP(InstantaneousUse, IsEscapingClosure)
OPERAND_OWNERSHIP(InstantaneousUse, ClassMethod)
OPERAND_OWNERSHIP(InstantaneousUse, SuperMethod)
OPERAND_OWNERSHIP(InstantaneousUse, BridgeObjectToWord)
OPERAND_OWNERSHIP(InstantaneousUse, ClassifyBridgeObject)
OPERAND_OWNERSHIP(InstantaneousUse, SetDeallocating)
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  OPERAND_OWNERSHIP(InstantaneousUse, RefTo##Name)                             \
  OPERAND_OWNERSHIP(InstantaneousUse, Name##ToRef)                             \
  OPERAND_OWNERSHIP(InstantaneousUse, StrongCopy##Name##Value)
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  OPERAND_OWNERSHIP(InstantaneousUse, RefTo##Name)                             \
  OPERAND_OWNERSHIP(InstantaneousUse, StrongCopy##Name##Value)
#include "swift/AST/ReferenceStorage.def"

// Unowned uses ignore the value's ownership
OPERAND_OWNERSHIP(UnownedInstantaneousUse, DebugValue)
OPERAND_OWNERSHIP(UnownedInstantaneousUse, CopyBlock)
OPERAND_OWNERSHIP(UnownedInstantaneousUse, CopyValue)
OPERAND_OWNERSHIP(UnownedInstantaneousUse, ObjCMethod)
OPERAND_OWNERSHIP(UnownedInstantaneousUse, ObjCSuperMethod)
OPERAND_OWNERSHIP(UnownedInstantaneousUse, UnmanagedRetainValue)
OPERAND_OWNERSHIP(UnownedInstantaneousUse, UnmanagedReleaseValue)
OPERAND_OWNERSHIP(UnownedInstantaneousUse, UnmanagedAutoreleaseValue)

// Instructions that currently violate structural ownership requirements,
// and therefore completely defeat canonicalization and optimization of any
// OSSA value that they use.
OPERAND_OWNERSHIP(PointerEscape, ProjectBox) // The result is a T*.
OPERAND_OWNERSHIP(PointerEscape, ProjectExistentialBox)
OPERAND_OWNERSHIP(PointerEscape, UncheckedOwnershipConversion)

// Instructions that escape reference bits with unenforced lifetime.
OPERAND_OWNERSHIP(BitwiseEscape, UncheckedBitwiseCast)
OPERAND_OWNERSHIP(BitwiseEscape, ValueToBridgeObject)
OPERAND_OWNERSHIP(BitwiseEscape, RefToRawPointer)
OPERAND_OWNERSHIP(BitwiseEscape, UncheckedTrivialBitCast)
// FIXME: verify that no-escape results are always trivial
OPERAND_OWNERSHIP(BitwiseEscape, ConvertEscapeToNoEscape)

// Instructions that end the lifetime of an owned value.
OPERAND_OWNERSHIP(DestroyingConsume, AutoreleaseValue)
OPERAND_OWNERSHIP(DestroyingConsume, DeallocBox)
OPERAND_OWNERSHIP(DestroyingConsume, DeallocExistentialBox)
OPERAND_OWNERSHIP(DestroyingConsume, DeallocRef)
OPERAND_OWNERSHIP(DestroyingConsume, DestroyValue)
OPERAND_OWNERSHIP(DestroyingConsume, EndLifetime)
OPERAND_OWNERSHIP(DestroyingConsume, BeginCOWMutation)
OPERAND_OWNERSHIP(DestroyingConsume, EndCOWMutation)

// Instructions that move an owned value.
OPERAND_OWNERSHIP(ForwardingConsume, CheckedCastValueBranch)
OPERAND_OWNERSHIP(ForwardingConsume, UnconditionalCheckedCastValue)
OPERAND_OWNERSHIP(ForwardingConsume, InitExistentialValue)
OPERAND_OWNERSHIP(ForwardingConsume, DeinitExistentialValue)
OPERAND_OWNERSHIP(ForwardingConsume, MarkUninitialized)
OPERAND_OWNERSHIP(ForwardingConsume, Throw)

// Instructions that expose a pointer within a borrow scope.
OPERAND_OWNERSHIP(InteriorPointer, RefElementAddr)
OPERAND_OWNERSHIP(InteriorPointer, RefTailAddr)
OPERAND_OWNERSHIP(InteriorPointer, OpenExistentialBox)
// FIXME: HopToExecutorInst should be an instantaneous use.
OPERAND_OWNERSHIP(InteriorPointer, HopToExecutor)

// Instructions that propagate a value value within a borrow scope.
OPERAND_OWNERSHIP(ForwardingBorrow, TupleExtract)
OPERAND_OWNERSHIP(ForwardingBorrow, StructExtract)
OPERAND_OWNERSHIP(ForwardingBorrow, DifferentiableFunctionExtract)
OPERAND_OWNERSHIP(ForwardingBorrow, LinearFunctionExtract)
// FIXME: OpenExistential[Box]Value should be able to take owned values too by
// using getForwardingOperandOwnership.
OPERAND_OWNERSHIP(ForwardingBorrow, OpenExistentialValue)
OPERAND_OWNERSHIP(ForwardingBorrow, OpenExistentialBoxValue)

OPERAND_OWNERSHIP(EndBorrow, EndBorrow)

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  OPERAND_OWNERSHIP(None, Load##Name)
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                         \
  OPERAND_OWNERSHIP(DestroyingConsume, Name##Release)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                      \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")                              \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...) OPERAND_OWNERSHIP(None, Name##ToRef)
#include "swift/AST/ReferenceStorage.def"

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  OPERAND_OWNERSHIP(InstantaneousUse, Store##Name)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                      \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#include "swift/AST/ReferenceStorage.def"

#undef OPERAND_OWNERSHIP

// Forwarding operations are conditionally either ForwardingConsumes or
// ForwardingBorrows, depending on the instruction's constant ownership
// attribute.
#define FORWARDING_OWNERSHIP(INST)                                             \
  OperandOwnership OperandOwnershipClassifier::visit##INST##Inst(              \
      INST##Inst *i) {                                                         \
    return i->getOwnershipKind().getForwardingOperandOwnership(         \
      /*allowUnowned*/false);                                           \
  }
FORWARDING_OWNERSHIP(Object)
FORWARDING_OWNERSHIP(OpenExistentialRef)
FORWARDING_OWNERSHIP(ConvertFunction)
FORWARDING_OWNERSHIP(RefToBridgeObject)
FORWARDING_OWNERSHIP(BridgeObjectToRef)
FORWARDING_OWNERSHIP(UnconditionalCheckedCast)
FORWARDING_OWNERSHIP(InitExistentialRef)
FORWARDING_OWNERSHIP(DifferentiableFunction)
FORWARDING_OWNERSHIP(LinearFunction)
#undef FORWARDING_OWNERSHIP

// Arbitrary value casts are forwarding instructions that are also allowed to
// propagate Unowned values. If the result is Unowned, then the operand must
// also be Unowned.
#define FORWARDING_ANY_OWNERSHIP(INST)                                         \
  OperandOwnership OperandOwnershipClassifier::visit##INST##Inst(              \
      INST##Inst *i) {                                                         \
    return i->getOwnershipKind().getForwardingOperandOwnership(         \
      /*allowUnowned*/true);                                            \
  }
FORWARDING_ANY_OWNERSHIP(Upcast)
FORWARDING_ANY_OWNERSHIP(UncheckedRefCast)
FORWARDING_ANY_OWNERSHIP(UncheckedValueCast)
FORWARDING_ANY_OWNERSHIP(CheckedCastBranch)
#undef FORWARDING_ANY_OWNERSHIP

// Any valid ownership kind can be combined with values of None ownership, but
// they cannot be combined with each other. An aggregates result ownership is
// the meet of its operands' ownership. A destructured member has the same
// ownership as its aggregate unless its type gives it None ownership.
//
// TODO: Aggregate operations should be Reborrows, not ForwardingBorrows,
// because the borrowed value is different on either side of the operation and
// the lifetimes of borrowed members could differ.
#define AGGREGATE_OWNERSHIP(INST)                                              \
  OperandOwnership OperandOwnershipClassifier::visit##INST##Inst(              \
      INST##Inst *i) {                                                         \
    return i->getOwnershipKind().getForwardingOperandOwnership(         \
      /*allowUnowned*/true);                                            \
  }
AGGREGATE_OWNERSHIP(Tuple)
AGGREGATE_OWNERSHIP(Struct)
AGGREGATE_OWNERSHIP(DestructureStruct)
AGGREGATE_OWNERSHIP(DestructureTuple)
AGGREGATE_OWNERSHIP(Enum)
AGGREGATE_OWNERSHIP(UncheckedEnumData)
AGGREGATE_OWNERSHIP(SwitchEnum)
#undef AGGREGATE_OWNERSHIP

// A begin_borrow is conditionally nested.
OperandOwnership
OperandOwnershipClassifier::visitBeginBorrowInst(BeginBorrowInst *borrow) {
  switch (borrow->getOperand().getOwnershipKind()) {
  case OwnershipKind::Any:
    llvm_unreachable("invalid value ownership");
  case OwnershipKind::None:
    return OperandOwnership::None;
  case OwnershipKind::Unowned:
    // FIXME: disallow borrowing an Unowned value. Temporarily model it as an
    // instantaneous use until SILGenFunction::emitClassMemberDestruction is
    // fixed.
    return OperandOwnership::UnownedInstantaneousUse;
  case OwnershipKind::Guaranteed:
    return OperandOwnership::NestedBorrow;
  case OwnershipKind::Owned:
    return OperandOwnership::Borrow;
  }
}

// MARK: Instructions whose use ownership depends on the operand in question.

OperandOwnership OperandOwnershipClassifier::
visitDeallocPartialRefInst(DeallocPartialRefInst *i) {
  if (getValue() == i->getInstance()) {
    return OperandOwnership::DestroyingConsume;
  }
  return OperandOwnership::None;
}

OperandOwnership
OperandOwnershipClassifier::visitSelectEnumInst(SelectEnumInst *i) {
  if (getValue() == i->getEnumOperand()) {
    return OperandOwnership::InstantaneousUse;
  }
  return getOwnershipKind().getForwardingOperandOwnership(
    /*allowUnowned*/true);
}

OperandOwnership OperandOwnershipClassifier::visitBranchInst(BranchInst *bi) {
  ValueOwnershipKind destBlockArgOwnershipKind =
      bi->getDestBB()->getArgument(getOperandIndex())->getOwnershipKind();

  // FIXME: remove this special case once all aggregate operations behave just
  // like phis.
  if (destBlockArgOwnershipKind == OwnershipKind::Guaranteed) {
    return OperandOwnership::Reborrow;
  }
  return destBlockArgOwnershipKind.getForwardingOperandOwnership(
    /*allowUnowned*/true);
}

OperandOwnership
OperandOwnershipClassifier::visitStoreBorrowInst(StoreBorrowInst *i) {
  if (getValue() == i->getSrc()) {
    return OperandOwnership::ForwardingBorrow;
  }
  return OperandOwnership::None;
}

static OperandOwnership getFunctionArgOwnership(SILArgumentConvention argConv) {
  switch (argConv) {
  case SILArgumentConvention::Indirect_In:
  case SILArgumentConvention::Direct_Owned:
    return OperandOwnership::ForwardingConsume;

  // A guaranteed argument is forwarded into the callee. However, from the
  // caller's point of view it looks like an instantaneous use. Consequently,
  // owned values may be passed to guaranteed arguments without an explicit
  // borrow scope in the caller.
  case SILArgumentConvention::Indirect_In_Constant:
  case SILArgumentConvention::Indirect_In_Guaranteed:
  case SILArgumentConvention::Direct_Guaranteed:
    return OperandOwnership::InstantaneousUse;

  case SILArgumentConvention::Direct_Unowned:
    return OperandOwnership::UnownedInstantaneousUse;

  case SILArgumentConvention::Indirect_Out:
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
    llvm_unreachable("Illegal convention for non-address types");
  }
}

OperandOwnership
OperandOwnershipClassifier::visitFullApply(FullApplySite apply) {
  // Before considering conventions, filter all (trivial) indirect
  // arguments. This also rules out result arguments.
  if (getValue()->getType().isAddress()) {
    return OperandOwnership::None;
  }
  SILArgumentConvention argConv = apply.isCalleeOperand(op)
    ? SILArgumentConvention(apply.getSubstCalleeType()->getCalleeConvention())
    : apply.getArgumentConvention(op);

  auto operandOwnership = getFunctionArgOwnership(argConv);

  // Allow passing callee operands with no ownership as guaranteed.
  // FIXME: why do we allow this?
  if (operandOwnership == OperandOwnership::ForwardingBorrow
      && apply.isCalleeOperand(op)) {
    return OperandOwnership::InstantaneousUse;
  }
  return operandOwnership;
}

OperandOwnership
OperandOwnershipClassifier::visitBeginApplyInst(BeginApplyInst *i) {
  return visitFullApply(i);
}

OperandOwnership OperandOwnershipClassifier::visitApplyInst(ApplyInst *i) {
  return visitFullApply(i);
}

OperandOwnership
OperandOwnershipClassifier::visitTryApplyInst(TryApplyInst *i) {
  return visitFullApply(i);
}

OperandOwnership
OperandOwnershipClassifier::visitPartialApplyInst(PartialApplyInst *i) {
  // partial_apply [stack] does not take ownership of its operands.
  if (i->isOnStack()) {
    return OperandOwnership::InstantaneousUse;
  }
  // All non-trivial types should be captured.
  return OperandOwnership::ForwardingConsume;
}

OperandOwnership OperandOwnershipClassifier::visitYieldInst(YieldInst *i) {
  // Before considering conventions, filter all indirect arguments.
  if (getValue()->getType().isAddress()) {
    return OperandOwnership::None;
  }
  auto fnType = i->getFunction()->getLoweredFunctionType();
  SILArgumentConvention argConv(
    fnType->getYields()[getOperandIndex()].getConvention());
  return getFunctionArgOwnership(argConv);
}

OperandOwnership OperandOwnershipClassifier::visitReturnInst(ReturnInst *i) {
  switch (i->getOwnershipKind()) {
  case OwnershipKind::Any:
  case OwnershipKind::Guaranteed:
    llvm_unreachable("invalid value ownership");
  case OwnershipKind::None:
    return OperandOwnership::None;
  case OwnershipKind::Unowned:
    return OperandOwnership::UnownedInstantaneousUse;
  case OwnershipKind::Owned:
    return OperandOwnership::ForwardingConsume;
  }
}

OperandOwnership OperandOwnershipClassifier::visitAssignInst(AssignInst *i) {
  if (getValue() != i->getSrc()) {
    return OperandOwnership::None;
  }
  return OperandOwnership::DestroyingConsume;
}

OperandOwnership
OperandOwnershipClassifier::visitAssignByWrapperInst(AssignByWrapperInst *i) {
  if (getValue() == i->getSrc()) {
    return OperandOwnership::DestroyingConsume;
  }
  if (getValue() == i->getDest()) {
    return OperandOwnership::None;
  }
  return OperandOwnership::InstantaneousUse; // initializer/setter closure
}

OperandOwnership OperandOwnershipClassifier::visitStoreInst(StoreInst *i) {
  if (getValue() != i->getSrc()) {
    return OperandOwnership::None;
  }
  return OperandOwnership::DestroyingConsume;
}

OperandOwnership OperandOwnershipClassifier::visitCopyBlockWithoutEscapingInst(
    CopyBlockWithoutEscapingInst *i) {
  // Consumes the closure parameter.
  if (getValue() == i->getClosure()) {
    return OperandOwnership::ForwardingConsume;
  }
  return OperandOwnership::UnownedInstantaneousUse;
}

OperandOwnership
OperandOwnershipClassifier::visitMarkDependenceInst(MarkDependenceInst *mdi) {
  // If we are analyzing "the value", we forward ownership.
  if (getValue() == mdi->getValue()) {
    return getOwnershipKind().getForwardingOperandOwnership(
      /*allowUnowned*/true);
  }
  // FIXME: Add an end_dependence instruction so we can treat mark_dependence as
  // a borrow of the base (mark_dependence %base -> end_dependence is analogous
  // to a borrow scope).
  return OperandOwnership::PointerEscape;
}

OperandOwnership OperandOwnershipClassifier::visitKeyPathInst(KeyPathInst *I) {
  // KeyPath moves the value in memory out of address operands, but the
  // ownership checker doesn't reason about that yet.
  return OperandOwnership::ForwardingConsume;
}

//===----------------------------------------------------------------------===//
//                            Builtin Use Checker
//===----------------------------------------------------------------------===//

namespace {

struct OperandOwnershipBuiltinClassifier
    : SILBuiltinVisitor<OperandOwnershipBuiltinClassifier, OperandOwnership> {
  using Map = OperandOwnership;

  OperandOwnership visitLLVMIntrinsic(BuiltinInst *bi, llvm::Intrinsic::ID id) {
    // LLVM intrinsics do not traffic in ownership, so if we have a result, it
    // must be trivial.
    return OperandOwnership::None;
  }

  // BUILTIN_TYPE_CHECKER_OPERATION does not live past the type checker.
#define BUILTIN_TYPE_CHECKER_OPERATION(ID, NAME)

#define BUILTIN(ID, NAME, ATTRS)                                               \
  OperandOwnership visit##ID(BuiltinInst *bi, StringRef attr);
#include "swift/AST/Builtins.def"

  OperandOwnership check(BuiltinInst *bi) { return visit(bi); }
};

} // end anonymous namespace

#define BUILTIN_OPERAND_OWNERSHIP(OWNERSHIP, ID)                               \
  OperandOwnership                                                             \
  OperandOwnershipBuiltinClassifier::visit##ID(BuiltinInst *, StringRef) {     \
    return OperandOwnership::OWNERSHIP;                                        \
  }
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ErrorInMain)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UnexpectedError)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, WillThrow)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AShr)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericAShr)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Add)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericAdd)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Alignof)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AllocRaw)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, And)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericAnd)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AssertConf)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AssignCopyArrayNoAlias)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AssignCopyArrayFrontToBack)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AssignCopyArrayBackToFront)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AssignTakeArray)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AssumeNonNegative)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AssumeTrue)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AtomicLoad)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AtomicRMW)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, AtomicStore)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, BitCast)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, CanBeObjCClass)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, CondFailMessage)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, CmpXChg)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, CondUnreachable)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, CopyArray)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, DeallocRaw)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, DestroyArray)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ExactSDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericExactSDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ExactUDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericExactUDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ExtractElement)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FAdd)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericFAdd)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_OEQ)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_OGE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_OGT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_OLE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_OLT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_ONE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_ORD)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_UEQ)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_UGE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_UGT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_ULE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_ULT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_UNE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FCMP_UNO)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericFDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FMul)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericFMul)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FNeg)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FPExt)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FPToSI)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FPToUI)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FPTrunc)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FRem)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericFRem)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, FSub)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericFSub)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Fence)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GetObjCTypeEncoding)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_EQ)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_NE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_SGE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_SGT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_SLE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_SLT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_UGE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_UGT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_ULE)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ICMP_ULT)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, InsertElement)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IntToFPWithOverflow)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IntToPtr)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IsOptionalType)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IsPOD)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IsConcrete)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IsBitwiseTakable)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IsSameMetatype)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, LShr)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericLShr)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Mul)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericMul)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, OnFastPath)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Once)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, OnceWithContext)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Or)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericOr)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, PtrToInt)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SAddOver)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericSDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SExt)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SExtOrBitCast)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SIToFP)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SMulOver)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SRem)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericSRem)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SSubOver)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SToSCheckedTrunc)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, SToUCheckedTrunc)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Expect)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Shl)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericShl)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Sizeof)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, StaticReport)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Strideof)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, StringObjectOr)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Sub)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericSub)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, TakeArrayNoAlias)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, TakeArrayBackToFront)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, TakeArrayFrontToBack)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Trunc)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, TruncOrBitCast)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, TSanInoutAccess)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UAddOver)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericUDiv)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UIToFP)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UMulOver)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, URem)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericURem)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, USubOver)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UToSCheckedTrunc)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UToUCheckedTrunc)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Unreachable)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, UnsafeGuaranteedEnd)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Xor)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GenericXor)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ZExt)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ZExtOrBitCast)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, ZeroInitializer)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, Swift3ImplicitObjCEntrypoint)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, PoundAssert)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, GlobalStringTablePointer)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, TypePtrAuthDiscriminator)
BUILTIN_OPERAND_OWNERSHIP(InstantaneousUse, IntInstrprofIncrement)

BUILTIN_OPERAND_OWNERSHIP(ForwardingConsume, COWBufferForReading)
BUILTIN_OPERAND_OWNERSHIP(ForwardingConsume, UnsafeGuaranteed)

// FIXME: These are considered InteriorPointer because they may propagate a
// pointer into a borrowed values. If they do not propagate an interior pointer,
// then they should be InstantaneousUse instead and should not require a
// guaranteed value.
BUILTIN_OPERAND_OWNERSHIP(InteriorPointer, CancelAsyncTask)
BUILTIN_OPERAND_OWNERSHIP(InteriorPointer, CreateAsyncTask)
BUILTIN_OPERAND_OWNERSHIP(InteriorPointer, CreateAsyncTaskFuture)
BUILTIN_OPERAND_OWNERSHIP(InteriorPointer, InitializeDefaultActor)
BUILTIN_OPERAND_OWNERSHIP(InteriorPointer, DestroyDefaultActor)

// FIXME: Why do these reqiuire a borrowed value at all?
BUILTIN_OPERAND_OWNERSHIP(ForwardingBorrow, AutoDiffAllocateSubcontext)
BUILTIN_OPERAND_OWNERSHIP(ForwardingBorrow, AutoDiffProjectTopLevelSubcontext)

// FIXME: ConvertTaskToJob is documented as taking NativePointer. It's operand's
// ownership should be 'None'.
BUILTIN_OPERAND_OWNERSHIP(ForwardingConsume, ConvertTaskToJob)

BUILTIN_OPERAND_OWNERSHIP(None, AutoDiffCreateLinearMapContext)

#undef BUILTIN_OPERAND_OWNERSHIP

#define SHOULD_NEVER_VISIT_BUILTIN(ID)                                         \
  OperandOwnership                                                             \
  OperandOwnershipBuiltinClassifier::visit##ID(BuiltinInst *, StringRef) {     \
    llvm_unreachable(                                                          \
        "Builtin should never be visited! E.x.: It may not have arguments");   \
  }
SHOULD_NEVER_VISIT_BUILTIN(GetCurrentAsyncTask)
#undef SHOULD_NEVER_VISIT_BUILTIN

// Builtins that should be lowered to SIL instructions so we should never see
// them.
#define BUILTIN_SIL_OPERATION(ID, NAME, CATEGORY)                              \
  OperandOwnership                                                             \
  OperandOwnershipBuiltinClassifier::visit##ID(BuiltinInst *, StringRef) {     \
    llvm_unreachable("Builtin should have been lowered to SIL instruction?!"); \
  }
#define BUILTIN(X, Y, Z)
#include "swift/AST/Builtins.def"

OperandOwnership OperandOwnershipClassifier::visitBuiltinInst(BuiltinInst *bi) {
  return OperandOwnershipBuiltinClassifier().check(bi);
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

OperandOwnership Operand::getOperandOwnership() const {
  // We consider type dependent uses to be instantaneous uses.
  //
  // NOTE: We could instead try to exclude type dependent uses from our system,
  // but that adds a bunch of Optionals and unnecessary types. This doesn't hurt
  // anything and allows us to eliminate Optionals and thus confusion in between
  // Optional::None and OwnershipKind::None.
  if (isTypeDependent())
    return OperandOwnership::InstantaneousUse;

  // If we do not have ownership enabled, just return any. This ensures that we
  // do not have any consuming uses and everything from an ownership perspective
  // is just a liveness use short-circuiting many of the optimizations.
  //
  // We do not ever call this function when an instruction isn't in a block.
  assert(getUser()->getParent() &&
         "Can not lookup ownership constraint unless inserted into block");
  if (auto *block = getUser()->getParent()) {
    auto *func = block->getParent();
    // If we don't have a function, then we must have a SILGlobalVariable. In
    // that case, we act as if we aren't in ownership.
    if (!func || !func->hasOwnership()) {
      return OperandOwnership(OperandOwnership::InstantaneousUse);
    }
  }

  OperandOwnershipClassifier classifier(getUser()->getModule(), *this);
  return classifier.visit(const_cast<SILInstruction *>(getUser()));
}
