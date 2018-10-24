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
using namespace swift::ownership;

//===----------------------------------------------------------------------===//
//                      OperandOwnershipKindClassifier
//===----------------------------------------------------------------------===//

namespace {

class OperandOwnershipKindClassifier
    : public SILInstructionVisitor<OperandOwnershipKindClassifier,
                                   OperandOwnershipKindMap> {
public:
  using Map = OperandOwnershipKindMap;

private:
  LLVM_ATTRIBUTE_UNUSED SILModule &mod;

  const Operand &op;
  ErrorBehaviorKind errorBehavior;
  bool checkingSubObject;

public:
  /// Create a new OperandOwnershipKindClassifier.
  ///
  /// In most cases, one should only pass in \p Op and \p BaseValue will be set
  /// to Op.get(). In cases where one is trying to verify subobjects, Op.get()
  /// should be the subobject and Value should be the parent object. An example
  /// of where one would want to do this is in the case of value projections
  /// like struct_extract.
  OperandOwnershipKindClassifier(SILModule &mod, const Operand &op,
                                 ErrorBehaviorKind errorBehavior,
                                 bool checkingSubObject)
      : mod(mod), op(op), errorBehavior(errorBehavior),
        checkingSubObject(checkingSubObject) {}

  bool isCheckingSubObject() const { return checkingSubObject; }

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
    return getOwnershipKind() == ValueOwnershipKind::Trivial ||
           getOwnershipKind() == ValueOwnershipKind::Any;
  }

  OperandOwnershipKindMap visitForwardingInst(SILInstruction *i,
                                              ArrayRef<Operand> ops);
  OperandOwnershipKindMap visitForwardingInst(SILInstruction *i) {
    return visitForwardingInst(i, i->getAllOperands());
  }

  OperandOwnershipKindMap
  visitEnumArgument(ValueOwnershipKind requiredConvention);
  OperandOwnershipKindMap
  visitApplyParameter(ValueOwnershipKind requiredConvention,
                      UseLifetimeConstraint requirement);
  OperandOwnershipKindMap visitFullApply(FullApplySite apply);

  OperandOwnershipKindMap visitCallee(CanSILFunctionType substCalleeType);
  OperandOwnershipKindMap
  checkTerminatorArgumentMatchesDestBB(SILBasicBlock *destBB, unsigned opIndex);

// Create declarations for all instructions, so we get a warning at compile
// time if any instructions do not have an implementation.
#define INST(Id, Parent) OperandOwnershipKindMap visit##Id(Id *);
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

/// Implementation for instructions without operands. These should never be
/// visited.
#define NO_OPERAND_INST(INST)                                                  \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() == 0 &&                                         \
           "Expected instruction without operands?!");                         \
    llvm_unreachable("Instruction without operand can not be compatible with " \
                     "any def's OwnershipValueKind");                          \
  }
NO_OPERAND_INST(AllocBox)
NO_OPERAND_INST(AllocExistentialBox)
NO_OPERAND_INST(AllocGlobal)
NO_OPERAND_INST(AllocStack)
NO_OPERAND_INST(FloatLiteral)
NO_OPERAND_INST(FunctionRef)
NO_OPERAND_INST(GlobalAddr)
NO_OPERAND_INST(GlobalValue)
NO_OPERAND_INST(IntegerLiteral)
NO_OPERAND_INST(Metatype)
NO_OPERAND_INST(ObjCProtocol)
NO_OPERAND_INST(RetainValue)
NO_OPERAND_INST(RetainValueAddr)
NO_OPERAND_INST(StringLiteral)
NO_OPERAND_INST(StrongRetain)
NO_OPERAND_INST(Unreachable)
NO_OPERAND_INST(Unwind)
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  NO_OPERAND_INST(StrongRetain##Name)                                          \
  NO_OPERAND_INST(Name##Retain)
#include "swift/AST/ReferenceStorage.def"
#undef NO_OPERAND_INST

/// Instructions whose arguments are always compatible with one convention.
#define CONSTANT_OWNERSHIP_INST(OWNERSHIP, USE_LIFETIME_CONSTRAINT, INST)      \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return Map::compatibilityMap(                                              \
        ValueOwnershipKind::OWNERSHIP,                                         \
        UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT);                       \
  }
CONSTANT_OWNERSHIP_INST(Guaranteed, MustBeLive, IsEscapingClosure)
CONSTANT_OWNERSHIP_INST(Guaranteed, MustBeLive, RefElementAddr)
CONSTANT_OWNERSHIP_INST(Guaranteed, MustBeLive, OpenExistentialValue)
CONSTANT_OWNERSHIP_INST(Guaranteed, MustBeLive, OpenExistentialBoxValue)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, AutoreleaseValue)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, DeallocBox)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, DeallocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, DeallocRef)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, DestroyValue)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, ReleaseValue)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, ReleaseValueAddr)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, StrongRelease)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, InitExistentialRef)
CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, EndLifetime)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, AbortApply)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, AddressToPointer)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, BeginAccess)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, BeginUnpairedAccess)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, BindMemory)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, CheckedCastAddrBranch)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, CondFail)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, CopyAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, DeallocStack)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, DebugValueAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, DeinitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, DestroyAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, EndAccess)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, EndApply)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, EndUnpairedAccess)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, IndexAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, IndexRawPointer)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, InitBlockStorageHeader)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, InitEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, InitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, InitExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, InjectEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, IsUnique)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, Load)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, LoadBorrow)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, MarkFunctionEscape)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, MarkUninitializedBehavior)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ObjCExistentialMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ObjCMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ObjCToThickMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, OpenExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, OpenExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, PointerToAddress)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, PointerToThinFunction)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ProjectBlockStorage)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ProjectValueBuffer)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, RawPointerToRef)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, SelectEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, SelectValue)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, StructElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, SwitchEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, SwitchValue)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, TailAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ThickToObjCMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ThinFunctionToPointer)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, ThinToThickFunction)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, UncheckedRefCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, UnconditionalCheckedCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, DeallocValueBuffer)
// SWIFT_ENABLE_TENSORFLOW
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, Gradient)
CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, GraphOperation)
#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, Load##Name)
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                         \
  CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, Name##Release)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                      \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")                              \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, Name##ToRef)
#include "swift/AST/ReferenceStorage.def"
#undef CONSTANT_OWNERSHIP_INST

/// Instructions whose arguments are always compatible with one convention.
#define CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(OWNERSHIP, USE_LIFETIME_CONSTRAINT, \
                                           INST)                               \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return Map::compatibilityMap(                                              \
        {{ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive},     \
         {ValueOwnershipKind::OWNERSHIP,                                       \
          UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT}});                   \
  }
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, MustBeInvalidated,
                                   CheckedCastValueBranch)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, MustBeInvalidated,
                                   UnconditionalCheckedCastValue)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, MustBeInvalidated,
                                   InitExistentialValue)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, MustBeInvalidated,
                                   DeinitExistentialValue)
#undef CONSTANT_OR_TRIVIAL_OWNERSHIP_INST

#define ACCEPTS_ANY_OWNERSHIP_INST(INST)                                       \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    return Map::allLive();                                                     \
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
#undef ACCEPTS_ANY_OWNERSHIP_INST

// Trivial if trivial typed, otherwise must accept owned?
#define ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP_OR_METATYPE(USE_LIFETIME_CONSTRAINT,  \
                                                     INST)                     \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    if (getType().is<AnyMetatypeType>()) {                                     \
      return Map::compatibilityMap(ValueOwnershipKind::Trivial,                \
                                   UseLifetimeConstraint::MustBeLive);         \
    }                                                                          \
    return Map::compatibleWithAllExcept(ValueOwnershipKind::Trivial);          \
  }
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP_OR_METATYPE(MustBeLive, ClassMethod)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP_OR_METATYPE(MustBeLive, ObjCMethod)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP_OR_METATYPE(MustBeLive, ObjCSuperMethod)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP_OR_METATYPE(MustBeLive, SuperMethod)
#undef ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP_OR_METATYPE

// Trivial if trivial typed, otherwise must accept owned?
#define ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(USE_LIFETIME_CONSTRAINT, INST)        \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return Map::compatibleWithAllExcept(ValueOwnershipKind::Trivial);          \
  }
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, BridgeObjectToWord)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, ClassifyBridgeObject)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, CopyBlock)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, OpenExistentialBox)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, RefTailAddr)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, RefToRawPointer)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, SetDeallocating)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, ProjectExistentialBox)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, UnmanagedRetainValue)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, UnmanagedReleaseValue)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, UnmanagedAutoreleaseValue)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, ConvertEscapeToNoEscape)
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, RefTo##Name)                    \
  ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, Name##ToRef)                    \
  ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, Copy##Name##Value)
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, RefTo##Name)
#include "swift/AST/ReferenceStorage.def"
#undef ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitForwardingInst(SILInstruction *i,
                                                    ArrayRef<Operand> ops) {
  assert(i->getNumOperands() && "Expected to have non-zero operands");
  assert(isOwnershipForwardingInst(i) &&
         "Expected to have an ownership forwarding inst");

  // Find the first index where we have a non-trivial value.
  auto iter = find_if(ops, [&i](const Operand &op) -> bool {
    if (i->isTypeDependentOperand(op))
      return false;
    return op.get().getOwnershipKind() != ValueOwnershipKind::Trivial;
  });

  // If we do not find a non-trivial value, then we know for sure that we have a
  // trivial value.
  if (iter == ops.end()) {
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
  }

  // Ok, we have at least a single non-trivial value. Grab the type of the
  // operand and see if it is trivial or non-trivial. If the type of the operand
  // is trivial, then return that we accept trivial here. Otherwise, return the
  // base ownership kind.
  if (getType().isTrivial(mod))
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);

  // Otherwise, return the value ownership kind and forwarding lifetime
  // constraint of the first non-trivial operand. This will ensure that all
  // non-trivial operands have the same ownership kind.
  unsigned index = std::distance(ops.begin(), iter);
  ValueOwnershipKind kind = ops[index].get().getOwnershipKind();
  auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();
  return Map::compatibilityMap(kind, lifetimeConstraint);
}

#define FORWARD_ANY_OWNERSHIP_INST(INST)                                       \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    return visitForwardingInst(i);                                             \
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
FORWARD_ANY_OWNERSHIP_INST(MarkUninitialized)
FORWARD_ANY_OWNERSHIP_INST(UncheckedEnumData)
FORWARD_ANY_OWNERSHIP_INST(DestructureStruct)
FORWARD_ANY_OWNERSHIP_INST(DestructureTuple)
#undef FORWARD_ANY_OWNERSHIP_INST

// An instruction that forwards a constant ownership or trivial ownership.
#define FORWARD_CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(                            \
    OWNERSHIP, USE_LIFETIME_CONSTRAINT, INST)                                  \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    assert(isGuaranteedForwardingInst(i) &&                                    \
           "Expected an ownership forwarding inst");                           \
    OperandOwnershipKindMap map;                                               \
    map.add(ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive);   \
    map.addCompatibilityConstraint(                                            \
        ValueOwnershipKind::OWNERSHIP,                                         \
        UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT);                       \
    return map;                                                                \
  }
FORWARD_CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, MustBeLive, TupleExtract)
FORWARD_CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, MustBeLive,
                                           StructExtract)
#undef CONSTANT_OR_TRIVIAL_OWNERSHIP_INST

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitDeallocPartialRefInst(
    DeallocPartialRefInst *i) {
  if (getValue() == i->getInstance()) {
    return Map::compatibilityMap(ValueOwnershipKind::Owned,
                                 UseLifetimeConstraint::MustBeInvalidated);
  }

  return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                               UseLifetimeConstraint::MustBeLive);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitSelectEnumInst(SelectEnumInst *i) {
  if (getValue() == i->getEnumOperand()) {
    return Map::allLive();
  }

  return visitForwardingInst(i, i->getAllOperands().drop_front());
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitAllocRefInst(AllocRefInst *i) {
  assert(i->getNumOperands() != 0 &&
         "If we reach this point, we must have a tail operand");
  return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                               UseLifetimeConstraint::MustBeLive);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitAllocRefDynamicInst(
    AllocRefDynamicInst *i) {
  assert(i->getNumOperands() != 0 &&
         "If we reach this point, we must have a tail operand");
  return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                               UseLifetimeConstraint::MustBeLive);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::checkTerminatorArgumentMatchesDestBB(
    SILBasicBlock *destBB, unsigned opIndex) {
  // Grab the ownership kind of the destination block.
  ValueOwnershipKind destBlockArgOwnershipKind =
      destBB->getArgument(opIndex)->getOwnershipKind();

  // Then if we do not have an enum, make sure that the conventions match.
  if (!getType().getEnumOrBoundGenericEnum()) {
    auto lifetimeConstraint =
        destBlockArgOwnershipKind.getForwardingLifetimeConstraint();
    return Map::compatibilityMap(destBlockArgOwnershipKind, lifetimeConstraint);
  }

  // Otherwise, we need to properly handle the sum type nature of enum
  // arguments.
  return visitEnumArgument(destBlockArgOwnershipKind);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitBranchInst(BranchInst *bi) {
  return checkTerminatorArgumentMatchesDestBB(bi->getDestBB(),
                                              getOperandIndex());
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitCondBranchInst(CondBranchInst *cbi) {
  // If our conditional branch is the condition, it is trivial. Check that the
  // ownership kind is trivial.
  if (cbi->isConditionOperandIndex(getOperandIndex()))
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);

  // Otherwise, make sure that our operand matches the ownership of the relevant
  // argument.
  //
  // TODO: Use more updated APIs here to get the operands/etc.
  if (cbi->isTrueOperandIndex(getOperandIndex())) {
    unsigned trueOffset = 1;
    return checkTerminatorArgumentMatchesDestBB(cbi->getTrueBB(),
                                                getOperandIndex() - trueOffset);
  }

  assert(cbi->isFalseOperandIndex(getOperandIndex()) &&
         "If an operand is not the condition index or a true operand index, it "
         "must be a false operand index");
  unsigned falseOffset = 1 + cbi->getTrueOperands().size();
  return checkTerminatorArgumentMatchesDestBB(cbi->getFalseBB(),
                                              getOperandIndex() - falseOffset);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitSwitchEnumInst(SwitchEnumInst *sei) {
  auto opTy = sei->getOperand()->getType();
  auto &mod = sei->getModule();
  // If our passed in type is trivial, we shouldn't have any non-trivial
  // successors. Just bail early returning trivial.
  if (opTy.isTrivial(mod))
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);

  // Otherwise, go through the cases of the enum. If we have any cases with
  // trivial payload or no payload cases, add trivial as a base ownership kind
  // we can accept.
  OperandOwnershipKindMap map;

  bool foundNonTrivialCase = false;

  auto *enumDecl = opTy.getEnumOrBoundGenericEnum();
  assert(enumDecl);
  for (auto *eltDecl : enumDecl->getAllElements()) {
    // If we have a no-payload case add that we support trivial and continue.
    if (!eltDecl->hasAssociatedValues()) {
      map.addCompatibilityConstraint(ValueOwnershipKind::Trivial,
                                     UseLifetimeConstraint::MustBeLive);
      continue;
    }

    // If we have a completely trivial payload case, then add that we support
    // trivial and continue.
    if (opTy.getEnumElementType(eltDecl, mod).isTrivial(mod)) {
      map.addCompatibilityConstraint(ValueOwnershipKind::Trivial,
                                     UseLifetimeConstraint::MustBeLive);
      continue;
    }

    // Otherwise, we have a non-trivial case. Set foundNonTrivialCase to
    // true. We will need to check the arguments of the switch_enum's successors
    // for the ownership kind that we can accept.
    foundNonTrivialCase = true;
  }

  // If we didn't find a non-trivial case, return the map we have constructed so
  // far.
  if (!foundNonTrivialCase)
    return map;

  // Otherwise, we want to find the ownership constraint of our successor
  // arguments.
  Optional<ValueOwnershipKind> nonTrivialKind;
  for (auto argArray : sei->getSuccessorBlockArguments()) {
    if (argArray.empty())
      continue;
    SILValue arg = argArray[getOperandIndex()];
    if (arg->getType().isTrivial(mod))
      continue;

    // If we haven't found a non-trivial kind yet, stash the kind we find.
    if (!nonTrivialKind) {
      nonTrivialKind = arg.getOwnershipKind();
      continue;
    }

    // Otherwise if we /do/ have a non trivial kind and the argument's ownership
    // kind is compatible, merge in case the first value we saw had Any
    // ownership.
    auto newKind = nonTrivialKind->merge(arg.getOwnershipKind());
    if (newKind) {
      nonTrivialKind = newKind;
      continue;
    }

    // Otherwise, we have inconsistent ownership in between our successors. To
    // be sure that we error, return an empty map.
    return Map();
  }

  // We should never have an enum with a non-trivial case where we do not have
  // at least one successor with a proper ownership qualifier since we either
  // switch over the entire enum implying we visit that case, or we go through
  // the default which will have our enum type as its type and thus some form of
  // non-trivial ownership. So it is correct to use the optional here without
  // checking.
  auto lifetimeConstraint = nonTrivialKind->getForwardingLifetimeConstraint();
  map.addCompatibilityConstraint(*nonTrivialKind, lifetimeConstraint);

  return map;
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitCheckedCastBranchInst(
    CheckedCastBranchInst *ccbi) {
  Optional<OperandOwnershipKindMap> map;
  for (auto argArray : ccbi->getSuccessorBlockArguments()) {
    assert(!argArray.empty());

    auto argOwnershipKind = argArray[getOperandIndex()]->getOwnershipKind();
    // If we do not have a map yet, initialize it and continue.
    if (!map) {
      auto lifetimeConstraint =
          argOwnershipKind.getForwardingLifetimeConstraint();
      map = Map::compatibilityMap(argOwnershipKind, lifetimeConstraint);
      continue;
    }

    // Otherwise, make sure that we can accept the rest of our
    // arguments. If not, we return an empty ownership kind to make
    // sure that we flag everything as an error.
    if (map->canAcceptKind(argOwnershipKind)) {
      continue;
    }

    return OperandOwnershipKindMap();
  }

  return map.getValue();
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitReturnInst(ReturnInst *ri) {
  bool isTrivial = ri->getOperand()->getType().isTrivial(mod);
  SILFunctionConventions fnConv = ri->getFunction()->getConventions();
  auto results = fnConv.getDirectSILResults();
  // FIXME: Shouldn't we return an empty OperandOwnershipKindMap here if we do
  // not have any results?
  if (results.empty() || isTrivial) {
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
  }

  CanGenericSignature sig = fnConv.funcTy->getGenericSignature();

  // Find the first index where we have a trivial value.
  auto iter = find_if(results, [this, &sig](const SILResultInfo &info) -> bool {
    return info.getOwnershipKind(mod, sig) != ValueOwnershipKind::Trivial;
  });

  // If we have all trivial, then we must be trivial. Why wasn't our original
  // type trivial? This is a hard error since this is a logic error in our code
  // here.
  if (iter == results.end())
    llvm_unreachable("Should have already checked a trivial type?!");

  ValueOwnershipKind base = iter->getOwnershipKind(mod, sig);

  for (const SILResultInfo &resultInfo :
       SILFunctionConventions::DirectSILResultRange(std::next(iter),
                                                    results.end())) {
    auto rKind = resultInfo.getOwnershipKind(mod, sig);
    // Ignore trivial types.
    if (rKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto mergedValue = base.merge(rKind);
    // If we fail to merge all types in, bail. We can not come up with a proper
    // result type. We assert here since this is a hard error in the normal
    // SILVerifier since the return type of the function would not match its
    // terminator.
    assert(mergedValue.hasValue() &&
           "Failed to merge all types in on a return?!");
    // In case Base is Any.
    base = mergedValue.getValue();
  }

  if (getType().getEnumOrBoundGenericEnum()) {
    return visitEnumArgument(base);
  }

  return Map::compatibilityMap(base, UseLifetimeConstraint::MustBeInvalidated);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitEndBorrowInst(EndBorrowInst *i) {
  // If we are checking a subobject, make sure that we are from a guaranteed
  // basic block argument.
  if (isCheckingSubObject()) {
    auto *phiArg = cast<SILPhiArgument>(op.get());
    (void)phiArg;
    return Map::compatibilityMap(ValueOwnershipKind::Guaranteed,
                                 UseLifetimeConstraint::MustBeLive);
  }

  /// An end_borrow is modeled as invalidating the guaranteed value preventing
  /// any further uses of the value.
  return Map::compatibilityMap(ValueOwnershipKind::Guaranteed,
                               UseLifetimeConstraint::MustBeInvalidated);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitThrowInst(ThrowInst *i) {
  return Map::compatibilityMap(ValueOwnershipKind::Owned,
                               UseLifetimeConstraint::MustBeInvalidated);
}

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  OperandOwnershipKindMap                                                      \
      OperandOwnershipKindClassifier::visitStore##Name##Inst(                  \
          Store##Name##Inst *i) {                                              \
    /* A store instruction implies that the value to be stored to be live, */  \
    /* but it does not touch the strong reference count of the value. We */    \
    /* also just care about liveness for the dest. So just match everything */ \
    /* as must be live. */                                                     \
    return Map::allLive();                                                     \
  }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                      \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#include "swift/AST/ReferenceStorage.def"

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitStoreBorrowInst(StoreBorrowInst *i) {
  if (getValue() == i->getSrc()) {
    return Map::compatibilityMap(ValueOwnershipKind::Guaranteed,
                                 UseLifetimeConstraint::MustBeLive);
  }
  return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                               UseLifetimeConstraint::MustBeLive);
}

// FIXME: Why not use SILArgumentConvention here?
OperandOwnershipKindMap OperandOwnershipKindClassifier::visitCallee(
    CanSILFunctionType substCalleeType) {
  ParameterConvention conv = substCalleeType->getCalleeConvention();
  switch (conv) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Constant:
    assert(!SILModuleConventions(mod).isSILIndirect(
        SILParameterInfo(substCalleeType, conv)));
    return Map::compatibilityMap(ValueOwnershipKind::Owned,
                                 UseLifetimeConstraint::MustBeInvalidated);
  case ParameterConvention::Indirect_In_Guaranteed:
    assert(!SILModuleConventions(mod).isSILIndirect(
        SILParameterInfo(substCalleeType, conv)));
    return Map::compatibilityMap(ValueOwnershipKind::Guaranteed,
                                 UseLifetimeConstraint::MustBeLive);
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("Illegal convention for callee");
  case ParameterConvention::Direct_Unowned:
    return Map::allLive();
  case ParameterConvention::Direct_Owned:
    return Map::compatibilityMap(ValueOwnershipKind::Owned,
                                 UseLifetimeConstraint::MustBeInvalidated);
  case ParameterConvention::Direct_Guaranteed:
    if (substCalleeType->isNoEscape())
      return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                   UseLifetimeConstraint::MustBeLive);
    // We want to accept guaranteed/owned in this position since we
    // treat the use of an owned parameter as an instantaneously
    // borrowed value for the duration of the call.
    return Map::compatibilityMap(
        {{ValueOwnershipKind::Guaranteed, UseLifetimeConstraint::MustBeLive},
         {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeLive}});
  }

  llvm_unreachable("Unhandled ParameterConvention in switch.");
}

// Visit an enum value that is passed at argument position, including block
// arguments, apply arguments, and return values.
//
// The operand definition's ownership kind may be known to be "trivial",
// but it is still valid to pass that enum to a argument nontrivial type.
// For example:
//
// %val = enum $Optional<SomeClass>, #Optional.none // trivial ownership
// apply %f(%val) : (@owned Optional<SomeClass>)    // owned argument
OperandOwnershipKindMap OperandOwnershipKindClassifier::visitEnumArgument(
    ValueOwnershipKind requiredKind) {
  // If this value is already categorized as a trivial ownership kind,
  // it is safe to pass to any argument convention. This is ok since
  // we know that the enum type must match up as checked by the
  // ownership verifier.
  OperandOwnershipKindMap map;
  map.addCompatibilityConstraint(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);

  // The operand has a non-trivial ownership kind. It must match the argument
  // convention.
  if (requiredKind != ValueOwnershipKind::Owned) {
    map.addCompatibilityConstraint(ValueOwnershipKind::Owned,
                                   UseLifetimeConstraint::MustBeLive);
  } else {
    map.addCompatibilityConstraint(ValueOwnershipKind::Owned,
                                   UseLifetimeConstraint::MustBeInvalidated);
  }
  map.addCompatibilityConstraint(ValueOwnershipKind::Guaranteed,
                                 UseLifetimeConstraint::MustBeLive);
  map.addCompatibilityConstraint(ValueOwnershipKind::Unowned,
                                 UseLifetimeConstraint::MustBeLive);
  return map;
}

// We allow for trivial cases of enums with non-trivial cases to be passed in
// non-trivial argument positions. This fits with modeling of a
// SILFunctionArgument as a phi in a global program graph.
OperandOwnershipKindMap OperandOwnershipKindClassifier::visitApplyParameter(
    ValueOwnershipKind kind, UseLifetimeConstraint requirement) {

  // Check if we have an enum. If not, then we just check against the passed in
  // convention.
  if (!getType().getEnumOrBoundGenericEnum()) {
    // We allow for owned to be passed to apply parameters.
    if (kind != ValueOwnershipKind::Owned) {
      return Map::compatibilityMap(
          {{kind, requirement},
           {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeLive}});
    }
    return Map::compatibilityMap(kind, requirement);
  }

  // Otherwise consider that we may have a payload with a trivial case
  // that has other non-trivial cases.
  return visitEnumArgument(kind);
}

// Handle Apply and TryApply.
OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitFullApply(FullApplySite apply) {
  // If we are visiting the callee operand, handle it specially.
  if (apply.isCalleeOperand(op)) {
    return visitCallee(apply.getSubstCalleeType());
  }

  // Indirect return arguments are address types.
  if (apply.isIndirectResultOperand(op)) {
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
  }

  unsigned argIndex = apply.getCalleeArgIndex(op);
  auto conv = apply.getSubstCalleeConv();
  SILParameterInfo paramInfo = conv.getParamInfoForSILArg(argIndex);

  switch (paramInfo.getConvention()) {
  case ParameterConvention::Direct_Owned:
    return visitApplyParameter(ValueOwnershipKind::Owned,
                               UseLifetimeConstraint::MustBeInvalidated);
  case ParameterConvention::Direct_Unowned:
    return Map::allLive();

  case ParameterConvention::Indirect_In: {
    // This expects an @trivial if we have lowered addresses and @
    if (conv.useLoweredAddresses()) {
      return visitApplyParameter(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
    }
    // TODO: Once trivial is subsumed in any, this goes away.
    auto map = visitApplyParameter(ValueOwnershipKind::Owned,
                                   UseLifetimeConstraint::MustBeInvalidated);
    map.addCompatibilityConstraint(ValueOwnershipKind::Trivial,
                                   UseLifetimeConstraint::MustBeLive);
    return map;
  }

  case ParameterConvention::Indirect_In_Guaranteed: {
    // This expects an @trivial if we have lowered addresses and @
    if (conv.useLoweredAddresses()) {
      return visitApplyParameter(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
    }
    // TODO: Once trivial is subsumed in any, this goes away.
    auto map = visitApplyParameter(ValueOwnershipKind::Guaranteed,
                                   UseLifetimeConstraint::MustBeLive);
    map.addCompatibilityConstraint(ValueOwnershipKind::Trivial,
                                   UseLifetimeConstraint::MustBeLive);
    return map;
  }

  // The following conventions should take address types and thus be
  // trivial.
  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    return visitApplyParameter(ValueOwnershipKind::Trivial,
                               UseLifetimeConstraint::MustBeLive);

  case ParameterConvention::Direct_Guaranteed:
    // A +1 value may be passed to a guaranteed argument. From the caller's
    // point of view, this is just like a normal non-consuming use.
    // Direct_Guaranteed only accepts non-trivial types, but trivial types are
    // already handled above.
    return visitApplyParameter(ValueOwnershipKind::Guaranteed,
                               UseLifetimeConstraint::MustBeLive);
  }
  llvm_unreachable("unhandled convension");
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitBeginApplyInst(BeginApplyInst *i) {
  return visitFullApply(i);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitApplyInst(ApplyInst *i) {
  return visitFullApply(i);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitTryApplyInst(TryApplyInst *i) {
  return visitFullApply(i);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitPartialApplyInst(PartialApplyInst *i) {
  return Map::compatibilityMap(
      {{ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive},
       // All non-trivial types should be captured.
       {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated}});
}

// TODO: FIX THIS
OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitYieldInst(YieldInst *i) {
  // Indirect return arguments are address types.
  //
  // TODO: Change this to check if this operand is an indirect result
  if (isAddressOrTrivialType())
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);

  auto fnType = i->getFunction()->getLoweredFunctionType();
  auto yieldInfo = fnType->getYields()[getOperandIndex()];
  switch (yieldInfo.getConvention()) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Direct_Owned:
    return visitApplyParameter(ValueOwnershipKind::Owned,
                               UseLifetimeConstraint::MustBeInvalidated);
  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Direct_Unowned:
    // We accept unowned, owned, and guaranteed in unowned positions.
    return Map::allLive();
  case ParameterConvention::Indirect_In_Guaranteed:
  case ParameterConvention::Direct_Guaranteed:
    return visitApplyParameter(ValueOwnershipKind::Guaranteed,
                               UseLifetimeConstraint::MustBeLive);
  // The following conventions should take address types.
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("Unexpected non-trivial parameter convention.");
  }
  llvm_unreachable("unhandled convension");
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitAssignInst(AssignInst *i) {
  if (getValue() != i->getSrc()) {
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
  }

  return Map::compatibilityMap({
      {ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive},
      {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated},
  });
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitStoreInst(StoreInst *i) {
  if (getValue() != i->getSrc()) {
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
  }

  return Map::compatibilityMap({
      {ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive},
      {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated},
  });
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitCopyBlockWithoutEscapingInst(
    CopyBlockWithoutEscapingInst *i) {
  // Consumes the closure parameter.
  if (getValue() == i->getClosure()) {
    return Map::compatibilityMap(ValueOwnershipKind::Owned,
                                 UseLifetimeConstraint::MustBeInvalidated);
  }

  return Map::compatibleWithAllExcept(ValueOwnershipKind::Trivial);
}

OperandOwnershipKindMap OperandOwnershipKindClassifier::visitMarkDependenceInst(
    MarkDependenceInst *mdi) {

  // Forward ownership if the mark_dependence instruction marks a dependence
  // on a @noescape function type for an escaping function type.
  if (getValue() == mdi->getValue())
    if (auto resFnTy = mdi->getType().getAs<SILFunctionType>())
      if (auto baseFnTy = mdi->getBase()->getType().getAs<SILFunctionType>())
        if (!resFnTy->isNoEscape() && baseFnTy->isNoEscape())
          return Map::compatibilityMap(
              ValueOwnershipKind::Owned,
              UseLifetimeConstraint::MustBeInvalidated);

  // We always treat mark dependence as a use that keeps a value alive. We will
  // be introducing a begin_dependence/end_dependence version of this later.
  return Map::allLive();
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitKeyPathInst(KeyPathInst *I) {
  // KeyPath moves the value in memory out of address operands, but the
  // ownership checker doesn't reason about that yet.
  return Map::compatibilityMap(
      {{ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive},
       {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated}});
}

//===----------------------------------------------------------------------===//
//                            Builtin Use Checker
//===----------------------------------------------------------------------===//

namespace {

struct OperandOwnershipKindBuiltinClassifier
    : SILBuiltinVisitor<OperandOwnershipKindBuiltinClassifier,
                        OperandOwnershipKindMap> {
  using Map = OperandOwnershipKindMap;

  OperandOwnershipKindMap visitLLVMIntrinsic(BuiltinInst *bi,
                                             llvm::Intrinsic::ID id) {
    // LLVM intrinsics do not traffic in ownership, so if we have a result, it
    // must be trivial.
    return {ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive};
  }

  // BUILTIN_TYPE_CHECKER_OPERATION does not live past the type checker.
#define BUILTIN_TYPE_CHECKER_OPERATION(ID, NAME)

#define BUILTIN(ID, NAME, ATTRS)                                               \
  OperandOwnershipKindMap visit##ID(BuiltinInst *bi, StringRef attr);
#include "swift/AST/Builtins.def"

  OperandOwnershipKindMap check(BuiltinInst *bi) { return visit(bi); }
};

} // end anonymous namespace

// This is correct today since we do not have any builtins which return
// @guaranteed parameters. This means that we can only have a lifetime ending
// use with our builtins if it is owned.
#define CONSTANT_OWNERSHIP_BUILTIN(OWNERSHIP, USE_LIFETIME_CONSTRAINT, ID)     \
  OperandOwnershipKindMap OperandOwnershipKindBuiltinClassifier::visit##ID(    \
      BuiltinInst *, StringRef) {                                              \
    return Map::compatibilityMap(                                              \
        ValueOwnershipKind::OWNERSHIP,                                         \
        UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT);                       \
  }
CONSTANT_OWNERSHIP_BUILTIN(Owned, MustBeLive, ErrorInMain)
CONSTANT_OWNERSHIP_BUILTIN(Owned, MustBeLive, UnexpectedError)
CONSTANT_OWNERSHIP_BUILTIN(Owned, MustBeLive, WillThrow)
CONSTANT_OWNERSHIP_BUILTIN(Owned, MustBeInvalidated, UnsafeGuaranteed)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AShr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Add)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Alignof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AllocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, And)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AssertConf)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AssignCopyArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AssignCopyArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AssignCopyArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AssignTakeArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AssumeNonNegative)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AssumeTrue)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AtomicLoad)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AtomicRMW)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AtomicStore)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, BitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, CanBeObjCClass)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, CmpXChg)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, CondUnreachable)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, CopyArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, DeallocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, DestroyArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ExactSDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ExactUDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ExtractElement)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FAdd)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_OEQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_OGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_OGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_OLE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_OLT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_ONE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_ORD)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_UEQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_UNE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FCMP_UNO)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FMul)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FNeg)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FPExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FPToSI)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FPToUI)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FPTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FRem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, FSub)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Fence)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, GetObjCTypeEncoding)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_EQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_NE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_SGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_SGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_SLE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_SLT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ICMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, InsertElement)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, IntToFPWithOverflow)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, IntToPtr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, IsOptionalType)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, IsPOD)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, IsBitwiseTakable)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, IsSameMetatype)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, LShr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Mul)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, OnFastPath)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Once)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, OnceWithContext)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Or)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, PtrToInt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SRem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SSubOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, SUCheckedConversion)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Shl)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Sizeof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, StaticReport)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Strideof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, StringObjectOr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Sub)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, TakeArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, TakeArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, TakeArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Trunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, TruncOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, TSanInoutAccess)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, UAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, UDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, UIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, UMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, URem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, USCheckedConversion)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, USubOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, UToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, UToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Unreachable)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, UnsafeGuaranteedEnd)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Xor)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ZExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ZExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, ZeroInitializer)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, Swift3ImplicitObjCEntrypoint)
// SWIFT_ENABLE_TENSORFLOW
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, TensorFlowSend)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, TensorFlowReceive)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AutoDiffCreateTape)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AutoDiffPushToTape)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AutoDiffPopFromTape)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, AutoDiffDestroyTape)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, MustBeLive, PoundAssert)
#undef CONSTANT_OWNERSHIP_BUILTIN

// Builtins that should be lowered to SIL instructions so we should never see
// them.
#define BUILTIN_SIL_OPERATION(ID, NAME, CATEGORY)                              \
  OperandOwnershipKindMap OperandOwnershipKindBuiltinClassifier::visit##ID(    \
      BuiltinInst *, StringRef) {                                              \
    llvm_unreachable("Builtin should have been lowered to SIL instruction?!"); \
  }
#define BUILTIN(X, Y, Z)
#include "swift/AST/Builtins.def"

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitBuiltinInst(BuiltinInst *bi) {
  return OperandOwnershipKindBuiltinClassifier().check(bi);
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

OperandOwnershipKindMap
Operand::getOwnershipKindMap(bool isForwardingSubValue) const {
  OperandOwnershipKindClassifier classifier(getUser()->getModule(), *this,
                                            ErrorBehaviorKind::ReturnFalse,
                                            isForwardingSubValue);
  return classifier.visit(const_cast<SILInstruction *>(getUser()));
}
