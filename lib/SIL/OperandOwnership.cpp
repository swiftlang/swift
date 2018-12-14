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
    return getOwnershipKind() == ValueOwnershipKind::Any;
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
NO_OPERAND_INST(DynamicFunctionRef)
NO_OPERAND_INST(PreviousDynamicFunctionRef)
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
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, AbortApply)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, AddressToPointer)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, BeginAccess)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, BeginUnpairedAccess)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, BindMemory)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, CheckedCastAddrBranch)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, CondFail)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, CopyAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, DeallocStack)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, DebugValueAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, DeinitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, DestroyAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, EndAccess)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, EndApply)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, EndUnpairedAccess)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, IndexAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, IndexRawPointer)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, InitBlockStorageHeader)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, InitEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, InitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, InitExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, InjectEnumAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, IsUnique)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, Load)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, LoadBorrow)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, MarkFunctionEscape)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ObjCExistentialMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ObjCMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ObjCToThickMetatype)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, OpenExistentialAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, OpenExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, PointerToAddress)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, PointerToThinFunction)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ProjectBlockStorage)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ProjectValueBuffer)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, RawPointerToRef)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, SelectEnumAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, SelectValue)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, StructElementAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, SwitchEnumAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, SwitchValue)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, TailAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ThickToObjCMetatype)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ThinFunctionToPointer)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, ThinToThickFunction)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, UncheckedRefCastAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, UnconditionalCheckedCastAddr)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(Any, MustBeLive, DeallocValueBuffer)
#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  CONSTANT_OWNERSHIP_INST(Any, MustBeLive, Load##Name)
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                         \
  CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, Name##Release)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                      \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")                              \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  CONSTANT_OWNERSHIP_INST(Any, MustBeLive, Name##ToRef)
#include "swift/AST/ReferenceStorage.def"
#undef CONSTANT_OWNERSHIP_INST

/// Instructions whose arguments are always compatible with one convention.
#define CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(OWNERSHIP, USE_LIFETIME_CONSTRAINT, \
                                           INST)                               \
  OperandOwnershipKindMap OperandOwnershipKindClassifier::visit##INST##Inst(   \
      INST##Inst *i) {                                                         \
    assert(i->getNumOperands() && "Expected to have non-zero operands");       \
    return Map::compatibilityMap(                                              \
        ValueOwnershipKind::OWNERSHIP,                                         \
        UseLifetimeConstraint::USE_LIFETIME_CONSTRAINT);                       \
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
    return Map::allLive();                                                     \
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
    return Map::allLive();                                                     \
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

  // Merge all of the ownership of our operands. If we get back a .none from the
  // merge, then we return an empty compatibility map. This ensures that we will
  // not be compatible with /any/ input triggering a special error in the
  // ownership verifier.
  Optional<ValueOwnershipKind> optionalKind =
      ValueOwnershipKind::merge(makeOptionalTransformRange(
          ops, [&i](const Operand &op) -> Optional<ValueOwnershipKind> {
            if (i->isTypeDependentOperand(op))
              return None;
            return op.get().getOwnershipKind();
          }));
  if (!optionalKind)
    return Map();

  auto kind = optionalKind.getValue();
  if (kind == ValueOwnershipKind::Any)
    return Map::allLive();
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

  return Map::allLive();
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
  return Map::allLive();
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitAllocRefDynamicInst(
    AllocRefDynamicInst *i) {
  assert(i->getNumOperands() != 0 &&
         "If we reach this point, we must have a tail operand");
  return Map::allLive();
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
    return Map::allLive();

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
    return Map::allLive();

  // Otherwise, go through the ownership constraints of our successor arguments
  // and merge them.
  auto mergedKind = ValueOwnershipKind::merge(makeTransformRange(
      sei->getSuccessorBlockArguments(),
      [&](PhiArgumentArrayRef array) -> ValueOwnershipKind {
        // If the array is empty, we have a non-payloaded case. Return any.
        if (array.empty())
          return ValueOwnershipKind::Any;

        // Otherwise, we should have a single element since a payload is
        // a tuple.
        assert(std::distance(array.begin(), array.end()) == 1);
        SILPhiArgument *arg = array.front();
        return arg->getOwnershipKind();
      }));

  // If we failed to merge, return an empty map so we will fail to pattern match
  // with any operand. This is a known signal to the verifier that we failed to
  // merge in a forwarding context.
  if (!mergedKind)
    return Map();
  auto kind = mergedKind.getValue();
  if (kind == ValueOwnershipKind::Any)
    return Map::allLive();
  auto lifetimeConstraint = kind.getForwardingLifetimeConstraint();
  return Map::compatibilityMap(kind, lifetimeConstraint);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitCheckedCastBranchInst(
    CheckedCastBranchInst *ccbi) {
  // TODO: Simplify this using ValueOwnershipKind::merge.
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

//// FIX THIS HERE
OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitReturnInst(ReturnInst *ri) {
  // If we have a trivial value, return allLive().
  bool isTrivial = ri->getOperand()->getType().isTrivial(mod);
  if (isTrivial) {
    return Map::allLive();
  }

  SILFunctionConventions fnConv = ri->getFunction()->getConventions();

  auto results = fnConv.getDirectSILResults();
  if (results.empty())
    return Map();

  CanGenericSignature sig = fnConv.funcTy->getGenericSignature();
  auto ownershipKindRange = makeTransformRange(results,
                                               [&](const SILResultInfo &info) {
                                                 return info.getOwnershipKind(mod, sig);
                                               });

  // Then merge all of our ownership kinds. If we fail to merge, return an empty
  // map so we fail on all operands.
  auto mergedBase = ValueOwnershipKind::merge(ownershipKindRange);
  if (!mergedBase)
    return Map();

  auto base = *mergedBase;

  // TODO: This may not be needed once trivial is any.
  if (getType().getEnumOrBoundGenericEnum()) {
    return visitEnumArgument(base);
  }

  return Map::compatibilityMap(base, base.getForwardingLifetimeConstraint());
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
  return Map::allLive();
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
      return Map::allLive();
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
  // Begin with an empty map.
  OperandOwnershipKindMap map;

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
    return Map::allLive();
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
      return Map::allLive();
    }
    // TODO: Once trivial is subsumed in any, this goes away.
    auto map = visitApplyParameter(ValueOwnershipKind::Owned,
                                   UseLifetimeConstraint::MustBeInvalidated);
    return map;
  }

  case ParameterConvention::Indirect_In_Guaranteed: {
    // This expects an @trivial if we have lowered addresses and @
    if (conv.useLoweredAddresses()) {
      return Map::allLive();
    }
    return visitApplyParameter(ValueOwnershipKind::Guaranteed,
                               UseLifetimeConstraint::MustBeLive);
  }

  // The following conventions should take address types and thus be
  // trivial.
  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    return Map::allLive();

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
       // All non-trivial types should be captured.
       ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated);
}

// TODO: FIX THIS
OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitYieldInst(YieldInst *i) {
  // Indirect return arguments are address types.
  //
  // TODO: Change this to check if this operand is an indirect result
  if (isAddressOrTrivialType())
    return Map::allLive();

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
    return Map::allLive();
  }

  return Map::compatibilityMap(ValueOwnershipKind::Owned,
                               UseLifetimeConstraint::MustBeInvalidated);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitStoreInst(StoreInst *i) {
  if (getValue() != i->getSrc()) {
    return Map::allLive();
  }

  return Map::compatibilityMap(ValueOwnershipKind::Owned,
                               UseLifetimeConstraint::MustBeInvalidated);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitCopyBlockWithoutEscapingInst(
    CopyBlockWithoutEscapingInst *i) {
  // Consumes the closure parameter.
  if (getValue() == i->getClosure()) {
    return Map::compatibilityMap(ValueOwnershipKind::Owned,
                                 UseLifetimeConstraint::MustBeInvalidated);
  }

  return Map::allLive();
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
       ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated);
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
    return Map::allLive();
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
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AShr)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Add)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Alignof)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AllocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, And)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AssertConf)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AssignCopyArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AssignCopyArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AssignCopyArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AssignTakeArray)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AssumeNonNegative)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AssumeTrue)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AtomicLoad)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AtomicRMW)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, AtomicStore)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, BitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, CanBeObjCClass)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, CmpXChg)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, CondUnreachable)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, CopyArray)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, DeallocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, DestroyArray)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ExactSDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ExactUDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ExtractElement)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FAdd)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_OEQ)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_OGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_OGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_OLE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_OLT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_ONE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_ORD)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_UEQ)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_UNE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FCMP_UNO)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FMul)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FNeg)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FPExt)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FPToSI)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FPToUI)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FPTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FRem)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, FSub)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Fence)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, GetObjCTypeEncoding)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_EQ)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_NE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_SGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_SGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_SLE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_SLT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ICMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, InsertElement)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, IntToFPWithOverflow)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, IntToPtr)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, IsOptionalType)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, IsPOD)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, IsBitwiseTakable)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, IsSameMetatype)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, LShr)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Mul)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, OnFastPath)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Once)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, OnceWithContext)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Or)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, PtrToInt)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SExt)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SRem)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SSubOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, SToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Shl)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Sizeof)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, StaticReport)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Strideof)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, StringObjectOr)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Sub)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, TakeArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, TakeArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, TakeArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Trunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, TruncOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, TSanInoutAccess)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, UAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, UDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, UIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, UMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, URem)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, USubOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, UToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, UToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Unreachable)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, UnsafeGuaranteedEnd)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Xor)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ZExt)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ZExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, ZeroInitializer)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, Swift3ImplicitObjCEntrypoint)
CONSTANT_OWNERSHIP_BUILTIN(Any, MustBeLive, PoundAssert)
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
