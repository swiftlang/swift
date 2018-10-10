//===--- SILOwnershipVerifier.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-ownership-verifier"

#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/TransformArrayRef.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/BranchPropagatedUser.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILOpenedArchetypesTracker.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include <algorithm>

using namespace swift;
using namespace swift::ownership;

// This is an option to put the SILOwnershipVerifier in testing mode. This
// causes the following:
//
// 1. Instead of printing an error message and aborting, the verifier will print
// the message and continue. This allows for FileCheck testing of the verifier.
//
// 2. SILInstruction::verifyOperandOwnership() is disabled. This is used for
// verification in SILBuilder. This causes errors to be printed twice, once when
// we build the IR and a second time when we perform a full verification of the
// IR. For testing purposes, we just want the later.
llvm::cl::opt<bool> IsSILOwnershipVerifierTestingEnabled(
    "sil-ownership-verifier-enable-testing",
    llvm::cl::desc("Put the sil ownership verifier in testing mode. See "
                   "comment in SILOwnershipVerifier.cpp above option for more "
                   "information."));

/// This is an option to turn off ownership verification on a specific file. We
/// still emit code as if we are in ownership mode, but we do not verify. This
/// is useful for temporarily turning off verification on tests.
static llvm::cl::opt<bool>
    DisableOwnershipVerification("disable-sil-ownership-verification");

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool isValueAddressOrTrivial(SILValue V, SILModule &M) {
  return V->getType().isAddress() ||
         V.getOwnershipKind() == ValueOwnershipKind::Trivial ||
         V.getOwnershipKind() == ValueOwnershipKind::Any;
}

// These operations forward both owned and guaranteed ownership.
static bool isOwnershipForwardingValueKind(SILNodeKind K) {
  switch (K) {
  case SILNodeKind::TupleInst:
  case SILNodeKind::StructInst:
  case SILNodeKind::EnumInst:
  case SILNodeKind::OpenExistentialRefInst:
  case SILNodeKind::UpcastInst:
  case SILNodeKind::UncheckedRefCastInst:
  case SILNodeKind::ConvertFunctionInst:
  case SILNodeKind::RefToBridgeObjectInst:
  case SILNodeKind::BridgeObjectToRefInst:
  case SILNodeKind::UnconditionalCheckedCastInst:
  case SILNodeKind::UncheckedEnumDataInst:
  case SILNodeKind::MarkUninitializedInst:
  case SILNodeKind::SelectEnumInst:
  case SILNodeKind::SwitchEnumInst:
  case SILNodeKind::CheckedCastBranchInst:
  case SILNodeKind::BranchInst:
  case SILNodeKind::CondBranchInst:
  case SILNodeKind::DestructureStructInst:
  case SILNodeKind::DestructureTupleInst:
    return true;
  default:
    return false;
  }
}

// These operations forward guaranteed ownership, but don't necessarily forward
// owned values.
static bool isGuaranteedForwardingValueKind(SILNodeKind K) {
  switch (K) {
  case SILNodeKind::TupleExtractInst:
  case SILNodeKind::StructExtractInst:
  case SILNodeKind::OpenExistentialValueInst:
  case SILNodeKind::OpenExistentialBoxValueInst:
    return true;
  default:
    return isOwnershipForwardingValueKind(K);
  }
}

static bool isGuaranteedForwardingValue(SILValue V) {
  return isGuaranteedForwardingValueKind(
      V->getKindOfRepresentativeSILNodeInObject());
}

static bool isGuaranteedForwardingInst(SILInstruction *I) {
  return isGuaranteedForwardingValueKind(SILNodeKind(I->getKind()));
}

LLVM_ATTRIBUTE_UNUSED
static bool isOwnershipForwardingInst(SILInstruction *I) {
  return isOwnershipForwardingValueKind(SILNodeKind(I->getKind()));
}

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

  bool compatibleWithOwnership(ValueOwnershipKind Kind) const {
    return getOwnershipKind().isCompatibleWith(Kind);
  }

  bool hasExactOwnership(ValueOwnershipKind Kind) const {
    return getOwnershipKind() == Kind;
  }

  bool isAddressOrTrivialType() const {
    if (getType().isAddress())
      return true;
    return getOwnershipKind() == ValueOwnershipKind::Trivial ||
      getOwnershipKind() == ValueOwnershipKind::Any;
  }

  OperandOwnershipKindMap visitForwardingInst(SILInstruction *I,
                                              ArrayRef<Operand> Ops);
  OperandOwnershipKindMap visitForwardingInst(SILInstruction *I) {
    return visitForwardingInst(I, I->getAllOperands());
  }

  OperandOwnershipKindMap
  visitEnumArgument(ValueOwnershipKind RequiredConvention);
  OperandOwnershipKindMap
  visitApplyParameter(ValueOwnershipKind RequiredConvention,
                      UseLifetimeConstraint Requirement);
  OperandOwnershipKindMap visitFullApply(FullApplySite apply);

  OperandOwnershipKindMap visitCallee(CanSILFunctionType SubstCalleeType);
  OperandOwnershipKindMap
  checkTerminatorArgumentMatchesDestBB(SILBasicBlock *DestBB, unsigned OpIndex);

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
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  NO_OPERAND_INST(StrongRetain##Name) \
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
#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  CONSTANT_OWNERSHIP_INST(Trivial, MustBeLive, Load##Name)
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  CONSTANT_OWNERSHIP_INST(Owned, MustBeInvalidated, Name##Release)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...") \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...) \
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
      INST##Inst *I) {                                                         \
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
      INST##Inst *I) {                                                         \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
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
      INST##Inst *I) {                                                         \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
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
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, RefTo##Name) \
  ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, Name##ToRef) \
  ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(MustBeLive, Copy##Name##Value)
#define UNCHECKED_REF_STORAGE(Name, ...) \
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
      INST##Inst *I) {                                                         \
    return visitForwardingInst(I);                                             \
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
      INST##Inst *I) {                                                         \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
    assert(isGuaranteedForwardingInst(I) &&                                    \
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
OperandOwnershipKindClassifier::visitSelectEnumInst(SelectEnumInst *I) {
  if (getValue() == I->getEnumOperand()) {
    return Map::allLive();
  }

  return visitForwardingInst(I, I->getAllOperands().drop_front());
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitAllocRefInst(AllocRefInst *I) {
  assert(I->getNumOperands() != 0
         && "If we reach this point, we must have a tail operand");
  return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                               UseLifetimeConstraint::MustBeLive);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitAllocRefDynamicInst(
    AllocRefDynamicInst *I) {
  assert(I->getNumOperands() != 0 &&
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
OperandOwnershipKindClassifier::visitBranchInst(BranchInst *BI) {
  return checkTerminatorArgumentMatchesDestBB(BI->getDestBB(),
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
OperandOwnershipKindClassifier::visitReturnInst(ReturnInst *RI) {
  bool IsTrivial = RI->getOperand()->getType().isTrivial(mod);
  SILFunctionConventions fnConv = RI->getFunction()->getConventions();
  auto Results = fnConv.getDirectSILResults();
  // FIXME: Shouldn't we return an empty OperandOwnershipKindMap here if we do
  // not have any results?
  if (Results.empty() || IsTrivial) {
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
  }

  CanGenericSignature Sig = fnConv.funcTy->getGenericSignature();

  // Find the first index where we have a trivial value.
  auto Iter = find_if(Results, [this, &Sig](const SILResultInfo &Info) -> bool {
    return Info.getOwnershipKind(mod, Sig) != ValueOwnershipKind::Trivial;
  });

  // If we have all trivial, then we must be trivial. Why wasn't our original
  // type trivial? This is a hard error since this is a logic error in our code
  // here.
  if (Iter == Results.end())
    llvm_unreachable("Should have already checked a trivial type?!");

  ValueOwnershipKind Base = Iter->getOwnershipKind(mod, Sig);

  for (const SILResultInfo &ResultInfo :
       SILFunctionConventions::DirectSILResultRange(std::next(Iter),
                                                    Results.end())) {
    auto RKind = ResultInfo.getOwnershipKind(mod, Sig);
    // Ignore trivial types.
    if (RKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto MergedValue = Base.merge(RKind);
    // If we fail to merge all types in, bail. We can not come up with a proper
    // result type. We assert here since this is a hard error in the normal
    // SILVerifier since the return type of the function would not match its
    // terminator.
    assert(MergedValue.hasValue() &&
           "Failed to merge all types in on a return?!");
    // In case Base is Any.
    Base = MergedValue.getValue();
  }

  if (getType().getEnumOrBoundGenericEnum()) {
    return visitEnumArgument(Base);
  }

  return Map::compatibilityMap(Base, UseLifetimeConstraint::MustBeInvalidated);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitEndBorrowInst(EndBorrowInst *I) {
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
OperandOwnershipKindClassifier::visitThrowInst(ThrowInst *I) {
  return Map::compatibilityMap(ValueOwnershipKind::Owned,
                               UseLifetimeConstraint::MustBeInvalidated);
}

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...)                          \
  OperandOwnershipKindMap                                                      \
      OperandOwnershipKindClassifier::visitStore##Name##Inst(                  \
          Store##Name##Inst *I) {                                              \
    /* A store instruction implies that the value to be stored to be live, */  \
    /* but it does not touch the strong reference count of the value. We */    \
    /* also just care about liveness for the dest. So just match everything */ \
    /* as must be live. */                                                     \
    return Map::allLive();                                                     \
  }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#include "swift/AST/ReferenceStorage.def"

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitStoreBorrowInst(StoreBorrowInst *I) {
  if (getValue() == I->getSrc()) {
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
    ValueOwnershipKind Kind, UseLifetimeConstraint Requirement) {

  // Check if we have an enum. If not, then we just check against the passed in
  // convention.
  if (!getType().getEnumOrBoundGenericEnum()) {
    // We allow for owned to be passed to apply parameters.
    if (Kind != ValueOwnershipKind::Owned) {
      return Map::compatibilityMap(
          {{Kind, Requirement},
           {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeLive}});
    }
    return Map::compatibilityMap(Kind, Requirement);
  }

  // Otherwise consider that we may have a payload with a trivial case
  // that has other non-trivial cases.
  return visitEnumArgument(Kind);
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
OperandOwnershipKindClassifier::visitBeginApplyInst(BeginApplyInst *I) {
  return visitFullApply(I);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitApplyInst(ApplyInst *I) {
  return visitFullApply(I);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitTryApplyInst(TryApplyInst *I) {
  return visitFullApply(I);
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitPartialApplyInst(PartialApplyInst *I) {
  return Map::compatibilityMap(
      {{ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive},
       // All non-trivial types should be captured.
       {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated}});
}

// TODO: FIX THIS
OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitYieldInst(YieldInst *I) {
  // Indirect return arguments are address types.
  //
  // TODO: Change this to check if this operand is an indirect result
  if (isAddressOrTrivialType())
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);

  auto fnType = I->getFunction()->getLoweredFunctionType();
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
OperandOwnershipKindClassifier::visitAssignInst(AssignInst *I) {
  if (getValue() != I->getSrc()) {
    return Map::compatibilityMap(ValueOwnershipKind::Trivial,
                                 UseLifetimeConstraint::MustBeLive);
  }

  return Map::compatibilityMap({
      {ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive},
      {ValueOwnershipKind::Owned, UseLifetimeConstraint::MustBeInvalidated},
  });
}

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitStoreInst(StoreInst *I) {
  if (getValue() != I->getSrc()) {
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
    CopyBlockWithoutEscapingInst *I) {
  // Consumes the closure parameter.
  if (getValue() == I->getClosure()) {
    return Map::compatibilityMap(ValueOwnershipKind::Owned,
                                 UseLifetimeConstraint::MustBeInvalidated);
  }

  return Map::compatibleWithAllExcept(ValueOwnershipKind::Trivial);
}

OperandOwnershipKindMap OperandOwnershipKindClassifier::visitMarkDependenceInst(
    MarkDependenceInst *MDI) {

  // Forward ownership if the mark_dependence instruction marks a dependence
  // on a @noescape function type for an escaping function type.
  if (getValue() == MDI->getValue())
    if (auto ResFnTy = MDI->getType().getAs<SILFunctionType>())
      if (auto BaseFnTy = MDI->getBase()->getType().getAs<SILFunctionType>())
        if (!ResFnTy->isNoEscape() && BaseFnTy->isNoEscape())
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

  OperandOwnershipKindMap visitLLVMIntrinsic(BuiltinInst *BI,
                                             llvm::Intrinsic::ID ID) {
    // LLVM intrinsics do not traffic in ownership, so if we have a result, it
    // must be trivial.
    return {ValueOwnershipKind::Trivial, UseLifetimeConstraint::MustBeLive};
  }

    // BUILTIN_TYPE_CHECKER_OPERATION does not live past the type checker.
#define BUILTIN_TYPE_CHECKER_OPERATION(ID, NAME)

#define BUILTIN(ID, NAME, ATTRS)                                               \
  OperandOwnershipKindMap visit##ID(BuiltinInst *BI, StringRef Attr);
#include "swift/AST/Builtins.def"

  OperandOwnershipKindMap check(BuiltinInst *BI) { return visit(BI); }
};

} // end anonymous namespace

// This is correct today since we do not have any builtins which return
// @guaranteed parameters. This means that we can only have a lifetime ending
// use with our builtins if it is owned.
#define CONSTANT_OWNERSHIP_BUILTIN(OWNERSHIP, USE_LIFETIME_CONSTRAINT, ID)     \
  OperandOwnershipKindMap OperandOwnershipKindBuiltinClassifier::visit##ID(    \
      BuiltinInst *BI, StringRef Attr) {                                       \
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
#undef CONSTANT_OWNERSHIP_BUILTIN

// Builtins that should be lowered to SIL instructions so we should never see
// them.
#define BUILTIN_SIL_OPERATION(ID, NAME, CATEGORY)                              \
  OperandOwnershipKindMap OperandOwnershipKindBuiltinClassifier::visit##ID(    \
      BuiltinInst *BI, StringRef Attr) {                                       \
    llvm_unreachable("Builtin should have been lowered to SIL instruction?!"); \
  }
#define BUILTIN(X, Y, Z)
#include "swift/AST/Builtins.def"

OperandOwnershipKindMap
OperandOwnershipKindClassifier::visitBuiltinInst(BuiltinInst *BI) {
  return OperandOwnershipKindBuiltinClassifier().check(BI);
}

//===----------------------------------------------------------------------===//
//                         SILValueOwnershipChecker
//===----------------------------------------------------------------------===//

namespace {

// TODO: This class uses a bunch of global state like variables. It should be
// refactored into a large state object that is used by functions.
class SILValueOwnershipChecker {
  /// The result of performing the check.
  llvm::Optional<bool> result;

  /// The module that we are in.
  SILModule &mod;

  /// A cache of dead-end basic blocks that we use to determine if we can
  /// ignore "leaks".
  DeadEndBlocks &deadEndBlocks;

  /// The value whose ownership we will check.
  SILValue value;

  /// The action that the checker should perform on detecting an error.
  ErrorBehaviorKind errorBehavior;

  /// The list of lifetime ending users that we found. Only valid if check is
  /// successful.
  SmallVector<BranchPropagatedUser, 16> lifetimeEndingUsers;

  /// The list of non lifetime ending users that we found. Only valid if check
  /// is successful.
  SmallVector<BranchPropagatedUser, 16> regularUsers;

  /// The list of implicit non lifetime ending users that we found. This
  /// consists of instructions like end_borrow that end a scoped lifetime. We
  /// must treat those as regular uses and ensure that our value is not
  /// destroyed while that sub-scope is valid.
  ///
  /// TODO: Rename to SubBorrowScopeUsers?
  SmallVector<BranchPropagatedUser, 4> implicitRegularUsers;

  /// The set of blocks that we have visited.
  SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks;

public:
  SILValueOwnershipChecker(
      SILModule &mod, DeadEndBlocks &deadEndBlocks, SILValue value,
      ErrorBehaviorKind errorBehavior,
      llvm::SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks)
      : result(), mod(mod), deadEndBlocks(deadEndBlocks), value(value),
        errorBehavior(errorBehavior), visitedBlocks(visitedBlocks) {
    assert(value && "Can not initialize a checker with an empty SILValue");
  }

  ~SILValueOwnershipChecker() = default;
  SILValueOwnershipChecker(SILValueOwnershipChecker &) = delete;
  SILValueOwnershipChecker(SILValueOwnershipChecker &&) = delete;

  bool check() {
    if (result.hasValue())
      return result.getValue();

    LLVM_DEBUG(llvm::dbgs() << "Verifying ownership of: " << *value);
    result = checkUses();
    if (!result.getValue())
      return false;

    SmallVector<BranchPropagatedUser, 32> allRegularUsers;
    copy(regularUsers, std::back_inserter(allRegularUsers));
    copy(implicitRegularUsers, std::back_inserter(allRegularUsers));
    result =
        valueHasLinearLifetime(value, lifetimeEndingUsers, allRegularUsers,
                               visitedBlocks, deadEndBlocks, errorBehavior);

    return result.getValue();
  }

  using user_array_transform =
      std::function<SILInstruction *(BranchPropagatedUser)>;
  using user_array = TransformArrayRef<user_array_transform>;

  /// A function that returns a range of lifetime ending users found for the
  /// given value.
  user_array getLifetimeEndingUsers() const {
    assert(result.hasValue() && "Can not call until check() is called");
    assert(result.getValue() && "Can not call if check() returned false");

    user_array_transform transform(
        [](BranchPropagatedUser user) -> SILInstruction * {
          return user.getInst();
        });
    return user_array(ArrayRef<BranchPropagatedUser>(lifetimeEndingUsers),
                      transform);
  }

  /// A function that returns a range of regular (i.e. "non lifetime ending")
  /// users found for the given value.
  user_array getRegularUsers() const {
    assert(result.hasValue() && "Can not call until check() is called");
    assert(result.getValue() && "Can not call if check() returned false");

    user_array_transform transform(
        [](BranchPropagatedUser user) -> SILInstruction * {
          return user.getInst();
        });
    return user_array(ArrayRef<BranchPropagatedUser>(regularUsers), transform);
  }

private:
  bool checkUses();
  bool gatherUsers(SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers,
                   SmallVectorImpl<BranchPropagatedUser> &regularUsers,
                   SmallVectorImpl<BranchPropagatedUser> &implicitRegularUsers);

  bool checkValueWithoutLifetimeEndingUses();

  bool checkFunctionArgWithoutLifetimeEndingUses(SILFunctionArgument *arg);
  bool checkYieldWithoutLifetimeEndingUses(BeginApplyResult *yield);

  bool isGuaranteedFunctionArgWithLifetimeEndingUses(
      SILFunctionArgument *arg,
      const SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers) const;
  bool isSubobjectProjectionWithLifetimeEndingUses(
      SILValue value,
      const SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers) const;

  /// Depending on our initialization, either return false or call Func and
  /// throw an error.
  bool handleError(function_ref<void()> &&messagePrinterFunc) const {
    if (errorBehavior.shouldPrintMessage()) {
      messagePrinterFunc();
    }

    if (errorBehavior.shouldReturnFalse()) {
      return false;
    }

    assert(errorBehavior.shouldAssert() && "At this point, we should assert");
    llvm_unreachable("triggering standard assertion failure routine");
  }
};

} // end anonymous namespace

bool SILValueOwnershipChecker::gatherUsers(
    SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers,
    SmallVectorImpl<BranchPropagatedUser> &nonLifetimeEndingUsers,
    SmallVectorImpl<BranchPropagatedUser> &implicitRegularUsers) {

  // See if Value is guaranteed. If we are guaranteed and not forwarding, then
  // we need to look through subobject uses for more uses. Otherwise, if we are
  // forwarding, we do not create any lifetime ending users/non lifetime ending
  // users since we verify against our base.
  auto ownershipKind = value.getOwnershipKind();
  bool isGuaranteed = ownershipKind == ValueOwnershipKind::Guaranteed;
  bool isOwned = ownershipKind == ValueOwnershipKind::Owned;

  if (isGuaranteed && isGuaranteedForwardingValue(value))
    return true;

  // Then gather up our initial list of users.
  SmallVector<Operand *, 8> users;
  std::copy(value->use_begin(), value->use_end(), std::back_inserter(users));

  auto addCondBranchToList = [](SmallVectorImpl<BranchPropagatedUser> &list,
                                CondBranchInst *cbi, unsigned operandIndex) {
    if (cbi->isConditionOperandIndex(operandIndex)) {
      list.emplace_back(cbi);
      return;
    }

    bool isTrueOperand = cbi->isTrueOperandIndex(operandIndex);
    list.emplace_back(cbi, isTrueOperand ? CondBranchInst::TrueIdx
                                         : CondBranchInst::FalseIdx);
  };

  bool foundError = false;
  while (!users.empty()) {
    Operand *op = users.pop_back_val();
    SILInstruction *user = op->getUser();

    // If this op is a type dependent operand, skip it. It is not interesting
    // from an ownership perspective.
    if (user->isTypeDependentOperand(*op))
      continue;

    bool isGuaranteedSubValue = false;
    if (isGuaranteed && isGuaranteedForwardingInst(op->getUser())) {
      isGuaranteedSubValue = true;
    }

    auto opOwnershipKindMap = op->getOwnershipKindMap(isGuaranteedSubValue);
    // If our ownership kind doesn't match, track that we found an error, emit
    // an error message optionally and then continue.
    if (!opOwnershipKindMap.canAcceptKind(ownershipKind)) {
      foundError = true;

      // If we did not support /any/ ownership kind, it means that we found a
      // conflicting answer so the kind map that was returned is the empty
      // map. Put out a more specific error here.
      if (!opOwnershipKindMap.data.any()) {
        handleError([&]() {
          llvm::errs() << "Function: '" << user->getFunction()->getName()
                       << "'\n"
                       << "Ill-formed SIL! Unable to compute ownership kind "
                          "map for user?!\n"
                       << "For terminator users, check that successors have "
                          "compatible ownership kinds.\n"
                       << "Value: " << op->get() << "User: " << *user
                       << "Operand Number: " << op->getOperandNumber() << '\n'
                       << "Conv: " << ownershipKind << "\n\n";
        });
        continue;
      }

      handleError([&]() {
        llvm::errs() << "Function: '" << user->getFunction()->getName() << "'\n"
                     << "Have operand with incompatible ownership?!\n"
                     << "Value: " << op->get() << "User: " << *user
                     << "Operand Number: " << op->getOperandNumber() << '\n'
                     << "Conv: " << ownershipKind << '\n'
                     << "OwnershipMap:\n"
                     << opOwnershipKindMap << '\n';
      });
      continue;
    }

    auto lifetimeConstraint =
        opOwnershipKindMap.getLifetimeConstraint(ownershipKind);
    if (lifetimeConstraint == UseLifetimeConstraint::MustBeInvalidated) {
      LLVM_DEBUG(llvm::dbgs() << "        Lifetime Ending User: " << *user);
      if (auto *cbi = dyn_cast<CondBranchInst>(user)) {
        addCondBranchToList(lifetimeEndingUsers, cbi, op->getOperandNumber());
      } else {
        lifetimeEndingUsers.emplace_back(user);
      }
    } else {
      LLVM_DEBUG(llvm::dbgs() << "        Regular User: " << *user);
      if (auto *cbi = dyn_cast<CondBranchInst>(user)) {
        addCondBranchToList(nonLifetimeEndingUsers, cbi,
                            op->getOperandNumber());
      } else {
        nonLifetimeEndingUsers.emplace_back(user);
      }
    }

    // If our base value is not guaranteed, we do not to try to visit
    // subobjects.
    if (!isGuaranteed) {
      // But if we are owned, check if we have any end_borrows. We
      // need to treat these as sub-scope users. We can rely on the
      // end_borrow to prevent recursion.
      if (isOwned) {
        // Do a check if any of our users are begin_borrows. If we find such a
        // use, then we want to include the end_borrow associated with the
        // begin_borrow in our NonLifetimeEndingUser lists.
        //
        // For correctness reasons we use indices to make sure that we can
        // append to NonLifetimeEndingUsers without needing to deal with
        // iterator invalidation.
        SmallVector<SILInstruction *, 4> endBorrowInsts;
        for (unsigned i : indices(nonLifetimeEndingUsers)) {
          if (auto *bbi = dyn_cast<BeginBorrowInst>(
                  nonLifetimeEndingUsers[i].getInst())) {
            copy(bbi->getEndBorrows(),
                 std::back_inserter(implicitRegularUsers));
          }
        }
      }
      continue;
    }

    // If we are guaranteed, but are not a guaranteed forwarding inst,
    // just continue. This user is just treated as a normal use.
    if (!isGuaranteedForwardingInst(user))
      continue;

    // At this point, we know that we must have a forwarded subobject. Since the
    // base type is guaranteed, we know that the subobject is either guaranteed
    // or trivial. We now split into two cases, if the user is a terminator or
    // not. If we do not have a terminator, then just add the uses of all of
    // User's results to the worklist.
    if (user->getResults().size()) {
      for (SILValue result : user->getResults()) {
        if (result.getOwnershipKind() == ValueOwnershipKind::Trivial) {
          continue;
        }

        // Now, we /must/ have a guaranteed subobject, so let's assert that the
        // user is actually guaranteed and add the subobject's users to our
        // worklist.
        assert(result.getOwnershipKind() == ValueOwnershipKind::Guaranteed &&
               "Our value is guaranteed and this is a forwarding instruction. "
               "Should have guaranteed ownership as well.");
        copy(result->getUses(), std::back_inserter(users));
      }

      continue;
    }

    assert(user->getResults().empty());

    auto *ti = dyn_cast<TermInst>(user);
    if (!ti) {
      continue;
    }

    // Otherwise if we have a terminator, add any as uses any end_borrow to
    // ensure that the subscope is completely enclsed within the super scope. We
    // require all of our arguments to be either trivial or guaranteed.
    for (auto &succ : ti->getSuccessors()) {
      auto *succBlock = succ.getBB();

      // If we do not have any arguments, then continue.
      if (succBlock->args_empty())
        continue;

      // Otherwise, make sure that all arguments are trivial or guaranteed. If
      // we fail, emit an error.
      //
      // TODO: We could ignore this error and emit a more specific error on the
      // actual terminator.
      for (auto *succArg : succBlock->getPhiArguments()) {
        // *NOTE* We do not emit an error here since we want to allow for more
        // specific errors to be found during use_verification.
        //
        // TODO: Add a flag that associates the terminator instruction with
        // needing to be verified. If it isn't verified appropriately, assert
        // when the verifier is destroyed.
        auto succArgOwnershipKind = succArg->getOwnershipKind();
        if (!succArgOwnershipKind.isTrivialOrCompatibleWith(ownershipKind)) {
          // This is where the error would go.
          continue;
        }

        // If we have a trivial value, just continue.
        if (succArgOwnershipKind == ValueOwnershipKind::Trivial)
          continue;

        // Otherwise add all end_borrow users for this BBArg to the
        // implicit regular user list. We know that BBArg must be
        // completely joint post-dominated by these users, so we use
        // them to ensure that all of BBArg's uses are completely
        // enclosed within the end_borrow of this argument.
        for (auto *op : succArg->getUses()) {
          if (auto *ebi = dyn_cast<EndBorrowInst>(op->getUser())) {
            implicitRegularUsers.push_back(ebi);
          }
        }
      }
    }
  }

  // Return true if we did not have an error and false if we did find an error.
  //
  // The reason why we use this extra variable is to make sure that when we are
  // testing, we print out all mismatching pairs rather than just the first.
  return !foundError;
}

bool SILValueOwnershipChecker::checkFunctionArgWithoutLifetimeEndingUses(
    SILFunctionArgument *arg) {
  switch (arg->getOwnershipKind()) {
  case ValueOwnershipKind::Guaranteed:
  case ValueOwnershipKind::Unowned:
  case ValueOwnershipKind::Trivial:
    return true;
  case ValueOwnershipKind::Any:
    llvm_unreachable(
        "Function arguments should never have ValueOwnershipKind::Any");
  case ValueOwnershipKind::Owned:
    break;
  }

  if (deadEndBlocks.isDeadEnd(arg->getParent()))
    return true;

  return !handleError([&] {
    llvm::errs() << "Function: '" << arg->getFunction()->getName() << "'\n"
                 << "    Owned function parameter without life ending uses!\n"
                 << "Value: " << *arg << '\n';
  });
}

bool SILValueOwnershipChecker::checkYieldWithoutLifetimeEndingUses(
    BeginApplyResult *yield) {
  switch (yield->getOwnershipKind()) {
  case ValueOwnershipKind::Guaranteed:
  case ValueOwnershipKind::Unowned:
  case ValueOwnershipKind::Trivial:
    return true;
  case ValueOwnershipKind::Any:
    llvm_unreachable("Yields should never have ValueOwnershipKind::Any");
  case ValueOwnershipKind::Owned:
    break;
  }

  if (deadEndBlocks.isDeadEnd(yield->getParent()->getParent()))
    return true;

  return !handleError([&] {
    llvm::errs() << "Function: '" << yield->getFunction()->getName() << "'\n"
                 << "    Owned yield without life ending uses!\n"
                 << "Value: " << *yield << '\n';
  });
}
bool SILValueOwnershipChecker::checkValueWithoutLifetimeEndingUses() {
  LLVM_DEBUG(llvm::dbgs() << "    No lifetime ending users?! Bailing early.\n");
  if (auto *arg = dyn_cast<SILFunctionArgument>(value)) {
    if (checkFunctionArgWithoutLifetimeEndingUses(arg)) {
      return true;
    }
  }

  if (auto *yield = dyn_cast<BeginApplyResult>(value)) {
    if (checkYieldWithoutLifetimeEndingUses(yield)) {
      return true;
    }
  }

  // Check if we are a guaranteed subobject. In such a case, we should never
  // have lifetime ending uses, since our lifetime is guaranteed by our
  // operand, so there is nothing further to do. So just return true.
  if (isGuaranteedForwardingValue(value) &&
      value.getOwnershipKind() == ValueOwnershipKind::Guaranteed)
    return true;

  // If we have an unowned value, then again there is nothing left to do.
  if (value.getOwnershipKind() == ValueOwnershipKind::Unowned)
    return true;

  if (auto *parentBlock = value->getParentBlock()) {
    if (deadEndBlocks.isDeadEnd(parentBlock)) {
      LLVM_DEBUG(llvm::dbgs() << "    Ignoring transitively unreachable value "
                              << "without users!\n"
                              << "    Function: '"
                              << value->getFunction()->getName() << "'\n"
                              << "    Value: " << *value << '\n');
      return true;
    }
  }

  if (!isValueAddressOrTrivial(value, mod)) {
    return !handleError([&] {
      llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n";
      if (value.getOwnershipKind() == ValueOwnershipKind::Owned) {
        llvm::errs() << "Error! Found a leaked owned value that was never "
                        "consumed.\n";
      } else {
        llvm::errs() << "Non trivial values, non address values, and non "
                        "guaranteed function args must have at least one "
                        "lifetime ending use?!\n";
      }
      llvm::errs() << "Value: " << *value << '\n';
    });
  }

  return true;
}

bool SILValueOwnershipChecker::isGuaranteedFunctionArgWithLifetimeEndingUses(
    SILFunctionArgument *arg,
    const llvm::SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers)
    const {
  if (arg->getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return true;

  return handleError([&] {
    llvm::errs() << "    Function: '" << arg->getFunction()->getName() << "'\n"
                 << "    Guaranteed function parameter with life ending uses!\n"
                 << "    Value: " << *arg;
    for (const auto &user : lifetimeEndingUsers) {
      llvm::errs() << "    Lifetime Ending User: " << *user;
    }
    llvm::errs() << '\n';
  });
}

bool SILValueOwnershipChecker::isSubobjectProjectionWithLifetimeEndingUses(
    SILValue value,
    const llvm::SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers)
    const {
  return handleError([&] {
    llvm::errs() << "    Function: '" << value->getFunction()->getName()
                 << "'\n"
                 << "    Subobject projection with life ending uses!\n"
                 << "    Value: " << *value;
    for (const auto &user : lifetimeEndingUsers) {
      llvm::errs() << "    Lifetime Ending User: " << *user;
    }
    llvm::errs() << '\n';
  });
}

bool SILValueOwnershipChecker::checkUses() {
  LLVM_DEBUG(llvm::dbgs() << "    Gathering and classifying uses!\n");

  // First go through V and gather up its uses. While we do this we:
  //
  // 1. Verify that none of the uses are in the same block. This would be an
  // overconsume so in this case we assert.
  // 2. Verify that the uses are compatible with our ownership convention.
  if (!gatherUsers(lifetimeEndingUsers, regularUsers, implicitRegularUsers)) {
    // Silently return false if this fails.
    //
    // If the user pass in a ErrorBehaviorKind that will assert, we
    // will have asserted in gatherUsers(). If we get here the user
    // asked us to optionally print out a message and indicate that
    // the verification failed.
    return false;
  }

  // We can only have no lifetime ending uses if we have:
  //
  // 1. A trivial typed value.
  // 2. An address type value.
  // 3. A guaranteed function argument.
  //
  // In the first two cases, it is easy to see that there is nothing further to
  // do but return false.
  //
  // In the case of a function argument, one must think about the issues a bit
  // more. Specifically, we should have /no/ lifetime ending uses of a
  // guaranteed function argument, since a guaranteed function argument should
  // outlive the current function always.
  if (lifetimeEndingUsers.empty() && checkValueWithoutLifetimeEndingUses()) {
    return false;
  }

  LLVM_DEBUG(llvm::dbgs() << "    Found lifetime ending users! Performing "
                             "initial checks\n");

  // See if we have a guaranteed function address. Guaranteed function addresses
  // should never have any lifetime ending uses.
  if (auto *arg = dyn_cast<SILFunctionArgument>(value)) {
    if (!isGuaranteedFunctionArgWithLifetimeEndingUses(arg,
                                                       lifetimeEndingUsers)) {
      return false;
    }
  }

  // Check if we are an instruction that forwards forwards guaranteed
  // ownership. In such a case, we are a subobject projection. We should not
  // have any lifetime ending uses.
  if (isGuaranteedForwardingValue(value) &&
      value.getOwnershipKind() == ValueOwnershipKind::Guaranteed) {
    if (!isSubobjectProjectionWithLifetimeEndingUses(value,
                                                     lifetimeEndingUsers)) {
      return false;
    }
  }

  return true;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

OperandOwnershipKindMap
Operand::getOwnershipKindMap(bool isForwardingSubValue) const {
  OperandOwnershipKindClassifier classifier(getUser()->getModule(), *this,
                                            ErrorBehaviorKind::ReturnFalse,
                                            isForwardingSubValue);
  return classifier.visit(const_cast<SILInstruction *>(getUser()));
}

void SILInstruction::verifyOperandOwnership() const {
#ifndef NDEBUG
  if (DisableOwnershipVerification)
    return;

  if (isStaticInitializerInst())
    return;

  // If SILOwnership is not enabled, do not perform verification.
  if (!getModule().getOptions().EnableSILOwnership)
    return;

  // If the given function has unqualified ownership or we have been asked by
  // the user not to verify this function, there is nothing to verify.
  if (!getFunction()->hasQualifiedOwnership() ||
      !getFunction()->shouldVerifyOwnership())
    return;

  // If we are testing the verifier, bail so we only print errors once when
  // performing a full verification, instead of additionally in the SILBuilder.
  if (IsSILOwnershipVerifierTestingEnabled)
    return;

  // If this is a terminator instruction, do not verify in SILBuilder. This is
  // because when building a new function, one must create the destination block
  // first which is an unnatural pattern and pretty brittle.
  if (isa<TermInst>(this))
    return;

  ErrorBehaviorKind errorBehavior;
  if (IsSILOwnershipVerifierTestingEnabled) {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndReturnFalse;
  } else {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndAssert;
  }
  for (const Operand &op : getAllOperands()) {
    // Skip type dependence operands.
    if (isTypeDependentOperand(op))
      continue;
    SILValue opValue = op.get();

    // Skip any SILUndef that we see.
    if (isa<SILUndef>(opValue))
      continue;
    auto operandOwnershipKindMap = op.getOwnershipKindMap();
    auto valueOwnershipKind = opValue.getOwnershipKind();
    if (operandOwnershipKindMap.canAcceptKind(valueOwnershipKind))
      continue;

    if (errorBehavior.shouldPrintMessage()) {
      llvm::errs() << "Found an operand with a value that is not compatible "
                      "with the operand's operand ownership kind map.\n";
      llvm::errs() << "Value: " << opValue;
      llvm::errs() << "Value Ownership Kind: " << valueOwnershipKind << "\n";
      llvm::errs() << "Instruction: " << *this;
      llvm::errs() << "Operand Ownership Kind Map: " << operandOwnershipKindMap;
    }

    if (errorBehavior.shouldReturnFalse())
      continue;

    assert(errorBehavior.shouldAssert() &&
           "At this point, we are expected to assert");
    llvm_unreachable("triggering standard assertion failure routine");
  }
#endif
}

void SILValue::verifyOwnership(SILModule &mod,
                               DeadEndBlocks *deadEndBlocks) const {
#ifndef NDEBUG
  if (DisableOwnershipVerification)
    return;

  // If we are SILUndef, just bail. SILUndef can pair with anything. Any uses of
  // the SILUndef will make sure that the matching checks out.
  if (isa<SILUndef>(*this))
    return;

  // Since we do not have SILUndef, we now know that getFunction() should return
  // a real function. Assert in case this assumption is no longer true.
  SILFunction *f = (*this)->getFunction();
  assert(f && "Instructions and arguments should have a function");

  // If the given function has unqualified ownership or we have been asked by
  // the user not to verify this function, there is nothing to verify.
  if (!f->hasQualifiedOwnership() || !f->shouldVerifyOwnership())
    return;

  ErrorBehaviorKind errorBehavior;
  if (IsSILOwnershipVerifierTestingEnabled) {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndReturnFalse;
  } else {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndAssert;
  }

  llvm::SmallPtrSet<SILBasicBlock *, 32> liveBlocks;
  if (deadEndBlocks) {
    SILValueOwnershipChecker(mod, *deadEndBlocks, *this, errorBehavior,
                             liveBlocks)
        .check();
  } else {
    DeadEndBlocks deadEndBlocks(f);
    SILValueOwnershipChecker(mod, deadEndBlocks, *this, errorBehavior,
                             liveBlocks)
        .check();
  }
#endif
}

bool OwnershipChecker::checkValue(SILValue value) {
  regularUsers.clear();
  lifetimeEndingUsers.clear();
  liveBlocks.clear();

  // If we are SILUndef, just bail. SILUndef can pair with anything. Any uses of
  // the SILUndef will make sure that the matching checks out.
  if (isa<SILUndef>(value))
    return false;

  // Since we do not have SILUndef, we now know that getFunction() should return
  // a real function. Assert in case this assumption is no longer true.
  SILFunction *f = value->getFunction();
  assert(f && "Instructions and arguments should have a function");

  // If the given function has unqualified ownership, there is nothing further
  // to verify.
  if (!f->hasQualifiedOwnership())
    return false;

  ErrorBehaviorKind errorBehavior(ErrorBehaviorKind::ReturnFalse);
  SILValueOwnershipChecker checker(mod, deadEndBlocks, value, errorBehavior,
                                   liveBlocks);
  if (!checker.check()) {
    return false;
  }

  // TODO: Make this more efficient.
  copy(checker.getRegularUsers(), std::back_inserter(regularUsers));
  copy(checker.getLifetimeEndingUsers(),
       std::back_inserter(lifetimeEndingUsers));
  return true;
}
