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

#include "TransitivelyUnreachableBlocks.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/DynamicCasts.h"
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

// The verifier is basically all assertions, so don't compile it with NDEBUG to
// prevent release builds from triggering spurious unused variable warnings.
#ifndef NDEBUG

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

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool compatibleOwnershipKinds(ValueOwnershipKind K1,
                                     ValueOwnershipKind K2) {
  return K1.merge(K2).hasValue();
}

static bool isValueAddressOrTrivial(SILValue V, SILModule &M) {
  return V->getType().isAddress() ||
         V.getOwnershipKind() == ValueOwnershipKind::Trivial;
}

static bool isOwnershipForwardingValueKind(ValueKind K) {
  switch (K) {
  case ValueKind::TupleInst:
  case ValueKind::StructInst:
  case ValueKind::EnumInst:
  case ValueKind::OpenExistentialRefInst:
  case ValueKind::UpcastInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::RefToBridgeObjectInst:
  case ValueKind::BridgeObjectToRefInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::TupleExtractInst:
  case ValueKind::StructExtractInst:
  case ValueKind::UncheckedEnumDataInst:
  case ValueKind::MarkUninitializedInst:
  case ValueKind::SelectEnumInst:
    return true;
  default:
    return false;
  }
}

static bool isOwnershipForwardingValue(SILValue V) {
  return isOwnershipForwardingValueKind(V->getKind());
}

static bool isOwnershipForwardingInst(SILInstruction *I) {
  return isOwnershipForwardingValueKind(I->getKind());
}

//===----------------------------------------------------------------------===//
//                      OwnershipCompatibilityUseChecker
//===----------------------------------------------------------------------===//

namespace {

struct OwnershipUseCheckerResult {
  bool HasCompatibleOwnership;
  bool ShouldCheckForDataflowViolations;
};

class OwnershipCompatibilityUseChecker
    : public SILInstructionVisitor<OwnershipCompatibilityUseChecker,
                                   OwnershipUseCheckerResult> {
  SILModule &Mod;
  const Operand &Op;
  SILValue BaseValue;

public:
  /// Create a new OwnershipCompatibilityUseChecker.
  ///
  /// In most cases, one should only pass in \p Op and \p BaseValue will be set
  /// to Op.get(). In cases where one is trying to verify subobjects, Op.get()
  /// should be the subobject and Value should be the parent object. An example
  /// of where one would want to do this is in the case of value projections
  /// like struct_extract.
  OwnershipCompatibilityUseChecker(SILModule &M, const Operand &Op,
                                   SILValue BaseValue)
      : Mod(M), Op(Op), BaseValue(BaseValue) {
    assert((BaseValue == Op.get() ||
            BaseValue.getOwnershipKind() == ValueOwnershipKind::Guaranteed) &&
           "Guaranteed values are the only values allowed to have subobject");
    // We only support subobjects on objects.
    assert((BaseValue->getType().isObject() || !isCheckingSubObject()) &&
           "Checking a subobject, but do not have an object base value?!");
  }

  bool isCheckingSubObject() const { return Op.get() != BaseValue; }

  SILValue getValue() const { return Op.get(); }

  ValueOwnershipKind getOwnershipKind() const {
    assert(getValue().getOwnershipKind() == Op.get().getOwnershipKind() &&
           "Expected ownership kind of parent value and operand");
    return getValue().getOwnershipKind();
  }

  unsigned getOperandIndex() const { return Op.getOperandNumber(); }

  SILType getType() const { return Op.get()->getType(); }

  bool compatibleWithOwnership(ValueOwnershipKind Kind) const {
    return compatibleOwnershipKinds(getOwnershipKind(), Kind);
  }

  bool isAddressOrTrivialType() const {
    if (getType().isAddress())
      return true;
    return getOwnershipKind() == ValueOwnershipKind::Trivial;
  }

  OwnershipUseCheckerResult visitForwardingInst(SILInstruction *I,
                                                ArrayRef<Operand> Ops);
  OwnershipUseCheckerResult visitForwardingInst(SILInstruction *I) {
    return visitForwardingInst(I, I->getAllOperands());
  }
  OwnershipUseCheckerResult
  visitApplyArgument(ValueOwnershipKind RequiredConvention, bool ShouldCheck);
  OwnershipUseCheckerResult
  visitNonTrivialEnum(EnumDecl *E, ValueOwnershipKind RequiredConvention);

  /// Check if \p User as compatible ownership with the SILValue that we are
  /// checking.
  ///
  /// \returns true if the user is a use that must be checked for dataflow
  /// violations.
  bool check(SILInstruction *User) {
    auto Result = visit(User);
    if (!Result.HasCompatibleOwnership) {
      llvm::errs() << "Function: '" << User->getFunction()->getName() << "'\n"
                   << "Have operand with incompatible ownership?!\n"
                   << "Value: " << *getValue() << "BaseValue: " << *BaseValue
                   << "User: " << *User << "Conv: " << getOwnershipKind()
                   << "\n\n";
      if (IsSILOwnershipVerifierTestingEnabled)
        return false;
      llvm_unreachable("triggering standard assertion failure routine");
    }

    assert((!Result.ShouldCheckForDataflowViolations ||
            !isAddressOrTrivialType()) &&
           "Address or trivial types should never be checked for dataflow "
           "violations");

    return Result.ShouldCheckForDataflowViolations;
  }

  OwnershipUseCheckerResult visitValueBase(ValueBase *) {
    llvm_unreachable("Unimplemented?!");
  }

  OwnershipUseCheckerResult visitCallee(CanSILFunctionType SubstCalleeType);
  OwnershipUseCheckerResult
  checkTerminatorArgumentMatchesDestBB(SILBasicBlock *DestBB, unsigned OpIndex);

// Create declarations for all instructions, so we get a warning at compile
// time if any instructions do not have an implementation.
#define INST(Id, Parent, TextualName, MemBehavior, MayRelease)                 \
  OwnershipUseCheckerResult visit##Id(Id *);
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

/// Implementation for instructions without operands. These should never be
/// visited.
#define NO_OPERAND_INST(INST)                                                  \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    assert(I->getNumOperands() == 0 &&                                         \
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
NO_OPERAND_INST(IntegerLiteral)
NO_OPERAND_INST(Metatype)
NO_OPERAND_INST(ObjCProtocol)
NO_OPERAND_INST(RetainValue)
NO_OPERAND_INST(StringLiteral)
NO_OPERAND_INST(StrongRetain)
NO_OPERAND_INST(StrongRetainUnowned)
NO_OPERAND_INST(UnownedRetain)
NO_OPERAND_INST(Unreachable)
#undef NO_OPERAND_INST

/// Instructions whose arguments are always compatible with one convention.
#define CONSTANT_OWNERSHIP_INST(OWNERSHIP,                                     \
                                SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS, INST)    \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
    if (ValueOwnershipKind::OWNERSHIP == ValueOwnershipKind::Trivial) {        \
      assert(isAddressOrTrivialType() &&                                       \
             "Trivial ownership requires a trivial type or an address");       \
    }                                                                          \
                                                                               \
    return {compatibleWithOwnership(ValueOwnershipKind::OWNERSHIP),            \
            SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS};                             \
  }
CONSTANT_OWNERSHIP_INST(Guaranteed, true, EndBorrowArgument)
CONSTANT_OWNERSHIP_INST(Guaranteed, false, RefElementAddr)
CONSTANT_OWNERSHIP_INST(Owned, true, AutoreleaseValue)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocBox)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocPartialRef)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocRef)
CONSTANT_OWNERSHIP_INST(Owned, true, DestroyValue)
CONSTANT_OWNERSHIP_INST(Owned, true, ReleaseValue)
CONSTANT_OWNERSHIP_INST(Owned, true, StrongRelease)
CONSTANT_OWNERSHIP_INST(Owned, true, StrongUnpin)
CONSTANT_OWNERSHIP_INST(Owned, true, UnownedRelease)
CONSTANT_OWNERSHIP_INST(Owned, true, InitExistentialRef)
CONSTANT_OWNERSHIP_INST(Trivial, false, AddressToPointer)
CONSTANT_OWNERSHIP_INST(Trivial, false, BindMemory)
CONSTANT_OWNERSHIP_INST(Trivial, false, CheckedCastAddrBranch)
CONSTANT_OWNERSHIP_INST(Trivial, false, CondFail)
CONSTANT_OWNERSHIP_INST(Trivial, false, CopyAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, DeallocStack)
CONSTANT_OWNERSHIP_INST(Trivial, false, DebugValueAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, DeinitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, DestroyAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, IndexAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, IndexRawPointer)
CONSTANT_OWNERSHIP_INST(Trivial, false, InitBlockStorageHeader)
CONSTANT_OWNERSHIP_INST(Trivial, false, InitEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, InitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, InitExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, false, InjectEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, IsNonnull)
CONSTANT_OWNERSHIP_INST(Trivial, false, IsUnique)
CONSTANT_OWNERSHIP_INST(Trivial, false, IsUniqueOrPinned)
CONSTANT_OWNERSHIP_INST(Trivial, false, Load)
CONSTANT_OWNERSHIP_INST(Trivial, false, LoadBorrow)
CONSTANT_OWNERSHIP_INST(Trivial, false, LoadUnowned)
CONSTANT_OWNERSHIP_INST(Trivial, false, LoadWeak)
CONSTANT_OWNERSHIP_INST(Trivial, false, MarkFunctionEscape)
CONSTANT_OWNERSHIP_INST(Trivial, false, MarkUninitializedBehavior)
CONSTANT_OWNERSHIP_INST(Trivial, false, ObjCExistentialMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Trivial, false, ObjCMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Trivial, false, ObjCToThickMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, false, OpenExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, OpenExistentialOpaque)
CONSTANT_OWNERSHIP_INST(Trivial, false, OpenExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, false, PointerToAddress)
CONSTANT_OWNERSHIP_INST(Trivial, false, PointerToThinFunction)
CONSTANT_OWNERSHIP_INST(Trivial, false, ProjectBlockStorage)
CONSTANT_OWNERSHIP_INST(Trivial, false, ProjectValueBuffer)
CONSTANT_OWNERSHIP_INST(Trivial, false, RawPointerToRef)
CONSTANT_OWNERSHIP_INST(Trivial, false, SelectEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, SelectValue)
CONSTANT_OWNERSHIP_INST(Trivial, false, StructElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, SwitchEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, SwitchValue)
CONSTANT_OWNERSHIP_INST(Trivial, false, TailAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, ThickToObjCMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, false, ThinFunctionToPointer)
CONSTANT_OWNERSHIP_INST(Trivial, false, ThinToThickFunction)
CONSTANT_OWNERSHIP_INST(Trivial, false, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(Trivial, false, UncheckedRefCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, UnconditionalCheckedCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, UnmanagedToRef)
CONSTANT_OWNERSHIP_INST(Trivial, false, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(Trivial, false, DeallocValueBuffer)
#undef CONSTANT_OWNERSHIP_INST

/// Instructions whose arguments are always compatible with one convention.
#define CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(OWNERSHIP,                                     \
                                SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS, INST)    \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
    if (ValueOwnershipKind::OWNERSHIP == ValueOwnershipKind::Trivial) {        \
      assert(isAddressOrTrivialType() &&                                       \
             "Trivial ownership requires a trivial type or an address");       \
    }                                                                          \
                                                                        \
    if (compatibleWithOwnership(ValueOwnershipKind::Trivial)) {         \
      return {true, false};                                             \
    }                                                                   \
    return {compatibleWithOwnership(ValueOwnershipKind::OWNERSHIP),            \
            SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS};                             \
  }
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, true, CheckedCastBranch)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, true, SwitchEnum)
#undef CONSTANT_OR_TRIVIAL_OWNERSHIP_INST

#define ACCEPTS_ANY_OWNERSHIP_INST(INST)                                       \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    return {true, false};                                                      \
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
#undef ACCEPTS_ANY_OWNERSHIP_INST

// Trivial if trivial typed, otherwise must accept owned?
#define ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS, \
                                         INST)                                 \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
    assert(!isAddressOrTrivialType() &&                                        \
           "Shouldn't have an address or a non trivial type");                 \
    bool compatible = getOwnershipKind() == ValueOwnershipKind::Any ||         \
                      !compatibleWithOwnership(ValueOwnershipKind::Trivial);   \
    return {compatible, SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS};                 \
  }
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, SuperMethod)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, BridgeObjectToWord)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, ClassMethod)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, CopyBlock)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, DynamicMethod)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, OpenExistentialBox)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefTailAddr)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefToRawPointer)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefToUnmanaged)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefToUnowned)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, SetDeallocating)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, StrongPin)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, UnownedToRef)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, CopyUnownedValue)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, ProjectExistentialBox)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, UnmanagedRetainValue)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, UnmanagedReleaseValue)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, UnmanagedAutoreleaseValue)
#undef ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitForwardingInst(SILInstruction *I, ArrayRef<Operand> Ops) {
  assert(I->getNumOperands() && "Expected to have non-zero operands");
  assert(isOwnershipForwardingInst(I) &&
         "Expected to have an ownership forwarding inst");

  // Find the first index where we have a trivial value.
  auto Iter = find_if(Ops, [&I](const Operand &Op) -> bool {
    if (I->isTypeDependentOperand(Op))
      return false;
    return Op.get().getOwnershipKind() != ValueOwnershipKind::Trivial;
  });

  // All trivial.
  if (Iter == Ops.end()) {
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  }

  unsigned Index = std::distance(Ops.begin(), Iter);
  ValueOwnershipKind Base = Ops[Index].get().getOwnershipKind();

  for (const Operand &Op : Ops.slice(Index + 1)) {
    if (I->isTypeDependentOperand(Op))
      continue;
    auto OpKind = Op.get().getOwnershipKind();
    if (OpKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto MergedValue = Base.merge(OpKind.Value);
    if (!MergedValue.hasValue()) {
      return {false, true};
    }
    Base = MergedValue.getValue();
  }

  // We only need to treat a forwarded instruction as a lifetime ending use of
  // it is owned.
  return {true, compatibleWithOwnership(ValueOwnershipKind::Owned)};
}

#define FORWARD_ANY_OWNERSHIP_INST(INST)                                       \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    return visitForwardingInst(I);                                             \
  }
FORWARD_ANY_OWNERSHIP_INST(Tuple)
FORWARD_ANY_OWNERSHIP_INST(Struct)
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
#undef FORWARD_ANY_OWNERSHIP_INST

// An instruction that forwards a constant ownership or trivial ownership.
#define FORWARD_CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(                            \
    OWNERSHIP, SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS, INST)                     \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
    assert(isOwnershipForwardingInst(I) &&                                     \
           "Expected an ownership forwarding inst");                           \
    if (ValueOwnershipKind::OWNERSHIP != ValueOwnershipKind::Trivial &&        \
        getOwnershipKind() == ValueOwnershipKind::Trivial) {                   \
      assert(isAddressOrTrivialType() &&                                       \
             "Trivial ownership requires a trivial type or an address");       \
      return {true, false};                                                    \
    }                                                                          \
    if (ValueOwnershipKind::OWNERSHIP == ValueOwnershipKind::Trivial) {        \
      assert(isAddressOrTrivialType() &&                                       \
             "Trivial ownership requires a trivial type or an address");       \
    }                                                                          \
                                                                               \
    return {compatibleWithOwnership(ValueOwnershipKind::OWNERSHIP),            \
            SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS};                             \
  }
FORWARD_CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, false, TupleExtract)
FORWARD_CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, false, StructExtract)
#undef CONSTANT_OR_TRIVIAL_OWNERSHIP_INST

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitSelectEnumInst(SelectEnumInst *I) {
  if (getValue() == I->getEnumOperand()) {
    return {true, false};
  }

  return visitForwardingInst(I, I->getAllOperands().drop_front());
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitAllocRefInst(AllocRefInst *I) {
  assert(I->getNumOperands() != 0
         && "If we reach this point, we must have a tail operand");
  return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitAllocRefDynamicInst(
    AllocRefDynamicInst *I) {
  assert(I->getNumOperands() != 0 &&
         "If we reach this point, we must have a tail operand");
  return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::checkTerminatorArgumentMatchesDestBB(
    SILBasicBlock *DestBB, unsigned OpIndex) {
  // Grab the ownership kind of the destination block.
  ValueOwnershipKind DestBlockArgOwnershipKind =
      DestBB->getArgument(OpIndex)->getOwnershipKind();

  // Then if we do not have an enum, make sure that the conventions match.
  EnumDecl *E = getType().getEnumOrBoundGenericEnum();
  if (!E) {
    return {compatibleWithOwnership(DestBlockArgOwnershipKind),
            getOwnershipKind() == ValueOwnershipKind::Owned};
  }

  return visitNonTrivialEnum(E, DestBlockArgOwnershipKind);
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitBranchInst(BranchInst *BI) {
  return checkTerminatorArgumentMatchesDestBB(BI->getDestBB(),
                                              getOperandIndex());
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitCondBranchInst(CondBranchInst *CBI) {
  // If our conditional branch is the condition, it is trivial. Check that the
  // ownership kind is trivial.
  if (CBI->isConditionOperandIndex(getOperandIndex()))
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};

  // Otherwise, make sure that our operand matches the
  if (CBI->isTrueOperandIndex(getOperandIndex())) {
    unsigned TrueOffset = 1;
    return checkTerminatorArgumentMatchesDestBB(CBI->getTrueBB(),
                                                getOperandIndex() - TrueOffset);
  }

  assert(CBI->isFalseOperandIndex(getOperandIndex()) &&
         "If an operand is not the condition index or a true operand index, it "
         "must be a false operand index");
  unsigned FalseOffset = 1 + CBI->getTrueOperands().size();
  return checkTerminatorArgumentMatchesDestBB(CBI->getFalseBB(),
                                              getOperandIndex() - FalseOffset);
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitReturnInst(ReturnInst *RI) {
  SILModule &M = RI->getModule();
  bool IsTrivial = RI->getOperand()->getType().isTrivial(M);
  SILFunctionConventions fnConv = RI->getFunction()->getConventions();
  auto Results = fnConv.getDirectSILResults();
  if (Results.empty() || IsTrivial) {
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  }

  CanGenericSignature Sig = fnConv.funcTy->getGenericSignature();

  // Find the first index where we have a trivial value.
  auto Iter = find_if(Results, [&M, &Sig](const SILResultInfo &Info) -> bool {
    return Info.getOwnershipKind(M, Sig) != ValueOwnershipKind::Trivial;
  });

  // If we have all trivial, then we must be trivial. Why wasn't our original
  // type trivial? This is a hard error since this is a logic error in our code
  // here.
  if (Iter == Results.end())
    llvm_unreachable("Should have already checked a trivial type?!");

  ValueOwnershipKind Base = Iter->getOwnershipKind(M, Sig);

  for (const SILResultInfo &ResultInfo :
       SILFunctionConventions::DirectSILResultRange(std::next(Iter),
                                                    Results.end())) {
    auto RKind = ResultInfo.getOwnershipKind(M, Sig);
    // Ignore trivial types.
    if (RKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto MergedValue = Base.merge(RKind);
    // If we fail to merge all types in, bail. We can not come up with a proper
    // result type.
    if (!MergedValue.hasValue()) {
      return {false, false};
    }
    // In case Base is Any.
    Base = MergedValue.getValue();
  }

  return {compatibleWithOwnership(Base), true};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitEndBorrowInst(EndBorrowInst *I) {
  // We do not consider the original value to be a verified use. But the value
  // does need to be alive.
  if (getOperandIndex() == EndBorrowInst::OriginalValue)
    return {true, false};
  // The borrowed value is a verified use though of the begin_borrow.
  return {compatibleWithOwnership(ValueOwnershipKind::Guaranteed), true};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitThrowInst(ThrowInst *I) {
  return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitStoreUnownedInst(StoreUnownedInst *I) {
  if (getValue() == I->getSrc())
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitStoreWeakInst(StoreWeakInst *I) {
  if (getValue() == I->getSrc())
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitStoreBorrowInst(StoreBorrowInst *I) {
  if (getValue() == I->getSrc())
    return {compatibleWithOwnership(ValueOwnershipKind::Guaranteed), false};
  return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
}

// FIXME: Why not use SILArgumentConvention here?
OwnershipUseCheckerResult OwnershipCompatibilityUseChecker::visitCallee(
    CanSILFunctionType SubstCalleeType) {
  ParameterConvention Conv = SubstCalleeType->getCalleeConvention();
  switch (Conv) {
  case ParameterConvention::Indirect_In:
    assert(!SILModuleConventions(Mod).isSILIndirect(
        SILParameterInfo(SubstCalleeType, Conv)));
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  case ParameterConvention::Indirect_In_Guaranteed:
    assert(!SILModuleConventions(Mod).isSILIndirect(
        SILParameterInfo(SubstCalleeType, Conv)));
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), false};
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("Illegal convention for callee");
  case ParameterConvention::Direct_Unowned:
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  case ParameterConvention::Direct_Owned:
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  case ParameterConvention::Direct_Guaranteed:
    return {compatibleWithOwnership(ValueOwnershipKind::Guaranteed), false};
  }

  llvm_unreachable("Unhandled ParameterConvention in switch.");
}

OwnershipUseCheckerResult OwnershipCompatibilityUseChecker::visitNonTrivialEnum(
    EnumDecl *E, ValueOwnershipKind RequiredKind) {
  // Otherwise, first see if the enum is completely trivial. In such a case, we
  // need an argument with a trivial convention. If we have an enum with at
  // least 1 non-trivial case, then we need an argument with a non-trivial
  // convention. If our parameter is trivial, then we just let it through in
  // such a case. Otherwise we need to make sure that the non-trivial ownership
  // convention matches the one on the argument parameter.

  // Check if this enum has at least one case that is non-trivially typed.
  bool HasNonTrivialCase =
      llvm::any_of(E->getAllElements(), [this](EnumElementDecl *E) -> bool {
        if (!E->getArgumentInterfaceType())
          return false;
        SILType EnumEltType = getType().getEnumElementType(E, Mod);
        return !EnumEltType.isTrivial(Mod);
      });

  // If we have all trivial cases, make sure we are compatible with a trivial
  // ownership kind.
  if (!HasNonTrivialCase) {
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  }

  // Otherwise, if this value is a trivial ownership kind, return.
  if (compatibleWithOwnership(ValueOwnershipKind::Trivial)) {
    return {true, false};
  }

  // And finally finish by making sure that if we have a non-trivial ownership
  // kind that it matches the argument's convention.
  return {compatibleWithOwnership(RequiredKind),
          compatibleWithOwnership(ValueOwnershipKind::Owned)};
}

// We allow for trivial cases of enums with non-trivial cases to be passed in
// non-trivial argument positions. This fits with modeling of a
// SILFunctionArgument as a phi in a global program graph.
OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitApplyArgument(ValueOwnershipKind Kind,
                                                     bool ShouldCheck) {
  // Check if we have an enum. If not, then we just check against the passed inc
  // onvention.
  EnumDecl *E = getType().getEnumOrBoundGenericEnum();
  if (!E) {
    return {compatibleWithOwnership(Kind), ShouldCheck};
  }
  return visitNonTrivialEnum(E, Kind);
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitApplyInst(ApplyInst *I) {
  // If we are visiting the callee, handle it specially.
  if (getOperandIndex() == 0)
    return visitCallee(I->getSubstCalleeType());

  switch (I->getArgumentConvention(getOperandIndex() - 1)) {
  case SILArgumentConvention::Indirect_In:
  case SILArgumentConvention::Indirect_In_Guaranteed:
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Indirect_Out:
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  case SILArgumentConvention::Direct_Owned:
    return visitApplyArgument(ValueOwnershipKind::Owned, true);
  case SILArgumentConvention::Direct_Unowned:
    if (isAddressOrTrivialType())
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    // We accept unowned, owned, and guaranteed in unowned positions.
    return {true, false};
  case SILArgumentConvention::Direct_Guaranteed:
    return visitApplyArgument(ValueOwnershipKind::Guaranteed, false);
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("No ownership associated with deallocating");
  }

  llvm_unreachable("Unhandled SILArgumentConvention in switch.");
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitTryApplyInst(TryApplyInst *I) {
  // If we are visiting the callee, handle it specially.
  if (getOperandIndex() == 0)
    return visitCallee(I->getSubstCalleeType());

  switch (I->getArgumentConvention(getOperandIndex() - 1)) {
  case SILArgumentConvention::Indirect_In:
  case SILArgumentConvention::Indirect_In_Guaranteed:
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Indirect_Out:
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  case SILArgumentConvention::Direct_Owned:
    return visitApplyArgument(ValueOwnershipKind::Owned, true);
  case SILArgumentConvention::Direct_Unowned:
    if (isAddressOrTrivialType())
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    // We accept unowned, owned, and guaranteed in unowned positions.
    return {true, false};
  case SILArgumentConvention::Direct_Guaranteed:
    return visitApplyArgument(ValueOwnershipKind::Guaranteed, false);
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("No ownership associated with deallocating");
  }

  llvm_unreachable("Unhandled SILArgumentConvention in switch.");
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitPartialApplyInst(PartialApplyInst *I) {
  // All non-trivial types should be captured.
  if (isAddressOrTrivialType()) {
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  }
  return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitAssignInst(AssignInst *I) {
  if (getValue() == I->getSrc()) {
    if (isAddressOrTrivialType()) {
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    }
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  }

  return {true, false};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitStoreInst(StoreInst *I) {
  if (getValue() == I->getSrc()) {
    if (isAddressOrTrivialType()) {
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    }
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  }
  return {true, false};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitMarkDependenceInst(
    MarkDependenceInst *MDI) {
  // We always treat mark dependence as a use that keeps a value alive. We will
  // be introducing a begin_dependence/end_dependence version of this later.
  return {true, false};
}

//===----------------------------------------------------------------------===//
//                            Builtin Use Checker
//===----------------------------------------------------------------------===//

namespace {

class OwnershipCompatibilityBuiltinUseChecker
    : public SILBuiltinVisitor<OwnershipCompatibilityBuiltinUseChecker,
                               OwnershipUseCheckerResult> {

  const OwnershipCompatibilityUseChecker &ParentChecker;

public:
  OwnershipCompatibilityBuiltinUseChecker(
      OwnershipCompatibilityUseChecker &ParentChecker)
      : ParentChecker(ParentChecker) {}

  SILValue getValue() const { return ParentChecker.getValue(); }

  ValueOwnershipKind getOwnershipKind() const {
    return ParentChecker.getOwnershipKind();
  }

  unsigned getOperandIndex() const { return ParentChecker.getOperandIndex(); }

  SILType getType() const { return ParentChecker.getType(); }

  bool compatibleWithOwnership(ValueOwnershipKind Kind) const {
    return ParentChecker.compatibleWithOwnership(Kind);
  }

  bool isAddressOrTrivialType() const {
    return ParentChecker.isAddressOrTrivialType();
  }

  OwnershipUseCheckerResult visitLLVMIntrinsic(BuiltinInst *BI,
                                               llvm::Intrinsic::ID ID) {
    // LLVM intrinsics do not traffic in ownership, so if we have a result, it
    // must be trivial.
    return {true, false};
  }

#define BUILTIN(ID, NAME, ATTRS)                                               \
  OwnershipUseCheckerResult visit##ID(BuiltinInst *BI, StringRef Attr);
#include "swift/AST/Builtins.def"

  OwnershipUseCheckerResult check(BuiltinInst *BI) { return visit(BI); }
};

} // end anonymous namespace

// This is correct today since we do not ahve any builtins which return
// @guaranteed parameters. This means that we can only have a lifetime ending
// use with our builtins if it is owned.
#define CONSTANT_OWNERSHIP_BUILTIN(OWNERSHIP, LIFETIME_ENDING_USE, ID)         \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityBuiltinUseChecker::visit##ID(BuiltinInst *BI,      \
                                                         StringRef Attr) {     \
    return {compatibleWithOwnership(ValueOwnershipKind::OWNERSHIP),            \
            LIFETIME_ENDING_USE};                                              \
  }
CONSTANT_OWNERSHIP_BUILTIN(Owned, false, ErrorInMain)
CONSTANT_OWNERSHIP_BUILTIN(Owned, false, UnexpectedError)
CONSTANT_OWNERSHIP_BUILTIN(Owned, false, WillThrow)
CONSTANT_OWNERSHIP_BUILTIN(Owned, true, UnsafeGuaranteed)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, AShr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Add)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Alignof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, AllocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, And)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, AssertConf)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, AssumeNonNegative)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, AtomicLoad)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, AtomicRMW)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, AtomicStore)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, BitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, CanBeObjCClass)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, CmpXChg)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, CondUnreachable)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, CopyArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, DeallocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, DestroyArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ExactSDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ExactUDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ExtractElement)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FAdd)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_OEQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_OGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_OGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_OLE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_OLT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_ONE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_ORD)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_UEQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_UNE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FCMP_UNO)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FMul)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FNeg)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FPExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FPToSI)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FPToUI)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FPTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FRem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, FSub)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Fence)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, GetObjCTypeEncoding)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_EQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_NE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_SGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_SGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_SLE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_SLT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ICMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, InsertElement)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, IntToFPWithOverflow)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, IntToPtr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, IsOptionalType)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, IsPOD)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, LShr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Mul)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, OnFastPath)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Once)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Or)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, PtrToInt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SRem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SSubOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, SUCheckedConversion)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Shl)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Sizeof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, StaticReport)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Strideof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Sub)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, TakeArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, TakeArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Trunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, TruncOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, UAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, UDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, UIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, UMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, URem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, USCheckedConversion)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, USubOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, UToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, UToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Unreachable)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, UnsafeGuaranteedEnd)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, Xor)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ZExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ZExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, false, ZeroInitializer)
#undef CONSTANT_OWNERSHIP_BUILTIN

// Builtins that should be lowered to SIL instructions so we should never see
// them.
#define BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(ID)                 \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityBuiltinUseChecker::visit##ID(BuiltinInst *BI,      \
                                                         StringRef Attr) {     \
    llvm_unreachable("Builtin should have been lowered to SIL instruction?!"); \
  }
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Retain)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Release)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Autorelease)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(TryPin)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Unpin)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Load)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(LoadRaw)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Take)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Destroy)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Assign)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Init)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(CastToUnknownObject)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(CastFromUnknownObject)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(CastToNativeObject)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(CastFromNativeObject)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(CastToBridgeObject)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(
    CastReferenceFromBridgeObject)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(
    CastBitPatternFromBridgeObject)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(BridgeToRawPointer)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(BridgeFromRawPointer)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(CastReference)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(ReinterpretCast)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(AddressOf)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(GepRaw)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(Gep)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(GetTailAddr)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(CondFail)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(FixLifetime)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(IsUnique)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(IsUniqueOrPinned)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(IsUnique_native)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(IsUniqueOrPinned_native)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(BindMemory)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(AllocWithTailElems)
BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS(ProjectTailElems)
#undef BUILTINS_THAT_SHOULD_HAVE_BEEN_LOWERED_TO_SILINSTS

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitBuiltinInst(BuiltinInst *BI) {
  return OwnershipCompatibilityBuiltinUseChecker(*this).check(BI);
}

//===----------------------------------------------------------------------===//
//                         SILValueOwnershipChecker
//===----------------------------------------------------------------------===//

namespace {

class SILValueOwnershipChecker {
  /// The module that we are in.
  SILModule &Mod;

  /// A cache of unreachable function blocks that we use to determine if we can
  /// ignore "leaks".
  const TransitivelyUnreachableBlocksInfo &TUB;

  /// The value whose ownership we will check.
  SILValue Value;

  // The worklist that we will use for our iterative reachability query.
  llvm::SmallVector<SILBasicBlock *, 32> Worklist;

  // The set of blocks with lifetime ending uses.
  llvm::SmallPtrSet<SILBasicBlock *, 8> BlocksWithLifetimeEndingUses;

  // The set of blocks with non-lifetime ending uses and the associated
  // non-lifetime ending use SILInstruction.
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *, 8>
      BlocksWithNonLifetimeEndingUses;

  // The blocks that we have already visited.
  llvm::SmallPtrSet<SILBasicBlock *, 32> VisitedBlocks;

  // A list of successor blocks that we must visit by the time the algorithm
  // terminates.
  llvm::SmallPtrSet<SILBasicBlock *, 8> SuccessorBlocksThatMustBeVisited;

public:
  SILValueOwnershipChecker(SILModule &M,
                           const TransitivelyUnreachableBlocksInfo &TUB,
                           SILValue V)
      : Mod(M), TUB(TUB), Value(V) {}

  ~SILValueOwnershipChecker() = default;
  SILValueOwnershipChecker(SILValueOwnershipChecker &) = delete;
  SILValueOwnershipChecker(SILValueOwnershipChecker &&) = delete;

  void check() {
    DEBUG(llvm::dbgs() << "Verifying ownership of: " << *Value);
    // First check that our uses have coherent ownership. If after evaluating
    // the ownership we do not need to check dataflow (due to performs
    // ValueOwnershipKind::None), then bail.
    if (!checkUses())
      return;
    checkDataflow();
  }

private:
  bool checkUses();
  void checkDataflow();
  void checkDataflowEndConditions();
  void
  gatherUsers(llvm::SmallVectorImpl<SILInstruction *> &LifetimeEndingUsers,
              llvm::SmallVectorImpl<SILInstruction *> &NonLifetimeEndingUsers);
  void uniqueNonLifetimeEndingUsers(
      ArrayRef<SILInstruction *> NonLifetimeEndingUsers);

  /// Returns true if the given block is in the BlocksWithLifetimeEndingUses
  /// set. This is a helper to extract out large logging messages so that the
  /// main logic is easy to read.
  bool doesBlockDoubleConsume(SILBasicBlock *UserBlock,
                              SILInstruction *LifetimeEndingUser = nullptr,
                              bool ShouldInsert = false);

  /// Returns true if the given block contains a non-lifetime ending use that is
  /// strictly later in the block than a lifetime ending use. If all
  /// non-lifetime ending uses are before the lifetime ending use, the block is
  /// removed from the BlocksWithNonLifetimeEndingUses map to show that the uses
  /// were found to properly be post-dominated by a lifetime ending use.
  bool doesBlockContainUseAfterFree(SILInstruction *LifetimeEndingUser,
                                    SILBasicBlock *UserBlock);

  bool checkValueWithoutLifetimeEndingUses();

  bool checkFunctionArgWithoutLifetimeEndingUses(SILFunctionArgument *Arg);
};

} // end anonymous namespace

bool SILValueOwnershipChecker::doesBlockContainUseAfterFree(
    SILInstruction *LifetimeEndingUser, SILBasicBlock *UserBlock) {
  auto Iter = BlocksWithNonLifetimeEndingUses.find(UserBlock);
  if (Iter == BlocksWithNonLifetimeEndingUses.end())
    return false;

  SILInstruction *NonLifetimeEndingUser = Iter->second;
  // Make sure that the non-lifetime ending use is before the lifetime
  // ending use. Otherwise, we have a use after free.
  if (std::find_if(LifetimeEndingUser->getIterator(), UserBlock->end(),
                   [&NonLifetimeEndingUser](const SILInstruction &I) -> bool {
                     return NonLifetimeEndingUser == &I;
                   }) != UserBlock->end()) {
    llvm::errs() << "Function: '" << Value->getFunction()->getName() << "'\n"
                 << "Found use after free?!\n"
                 << "Value: " << *Value
                 << "Consuming User: " << *LifetimeEndingUser
                 << "Non Consuming User: " << *Iter->second << "Block: bb"
                 << UserBlock->getDebugID() << "\n\n";
    return true;
  }

  // Erase the use since we know that it is properly joint post-dominated.
  BlocksWithNonLifetimeEndingUses.erase(Iter);
  return false;
}

bool SILValueOwnershipChecker::doesBlockDoubleConsume(
    SILBasicBlock *UserBlock, SILInstruction *LifetimeEndingUser,
    bool ShouldInsert) {
  if ((ShouldInsert && BlocksWithLifetimeEndingUses.insert(UserBlock).second) ||
      !BlocksWithLifetimeEndingUses.count(UserBlock))
    return false;

  llvm::errs() << "Function: '" << Value->getFunction()->getName() << "'\n"
               << "Found over consume?!\n"
               << "Value: " << *Value;
  if (LifetimeEndingUser)
    llvm::errs() << "User: " << *LifetimeEndingUser;
  llvm::errs() << "Block: bb" << UserBlock->getDebugID() << "\n\n";

  return true;
}

void SILValueOwnershipChecker::gatherUsers(
    llvm::SmallVectorImpl<SILInstruction *> &LifetimeEndingUsers,
    llvm::SmallVectorImpl<SILInstruction *> &NonLifetimeEndingUsers) {

  // See if Value is guaranteed. If we are guaranteed and not forwarding, then
  // we need to look through subobject uses for more uses. Otherwise, if we are
  // forwarding, we do not create any lifetime ending users/non lifetime ending
  // users since we verify against our base.
  bool IsGuaranteed =
      Value.getOwnershipKind() == ValueOwnershipKind::Guaranteed;

  if (IsGuaranteed && isOwnershipForwardingValue(Value))
    return;

  // Then gather up our initial list of users.
  llvm::SmallVector<Operand *, 8> Users;
  std::copy(Value->use_begin(), Value->use_end(), std::back_inserter(Users));

  while (!Users.empty()) {
    Operand *Op = Users.pop_back_val();
    auto *User = Op->getUser();

    // If this op is a type dependent operand, skip it. It is not interesting
    // from an ownership perspective.
    if (User->isTypeDependentOperand(*Op))
      continue;

    if (OwnershipCompatibilityUseChecker(Mod, *Op, Value).check(User)) {
      DEBUG(llvm::dbgs() << "        Lifetime Ending User: " << *User);
      LifetimeEndingUsers.push_back(User);
    } else {
      DEBUG(llvm::dbgs() << "        Regular User: " << *User);
      NonLifetimeEndingUsers.push_back(User);
    }

    // If our base value is not guaranteed or our intermediate value is not an
    // ownership forwarding inst, continue. We do not want to visit any
    // subobjects recursively.
    if (!IsGuaranteed || !isOwnershipForwardingInst(User)) {
      continue;
    }

    // At this point, we know that we must have a forwarded subobject. Since the
    // base type is guaranteed, we know that the subobject is either guaranteed
    // or trivial. The trivial case is not interesting for ARC verification, so
    // if the user has a trivial ownership kind, continue.
    if (SILValue(User).getOwnershipKind() == ValueOwnershipKind::Trivial) {
      continue;
    }

    // Now, we /must/ have a guaranteed subobject, so lets assert that the user
    // is actually guaranteed and add the subobject's users to our worklist.
    assert(SILValue(User).getOwnershipKind() ==
               ValueOwnershipKind::Guaranteed &&
           "Our value is guaranteed and this is a forwarding instruction. "
           "Should have guaranteed ownership as well.");
    std::copy(User->use_begin(), User->use_end(), std::back_inserter(Users));
  }
}

// Unique our non lifetime ending user list by only selecting the last user in
// each block.
void SILValueOwnershipChecker::uniqueNonLifetimeEndingUsers(
    ArrayRef<SILInstruction *> NonLifetimeEndingUsers) {
  for (SILInstruction *User : NonLifetimeEndingUsers) {
    auto *UserBlock = User->getParent();
    // First try to associate User with User->getParent().
    auto Result =
        BlocksWithNonLifetimeEndingUses.insert(std::make_pair(UserBlock, User));

    // If the insertion succeeds, then we know that there is no more work to
    // be done, so process the next use.
    if (Result.second)
      continue;

    // If the insertion fails, then we have at least two non-lifetime ending
    // uses in the same block. Since we are performing a liveness type of
    // dataflow, we only need the last non-lifetime ending use to show that all
    // lifetime ending uses post dominate both. Thus, see if Use is after
    // Result.first->second in the use list. If Use is not later, then we wish
    // to keep the already mapped value, not use, so continue.
    if (std::find_if(Result.first->second->getIterator(), UserBlock->end(),
                     [&User](const SILInstruction &I) -> bool {
                       return User == &I;
                     }) == UserBlock->end()) {
      continue;
    }

    // At this point, we know that Use is later in the Block than
    // Result.first->second, so store Use instead.
    Result.first->second = User;
  }
}

bool SILValueOwnershipChecker::checkFunctionArgWithoutLifetimeEndingUses(
    SILFunctionArgument *Arg) {
  switch (Arg->getOwnershipKind()) {
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

  if (TUB.isUnreachable(Arg->getParent()))
    return true;

  llvm::errs() << "Function: '" << Arg->getFunction()->getName() << "'\n"
               << "    Owned function parameter without life "
                  "ending uses!\n"
               << "Value: " << *Arg << '\n';
  if (IsSILOwnershipVerifierTestingEnabled)
    return true;
  llvm_unreachable("triggering standard assertion failure routine");
}

bool SILValueOwnershipChecker::checkValueWithoutLifetimeEndingUses() {
  DEBUG(llvm::dbgs() << "    No lifetime ending users?! Bailing early.\n");
  if (auto *Arg = dyn_cast<SILFunctionArgument>(Value)) {
    if (checkFunctionArgWithoutLifetimeEndingUses(Arg)) {
      return true;
    }
  }

  // Check if we are a guaranteed subobject. In such a case, we should never
  // have lifetime ending uses, since our lifetime is guaranteed by our
  // operand, so there is nothing further to do. So just return true.
  if (isOwnershipForwardingValue(Value) &&
      Value.getOwnershipKind() == ValueOwnershipKind::Guaranteed)
    return true;

  // If we have an unowned value, then again there is nothing left to do.
  if (Value.getOwnershipKind() == ValueOwnershipKind::Unowned)
    return true;

  if (auto *ParentBlock = Value->getParentBlock()) {
    if (TUB.isUnreachable(ParentBlock)) {
      DEBUG(llvm::dbgs() << "    Ignoring transitively unreachable value "
                         << "without users!\n"
                         << "    Function: '" << Value->getFunction()->getName()
                         << "'\n"
                         << "    Value: " << *Value << '\n');
      return true;
    }
  }

  if (!isValueAddressOrTrivial(Value, Mod)) {
    llvm::errs() << "Function: '" << Value->getFunction()->getName() << "'\n"
                 << "Non trivial values, non address values, and non "
                    "guaranteed function args must have at least one "
                    "lifetime ending use?!\n"
                 << "Value: " << *Value << '\n';
    if (IsSILOwnershipVerifierTestingEnabled)
      return true;
    llvm_unreachable("triggering standard assertion failure routine");
  }

  return true;
}

static bool isGuaranteedFunctionArgWithLifetimeEndingUses(
    SILFunctionArgument *Arg,
    const llvm::SmallVectorImpl<SILInstruction *> &LifetimeEndingUsers) {
  if (Arg->getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return true;

  llvm::errs() << "    Function: '" << Arg->getFunction()->getName() << "'\n"
               << "    Guaranteed function parameter with life ending uses!\n"
               << "    Value: " << *Arg;
  for (auto *U : LifetimeEndingUsers) {
    llvm::errs() << "    Lifetime Ending User: " << *U;
  }
  llvm::errs() << '\n';
  if (IsSILOwnershipVerifierTestingEnabled)
    return false;
  llvm_unreachable("triggering standard assertion failure routine");
}

static bool isSubobjectProjectionWithLifetimeEndingUses(
    SILValue Value,
    const llvm::SmallVectorImpl<SILInstruction *> &LifetimeEndingUsers) {
  llvm::errs() << "    Function: '" << Value->getFunction()->getName() << "'\n"
               << "    Subobject projection with life ending uses!\n"
               << "    Value: " << *Value;
  for (auto *U : LifetimeEndingUsers) {
    llvm::errs() << "    Lifetime Ending User: " << *U;
  }
  llvm::errs() << '\n';
  if (IsSILOwnershipVerifierTestingEnabled)
    return false;
  llvm_unreachable("triggering standard assertion failure routine");
}

bool SILValueOwnershipChecker::checkUses() {
  DEBUG(llvm::dbgs() << "    Gathering and classifying uses!\n");

  // First go through V and gather up its uses. While we do this we:
  //
  // 1. Verify that none of the uses are in the same block. This would be an
  // overconsume so in this case we assert.
  // 2. Verify that the uses are compatible with our ownership convention.
  llvm::SmallVector<SILInstruction *, 16> LifetimeEndingUsers;
  llvm::SmallVector<SILInstruction *, 16> NonLifetimeEndingUsers;
  gatherUsers(LifetimeEndingUsers, NonLifetimeEndingUsers);

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
  if (LifetimeEndingUsers.empty() && checkValueWithoutLifetimeEndingUses()) {
    return false;
  }

  DEBUG(llvm::dbgs() << "    Found lifetime ending users! Performing initial "
                        "checks\n");

  // See if we have a guaranteed function address. Guaranteed function addresses
  // should never have any lifetime ending uses.
  if (auto *Arg = dyn_cast<SILFunctionArgument>(Value)) {
    if (!isGuaranteedFunctionArgWithLifetimeEndingUses(Arg,
                                                       LifetimeEndingUsers)) {
      return false;
    }
  }

  // Check if we are an instruction that forwards ownership that forwards
  // guaranteed ownership. In such a case, we are a subobject projection. We
  // should not have any lifetime ending uses.
  if (isOwnershipForwardingValue(Value) &&
      Value.getOwnershipKind() == ValueOwnershipKind::Guaranteed) {
    if (!isSubobjectProjectionWithLifetimeEndingUses(Value,
                                                     LifetimeEndingUsers)) {
      return false;
    }
  }

  // Then add our non lifetime ending users and their blocks to the
  // BlocksWithNonLifetimeEndingUses map. While we do this, if we have multiple
  // uses in the same block, we only accept the last use since from a liveness
  // perspective that is all we care about.
  uniqueNonLifetimeEndingUsers(NonLifetimeEndingUsers);

  // Finally, we go through each one of our lifetime ending users performing the
  // following operation:
  //
  // 1. Verifying that no two lifetime ending users are in the same block. This
  // is accomplished by adding the user blocks to the
  // BlocksWithLifetimeEndingUses list. This avoids double consumes.
  //
  // 2. Verifying that no predecessor is a block with a lifetime ending use. The
  // reason why this is necessary is because we wish to not add elements to the
  // worklist twice. Thus we want to check if we have already visited a
  // predecessor.
  llvm::SmallVector<std::pair<SILInstruction *, SILBasicBlock *>, 32>
      PredsToAddToWorklist;
  for (SILInstruction *User : LifetimeEndingUsers) {
    SILBasicBlock *UserBlock = User->getParent();
    // If the block does over consume, we either assert or return false. We only
    // return false when debugging.
    if (doesBlockDoubleConsume(UserBlock, User, true)) {
      if (IsSILOwnershipVerifierTestingEnabled)
        return false;
      llvm_unreachable("triggering standard assertion failure routine");
    }

    // Then check if the given block has a use after free.
    if (doesBlockContainUseAfterFree(User, UserBlock)) {
      if (IsSILOwnershipVerifierTestingEnabled)
        return false;
      llvm_unreachable("triggering standard assertion failure routine");
    }

    // If this user is in the same block as the value, do not visit
    // predecessors. We must be extra tolerant here since we allow for
    // unreachable code.
    if (UserBlock == Value->getParentBlock())
      continue;

    // Then for each predecessor of this block...
    for (auto *Pred : UserBlock->getPredecessorBlocks()) {
      // If this block is not a block that we have already put on the list, add
      // it to the worklist.
      PredsToAddToWorklist.push_back({User, Pred});
    }
  }

  for (auto *I : LifetimeEndingUsers) {
    // Finally add the user block to the visited list so we do not try to add it
    // to our must visit successor list.
    VisitedBlocks.insert(I->getParent());
  }

  // Make sure not to add predecessors to our worklist if we only have 1
  // lifetime ending user and it is in the same block as our def.
  if (LifetimeEndingUsers.size() == 1 &&
      LifetimeEndingUsers[0]->getParent() == Value->getParentBlock()) {
    return true;
  }

  // Now that we have marked all of our producing blocks, we go through our
  // PredsToAddToWorklist list and add our preds, making sure that none of these
  // preds are in BlocksWithLifetimeEndingUses.
  for (auto Pair : PredsToAddToWorklist) {
    SILBasicBlock *PredBlock;
    SILInstruction *User;
    std::tie(User, PredBlock) = Pair;

    // Make sure that the predecessor is not in our
    // BlocksWithLifetimeEndingUses list.
    if (doesBlockDoubleConsume(PredBlock, User)) {
      if (IsSILOwnershipVerifierTestingEnabled)
        return false;
      llvm_unreachable("triggering standard assertion failure routine");
    }

    if (!VisitedBlocks.insert(PredBlock).second)
      continue;
    Worklist.push_back(PredBlock);
  }

  return true;
}

void SILValueOwnershipChecker::checkDataflow() {
  DEBUG(llvm::dbgs() << "    Beginning to check dataflow constraints\n");
  // Until the worklist is empty...
  while (!Worklist.empty()) {
    // Grab the next block to visit.
    SILBasicBlock *BB = Worklist.pop_back_val();
    DEBUG(llvm::dbgs() << "    Visiting Block: bb" << BB->getDebugID() << '\n');

    // Since the block is on our worklist, we know already that it is not a
    // block with lifetime ending uses, due to the invariants of our loop.

    // First remove BB from the SuccessorBlocksThatMustBeVisited list. This
    // ensures that when the algorithm terminates, we know that BB was not the
    // beginning of a non-covered path to the exit.
    SuccessorBlocksThatMustBeVisited.erase(BB);

    // Then remove BB from BlocksWithNonLifetimeEndingUses so we know that
    // this block was properly joint post-dominated by our lifetime ending
    // users.
    BlocksWithNonLifetimeEndingUses.erase(BB);

    // Ok, now we know that we do not have an overconsume. If this block does
    // not end in a no return function, we need to update our state for our
    // successors to make sure by the end of the traversal we visit them.
    //
    // We must consider such no-return blocks since we may be running during
    // SILGen before NoReturn folding has run.
    for (SILBasicBlock *SuccBlock : BB->getSuccessorBlocks()) {
      // If we already visited the successor, there is nothing to do since we
      // already visited the successor.
      if (VisitedBlocks.count(SuccBlock))
        continue;

      // Then check if the successor is a transitively unreachable block. In
      // such a case, we ignore it since we are going to leak along that path.
      if (TUB.isUnreachable(SuccBlock))
        continue;

      // Otherwise, add the successor to our SuccessorBlocksThatMustBeVisited
      // set to ensure that we assert if we do not visit it by the end of the
      // algorithm.
      SuccessorBlocksThatMustBeVisited.insert(SuccBlock);
    }

    // If we are at the dominating block of our walk, continue. There is nothing
    // further to do since we do not want to visit the predecessors of our
    // dominating block. On the other hand, we do want to add its successors to
    // the SuccessorBlocksThatMustBeVisited set.
    if (BB == Value->getParentBlock())
      continue;

    // Then for each predecessor of this block:
    //
    // 1. If we have visited the predecessor already, that it is not a block
    // with lifetime ending uses. If it is a block with uses, then we have a
    // double release... so assert. If not, we continue.
    //
    // 2. We add the predecessor to the worklist if we have not visited it yet.
    for (auto *PredBlock : BB->getPredecessorBlocks()) {
      if (doesBlockDoubleConsume(PredBlock)) {
        if (IsSILOwnershipVerifierTestingEnabled)
          return;
        llvm_unreachable("triggering standard assertion failure routine");
      }

      if (VisitedBlocks.count(PredBlock)) {
        continue;
      }

      VisitedBlocks.insert(PredBlock);
      Worklist.push_back(PredBlock);
    }
  }

  // Make sure that we visited all successor blocks that we needed to visit to
  // make sure we didn't leak.
  if (!SuccessorBlocksThatMustBeVisited.empty()) {
    llvm::errs()
        << "Function: '" << Value->getFunction()->getName() << "'\n"
        << "Error! Found a leak due to a consuming post-dominance failure!\n"
        << "    Value: " << *Value << "    Post Dominating Failure Blocks:\n";
    for (auto *BB : SuccessorBlocksThatMustBeVisited) {
      llvm::errs() << "        bb" << BB->getDebugID();
    }
    llvm::errs() << '\n';
    if (IsSILOwnershipVerifierTestingEnabled)
      return;
    llvm_unreachable("triggering standard assertion failure routine");
  }

  // Make sure that we do not have any lifetime ending uses left to visit. If we
  // do, then these non lifetime ending uses must be outside of our "alive"
  // blocks implying a use-after free.
  if (!BlocksWithNonLifetimeEndingUses.empty()) {
    llvm::errs()
        << "Function: '" << Value->getFunction()->getName() << "'\n"
        << "Found use after free due to unvisited non lifetime ending uses?!\n"
        << "Value: " << *Value << "    Remaining Users:\n";
    for (auto &Pair : BlocksWithNonLifetimeEndingUses) {
      llvm::errs() << "User:" << *Pair.second << "Block: bb"
                   << Pair.first->getDebugID() << "\n";
    }
    llvm::errs() << "\n";
    if (IsSILOwnershipVerifierTestingEnabled)
      return;
    llvm_unreachable("triggering standard assertion failure routine");
  }
}

#endif

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

void SILInstruction::verifyOperandOwnership() const {
#ifndef NDEBUG
  // If SILOwnership is not enabled, do not perform verification.
  if (!getModule().getOptions().EnableSILOwnership)
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

  auto *Self = const_cast<SILInstruction *>(this);
  for (const Operand &Op : getAllOperands()) {
    if (isTypeDependentOperand(Op))
      continue;
    OwnershipCompatibilityUseChecker(getModule(), Op, Op.get()).check(Self);
  }
#endif
}

void SILValue::verifyOwnership(SILModule &Mod,
                               TransitivelyUnreachableBlocksInfo *TUB) const {
#ifndef NDEBUG
  if (TUB) {
    SILValueOwnershipChecker(Mod, *TUB, *this).check();
  } else {
    PostOrderFunctionInfo NewPOFI((*this)->getFunction());
    TransitivelyUnreachableBlocksInfo TUB(NewPOFI);
    SILValueOwnershipChecker(Mod, TUB, *this).check();
  }
#endif
}
