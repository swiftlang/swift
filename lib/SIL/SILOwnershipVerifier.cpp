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
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/PrettyStackTrace.h"
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

using namespace swift;

// The verifier is basically all assertions, so don't compile it with NDEBUG to
// prevent release builds from triggering spurious unused variable warnings.
#ifndef NDEBUG

llvm::cl::opt<bool> PrintMessageInsteadOfAssert(
    "sil-ownership-verifier-do-not-assert",
    llvm::cl::desc("Print out message instead of asserting. "
                   "Meant for debugging"));

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool compatibleOwnershipKinds(ValueOwnershipKind K1,
                                     ValueOwnershipKind K2) {
  return K1.merge(K2).hasValue();
}

static bool isValueAddressOrTrivial(SILValue V, SILModule &M) {
  return V->getType().isAddress() || V->getType().isTrivial(M);
}

//===----------------------------------------------------------------------===//
//                    OwnershipCompatibilityCheckerVisitor
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

public:
  OwnershipCompatibilityUseChecker(SILModule &M, const Operand &Op)
      : Mod(M), Op(Op) {}

  SILValue getValue() const { return Op.get(); }

  ValueOwnershipKind getOwnershipKind() const {
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
    return getType().isTrivial(Mod);
  }

  void error(SILInstruction *User) {
    llvm::errs() << "Have operand with incompatible ownership?!\n"
                 << "Value: " << *getValue() << "User: " << *User
                 << "Conv: " << getOwnershipKind() << "\n";
    if (PrintMessageInsteadOfAssert)
      return;
    llvm_unreachable("triggering standard assertion failure routine");
  }

  OwnershipUseCheckerResult visitForwardingInst(SILInstruction *I);

  /// Check if \p User as compatible ownership with the SILValue that we are
  /// checking.
  ///
  /// \returns true if the user is a use that must be checked for dataflow
  /// violations.
  bool check(SILInstruction *User) {
    auto Result = visit(User);
    if (!Result.HasCompatibleOwnership) {
      error(User);
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
NO_OPERAND_INST(AllocRef)
NO_OPERAND_INST(AllocRefDynamic)
NO_OPERAND_INST(AllocStack)
NO_OPERAND_INST(AllocValueBuffer)
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
NO_OPERAND_INST(ValueMetatype)
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
CONSTANT_OWNERSHIP_INST(Guaranteed, false, TupleExtract)
CONSTANT_OWNERSHIP_INST(Guaranteed, false, StructExtract)
CONSTANT_OWNERSHIP_INST(Guaranteed, false, UncheckedEnumData)
CONSTANT_OWNERSHIP_INST(Owned, true, AutoreleaseValue)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocBox)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocPartialRef)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocRef)
CONSTANT_OWNERSHIP_INST(Owned, true, DeallocValueBuffer)
CONSTANT_OWNERSHIP_INST(Owned, true, DestroyValue)
CONSTANT_OWNERSHIP_INST(Owned, true, ReleaseValue)
CONSTANT_OWNERSHIP_INST(Owned, true, StrongRelease)
CONSTANT_OWNERSHIP_INST(Owned, true, StrongUnpin)
CONSTANT_OWNERSHIP_INST(Owned, true, SwitchEnum)
CONSTANT_OWNERSHIP_INST(Owned, true, UnownedRelease)
CONSTANT_OWNERSHIP_INST(Owned, true, InitExistentialRef)
CONSTANT_OWNERSHIP_INST(Guaranteed, true,
                        OpenExistentialRef) // We may need a take here.
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
CONSTANT_OWNERSHIP_INST(Trivial, false, MarkUninitialized)
CONSTANT_OWNERSHIP_INST(Trivial, false, MarkUninitializedBehavior)
CONSTANT_OWNERSHIP_INST(Trivial, false, ObjCExistentialMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Trivial, false, ObjCMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Trivial, false, ObjCToThickMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, false, OpenExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, OpenExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, false, PointerToAddress)
CONSTANT_OWNERSHIP_INST(Trivial, false, PointerToThinFunction)
CONSTANT_OWNERSHIP_INST(Trivial, false, ProjectBlockStorage)
CONSTANT_OWNERSHIP_INST(Trivial, false, ProjectExistentialBox)
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
CONSTANT_OWNERSHIP_INST(Trivial, false, UncheckedTrivialBitCast)
CONSTANT_OWNERSHIP_INST(Trivial, false, UnconditionalCheckedCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, false, UnmanagedToRef)
#undef CONSTANT_OWNERSHIP_INST

#define ACCEPTS_ANY_OWNERSHIP_INST(INST)                                       \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    return {true, false};                                                      \
  }
ACCEPTS_ANY_OWNERSHIP_INST(BeginBorrow)
ACCEPTS_ANY_OWNERSHIP_INST(CopyValue)
ACCEPTS_ANY_OWNERSHIP_INST(DebugValue)
ACCEPTS_ANY_OWNERSHIP_INST(FixLifetime)
ACCEPTS_ANY_OWNERSHIP_INST(SelectEnum)
ACCEPTS_ANY_OWNERSHIP_INST(UncheckedBitwiseCast) // Is this right?
ACCEPTS_ANY_OWNERSHIP_INST(WitnessMethod)        // Is this right?
ACCEPTS_ANY_OWNERSHIP_INST(ProjectBox)           // The result is a T*.
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
// DynamicMethodBranch: Is this right? I think this is taken at +1.
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, DynamicMethodBranch)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, ExistentialMetatype)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, OpenExistentialBox)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefElementAddr)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefTailAddr)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefToRawPointer)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefToUnmanaged)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, RefToUnowned)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, SetDeallocating)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, StrongPin)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, UnownedToRef)
ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(false, CopyUnownedValue)
#undef ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitForwardingInst(SILInstruction *I) {
  assert(I->getNumOperands() && "Expected to have non-zero operands");
  ArrayRef<Operand> Ops = I->getAllOperands();
  ValueOwnershipKind Base = getOwnershipKind();
  for (const Operand &Op : Ops) {
    auto MergedValue = Base.merge(Op.get().getOwnershipKind());
    if (!MergedValue.hasValue())
      return {false, true};
    Base = MergedValue.getValue();
  }
  return {true, !isAddressOrTrivialType()};
}

#define FORWARD_OWNERSHIP_INST(INST)                                           \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    return visitForwardingInst(I);                                             \
  }

FORWARD_OWNERSHIP_INST(Tuple)
FORWARD_OWNERSHIP_INST(Struct)
FORWARD_OWNERSHIP_INST(Enum)
// All of these should really have take falgs and be guaranteed otherwise.
FORWARD_OWNERSHIP_INST(Upcast)
FORWARD_OWNERSHIP_INST(UncheckedRefCast)
FORWARD_OWNERSHIP_INST(ConvertFunction)
FORWARD_OWNERSHIP_INST(RefToBridgeObject)
FORWARD_OWNERSHIP_INST(BridgeObjectToRef)
FORWARD_OWNERSHIP_INST(UnconditionalCheckedCast)
// This should be based off of the argument.
FORWARD_OWNERSHIP_INST(Branch)
FORWARD_OWNERSHIP_INST(CondBranch)
FORWARD_OWNERSHIP_INST(CheckedCastBranch)
#undef FORWARD_OWNERSHIP_INST

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitReturnInst(ReturnInst *RI) {
  SILModule &M = RI->getModule();
  bool IsTrivial = RI->getOperand()->getType().isTrivial(M);
  auto FnType = RI->getFunction()->getLoweredFunctionType();
  auto Results = FnType->getDirectResults();
  if (Results.empty() || IsTrivial) {
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  }

  CanGenericSignature Sig = FnType->getGenericSignature();

  // Find the first index where we have a trivial value.
  auto Iter = find_if(Results, [&M, &Sig](const SILResultInfo &Info) -> bool {
    return Info.getOwnershipKind(M, Sig) != ValueOwnershipKind::Trivial;
  });

  // If we have all trivial, then we must be trivial. Why wasn't our original
  // type trivial? This is a hard error since this is a logic error in our code
  // here.
  if (Iter == Results.end())
    llvm_unreachable("Should have already checked a trivial type?!");

  unsigned Index = std::distance(Results.begin(), Iter);
  ValueOwnershipKind Base = Results[Index].getOwnershipKind(M, Sig);

  for (const SILResultInfo &ResultInfo : Results.slice(Index + 1)) {
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
  // Error objects are trivial? If this fails, fix this.
  return {true, false};
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

OwnershipUseCheckerResult OwnershipCompatibilityUseChecker::visitCallee(
    CanSILFunctionType SubstCalleeType) {
  ParameterConvention Conv = SubstCalleeType->getCalleeConvention();
  switch (Conv) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Guaranteed:
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
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  case SILArgumentConvention::Direct_Unowned:
    if (isAddressOrTrivialType())
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    return {compatibleWithOwnership(ValueOwnershipKind::Unowned), false};
  case SILArgumentConvention::Direct_Guaranteed:
    return {compatibleWithOwnership(ValueOwnershipKind::Guaranteed), false};
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
    return {true, false};
  case SILArgumentConvention::Direct_Owned:
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  case SILArgumentConvention::Direct_Unowned:
    if (isAddressOrTrivialType())
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    return {compatibleWithOwnership(ValueOwnershipKind::Unowned), false};
  case SILArgumentConvention::Direct_Guaranteed:
    return {compatibleWithOwnership(ValueOwnershipKind::Guaranteed), false};
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
OwnershipCompatibilityUseChecker::visitBuiltinInst(BuiltinInst *I) {
  // This needs to be updated.
  return {true, false};
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
    MarkDependenceInst *I) {
  // This needs to be updated.
  llvm_unreachable("Not implemented");
}

//===----------------------------------------------------------------------===//
//                         SILValueOwnershipChecker
//===----------------------------------------------------------------------===//

namespace {

class SILValueOwnershipChecker {
  /// The module that we are in.
  SILModule &Mod;

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
  SILValueOwnershipChecker(SILModule &M, SILValue V) : Mod(M), Value(V) {}

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
    llvm::errs() << "Found use after free?!\n"
                 << "Function: '" << Value->getFunction()->getName() << "'\n"
                 << "Value: " << *Value
                 << "Consuming User: " << *LifetimeEndingUser
                 << "Non Consuming User: " << *Iter->second << "Block:\n"
                 << *UserBlock << "\n";
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
  llvm::errs() << "Block:\n" << *UserBlock << "\n";

  return true;
}

void SILValueOwnershipChecker::gatherUsers(
    llvm::SmallVectorImpl<SILInstruction *> &LifetimeEndingUsers,
    llvm::SmallVectorImpl<SILInstruction *> &NonLifetimeEndingUsers) {
  for (Operand *Op : Value->getUses()) {
    auto *User = Op->getUser();
    if (OwnershipCompatibilityUseChecker(Mod, *Op).check(User)) {
      DEBUG(llvm::dbgs() << "        Lifetime Ending User: " << *User);
      LifetimeEndingUsers.push_back(User);
    } else {
      DEBUG(llvm::dbgs() << "        Regular User: " << *User);
      NonLifetimeEndingUsers.push_back(User);
    }
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

  // If we do not have any lifetime ending users, there is nothing to
  // check. This occurs with trivial types and addresses. Return false.
  if (LifetimeEndingUsers.empty()) {
    DEBUG(llvm::dbgs() << "    No lifetime ending users?! Bailing early.\n");
    assert(isValueAddressOrTrivial(Value, Mod) &&
           "Must always check the lifetime for non-trivial, non-address types");
    return false;
  }

  DEBUG(llvm::dbgs() << "    Found lifetime ending users! Performing initial "
                        "checks\n");

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
      if (PrintMessageInsteadOfAssert)
        return false;
      llvm_unreachable("triggering standard assertion failure routine");
    }

    // Then check if the given block has a use after free.
    if (doesBlockContainUseAfterFree(User, UserBlock)) {
      if (PrintMessageInsteadOfAssert)
        return false;
      llvm_unreachable("triggering standard assertion failure routine");
    }

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
      if (PrintMessageInsteadOfAssert)
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
    DEBUG(llvm::dbgs() << "    Visiting Block:\n" << *BB);

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

    // Ok, now we know that we do not have an overconsume. So now we need to
    // update our state for our successors to make sure by the end of the block,
    // we visit them.
    for (SILBasicBlock *SuccBlock : BB->getSuccessorBlocks()) {
      // If we already visited the successor, there is nothing to do since we
      // already visited the successor.
      if (VisitedBlocks.count(SuccBlock))
        continue;

      // Otherwise, add the successor to our SuccessorBlocksThatMustBeVisited
      // set to ensure that we assert if we do not visit it by the end of the
      // algorithm.
      SuccessorBlocksThatMustBeVisited.insert(SuccBlock);
    }

    // Then for each predecessor of this block:
    //
    // 1. If we have visited the predecessor already, that it is not a block
    // with lifetime ending uses. If it is a block with uses, then we have a
    // double release... so assert. If not, we continue.
    //
    // 2. We add the predecessor to the worklist if we have not visited it yet.
    for (auto *PredBlock : BB->getPredecessorBlocks()) {
      if (doesBlockDoubleConsume(PredBlock)) {
        if (PrintMessageInsteadOfAssert)
          return;
        llvm_unreachable("triggering standard assertion failure routine");
      }

      if (VisitedBlocks.count(PredBlock)) {
        continue;
      }
      Worklist.push_back(PredBlock);
    }
  }

  // Make sure that we visited all successor blocks that we needed to visit to
  // make sure we didn't leak.
  if (!SuccessorBlocksThatMustBeVisited.empty()) {
    llvm::errs()
        << "Error! Found a leak due to a consuming post-dominance failure!\n"
        << "Function: '" << Value->getFunction()->getName() << "'\n"
        << "    Value: " << *Value << "    Post Dominating Failure Blocks:\n";
    for (auto *BB : SuccessorBlocksThatMustBeVisited) {
      llvm::errs() << *BB;
    }
    if (PrintMessageInsteadOfAssert)
      return;
    llvm_unreachable("triggering standard assertion failure routine");
  }

  // Make sure that we do not have any lifetime ending uses left to visit. If we
  // do, then these non lifetime ending uses must be outside of our "alive"
  // blocks implying a use-after free.
  if (!BlocksWithNonLifetimeEndingUses.empty()) {
    llvm::errs()
        << "Found use after free due to unvisited non lifetime ending uses?!\n"
        << "Function: '" << Value->getFunction()->getName() << "'\n"
        << "Value: " << *Value << "    Remaining Users:\n";
    for (auto &Pair : BlocksWithNonLifetimeEndingUses) {
      llvm::errs() << "User:" << *Pair.second << "Block:\n"
                   << *Pair.first << "\n";
    }
    if (PrintMessageInsteadOfAssert)
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
  auto *Self = const_cast<SILInstruction *>(this);
  for (const Operand &Op : getAllOperands()) {
    OwnershipCompatibilityUseChecker(getModule(), Op).check(Self);
  }
#endif
}

void SILValue::verifyOwnership(SILModule &Mod) const {
#ifndef NDEBUG
  SILValueOwnershipChecker(Mod, *this).check();
#endif
}
