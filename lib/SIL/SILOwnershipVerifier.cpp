//===--- SILOwnershipVerifier.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool compatibleOwnershipKinds(ValueOwnershipKind K1,
                                     ValueOwnershipKind K2) {
  return ValueOwnershipKindMerge(K1, K2).hasValue();
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
  const Operand &Op;

public:
  OwnershipCompatibilityUseChecker(const Operand &Op) : Op(Op) {}

  SILValue getValue() { return Op.get(); }

  ValueOwnershipKind getOwnershipKind() {
    return getValue().getOwnershipKind();
  }

  unsigned getOperandIndex() { return Op.getOperandNumber(); }

  SILType getType() { return Op.get()->getType(); }

  bool compatibleWithOwnership(ValueOwnershipKind Kind) {
    return compatibleOwnershipKinds(getOwnershipKind(), Kind);
  }

  void error(SILInstruction *User) {
    llvm::errs() << "Have operand with incompatible ownership?!\n"
                 << "Value: " << *getValue() << "User: " << *User
                 << "Conv: " << getOwnershipKind() << "\n";
    llvm_unreachable("triggering standard assertion failure routine");
  }

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

    return Result.ShouldCheckForDataflowViolations;
  }

  OwnershipUseCheckerResult visitValueBase(ValueBase *) {
    llvm_unreachable("Unimplemented?!");
  }

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
      assert((getType().isAddress() || getType().isTrivial(I->getModule())) && \
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
CONSTANT_OWNERSHIP_INST(Trivial, false, ProjectBox)
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
#undef ACCEPTS_ANY_OWNERSHIP_INST

// Trivial if trivial typed, otherwise must accept owned?
#define ACCEPTS_ANY_NONTRIVIAL_OWNERSHIP(SHOULD_CHECK_FOR_DATAFLOW_VIOLATIONS, \
                                         INST)                                 \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
    assert((!getType().isAddress() && !getType().isTrivial(I->getModule())) && \
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

#define FORWARD_OWNERSHIP_INST(INST)                                           \
  OwnershipUseCheckerResult                                                    \
      OwnershipCompatibilityUseChecker::visit##INST##Inst(INST##Inst *I) {     \
    assert(I->getNumOperands() && "Expected to have non-zero operands");       \
    ArrayRef<Operand> Ops = I->getAllOperands();                               \
    llvm::Optional<ValueOwnershipKind> Base = getOwnershipKind();              \
    for (const Operand &Op : Ops) {                                            \
      Base = ValueOwnershipKindMerge(Base, Op.get().getOwnershipKind());       \
      if (!Base.hasValue())                                                    \
        return {false, true};                                                  \
    }                                                                          \
    return {true, true};                                                       \
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
  auto Results =
      RI->getFunction()->getLoweredFunctionType()->getDirectResults();
  if (Results.empty() || IsTrivial) {
    return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
  }

  // Find the first index where we have a trivial value.
  auto Iter = find_if(Results, [&M](const SILResultInfo &Info) -> bool {
    return Info.getOwnershipKind(M) != ValueOwnershipKind::Trivial;
  });

  // If we have all trivial, then we must be trivial. Why wasn't our original
  // type trivial? This is a hard error since this is a logic error in our code
  // here.
  if (Iter == Results.end())
    llvm_unreachable("Should have already checked a trivial type?!");

  unsigned Index = std::distance(Results.begin(), Iter);
  ValueOwnershipKind Base = Results[Index].getOwnershipKind(M);

  for (const SILResultInfo &ResultInfo : Results.slice(Index + 1)) {
    auto RKind = ResultInfo.getOwnershipKind(M);
    // Ignore trivial types.
    if (ValueOwnershipKindMerge(RKind, ValueOwnershipKind::Trivial))
      continue;

    auto MergedValue = ValueOwnershipKindMerge(Base, RKind);
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
  // We do not consider the source to be a verified use for now.
  if (getOperandIndex() == EndBorrowInst::Src)
    return {true, false};
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

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitApplyInst(ApplyInst *I) {
  // The first operand of an apply is the callee.
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
    if (getValue()->getType().isTrivial(I->getModule()))
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    return {compatibleWithOwnership(ValueOwnershipKind::Unowned), false};
  case SILArgumentConvention::Direct_Guaranteed:
    return {compatibleWithOwnership(ValueOwnershipKind::Guaranteed), false};
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("No ownership associated with deallocating");
  }
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitTryApplyInst(TryApplyInst *I) {
  // The first operand of an apply is the callee.
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
    if (getValue()->getType().isTrivial(I->getModule()))
      return {compatibleWithOwnership(ValueOwnershipKind::Trivial), false};
    return {compatibleWithOwnership(ValueOwnershipKind::Unowned), false};
  case SILArgumentConvention::Direct_Guaranteed:
    return {compatibleWithOwnership(ValueOwnershipKind::Guaranteed), false};
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("No ownership associated with deallocating");
  }
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitPartialApplyInst(PartialApplyInst *I) {
  // All non-trivial types should be captured.
  if (getValue()->getType().isTrivial(I->getModule())) {
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
  if (getValue() == I->getSrc())
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
  return {true, false};
}

OwnershipUseCheckerResult
OwnershipCompatibilityUseChecker::visitStoreInst(StoreInst *I) {
  if (getValue() == I->getSrc())
    return {compatibleWithOwnership(ValueOwnershipKind::Owned), true};
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
  SILValueOwnershipChecker(SILValue V) : Value(V) {}

  ~SILValueOwnershipChecker() = default;
  SILValueOwnershipChecker(SILValueOwnershipChecker &) = delete;
  SILValueOwnershipChecker(SILValueOwnershipChecker &&) = delete;

  void check() {
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
};

} // end anonymous namespace

bool SILValueOwnershipChecker::checkUses() {
  // First go through V and gather up its uses. While we do this we:
  //
  // 1. Verify that none of the uses are in the same block. This would be an
  // overconsume so in this case we assert.
  // 2. Verify that the uses are compatible with our ownership convention.
  llvm::SmallVector<SILInstruction *, 16> LifetimeEndingUsers;
  llvm::SmallVector<SILInstruction *, 16> NonLifetimeEndingUsers;
  for (Operand *Op : Value->getUses()) {
    auto *User = Op->getUser();
    if (OwnershipCompatibilityUseChecker(*Op).check(User)) {
      LifetimeEndingUsers.push_back(User);
    } else {
      NonLifetimeEndingUsers.push_back(User);
    }
  }

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

  for (SILInstruction *User : LifetimeEndingUsers) {
    SILBasicBlock *UserBlock = User->getParent();
    bool InsertedBlock = BlocksWithLifetimeEndingUses.insert(UserBlock).second;
    if (!InsertedBlock) {
      llvm::errs() << "Found over consume?!\n"
                   << "Value: " << *Value << "User: " << *User << "Block:\n"
                   << *UserBlock << "\n";
      llvm_unreachable("triggering standard assertion failure routine");
    }

    auto Iter = BlocksWithNonLifetimeEndingUses.find(UserBlock);
    if (Iter != BlocksWithNonLifetimeEndingUses.end()) {
      SILInstruction *NonLifetimeEndingUser = Iter->second;
      // Make sure that the non-lifetime ending use is before the lifetime
      // ending use. Otherwise, we have a use after free.
      if (std::find_if(
              User->getIterator(), UserBlock->end(),
              [&NonLifetimeEndingUser](const SILInstruction &I) -> bool {
                return NonLifetimeEndingUser == &I;
              }) != UserBlock->end()) {
        llvm::errs() << "Found use after free?!\n"
                     << "Value: " << *Value << "Consuming User: " << *User
                     << "Non Consuming User: " << *Iter->second << "Block:\n"
                     << *UserBlock << "\n";
        llvm_unreachable("triggering standard assertion failure routine");
      }

      // Erase the use since we know that it is properly joint post-dominated.
      BlocksWithNonLifetimeEndingUses.erase(Iter);
    }

    // Add the user block to the visited list so we do not try to add it to our
    // must visit successor list.
    VisitedBlocks.insert(UserBlock);

    // Now add all predecessors of this block to the worklist.
    for (auto *Preds : UserBlock->getPredecessorBlocks()) {
      VisitedBlocks.insert(Preds);
      Worklist.push_back(Preds);
    }
  }

  return true;
}

void SILValueOwnershipChecker::checkDataflow() {
  // Until the worklist is empty...
  while (!Worklist.empty()) {
    // Grab the next block to visit.
    SILBasicBlock *BB = Worklist.pop_back_val();

    // Then assert that this block is not a use containing block. If we are
    // visiting a use-containing block, we have an over-consume.
    bool IsBlockWithUses = BlocksWithLifetimeEndingUses.count(BB);
    if (IsBlockWithUses) {
      llvm::errs() << "Found over consume?!\n"
                   << "Value: " << *Value << "Block:\n"
                   << *BB << "\n";
      llvm_unreachable("triggering standard assertion failure routine");
    }

    // Ok, now we know that we are not double-consuming. Update our data
    // structures.

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

    // Then add each predecessor of this block to the worklist if we have not
    // visited it yet.
    for (auto *PredBlock : BB->getPredecessorBlocks()) {
      if (VisitedBlocks.count(PredBlock))
        continue;
      Worklist.push_back(PredBlock);
    }
  }

  // Make sure that we visited all successor blocks that we needed to visit to
  // make sure we didn't leak.
  if (!SuccessorBlocksThatMustBeVisited.empty()) {
    llvm::errs()
        << "Error! Found a leak due to a consuming post-dominance failure!\n"
        << "    Value: " << *Value << "    Post Dominating Failure Blocks:\n";
    for (auto *BB : SuccessorBlocksThatMustBeVisited) {
      llvm::errs() << *BB;
    }
    llvm_unreachable("triggering standard assertion failure routine");
  }

  // Make sure that we do not have any lifetime ending uses left to visit. If we
  // do, then these non lifetime ending uses must be outside of our "alive"
  // blocks implying a use-after free.
  if (!BlocksWithNonLifetimeEndingUses.empty()) {
    llvm::errs()
        << "Found use after free due to unvisited non lifetime ending uses?!\n"
        << "Value: " << *Value << "    Remaining Users:\n";
    for (auto &Pair : BlocksWithNonLifetimeEndingUses) {
      llvm::errs() << "User:" << *Pair.second << "Block:\n"
                   << *Pair.first << "\n";
    }
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
    OwnershipCompatibilityUseChecker(Op).check(Self);
  }
#endif
}

void SILValue::verifyOwnership() const {
#ifndef NDEBUG
  SILValueOwnershipChecker(*this).check();
#endif
}
