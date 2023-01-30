//===--- MoveOnlyAddressChecker.cpp ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Move Only Checking of Addresses
/// -------------------------------
///
/// In this file, we implement move checking of addresses. This allows for the
/// compiler to perform move checking of address only lets, vars, inout args,
/// and mutating self.
///
/// Algorithm At a High Level
/// -------------------------
///
/// At a high level, this algorithm can be conceptualized as attempting to
/// completely classify the memory behavior of the recursive uses of a move only
/// marked address and then seek to transform those uses such that they are in
/// "simple move only address" form. We define "simple move only address" to
/// mean that along any path from an init of an address to a consume, all uses
/// are guaranteed to be semantically "borrow uses". If we find that any such
/// owned uses can not be turned into a "borrow" use, then we emit an error
/// since we would need to insert a copy there.
///
/// To implement this, our algorithm works in 4 stages: a use classification
/// stage, a dataflow stage, and then depending on success/failure one of two
/// transform stagesWe describe them below.
///
/// Use Classification Stage
/// ~~~~~~~~~~~~~~~~~~~~~~~~
///
/// Here we use an AccessPath based analysis to transitively visit all uses of
/// our marked address and classify a use as one of the following kinds of uses:
///
/// * init - store [init], copy_addr [init] %dest.
/// * destroy - destroy_addr.
/// * pureTake - load [take], copy_addr [take] %src.
/// * copyTransformableToTake - certain load [copy], certain copy_addr ![take]
/// %src of a temporary %dest.
/// * reinit - store [assign], copy_addr ![init] %dest
/// * borrow - load_borror, a load [copy] without consuming uses.
/// * livenessOnly - a read only use of the address.
///
/// We classify these by adding them to several disjoint SetVectors which track
/// membership.
///
/// When we classify an instruction as copyTransformableToTake, we perform some
/// extra preprocessing to determine if we can actually transform this copy to a
/// take. This means that we:
///
/// 1. For loads, we perform object move only checking. If we find a need for
/// multiple copies, we emit an error. If we find no extra copies needed, we
/// classify the load [copy] as a take if it has any last consuming uses and a
/// borrow if it only has destroy_addr consuming uses.
///
/// 2. For copy_addr, we pattern match if a copy_addr is initializing a "simple
/// temporary" (an alloc_stack with only one use that initializes it, a
/// copy_addr [init] in the same block). In this case, if the copy_addr only has
/// destroy_addr consuming uses, we treat it as a borrow... otherwise, we treat
/// it as a take. If we find any extra initializations, we fail the visitor so
/// we emit a "I don't understand this error" so that users report this case and
/// we can extend it as appropriate.
///
/// If we fail in either case, if we emit an error, we bail early with success
/// so we can assume invariants later in the dataflow stages that make the
/// dataflow easier.
///
/// Dataflow Stage
/// ~~~~~~~~~~~~~~
///
/// To perform our dataflow, we do the following:
///
/// 1. We walk each block from top to bottom performing the single block version
/// of the algorithm and preparing field sensitive pruned liveness.
///
/// 2. If we need to, we then use field sensitive pruned liveness to perform
/// global dataflow to determine if any of our takeOrCopies are within the
/// boundary lifetime implying a violation.
///
/// Success Transformation
/// ~~~~~~~~~~~~~~~~~~~~~~
///
/// Upon success Now that we know that we can change our address into "simple
/// move only address form", we transform the IR in the following way:
///
/// 1. Any load [copy] that are classified as borrows are changed to
/// load_borrow.
/// 2. Any load [copy] that are classified as takes are changed to load [take].
/// 3. Any copy_addr [init] temporary allocation are eliminated with their
///    destroy_addr. All uses are placed on the source address.
/// 4. Any destroy_addr that is paired with a copyTransformableToTake is
///    eliminated.
///
/// Fail Transformation
/// ~~~~~~~~~~~~~~~~~~~
///
/// If we emit any diagnostics, we loop through the function one last time after
/// we are done processing and convert all load [copy]/copy_addr of move only
/// types into their explicit forms. We take a little more compile time, but we
/// are going to fail anyways at this point, so it is ok to do so since we will
/// fail before attempting to codegen into LLVM IR.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-move-only-checker"

#include "swift/AST/AccessScope.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/SmallBitVector.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/Consumption.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILArgumentConvention.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/NonLocalAccessBlockAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalizeOSSALifetime.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include "MoveOnlyDiagnostics.h"
#include "MoveOnlyObjectChecker.h"

#include <utility>

using namespace swift;
using namespace swift::siloptimizer;

//===----------------------------------------------------------------------===//
//                           MARK: Memory Utilities
//===----------------------------------------------------------------------===//

static bool memInstMustInitialize(Operand *memOper) {
  SILValue address = memOper->get();

  SILInstruction *memInst = memOper->getUser();

  switch (memInst->getKind()) {
  default:
    return false;

  case SILInstructionKind::CopyAddrInst: {
    auto *CAI = cast<CopyAddrInst>(memInst);
    return CAI->getDest() == address && CAI->isInitializationOfDest();
  }
  case SILInstructionKind::ExplicitCopyAddrInst: {
    auto *CAI = cast<ExplicitCopyAddrInst>(memInst);
    return CAI->getDest() == address && CAI->isInitializationOfDest();
  }
  case SILInstructionKind::MarkUnresolvedMoveAddrInst: {
    return cast<MarkUnresolvedMoveAddrInst>(memInst)->getDest() == address;
  }
  case SILInstructionKind::InitExistentialAddrInst:
  case SILInstructionKind::InitEnumDataAddrInst:
  case SILInstructionKind::InjectEnumAddrInst:
    return true;

  case SILInstructionKind::BeginApplyInst:
  case SILInstructionKind::TryApplyInst:
  case SILInstructionKind::ApplyInst: {
    FullApplySite applySite(memInst);
    return applySite.isIndirectResultOperand(*memOper);
  }
  case SILInstructionKind::StoreInst: {
    auto qual = cast<StoreInst>(memInst)->getOwnershipQualifier();
    return qual == StoreOwnershipQualifier::Init ||
           qual == StoreOwnershipQualifier::Trivial;
  }

#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)             \
  case SILInstructionKind::Store##Name##Inst:                                  \
    return cast<Store##Name##Inst>(memInst)->isInitializationOfDest();
#include "swift/AST/ReferenceStorage.def"
  }
}

static bool memInstMustReinitialize(Operand *memOper) {
  SILValue address = memOper->get();

  SILInstruction *memInst = memOper->getUser();

  switch (memInst->getKind()) {
  default:
    return false;

  case SILInstructionKind::CopyAddrInst: {
    auto *CAI = cast<CopyAddrInst>(memInst);
    return CAI->getDest() == address && !CAI->isInitializationOfDest();
  }
  case SILInstructionKind::ExplicitCopyAddrInst: {
    auto *CAI = cast<ExplicitCopyAddrInst>(memInst);
    return CAI->getDest() == address && !CAI->isInitializationOfDest();
  }
  case SILInstructionKind::YieldInst: {
    auto *yield = cast<YieldInst>(memInst);
    return yield->getYieldInfoForOperand(*memOper).isIndirectInOut();
  }
  case SILInstructionKind::BeginApplyInst:
  case SILInstructionKind::TryApplyInst:
  case SILInstructionKind::ApplyInst: {
    FullApplySite applySite(memInst);
    return applySite.getArgumentOperandConvention(*memOper).isInoutConvention();
  }
  case SILInstructionKind::StoreInst:
    return cast<StoreInst>(memInst)->getOwnershipQualifier() ==
           StoreOwnershipQualifier::Assign;

#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)             \
  case SILInstructionKind::Store##Name##Inst:                                  \
    return !cast<Store##Name##Inst>(memInst)->isInitializationOfDest();
#include "swift/AST/ReferenceStorage.def"
  }
}

/// Is this a reinit instruction that we know how to convert into its init form.
static bool isReinitToInitConvertibleInst(SILInstruction *memInst) {
  switch (memInst->getKind()) {
  default:
    return false;

  case SILInstructionKind::CopyAddrInst: {
    auto *cai = cast<CopyAddrInst>(memInst);
    return !cai->isInitializationOfDest();
  }
  case SILInstructionKind::StoreInst: {
    auto *si = cast<StoreInst>(memInst);
    return si->getOwnershipQualifier() == StoreOwnershipQualifier::Assign;
  }
  }
}

static void convertMemoryReinitToInitForm(SILInstruction *memInst) {
  switch (memInst->getKind()) {
  default:
    llvm_unreachable("unsupported?!");

  case SILInstructionKind::CopyAddrInst: {
    auto *cai = cast<CopyAddrInst>(memInst);
    cai->setIsInitializationOfDest(IsInitialization_t::IsInitialization);
    return;
  }
  case SILInstructionKind::StoreInst: {
    auto *si = cast<StoreInst>(memInst);
    si->setOwnershipQualifier(StoreOwnershipQualifier::Init);
    return;
  }
  }
}

static bool memInstMustConsume(Operand *memOper) {
  SILValue address = memOper->get();

  SILInstruction *memInst = memOper->getUser();

  switch (memInst->getKind()) {
  default:
    return false;

  case SILInstructionKind::CopyAddrInst: {
    auto *CAI = cast<CopyAddrInst>(memInst);
    return (CAI->getSrc() == address && CAI->isTakeOfSrc()) ||
           (CAI->getDest() == address && !CAI->isInitializationOfDest());
  }
  case SILInstructionKind::ExplicitCopyAddrInst: {
    auto *CAI = cast<CopyAddrInst>(memInst);
    return (CAI->getSrc() == address && CAI->isTakeOfSrc()) ||
           (CAI->getDest() == address && !CAI->isInitializationOfDest());
  }
  case SILInstructionKind::BeginApplyInst:
  case SILInstructionKind::TryApplyInst:
  case SILInstructionKind::ApplyInst: {
    FullApplySite applySite(memInst);
    return applySite.getArgumentOperandConvention(*memOper).isOwnedConvention();
  }
  case SILInstructionKind::PartialApplyInst: {
    // If we are on the stack, we do not consume. Otherwise, we do.
    return !cast<PartialApplyInst>(memInst)->isOnStack();
  }
  case SILInstructionKind::DestroyAddrInst:
    return true;
  case SILInstructionKind::LoadInst:
    return cast<LoadInst>(memInst)->getOwnershipQualifier() ==
           LoadOwnershipQualifier::Take;
  }
}

/// Returns true if \p value a function argument from an inout argument or a
/// value extracted from a closure captured box that we did not convert to an
/// address.
///
/// These are cases where we want to treat the end of the function as a liveness
/// use to ensure that we reinitialize \p value before the end of the function
/// if we consume \p value in the function body.
static bool isInOutDefThatNeedsEndOfFunctionLiveness(SILValue value) {
  if (auto *fArg = dyn_cast<SILFunctionArgument>(value)) {
    switch (fArg->getArgumentConvention()) {
    case SILArgumentConvention::Indirect_In:
    case SILArgumentConvention::Indirect_Out:
    case SILArgumentConvention::Indirect_In_Guaranteed:
    case SILArgumentConvention::Direct_Guaranteed:
    case SILArgumentConvention::Direct_Owned:
    case SILArgumentConvention::Direct_Unowned:
      return false;
    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_InoutAliasable:
      LLVM_DEBUG(llvm::dbgs() << "Found inout arg: " << *fArg);
      return true;
    }
  }

  if (auto *pbi = dyn_cast<ProjectBoxInst>(value)) {
    if (auto *fArg = dyn_cast<SILFunctionArgument>(pbi->getOperand())) {
      if (!fArg->isClosureCapture())
        return false;
      LLVM_DEBUG(llvm::dbgs() << "Found inout arg: " << *fArg);
      return true;
    }
  }

  return false;
}

//===----------------------------------------------------------------------===//
//                              MARK: Use State
//===----------------------------------------------------------------------===//

namespace {

struct UseState {
  MarkMustCheckInst *address;

  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> destroys;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> livenessUses;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> borrows;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> takeInsts;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> copyInsts;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> initInsts;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> reinitInsts;
  SmallSetVector<SILInstruction *, 2> inoutTermUsers;

  SILFunction *getFunction() const { return address->getFunction(); }

  /// Returns true if this is a terminator instruction that although it doesn't
  /// use our inout argument directly is used by the pass to ensure that we
  /// reinit said argument if we consumed it in the body of the function.
  bool isInOutTermUser(SILInstruction *inst) const {
    return inoutTermUsers.count(inst);
  }

  void clear() {
    address = nullptr;
    destroys.clear();
    livenessUses.clear();
    borrows.clear();
    copyInsts.clear();
    takeInsts.clear();
    initInsts.clear();
    reinitInsts.clear();
    inoutTermUsers.clear();
  }

  void dump() {
    llvm::dbgs() << "AddressUseState!\n";
    llvm::dbgs() << "Destroys:\n";
    for (auto pair : destroys) {
      llvm::dbgs() << *pair.first;
    }
    llvm::dbgs() << "LivenessUses:\n";
    for (auto pair : livenessUses) {
      llvm::dbgs() << *pair.first;
    }
    llvm::dbgs() << "Borrows:\n";
    for (auto pair : borrows) {
      llvm::dbgs() << *pair.first;
    }
    llvm::dbgs() << "Takes:\n";
    for (auto pair : takeInsts) {
      llvm::dbgs() << *pair.first;
    }
    llvm::dbgs() << "Copies:\n";
    for (auto pair : copyInsts) {
      llvm::dbgs() << *pair.first;
    }
    llvm::dbgs() << "Inits:\n";
    for (auto pair : initInsts) {
      llvm::dbgs() << *pair.first;
    }
    llvm::dbgs() << "Reinits:\n";
    for (auto pair : reinitInsts) {
      llvm::dbgs() << *pair.first;
    }
    llvm::dbgs() << "InOut Term Users:\n";
    for (auto *inst : inoutTermUsers) {
      llvm::dbgs() << *inst;
    }
  }

  void
  initializeLiveness(FieldSensitiveMultiDefPrunedLiveRange &prunedLiveness);

  void initializeInOutTermUsers() {
    if (!isInOutDefThatNeedsEndOfFunctionLiveness(address->getOperand()))
      return;

    SmallVector<SILBasicBlock *, 8> exitBlocks;
    address->getFunction()->findExitingBlocks(exitBlocks);
    for (auto *block : exitBlocks) {
      LLVM_DEBUG(llvm::dbgs() << "    Adding term as liveness user: "
                              << *block->getTerminator());
      inoutTermUsers.insert(block->getTerminator());
    }
  }

  bool isConsume(SILInstruction *inst, TypeTreeLeafTypeRange span) const {
    {
      auto iter = takeInsts.find(inst);
      if (iter != takeInsts.end()) {
        if (span.setIntersection(iter->second))
          return true;
      }
    }
    {
      auto iter = copyInsts.find(inst);
      if (iter != copyInsts.end()) {
        if (span.setIntersection(iter->second))
          return true;
      }
    }
    return false;
  }

  bool isCopy(SILInstruction *inst, const SmallBitVector &bv) const {
    auto iter = copyInsts.find(inst);
    if (iter != copyInsts.end()) {
      for (unsigned index : iter->second.getRange()) {
        if (bv[index])
          return true;
      }
    }
    return false;
  }

  bool isLivenessUse(SILInstruction *inst, TypeTreeLeafTypeRange span) const {
    {
      auto iter = livenessUses.find(inst);
      if (iter != livenessUses.end()) {
        if (span.setIntersection(iter->second))
          return true;
      }
    }
    {
      auto iter = borrows.find(inst);
      if (iter != borrows.end()) {
        if (span.setIntersection(iter->second))
          return true;
      }
    }

    if (!isReinitToInitConvertibleInst(inst)) {
      auto iter = reinitInsts.find(inst);
      if (iter != reinitInsts.end()) {
        if (span.setIntersection(iter->second))
          return true;
      }
    }

    // An "inout terminator use" is an implicit liveness use of the entire
    // value. This is because we need to ensure that our inout value is
    // reinitialized along exit paths.
    if (inoutTermUsers.count(inst))
      return true;

    return false;
  }

  bool isInitUse(SILInstruction *inst, TypeTreeLeafTypeRange span) const {
    {
      auto iter = initInsts.find(inst);
      if (iter != initInsts.end()) {
        if (span.setIntersection(iter->second))
          return true;
      }
    }
    if (isReinitToInitConvertibleInst(inst)) {
      auto iter = reinitInsts.find(inst);
      if (iter != reinitInsts.end()) {
        if (span.setIntersection(iter->second))
          return true;
      }
    }
    return false;
  }
};

} // namespace

void UseState::initializeLiveness(
    FieldSensitiveMultiDefPrunedLiveRange &liveness) {
  // We begin by initializing all of our init uses.
  for (auto initInstAndValue : initInsts) {
    liveness.initializeDef(initInstAndValue.first, initInstAndValue.second);
  }

  // If we have a reinitInstAndValue that we are going to be able to convert
  // into a simple init, add it as an init. We are going to consider the rest of
  // our reinit uses to be liveness uses.
  for (auto reinitInstAndValue : reinitInsts) {
    if (isReinitToInitConvertibleInst(reinitInstAndValue.first))
      liveness.initializeDef(reinitInstAndValue.first,
                             reinitInstAndValue.second);
  }

  // Then check if our markedValue is from an argument that is in,
  // in_guaranteed, inout, or inout_aliasable, consider the marked address to be
  // the initialization point.
  if (auto *fArg = dyn_cast<SILFunctionArgument>(address->getOperand())) {
    switch (fArg->getArgumentConvention()) {
    case swift::SILArgumentConvention::Indirect_In:
    case swift::SILArgumentConvention::Indirect_In_Guaranteed:
    case swift::SILArgumentConvention::Indirect_Inout:
    case swift::SILArgumentConvention::Indirect_InoutAliasable:
      // We need to add our address to the initInst array to make sure that
      // later invariants that we assert upon remain true.
      LLVM_DEBUG(llvm::dbgs()
                 << "Found in/in_guaranteed/inout/inout_aliasable argument as "
                    "an init... adding mark_must_check as init!\n");
      initInsts.insert({address, liveness.getTopLevelSpan()});
      liveness.initializeDef(address, liveness.getTopLevelSpan());
      break;
    case swift::SILArgumentConvention::Indirect_Out:
      llvm_unreachable("Should never have out addresses here");
    case swift::SILArgumentConvention::Direct_Owned:
    case swift::SILArgumentConvention::Direct_Unowned:
    case swift::SILArgumentConvention::Direct_Guaranteed:
      llvm_unreachable("Working with addresses");
    }
  }

  // See if our address is from a closure guaranteed box that we did not promote
  // to an address. In such a case, just treat our mark_must_check as the init
  // of our value.
  if (auto *projectBox = dyn_cast<ProjectBoxInst>(address->getOperand())) {
    if (auto *fArg = dyn_cast<SILFunctionArgument>(projectBox->getOperand())) {
      if (fArg->isClosureCapture()) {
        assert(fArg->getArgumentConvention() ==
                   SILArgumentConvention::Direct_Guaranteed &&
               "Just a paranoid assert check to make sure this code is thought "
               "about if we change the convention in some way");
        // We need to add our address to the initInst array to make sure that
        // later invariants that we assert upon remain true.
        LLVM_DEBUG(llvm::dbgs() << "Found move only arg closure box use... "
                                   "adding mark_must_check as init!\n");
        initInsts.insert({address, liveness.getTopLevelSpan()});
        liveness.initializeDef(address, liveness.getTopLevelSpan());
      }
    }
  }

  // Now that we have finished initialization of defs, change our multi-maps
  // from their array form to their map form.
  liveness.finishedInitializationOfDefs();

  LLVM_DEBUG(llvm::dbgs() << "Liveness with just inits:\n";
             liveness.print(llvm::dbgs()));

  // Now at this point, we have defined all of our defs so we can start adding
  // uses to the liveness.
  for (auto reinitInstAndValue : reinitInsts) {
    if (!isReinitToInitConvertibleInst(reinitInstAndValue.first)) {
      liveness.updateForUse(reinitInstAndValue.first, reinitInstAndValue.second,
                            false /*lifetime ending*/);
      LLVM_DEBUG(llvm::dbgs() << "Added liveness for reinit: "
                              << *reinitInstAndValue.first;
                 liveness.print(llvm::dbgs()));
    }
  }

  // Then add all of the takes that we saw propagated up to the top of our
  // block. Since we have done this for all of our defs
  for (auto takeInstAndValue : takeInsts) {
    liveness.updateForUse(takeInstAndValue.first, takeInstAndValue.second,
                          true /*lifetime ending*/);
    LLVM_DEBUG(llvm::dbgs()
                   << "Added liveness for take: " << *takeInstAndValue.first;
               liveness.print(llvm::dbgs()));
  }
  for (auto copyInstAndValue : copyInsts) {
    liveness.updateForUse(copyInstAndValue.first, copyInstAndValue.second,
                          true /*lifetime ending*/);
    LLVM_DEBUG(llvm::dbgs()
                   << "Added liveness for copy: " << *copyInstAndValue.first;
               liveness.print(llvm::dbgs()));
  }

  // Do the same for our borrow and liveness insts.
  for (auto livenessInstAndValue : borrows) {
    liveness.updateForUse(livenessInstAndValue.first,
                          livenessInstAndValue.second,
                          false /*lifetime ending*/);
    auto *li = cast<LoadInst>(livenessInstAndValue.first);
    auto accessPathWithBase =
        AccessPathWithBase::computeInScope(li->getOperand());
    if (auto *beginAccess =
            dyn_cast<BeginAccessInst>(accessPathWithBase.base)) {
      for (auto *endAccess : beginAccess->getEndAccesses()) {
        liveness.updateForUse(endAccess, livenessInstAndValue.second,
                              false /*lifetime ending*/);
      }
    } else {
      for (auto *ebi : li->getConsumingUses()) {
        liveness.updateForUse(ebi->getUser(), livenessInstAndValue.second,
                              false /*lifetime ending*/);
      }
    }
    LLVM_DEBUG(llvm::dbgs() << "Added liveness for borrow: "
                            << *livenessInstAndValue.first;
               liveness.print(llvm::dbgs()));
  }

  for (auto livenessInstAndValue : livenessUses) {
    if (auto *lbi = dyn_cast<LoadBorrowInst>(livenessInstAndValue.first)) {
      auto accessPathWithBase =
          AccessPathWithBase::computeInScope(lbi->getOperand());
      if (auto *beginAccess =
              dyn_cast<BeginAccessInst>(accessPathWithBase.base)) {
        for (auto *endAccess : beginAccess->getEndAccesses()) {
          liveness.updateForUse(endAccess, livenessInstAndValue.second,
                                false /*lifetime ending*/);
        }
      } else {
        for (auto *ebi : lbi->getEndBorrows()) {
          liveness.updateForUse(ebi, livenessInstAndValue.second,
                                false /*lifetime ending*/);
        }
      }
    } else {
      liveness.updateForUse(livenessInstAndValue.first,
                            livenessInstAndValue.second,
                            false /*lifetime ending*/);
    }
    LLVM_DEBUG(llvm::dbgs() << "Added liveness for livenessInst: "
                            << *livenessInstAndValue.first;
               liveness.print(llvm::dbgs()));
  }

  // Finally, if we have an inout argument, add a liveness use of the entire
  // value on terminators in blocks that are exits from the function. This
  // ensures that along all paths, if our inout is not reinitialized before we
  // exit the function, we will get an error. We also stash these users into
  // inoutTermUser so we can quickly recognize them later and emit a better
  // error msg.
  for (auto *inst : inoutTermUsers) {
    liveness.updateForUse(inst, TypeTreeLeafTypeRange(address),
                          false /*lifetime ending*/);
    LLVM_DEBUG(llvm::dbgs() << "Added liveness for inoutTermUser: " << *inst;
               liveness.print(llvm::dbgs()));
  }

  LLVM_DEBUG(llvm::dbgs() << "Final Liveness:\n"; liveness.print(llvm::dbgs()));
}

//===----------------------------------------------------------------------===//
//                          MARK: Global Block State
//===----------------------------------------------------------------------===//

namespace {

struct BlockState {
  using Map = llvm::DenseMap<SILBasicBlock *, BlockState>;

  /// This is either the liveness up or take up inst that projects
  /// up. We set this state according to the following rules:
  ///
  /// 1. If we are tracking a takeUp, we always take it even if we have a
  /// livenessUp.
  ///
  /// 2. If we have a livenessUp and do not have a take up, we track that
  /// instead.
  ///
  /// The reason why we do this is that we want to catch use after frees when
  /// non-consuming uses are later than a consuming use.
  SILInstruction *userUp;

  /// If we are init down, then we know that we can not transfer our take
  /// through this block and should stop traversing.
  bool isInitDown;

  BlockState() : userUp(nullptr) {}

  BlockState(SILInstruction *userUp, bool isInitDown)
      : userUp(userUp), isInitDown(isInitDown) {}
};

} // namespace

//===----------------------------------------------------------------------===//
//                 MARK: Forward Declaration of Main Checker
//===----------------------------------------------------------------------===//

namespace {

struct ConsumeInfo {
  /// Map blocks on the lifetime boundary to the last consuming instruction.
  llvm::MapVector<
      SILBasicBlock *,
      SmallVector<std::pair<SILInstruction *, TypeTreeLeafTypeRange>, 1>>
      finalBlockConsumes;

  bool isFrozen = false;

public:
  void print(llvm::raw_ostream &os) const {
    for (auto &blockInstRangePairVector : finalBlockConsumes) {
      os << "Dumping state for block bb"
         << blockInstRangePairVector.first->getDebugID() << '\n';
      for (auto &instRangePairVector : blockInstRangePairVector.second) {
        auto *inst = instRangePairVector.first;
        if (!inst)
          continue;
        os << "Inst: " << *inst;
        os << "Range: ";
        instRangePairVector.second.dump();
        os << '\n';
      }
    }
  }

  void clear() {
    finalBlockConsumes.clear();
    isFrozen = false;
  }

  /// This is expensive! Only use it in debug mode!
  bool hasUnclaimedConsumes() const {
    assert(isFrozen);
    bool foundAny = false;
    for (auto range : finalBlockConsumes) {
      for (auto elt : range.second) {
        foundAny |= bool(elt.first);
      }
    }
    return foundAny;
  }

  void recordFinalConsume(SILInstruction *inst, TypeTreeLeafTypeRange span) {
    assert(!isFrozen);
    auto iter = finalBlockConsumes.insert({inst->getParent(), {{inst, span}}});
    if (iter.second)
      return;
    LLVM_DEBUG(llvm::dbgs() << "Recorded Final Consume: " << *inst);
    iter.first->second.emplace_back(inst, span);
  }

  void finishRecordingFinalConsumes() {
    assert(!isFrozen);
    for (auto &pair : finalBlockConsumes) {
      llvm::stable_sort(
          pair.second,
          [](const std::pair<SILInstruction *, TypeTreeLeafTypeRange> &lhs,
             const std::pair<SILInstruction *, TypeTreeLeafTypeRange> &rhs) {
            return lhs.first < rhs.first;
          });
    }
    isFrozen = true;

    LLVM_DEBUG(llvm::dbgs() << "Final recorded consumes!\n";
               print(llvm::dbgs()));
  }

  // Return true if this instruction is marked as a final consume point of the
  // current def's live range. A consuming instruction can only be claimed once
  // because instructions like `tuple` can consume the same value via multiple
  // operands.
  //
  // Can only be used once frozen.
  bool claimConsume(SILInstruction *inst, TypeTreeLeafTypeRange range) {
    assert(isFrozen);

    bool claimedConsume = false;

    auto &iter = finalBlockConsumes[inst->getParent()];
    for (unsigned i : indices(iter)) {
      auto &instRangePair = iter[i];
      if (instRangePair.first == inst && instRangePair.second == range) {
        instRangePair.first = nullptr;
        claimedConsume = true;
        LLVM_DEBUG(llvm::dbgs() << "Claimed consume: " << *inst);
      }
    }

    return claimedConsume;
  }

  ConsumeInfo() {}
  ConsumeInfo(CanonicalOSSAConsumeInfo const &) = delete;
  ConsumeInfo &operator=(ConsumeInfo const &) = delete;
};

struct MoveOnlyChecker {
  bool changed = false;

  SILFunction *fn;

  /// A set of mark_must_check that we are actually going to process.
  SmallSetVector<MarkMustCheckInst *, 32> moveIntroducersToProcess;

  /// The instruction deleter used by \p canonicalizer.
  InstructionDeleter deleter;

  /// State to run CanonicalizeOSSALifetime.
  OSSACanonicalizer canonicalizer;

  /// Per mark must check address use state.
  UseState addressUseState;

  /// Diagnostic emission routines wrapped around a consuming use cache. This
  /// ensures that we only emit a single error per use per marked value.
  DiagnosticEmitter diagnosticEmitter;

  /// Information about destroys that we use when inserting destroys.
  ConsumeInfo consumes;

  MoveOnlyChecker(SILFunction *fn, DeadEndBlocks *deBlocks,
                  NonLocalAccessBlockAnalysis *accessBlockAnalysis,
                  DominanceInfo *domTree)
      : fn(fn), deleter(), canonicalizer(), diagnosticEmitter() {
    deleter.setCallbacks(std::move(
        InstModCallbacks().onDelete([&](SILInstruction *instToDelete) {
          if (auto *mvi = dyn_cast<MarkMustCheckInst>(instToDelete))
            moveIntroducersToProcess.remove(mvi);
          instToDelete->eraseFromParent();
        })));
    canonicalizer.init(fn, accessBlockAnalysis, domTree, deleter);
    diagnosticEmitter.init(fn, &canonicalizer);
  }

  /// Search through the current function for candidate mark_must_check
  /// [noimplicitcopy]. If we find one that does not fit a pattern that we
  /// understand, emit an error diagnostic telling the programmer that the move
  /// checker did not know how to recognize this code pattern.
  ///
  /// Returns true if we emitted a diagnostic. Returns false otherwise.
  bool searchForCandidateMarkMustChecks();

  /// After we have emitted a diagnostic, we need to clean up the instruction
  /// stream by converting /all/ copies of move only typed things to use
  /// explicit_copy_value so that we maintain the SIL invariant that in
  /// canonical SIL move only types are not copied by normal copies.
  ///
  /// Returns true if we actually changed any instructions.
  void cleanupAfterEmittingDiagnostic();

  /// Emits an error diagnostic for \p markedValue.
  void performObjectCheck(MarkMustCheckInst *markedValue);

  bool performSingleCheck(MarkMustCheckInst *markedValue);

  bool checkFunction();

  void insertDestroysOnBoundary(FieldSensitiveMultiDefPrunedLiveRange &liveness,
                                FieldSensitivePrunedLivenessBoundary &boundary);

  void rewriteUses(FieldSensitiveMultiDefPrunedLiveRange &liveness,
                   const FieldSensitivePrunedLivenessBoundary &boundary);

  void handleSingleBlockDestroy(SILInstruction *destroy, bool isReinit);
};

} // namespace

//===----------------------------------------------------------------------===//
//                   MARK: GatherLexicalLifetimeUseVisitor
//===----------------------------------------------------------------------===//

namespace {

/// Visit all of the uses of value in preparation for running our algorithm.
struct GatherUsesVisitor : public AccessUseVisitor {
  MoveOnlyChecker &moveChecker;
  UseState &useState;
  MarkMustCheckInst *markedValue;
  bool emittedEarlyDiagnostic = false;
  DiagnosticEmitter &diagnosticEmitter;

  // Pruned liveness used to validate that load [take]/load [copy] can be
  // converted to load_borrow without violating exclusivity.
  SSAPrunedLiveness &liveness;

  GatherUsesVisitor(MoveOnlyChecker &moveChecker, UseState &useState,
                    MarkMustCheckInst *markedValue,
                    DiagnosticEmitter &diagnosticEmitter,
                    SSAPrunedLiveness &gatherUsesLiveness)
      : AccessUseVisitor(AccessUseType::Overlapping,
                         NestedAccessType::IgnoreAccessBegin),
        moveChecker(moveChecker), useState(useState), markedValue(markedValue),
        diagnosticEmitter(diagnosticEmitter), liveness(gatherUsesLiveness) {}

  bool visitUse(Operand *op, AccessUseType useTy) override;
  void reset(MarkMustCheckInst *address) { useState.address = address; }
  void clear() { useState.clear(); }

  /// For now always markedValue. If we start using this for move address
  /// checking, we need to check against the operand of the markedValue. This is
  /// because for move checking, our marker is placed along the variables
  /// initialization so we are always going to have all later uses from the
  /// marked value. For the move operator though we will want this to be the
  /// base address that we are checking which should be the operand of the mark
  /// must check value.
  SILValue getRootAddress() const { return markedValue; }

  /// Returns true if we emitted an error.
  bool checkForExclusivityHazards(LoadInst *li) {
    SWIFT_DEFER { liveness.clear(); };

    LLVM_DEBUG(llvm::dbgs() << "Checking for exclusivity hazards for: " << *li);

    // Grab our access path with in scope. We want to find the inner most access
    // scope.
    auto accessPathWithBase =
        AccessPathWithBase::computeInScope(li->getOperand());
    auto accessPath = accessPathWithBase.accessPath;
    // TODO: Make this a we don't understand error.
    assert(accessPath.isValid() && "Invalid access path?!");

    auto *bai = dyn_cast<BeginAccessInst>(accessPathWithBase.base);

    if (!bai) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    No begin access... so no exclusivity violation!\n");
      return false;
    }

    bool emittedError = false;
    liveness.initializeDef(bai);
    liveness.computeSimple();
    for (auto *consumingUse : li->getConsumingUses()) {
      if (!liveness.isWithinBoundary(consumingUse->getUser())) {
        diagnosticEmitter.emitAddressExclusivityHazardDiagnostic(
            markedValue, consumingUse->getUser());
        emittedError = true;
      }
    }
    return emittedError;
  }
};

} // end anonymous namespace

// Filter out recognized uses that do not write to memory.
//
// TODO: Ensure that all of the conditional-write logic below is encapsulated in
// mayWriteToMemory and just call that instead. Possibly add additional
// verification that visitAccessPathUses recognizes all instructions that may
// propagate pointers (even though they don't write).
bool GatherUsesVisitor::visitUse(Operand *op, AccessUseType useTy) {
  // If this operand is for a dependent type, then it does not actually access
  // the operand's address value. It only uses the metatype defined by the
  // operation (e.g. open_existential).
  if (op->isTypeDependent()) {
    return true;
  }

  // We don't care about debug instructions.
  if (op->getUser()->isDebugInstruction())
    return true;

  // For convenience, grab the user of op.
  auto *user = op->getUser();

  // First check if we have init/reinit. These are quick/simple.
  if (::memInstMustInitialize(op)) {
    LLVM_DEBUG(llvm::dbgs() << "Found init: " << *user);

    // TODO: What about copy_addr of itself. We really should just pre-process
    // those maybe.
    auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
    if (!leafRange)
      return false;

    assert(!useState.initInsts.count(user));
    useState.initInsts.insert({user, *leafRange});
    return true;
  }

  if (::memInstMustReinitialize(op)) {
    LLVM_DEBUG(llvm::dbgs() << "Found reinit: " << *user);
    assert(!useState.reinitInsts.count(user));
    auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
    if (!leafRange)
      return false;
    useState.reinitInsts.insert({user, *leafRange});
    return true;
  }

  // Then handle destroy_addr specially. We want to as part of our dataflow to
  // ignore destroy_addr, so we need to track it separately from other uses.
  if (auto *dvi = dyn_cast<DestroyAddrInst>(user)) {
    // If we see a destroy_addr not on our base address, bail! Just error and
    // say that we do not understand the code.
    if (dvi->getOperand() != useState.address) {
      LLVM_DEBUG(llvm::dbgs()
                 << "!!! Error! Found destroy_addr no on base address: "
                 << *useState.address << "destroy: " << *dvi);
      return false;
    }

    LLVM_DEBUG(llvm::dbgs() << "Found destroy_addr: " << *dvi);
    auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
    if (!leafRange)
      return false;

    useState.destroys.insert({dvi, *leafRange});
    return true;
  }

  // Ignore dealloc_stack.
  if (isa<DeallocStackInst>(user))
    return true;

  // Ignore end_access.
  if (isa<EndAccessInst>(user))
    return true;

  // At this point, we have handled all of the non-loadTakeOrCopy/consuming
  // uses.
  if (auto *copyAddr = dyn_cast<CopyAddrInst>(user)) {
    assert(op->getOperandNumber() == CopyAddrInst::Src &&
           "Should have dest above in memInstMust{Rei,I}nitialize");

    if (markedValue->getCheckKind() == MarkMustCheckInst::CheckKind::NoCopy) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Found mark must check [nocopy] error: " << *user);
      diagnosticEmitter.emitAddressDiagnosticNoCopy(markedValue, copyAddr);
      emittedEarlyDiagnostic = true;
      return true;
    }

    auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
    if (!leafRange)
      return false;

    if (copyAddr->isTakeOfSrc()) {
      LLVM_DEBUG(llvm::dbgs() << "Found take: " << *user);
      useState.takeInsts.insert({user, *leafRange});
    } else {
      LLVM_DEBUG(llvm::dbgs() << "Found copy: " << *user);
      useState.copyInsts.insert({user, *leafRange});
    }
    return true;
  }

  // Then find load [copy], load [take] that are really takes since we need
  // copies for the loaded value. If we find that we need copies at that level
  // (due to e.x.: multiple consuming uses), we emit an error and bail. This
  // ensures that later on, we can assume that all of our load [take], load
  // [copy] actually follow move semantics at the object level and thus are
  // viewed as a consume requiring a copy. This is important since SILGen often
  // emits code of this form and we need to recognize it as a copy of the
  // underlying var.
  if (auto *li = dyn_cast<LoadInst>(user)) {
    if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy ||
        li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
      SWIFT_DEFER { moveChecker.canonicalizer.clear(); };

      // Canonicalize the lifetime of the load [take], load [copy].
      moveChecker.changed |= moveChecker.canonicalizer.canonicalize(li);

      // If we are asked to perform guaranteed checking, emit an error if we
      // have /any/ consuming uses. This is a case that can always be converted
      // to a load_borrow if we pass the check.
      if (markedValue->getCheckKind() == MarkMustCheckInst::CheckKind::NoCopy) {
        if (!moveChecker.canonicalizer.foundAnyConsumingUses()) {
          LLVM_DEBUG(llvm::dbgs()
                     << "Found mark must check [nocopy] error: " << *user);
          moveChecker.diagnosticEmitter.emitObjectGuaranteedDiagnostic(
              markedValue);
          emittedEarlyDiagnostic = true;
          return true;
        }

        // If set, this will tell the checker that we can change this load into
        // a load_borrow.
        auto leafRange =
            TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
        if (!leafRange)
          return false;

        LLVM_DEBUG(llvm::dbgs() << "Found potential borrow: " << *user);

        if (checkForExclusivityHazards(li)) {
          LLVM_DEBUG(llvm::dbgs() << "Found exclusivity violation?!\n");
          emittedEarlyDiagnostic = true;
          return true;
        }

        useState.borrows.insert({user, *leafRange});
        return true;
      }

      // First check if we had any consuming uses that actually needed a
      // copy. This will always be an error and we allow the user to recompile
      // and eliminate the error. This just allows us to rely on invariants
      // later.
      if (moveChecker.canonicalizer.foundConsumingUseRequiringCopy()) {
        LLVM_DEBUG(llvm::dbgs()
                   << "Found that load at object level requires copies!\n");
        // If we failed to understand how to perform the check or did not find
        // any targets... continue. In the former case we want to fail with a
        // checker did not understand diagnostic later and in the former, we
        // succeeded.
        // Otherwise, emit the diagnostic.
        moveChecker.diagnosticEmitter.emitObjectOwnedDiagnostic(markedValue);
        emittedEarlyDiagnostic = true;
        LLVM_DEBUG(llvm::dbgs() << "Emitted early object level diagnostic.\n");
        return true;
      }

      // Then if we had any final consuming uses, mark that this liveness use is
      // a take/copy and if not, mark this as a borrow.
      auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
      if (!leafRange)
        return false;

      if (!moveChecker.canonicalizer.foundFinalConsumingUses()) {
        LLVM_DEBUG(llvm::dbgs() << "Found potential borrow inst: " << *user);
        if (checkForExclusivityHazards(li)) {
          LLVM_DEBUG(llvm::dbgs() << "Found exclusivity violation?!\n");
          emittedEarlyDiagnostic = true;
          return true;
        }

        useState.borrows.insert({user, *leafRange});
      } else {
        // If we had a load [copy], store this into the copy list. These are the
        // things that we must merge into destroy_addr or reinits after we are
        // done checking. The load [take] are already complete and good to go.
        if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
          LLVM_DEBUG(llvm::dbgs() << "Found take inst: " << *user);
          useState.takeInsts.insert({user, *leafRange});
        } else {
          LLVM_DEBUG(llvm::dbgs() << "Found copy inst: " << *user);
          useState.copyInsts.insert({user, *leafRange});
        }
      }
      return true;
    }
  }

  // Now that we have handled or loadTakeOrCopy, we need to now track our
  // additional pure takes.
  if (::memInstMustConsume(op)) {
    auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
    if (!leafRange)
      return false;
    LLVM_DEBUG(llvm::dbgs() << "Pure consuming use: " << *user);
    useState.takeInsts.insert({user, *leafRange});
    return true;
  }

  if (auto fas = FullApplySite::isa(op->getUser())) {
    switch (fas.getArgumentConvention(*op)) {
    case SILArgumentConvention::Indirect_In_Guaranteed: {
      auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
      if (!leafRange)
        return false;

      useState.livenessUses.insert({user, *leafRange});
      return true;
    }

    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_InoutAliasable:
    case SILArgumentConvention::Indirect_In:
    case SILArgumentConvention::Indirect_Out:
    case SILArgumentConvention::Direct_Unowned:
    case SILArgumentConvention::Direct_Owned:
    case SILArgumentConvention::Direct_Guaranteed:
      break;
    }
  }

  // If we don't fit into any of those categories, just track as a liveness
  // use. We assume all such uses must only be reads to the memory. So we assert
  // to be careful.
  auto leafRange = TypeTreeLeafTypeRange::get(op->get(), getRootAddress());
  if (!leafRange)
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Found liveness use: " << *user);
#ifndef NDEBUG
  if (user->mayWriteToMemory()) {
    llvm::errs() << "Found a write classified as a liveness use?!\n";
    llvm::errs() << "Use: " << *user;
    llvm_unreachable("standard failure");
  }
#endif
  useState.livenessUses.insert({user, *leafRange});

  return true;
}

//===----------------------------------------------------------------------===//
//                           MARK: Global Dataflow
//===----------------------------------------------------------------------===//

namespace {

using InstLeafTypePair = std::pair<SILInstruction *, TypeTreeLeafTypeRange>;
using InstOptionalLeafTypePair =
    std::pair<SILInstruction *, Optional<TypeTreeLeafTypeRange>>;

/// Post process the found liveness and emit errors if needed. TODO: Better
/// name.
struct GlobalLivenessChecker {
  UseState &addressUseState;
  DiagnosticEmitter &diagnosticEmitter;
  FieldSensitiveMultiDefPrunedLiveRange &liveness;
  SmallBitVector livenessVector;
  bool hadAnyErrorUsers = false;

  GlobalLivenessChecker(UseState &addressUseState,
                        DiagnosticEmitter &diagnosticEmitter,
                        FieldSensitiveMultiDefPrunedLiveRange &liveness)
      : addressUseState(addressUseState), diagnosticEmitter(diagnosticEmitter),
        liveness(liveness) {}

  /// Returns true if we emitted any errors.
  bool compute();

  bool testInstVectorLiveness(
      llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4>
          &instsToTest);

  void clear() {
    livenessVector.clear();
    hadAnyErrorUsers = false;
  }
};

} // namespace

bool GlobalLivenessChecker::testInstVectorLiveness(
    llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4>
        &instsToTest) {
  bool emittedDiagnostic = false;

  for (auto takeInstAndValue : instsToTest) {
    LLVM_DEBUG(llvm::dbgs() << "    Checking: " << *takeInstAndValue.first);

    // Check if we are in the boundary...

    // If the bit vector does not contain any set bits, then we know that we did
    // not have any boundary violations for any leaf node of our root value.
    if (!liveness.isWithinBoundary(takeInstAndValue.first,
                                   takeInstAndValue.second)) {
      // TODO: Today, we don't tell the user the actual field itself where the
      // violation occured and just instead just shows the two instructions. We
      // could be more specific though...
      LLVM_DEBUG(llvm::dbgs() << "        Not within the boundary.\n");
      continue;
    }
    LLVM_DEBUG(llvm::dbgs()
               << "        Within the boundary! Emitting an error\n");

    // Ok, we have an error and via the bit vector know which specific leaf
    // elements of our root type were within the per field boundary. We need to
    // go find the next reachable use that overlap with its sub-element. We only
    // emit a single error per use even if we get multiple sub elements that
    // match it. That helps reduce the amount of errors.
    //
    // DISCUSSION: It is important to note that this follows from the separation
    // of concerns behind this pass: we have simplified how we handle liveness
    // by losing this information. That being said, since we are erroring it is
    // ok that we are taking a little more time since we are not going to
    // codegen this code.
    //
    // That being said, set the flag that we saw at least one error, so we can
    // exit early after this loop.
    hadAnyErrorUsers = true;

    // B/c of the separation of concerns with our liveness, we now need to walk
    // blocks to go find the specific later takes that are reachable from this
    // take. It is ok that we are doing a bit more work here since we are going
    // to exit and not codegen.
    auto *errorUser = takeInstAndValue.first;
    auto errorSpan = takeInstAndValue.second;

    // First walk from errorUser to the end of the block, looking for a take or
    // a liveness use. If we find a single block error, emit the error and
    // continue.
    if (errorUser != errorUser->getParent()->getTerminator()) {
      bool foundSingleBlockError = false;
      for (auto ii = std::next(errorUser->getIterator()),
                ie = errorUser->getParent()->end();
           ii != ie; ++ii) {
        if (addressUseState.isConsume(&*ii, errorSpan)) {
          diagnosticEmitter.emitAddressDiagnostic(
              addressUseState.address, &*ii, errorUser, true /*is consuming*/);
          foundSingleBlockError = true;
          emittedDiagnostic = true;
          break;
        }

        if (addressUseState.isLivenessUse(&*ii, errorSpan)) {
          diagnosticEmitter.emitAddressDiagnostic(
              addressUseState.address, &*ii, errorUser, false /*is consuming*/,
              addressUseState.isInOutTermUser(&*ii));
          foundSingleBlockError = true;
          emittedDiagnostic = true;
          break;
        }

        if (addressUseState.isInitUse(&*ii, errorSpan)) {
          llvm::errs() << "Should not have errored if we see an init?! Init: "
                       << *ii;
          llvm_unreachable("Standard compiler error");
        }
      }
      if (foundSingleBlockError)
        continue;
    }

    // If we didn't find a single block error, then we need to go search for our
    // liveness error in successor blocks. We know that this means that our
    // current block must be live out. Do a quick check just to be careful.
    using IsLive = PrunedLiveBlocks::IsLive;
    SmallVector<IsLive, 8> isLiveArray;
#ifndef NDEBUG
    liveness.getBlockLiveness(errorUser->getParent(), errorSpan, isLiveArray);
    assert(llvm::all_of(
               isLiveArray,
               [](IsLive liveness) { return liveness = IsLive::LiveOut; }) &&
           "Should be live out?!");
    isLiveArray.clear();
#endif

    BasicBlockWorklist worklist(errorUser->getFunction());
    for (auto *succBlock : errorUser->getParent()->getSuccessorBlocks())
      worklist.pushIfNotVisited(succBlock);

    LLVM_DEBUG(llvm::dbgs() << "Performing forward traversal from errorUse "
                               "looking for the cause of liveness!\n");

    SmallSetVector<SILInstruction *, 1> violatingInst;
    bool foundSingleBlockError = false;
    while (auto *block = worklist.pop()) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Visiting block: bb" << block->getDebugID() << "\n");

      SWIFT_DEFER { isLiveArray.clear(); };
      liveness.getBlockLiveness(block, takeInstAndValue.second, isLiveArray);

      // If we hit an init or dead along all bits in the block, we do not need
      // to process further successors.
      bool shouldVisitSuccessors = false;

      // Now search forward for uses.
      for (auto isLive : isLiveArray) {
        switch (isLive) {
        case IsLive::Dead:
          LLVM_DEBUG(llvm::dbgs() << "    Dead block!\n");
          // Ignore a dead block. Our error use could not be in such a block.
          //
          // This can happen for instance along an exit block of a loop where
          // the error use is within the loop.
          continue;
        case IsLive::LiveOut:
          LLVM_DEBUG(llvm::dbgs() << "    Live out block!\n");
          // If we see a live out block that is also a def block, we need to fa
          assert(!liveness.isDefBlock(block, errorSpan) &&
                 "If in def block... we are in liveness block");
          [[clang::fallthrough]];
        case IsLive::LiveWithin:
          if (isLive == IsLive::LiveWithin)
            LLVM_DEBUG(llvm::dbgs() << "    Live within block!\n");

          bool foundInit = false;
          for (auto &blockInst : *block) {
            LLVM_DEBUG(llvm::dbgs() << "        Inst: " << blockInst);

            if (addressUseState.isConsume(&blockInst, errorSpan)) {
              LLVM_DEBUG(llvm::dbgs() << "            Is consume!\n");
              diagnosticEmitter.emitAddressDiagnostic(addressUseState.address,
                                                      &blockInst, errorUser,
                                                      true /*is consuming*/);
              foundSingleBlockError = true;
              emittedDiagnostic = true;
              break;
            }

            if (addressUseState.isLivenessUse(&blockInst, errorSpan)) {
              LLVM_DEBUG(llvm::dbgs() << "            Is liveness use!\n");
              diagnosticEmitter.emitAddressDiagnostic(
                  addressUseState.address, &blockInst, errorUser,
                  false /*is consuming*/,
                  addressUseState.isInOutTermUser(&blockInst));
              foundSingleBlockError = true;
              emittedDiagnostic = true;
              break;
            }

            // If we find an init use for this bit... just break.
            if (addressUseState.isInitUse(&blockInst, errorSpan)) {
              foundInit = true;
              break;
            }
          }

          // If we did not find an init and visited the entire block... we need
          // to visit successors for at least one bit.
          if (!foundInit)
            shouldVisitSuccessors = true;

          assert((isLive == IsLive::LiveOut || foundSingleBlockError ||
                  foundInit) &&
                 "Should either have a pure live out, found an init, or we "
                 "should have found "
                 "an error.");
        }

        // If we found an error, break out of the loop. We don't have further
        // work to do.
        if (foundSingleBlockError) {
          break;
        }
      }

      // If we found an error, just bail without processing additional blocks.
      if (foundSingleBlockError)
        break;

      // If we saw only dead blocks or found inits for all bits... then we do
      // not need to process further
      if (!shouldVisitSuccessors)
        continue;

      // If we didn't find a single block error, add successors to the worklist
      // and visit them.
      for (auto *succBlock : block->getSuccessorBlocks())
        worklist.pushIfNotVisited(succBlock);
    }
  }

  return emittedDiagnostic;
}

bool GlobalLivenessChecker::compute() {
  // Then revisit our takes, this time checking if we are within the boundary
  // and if we are, emit an error.
  LLVM_DEBUG(llvm::dbgs() << "Checking takes for errors!\n");
  bool emittedDiagnostic = false;

  emittedDiagnostic |= testInstVectorLiveness(addressUseState.takeInsts);
  emittedDiagnostic |= testInstVectorLiveness(addressUseState.copyInsts);

  // If we emitted an error user, we should always emit at least one
  // diagnostic. If we didn't there is a bug in the implementation.
  assert(hadAnyErrorUsers == emittedDiagnostic);
  return hadAnyErrorUsers;
}

//===----------------------------------------------------------------------===//
//                       MARK: Main Pass Implementation
//===----------------------------------------------------------------------===//

void MoveOnlyChecker::cleanupAfterEmittingDiagnostic() {
  for (auto &block : *fn) {
    for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
      auto *inst = &*ii;
      ++ii;

      // Convert load [copy] -> load_borrow + explicit_copy_value.
      if (auto *li = dyn_cast<LoadInst>(inst)) {
        if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
          SILBuilderWithScope builder(li);
          auto *lbi = builder.createLoadBorrow(li->getLoc(), li->getOperand());
          auto *cvi = builder.createExplicitCopyValue(li->getLoc(), lbi);
          builder.createEndBorrow(li->getLoc(), lbi);
          li->replaceAllUsesWith(cvi);
          li->eraseFromParent();
          changed = true;
        }
      }

      // Convert copy_addr !take of src to its explicit value form so we don't
      // error.
      if (auto *copyAddr = dyn_cast<CopyAddrInst>(inst)) {
        if (!copyAddr->isTakeOfSrc()) {
          SILBuilderWithScope builder(copyAddr);
          builder.createExplicitCopyAddr(
              copyAddr->getLoc(), copyAddr->getSrc(), copyAddr->getDest(),
              IsTake_t(copyAddr->isTakeOfSrc()),
              IsInitialization_t(copyAddr->isInitializationOfDest()));
          copyAddr->eraseFromParent();
          changed = true;
        }
      }
    }
  }
}

bool MoveOnlyChecker::searchForCandidateMarkMustChecks() {
  bool emittedDiagnostic = false;
  for (auto &block : *fn) {
    for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
      auto *mmci = dyn_cast<MarkMustCheckInst>(&*ii);
      ++ii;

      if (!mmci || !mmci->hasMoveCheckerKind() || !mmci->getType().isAddress())
        continue;

      // Skip any alloc_box due to heap to stack failing on a box capture. This
      // will just cause an error.
      if (auto *pbi = dyn_cast<ProjectBoxInst>(mmci->getOperand())) {
        if (isa<AllocBoxInst>(pbi->getOperand())) {
          LLVM_DEBUG(
              llvm::dbgs()
              << "Early emitting diagnostic for unsupported alloc box!\n");
          diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(mmci);
          emittedDiagnostic = true;
          continue;
        }

        if (auto *bbi = dyn_cast<BeginBorrowInst>(pbi->getOperand())) {
          if (isa<AllocBoxInst>(bbi->getOperand())) {
            LLVM_DEBUG(
                llvm::dbgs()
                << "Early emitting diagnostic for unsupported alloc box!\n");
            diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(mmci);
            emittedDiagnostic = true;
            continue;
          }
        }
      }

      moveIntroducersToProcess.insert(mmci);
    }
  }
  return emittedDiagnostic;
}

/// Create a new destroy_value instruction before the specified instruction and
/// record it as a final consume.
static void insertDestroyBeforeInstruction(UseState &addressUseState,
                                           SILInstruction *nextInstruction,
                                           SILValue baseAddress,
                                           SmallBitVector &bv,
                                           ConsumeInfo &consumes) {
  // If we need all bits...
  if (bv.all()) {
    // And our next instruction is a destroy_addr on the base address, just
    // claim that destroy instead of inserting another destroy_addr.
    if (auto *dai = dyn_cast<DestroyAddrInst>(nextInstruction)) {
      if (dai->getOperand() == baseAddress) {
        consumes.recordFinalConsume(dai, TypeTreeLeafTypeRange(0, bv.size()));
        return;
      }
    }

    // Otherwise, create a new destroy addr on the entire address.
    SILBuilderWithScope builder(nextInstruction);
    auto loc =
        RegularLocation::getAutoGeneratedLocation(nextInstruction->getLoc());
    auto *dai = builder.createDestroyAddr(loc, baseAddress);
    consumes.recordFinalConsume(dai, TypeTreeLeafTypeRange(0, bv.size()));
    addressUseState.destroys.insert({dai, TypeTreeLeafTypeRange(0, bv.size())});
    return;
  }

  // Otherwise, we have a partially destroyed type. Create new destroy addr for
  // each contiguous range of elts. This should only happen for structs/tuples.
  SILBuilderWithScope builder(nextInstruction);
  auto loc =
      RegularLocation::getAutoGeneratedLocation(nextInstruction->getLoc());
  SmallVector<std::pair<SILValue, TypeTreeLeafTypeRange>> valuesToDestroy;
  TypeTreeLeafTypeRange::constructProjectionsForNeededElements(
      baseAddress, nextInstruction, bv, valuesToDestroy);
  while (!valuesToDestroy.empty()) {
    auto pair = valuesToDestroy.pop_back_val();
    if (pair.first->getType().isTrivial(*nextInstruction->getFunction()))
      continue;
    auto *dai = builder.createDestroyAddr(loc, pair.first);
    consumes.recordFinalConsume(dai, pair.second);
    addressUseState.destroys.insert({dai, pair.second});
  }
}

void MoveOnlyChecker::insertDestroysOnBoundary(
    FieldSensitiveMultiDefPrunedLiveRange &liveness,
    FieldSensitivePrunedLivenessBoundary &boundary) {
  using IsInterestingUser = FieldSensitivePrunedLiveness::IsInterestingUser;
  LLVM_DEBUG(llvm::dbgs() << "Inserting destroys on boundary!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Visiting users!\n");

  for (auto &pair : boundary.getLastUsers()) {
    auto *inst = pair.first;
    auto &bv = pair.second;

    LLVM_DEBUG(llvm::dbgs() << "        User: " << *inst);

    auto interestingUse = liveness.isInterestingUser(inst);
    switch (interestingUse.first) {
    case IsInterestingUser::LifetimeEndingUse:
      LLVM_DEBUG(llvm::dbgs()
                 << "        Lifetime ending use! Recording final consume!\n");
      consumes.recordFinalConsume(inst, *interestingUse.second);
      continue;
    case IsInterestingUser::NonLifetimeEndingUse:
    case IsInterestingUser::NonUser:
      LLVM_DEBUG(llvm::dbgs() << "        NoneUser or NonLifetimeEndingUse! "
                                 "inserting destroy before instruction!\n");
      // If we are dealing with an inout parameter, we will have modeled our
      // last use by treating a return inst as a last use. Since it doesn't have
      // any successors, this results in us not inserting any destroy_addr.
      if (isa<TermInst>(inst)) {
        auto *block = inst->getParent();
        for (auto *succBlock : block->getSuccessorBlocks()) {
          auto *insertPt = &*succBlock->begin();
          insertDestroyBeforeInstruction(addressUseState, insertPt,
                                         liveness.getRootValue(), bv, consumes);
        }
        continue;
      }

      auto *insertPt = inst->getNextInstruction();
      insertDestroyBeforeInstruction(addressUseState, insertPt,
                                     liveness.getRootValue(), bv, consumes);
      continue;
    }
  }

  for (auto pair : boundary.getBoundaryEdges()) {
    auto *insertPt = &*pair.first->begin();
    insertDestroyBeforeInstruction(addressUseState, insertPt,
                                   liveness.getRootValue(), pair.second,
                                   consumes);
    LLVM_DEBUG(llvm::dbgs() << "    Inserting destroy on edge bb"
                            << pair.first->getDebugID() << "\n");
  }

  for (auto defPair : boundary.getDeadDefs()) {
    LLVM_DEBUG(llvm::dbgs()
               << "    Inserting destroy on dead def" << *defPair.first);

    if (auto *arg = dyn_cast<SILArgument>(defPair.first)) {
      auto *insertPt = &*arg->getParent()->begin();
      insertDestroyBeforeInstruction(addressUseState, insertPt,
                                     liveness.getRootValue(), defPair.second,
                                     consumes);
    } else {
      // If our dead def is a mark_must_check and we are processing an inout
      // argument, do not insert a destroy_addr. We are cheating a little bit by
      // modeling the initial value as a mark_must_check... so we need to
      // compensate for our cheating by not inserting the destroy_addr here
      // since we would be destroying the inout argument before we use it.
      if (auto *markMustCheckInst =
              dyn_cast<MarkMustCheckInst>(defPair.first)) {
        if (auto *arg = dyn_cast<SILFunctionArgument>(
                markMustCheckInst->getOperand())) {
          if (arg->getArgumentConvention().isInoutConvention()) {
            continue;
          }
        }
      }

      auto *inst = cast<SILInstruction>(defPair.first);
      auto *insertPt = inst->getNextInstruction();
      assert(insertPt && "def instruction was a terminator");
      insertDestroyBeforeInstruction(addressUseState, insertPt,
                                     liveness.getRootValue(), defPair.second,
                                     consumes);
    }
  }

  consumes.finishRecordingFinalConsumes();
}

void MoveOnlyChecker::rewriteUses(
    FieldSensitiveMultiDefPrunedLiveRange &liveness,
    const FieldSensitivePrunedLivenessBoundary &boundary) {
  // First remove all destroy_addr that have not been claimed.
  for (auto destroyPair : addressUseState.destroys) {
    if (!consumes.claimConsume(destroyPair.first, destroyPair.second)) {
      destroyPair.first->eraseFromParent();
    }
  }

  // Then convert all claimed reinits to inits.
  for (auto reinitPair : addressUseState.reinitInsts) {
    if (!isReinitToInitConvertibleInst(reinitPair.first))
      continue;
    if (!consumes.claimConsume(reinitPair.first, reinitPair.second))
      convertMemoryReinitToInitForm(reinitPair.first);
  }

  // Check all takes.
  for (auto takeInst : addressUseState.takeInsts) {
    bool claimedConsume =
        consumes.claimConsume(takeInst.first, takeInst.second);
    (void)claimedConsume;
    assert(claimedConsume && "Should claim all copies?!");
  }

  // Then rewrite all copy insts to be takes and claim them.
  for (auto copyInst : addressUseState.copyInsts) {
    bool claimedConsume =
        consumes.claimConsume(copyInst.first, copyInst.second);
    if (!claimedConsume) {
      llvm::errs()
          << "Found consume that was not recorded as a 'claimed consume'!\n";
      llvm::errs() << "Unrecorded consume: " << *copyInst.first;
      llvm_unreachable("Standard compiler abort?!");
    }
    if (auto *li = dyn_cast<LoadInst>(copyInst.first)) {
      // Convert this to its take form.
      auto accessPath = AccessPathWithBase::computeInScope(li->getOperand());
      if (auto *access = dyn_cast<BeginAccessInst>(accessPath.base))
        access->setAccessKind(SILAccessKind::Modify);
      li->setOwnershipQualifier(LoadOwnershipQualifier::Take);
      changed = true;
      continue;
    }

    if (auto *copy = dyn_cast<CopyAddrInst>(copyInst.first)) {
      // Convert this to its take form.
      auto accessPath = AccessPathWithBase::computeInScope(copy->getSrc());
      if (auto *access = dyn_cast<BeginAccessInst>(accessPath.base))
        access->setAccessKind(SILAccessKind::Modify);
      copy->setIsTakeOfSrc(IsTake);
      continue;
    }

    llvm::dbgs() << "Unhandled copy user: " << *copyInst.first;
    llvm_unreachable("Unhandled case?!");
  }

  // Finally now that we have placed all of our destroys in the appropriate
  // places, convert any copies that we know are borrows into begin_borrow. We
  // do not need to worry about expanding scopes since if we needed to expand a
  // scope, we would have emitted the scope expansion error during diagnostics.
  for (auto pair : addressUseState.borrows) {
    if (auto *li = dyn_cast<LoadInst>(pair.first)) {
      if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
        // If we had a load [copy], borrow then we know that all of its destroys
        // must have been destroy_value. So we can just gather up those
        // destroy_value and use then to create a new load_borrow scope.
        SILBuilderWithScope builder(li);
        auto *lbi = builder.createLoadBorrow(li->getLoc(), li->getOperand());

        for (auto *consumeUse : li->getConsumingUses()) {
          auto *dvi = cast<DestroyValueInst>(consumeUse->getUser());
          SILBuilderWithScope destroyBuilder(dvi);
          destroyBuilder.createEndBorrow(dvi->getLoc(), lbi);
          dvi->eraseFromParent();
          changed = true;
        }

        li->replaceAllUsesWith(lbi);
        li->eraseFromParent();
        continue;
      }
    }

    llvm::dbgs() << "Borrow: " << *pair.first;
    llvm_unreachable("Unhandled case?!");
  }

#ifndef NDEBUG
  if (consumes.hasUnclaimedConsumes()) {
    llvm::errs() << "Found unclaimed consumes?!\n";
    consumes.print(llvm::errs());
    llvm_unreachable("Standard error?!");
  }
#endif
}

bool MoveOnlyChecker::performSingleCheck(MarkMustCheckInst *markedAddress) {
  SWIFT_DEFER { diagnosticEmitter.clearUsesWithDiagnostic(); };

  auto accessPathWithBase = AccessPathWithBase::compute(markedAddress);
  auto accessPath = accessPathWithBase.accessPath;
  if (!accessPath.isValid()) {
    LLVM_DEBUG(llvm::dbgs() << "Invalid access path: " << *markedAddress);
    return false;
  }

  // Then gather all uses of our address by walking from def->uses. We use this
  // to categorize the uses of this address into their ownership behavior (e.x.:
  // init, reinit, take, destroy, etc.).
  SmallVector<SILBasicBlock *, 32> gatherUsesDiscoveredBlocks;
  SSAPrunedLiveness gatherUsesLiveness(&gatherUsesDiscoveredBlocks);
  GatherUsesVisitor visitor(*this, addressUseState, markedAddress,
                            diagnosticEmitter, gatherUsesLiveness);
  SWIFT_DEFER { visitor.clear(); };
  visitor.reset(markedAddress);
  if (!visitAccessPathUses(visitor, accessPath, fn)) {
    LLVM_DEBUG(llvm::dbgs() << "Failed access path visit: " << *markedAddress);
    return false;
  }
  addressUseState.initializeInOutTermUsers();

  // If we found a load [copy] or copy_addr that requires multiple copies or an
  // exclusivity error, then we emitted an early error. Bail now and allow the
  // user to fix those errors and recompile to get further errors.
  //
  // DISCUSSION: The reason why we do this is in the dataflow below we want to
  // be able to assume that the load [copy] or copy_addr/copy_addr [init] are
  // actual last uses, but the frontend that emitted the code for simplicity
  // emitted a copy from the base address + a destroy_addr of the use. By
  // bailing here, we can make that assumption since we would have errored
  // earlier otherwise.
  if (diagnosticEmitter.emittedAnyDiagnostics())
    return true;

  //===---
  // Liveness Checking
  //
  SmallVector<SILBasicBlock *, 32> discoveredBlocks;
  FieldSensitiveMultiDefPrunedLiveRange liveness(fn, markedAddress,
                                                 &discoveredBlocks);
  addressUseState.initializeLiveness(liveness);

  // Then compute the takes that are within the cumulative boundary of
  // liveness that we have computed. If we find any, they are the errors ones.
  GlobalLivenessChecker emitter(addressUseState, diagnosticEmitter, liveness);

  // If we had any errors, we do not want to modify the SIL... just bail.
  if (emitter.compute()) {
    return true;
  }

  // Ok, we now know that we fit our model since we did not emit errors and thus
  // can begin the transformation.
  SWIFT_DEFER { consumes.clear(); };
  FieldSensitivePrunedLivenessBoundary boundary(liveness.getNumSubElements());
  liveness.computeBoundary(boundary);
  insertDestroysOnBoundary(liveness, boundary);
  rewriteUses(liveness, boundary);

  return true;
}

bool MoveOnlyChecker::checkFunction() {
  // First search for candidates to process and emit diagnostics on any
  // mark_must_check [noimplicitcopy] we didn't recognize.
  bool emittedDiagnostic = searchForCandidateMarkMustChecks();

  // If we didn't find any introducers to check, just return changed.
  //
  // NOTE: changed /can/ be true here if we had any mark_must_check
  // [noimplicitcopy] that we didn't understand and emitting a diagnostic upon
  // and then deleting.
  if (moveIntroducersToProcess.empty()) {
    if (emittedDiagnostic)
      cleanupAfterEmittingDiagnostic();
    return changed;
  }

  for (auto *markedValue : moveIntroducersToProcess) {
    LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *markedValue);

    // Perform our address check.
    if (!performSingleCheck(markedValue)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Failed to perform single check! Emitting error!\n");
      // If we fail the address check in some way, set the diagnose!
      diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(markedValue);
    }
  }
  emittedDiagnostic = diagnosticEmitter.emittedAnyDiagnostics();

  // Ok, now that we have performed our checks, we need to eliminate all mark
  // must check inst since it is invalid for these to be in canonical SIL and
  // our work is done here.
  while (!moveIntroducersToProcess.empty()) {
    auto *markedInst = moveIntroducersToProcess.pop_back_val();
    markedInst->replaceAllUsesWith(markedInst->getOperand());
    markedInst->eraseFromParent();
    changed = true;
  }

  // Once we have finished processing, if we emitted any diagnostics, then we
  // may have copy_addr [init], load [copy] of @moveOnly typed values. This is
  // not valid in Canonical SIL, so we need to ensure that those copy_value
  // become explicit_copy_value. This is ok to do since we are already going to
  // fail the compilation and just are trying to maintain SIL invariants.
  //
  // It is also ok that we use a little more compile time and go over the
  // function again, since we are going to fail the compilation and not codegen.
  if (emittedDiagnostic) {
    cleanupAfterEmittingDiagnostic();
  }

  return changed;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MoveOnlyCheckerPass : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // Only run this pass if the move only language feature is enabled.
    if (!fn->getASTContext().LangOpts.Features.contains(Feature::MoveOnly))
      return;

    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    assert(fn->getModule().getStage() == SILStage::Raw &&
           "Should only run on Raw SIL");
    LLVM_DEBUG(llvm::dbgs() << "===> MoveOnly Addr Checker. Visiting: "
                            << fn->getName() << '\n');
    auto *accessBlockAnalysis = getAnalysis<NonLocalAccessBlockAnalysis>();
    auto *dominanceAnalysis = getAnalysis<DominanceAnalysis>();
    DominanceInfo *domTree = dominanceAnalysis->get(fn);
    auto *deAnalysis = getAnalysis<DeadEndBlocksAnalysis>()->get(fn);

    if (MoveOnlyChecker(getFunction(), deAnalysis, accessBlockAnalysis, domTree)
            .checkFunction()) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveOnlyAddressChecker() {
  return new MoveOnlyCheckerPass();
}
