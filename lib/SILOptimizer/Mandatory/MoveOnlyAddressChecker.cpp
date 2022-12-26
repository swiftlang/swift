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

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
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
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include "MoveOnlyDiagnostics.h"
#include "MoveOnlyObjectChecker.h"

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

//===----------------------------------------------------------------------===//
//                              MARK: Use State
//===----------------------------------------------------------------------===//

namespace {

struct UseState {
  SILValue address;
  SmallSetVector<DestroyAddrInst *, 4> destroys;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> livenessUses;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> borrows;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> takeInsts;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> copyInsts;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> initInsts;
  llvm::SmallMapVector<SILInstruction *, TypeTreeLeafTypeRange, 4> reinitInsts;

  SILFunction *getFunction() const { return address->getFunction(); }
  void clear() {
    address = SILValue();
    destroys.clear();
    livenessUses.clear();
    borrows.clear();
    copyInsts.clear();
    takeInsts.clear();
    initInsts.clear();
    reinitInsts.clear();
  }
};

} // namespace

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
    diagnosticEmitter.fn = fn;
    diagnosticEmitter.canonicalizer = &canonicalizer;
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

  bool check();
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
  void reset(SILValue address) { useState.address = address; }
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
    useState.destroys.insert(dvi);
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
      LLVM_DEBUG(llvm::dbgs() << "Found load: " << *li);
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
//                       MARK: Local Per Block Dataflow
//===----------------------------------------------------------------------===//

namespace {

using InstLeafTypePair = std::pair<SILInstruction *, TypeTreeLeafTypeRange>;
using InstOptionalLeafTypePair =
    std::pair<SILInstruction *, Optional<TypeTreeLeafTypeRange>>;

/// A per block state structure that we use as we walk the block. We do not
/// persist these. We use this data structure so we can simplify the code
/// below using helper functions.
struct LocalDataflowState {
  // As we walk the block, this is set to the state associated with the last
  // take we have seen. If this is non-null when we reach the top of the
  // block, this is the take that is propagated upwards out of the block.
  InstOptionalLeafTypePair takeUp = {nullptr, {}};

  /// This is the last liveness providing instruction that we saw since the
  /// last init or the end of the block. If this is set when we reach the top
  /// of the block, this is the liveness instruction that we use for the
  ///
  /// This is reset when we track a new init since we want to make sure that
  /// we do not hit any liveness errors from takes that may be earlier than
  /// the init in the block.
  InstOptionalLeafTypePair livenessUp = {nullptr, {}};

  // MARK: Local Dataflow Kill State

  /// If we have not yet seen a take or an init in this block, this is set to
  /// false. Once we have seen one of those, we set this to true. We use this
  /// to generate our kill set for the block and do it only once.
  bool foundFirstNonLivenessUse = false;

  /// This is the first take that we see as we walk up the block if we haven't
  /// yet seen an init. The reason why we track this is that if we have
  /// liveness in a successor block, we need to error on this take.
  InstOptionalLeafTypePair firstTake = {nullptr, {}};

  /// This is the first init that we see in the block if we have not seen a
  /// different init. If this is set, then we know that we need to kill the
  /// variable in this block. It also lets us know that any takes earlier in
  /// the block that we see can not have a liveness error due to liveness in
  /// earlier blocks.
  InstOptionalLeafTypePair firstInit = {nullptr, {}};

  void initializeLivenessUp(SILInstruction *inst, SILValue address) {
    livenessUp = {inst, TypeTreeLeafTypeRange(address)};
  }

  /// If we are not already tracking liveness, begin tracking liveness for
  /// this type tree range. If we are already tracking liveness, use the later
  /// instruction.
  void trackLivenessUp(SILInstruction *inst, TypeTreeLeafTypeRange range) {
    if (livenessUp.first) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    Found liveness use! Already tracking liveness. Inst: "
                 << inst);
      return;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "    Found liveness use! Propagating liveness. Inst: "
               << inst);
    livenessUp = {inst, range};
  }

  void trackTake(SILInstruction *inst, TypeTreeLeafTypeRange range) {
    // If we haven't found a first init use, we need to track a first take
    // since we need to make sure that we do not have any liveness issues
    // coming up from a successor block.
    if (!foundFirstNonLivenessUse)
      firstTake = {inst, range};
    takeUp = {inst, range};
    foundFirstNonLivenessUse = true;
  }

  void trackInit(SILInstruction *inst, TypeTreeLeafTypeRange range) {
    // Since we are tracking a new variable, reset takeUp and livenessUp.
    takeUp = {nullptr, {}};
    livenessUp = {nullptr, {}};

    // Then see if we need to set firstInit so we setup that this block kills
    // the given range.
    if (!foundFirstNonLivenessUse) {
      LLVM_DEBUG(llvm::dbgs() << "    Updated with new init: " << inst);
      firstInit = {inst, range};
    } else {
      LLVM_DEBUG(
          llvm::dbgs()
          << "    Found init! Already have first non liveness use. Inst: "
          << inst);
    }
    foundFirstNonLivenessUse = true;
  }

  /// Track a take given that we emitted a liveness error.
  ///
  /// In this case, we reset livenessUp and track the take. We do this to
  /// ensure that earlier errors are on the consuming take rather than on the
  /// liveness use. This is especially important when emitting "inout not
  /// reintialized" errors since we only want one of those to be emitted.
  void trackTakeForLivenessError(SILInstruction *take,
                                 TypeTreeLeafTypeRange range) {
    livenessUp = {nullptr, {}};
    trackTake(take, range);
  }

  bool isTrackingAnyState() const { return takeUp.first || livenessUp.first; }

  bool hasLivenessUp() const { return livenessUp.first; }

  SILInstruction *getLivenessUp() const { return livenessUp.first; }

  TypeTreeLeafTypeRange getLivenessUpTypeRange() const {
    return *livenessUp.second;
  }

  SILInstruction *getFirstInit() const { return firstInit.first; }

  TypeTreeLeafTypeRange getFirstInitTypeRange() const {
    return *firstInit.second;
  }

  SILInstruction *getFirstTake() const { return firstTake.first; }

  TypeTreeLeafTypeRange getFirstTakeTypeRange() const {
    return *firstTake.second;
  }

  bool hasTakeUp() const { return takeUp.first; }

  SILInstruction *getTakeUp() const { return takeUp.first; }

  TypeTreeLeafTypeRange getTakeUpTypeRange() const { return *takeUp.second; }
};

struct BlockSummaries {
  SmallVector<InstLeafTypePair, 32> initDownInsts;
  SmallVector<InstLeafTypePair, 32> takeDownInsts;
  SmallVector<InstLeafTypePair, 32> takeUpInsts;
  SmallVector<InstLeafTypePair, 32> livenessUpInsts;
  BlockState::Map blockToState;

  MarkMustCheckInst *markedAddress;
  bool isInOut;
  DiagnosticEmitter &diagnosticEmitter;
  UseState &addressUseState;

  BlockSummaries(MoveOnlyChecker &checker, MarkMustCheckInst *markedAddress)
      : markedAddress(markedAddress), isInOut(false),
        diagnosticEmitter(checker.diagnosticEmitter),
        addressUseState(checker.addressUseState) {
    if (auto *fArg = dyn_cast<SILFunctionArgument>(markedAddress->getOperand()))
      isInOut |= fArg->getArgumentConvention().isInoutConvention();
  }

  void summarize(SILBasicBlock &block);

  /// After we have summarized all of our blocks, initialize the given pruned
  /// liveness with the information needed from our summaries to perform
  /// multi-block liveness dataflow.
  void initializeLiveness(FieldSensitiveAddressPrunedLiveness &liveness,
                          SmallPtrSetImpl<SILInstruction *> &inoutTermUsers);
};

} // anonymous namespace

void BlockSummaries::summarize(SILBasicBlock &block) {
  LLVM_DEBUG(llvm::dbgs() << "Visiting block: bb" << block.getDebugID()
                          << "\n");
  // Then walk backwards from the bottom of the block to the top, initializing
  // its state, handling any completely in block diagnostics.
  auto *term = block.getTerminator();
  bool isExitBlock = term->isFunctionExiting();

  LocalDataflowState state;
  for (auto &inst : llvm::reverse(block)) {
    if (isExitBlock && isInOut && &inst == term) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    Found inout term liveness user: " << inst);
      state.initializeLivenessUp(&inst, markedAddress);
      continue;
    }

    {
      auto iter = addressUseState.takeInsts.find(&inst);
      if (iter != addressUseState.takeInsts.end()) {
        // If we are not yet tracking a "take up" or a "liveness up", then we
        // can update our state. In those other two cases we emit an error
        // diagnostic below.
        if (!state.isTrackingAnyState()) {
          LLVM_DEBUG(llvm::dbgs()
                     << "    Tracking new take up: " << *iter->first);
          state.trackTake(iter->first, iter->second);
          continue;
        }

        bool emittedSomeDiagnostic = false;
        if (auto *takeUp = state.getTakeUp()) {
          LLVM_DEBUG(llvm::dbgs() << "    Found two takes, emitting error!\n");
          LLVM_DEBUG(llvm::dbgs() << "    First take: " << *takeUp);
          LLVM_DEBUG(llvm::dbgs() << "    Second take: " << inst);
          diagnosticEmitter.emitAddressDiagnostic(markedAddress, takeUp, &inst,
                                                  true /*is consuming*/);
          emittedSomeDiagnostic = true;
        }

        // If we found a liveness inst, we are going to emit an error since we
        // have a use after free.
        if (auto *livenessUpInst = state.getLivenessUp()) {
          // If we are tracking state for an inout and our liveness inst is a
          // function exiting instruction, we want to emit a special
          // diagnostic error saying that the user has not reinitialized inout
          // along a path to the end of the function.
          if (livenessUpInst == term && isInOut) {
            LLVM_DEBUG(llvm::dbgs()
                       << "    Found liveness inout error: " << inst);
            // Even though we emit a diagnostic for inout here, we actually
            // want to no longer track the inout liveness use and instead want
            // to track the consuming use so that earlier errors are on the
            // take and not on the inout. This is to ensure that we only emit
            // a single inout not reinitialized before end of function error
            // if we have multiple consumes along that path.
            diagnosticEmitter.emitInOutEndOfFunctionDiagnostic(markedAddress,
                                                               &inst);
            state.trackTakeForLivenessError(iter->first, iter->second);
          } else {
            // Otherwise, we just emit a normal liveness error.
            LLVM_DEBUG(llvm::dbgs() << "    Found liveness error: " << inst);
            diagnosticEmitter.emitAddressDiagnostic(markedAddress,
                                                    livenessUpInst, &inst,
                                                    false /*is not consuming*/);
            state.trackTakeForLivenessError(iter->first, iter->second);
          }
          emittedSomeDiagnostic = true;
        }

        (void)emittedSomeDiagnostic;
        assert(emittedSomeDiagnostic);
        continue;
      }
    }

    {
      auto iter = addressUseState.copyInsts.find(&inst);
      if (iter != addressUseState.copyInsts.end()) {
        // If we are not yet tracking a "take up" or a "liveness up", then we
        // can update our state. In those other two cases we emit an error
        // diagnostic below.
        if (!state.isTrackingAnyState()) {
          LLVM_DEBUG(llvm::dbgs()
                     << "    Tracking new take up: " << *iter->first);
          state.trackTake(iter->first, iter->second);
          continue;
        }

        bool emittedSomeDiagnostic = false;
        if (auto *takeUp = state.getTakeUp()) {
          LLVM_DEBUG(llvm::dbgs() << "    Found two takes, emitting error!\n");
          LLVM_DEBUG(llvm::dbgs() << "    First take: " << *takeUp);
          LLVM_DEBUG(llvm::dbgs() << "    Second take: " << inst);
          diagnosticEmitter.emitAddressDiagnostic(markedAddress, takeUp, &inst,
                                                  true /*is consuming*/);
          emittedSomeDiagnostic = true;
        }

        // If we found a liveness inst, we are going to emit an error since we
        // have a use after free.
        if (auto *livenessUpInst = state.getLivenessUp()) {
          // If we are tracking state for an inout and our liveness inst is a
          // function exiting instruction, we want to emit a special
          // diagnostic error saying that the user has not reinitialized inout
          // along a path to the end of the function.
          if (livenessUpInst == term && isInOut) {
            LLVM_DEBUG(llvm::dbgs()
                       << "    Found liveness inout error: " << inst);
            // Even though we emit a diagnostic for inout here, we actually
            // want to no longer track the inout liveness use and instead want
            // to track the consuming use so that earlier errors are on the
            // take and not on the inout. This is to ensure that we only emit
            // a single inout not reinitialized before end of function error
            // if we have multiple consumes along that path.
            diagnosticEmitter.emitInOutEndOfFunctionDiagnostic(markedAddress,
                                                               &inst);
            state.trackTakeForLivenessError(iter->first, iter->second);
          } else {
            // Otherwise, we just emit a normal liveness error.
            LLVM_DEBUG(llvm::dbgs() << "    Found liveness error: " << inst);
            diagnosticEmitter.emitAddressDiagnostic(markedAddress,
                                                    livenessUpInst, &inst,
                                                    false /*is not consuming*/);
            state.trackTakeForLivenessError(iter->first, iter->second);
          }
          emittedSomeDiagnostic = true;
        }

        (void)emittedSomeDiagnostic;
        assert(emittedSomeDiagnostic);
        continue;
      }
    }

    {
      auto iter = addressUseState.livenessUses.find(&inst);
      if (iter != addressUseState.livenessUses.end()) {
        state.trackLivenessUp(iter->first, iter->second);
        continue;
      }
    }

    // Just treat borrows at this point as liveness requiring.
    {
      auto iter = addressUseState.borrows.find(&inst);
      if (iter != addressUseState.borrows.end()) {
        state.trackLivenessUp(iter->first, iter->second);
        continue;
      }
    }

    // If we have an init, then unset previous take up and liveness up.
    {
      auto iter = addressUseState.initInsts.find(&inst);
      if (iter != addressUseState.initInsts.end()) {
        state.trackInit(iter->first, iter->second);
        continue;
      }
    }

    // We treat reinits in the following way:
    //
    // 1. For takes that we want to treat like destroy_addr (e.x.: store
    // [assign] and copy_addr [!init], we treat the reinit as only a kill and
    // not an additional take. This is because, we are treating their destroy
    // as a last use like destroy_addr. The hope is that as part of doing
    // fixups, we can just change this to a store [init] if there isn't an
    // earlier reachable take use, like we do with destroy_addr.
    //
    // 2. If we are unable to convert the reinit, it is a reinit like a
    // YieldInst or an ApplySite. In that case, we treat this as an actual
    // kill/take and reinit with a new value.
    {
      auto iter = addressUseState.reinitInsts.find(&inst);
      if (iter != addressUseState.reinitInsts.end()) {
        state.trackInit(iter->first, iter->second);
        continue;
      }
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "End of block. Dumping Results!\n");
  if (auto *firstInit = state.getFirstInit()) {
    LLVM_DEBUG(llvm::dbgs() << "    First Init Down: " << *firstInit);
    initDownInsts.emplace_back(firstInit, state.getFirstInitTypeRange());
  } else {
    LLVM_DEBUG(llvm::dbgs() << "    No Init Down!\n");
  }

  // At this point we want to begin mapping blocks to "first" users.
  if (auto *takeUp = state.getTakeUp()) {
    LLVM_DEBUG(llvm::dbgs() << "Take Up: " << takeUp);
    blockToState.try_emplace(&block, BlockState(takeUp, state.getFirstInit()));
    takeUpInsts.emplace_back(takeUp, state.getTakeUpTypeRange());
  } else {
    LLVM_DEBUG(llvm::dbgs() << "    No Take Up!\n");
  }

  if (auto *liveness = state.getLivenessUp()) {
    // This try_emplace fail if we already above initialized blockToState
    // above in the previous if block.
    blockToState.try_emplace(&block,
                             BlockState(liveness, state.getFirstInit()));
    LLVM_DEBUG(llvm::dbgs() << "Liveness Up: " << *liveness);
    livenessUpInsts.emplace_back(liveness, state.getLivenessUpTypeRange());
  } else {
    LLVM_DEBUG(llvm::dbgs() << "    No Liveness Up!\n");
  }

  if (auto *firstTakeInst = state.getFirstTake()) {
    LLVM_DEBUG(llvm::dbgs() << "    First Take Down: " << *firstTakeInst);
    takeDownInsts.emplace_back(firstTakeInst, state.getFirstTakeTypeRange());
    // We only emplace if we didn't already have a takeUp above. In such a
    // case, the try_emplace fails.
    blockToState.try_emplace(&block, BlockState(nullptr, false));
  } else {
    LLVM_DEBUG(llvm::dbgs() << "    No Take Down!\n");
  }
}

void BlockSummaries::initializeLiveness(
    FieldSensitiveAddressPrunedLiveness &liveness,
    SmallPtrSetImpl<SILInstruction *> &inoutTermUsers) {
  // At this point, we have handled all of the single block cases and have
  // simplified the remaining cases to global cases that we compute using
  // liveness. We begin by using all of our init down blocks as def blocks.
  for (auto initInstAndValue : initDownInsts)
    liveness.initializeDefBlock(initInstAndValue.first->getParent(),
                                initInstAndValue.second);

  // Then add all of the takes that we saw propagated up to the top of our
  // block. Since we have done this for all of our defs
  for (auto takeInstAndValue : takeUpInsts)
    liveness.updateForUse(takeInstAndValue.first, takeInstAndValue.second,
                          true /*lifetime ending*/);
  // Do the same for our borrow and liveness insts.
  for (auto livenessInstAndValue : livenessUpInsts)
    liveness.updateForUse(livenessInstAndValue.first,
                          livenessInstAndValue.second,
                          false /*lifetime ending*/);

  // Finally, if we have an inout argument, add a liveness use of the entire
  // value on terminators in blocks that are exits from the function. This
  // ensures that along all paths, if our inout is not reinitialized before we
  // exit the function, we will get an error. We also stash these users into
  // inoutTermUser so we can quickly recognize them later and emit a better
  // error msg.
  if (auto *fArg = dyn_cast<SILFunctionArgument>(markedAddress->getOperand())) {
    if (fArg->getArgumentConvention() ==
        SILArgumentConvention::Indirect_Inout) {
      SmallVector<SILBasicBlock *, 8> exitBlocks;
      markedAddress->getFunction()->findExitingBlocks(exitBlocks);
      for (auto *block : exitBlocks) {
        inoutTermUsers.insert(block->getTerminator());
        liveness.updateForUse(block->getTerminator(),
                              TypeTreeLeafTypeRange(markedAddress),
                              false /*lifetime ending*/);
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//                           MARK: Global Dataflow
//===----------------------------------------------------------------------===//

namespace {

/// Post process the found liveness and emit errors if needed. TODO: Better
/// name.
struct GlobalDataflow {
  BlockSummaries &summaries;
  FieldSensitiveAddressPrunedLiveness &liveness;
  SmallPtrSetImpl<SILInstruction *> &inoutTermInstUsers;
  SmallBitVector livenessVector;
  bool hadAnyErrorUsers = false;

  GlobalDataflow(BlockSummaries &summaries,
                 FieldSensitiveAddressPrunedLiveness &liveness,
                 SmallPtrSetImpl<SILInstruction *> &inoutTermInstUsers)
      : summaries(summaries), liveness(liveness),
        inoutTermInstUsers(inoutTermInstUsers) {}

  /// Returns true if we emitted any errors.
  bool compute();

  void clear() {
    livenessVector.clear();
    hadAnyErrorUsers = false;
  }

  std::pair<bool, bool> testInstVectorLiveness(
      SmallVectorImpl<std::pair<SILInstruction *, TypeTreeLeafTypeRange>>
          &instsToTest);
  BlockState::Map &getBlockToState() const { return summaries.blockToState; }
};

} // namespace

std::pair<bool, bool> GlobalDataflow::testInstVectorLiveness(
    SmallVectorImpl<std::pair<SILInstruction *, TypeTreeLeafTypeRange>>
        &instsToTest) {
  bool emittedDiagnostic = false;
  bool foundSingleBlockTakeDueToInitDown = false;

  for (auto takeInstAndValue : instsToTest) {
    LLVM_DEBUG(llvm::dbgs() << "    Checking: " << *takeInstAndValue.first);

    // Check if we are in the boundary...
    liveness.isWithinBoundary(takeInstAndValue.first, livenessVector);

    // If the bit vector does not contain any set bits, then we know that we did
    // not have any boundary violations for any leaf node of our root value.
    if (!livenessVector.any()) {
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

    // Before we do anything, grab the state for our errorUser's block from the
    // blockState and check if it is an init block. If so, we have no further
    // work to do since we already found correctness due to our single basic
    // block check. So, we have nothing further to do.
    if (getBlockToState().find(errorUser->getParent())->second.isInitDown) {
      // Set the flag that we saw an init down so that our assert later that we
      // actually emitted an error doesn't trigger.
      foundSingleBlockTakeDueToInitDown = true;
      continue;
    }

    BasicBlockWorklist worklist(errorUser->getFunction());
    for (auto *succBlock : errorUser->getParent()->getSuccessorBlocks())
      worklist.pushIfNotVisited(succBlock);

    LLVM_DEBUG(llvm::dbgs() << "Performing forward traversal from errorUse "
                               "looking for the cause of liveness!\n");

    while (auto *block = worklist.pop()) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Visiting block: bb" << block->getDebugID() << "\n");
      auto iter = getBlockToState().find(block);
      if (iter == getBlockToState().end()) {
        LLVM_DEBUG(llvm::dbgs() << "    No State! Skipping!\n");
        for (auto *succBlock : block->getSuccessorBlocks())
          worklist.pushIfNotVisited(succBlock);
        continue;
      }

      auto *blockUser = iter->second.userUp;

      if (blockUser) {
        LLVM_DEBUG(llvm::dbgs() << "    Found userUp: " << *blockUser);
        auto info = liveness.isInterestingUser(blockUser);
        // Make sure that it overlaps with our range...
        if (info.second->contains(*info.second)) {
          LLVM_DEBUG(llvm::dbgs() << "    Emitted diagnostic for it!\n");
          // and if it does... emit our diagnostic and continue to see if we
          // find errors along other paths.
          // if (invalidUsesWithDiagnostics.insert(errorUser
          bool isConsuming =
              info.first ==
              FieldSensitiveAddressPrunedLiveness::LifetimeEndingUse;
          summaries.diagnosticEmitter.emitAddressDiagnostic(
              summaries.markedAddress, blockUser, errorUser, isConsuming,
              inoutTermInstUsers.count(blockUser));
          emittedDiagnostic = true;
          continue;
        }
      }

      // Otherwise, add successors and continue! We didn't overlap with this
      // use.
      LLVM_DEBUG(llvm::dbgs() << "    Does not overlap at the type level, no "
                                 "diagnostic! Visiting successors!\n");
      for (auto *succBlock : block->getSuccessorBlocks())
        worklist.pushIfNotVisited(succBlock);
    }
  }

  return {emittedDiagnostic, foundSingleBlockTakeDueToInitDown};
}

bool GlobalDataflow::compute() {
  // Then revisit our takes, this time checking if we are within the boundary
  // and if we are, emit an error.
  LLVM_DEBUG(llvm::dbgs() << "Checking takes for errors!\n");
  bool emittedDiagnostic = false;
  bool foundSingleBlockTakeDueToInitDown = false;

  auto pair = testInstVectorLiveness(summaries.takeUpInsts);
  emittedDiagnostic |= pair.first;
  foundSingleBlockTakeDueToInitDown |= pair.second;

  pair = testInstVectorLiveness(summaries.takeDownInsts);
  emittedDiagnostic |= pair.first;
  foundSingleBlockTakeDueToInitDown |= pair.second;

  // If we emitted an error user, we should always emit at least one
  // diagnostic. If we didn't there is a bug in the implementation.
  assert(!hadAnyErrorUsers || emittedDiagnostic ||
         foundSingleBlockTakeDueToInitDown);
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

  // If we found a load [copy] or copy_addr that requires multiple copies, then
  // we emitted an early error. Bail now and allow the user to fix those errors
  // and recompile to get further errors.
  //
  // DISCUSSION: The reason why we do this is in the dataflow below we want to
  // be able to assume that the load [copy] or copy_addr/copy_addr [init] are
  // actual last uses, but the frontend that emitted the code for simplicity
  // emitted a copy from the base address + a destroy_addr of the use. By
  // bailing here, we can make that assumption since we would have errored
  // earlier otherwise.
  if (visitor.emittedEarlyDiagnostic)
    return true;

  // Now walk all of the blocks in the function... performing the single basic
  // block version of the algorithm and initializing our pruned liveness for our
  // interprocedural processing.
  LLVM_DEBUG(llvm::dbgs() << "Performing single basic block checks!\n");
  BlockSummaries summaries(*this, markedAddress);
  for (auto &block : *fn) {
    summaries.summarize(block);
  }

  // If we emitted a diagnostic while performing the single block check, just
  // bail early and let the user fix these issues in this functionand re-run the
  // compiler.
  //
  // DISCUSSION: This ensures that later when performing the global dataflow, we
  // can rely on the invariant that any potential single block cases are correct
  // already.
  if (diagnosticEmitter.emittedAnyDiagnostics())
    return true;

  //---
  // Multi-Block Liveness Dataflow
  //

  SmallVector<SILBasicBlock *, 32> discoveredBlocks;
  FieldSensitiveAddressPrunedLiveness liveness(fn, markedAddress,
                                               &discoveredBlocks);
  SmallPtrSet<SILInstruction *, 8> inoutTermUsers;
  summaries.initializeLiveness(liveness, inoutTermUsers);

  // If we have multiple blocks in the function, now run the global pruned
  // liveness dataflow.
  if (std::next(fn->begin()) != fn->end()) {
    // Then compute the takes that are within the cumulative boundary of
    // liveness that we have computed. If we find any, they are the errors ones.
    GlobalDataflow emitter(summaries, liveness, inoutTermUsers);

    // If we had any errors, we do not want to modify the SIL... just bail.
    if (emitter.compute()) {
      // TODO: Remove next line.
      diagnosticEmitter.valuesWithDiagnostics.insert(markedAddress);
      return true;
    }
  }

  // Ok, we not have emitted our main errors. Now we begin the
  // transformation. We begin by processing borrows. We can also emit errors
  // here if we find that we can not expand the borrow scope of the load [copy]
  // to all of its uses.
  SmallVector<DestroyValueInst *, 8> destroys;
  SmallVector<EndAccessInst *, 8> endAccesses;
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
          destroys.push_back(dvi);
          changed = true;
        }

        for (auto *d : destroys)
          d->eraseFromParent();
        li->replaceAllUsesWith(lbi);
        li->eraseFromParent();
        continue;
      }
    }

    llvm::dbgs() << "Borrow: " << *pair.first;
    llvm_unreachable("Unhandled case?!");
  }

  // Now that we have rewritten all read only copies into borrows, we need to
  // handle merging of copyable takes/destroys. Begin by pairing destroy_addr
  // and copyableTake.
  for (auto *destroy : addressUseState.destroys) {
    // First if we aren't the first instruction in the block, see if we have a
    // copyableToTake that pairs with us. If we are the first instruction in the
    // block, then we just go straight to the global dataflow stage below.
    if (destroy->getIterator() != destroy->getParent()->begin()) {
      bool foundSingleBlockCase = false;
      for (auto ii = std::prev(destroy->getReverseIterator()),
                ie = destroy->getParent()->rend();
           ii != ie; ++ii) {
        if (addressUseState.initInsts.count(&*ii) ||
            addressUseState.reinitInsts.count(&*ii))
          break;

        assert(!addressUseState.takeInsts.count(&*ii) &&
               "Double destroy?! Should have errored on this?!");

        if (addressUseState.copyInsts.count(&*ii)) {
          if (auto *li = dyn_cast<LoadInst>(&*ii)) {
            // If we have a copy in the single block case, erase the destroy
            // and visit the next one.
            destroy->eraseFromParent();
            changed = true;
            foundSingleBlockCase = true;
            break;
          }

          if (auto *copyAddr = dyn_cast<CopyAddrInst>(&*ii)) {
            destroy->eraseFromParent();
            changed = true;
            foundSingleBlockCase = true;
            break;
          }
        }

        if (addressUseState.livenessUses.count(&*ii)) {
          // If we have a liveness use, we break since this destroy_addr is
          // needed along this path.
          break;
        }
      }

      if (foundSingleBlockCase)
        continue;
    }
  }

  // Now that we have rewritten all borrows, convert any takes that are today
  // copies into their take form. If we couldn't do this, we would have had an
  // error.
  for (auto take : addressUseState.copyInsts) {
    if (auto *li = dyn_cast<LoadInst>(take.first)) {
      // Convert this to its take form.
      if (auto *access = dyn_cast<BeginAccessInst>(li->getOperand()))
        access->setAccessKind(SILAccessKind::Modify);
      li->setOwnershipQualifier(LoadOwnershipQualifier::Take);
      changed = true;
      continue;
    }

    if (auto *copy = dyn_cast<CopyAddrInst>(take.first)) {
      // Convert this to its take form.
      if (auto *access = dyn_cast<BeginAccessInst>(copy->getSrc()))
        access->setAccessKind(SILAccessKind::Modify);
      copy->setIsTakeOfSrc(IsTake);
      continue;
    }

    llvm::dbgs() << "Unhandled copy user: " << *take.first;
    llvm_unreachable("Unhandled case?!");
  }

  return true;
}

bool MoveOnlyChecker::check() {
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
            .check()) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveOnlyAddressChecker() {
  return new MoveOnlyCheckerPass();
}
