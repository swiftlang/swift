//===--- MoveOnlyChecker.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-move-only-checker"

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/NodeBits.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PostOrder.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/StackList.h"
#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/NonLocalAccessBlockAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/CanonicalizeOSSALifetime.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "clang/AST/DeclTemplate.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/IntervalMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/RecyclingAllocator.h"

#include "MoveOnlyBorrowToDestructure.h"
#include "MoveOnlyDiagnostics.h"
#include "MoveOnlyObjectChecker.h"

using namespace swift;
using namespace swift::siloptimizer;

//===----------------------------------------------------------------------===//
//                Mark Must Check Candidate Search for Objects
//===----------------------------------------------------------------------===//

bool swift::siloptimizer::searchForCandidateObjectMarkMustChecks(
    SILFunction *fn,
    SmallSetVector<MarkMustCheckInst *, 32> &moveIntroducersToProcess,
    DiagnosticEmitter &emitter) {
  bool localChanged = false;
  for (auto &block : *fn) {
    for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
      auto *mmci = dyn_cast<MarkMustCheckInst>(&*ii);
      ++ii;

      if (!mmci || !mmci->hasMoveCheckerKind() || !mmci->getType().isObject())
        continue;

      // Handle guaranteed/owned move arguments and values.
      //
      // We are pattern matching against these patterns:
      //
      // bb0(%0 : @guaranteed $T):
      //   %1 = copy_value %0
      //   %2 = mark_must_check [no_copy] %1
      // bb0(%0 : @owned $T):
      //   %1 = mark_must_check [no_copy] %2
      //
      // This is forming a let or an argument.
      // bb0:
      //   %1 = move_value [lexical] %0
      //   %2 = mark_must_check [no_implicit_copy] %1
      //
      // This occurs when SILGen materializes a temporary move only value?
      // bb0:
      //   %1 = begin_borrow [lexical] %0
      //   %2 = copy_value %1
      //   %3 = mark_must_check [no_copy] %2
      if (mmci->getOperand()->getType().isMoveOnly() &&
          !mmci->getOperand()->getType().isMoveOnlyWrapped()) {
        if (auto *cvi = dyn_cast<CopyValueInst>(mmci->getOperand())) {
          if (auto *arg = dyn_cast<SILFunctionArgument>(cvi->getOperand())) {
            if (arg->getOwnershipKind() == OwnershipKind::Guaranteed) {
              moveIntroducersToProcess.insert(mmci);
              continue;
            }
          }

          if (auto *bbi = dyn_cast<BeginBorrowInst>(cvi->getOperand())) {
            if (bbi->isLexical()) {
              moveIntroducersToProcess.insert(mmci);
              continue;
            }
          }
        }

        // Any time we have a lexical move_value, we can process it.
        if (auto *mvi = dyn_cast<MoveValueInst>(mmci->getOperand())) {
          if (mvi->isLexical()) {
            moveIntroducersToProcess.insert(mmci);
            continue;
          }
        }

        if (auto *arg = dyn_cast<SILFunctionArgument>(mmci->getOperand())) {
          if (arg->getOwnershipKind() == OwnershipKind::Owned) {
            moveIntroducersToProcess.insert(mmci);
            continue;
          }
        }
      }

      // Handle guaranteed arguments.
      //
      // We are pattern matching this pattern:
      //
      // bb0(%0 : @guaranteed $T):
      //   %1 = copyable_to_moveonlywrapper [guaranteed] %0
      //   %2 = copy_value %1
      //   %3 = mark_must_check [no_copy] %2
      //
      // NOTE: Unlike with owned arguments, we do not need to insert a
      // begin_borrow lexical since the lexical value comes from the guaranteed
      // argument itself.
      //
      // NOTE: When we are done checking, we will eliminate the copy_value,
      // mark_must_check inst to leave the IR in a guaranteed state.
      if (auto *cvi = dyn_cast<CopyValueInst>(mmci->getOperand())) {
        if (auto *cvt = dyn_cast<CopyableToMoveOnlyWrapperValueInst>(
                cvi->getOperand())) {
          if (auto *arg = dyn_cast<SILFunctionArgument>(cvt->getOperand())) {
            if (arg->isNoImplicitCopy() &&
                arg->getOwnershipKind() == OwnershipKind::Guaranteed) {
              moveIntroducersToProcess.insert(mmci);
              continue;
            }
          }
        }
      }

      // Handle trivial arguments and values.
      //
      // In the instruction stream this looks like:
      //
      // bb0(%0 : $Trivial):
      //  %1 = copyable_to_moveonlywrapper [owned] %0
      //  %2 = move_value [lexical] %1
      //  %3 = mark_must_check [no_implicit_copy] %2
      //
      // *OR*
      //
      // bb0(%0 : $Trivial):
      //  %1 = copyable_to_moveonlywrapper [owned] %0
      //  %2 = move_value [lexical] %1
      //  %3 = mark_must_check [no_copy] %2
      //
      // We are relying on a structural SIL requirement that %0 has only one
      // use, %1. This is validated by the SIL verifier. In this case, we need
      // the move_value [lexical] to ensure that we get a lexical scope for the
      // non-trivial value.
      if (auto *mvi = dyn_cast<MoveValueInst>(mmci->getOperand())) {
        if (mvi->isLexical()) {
          if (auto *cvt = dyn_cast<CopyableToMoveOnlyWrapperValueInst>(
                  mvi->getOperand())) {
            if (cvt->getOperand()->getType().isTrivial(*fn)) {
              moveIntroducersToProcess.insert(mmci);
              continue;
            }
          }
        }
      }

      // Handle owned arguments.
      //
      // We are pattern matching this:
      //
      // bb0(%0 : @owned $T):
      //   %1 = copyable_to_moveonlywrapper [owned] %0
      //   %2 = move_value [lexical] %1
      //   %3 = mark_must_check [no_implicit_copy_owned] %2
      if (auto *mvi = dyn_cast<MoveValueInst>(mmci->getOperand())) {
        if (mvi->isLexical()) {
          if (auto *cvt = dyn_cast<CopyableToMoveOnlyWrapperValueInst>(
                  mvi->getOperand())) {
            if (auto *arg = dyn_cast<SILFunctionArgument>(cvt->getOperand())) {
              if (arg->isNoImplicitCopy()) {
                moveIntroducersToProcess.insert(mmci);
                continue;
              }
            }
          }
        }
      }

      // Handle non-trivial values.
      //
      // We are looking for the following pattern:
      //
      //  %1 = begin_borrow [lexical] %0
      //  %2 = copy_value %1
      //  %3 = copyable_to_moveonlywrapper [owned] %2
      //  %4 = mark_must_check [no_implicit_copy]
      //
      // Or for a move only type, we look for a move_value [lexical].
      if (auto *mvi = dyn_cast<CopyableToMoveOnlyWrapperValueInst>(
              mmci->getOperand())) {
        if (auto *cvi = dyn_cast<CopyValueInst>(mvi->getOperand())) {
          if (auto *bbi = dyn_cast<BeginBorrowInst>(cvi->getOperand())) {
            if (bbi->isLexical()) {
              moveIntroducersToProcess.insert(mmci);
              continue;
            }
          }
        }
      }

      // Handle trivial values.
      //
      // We pattern match:
      //
      // %1 = copyable_to_moveonlywrapper [owned] %0
      // %2 = move_value [lexical] %1
      // %3 = mark_must_check [no_implicit_copy] %2
      if (auto *cvi = dyn_cast<ExplicitCopyValueInst>(mmci->getOperand())) {
        if (auto *bbi = dyn_cast<BeginBorrowInst>(cvi->getOperand())) {
          if (bbi->isLexical()) {
            moveIntroducersToProcess.insert(mmci);
            continue;
          }
        }
      }

      // If we see a mark_must_check that is marked no implicit copy that we
      // don't understand, emit a diagnostic to fail the compilation. This
      // ensures that if someone marks something no implicit copy and we fail to
      // check it, we fail the compilation.
      //
      // We then RAUW the mark_must_check once we have emitted the error since
      // later passes expect that mark_must_check has been eliminated by
      // us. Since we are failing already, this is ok to do.
      emitter.emitCheckerDoesntUnderstandDiagnostic(mmci);
      mmci->replaceAllUsesWith(mmci->getOperand());
      mmci->eraseFromParent();
      localChanged = true;
    }
  }
  return localChanged;
}

//===----------------------------------------------------------------------===//
//                  MARK: Cleanup After Emitting Diagnostic
//===----------------------------------------------------------------------===//

bool swift::siloptimizer::cleanupSILAfterEmittingObjectMoveOnlyDiagnostics(
    SILFunction *fn) {
  bool localChanged = false;
  for (auto &block : *fn) {
    for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
      if (auto *cvi = dyn_cast<CopyValueInst>(&*ii)) {
        ++ii;

        if (!cvi || !cvi->getOperand()->getType().isMoveOnly())
          continue;

        SILBuilderWithScope b(cvi);
        auto *expCopy =
            b.createExplicitCopyValue(cvi->getLoc(), cvi->getOperand());
        cvi->replaceAllUsesWith(expCopy);
        cvi->eraseFromParent();
        localChanged = true;
        continue;
      }

      // Also eliminate any mark_must_check on objects, just to be safe. We
      // emitted an object level diagnostic and if the user wants to get more
      // diagnostics, they should fix these diagnostics and recompile.
      if (auto *mmci = dyn_cast<MarkMustCheckInst>(&*ii)) {
        ++ii;

        if (mmci->getType().isAddress())
          continue;

        mmci->replaceAllUsesWith(mmci->getOperand());
        mmci->eraseFromParent();
        localChanged = true;
        continue;
      }

      ++ii;
    }
  }
  return localChanged;
}

//===----------------------------------------------------------------------===//
//                          MARK: OSSACanonicalizer
//===----------------------------------------------------------------------===//

void OSSACanonicalizer::computeBoundaryData(SILValue value) {
  // Now we have our liveness information. First compute the original boundary
  // (which ignores destroy_value).
  PrunedLivenessBoundary originalBoundary;
  canonicalizer->findOriginalBoundary(originalBoundary);

  // Then use that information to stash for our diagnostics the boundary
  // consuming/non-consuming users as well as enter the boundary consuming users
  // into the boundaryConsumignUserSet for quick set testing later.
  using IsInterestingUser = CanonicalizeOSSALifetime::IsInterestingUser;
  InstructionSet boundaryConsumingUserSet(value->getFunction());
  for (auto *lastUser : originalBoundary.lastUsers) {
    LLVM_DEBUG(llvm::dbgs() << "Looking at boundary use: " << *lastUser);
    switch (canonicalizer->isInterestingUser(lastUser)) {
    case IsInterestingUser::NonUser:
      llvm_unreachable("Last user of original boundary should be a user?!");
    case IsInterestingUser::NonLifetimeEndingUse:
      LLVM_DEBUG(llvm::dbgs() << "    NonLifetimeEndingUse!\n");
      nonConsumingBoundaryUsers.push_back(lastUser);
      continue;
    case IsInterestingUser::LifetimeEndingUse:
      LLVM_DEBUG(llvm::dbgs() << "    LifetimeEndingUse!\n");
      consumingBoundaryUsers.push_back(lastUser);
      boundaryConsumingUserSet.insert(lastUser);
      continue;
    }
  }

  // Then go through any of the consuming interesting uses found by our liveness
  // and any that are not on the boundary are ones that we must error for.
  for (auto *consumingUser : canonicalizer->getLifetimeEndingUsers()) {
    bool isConsumingUseOnBoundary =
        boundaryConsumingUserSet.contains(consumingUser);
    LLVM_DEBUG(llvm::dbgs() << "Is consuming user on boundary "
                            << (isConsumingUseOnBoundary ? "yes" : "no") << ": "
                            << *consumingUser);
    if (!isConsumingUseOnBoundary) {
      consumingUsesNeedingCopy.push_back(consumingUser);
    }
  }
}

bool OSSACanonicalizer::canonicalize(SILValue value) {
  // First compute liveness. If we fail, bail.
  if (!computeLiveness(value)) {
    return false;
  }

  computeBoundaryData(value);

  // Finally, rewrite lifetimes.
  rewriteLifetimes();

  return true;
}

//===----------------------------------------------------------------------===//
//                         MARK: Forward Declaration
//===----------------------------------------------------------------------===//

namespace {

struct MoveOnlyChecker {
  SILFunction *fn;

  bool changed = false;

  /// A set of mark_must_check that we are actually going to process.
  SmallSetVector<MarkMustCheckInst *, 32> moveIntroducersToProcess;

  borrowtodestructure::IntervalMapAllocator allocator;

  MoveOnlyChecker(SILFunction *fn, DeadEndBlocks *deBlocks) : fn(fn) {}

  void check(DominanceInfo *domTree, PostOrderAnalysis *poa);

  bool convertBorrowExtractsToOwnedDestructures(MarkMustCheckInst *mmci,
                                                DiagnosticEmitter &emitter,
                                                DominanceInfo *domTree,
                                                PostOrderAnalysis *poa);

  /// After we have emitted a diagnostic, we need to clean up the instruction
  /// stream by converting /all/ copies of move only typed things to use
  /// explicit_copy_value so that we maintain the SIL invariant that in
  /// canonical SIL move only types are not copied by normal copies.
  ///
  /// Returns true if we actually changed any instructions.
  bool cleanupAfterEmittingDiagnostic() {
    bool localChange = cleanupSILAfterEmittingObjectMoveOnlyDiagnostics(fn);
    changed |= localChange;
    return localChange;
  }

  bool checkForSameInstMultipleUseErrors(MarkMustCheckInst *base,
                                         DiagnosticEmitter &emitter);
};

} // namespace

bool MoveOnlyChecker::convertBorrowExtractsToOwnedDestructures(
    MarkMustCheckInst *mmci, DiagnosticEmitter &diagnosticEmitter,
    DominanceInfo *domTree, PostOrderAnalysis *poa) {
  BorrowToDestructureTransform transform(allocator, mmci, diagnosticEmitter,
                                         poa);
  if (!transform.transform()) {
    LLVM_DEBUG(llvm::dbgs()
               << "Failed to perform borrow to destructure transform!\n");
    return false;
  }

  return true;
}

bool MoveOnlyChecker::checkForSameInstMultipleUseErrors(
    MarkMustCheckInst *mmci, DiagnosticEmitter &diagnosticEmitter) {
  LLVM_DEBUG(llvm::dbgs() << "Checking for same inst multiple use error!\n");

  SmallFrozenMultiMap<SILInstruction *, Operand *, 8> instToOperandsMap;
  StackList<Operand *> worklist(mmci->getFunction());
  for (auto *use : mmci->getUses())
    worklist.push_back(use);

  while (!worklist.empty()) {
    auto *nextUse = worklist.pop_back_val();

    switch (nextUse->getOperandOwnership()) {
    case OperandOwnership::NonUse:
      continue;

    // Conservatively treat a conversion to an unowned value as a pointer
    // escape. If we see this in the SIL, fail and return false so we emit a
    // "compiler doesn't understand error".
    case OperandOwnership::ForwardingUnowned:
    case OperandOwnership::PointerEscape:
    case OperandOwnership::BitwiseEscape:
      LLVM_DEBUG(llvm::dbgs()
                 << "        Found forwarding unowned or escape!\n");
      return false;

    case OperandOwnership::TrivialUse:
    case OperandOwnership::InstantaneousUse:
    case OperandOwnership::UnownedInstantaneousUse:
      // Look through copy_value.
      if (auto *cvi = dyn_cast<CopyValueInst>(nextUse->getUser())) {
        for (auto *use : cvi->getUses())
          worklist.push_back(use);
        continue;
      }

      // Treat these as non-consuming uses that could have a consuming use as an
      // additional operand.
      LLVM_DEBUG(llvm::dbgs()
                 << "        Found non consuming use: " << *nextUse->getUser());
      instToOperandsMap.insert(nextUse->getUser(), nextUse);
      continue;
    case OperandOwnership::InteriorPointer:
      // We do not care about interior pointer uses since there aren't any
      // interior pointer using instructions that are also consuming uses.
      continue;

    case OperandOwnership::DestroyingConsume:
      if (isa<DestroyValueInst>(nextUse->getUser()))
        continue;
      [[fallthrough]];
    case OperandOwnership::ForwardingConsume:
      LLVM_DEBUG(llvm::dbgs()
                 << "        Found consuming use: " << *nextUse->getUser());

      instToOperandsMap.insert(nextUse->getUser(), nextUse);
      continue;

    case OperandOwnership::EndBorrow:
    case OperandOwnership::Reborrow:
    case OperandOwnership::GuaranteedForwarding:
      llvm_unreachable(
          "We do not process borrows recursively so should never see this.");

    case OperandOwnership::Borrow:
      // We don't care about borrows so we don't process them recursively
      continue;
    }
  }

  // Ok, we have our list of potential uses. Sort the multi-map and then search
  // for errors.
  instToOperandsMap.setFrozen();
  for (auto pair : instToOperandsMap.getRange()) {
    LLVM_DEBUG(llvm::dbgs()
               << "Checking inst for multiple uses: " << *pair.first);
    Operand *foundConsumingUse = nullptr;
    Operand *foundNonConsumingUse = nullptr;
    for (auto *use : pair.second) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    Visiting use: " << use->getOperandNumber() << '\n');
      if (use->isConsuming()) {
        LLVM_DEBUG(llvm::dbgs() << "        Is consuming!\n");
        if (foundConsumingUse) {
          // Emit error.
          LLVM_DEBUG(
              llvm::dbgs()
              << "        Had previous consuming use! Emitting error!\n");
          diagnosticEmitter.emitObjectInstConsumesValueTwice(
              mmci, foundConsumingUse, use);
          continue;
        }

        if (foundNonConsumingUse) {
          LLVM_DEBUG(
              llvm::dbgs()
              << "        Had previous non consuming use! Emitting error!\n");
          // Emit error.
          diagnosticEmitter.emitObjectInstConsumesAndUsesValue(
              mmci, use, foundNonConsumingUse);
          continue;
        }

        if (!foundConsumingUse)
          foundConsumingUse = use;
        continue;
      }

      LLVM_DEBUG(llvm::dbgs() << "        Is non consuming!\n");
      if (foundConsumingUse) {
        // Emit error.
        LLVM_DEBUG(llvm::dbgs()
                   << "        Had previous consuming use! Emitting error!\n");
        diagnosticEmitter.emitObjectInstConsumesAndUsesValue(
            mmci, foundConsumingUse, use);
        continue;
      }

      if (!foundNonConsumingUse)
        foundNonConsumingUse = use;
    }
  }

  return true;
}

//===----------------------------------------------------------------------===//
//                             MARK: Main Routine
//===----------------------------------------------------------------------===//

void MoveOnlyChecker::check(DominanceInfo *domTree, PostOrderAnalysis *poa) {
  auto callbacks =
      InstModCallbacks().onDelete([&](SILInstruction *instToDelete) {
        if (auto *mvi = dyn_cast<MarkMustCheckInst>(instToDelete))
          moveIntroducersToProcess.remove(mvi);
        instToDelete->eraseFromParent();
      });
  InstructionDeleter deleter(std::move(callbacks));
  OSSACanonicalizer canonicalizer;
  canonicalizer.init(fn, domTree, deleter);
  DiagnosticEmitter diagnosticEmitter;
  diagnosticEmitter.init(fn, &canonicalizer);

  // First search for candidates to process and emit diagnostics on any
  // mark_must_check [noimplicitcopy] we didn't recognize.
  bool madeChange = searchForCandidateObjectMarkMustChecks(
      fn, moveIntroducersToProcess, diagnosticEmitter);
  LLVM_DEBUG(llvm::dbgs()
             << "Emitting diagnostic when checking for mark must check inst: "
             << (diagnosticEmitter.emittedAnyDiagnostics() ? "yes" : "no")
             << '\n');

  // If we didn't find any introducers to check, just return if we emitted an
  // error (which is the only way we emitted a change to the instruction
  // stream).
  //
  // NOTE: changed /can/ be true here if we had any mark_must_check
  // [noimplicitcopy] that we didn't understand and emitting a diagnostic upon
  // and then deleting.
  if (moveIntroducersToProcess.empty()) {
    LLVM_DEBUG(llvm::dbgs()
               << "No move introducers found?! Returning early?!\n");
    if (madeChange) {
      cleanupAfterEmittingDiagnostic();
    }
    return;
  }

  auto moveIntroducers = llvm::makeArrayRef(moveIntroducersToProcess.begin(),
                                            moveIntroducersToProcess.end());
  for (auto *introducer : moveIntroducers) {
    LLVM_DEBUG(llvm::dbgs() << "Found move introducer: " << *introducer);
  }

  while (!moveIntroducers.empty()) {
    SWIFT_DEFER { canonicalizer.clear(); };

    MarkMustCheckInst *markedValue = moveIntroducers.front();
    moveIntroducers = moveIntroducers.drop_front(1);
    LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *markedValue);

    // Before we do anything, we need to look for borrowed extracted values and
    // convert them to destructure operations.
    unsigned diagnosticCount = diagnosticEmitter.getDiagnosticCount();
    if (!convertBorrowExtractsToOwnedDestructures(
            markedValue, diagnosticEmitter, domTree, poa)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Borrow extract to owned destructure transformation didn't "
                    "understand part of the SIL\n");
      diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(markedValue);
      continue;
    }

    // If we emitted any non-exceptional diagnostics in
    // convertBorrowExtractsToOwnedDestructures, continue and process the next
    // instruction. The user can fix and re-compile. We want the OSSA
    // canonicalizer to be able to assume that all such borrow + struct_extract
    // uses were already handled.
    if (diagnosticCount != diagnosticEmitter.getDiagnosticCount()) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Emitting diagnostic in BorrowExtractToOwnedDestructure "
                    "transformation!\n");
      continue;
    }

    // First search for transitive consuming uses and prove that we do not have
    // any errors where a single instruction consumes the same value twice or
    // consumes and uses a value.
    if (!checkForSameInstMultipleUseErrors(markedValue, diagnosticEmitter)) {
      LLVM_DEBUG(llvm::dbgs() << "checkForSameInstMultipleUseError didn't "
                                 "understand part of the SIL\n");
      diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(markedValue);
      continue;
    }

    if (diagnosticCount != diagnosticEmitter.getDiagnosticCount()) {
      LLVM_DEBUG(llvm::dbgs() << "Found single inst multiple user error!\n");
      continue;
    }

    // Once that is complete, we then begin to canonicalize ownership, finding
    // our boundary and any uses that need a copy. We in this section only deal
    // with instructions due to our first step where we emitted errors for
    // instructions containing multiple operands.

    // Step 1. Compute liveness.
    if (!canonicalizer.computeLiveness(markedValue)) {
      diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(markedValue);
      LLVM_DEBUG(
          llvm::dbgs()
          << "Emitted checker doesnt understand diagnostic! Exiting early!\n");
      continue;
    } else {
      // Always set changed to true if we succeeded in canonicalizing since we
      // may have rewritten copies.
      changed = true;
    }

    // NOTE: In the following we only rewrite lifetimes once we have emitted
    // diagnostics. This ensures that we can emit diagnostics using the the
    // liveness information before rewrite lifetimes has enriched the liveness
    // info with maximized liveness information.

    // Step 2. Compute our boundary non consuming, consuming uses, and consuming
    // uses that need copies.
    canonicalizer.computeBoundaryData(markedValue);

    // If we are asked to perform guaranteed checking, emit an error if we have
    // /any/ consuming boundary uses or uses that need copies and then rewrite
    // lifetimes.
    if (markedValue->getCheckKind() == MarkMustCheckInst::CheckKind::NoCopy) {
      if (canonicalizer.foundAnyConsumingUses()) {
        diagnosticEmitter.emitObjectGuaranteedDiagnostic(markedValue);
      }
      canonicalizer.rewriteLifetimes();
      continue;
    }

    if (!canonicalizer.foundConsumingUseRequiringCopy()) {
      // If we failed to understand how to perform the check or did not find
      // any targets... continue. In the former case we want to fail with a
      // checker did not understand diagnostic later and in the former, we
      // succeeded.
      canonicalizer.rewriteLifetimes();
      continue;
    }

    // Finally emit our object owned diagnostics and then rewrite lifetimes.
    diagnosticEmitter.emitObjectOwnedDiagnostic(markedValue);
    canonicalizer.rewriteLifetimes();
  }

  bool emittedDiagnostic = diagnosticEmitter.emittedAnyDiagnostics();
  LLVM_DEBUG(llvm::dbgs() << "Emitting checker based diagnostic: "
                          << (emittedDiagnostic ? "yes" : "no") << '\n');

  // Ok, we have success. All of our marker instructions were proven as safe or
  // we emitted a diagnostic. Now we need to clean up the IR by eliminating our
  // marker instructions to signify that the checked SIL is correct. We also
  // perform some small cleanups for guaranteed values if we emitted a
  // diagnostic on them.
  //
  // NOTE: This is enforced in the verifier by only allowing MarkMustCheckInst
  // in Raw SIL. This ensures we do not miss any.
  //
  // NOTE: destroys is a separate array that we use to avoid iterator
  // invalidation when cleaning up destroy_value of guaranteed checked values.
  SmallVector<DestroyValueInst *, 8> destroys;
  while (!moveIntroducersToProcess.empty()) {
    auto *markedInst = moveIntroducersToProcess.pop_back_val();

    // If we didn't emit a diagnostic on a non-trivial guaranteed argument,
    // eliminate the copy_value, destroy_values, and the mark_must_check.
    if (!diagnosticEmitter.emittedDiagnosticForValue(markedInst)) {
      if (markedInst->getCheckKind() == MarkMustCheckInst::CheckKind::NoCopy) {
        if (auto *cvi = dyn_cast<CopyValueInst>(markedInst->getOperand())) {
          SingleValueInstruction *i = cvi;
          if (auto *copyToMoveOnly =
                  dyn_cast<CopyableToMoveOnlyWrapperValueInst>(
                      cvi->getOperand())) {
            i = copyToMoveOnly;
          }

          if (auto *arg = dyn_cast<SILFunctionArgument>(i->getOperand(0))) {
            if (arg->getOwnershipKind() == OwnershipKind::Guaranteed) {
              for (auto *use : markedInst->getConsumingUses()) {
                destroys.push_back(cast<DestroyValueInst>(use->getUser()));
              }
              while (!destroys.empty())
                destroys.pop_back_val()->eraseFromParent();
              markedInst->replaceAllUsesWith(arg);
              markedInst->eraseFromParent();
              cvi->eraseFromParent();
              continue;
            }
          }
        }
      }
    }

    markedInst->replaceAllUsesWith(markedInst->getOperand());
    markedInst->eraseFromParent();
    changed = true;
  }

  // Once we have finished processing, if we emitted any diagnostics, then we
  // may have copy_value of move only and @moveOnly wrapped type values. This is
  // not valid in Canonical SIL, so we need to ensure that those copy_value
  // become explicit_copy_value. This is ok to do since we are already going to
  // fail the compilation and just are trying to maintain SIL invariants.
  //
  // It is also ok that we use a little more compile time and go over the
  // function again, since we are going to fail the compilation and not codegen.
  if (emittedDiagnostic) {
    changed |= cleanupAfterEmittingDiagnostic();
  }
}

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
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

    LLVM_DEBUG(llvm::dbgs() << "===> MoveOnly Object Checker. Visiting: "
                            << fn->getName() << '\n');

    auto *dominanceAnalysis = getAnalysis<DominanceAnalysis>();
    DominanceInfo *domTree = dominanceAnalysis->get(fn);
    auto *deAnalysis = getAnalysis<DeadEndBlocksAnalysis>()->get(fn);
    auto *postOrderAnalysis = getAnalysis<PostOrderAnalysis>();

    MoveOnlyChecker checker(getFunction(), deAnalysis);
    checker.check(domTree, postOrderAnalysis);
    if (checker.changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveOnlyObjectChecker() {
  return new MoveOnlyCheckerPass();
}
