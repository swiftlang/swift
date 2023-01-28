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
#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/InstructionUtils.h"
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
      }

      ++ii;
    }
  }
  return localChanged;
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

  MoveOnlyChecker(SILFunction *fn, DeadEndBlocks *deBlocks) : fn(fn) {}

  void check(DominanceInfo *domTree);

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
};

} // namespace

//===----------------------------------------------------------------------===//
//                             MARK: Main Routine
//===----------------------------------------------------------------------===//

void MoveOnlyChecker::check(DominanceInfo *domTree) {
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

    // First canonicalize ownership.
    //
    // NOTE: We previously ran BorrowToDestructureTransform to ensure that any
    // struct_extract/tuple_extracts that we see are converted to destructures
    // or we errored.
    if (!canonicalizer.canonicalize(markedValue)) {
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

    // If we are asked to perform guaranteed checking, emit an error if we have
    // /any/ consuming uses.
    if (markedValue->getCheckKind() == MarkMustCheckInst::CheckKind::NoCopy) {
      if (canonicalizer.foundAnyConsumingUses()) {
        diagnosticEmitter.emitObjectGuaranteedDiagnostic(markedValue);
      }
      continue;
    }

    if (!canonicalizer.foundConsumingUseRequiringCopy()) {
      // If we failed to understand how to perform the check or did not find
      // any targets... continue. In the former case we want to fail with a
      // checker did not understand diagnostic later and in the former, we
      // succeeded.
      continue;
    }

    diagnosticEmitter.emitObjectOwnedDiagnostic(markedValue);
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
          if (auto *arg = dyn_cast<SILFunctionArgument>(cvi->getOperand())) {
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

    MoveOnlyChecker checker(getFunction(), deAnalysis);
    checker.check(domTree);
    if (checker.changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

    if (getOptions().VerifyAll) {
      for (auto &block : *getFunction()) {
        for (auto &inst : block) {
          if (auto *cvi = dyn_cast<CopyValueInst>(&inst)) {
            if (cvi->getOperand()->getType().isMoveOnly()) {
              llvm::errs() << "Should have eliminated copy at this point: "
                           << *cvi;
              llvm::report_fatal_error("standard compiler error");
            }
          }
        }
      }
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveOnlyObjectChecker() {
  return new MoveOnlyCheckerPass();
}
