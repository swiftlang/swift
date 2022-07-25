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
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/NonLocalAccessBlockAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalOSSALifetime.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

static StringRef getVariableNameForValue(MarkMustCheckInst *mmci) {
  StringRef varName = "unknown";
  if (auto *use = getSingleDebugUse(mmci)) {
    DebugVarCarryingInst debugVar(use->getUser());
    if (auto varInfo = debugVar.getVarInfo()) {
      varName = varInfo->Name;
    } else {
      if (auto *decl = debugVar.getDecl()) {
        varName = decl->getBaseName().userFacingName();
      }
    }
  }

  return varName;
}

//===----------------------------------------------------------------------===//
//                                 Main Pass
//===----------------------------------------------------------------------===//

namespace {

struct MoveOnlyChecker {
  SILFunction *fn;

  /// A set of mark_must_check that we are actually going to process.
  SmallSetVector<MarkMustCheckInst *, 32> moveIntroducersToProcess;

  /// A per mark must check, vector of uses that copy propagation says need a
  /// copy and thus are not final consuming uses.
  SmallVector<Operand *, 32> consumingUsesNeedingCopy;

  /// A per mark must check, vector of consuming uses that copy propagation says
  /// are actual last uses.
  SmallVector<Operand *, 32> finalConsumingUses;

  MoveOnlyChecker(SILFunction *fn, DeadEndBlocks *deBlocks) : fn(fn) {}

  /// Search through the current function for candidate mark_must_check
  /// [noimplicitcopy]. If we find one that does not fit a pattern that we
  /// understand, emit an error diagnostic telling the programmer that the move
  /// checker did not know how to recognize this code pattern.
  ///
  /// \returns true if we deleted a mark_must_check inst that we didn't
  /// recognize after emitting the diagnostic.
  bool searchForCandidateMarkMustChecks();

  /// Emits an error diagnostic for \p markedValue.
  void emitDiagnostic(MarkMustCheckInst *markedValue,
                      bool originalValueGuaranteed);

  bool check(NonLocalAccessBlockAnalysis *accessBlockAnalysis,
             DominanceInfo *domTree);
};

} // namespace

bool MoveOnlyChecker::searchForCandidateMarkMustChecks() {
  bool changed = false;
  for (auto &block : *fn) {
    for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
      auto *mmci = dyn_cast<MarkMustCheckInst>(&*ii);
      ++ii;

      if (!mmci || !mmci->hasMoveCheckerKind())
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
      diagnose(fn->getASTContext(), mmci->getLoc().getSourceLoc(),
               diag::sil_moveonlychecker_not_understand_mark_move);
      mmci->replaceAllUsesWith(mmci->getOperand());
      mmci->eraseFromParent();
      changed = true;
    }
  }
  return changed;
}

void MoveOnlyChecker::emitDiagnostic(MarkMustCheckInst *markedValue,
                                     bool originalValueGuaranteed) {
  auto &astContext = fn->getASTContext();
  StringRef varName = getVariableNameForValue(markedValue);

  if (originalValueGuaranteed) {
    diagnose(astContext,
             markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_guaranteed_value_consumed, varName);
  } else {
    diagnose(astContext,
             markedValue->getDefiningInstruction()->getLoc().getSourceLoc(),
             diag::sil_moveonlychecker_owned_value_consumed_more_than_once,
             varName);
  }

  while (consumingUsesNeedingCopy.size()) {
    auto *consumingUse = consumingUsesNeedingCopy.pop_back_val();

    // See if the consuming use is an owned moveonly_to_copyable whose only
    // user is a return. In that case, use the return loc instead. We do this
    // b/c it is illegal to put a return value location on a non-return value
    // instruction... so we have to hack around this slightly.
    auto *user = consumingUse->getUser();
    auto loc = user->getLoc();
    if (auto *mtc = dyn_cast<MoveOnlyWrapperToCopyableValueInst>(user)) {
      if (auto *ri = mtc->getSingleUserOfType<ReturnInst>()) {
        loc = ri->getLoc();
      }
    }

    diagnose(astContext, loc.getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
  }

  while (finalConsumingUses.size()) {
    auto *consumingUse = finalConsumingUses.pop_back_val();

    // See if the consuming use is an owned moveonly_to_copyable whose only
    // user is a return. In that case, use the return loc instead. We do this
    // b/c it is illegal to put a return value location on a non-return value
    // instruction... so we have to hack around this slightly.
    auto *user = consumingUse->getUser();
    auto loc = user->getLoc();
    if (auto *mtc = dyn_cast<MoveOnlyWrapperToCopyableValueInst>(user)) {
      if (auto *ri = mtc->getSingleUserOfType<ReturnInst>()) {
        loc = ri->getLoc();
      }
    }

    diagnose(astContext, loc.getSourceLoc(),
             diag::sil_moveonlychecker_consuming_use_here);
  }
}

bool MoveOnlyChecker::check(NonLocalAccessBlockAnalysis *accessBlockAnalysis,
                            DominanceInfo *domTree) {
  bool changed = false;

  // First search for candidates to process and emit diagnostics on any
  // mark_must_check [noimplicitcopy] we didn't recognize.
  changed |= searchForCandidateMarkMustChecks();

  // If we didn't find any introducers to check, just return changed.
  //
  // NOTE: changed /can/ be true here if we had any mark_must_check
  // [noimplicitcopy] that we didn't understand and emitting a diagnostic upon
  // and then deleting.
  if (moveIntroducersToProcess.empty())
    return changed;

  auto callbacks =
      InstModCallbacks().onDelete([&](SILInstruction *instToDelete) {
        if (auto *mvi = dyn_cast<MarkMustCheckInst>(instToDelete))
          moveIntroducersToProcess.remove(mvi);
        instToDelete->eraseFromParent();
      });
  InstructionDeleter deleter(std::move(callbacks));

  auto foundConsumingUseNeedingCopy = [&](Operand *use) {
    consumingUsesNeedingCopy.push_back(use);
  };
  auto foundConsumingUseNotNeedingCopy = [&](Operand *use) {
    finalConsumingUses.push_back(use);
  };

  CanonicalizeOSSALifetime canonicalizer(
      false /*pruneDebugMode*/, false /*poisonRefsMode*/, accessBlockAnalysis,
      domTree, deleter, foundConsumingUseNeedingCopy,
      foundConsumingUseNotNeedingCopy);
  auto moveIntroducers = llvm::makeArrayRef(moveIntroducersToProcess.begin(),
                                            moveIntroducersToProcess.end());
  SmallPtrSet<MarkMustCheckInst *, 4> valuesWithDiagnostics;
  while (!moveIntroducers.empty()) {
    SWIFT_DEFER {
      consumingUsesNeedingCopy.clear();
      finalConsumingUses.clear();
    };

    MarkMustCheckInst *markedValue = moveIntroducers.front();
    moveIntroducers = moveIntroducers.drop_front(1);
    LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *markedValue);

    // First canonicalize ownership.
    changed |= canonicalizer.canonicalizeValueLifetime(markedValue);

    // If we are asked to perform guaranteed checking, emit an error if we have
    // /any/ consuming uses.
    if (markedValue->getCheckKind() == MarkMustCheckInst::CheckKind::NoCopy) {
      if (!consumingUsesNeedingCopy.empty() || !finalConsumingUses.empty()) {
        emitDiagnostic(markedValue, true /*original value guaranteed*/);
        valuesWithDiagnostics.insert(markedValue);
      }
      continue;
    }

    if (consumingUsesNeedingCopy.empty()) {
      // If we failed to understand how to perform the check or did not find
      // any targets... continue. In the former case we want to fail with a
      // checker did not understand diagnostic later and in the former, we
      // succeeded.
      continue;
    }

    emitDiagnostic(markedValue, false /*original value guaranteed*/);
    valuesWithDiagnostics.insert(markedValue);
  }
  bool emittedDiagnostic = valuesWithDiagnostics.size();

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
    if (!valuesWithDiagnostics.count(markedInst)) {
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
  // may have copy_value of @moveOnly typed values. This is not valid in
  // Canonical SIL, so we need to ensure that those copy_value become
  // explicit_copy_value. This is ok to do since we are already going to fail
  // the compilation and just are trying to maintain SIL invariants.
  //
  // It is also ok that we use a little more compile time and go over the
  // function again, since we are going to fail the compilation and not codegen.
  if (emittedDiagnostic) {
    for (auto &block : *fn) {
      for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
        auto *cvi = dyn_cast<CopyValueInst>(&*ii);
        ++ii;

        if (!cvi || !cvi->getOperand()->getType().isMoveOnlyWrapped())
          continue;

        SILBuilderWithScope b(cvi);
        auto *expCopy =
            b.createExplicitCopyValue(cvi->getLoc(), cvi->getOperand());
        cvi->replaceAllUsesWith(expCopy);
        cvi->eraseFromParent();
      }
    }
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

    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    assert(fn->getModule().getStage() == SILStage::Raw &&
           "Should only run on Raw SIL");

    auto *accessBlockAnalysis = getAnalysis<NonLocalAccessBlockAnalysis>();
    auto *dominanceAnalysis = getAnalysis<DominanceAnalysis>();
    DominanceInfo *domTree = dominanceAnalysis->get(fn);
    auto *deAnalysis = getAnalysis<DeadEndBlocksAnalysis>()->get(fn);

    if (MoveOnlyChecker(getFunction(), deAnalysis)
            .check(accessBlockAnalysis, domTree)) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveOnlyChecker() {
  return new MoveOnlyCheckerPass();
}
