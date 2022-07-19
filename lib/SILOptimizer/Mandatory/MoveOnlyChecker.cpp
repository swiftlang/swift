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
//                    Copy of Borrowed Projection Checker
//===----------------------------------------------------------------------===//

namespace {

struct LivenessInfo {
  SmallVector<SILBasicBlock *, 8> discoveredBlocks;
  PrunedLiveness liveness;
  PrunedLivenessBoundary livenessBoundary;

  LivenessInfo() : discoveredBlocks(), liveness(&discoveredBlocks) {}

  void clear() {
    discoveredBlocks.clear();
    liveness.clear();
    livenessBoundary.clear();
  }

  void computeBoundary() {
    assert(!liveness.empty());
    livenessBoundary.compute(liveness);
  }

  ArrayRef<SILInstruction *> getLastUsers() const {
    return livenessBoundary.lastUsers;
  }
};

/// Once we have finished checking that a base owned value is not consumed
/// unnecessarily, we need to look at all instances where the owned value was
/// borrowed and prove that it was never copied in its borrowed form because the
/// borrowed value must be consumed.
///
/// As part of this optimization, we flatten the borrowed lifetime and eliminate
/// any copies that are only consumed by a destroy_value. If we find that the
/// flattened borrowed value needs a copy to extend the lifetime of the value,
/// we treat the lifetime extending use as requiring a consuming use.
struct CopyOfBorrowedProjectionChecker {
  SmallVector<SILInstruction *, 4> foundConsumesOfBorrowedSubValues;
  SmallVector<SILInstruction *, 4> foundCopiesOfBorrowedSubValues;
  SmallVector<SILInstruction *, 4> foundDestroysOfBorrowedSubValues;
  SmallVector<SILInstruction *, 4> foundRecursiveBorrows;
  SmallVector<SILInstruction *, 4> foundRecursiveEndBorrow;
  SmallVector<ForwardingOperand, 4> foundOwnedForwardingUses;
  LivenessInfo livenessInfo;
  LivenessInfo markedValueExtendedLiveRangeInfo;
  DeadEndBlocks *deBlocks;

  CopyOfBorrowedProjectionChecker(DeadEndBlocks *deBlocks)
      : deBlocks(deBlocks) {}

  /// Performs checking for \p markedValue. Returns true if we were able to
  /// successfully check/optimize. Returns false if we failed and did not
  /// perform any work. In such a case, we should bail early even though we did
  /// not emit a diagnostic so we emit a "checker did not understand" diagnostic
  /// later.
  bool check(SILValue markedValue);

  bool shouldEmitDiagnostic() const {
    return !foundConsumesOfBorrowedSubValues.empty();
  }

  void clear() {
    foundConsumesOfBorrowedSubValues.clear();
    foundCopiesOfBorrowedSubValues.clear();
    foundDestroysOfBorrowedSubValues.clear();
    foundRecursiveBorrows.clear();
    foundRecursiveEndBorrow.clear();
    foundOwnedForwardingUses.clear();
    livenessInfo.clear();
    markedValueExtendedLiveRangeInfo.clear();
  }
};

} // namespace

/// Walking from defs -> uses, see if \p markedValue has any uses by a borrow
/// introducing instruction looking through owned consuming instructions. \p
/// foundBorrows contains the resulting borrows that we found. \p
/// extendedLiveRangeLiveness is returned having been initialized with the live
/// range of markedValue ignoring Owned Forwarding.
static void
gatherBorrowsToCheckOfMovedValue(SILValue markedValue,
                                 SmallVectorImpl<SILValue> &foundBorrows,
                                 PrunedLiveness &extendedLiveRangeLiveness) {
  SmallVector<SILValue, 8> worklist;
  worklist.push_back(markedValue);

  extendedLiveRangeLiveness.clear();
  extendedLiveRangeLiveness.initializeDefBlock(markedValue->getParentBlock());

  while (!worklist.empty()) {
    auto value = worklist.pop_back_val();

    for (auto *use : value->getUses()) {
      auto *user = use->getUser();
      switch (use->getOperandOwnership()) {
      case OperandOwnership::NonUse:
        break;
      case OperandOwnership::TrivialUse:
        llvm_unreachable("this operand cannot handle ownership");
      case OperandOwnership::ForwardingUnowned:
      case OperandOwnership::PointerEscape:
      case OperandOwnership::InstantaneousUse:
      case OperandOwnership::UnownedInstantaneousUse:
      case OperandOwnership::BitwiseEscape:
        // We don't care about these. We are looking for borrows and
        // forwarding consumes!
        break;
      case OperandOwnership::ForwardingConsume:
        if (auto op = ForwardingOperand(use)) {
          bool addedToWorklist = false;
          op.visitForwardedValues([&](SILValue v) {
            // If the forwarded value is non-trivial, we want to visit it.
            //
            // QUESTION: Should we make this use then a non-lifetime ending use
            // just in case?
            if (!v->getType().isTrivial(*user->getFunction())) {
              addedToWorklist = true;
              worklist.push_back(v);
            }
            return true;
          });
          // If we have a forwarded value to look through, we want its uses to
          // provide liveness. If we did not find any such values, we want to
          // still track this value as the end of our parent value's lifetime.
          if (addedToWorklist)
            break;
        }

        extendedLiveRangeLiveness.updateForUse(user,
                                               true /*is lifetime ending*/);
        break;
      case OperandOwnership::DestroyingConsume:
        // We do not care about destroying consume uses.
        extendedLiveRangeLiveness.updateForUse(user,
                                               true /*is lifetime ending*/);
        break;
      case OperandOwnership::Borrow: {
        foundBorrows.push_back(cast<BeginBorrowInst>(user));
        break;
      }
      case OperandOwnership::EndBorrow:
      case OperandOwnership::ForwardingBorrow:
      case OperandOwnership::InteriorPointer:
      case OperandOwnership::Reborrow:
        llvm_unreachable("Should never see these on owned values");
      }
    }
  }
}

bool CopyOfBorrowedProjectionChecker::check(SILValue markedValue) {
  auto *parentBlock = markedValue->getParentBlock();
  if (!parentBlock)
    return false;

  auto &liveness = livenessInfo.liveness;
  liveness.initializeDefBlock(parentBlock);

  LLVM_DEBUG(llvm::dbgs() << "CopyOfBorrowedProjection. MovedValue: "
                          << markedValue);

  // Call this utility routine to gather up our worklist of borrows and also the
  // liveness of the moved value looking through forwarding uses. We use this
  // liveness info later to determine if we require a copy_value of a subobject
  // b/c the subobject use is outside the lifetime of the of the marked object.
  SmallVector<SILValue, 8> worklist;
  gatherBorrowsToCheckOfMovedValue(markedValue, worklist,
                                   markedValueExtendedLiveRangeInfo.liveness);

  if (worklist.empty()) {
    LLVM_DEBUG(llvm::dbgs() << "CopyOfBorrowedProjection. No borrows to check! "
                               "No Diagnostic Needed!\n");
    return true;
  }

  // Ok, we now have our guaranteed values. Looking through guaranteed
  // forwarding uses, search for a copy_value. If we found one, then we add
  // it to the consumingUsesNeedingCopy array and use the same processing
  // below. Otherwise, if we do not find any, then we are truly safe and
  // continue.
  while (!worklist.empty()) {
    SILValue value = worklist.pop_back_val();
    LLVM_DEBUG(llvm::dbgs()
               << "CopyOfBorrowedProjection. Visiting Value: " << value);

    for (auto *use : value->getUses()) {
      auto *user = use->getUser();
      LLVM_DEBUG(llvm::dbgs()
                 << "CopyOfBorrowedProjection.     User: " << *user);
      LLVM_DEBUG(llvm::dbgs()
                 << "CopyOfBorrowedProjection.     Operand Ownership: "
                 << use->getOperandOwnership() << '\n');

      switch (use->getOperandOwnership()) {
      case OperandOwnership::NonUse:
        break;
      case OperandOwnership::TrivialUse:
        llvm_unreachable("this operand cannot handle ownership");

      // Conservatively treat a conversion to an unowned value as a pointer
      // escape. Is it legal to canonicalize ForwardingUnowned?
      case OperandOwnership::ForwardingUnowned:
      case OperandOwnership::PointerEscape:
        // Just add liveness.
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        break;
      case OperandOwnership::InstantaneousUse:
      case OperandOwnership::UnownedInstantaneousUse:
      case OperandOwnership::BitwiseEscape:
        // Add liveness to be careful around dead end blocks until in OSSA we no
        // longer use those.
        liveness.updateForUse(user, /*lifetimeEnding*/ false);

        // Look through copy_value.
        if (auto *cvi = dyn_cast<CopyValueInst>(user)) {
          foundCopiesOfBorrowedSubValues.push_back(cvi);
          worklist.push_back(cvi);
        }

        break;
      case OperandOwnership::ForwardingConsume: {
        if (auto op = ForwardingOperand(use)) {
          // If our user is not directly forwarding, we cannot convert its
          // ownership to be guaranteed, so we treat it as a true consuming use.
          if (!op.preservesOwnership()) {
            foundConsumesOfBorrowedSubValues.push_back(user);
            liveness.updateForUse(user, /*lifetimeEnding*/ true);
            break;
          }

          // Otherwise, add liveness and recurse into results.
          foundOwnedForwardingUses.push_back(op);
          liveness.updateForUse(user, /*lifetimeEnding*/ false);
          llvm::copy(user->getResults(), std::back_inserter(worklist));
          break;
        }

        // If we do not have a forwarding operand, treat this like a consume.
        liveness.updateForUse(user, /*lifetimeEnding*/ true);
        foundConsumesOfBorrowedSubValues.push_back(user);
        break;
      }
      case OperandOwnership::DestroyingConsume:
        liveness.updateForUse(user, /*lifetimeEnding*/ true);

        if (!isa<DestroyValueInst>(user)) {
          foundConsumesOfBorrowedSubValues.push_back(user);
          break;
        }

        foundDestroysOfBorrowedSubValues.push_back(user);
        break;
      case OperandOwnership::Borrow: {
        auto *bbi = cast<BeginBorrowInst>(user);
        // Only add borrows to liveness if the borrow isn't lexical. If it is
        // a lexical borrow, we have created an entirely new source level
        // binding that should be tracked separately.
        //
        // TODO: Is this still true with the changes?
        if (!bbi->isLexical()) {
          bool failed = !liveness.updateForBorrowingOperand(use);
          // If we fail, we will just bail and not eliminate these copies.
          // Once we have @moveOnly in the SIL type system, this will result
          // in a diagnostic that says we were not able to eliminate a
          // copy. But that is in a later pass.
          if (failed)
            return false;
          foundRecursiveBorrows.push_back(bbi);
          worklist.push_back(bbi);
        }
        break;
      }
      case OperandOwnership::ForwardingBorrow:
        // A forwarding borrow is validated as part of its parent borrow. So
        // just mark it as extending liveness and look through it.
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        assert(OwnershipForwardingMixin::isa(use->getUser()));

        if (auto *termInst =
                dyn_cast<OwnershipForwardingTermInst>(use->getUser())) {
          for (auto argList : termInst->getSuccessorBlockArgumentLists()) {
            worklist.push_back(argList[use->getOperandNumber()]);
          }
          break;
        }

        for (auto result : use->getUser()->getResults()) {
          if (result.getOwnershipKind() == OwnershipKind::Guaranteed)
            worklist.push_back(result);
        }
        break;
      case OperandOwnership::InteriorPointer: {
        // We do not care about interior pointer uses. Just propagate
        // liveness. Should add all address uses as liveness uses.
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        auto ptrOp = InteriorPointerOperand(use);
        assert(ptrOp);
        SmallVector<Operand *, 8> foundUses;
        ptrOp.findTransitiveUses(&foundUses);
        for (auto *op : foundUses) {
          liveness.updateForUse(op->getUser(), /*lfietimeEnding*/ false);
        }
        break;
      }
      case OperandOwnership::EndBorrow:
        foundRecursiveEndBorrow.push_back(user);
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        break;
      case OperandOwnership::Reborrow:
        // Reborrows do not occur this early in the pipeline.
        llvm_unreachable(
            "Reborrows do not occur until we optimize later in the pipeline");
      }
    }
  }

  // Ok, we have finished processing out worklist. If we found any /real/
  // consumes, we are going to error in the caller. So just bail now.
  if (foundConsumesOfBorrowedSubValues.size()) {
    LLVM_DEBUG(
        llvm::dbgs()
        << "Found consume of borrowed subvalues! Will Emit Diagnostic!\n");
    return true;
  }

  // Otherwise, the only reason why we could need a copy_value is if the copy is
  // needed to lifetime extend the sub-object outside of the lifetime of the
  // parent value. We use the liveness information that we gathered above to do
  // this. We gathered two forms of liveness info:
  //
  // 1. liveness: the complete liveness info looking through copies/borrows.
  //
  // 2. markedValueExtendedLiveRangeInfo.liveness: this is the owned liveness
  //    information defined for the marked value looking through owned
  //    forwarding instructions.
  //
  // We need to show that the 2nd is within the boundary of the first.
  //
  // First though if we do not have any liveness at all, then we did not have
  // any uses at all. So we can just return success.
  if (liveness.empty()) {
    LLVM_DEBUG(llvm::dbgs()
               << "No liveness requiring uses! No diagnostic needed!\n");
    return true;
  }

  // If we did not have an extended live range liveness for our value then that
  // means that we must be post-dominated by dead end blocks since otherwise we
  // would be leaking. That is illegal in OSSA, so we know that we do not need
  // any copy_value for lifetime extension since our value must be live until
  // the end of the function.
  if (markedValueExtendedLiveRangeInfo.liveness.empty()) {
    LLVM_DEBUG(llvm::dbgs()
               << "No marked value extended live range info liveness!");
    return true;
  }

  // Then see if our underlying markedValue's consuming uses are all not within
  // the boundary of the uses. If they are within the boundary, then we know
  // that there is some copy_value that is requiring to lifetime extend due to a
  // non-consuming use that is after the lifetime of the value has ended. We
  // need to emit a diagnostic on this. For now, lets just emit it as a
  // consuming use.
  //
  // TODO: Emit a better diagnostic here that talks about the need for lifetime
  // extension.
  SWIFT_DEFER { livenessInfo.livenessBoundary.clear(); };
  livenessInfo.computeBoundary();
  markedValueExtendedLiveRangeInfo.computeBoundary();

  LLVM_DEBUG(llvm::dbgs()
             << "Checking if copy_value needed for lifetime extension!\n");

  for (auto *user : markedValueExtendedLiveRangeInfo.getLastUsers()) {
    if (!liveness.isWithinBoundary(user))
      continue;

    // Ok, we have some use that requires lifetime extension. It is going to be
    // one of the boundary uses of our liveness query. In order to figure out
    // which one it actually is, we see which is reachable from this use. In
    // such a case, we are going to emit a diagnostic so we can use a bit more
    // compile time since we are going to stop optimizing after codegening.
    //
    // TODO: Actually implement this... for now we just error on all boundary
    // uses.
    llvm::copy(livenessInfo.getLastUsers(),
               std::back_inserter(foundConsumesOfBorrowedSubValues));
  }

  // If we are going to emit a diagnostic on a liveness boundary based
  // copy_value, bail early.
  if (foundConsumesOfBorrowedSubValues.size()) {
    LLVM_DEBUG(llvm::dbgs() << "Found copy_value needed for lifetime "
                               "extension! Emitting Diagnostic!\n");
#ifndef NDEBUG
    for (auto *user : livenessInfo.getLastUsers()) {
      LLVM_DEBUG(llvm::dbgs() << "User: " << *user);
    }
#endif
    return true;
  }

  // Otherwise, we have success. There aren't any consumes of the underlying
  // value and all copy_value are balanced by destroy_values (modulo forwarding
  // uses) or we would have emitted a diagnostic.
  //
  // TODO: We should be able to eliminate all copy/destroys that we found above
  // since we proved that all uses are within the lifetime of the base owned
  // value. But this at least gets the correct diagnostic in place for
  // prototyping purposes.
  LLVM_DEBUG(llvm::dbgs() << "No copy needed! No Diagnostic needed\n");
  return true;
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
  /// copy.
  SmallVector<Operand *, 32> consumingUsesNeedingCopy;

  /// A per mark must check, vector of uses that copy propagation says do not
  /// need a copy. This could include lifetime extension.
  ///
  /// TODO: Rename this.
  SmallVector<Operand *, 32> consumingUsesNotNeedingCopy;

  CopyOfBorrowedProjectionChecker copyOfBorrowedProjectionChecker;

  MoveOnlyChecker(SILFunction *fn, DeadEndBlocks *deBlocks)
      : fn(fn), copyOfBorrowedProjectionChecker(deBlocks) {}

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

  while (consumingUsesNotNeedingCopy.size()) {
    auto *consumingUse = consumingUsesNotNeedingCopy.pop_back_val();

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

  auto &foundConsumesOfBorrowedSubValues =
      copyOfBorrowedProjectionChecker.foundConsumesOfBorrowedSubValues;
  while (foundConsumesOfBorrowedSubValues.size()) {
    auto *user = foundConsumesOfBorrowedSubValues.pop_back_val();
    auto loc = user->getLoc();

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
    consumingUsesNotNeedingCopy.push_back(use);
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
      consumingUsesNotNeedingCopy.clear();
      copyOfBorrowedProjectionChecker.clear();
    };

    MarkMustCheckInst *markedValue = moveIntroducers.front();
    moveIntroducers = moveIntroducers.drop_front(1);
    LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *markedValue);

    // First canonicalize ownership.
    changed |= canonicalizer.canonicalizeValueLifetime(markedValue);

    // If we are asked to perform guaranteed checking, emit an error if we have
    // /any/ consuming uses.
    if (markedValue->getCheckKind() == MarkMustCheckInst::CheckKind::NoCopy) {
      if (!consumingUsesNeedingCopy.empty() ||
          !consumingUsesNotNeedingCopy.empty()) {
        emitDiagnostic(markedValue, true /*original value guaranteed*/);
        valuesWithDiagnostics.insert(markedValue);
      }
      continue;
    }

    if (consumingUsesNeedingCopy.empty()) {
      // If we don't see any situations where we need a direct copy, check if we
      // have any copy_value from any user of ours that is a borrow
      // introducer. In such a case, a copy is needed but the user needs to use
      // _copy to explicit copy the value since they are extracting out a
      // subvalue.
      if (!copyOfBorrowedProjectionChecker.check(markedValue) ||
          !copyOfBorrowedProjectionChecker.shouldEmitDiagnostic()) {
        // If we failed to understand how to perform the check or did not find
        // any targets... continue. In the former case we want to fail with a
        // checker did not understand diagnostic later and in the former, we
        // succeeded.
        continue;
      }

      // Otherwise, emit our diagnostic below.
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
