//===--- MoveKillsCopyableValuesChecker.cpp -------------------------------===//
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

#define DEBUG_TYPE "sil-move-kills-copyable-values-checker"

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/CanonicalOSSALifetime.h"

using namespace swift;

static llvm::cl::opt<bool>
    DisableUnhandledMoveDiagnostic("sil-disable-unknown-move-diagnostic");

//===----------------------------------------------------------------------===//
//                            Diagnostic Utilities
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
//                             Canonical Liveness
//===----------------------------------------------------------------------===//

namespace {

struct CheckerLivenessInfo {
  GraphNodeWorklist<SILValue, 8> defUseWorklist;
  SmallSetVector<Operand *, 8> consumingUse;
  SmallSetVector<SILInstruction *, 8> nonLifetimeEndingUsesInLiveOut;
  SmallVector<Operand *, 8> interiorPointerTransitiveUses;
  PrunedLiveness liveness;

  CheckerLivenessInfo()
      : nonLifetimeEndingUsesInLiveOut(),
        liveness(nullptr, &nonLifetimeEndingUsesInLiveOut) {}

  void initDef(SILValue def) { defUseWorklist.insert(def); }

  /// Compute the liveness for any value currently in the defUseWorklist.
  ///
  /// Returns false if we found any escapes. Returns true if no escape uses were
  /// found. NOTE: Even if we return false, we still visit all uses and compute
  /// liveness normally. We may just be missing uses through the escaping use.
  bool compute();

  void clear() {
    defUseWorklist.clear();
    liveness.clear();
    consumingUse.clear();
    interiorPointerTransitiveUses.clear();
    nonLifetimeEndingUsesInLiveOut.clear();
  }
};

} // end anonymous namespace

bool CheckerLivenessInfo::compute() {
  LLVM_DEBUG(llvm::dbgs() << "LivenessVisitor Begin!\n");
  while (SILValue value = defUseWorklist.pop()) {
    LLVM_DEBUG(llvm::dbgs() << "New Value: " << value);
    SWIFT_DEFER { LLVM_DEBUG(llvm::dbgs() << "Finished Value: " << value); };

    for (Operand *use : value->getUses()) {
      auto *user = use->getUser();
      LLVM_DEBUG(llvm::dbgs() << "    User: " << *user);
      // Recurse through copies.
      if (auto *copy = dyn_cast<CopyValueInst>(user)) {
        LLVM_DEBUG(llvm::dbgs() << "    Copy Value. Looking through it\n");
        defUseWorklist.insert(copy);
        continue;
      }

      LLVM_DEBUG(llvm::dbgs() << "    OperandOwnership: "
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
        // This is an escape but it is up to the user to handle this, move
        // checking stops here.
        break;
      case OperandOwnership::InstantaneousUse:
      case OperandOwnership::UnownedInstantaneousUse:
      case OperandOwnership::BitwiseEscape:
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        break;
      case OperandOwnership::ForwardingConsume:
        consumingUse.insert(use);
        liveness.updateForUse(user, /*lifetimeEnding*/ true);
        break;
      case OperandOwnership::DestroyingConsume:
        // destroy_value does not force pruned liveness (but store etc. does).
        if (!isa<DestroyValueInst>(user)) {
          liveness.updateForUse(user, /*lifetimeEnding*/ true);
        }
        consumingUse.insert(use);
        break;
      case OperandOwnership::Borrow: {
        if (auto *bbi = dyn_cast<BeginBorrowInst>(user)) {
          // Only add borrows to liveness if the borrow isn't lexical. If it is
          // a lexical borrow, we have created an entirely new source level
          // binding that should be tracked separately.
          if (!bbi->isLexical()) {
            bool failed = !liveness.updateForBorrowingOperand(use);
            if (failed)
              return false;
          }
        }
        break;
      }
      case OperandOwnership::ForwardingBorrow:
        // A forwarding borrow is validated as part of its parent borrow. So
        // just mark it as extending liveness and look through it.
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        for (SILValue result : user->getResults()) {
          if (result.getOwnershipKind() == OwnershipKind::Guaranteed)
            defUseWorklist.insert(result);
        }
        break;
      case OperandOwnership::InteriorPointer: {
        // An interior pointer user extends liveness until the end of the
        // interior pointer section.
        //
        // TODO: We really should have all OperandOwnership::InteriorPointer
        // instructions be valid to pass to InteriorPointerOperand. Some
        // builtins do not do it today and it is probably a misuse of the
        // system. That being said, lets do our best here.
        if (auto operand = InteriorPointerOperand(use)) {
          auto addrUseKind =
              operand.findTransitiveUses(&interiorPointerTransitiveUses);
          (void)addrUseKind;
          while (!interiorPointerTransitiveUses.empty()) {
            auto *addrUse = interiorPointerTransitiveUses.pop_back_val();
            liveness.updateForUse(addrUse->getUser(), /*lifetimeEnding*/ false);
          }
        }
        break;
      }
      case OperandOwnership::EndBorrow:
        // Don't care about this use.
        break;
      case OperandOwnership::Reborrow:
        // Reborrows do not occur this early in the pipeline.
        llvm_unreachable(
            "Reborrows do not occur until we optimize later in the pipeline");
      }
    }
  }

  // We succeeded if we reached this point since we handled all uses.
  return true;
}

//===----------------------------------------------------------------------===//
//                                 Main Pass
//===----------------------------------------------------------------------===//

namespace {

struct MoveKillsCopyableValuesChecker {
  SILFunction *fn;
  CheckerLivenessInfo livenessInfo;
  DominanceInfo *dominanceToUpdate;
  SILLoopInfo *loopInfoToUpdate;

  MoveKillsCopyableValuesChecker(SILFunction *fn)
      : fn(fn), livenessInfo(), dominanceToUpdate(nullptr),
        loopInfoToUpdate(nullptr) {}

  void setDominanceToUpdate(DominanceInfo *newDFI) {
    dominanceToUpdate = newDFI;
  }

  void setLoopInfoToUpdate(SILLoopInfo *newLFI) { loopInfoToUpdate = newLFI; }

  bool check();

  void emitDiagnosticForMove(SILValue borrowedValue,
                             StringRef borrowedValueName, MoveValueInst *mvi);
};

} // namespace

static SourceLoc getSourceLocFromValue(SILValue value) {
  if (auto *defInst = value->getDefiningInstruction())
    return defInst->getLoc().getSourceLoc();
  if (auto *arg = dyn_cast<SILFunctionArgument>(value))
    return arg->getDecl()->getLoc();
  llvm_unreachable("Do not know how to get source loc for value?!");
}

void MoveKillsCopyableValuesChecker::emitDiagnosticForMove(
    SILValue borrowedValue, StringRef borrowedValueName, MoveValueInst *mvi) {
  auto &astContext = fn->getASTContext();

  // First we emit the main error and then the note on where the move was.
  diagnose(astContext, getSourceLocFromValue(borrowedValue),
           diag::sil_movekillscopyablevalue_value_consumed_more_than_once,
           borrowedValueName);
  diagnose(astContext, mvi->getLoc().getSourceLoc(),
           diag::sil_movekillscopyablevalue_move_here);

  // Then we do a bit of work to figure out where /all/ of the later uses than
  // mvi are so we can emit notes to the user telling them this is a problem
  // use. We can do a little more work here since we are going to be emitting a
  // fatalError ending the program.
  auto *mviBlock = mvi->getParent();
  auto mviBlockLiveness = livenessInfo.liveness.getBlockLiveness(mviBlock);
  switch (mviBlockLiveness) {
  case PrunedLiveBlocks::Dead:
    llvm_unreachable("We should never see this");
  case PrunedLiveBlocks::LiveWithin: {
    // The boundary was within our block. We need to search for uses later than
    // us and emit a diagnostic upon them. Then we return. We leave the rest of
    // the function for the implementation of the LiveOutCase.
    for (SILInstruction &inst :
         make_range(std::next(mvi->getIterator()), mviBlock->end())) {
      switch (livenessInfo.liveness.isInterestingUser(&inst)) {
      case PrunedLiveness::NonUser:
        break;
      case PrunedLiveness::NonLifetimeEndingUse:
      case PrunedLiveness::LifetimeEndingUse:
        diagnose(astContext, inst.getLoc().getSourceLoc(),
                 diag::sil_movekillscopyablevalue_use_here);
        break;
      }
    }
    return;
  }
  case PrunedLiveBlocks::LiveOut: {
    // The boundary was later than us, we need to do a full on CFG search, which
    // we do below.
    break;
  }
  }

  // Just to check for dumb mistakes, assert we are LiveOut here.
  assert(mviBlockLiveness == PrunedLiveBlocks::LiveOut &&
         "We are handling only the live out case here. The rest of the cases "
         "were handled in the switch above and return early upon success");

  BasicBlockWorklist worklist(mvi->getFunction());
  for (auto *succBlock : mvi->getParent()->getSuccessorBlocks()) {
    worklist.pushIfNotVisited(succBlock);
  }

  // In order to make sure that we do not miss uses that are within loops, we
  // maintain a list of all user sets. The issue is that a block at a deeper
  // loop level than our def, even if it contained the use that triggered the
  // issue will be LiveOut. So when we see a live out block, we perform this
  // extra check and emit a diagnostic if needed.
  BasicBlockSet usesToCheckForInLiveOutBlocks(mvi->getFunction());
  for (auto *user : livenessInfo.nonLifetimeEndingUsesInLiveOut)
    usesToCheckForInLiveOutBlocks.insert(user->getParent());
  for (auto *consumingUse : livenessInfo.consumingUse)
    usesToCheckForInLiveOutBlocks.insert(consumingUse->getParentBlock());

  while (auto *block = worklist.pop()) {
    if (PrunedLiveBlocks::LiveOut ==
        livenessInfo.liveness.getBlockLiveness(block)) {
      // Make sure that if we have a liveout block that is at a lower level in
      // the loop nest than our def and we have a use in that block, that we
      // emit an error. We know it is after the move since we are visiting
      // instructions in successors of move.
      if (usesToCheckForInLiveOutBlocks.contains(block)) {
        for (SILInstruction &inst : *block) {
          if (livenessInfo.nonLifetimeEndingUsesInLiveOut.contains(&inst)) {
            diagnose(astContext, inst.getLoc().getSourceLoc(),
                     diag::sil_movekillscopyablevalue_use_here);
            continue;
          }
          for (auto &op : inst.getAllOperands()) {
            if (livenessInfo.consumingUse.contains(&op)) {
              // If one of our in loop moves is ourselves, then we know that our
              // original value is outside of the loop and thus we have a loop
              // carry dataflow violation.
              if (mvi == &inst) {
                diagnose(
                    astContext, inst.getLoc().getSourceLoc(),
                    diag::sil_movekillscopyablevalue_value_consumed_in_loop);
                continue;
              }

              diagnose(astContext, inst.getLoc().getSourceLoc(),
                       diag::sil_movekillscopyablevalue_use_here);
              continue;
            }
          }
        }
      }

      for (auto *succBlock : block->getSuccessorBlocks()) {
        worklist.pushIfNotVisited(succBlock);
      }
      continue;
    }

    // The boundary was within the block. We need to search for interesting uses
    // in the block and then emit diagnostics upon them.
    for (SILInstruction &inst : *block) {
      switch (livenessInfo.liveness.isInterestingUser(&inst)) {
      case PrunedLiveness::NonUser:
        break;
      case PrunedLiveness::NonLifetimeEndingUse:
      case PrunedLiveness::LifetimeEndingUse:
        diagnose(astContext, inst.getLoc().getSourceLoc(),
                 diag::sil_movekillscopyablevalue_use_here);
        break;
      }
    }
  }
}

bool MoveKillsCopyableValuesChecker::check() {
  SmallSetVector<SILValue, 32> valuesToCheck;

  for (auto *arg : fn->getEntryBlock()->getSILFunctionArguments()) {
    if (arg->getOwnershipKind() == OwnershipKind::Owned)
      valuesToCheck.insert(arg);
  }

  for (auto &block : *fn) {
    for (auto &ii : block) {
      if (auto *bbi = dyn_cast<BeginBorrowInst>(&ii)) {
        if (bbi->isLexical())
          valuesToCheck.insert(bbi);
        continue;
      }
    }
  }

  if (valuesToCheck.empty())
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Visiting Function: " << fn->getName() << "\n");
  auto valuesToProcess =
      llvm::makeArrayRef(valuesToCheck.begin(), valuesToCheck.end());
  auto &mod = fn->getModule();

  // If we do not emit any diagnostics, we need to put in a break after each dbg
  // info carrying inst for a lexical value that we find a move on. This ensures
  // that we avoid a behavior today in SelectionDAG that causes dbg info addr to
  // be always sunk to the end of a block.
  //
  // TODO: We should add llvm.dbg.addr support for fastisel and also teach
  // CodeGen how to handle llvm.dbg.addr better.
  while (!valuesToProcess.empty()) {
    auto lexicalValue = valuesToProcess.front();
    valuesToProcess = valuesToProcess.drop_front(1);
    LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *lexicalValue);

    // Then compute liveness.
    SWIFT_DEFER { livenessInfo.clear(); };
    livenessInfo.initDef(lexicalValue);

    // We only fail to optimize if for some reason we hit reborrows. This is
    // temporary since we really should just ban reborrows in Raw SIL.
    bool canOptimize = livenessInfo.compute();
    if (!canOptimize)
      continue;

    // Then look at all of our found consuming uses. See if any of these are
    // _move that are within the boundary.
    bool foundMove = false;
    auto dbgVarInst = DebugVarCarryingInst::getFromValue(lexicalValue);
    StringRef varName = DebugVarCarryingInst::getName(dbgVarInst);
    for (auto *use : livenessInfo.consumingUse) {
      if (auto *mvi = dyn_cast<MoveValueInst>(use->getUser())) {
        // Only emit diagnostics if our move value allows us to.
        if (!mvi->getAllowDiagnostics())
          continue;

        // Now that we know we may emit diagnostics for this, unset allows
        // diagnostics so that we skip these when we search at the end for
        // unvisited move_value [allows_diagnostics].
        mvi->setAllowsDiagnostics(false);

        LLVM_DEBUG(llvm::dbgs() << "Move Value: " << *mvi);
        if (livenessInfo.liveness.isWithinBoundary(mvi)) {
          LLVM_DEBUG(llvm::dbgs() << "    WithinBoundary: Yes!\n");
          emitDiagnosticForMove(lexicalValue, varName, mvi);
        } else {
          LLVM_DEBUG(llvm::dbgs() << "    WithinBoundary: No!\n");
          if (auto varInfo = dbgVarInst.getVarInfo()) {
            auto *next = mvi->getNextInstruction();
            SILBuilderWithScope builder(next);
            // We need to make sure any undefs we put in are the same loc/debug
            // scope as our original so that the backend treats them as
            // referring to the same "debug entity".
            builder.setCurrentDebugScope(dbgVarInst->getDebugScope());
            builder.createDebugValue(
                dbgVarInst->getLoc(),
                SILUndef::get(mvi->getOperand()->getType(), mod), *varInfo,
                false /*poison*/, true /*moved*/);
          }
        }
        foundMove = true;
      }
    }

    // If we found a move, mark our debug var inst as having a moved value. This
    // ensures we emit llvm.dbg.addr instead of llvm.dbg.declare in IRGen.
    if (foundMove) {
      dbgVarInst.markAsMoved();
    }
  }

  return false;
}

//===----------------------------------------------------------------------===//
//                        Unsupported Use Case Errors
//===----------------------------------------------------------------------===//

static void emitUnsupportedUseCaseError(MoveValueInst *mvi) {
  auto &astContext = mvi->getModule().getASTContext();
  auto diag = diag::
    sil_movekillscopyablevalue_move_applied_to_unsupported_move;
  diagnose(astContext, mvi->getLoc().getSourceLoc(), diag);
  mvi->setAllowsDiagnostics(false);
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MoveKillsCopyableValuesCheckerPass : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    assert(fn->getModule().getStage() == SILStage::Raw &&
           "Should only run on Raw SIL");

    MoveKillsCopyableValuesChecker checker(getFunction());

    // If we already had dominance or loop info generated, update them when
    // splitting blocks.
    auto *dominanceAnalysis = getAnalysis<DominanceAnalysis>();
    if (dominanceAnalysis->hasFunctionInfo(fn))
      checker.setDominanceToUpdate(dominanceAnalysis->get(fn));
    auto *loopAnalysis = getAnalysis<SILLoopAnalysis>();
    if (loopAnalysis->hasFunctionInfo(fn))
      checker.setLoopInfoToUpdate(loopAnalysis->get(fn));

    if (checker.check()) {
      AnalysisPreserver preserveDominance(dominanceAnalysis);
      AnalysisPreserver preserveLoop(loopAnalysis);
      invalidateAnalysis(
          SILAnalysis::InvalidationKind::BranchesAndInstructions);
    }

    // Now search through our function one last time and any move_value
    // [allows_diagnostics] that remain are ones that we did not know how to
    // check so emit a diagnostic so the user doesn't assume that they have
    // guarantees.
    //
    // TODO: Emit specific diagnostics here (e.x.: _move of global).
    if (DisableUnhandledMoveDiagnostic)
      return;
    for (auto &block : *fn) {
      for (auto &inst : block) {
        if (auto *mvi = dyn_cast<MoveValueInst>(&inst)) {
          if (mvi->getAllowDiagnostics()) {
            emitUnsupportedUseCaseError(mvi);
          }
        }
      }
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveKillsCopyableValuesChecker() {
  return new MoveKillsCopyableValuesCheckerPass();
}
