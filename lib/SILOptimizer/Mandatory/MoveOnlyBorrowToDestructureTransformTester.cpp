//===--- MoveOnlyBorrowToDestructureTransform.cpp -------------------------===//
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
/// \file This is a pass that converts the borrow + gep pattern to destructures
/// or emits an error if it cannot be done. It is assumed that it runs
/// immediately before move checking of objects runs. This ensures that the move
/// checker does not need to worry about this problem and instead can just check
/// that the newly inserted destructures do not cause move only errors.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-move-only-checker"

#include "MoveOnlyDiagnostics.h"
#include "MoveOnlyObjectChecker.h"

#include "swift/Basic/Defer.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/Basic/BlotSetVector.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "llvm/ADT/ArrayRef.h"

using namespace swift;
using namespace swift::siloptimizer;

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

static bool runTransform(SILFunction *fn,
                         ArrayRef<MarkMustCheckInst *> moveIntroducersToProcess,
                         PostOrderAnalysis *poa,
                         DiagnosticEmitter &diagnosticEmitter) {
  BorrowToDestructureTransform::IntervalMapAllocator allocator;
  bool madeChange = false;
  while (!moveIntroducersToProcess.empty()) {
    auto *mmci = moveIntroducersToProcess.back();
    moveIntroducersToProcess = moveIntroducersToProcess.drop_back();

    unsigned currentDiagnosticCount = diagnosticEmitter.getDiagnosticCount();

    StackList<BeginBorrowInst *> borrowWorklist(mmci->getFunction());

    // If we failed to gather borrows due to the transform not understanding
    // part of the SIL, emit a diagnostic, RAUW the mark must check, and
    // continue.
    if (!BorrowToDestructureTransform::gatherBorrows(mmci, borrowWorklist)) {
      diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(mmci);
      mmci->replaceAllUsesWith(mmci->getOperand());
      mmci->eraseFromParent();
      madeChange = true;
      continue;
    }

    // If we do not have any borrows to process, continue and process the next
    // instruction.
    if (borrowWorklist.empty())
      continue;

    SmallVector<SILBasicBlock *, 8> discoveredBlocks;

    // Now that we have found all of our borrows, we want to find struct_extract
    // uses of our borrow as well as any operands that cannot use an owned
    // value.
    SWIFT_DEFER { discoveredBlocks.clear(); };
    BorrowToDestructureTransform transform(allocator, mmci, diagnosticEmitter,
                                           poa, discoveredBlocks);

    // Attempt to gather uses. Return if we saw something that we did not
    // understand. Emit a compiler did not understand diagnostic, RAUW the mmci
    // so later passes do not see it, and set madeChange to true.
    if (!transform.gatherUses(borrowWorklist)) {
      diagnosticEmitter.emitCheckerDoesntUnderstandDiagnostic(mmci);
      mmci->replaceAllUsesWith(mmci->getOperand());
      mmci->eraseFromParent();
      madeChange = true;
      continue;
    }

    // Next make sure that any destructure needing instructions are on the
    // boundary in a per bit field sensitive manner.
    transform.checkDestructureUsesOnBoundary();

    // If we emitted any diagnostic, set madeChange to true, eliminate our mmci,
    // and continue.
    if (currentDiagnosticCount != diagnosticEmitter.getDiagnosticCount()) {
      mmci->replaceAllUsesWith(mmci->getOperand());
      mmci->eraseFromParent();
      madeChange = true;
      continue;
    }

    // At this point, we know that all of our destructure requiring uses are on
    // the boundary of our live range. Now we need to do the rewriting.
    transform.blockToAvailableValues.emplace(transform.liveness);
    transform.rewriteUses();

    // Now that we have done our rewritting, we need to do a few cleanups.
    //
    // NOTE: We do not eliminate our mark_must_check since we want later passes
    // to do additional checking upon the mark_must_check including making sure
    // that our destructures do not need cause the need for additional copies to
    // be inserted. We only eliminate the mark_must_check if we emitted a
    // diagnostic of some sort.
    transform.cleanup(borrowWorklist);
    madeChange = true;
  }

  return madeChange;
}

namespace {

class MoveOnlyBorrowToDestructureTransformPass : public SILFunctionTransform {
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

    LLVM_DEBUG(llvm::dbgs()
               << "===> MoveOnlyBorrowToDestructureTransform. Visiting: "
               << fn->getName() << '\n');

    auto *postOrderAnalysis = getAnalysis<PostOrderAnalysis>();

    SmallSetVector<MarkMustCheckInst *, 32> moveIntroducersToProcess;
    DiagnosticEmitter emitter;

    bool madeChange = searchForCandidateObjectMarkMustChecks(
        getFunction(), moveIntroducersToProcess, emitter);
    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

    if (emitter.emittedAnyDiagnostics()) {
      if (cleanupSILAfterEmittingObjectMoveOnlyDiagnostics(fn))
        invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
      return;
    }

    auto introducers = llvm::makeArrayRef(moveIntroducersToProcess.begin(),
                                          moveIntroducersToProcess.end());
    if (runTransform(fn, introducers, postOrderAnalysis, emitter)) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

    if (emitter.emittedAnyDiagnostics()) {
      if (cleanupSILAfterEmittingObjectMoveOnlyDiagnostics(fn))
        invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // namespace

SILTransform *swift::createMoveOnlyBorrowToDestructureTransform() {
  return new MoveOnlyBorrowToDestructureTransformPass();
}
