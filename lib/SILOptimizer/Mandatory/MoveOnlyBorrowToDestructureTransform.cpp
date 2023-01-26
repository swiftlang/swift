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
/// \file This is a utility pass that simulates how the move only object/address
/// checker run the borrow to destructure transform. It is intended to be used
/// only for testing purposes.
///
/// TODO: Once the BorrowToDestructureTransform moves to ./SILOptimizer/Utils,
/// this should move to ./SILOptimizer/UtilityPasses
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-move-only-checker"

#include "MoveOnlyDiagnostics.h"
#include "MoveOnlyObjectChecker.h"

#include "swift/Basic/Defer.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/ArrayRef.h"

using namespace swift;
using namespace swift::siloptimizer;

static bool runTester(SILFunction *fn,
                      ArrayRef<MarkMustCheckInst *> moveIntroducersToProcess,
                      PostOrderAnalysis *poa,
                      DiagnosticEmitter &diagnosticEmitter) {
  BorrowToDestructureTransform::IntervalMapAllocator allocator;
  bool madeChange = false;
  while (!moveIntroducersToProcess.empty()) {
    auto *mmci = moveIntroducersToProcess.back();
    moveIntroducersToProcess = moveIntroducersToProcess.drop_back();

    StackList<BeginBorrowInst *> borrowWorklist(mmci->getFunction());

    // If we failed to gather borrows due to the transform not understanding
    // part of the SIL, fail and return false.
    if (!BorrowToDestructureTransform::gatherBorrows(mmci, borrowWorklist))
      return madeChange;

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

    // Attempt to gather uses. Return false if we saw something that we did not
    // understand.
    if (!transform.gatherUses(borrowWorklist))
      return madeChange;

    // Next make sure that any destructure needing instructions are on the
    // boundary in a per bit field sensitive manner.
    transform.checkDestructureUsesOnBoundary();

    // If we emitted any diagnostic, break out. We return true since we actually
    // succeeded in our processing by finding the error. We only return false if
    // we want to tell the rest of the checker that there was an internal
    // compiler error that we need to emit a "compiler doesn't understand
    // error".
    if (diagnosticEmitter.emittedAnyDiagnostics())
      return madeChange;

    // At this point, we know that all of our destructure requiring uses are on
    // the boundary of our live range. Now we need to do the rewriting.
    transform.blockToAvailableValues.emplace(transform.liveness);
    transform.rewriteUses();

    // Now that we have done our rewritting, we need to do a few cleanups.
    transform.cleanup(borrowWorklist);
  }

  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

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

    LLVM_DEBUG(llvm::dbgs() << "===> MoveOnly Object Checker. Visiting: "
                            << fn->getName() << '\n');

    auto *postOrderAnalysis = getAnalysis<PostOrderAnalysis>();

    SmallSetVector<MarkMustCheckInst *, 32> moveIntroducersToProcess;
    DiagnosticEmitter emitter;

    bool madeChange = searchForCandidateObjectMarkMustChecks(
        getFunction(), moveIntroducersToProcess, emitter);
    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

    if (emitter.emittedAnyDiagnostics())
      return;

    auto introducers = llvm::makeArrayRef(moveIntroducersToProcess.begin(),
                                          moveIntroducersToProcess.end());
    if (runTester(fn, introducers, postOrderAnalysis, emitter)) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // namespace

SILTransform *swift::createMoveOnlyBorrowToDestructureTransform() {
  return new MoveOnlyBorrowToDestructureTransformPass();
}
