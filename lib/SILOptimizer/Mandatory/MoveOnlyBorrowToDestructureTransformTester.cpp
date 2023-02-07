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
using namespace swift::siloptimizer::borrowtodestructure;

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

static bool runTransform(SILFunction *fn,
                         ArrayRef<MarkMustCheckInst *> moveIntroducersToProcess,
                         PostOrderAnalysis *poa,
                         DiagnosticEmitter &diagnosticEmitter) {
  IntervalMapAllocator allocator;
  bool madeChange = false;
  while (!moveIntroducersToProcess.empty()) {
    auto *mmci = moveIntroducersToProcess.back();
    moveIntroducersToProcess = moveIntroducersToProcess.drop_back();

    BorrowToDestructureTransform transform(allocator, mmci, diagnosticEmitter,
                                           poa);
    transform.transform();
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
