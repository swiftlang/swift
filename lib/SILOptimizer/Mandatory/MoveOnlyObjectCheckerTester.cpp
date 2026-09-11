//===--- MoveOnlyObjectCheckerTester.cpp ----------------------------------===//
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

#define DEBUG_TYPE "sil-move-only-checker"

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/NonLocalAccessBlockAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "llvm/Support/Debug.h"

#include "MoveOnlyBorrowToDestructureUtils.h"
#include "MoveOnlyDiagnostics.h"
#include "MoveOnlyObjectCheckerUtils.h"
#include "MoveOnlyUtils.h"

using namespace swift;
using namespace swift::siloptimizer;

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MoveOnlyObjectCheckerTesterPass : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    assert(fn->getModule().getStage() == SILStage::Raw &&
           "Should only run on Raw SIL");

    LLVM_DEBUG(llvm::dbgs() << "===> MoveOnly Object Checker. Visiting: "
                            << fn->getName() << '\n');

    auto *dominanceAnalysis = getAnalysis<DominanceAnalysis>();
    DominanceInfo *domTree = dominanceAnalysis->get(fn);
    auto *poa = getAnalysis<PostOrderAnalysis>();
    auto *deba = getAnalysis<DeadEndBlocksAnalysis>();

    DiagnosticEmitter diagnosticEmitter(fn);
    borrowtodestructure::IntervalMapAllocator allocator;

    unsigned diagCount = diagnosticEmitter.getDiagnosticCount();
    llvm::SmallSetVector<MarkUnresolvedNonCopyableValueInst *, 32>
        moveIntroducersToProcess;
    bool madeChange =
        searchForCandidateObjectMarkUnresolvedNonCopyableValueInsts(
            fn, moveIntroducersToProcess, diagnosticEmitter);

    LLVM_DEBUG(llvm::dbgs()
               << "Emitting diagnostic when checking for mark must check inst: "
               << (diagCount != diagnosticEmitter.getDiagnosticCount() ? "yes"
                                                                       : "no")
               << '\n');

    if (moveIntroducersToProcess.empty()) {
      LLVM_DEBUG(llvm::dbgs()
                 << "No move introducers found?! Returning early?!\n");
    } else {
      diagCount = diagnosticEmitter.getDiagnosticCount();
      MoveOnlyObjectChecker checker{diagnosticEmitter, domTree, deba, poa,
                                    allocator};
      madeChange |= checker.check(moveIntroducersToProcess);
    }

    if (diagCount != diagnosticEmitter.getDiagnosticCount()) {
      emitCheckerMissedCopyOfNonCopyableTypeErrors(getFunction(),
                                                   diagnosticEmitter);
    }

    madeChange |=
        cleanupNonCopyableCopiesAfterEmittingDiagnostic(getFunction());

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveOnlyObjectChecker() {
  return new MoveOnlyObjectCheckerTesterPass();
}
