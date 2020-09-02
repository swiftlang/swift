//===--- SemanticARCOpts.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-semantic-arc-opts"

#include "swift/Basic/LLVM.h"

#include "OwnershipLiveRange.h"
#include "OwnershipPhiOperand.h"
#include "SemanticARCOptVisitor.h"

#include "swift/Basic/BlotSetVector.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/MultiMapCache.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::semanticarc;

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

bool swift::semanticarc::VerifyAfterTransform;

static llvm::cl::opt<bool, true> VerifyAfterTransformOption(
    "sil-semantic-arc-opts-verify-after-transform", llvm::cl::Hidden,
    llvm::cl::location(VerifyAfterTransform), llvm::cl::init(false));

bool SemanticARCOptVisitor::optimize() {
  bool madeChange = false;

  // First process the worklist until we reach a fixed point.
  madeChange |= processWorklist();

  {
    // If we made a change, set that we assume we are at fixed point and then
    // re-run the worklist so that we can
    // properly seeded the ARC peephole map.
    assumingAtFixedPoint = true;
    SWIFT_DEFER { assumingAtFixedPoint = false; };

    // Add everything in visitedSinceLastMutation to the worklist so we
    // recompute our fixed point.
    drainVisitedSinceLastMutationIntoWorklist();

    // Then re-run the worklist. We shouldn't modify anything since we are at a
    // fixed point and are just using this to seed the
    // joinedOwnedIntroducerToConsumedOperands after we have finished changing
    // things. If we did change something, we did something weird, so assert!
    bool madeAdditionalChanges = processWorklist();
    (void)madeAdditionalChanges;
    assert(!madeAdditionalChanges && "Should be at the fixed point");
  }

  // Then use the newly seeded peephole map to
  madeChange |= performPostPeepholeOwnedArgElimination();

  return madeChange;
}

bool SemanticARCOptVisitor::processWorklist() {
  // NOTE: The madeChange here is not strictly necessary since we only have
  // items added to the worklist today if we have already made /some/ sort of
  // change. That being said, I think there is a low cost to including this here
  // and makes the algorithm more correct, visually and in the face of potential
  // refactoring.
  bool madeChange = false;

  while (!worklist.empty()) {
    // Pop the last element off the list. If we were returned None, we blotted
    // this element, so skip it.
    SILValue next = worklist.pop_back_val().getValueOr(SILValue());
    if (!next)
      continue;

    // First check if this is a value that we have visited since the last time
    // we erased an instruction. If we have visited it, skip it. Every time we
    // modify something, we should be deleting an instruction, so we have not
    // found any further information.
    if (!visitedSinceLastMutation.insert(next).second) {
      continue;
    }

    // First check if this is an instruction that is trivially dead. This can
    // occur if we eliminate rr traffic resulting in dead projections and the
    // like.
    //
    // If we delete, we first add all of our deleted instructions operands to
    // the worklist and then remove all results (since we are going to delete
    // the instruction).
    if (auto *defInst = next->getDefiningInstruction()) {
      if (isInstructionTriviallyDead(defInst)) {
        assert(!assumingAtFixedPoint &&
               "Assumed was at fixed point and recomputing state?!");
        deleteAllDebugUses(defInst);
        eraseInstruction(defInst);
        madeChange = true;
        if (VerifyAfterTransform) {
          F.verify();
        }
        continue;
      }
    }

    // Otherwise, if we have a single value instruction (to be expanded later
    // perhaps), try to visit that value recursively.
    if (auto *svi = dyn_cast<SingleValueInstruction>(next)) {
      bool madeSingleChange = visit(svi);
      assert((!madeSingleChange || !assumingAtFixedPoint) &&
             "Assumed was at fixed point and modified state?!");
      madeChange |= madeSingleChange;
      if (VerifyAfterTransform && madeSingleChange) {
        F.verify();
      }
      continue;
    }
  }

  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

// Even though this is a mandatory pass, it is rerun after deserialization in
// case DiagnosticConstantPropagation exposed anything new in this assert
// configuration.
struct SemanticARCOpts : SILFunctionTransform {
  bool guaranteedOptsOnly;

  SemanticARCOpts(bool guaranteedOptsOnly)
      : guaranteedOptsOnly(guaranteedOptsOnly) {}

  void run() override {
    SILFunction &f = *getFunction();

    // Return early if we are not performing OSSA optimizations.
    if (!f.getModule().getOptions().EnableOSSAOptimizations)
      return;

    // Make sure we are running with ownership verification enabled.
    assert(f.getModule().getOptions().VerifySILOwnership &&
           "Can not perform semantic arc optimization unless ownership "
           "verification is enabled");

    SemanticARCOptVisitor visitor(f, guaranteedOptsOnly);

    // Add all the results of all instructions that we want to visit to the
    // worklist.
    for (auto &block : f) {
      for (auto &inst : block) {
        if (SemanticARCOptVisitor::shouldVisitInst(&inst)) {
          for (SILValue v : inst.getResults()) {
            visitor.worklist.insert(v);
          }
        }
      }
    }

    // Then process the worklist. We only destroy instructions, so invalidate
    // that. Once we modify the ownership of block arguments, we will need to
    // perhaps invalidate branches as well.
    if (visitor.optimize()) {
      invalidateAnalysis(
          SILAnalysis::InvalidationKind::BranchesAndInstructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() {
  return new SemanticARCOpts(false /*guaranteed*/);
}

SILTransform *swift::createGuaranteedARCOpts() {
  return new SemanticARCOpts(true /*guaranteed*/);
}
