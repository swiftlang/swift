//===--- SILGenCleanup.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Perform peephole-style "cleanup" to aid SIL diagnostic passes.
///
//===----------------------------------------------------------------------===//

#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILBasicBlock.h"
#define DEBUG_TYPE "silgen-cleanup"

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"

using namespace swift;

// Define a CanonicalizeInstruction subclass for use in SILGenCleanup.
struct SILGenCanonicalize final : CanonicalizeInstruction {
  bool changed = false;
  llvm::SmallPtrSet<SILInstruction *, 16> deadOperands;

  SILGenCanonicalize(DeadEndBlocks &deadEndBlocks)
      : CanonicalizeInstruction(DEBUG_TYPE, deadEndBlocks) {}

  void notifyNewInstruction(SILInstruction *) override { changed = true; }

  // Just delete the given 'inst' and record its operands. The callback isn't
  // allowed to mutate any other instructions.
  void killInstruction(SILInstruction *inst) override {
    deadOperands.erase(inst);
    for (auto &operand : inst->getAllOperands()) {
      if (auto *operInst = operand.get()->getDefiningInstruction())
        deadOperands.insert(operInst);
    }
    inst->eraseFromParent();
    changed = true;
  }

  void notifyHasNewUsers(SILValue) override { changed = true; }

  /// Delete trivially dead instructions in non-deterministic order.
  ///
  /// We either have that nextII is endII or if nextII is not endII then endII
  /// is nextII->getParent()->end().
  SILBasicBlock::iterator deleteDeadOperands(SILBasicBlock::iterator nextII,
                                             SILBasicBlock::iterator endII) {
    auto callbacks = InstModCallbacks().onDelete([&](SILInstruction *deadInst) {
      LLVM_DEBUG(llvm::dbgs() << "Trivially dead: " << *deadInst);

      // If nextII is the instruction we are going to delete, move nextII past
      // it.
      if (deadInst->getIterator() == nextII)
        ++nextII;

      // Then remove the instruction from the set and delete it.
      deadOperands.erase(deadInst);
      deadInst->eraseFromParent();
    });

    while (!deadOperands.empty()) {
      SILInstruction *deadOperInst = *deadOperands.begin();

      // Make sure at least the first instruction is removed from the set.
      deadOperands.erase(deadOperInst);

      // Then delete this instruction/everything else that we can.
      eliminateDeadInstruction(deadOperInst, callbacks);
    }
    return nextII;
  }
};

//===----------------------------------------------------------------------===//
// SILGenCleanup: Per-Function Transform
//===----------------------------------------------------------------------===//

namespace {

// SILGenCleanup performs cleanup of SILGen output before diagnostic passes. The
// pass manager runs function passes in bottom-up call-graph order (via
// BottomUpFunctionOrder), ensuring closures are cleaned up before their parent
// functions.
//
// The needBreakInfiniteLoops / needCompleteLifetimes flags set during SILGen
// are cleared by postEmitFunction, so the pass manager's pre-condition is
// satisfied.
struct SILGenCleanup : SILFunctionTransform {
  void run() override;
};

static void removeEndFormalScopeMarkers(SILFunction &f) {
  // TODO: Skip this when lifetime resolution is enabled, since that pass
  // will make use of these markers, and remove them in the course of its own
  // lowering.
  
  for (SILBasicBlock &block: f) {
    for (auto i = block.begin(), e = block.end(); i != e; ) {
      auto cur = i++;
      if (auto efs = dyn_cast<EndFormalScopeInst>(&*cur)) {
        efs->eraseFromParent();
      }
    }
  }
}

static void clearUnresolvedMarkers(SILFunction &f) {
  // TODO: Skip this when lifetime resolution is enabled, since that pass
  // will potentially reestablish unresolved accesses as needed to respect
  // value lifetimes.
  for (SILBasicBlock &block : f) {
    for (auto i = block.begin(), e = block.end(); i != e; ++i) {
      if (auto bap = dyn_cast<BeginApplyInst>(&*i)) {
        bap->setUnresolved(false);
      }
      if (auto bac = dyn_cast<BeginAccessInst>(&*i)) {
        bac->setUnresolved(false);
      }
    }
  }

}

void SILGenCleanup::run() {
  SILFunction *function = getFunction();
  if (!function->isDefinition())
    return;

  PrettyStackTraceSILFunction stackTrace("silgen cleanup", function);

  LLVM_DEBUG(llvm::dbgs()
             << "\nRunning SILGenCleanup on " << function->getName() << "\n");

  clearUnresolvedMarkers(*function);
  removeEndFormalScopeMarkers(*function);
  removeUnreachableBlocks(*function);
  bool changed = fixupBorrowAccessors(getPassManager(), function);
  breakInfiniteLoops(getPassManager(), function);
  completeAllLifetimes(getPassManager(), function,
                       /*includeTrivialVars=*/true);
  // Release compilers verify ownership once, in OwnershipModelEliminator.
  if (CONDITIONAL_ASSERT_enabled())
    function->verifyOwnership(/*deadEndBlocks=*/nullptr);

  DeadEndBlocks deadEndBlocks(function);
  SILGenCanonicalize sgCanonicalize(deadEndBlocks);

  // Iterate over all blocks even if they aren't reachable. No phi-less
  // dataflow cycles should have been created yet, and these transformations
  // are simple enough they shouldn't be affected by cycles.
  for (auto &bb : *function) {
    for (auto ii = bb.begin(), ie = bb.end(); ii != ie;) {
      ii = sgCanonicalize.canonicalize(&*ii);
      ii = sgCanonicalize.deleteDeadOperands(ii, ie);
    }
  }
  changed |= sgCanonicalize.changed;
  if (changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

} // end anonymous namespace

SILTransform *swift::createSILGenCleanup() { return new SILGenCleanup(); }
