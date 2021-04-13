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

#define DEBUG_TYPE "silgen-cleanup"

#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

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

  /// Delete trivially dead instructions in non-determistic order.
  ///
  /// We either have that nextII is endII or if nextII is not endII then endII
  /// is nextII->getParent()->end().
  SILBasicBlock::iterator deleteDeadOperands(SILBasicBlock::iterator nextII,
                                             SILBasicBlock::iterator endII) {
    // Each iteration, we store the instructions that will be deleted in the
    // iteration here and use it to ensure that nextII is moved past /all/
    // instructions that we are going to delete no matter the order (since we
    // are visiting instructions in non-deterministic order).
    SmallPtrSet<SILInstruction *, 16> willBeDeletedInIteration;

    while (!deadOperands.empty()) {
      SILInstruction *deadOperInst = *deadOperands.begin();

      // Make sure at least the first instruction is removed from the set.
      deadOperands.erase(deadOperInst);

      // Then add our initial instruction to the will be deleted set.
      willBeDeletedInIteration.insert(deadOperInst);
      SWIFT_DEFER { willBeDeletedInIteration.clear(); };

      eliminateDeadInstruction(deadOperInst, [&](SILInstruction *deadInst) {
        LLVM_DEBUG(llvm::dbgs() << "Trivially dead: " << *deadInst);

        // Add our instruction to the will be deleted set.
        willBeDeletedInIteration.insert(deadInst);

        // Then look through /all/ instructions that we are going to delete in
        // this iteration until we hit the end of the list. This ensures that in
        // a situation like the following:
        //
        // ```
        //   (%1, %2) = multi_result_inst %0   (a)
        //   inst_to_delete %1                 (b)
        //   inst_to_delete %2                 (c)
        // ```
        //
        // If nextII is on (b), but we visit (c) before visiting (b), then we
        // will end up with nextII on (c) after we are done and then delete
        // (c). In contrast by using the set when we process (b) after (c), we
        // first see that (b) is nextII [since it is in the set] so move to (c)
        // and then see that (c) is in the set as well (since we inserted it
        // previously) and skip that.
        while (nextII != endII && willBeDeletedInIteration.count(&*nextII))
          ++nextII;

        // Then remove the instruction from the set.
        deadOperands.erase(deadInst);
      });
    }
    return nextII;
  }
};

//===----------------------------------------------------------------------===//
// SILGenCleanup: Top-Level Module Transform
//===----------------------------------------------------------------------===//

namespace {

// SILGenCleanup must run on all functions that will be seen by any analysis
// used by diagnostics before transforming the function that requires the
// analysis. e.g. Closures need to be cleaned up before the closure's parent can
// be diagnosed.
//
// TODO: This pass can be converted to a function transform if the mandatory
// pipeline runs in bottom-up closure order.
struct SILGenCleanup : SILModuleTransform {
  void run() override;
};

void SILGenCleanup::run() {
  auto &module = *getModule();
  for (auto &function : module) {
    LLVM_DEBUG(llvm::dbgs()
               << "\nRunning SILGenCleanup on " << function.getName() << "\n");

    DeadEndBlocks deadEndBlocks(&function);
    SILGenCanonicalize sgCanonicalize(deadEndBlocks);

    // Iterate over all blocks even if they aren't reachable. No phi-less
    // dataflow cycles should have been created yet, and these transformations
    // are simple enough they shouldn't be affected by cycles.
    for (auto &bb : function) {
      for (auto ii = bb.begin(), ie = bb.end(); ii != ie;) {
        ii = sgCanonicalize.canonicalize(&*ii);
        ii = sgCanonicalize.deleteDeadOperands(ii, ie);
      }
    }
    if (sgCanonicalize.changed) {
      auto invalidKind = SILAnalysis::InvalidationKind::Instructions;
      invalidateAnalysis(&function, invalidKind);
    }
  }
}

} // end anonymous namespace

SILTransform *swift::createSILGenCleanup() { return new SILGenCleanup(); }
