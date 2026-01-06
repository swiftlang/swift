//===--- DebugInfoCanonicalizer.cpp ---------------------------------------===//
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
///
/// This file contains transformations that propagate debug info at the SIL
/// level to make IRGen's job easier. The specific transformations that we
/// perform is that we clone dominating debug_value for a specific
/// SILDebugVariable after all coroutine-func-let boundary instructions. This in
/// practice this as an algorithm works as follows:
///
/// 1. We walk the CFG along successors. By doing this we guarantee that we
/// visit
///    blocks after their dominators.
///
/// 2. When we visit a block, we walk the block from start->end. During this
/// walk:
///
///    a. We grab a new block state from the centralized block->blockState map.
///    This
///       state is a [SILDebugVariable : DebugValueInst].
///
///    b. If we see a debug_value, we map blockState[debug_value.getDbgVar()] =
///       debug_value. This ensures that when we get to the bottom of the block,
///       we have pairs of SILDebugVariable + last debug_value on it.
///
///    c. If we see any coroutine funclet boundaries, we clone the current
///    tracked
///       set of our block state and then walk up the dom tree dumping in each
///       block any debug_value with a SILDebugVariable that we have not already
///       dumped. This is maintained by using a visited set of SILDebugVariable
///       for each funclet boundary.
///
/// The end result is that at the beginning of each funclet we will basically
/// declare the debug info for an addr.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-onone-debuginfo-canonicalizer"

#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallSet.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static SILInstruction *cloneDebugValue(DebugVarCarryingInst original,
                                       SILInstruction *insertPt) {
  SILBuilderWithScope builder(std::next(insertPt->getIterator()));
  builder.setCurrentDebugScope(original->getDebugScope());
  return builder.createDebugValue(
      original->getLoc(), original.getOperandForDebugValueClone(),
      *original.getVarInfo(), DontPoisonRefs, UsesMoveableValueDebugInfo);
}

static SILInstruction *cloneDebugValue(DebugVarCarryingInst original,
                                       SILBasicBlock *block) {
  SILBuilderWithScope builder(&block->front());
  builder.setCurrentDebugScope(original->getDebugScope());
  return builder.createDebugValue(
      original->getLoc(), original.getOperandForDebugValueClone(),
      *original.getVarInfo(), DontPoisonRefs, UsesMoveableValueDebugInfo);
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

namespace {

struct BlockState {
  llvm::SmallMapVector<SILDebugVariable, DebugVarCarryingInst, 4> debugValues;
};

struct DebugInfoCanonicalizer {
  SILFunction *fn;
  DominanceAnalysis *da;
  DominanceInfo *dt;
  llvm::MapVector<SILBasicBlock *, BlockState> blockToBlockState;

  DebugInfoCanonicalizer(SILFunction *fn, DominanceAnalysis *da)
      : fn(fn), da(da), dt(nullptr) {}

  // We only need the dominance info if we actually see a funclet boundary. So
  // make this lazy so we only create the dom tree in functions that actually
  // use coroutines.
  DominanceInfo *getDominance() {
    if (!dt)
      dt = da->get(fn);
    return dt;
  }

  bool process();

  /// NOTE: insertPt->getParent() may not equal startBlock! This is b/c if we
  /// are propagating from a yield, we want to begin in the yields block, not
  /// the yield's insertion point successor block.
  bool propagateDebugValuesFromDominators(
      PointerUnion<SILInstruction *, SILBasicBlock *> insertPt,
      SILBasicBlock *startBlock,
      llvm::SmallDenseSet<SILDebugVariable, 8> &seenDebugVars) {
    LLVM_DEBUG(llvm::dbgs() << "==> PROPAGATING VALUE\n");
    if (isa<SILInstruction *>(insertPt)) {
      LLVM_DEBUG(llvm::dbgs() << "Inst: " << *cast<SILInstruction *>(insertPt));
    }

    auto *dt = getDominance();
    auto *domTreeNode = dt->getNode(startBlock);
    auto *rootNode = dt->getRootNode();
    if (domTreeNode == rootNode) {
      LLVM_DEBUG(llvm::dbgs() << "Root node! Nothing to propagate!\n");
      return false;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "Root Node: " << rootNode->getBlock()->getDebugID() << '\n');

    // We already emitted in our caller all debug_value needed from the block we
    // were processing. We just need to walk up the dominator tree until we
    // process the root node.
    bool madeChange = false;
    do {
      domTreeNode = domTreeNode->getIDom();
      LLVM_DEBUG(llvm::dbgs() << "Visiting idom: "
                              << domTreeNode->getBlock()->getDebugID() << '\n');
      auto &domBlockState = blockToBlockState[domTreeNode->getBlock()];
      for (auto &pred : domBlockState.debugValues) {
        // If we see a nullptr, we had a SILUndef. Do not clone, but mark this
        // as a debug var we have seen so if it is again defined in previous
        // blocks, we don't clone.
        if (!pred.second) {
          seenDebugVars.insert(pred.first);
          continue;
        }

        LLVM_DEBUG(llvm::dbgs() << "Has DebugValue: " << *pred.second);

        // If we have already inserted something for this debug_value,
        // continue.
        if (!seenDebugVars.insert(pred.first).second) {
          LLVM_DEBUG(llvm::dbgs() << "Already seen this one... skipping!\n");
          continue;
        }

        // Otherwise do the clone.
        LLVM_DEBUG(llvm::dbgs() << "Haven't seen this one... cloning!\n");
        if (auto *inst = insertPt.dyn_cast<SILInstruction *>()) {
          cloneDebugValue(pred.second, inst);
        } else {
          cloneDebugValue(pred.second, cast<SILBasicBlock *>(insertPt));
        }

        madeChange = true;
      }
    } while (domTreeNode != rootNode);

    return madeChange;
  }
};

} // namespace

bool DebugInfoCanonicalizer::process() {
  bool madeChange = false;

  // We walk along successor edges depth first. This guarantees that we will
  // visit any dominator of a specific block before we visit that block since
  // any path to the block along successors by definition of dominators we must
  // go through all such dominators.
  BasicBlockWorklist worklist(&*fn->begin());
  llvm::SmallDenseSet<SILDebugVariable, 8> seenDebugVars;

  while (auto *block = worklist.pop()) {
    LLVM_DEBUG(llvm::dbgs()
               << "BB: Visiting. bb" << block->getDebugID() << '\n');
    auto &state = blockToBlockState[block];

    // Then for each inst in the block...
    for (auto &inst : *block) {
      LLVM_DEBUG(llvm::dbgs() << "    Inst: " << inst);

      // Skip any alloc box inst we see, we do not support them yet.
      if (isa<AllocBoxInst>(&inst))
        continue;

      // If we have a debug_value or alloc_stack that was moved, store state for
      // it. Once the isa check above is removed, this will handle alloc_box as
      // well.
      if (auto dvi = DebugVarCarryingInst(&inst)) {
        if (!dvi.getWasMoved())
          continue;

        LLVM_DEBUG(llvm::dbgs() << "        Found DebugValueInst!\n");
        auto debugInfo = dvi.getVarInfo();
        if (!debugInfo) {
          LLVM_DEBUG(llvm::dbgs() << "        Has no var info?! Skipping!\n");
          continue;
        }
        // Strip things we don't need in the map.
        debugInfo->DIExpr = debugInfo->DIExpr.getFragmentPart();
        debugInfo->Type = {};

        // Otherwise, we may have a new debug_value to track. Try to begin
        // tracking it...
        auto iter = state.debugValues.insert({*debugInfo, dvi});

        // If we already have one, we failed to insert... So update the iter
        // by hand. We track the last instance always.
        if (!iter.second) {
          iter.first->second = dvi;
        }
        LLVM_DEBUG(llvm::dbgs() << "        ==> Updated Map.\n");
        continue;
      }

      // Otherwise, check if we have a coroutine boundary non-terminator
      // instruction. If we do, we just dump the relevant debug_value right
      // afterwards.
      auto shouldHandleNonTermInst = [](SILInstruction *inst) -> bool {
        // This handles begin_apply.
        if (auto fas = FullApplySite::isa(inst)) {
          if (fas.beginsCoroutineEvaluation() || fas.isAsync())
            return true;
        }
        if (isa<HopToExecutorInst>(inst))
          return true;
        if (isa<EndApplyInst>(inst) || isa<AbortApplyInst>(inst))
          return true;
        return false;
      };
      if (shouldHandleNonTermInst(&inst)) {
        LLVM_DEBUG(llvm::dbgs() << "        Found apply edge!.\n");
        // Clone all of the debug_values that we are currently tracking both
        // after the begin_apply,
        SWIFT_DEFER { seenDebugVars.clear(); };

        for (auto &pred : state.debugValues) {
          // If we found a SILUndef, mark this debug var as seen but do not
          // clone.
          if (!pred.second) {
            seenDebugVars.insert(pred.first);
            continue;
          }

          cloneDebugValue(pred.second, &inst);
          // Inside our block, we know that we do not have any repeats since we
          // always track the last debug var.
          seenDebugVars.insert(pred.first);
          madeChange = true;
        }

        // Then walk up the idoms until we reach the entry searching for
        // seenDebugVars.
        madeChange |= propagateDebugValuesFromDominators(
            &inst, inst.getParent(), seenDebugVars);
        continue;
      }

      // Otherwise, we have a yield. We handle this separately since we need to
      // insert the debug_value into its successor blocks.
      if (auto *yi = dyn_cast<YieldInst>(&inst)) {
        LLVM_DEBUG(llvm::dbgs() << "    Found Yield: " << *yi);

        SWIFT_DEFER { seenDebugVars.clear(); };

        // Duplicate all of our tracked debug values into our successor
        // blocks.
        for (auto *succBlock : yi->getSuccessorBlocks()) {
          LLVM_DEBUG(llvm::dbgs() << "        Visiting Succ: bb"
                                  << succBlock->getDebugID() << '\n');
          for (auto &pred : state.debugValues) {
            if (!pred.second)
              continue;
            LLVM_DEBUG(llvm::dbgs() << "            Cloning: " << *pred.second);
            cloneDebugValue(pred.second, succBlock);
            madeChange = true;
          }

          // We start out dataflow in yi, not in inst, even though we use inst
          // as the insert pt. This is b/c inst is in the successor block we
          // haven't processed yet so we would emit any debug_value in the
          // yields own block twice.
          madeChange |= propagateDebugValuesFromDominators(
              succBlock, yi->getParent(), seenDebugVars);
        }
      }
    }

    // Now add the block's successor to the worklist if we haven't visited them
    // yet.
    for (auto *succBlock : block->getSuccessorBlocks())
      worklist.pushIfNotVisited(succBlock);
  }

  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class DebugInfoCanonicalizerTransform : public SILFunctionTransform {
  void run() override {
    DebugInfoCanonicalizer canonicalizer(getFunction(),
                                         getAnalysis<DominanceAnalysis>());
    if (canonicalizer.process()) {
      invalidateAnalysis(
          SILAnalysis::InvalidationKind::BranchesAndInstructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createDebugInfoCanonicalizer() {
  return new DebugInfoCanonicalizerTransform();
}
