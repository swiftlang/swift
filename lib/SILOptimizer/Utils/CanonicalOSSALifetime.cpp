//===--- CanonicalOSSALifetime.cpp - Canonicalize OSSA value lifetimes ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This top-level API rewrites the extended lifetime of a SILValue:
///
///     bool canonicalizeValueLifetime(SILValue def, CanonicalOSSALifetime&)
///
/// Each time it's called on a single OSSA value, `def`, it performs three
/// steps:
///
/// 1. Compute "pruned" liveness of def and its copies, ignoring original
///    destroys.
///
/// 2. Find `def`s final destroy points based on its pruned liveness.
///
/// 3. Rewrite `def`s original copies and destroys, inserting new copies
/// where needed.
///
/// The state used by this transformation is encapsulated in
/// CanonicalOSSALifetime, simply refered to as `lifetime`:
///
/// - Step 1 initializes `lifetime.liveBlocks`.
///
/// - Step 2 initializes `lifetime.consumes` and inserts new destroy_value
///   instructions.
///
/// - Step 3 deletes original copies and destroys and inserts new copies.
///
/// Example #1: Handle consuming and nonconsuming uses.
///
///     bb0(%arg : @owned $T, %addr : @trivial $*T):
///       %copy = copy_value %arg : $T
///       debug_value %copy : $T
///       store %copy to [init] %addr : $*T
///       debug_value %arg : $T
///       debug_value_addr %addr : $*T
///       destroy_value %arg : $T
///
/// Will be transformed to:
///
///     bb0(%arg : @owned $T, %addr : @trivial $*T):
///       // The original copy is deleted.
///       debug_value %arg : $T
///       // A new copy_value is inserted before the consuming store.
///       %copy = copy_value %arg : $T
///       store %copy to [init] %addr : $*T
///       // The non-consuming use now uses the original value.
///       debug_value %arg : $T
///       // A new destroy is inserted after the last use.
///       destroy_value %arg : $T
///       debug_value_addr %addr : $*T
///       // The original destroy is deleted.
///
/// Example #2: Handle control flow.
///
///     bb0(%arg : @owned $T):
///       cond_br %_, bb1, bb2
///     bb1:
///       br bb3
///     bb2:
///       debug_value %arg : $T
///       %copy = copy_value %arg : $T
///       destroy_value %copy : $T
///       br bb3
///     bb3:
///       destroy_value %arg : $T
///
/// Will be transformed to:
///
///     bb0(%arg : @owned $T):
///       cond_br %_, bb1, bb2
///     bb1:
///       destroy_value %arg : $T
///       br bb3
///     bb2:
///       // The original copy is deleted.
///       debug_value %arg : $T
///       destroy_value %arg : $T
///       br bb3
///     bb2:
///       // The original destroy is deleted.
///
///
/// TODO: Guaranteed Values...
///
/// CanonicalOSSALifetime is currently limited to owned values, but will be
/// extended canonicalize guaranteed values. To handle copies of guaranteed
/// values, pruned liveness will first be computed for the guaranteed
/// scope. Then a second pruned liveness will be computed for all the uses
/// exactly as done for owned values. If any uses are outside the scope, then a
/// single copy at the beginning of that scope will provide an owned value to
/// all those uses. Destroy placement will then proceed exactly as it does for
/// owned values. Finally, copy rewriting will proceed just as it does for owned
/// values, inserting a copy for any consuming use within the lifetime. The only
/// difference is that uses within the borrow scope will use the borrowed value,
/// and uses outside will use the copy of the borrow.
///
/// TODO: If all client passes can maintain block numbers, then the
/// SmallDenseMaps/SetVectors can be replaced with bitsets/sparsesets.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "copy-propagation"

#include "swift/SILOptimizer/Utils/CanonicalOSSALifetime.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;
using llvm::SmallSetVector;

STATISTIC(NumCopiesEliminated, "number of copy_value instructions removed");
STATISTIC(NumDestroysEliminated,
          "number of destroy_value instructions removed");
STATISTIC(NumCopiesGenerated, "number of copy_value instructions created");
STATISTIC(NumDestroysGenerated, "number of destroy_value instructions created");
STATISTIC(NumUnknownUsers, "number of functions with unknown users");

//===----------------------------------------------------------------------===//
//                    MARK: Step 1. Compute pruned liveness
//===----------------------------------------------------------------------===//

static bool computeCanonicalLiveness(CanonicalOSSALifetime &lifetime) {
  SmallSetVector<SILValue, 8> defUseWorkList;
  defUseWorkList.insert(lifetime.def());
  while (!defUseWorkList.empty()) {
    SILValue value = defUseWorkList.pop_back_val();
    for (Operand *use : value->getUses()) {
      auto *user = use->getUser();

      // Recurse through copies.
      if (auto *copy = dyn_cast<CopyValueInst>(user)) {
        defUseWorkList.insert(copy);
        continue;
      }
      switch (use->getOperandOwnership()) {
      case OperandOwnership::NonUse:
        continue;
      case OperandOwnership::TrivialUse:
      case OperandOwnership::ForwardingUnowned:
        llvm_unreachable("this operand cannot handle ownership");
      case OperandOwnership::PointerEscape:
        return false;
      case OperandOwnership::InstantaneousUse:
      case OperandOwnership::UnownedInstantaneousUse:
      case OperandOwnership::BitwiseEscape:
        lifetime.updateLivenessForUse(use);
        break;
      case OperandOwnership::ForwardingConsume:
        lifetime.recordLifetimeEnd(use);
        lifetime.updateLivenessForUse(use);
        break;
      case OperandOwnership::DestroyingConsume:
        // destroy_value does not force pruned liveness (but store etc. does).
        if (!isa<DestroyValueInst>(user)) {
          lifetime.updateLivenessForUse(use);
        }
        lifetime.recordLifetimeEnd(use);
        break;
      case OperandOwnership::Borrow:
        // An entire borrow scope is considered a single use that occurs at the
        // point of the end_borrow.
        BorrowingOperand(use).visitLocalEndScopeUses([&lifetime](Operand *end){
          lifetime.updateLivenessForUse(end);
          return true;
        });
        break;
      case OperandOwnership::NestedBorrow:
        // TODO: should be handled exactly like Borrow
      case OperandOwnership::InteriorPointer:
        // TODO: can be ignored since rewriting does not shrink the borrow scope
      case OperandOwnership::ForwardingBorrow:
        // TODO: For liveness, handle forwarded borrows like a copy.
      case OperandOwnership::EndBorrow:
      case OperandOwnership::Reborrow:
        // TODO: record EndBorrow and Reborrow as both a liveness use and
        // potential lifetime end
        llvm_unreachable("unexpected guaranteed value");
      }
    }
  }
  return true;
}

//===----------------------------------------------------------------------===//
// MARK: Step 2. Find the destroy points of the current def based on the pruned
// liveness computed in Step 1.
//===----------------------------------------------------------------------===//

/// The liveness boundary is at a CFG edge `predBB` -> `succBB`, meaning that
/// `lifetime.def()` is live out of at least one other `predBB` successor.
///
/// Create and record a final destroy_value at the beginning of `succBB`
/// (assuming no critical edges).
static void insertDestroyOnCFGEdge(SILBasicBlock *predBB, SILBasicBlock *succBB,
                                   CanonicalOSSALifetime &lifetime) {
  assert(succBB->getSinglePredecessorBlock() == predBB
         && "value is live-out on another predBB successor: critical edge?");

  SILBuilderWithScope builder(succBB->begin());
  auto loc = RegularLocation::getAutoGeneratedLocation(
    succBB->begin()->getLoc().getSourceLoc());
  auto *di = builder.createDestroyValue(loc, lifetime.def());

  lifetime.consumes.recordFinalConsume(di);

  lifetime.setChanged();
  ++NumDestroysGenerated;
  LLVM_DEBUG(llvm::dbgs() << "  Destroy on edge "; di->dump());
}

/// This liveness boundary is within a basic block at the given position.
///
/// Create a final destroy, immediately after `pos`.
static void insertDestroyAtInst(SILBasicBlock::iterator pos,
                                DestroyValueInst *existingDestroy,
                                CanonicalOSSALifetime &lifetime) {
  if (existingDestroy) {
    lifetime.consumes.recordFinalConsume(existingDestroy);
    return;
  }
  SILBuilderWithScope builder(pos);
  auto loc = RegularLocation::getAutoGeneratedLocation(
    (*pos).getLoc().getSourceLoc());
  auto *di = builder.createDestroyValue(loc, lifetime.def());
  lifetime.consumes.recordFinalConsume(di);

  lifetime.setChanged();
  ++NumDestroysGenerated;
  LLVM_DEBUG(llvm::dbgs() << "  Destroy at last use "; di->dump());
}

// The pruned liveness boundary is within the given basic block. Find the
// block's last use. If the last use consumes the value, record it as a
// destroy. Otherwise, insert a new destroy_value.
//
// Return true if a new destroy was inserted.
static void findOrInsertDestroyInBlock(SILBasicBlock *bb,
                                       CanonicalOSSALifetime &lifetime) {
  auto *defInst = lifetime.def()->getDefiningInstruction();
  DestroyValueInst *existingDestroy = nullptr;
  auto instIter = bb->getTerminator()->getIterator();
  while (true) {
    auto *inst = &*instIter;
    switch (lifetime.isInterestingUser(inst)) {
    case CanonicalOSSALifetime::NonUser:
      break;
    case CanonicalOSSALifetime::NonConsumingUse:
      // Insert a destroy after this non-consuming use.
      if (inst == bb->getTerminator()) {
        for (auto &succ : bb->getSuccessors()) {
          insertDestroyOnCFGEdge(bb, succ, lifetime);
        }
      } else {
        insertDestroyAtInst(std::next(instIter), existingDestroy, lifetime);
      }
      return;
    case CanonicalOSSALifetime::ConsumingUse:
      // This consuming use becomes a final destroy.
      lifetime.consumes.recordFinalConsume(inst);
      return;
    }
    // This is not a potential last user. Keep scanning.
    // Allow lifetimes to be artificially extended up to the next call.
    if (ApplySite::isa(inst)) {
      existingDestroy = nullptr;
    } else if (!existingDestroy) {
      if (auto *destroy = dyn_cast<DestroyValueInst>(inst)) {
        if (getCanonicalCopiedDef(destroy->getOperand()) == lifetime.def()) {
          existingDestroy = destroy;
        }
      }
    }
    if (instIter == bb->begin()) {
      assert(cast<SILArgument>(lifetime.def())->getParent() == bb);
      insertDestroyAtInst(instIter, existingDestroy, lifetime);
      return;
    }
    --instIter;
    // If the original def is reached, this is a dead live range. Insert a
    // destroy immediately after the def.
    if (&*instIter == defInst) {
      insertDestroyAtInst(std::next(instIter), existingDestroy, lifetime);
      return;
    }
  }
}

/// Populate `lifetime.consumes` with the final destroy points once copies are
/// eliminated. This only applies to owned values.
///
/// Observations:
/// - lifetime.def() must be postdominated by some subset of its
///   consuming uses, including destroys on all return paths.
/// - The postdominating consumes cannot be within nested loops.
/// - Any blocks in nested loops are now marked LiveOut.
static void findOrInsertDestroys(CanonicalOSSALifetime &lifetime) {
  // Visit each original consuming use or destroy as the starting point for a
  // backward CFG traversal.
  auto lifetimeEndBlocks = lifetime.getLifetimeEndBlocks();
  SmallSetVector<SILBasicBlock *, 8> worklist(lifetimeEndBlocks.begin(),
                                              lifetimeEndBlocks.end());
  // This worklist is also a visited set, so we never pop the entries.
  for (unsigned blockIdx = 0; blockIdx < worklist.size(); ++blockIdx) {
    // Process each block that has not been visited and is not LiveOut.
    SILBasicBlock *bb = worklist[blockIdx];
    switch (lifetime.getBlockLiveness(bb)) {
    case PrunedLiveBlocks::LiveOut:
      // A lifetimeEndBlock may be determined to be LiveOut after analyzing the
      // extended lifetime. It is irrelevent for finding the boundary.
      break;
    case PrunedLiveBlocks::LiveWithin: {
      // The liveness boundary is inside this block. Insert a final destroy
      // inside the block if it doesn't already have one.
      findOrInsertDestroyInBlock(bb, lifetime);
      break;
    }
    case PrunedLiveBlocks::Dead:
      // Continue searching upward to find the pruned liveness boundary.
      for (auto *predBB : bb->getPredecessorBlocks()) {
        if (lifetime.getBlockLiveness(predBB) == PrunedLiveBlocks::LiveOut) {
          insertDestroyOnCFGEdge(predBB, bb, lifetime);
        } else {
          worklist.insert(predBB);
        }
      }
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: Step 3. Rewrite copies and destroys for a single copied definition.
//===----------------------------------------------------------------------===//

/// The lifetime extends beyond given consuming use. Copy the value.
static void copyLiveUse(Operand *use) {
  SILInstruction *user = use->getUser();
  SILBuilderWithScope B(user->getIterator());

  auto loc = RegularLocation::getAutoGeneratedLocation(
    user->getLoc().getSourceLoc());
  auto *copy = B.createCopyValue(loc, use->get());
  use->set(copy);

  ++NumCopiesGenerated;
  LLVM_DEBUG(llvm::dbgs() << "  Copying at last use "; copy->dump());
}

/// Revisit the def-use chain of `lifetime.def()`. Mark unneeded original
/// copies and destroys for deletion. Insert new copies for interior uses that
/// require ownership of the used operand.
static bool rewriteCopies(SILValue def, CanonicalOSSAConsumeInfo &consumes) {
  SmallSetVector<SILInstruction *, 8> instsToDelete;
  SmallSetVector<CopyValueInst *, 8> defUseWorklist;
  bool changed = false;

  // Visit each operand in the def-use chain.
  //
  // Return true if the operand can use the current definition. Return false if
  // it requires a copy.
  auto visitUse = [&](Operand *use) {
    auto *user = use->getUser();
    // Recurse through copies.
    if (auto *copy = dyn_cast<CopyValueInst>(user)) {
      defUseWorklist.insert(copy);
      return true;
    }
    if (auto *destroy = dyn_cast<DestroyValueInst>(user)) {
      // If this destroy was marked as a final destroy, ignore it; otherwise,
      // delete it.
      if (!consumes.claimConsume(destroy)) {
        instsToDelete.insert(destroy);
        LLVM_DEBUG(llvm::dbgs() << "  Removing "; destroy->dump());
        ++NumDestroysEliminated;
      }
      return true;
    }

    // Nonconsuming uses do not need copies and cannot be marked as destroys.
    if (!use->isLifetimeEnding())
      return true;

    // If this use was marked as a final destroy *and* this is the first
    // consumed operand we have visited, then ignore it. Otherwise, treat it as
    // an "interior" consuming use and insert a copy.
    return consumes.claimConsume(user);
  };

  // Perform a def-use traversal, visiting each use operand.
  for (auto useIter = def->use_begin(); useIter != def->use_end();) {
    Operand *use = *useIter++;
    if (!visitUse(use)) {
      copyLiveUse(use);
      changed = true;
    }
  }
  while (!defUseWorklist.empty()) {
    CopyValueInst *srcCopy = defUseWorklist.pop_back_val();
    // Recurse through copies, then remove them.
    Operand *reusedCopyOp = nullptr;
    for (auto useIter = srcCopy->use_begin(); useIter != srcCopy->use_end();) {
      Operand *use = *useIter++;
      if (!visitUse(use)) {
        if (!reusedCopyOp && srcCopy->getParent() == use->getParentBlock()) {
          reusedCopyOp = use;
        } else {
          copyLiveUse(use);
          changed = true;
        }
      }
    }
    if (!(reusedCopyOp && srcCopy->hasOneUse())) {
      changed = true;
      srcCopy->replaceAllUsesWith(srcCopy->getOperand());
      if (reusedCopyOp) {
        reusedCopyOp->set(srcCopy);
      } else {
        instsToDelete.insert(srcCopy);
        LLVM_DEBUG(llvm::dbgs() << "  Removing "; srcCopy->dump());
        ++NumCopiesEliminated;
      }
    }
  }
  assert(consumes.empty());

  // Remove the leftover copy_value and destroy_value instructions.
  if (!instsToDelete.empty()) {
    recursivelyDeleteTriviallyDeadInstructions(instsToDelete.takeVector(),
                                               /*force=*/true);
    changed = true;
  }
  return changed;
}

//===----------------------------------------------------------------------===//
//                            MARK: Top-Level API
//===----------------------------------------------------------------------===//

bool swift::canonicalizeValueLifetime(SILValue def,
                                      CanonicalOSSALifetime &lifetime) {
  // FIXME: handle guaranteed values by creating another pruned liveness for the
  // borrow scope as described in the file-level comment.
  if (def.getOwnershipKind() != OwnershipKind::Owned) {
    return false;
  }
  lifetime.initDef(def);
  // Step 1: computeLiveness
  if (!computeCanonicalLiveness(lifetime)) {
    lifetime.clearLiveness();
    return false;
  }
  // Step 2: findOrInsertDestroys
  findOrInsertDestroys(lifetime);
  // Invalidate book-keeping before deleting instructions.
  lifetime.clearLiveness();
  // Step 3: rewriteCopies
  if (rewriteCopies(def, lifetime.consumes))
    lifetime.setChanged();
  return true;
}

//===----------------------------------------------------------------------===//
//                              MARK: Debugging
//===----------------------------------------------------------------------===//

SWIFT_ASSERT_ONLY_DECL(
  void CanonicalOSSAConsumeInfo::dump() const {
    llvm::dbgs() << "Consumes:";
    for (auto &blockAndInst : finalBlockConsumes) {
      llvm::dbgs() << "  " << *blockAndInst.getSecond();
    }
  })
