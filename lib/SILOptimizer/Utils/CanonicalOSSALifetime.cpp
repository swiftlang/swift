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
///     bool CanonicalizeOSSALifetime::canonicalizeValueLifetime(SILValue def)
///
/// Each time it's called on a single OSSA value, `def`, it performs three
/// steps:
///
/// 1. Compute "pruned" liveness of def and its copies, ignoring original
///    destroys. Initializes `liveness`.
///
/// 2. Find `def`s final destroy points based on its pruned
///    liveness. Initializes `consumes` and inserts new destroy_value
///    instructions.
///
/// 3. Rewrite `def`s original copies and destroys, inserting new copies where
///    needed. Deletes original copies and destroys and inserts new copies.
///
/// See CanonicalOSSALifetime.h for examples.
///
/// TODO: Enable canonical-ossa-rewrite-borrows to rewrite single-block borrows.
/// Add support for multi-block borrows, load_borrow, and phi by using
/// persistentCopies.
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

/// This use-def walk must be consistent with the def-use walks performed
/// within the canonicalizeValueLifetime() implementation.
SILValue CanonicalizeOSSALifetime::getCanonicalCopiedDef(SILValue v) {
  while (auto *copy = dyn_cast<CopyValueInst>(v)) {
    auto def = copy->getOperand();
    if (def.getOwnershipKind() == OwnershipKind::Owned) {
      v = def;
      continue;
    }
    if (auto borrowedVal = BorrowedValue::get(def)) {
      // Any def's that aren't filtered out here must be handled by
      // computeBorrowLiveness.
      switch (borrowedVal->kind) {
      case BorrowedValueKind::SILFunctionArgument:
      case BorrowedValueKind::BeginBorrow:
        return def;
      case BorrowedValueKind::LoadBorrow:
      case BorrowedValueKind::Phi:
        break;
      }
    }
    // This guaranteed value cannot be handled, treat the copy as an owned
    // live range def instead.
    return copy;
  }
  return v;
}

//===----------------------------------------------------------------------===//
//                        MARK: Rewrite borrow scopes
//===----------------------------------------------------------------------===//

llvm::cl::opt<bool>
    EnableRewriteBorrows("canonical-ossa-rewrite-borrows", llvm::cl::init(true),
                         llvm::cl::desc("Enable rewriting borrow scopes"));

bool CanonicalizeOSSALifetime::computeBorrowLiveness() {
  auto borrowedVal = BorrowedValue::get(currentDef);
  if (!borrowedVal) {
    return false;
  }
  switch (borrowedVal->kind) {
  case BorrowedValueKind::SILFunctionArgument:
    // For efficiency, function arguments skip liveness.
    return true;
  case BorrowedValueKind::LoadBorrow:
  case BorrowedValueKind::Phi:
    // TODO: Canonicalize load_borrow scope and phi once consolidateBorrowScope
    // can handle persistentCopies.
    return false;
  case BorrowedValueKind::BeginBorrow:
    break;
  }
  if (!EnableRewriteBorrows) {
    return false;
  }
  borrowedVal->visitLocalScopeEndingUses([this](Operand *use) {
    liveness.updateForUse(use, /*lifetimeEnding*/ true);
  });

  // TODO: Remove this check. Canonicalize multi-block borrow scopes only after
  // consolidateBorrowScope can handle persistentCopies, otherwise we may end up
  // generating more dynamic copies than the non-canonical form.
  if (liveness.numLiveBlocks() > 1) {
    return false;
  }
  return true;
}

// Create a copy for outer uses of the borrow scope introduced by
// currentDef. This copy should only be used by outer uses in the same block
// as the borrow scope.
//
// To use an existing outer copy, we could find its earliest consume. But the
// new copy will immediately canonicalized and a canonical begin_borrow scope
// have no outside uses of its first block.
static CopyValueInst *createOuterCopy(BeginBorrowInst *beginBorrow) {
  SILBuilderWithScope B(beginBorrow);

  auto loc = RegularLocation::getAutoGeneratedLocation(
      beginBorrow->getLoc().getSourceLoc());
  auto *copy = B.createCopyValue(loc, beginBorrow->getOperand());

  ++NumCopiesGenerated;
  LLVM_DEBUG(llvm::dbgs() << "  Outer copy " << *copy);

  return copy;
}

// TODO: Canonicalize multi-block borrow scopes, load_borrow scope, and phi
// borrow scopes by adding one copy per block to persistentCopies for
// each block that dominates an outer use.
void CanonicalizeOSSALifetime::consolidateBorrowScope() {
  if (isa<SILFunctionArgument>(currentDef)) {
    return;
  }
  // Gather all outer uses before rewriting any to avoid scanning any basic
  // block more than once.
  SmallVector<Operand *, 8> outerUses;
  llvm::SmallDenseSet<SILInstruction *, 8> outerUseInsts;
  auto recordOuterUse = [&](Operand *use) {
    outerUses.push_back(use);
    outerUseInsts.insert(use->getUser());
  };

  defUseWorklist.clear();
  defUseWorklist.insert(currentDef);
  while (!defUseWorklist.empty()) {
    SILValue value = defUseWorklist.pop_back_val();
    for (Operand *use : value->getUses()) {
      auto *user = use->getUser();
      // Recurse through copies.
      if (auto *copy = dyn_cast<CopyValueInst>(user)) {
        defUseWorklist.insert(copy);
        continue;
      }
      // debug_value uses are handled like normal uses here. They should be
      // stripped later if required when handling outerCopy or persistentCopies.
      if (liveness.getBlockLiveness(user->getParent())
          == PrunedLiveBlocks::LiveOut) {
        continue;
      }
      switch (use->getOperandOwnership()) {
      case OperandOwnership::NonUse:
        break;
      case OperandOwnership::TrivialUse:
        llvm_unreachable("this operand cannot handle ownership");

      case OperandOwnership::InteriorPointer:
      case OperandOwnership::ForwardingBorrow:
      case OperandOwnership::EndBorrow:
      case OperandOwnership::Reborrow:
        // Ignore uses that must be within the borrow scope.
        break;

      case OperandOwnership::ForwardingUnowned:
      case OperandOwnership::PointerEscape:
      case OperandOwnership::InstantaneousUse:
      case OperandOwnership::UnownedInstantaneousUse:
      case OperandOwnership::BitwiseEscape:
      case OperandOwnership::ForwardingConsume:
      case OperandOwnership::DestroyingConsume:
      case OperandOwnership::Borrow:
        recordOuterUse(use);
        break;
      }
    }
  } // end def-use traversal

  // Remove outer uses that occur before the end of the borrow scope.
  auto *beginBorrow = cast<BeginBorrowInst>(currentDef);
  BorrowedValue::get(beginBorrow)->visitLocalScopeEndingUses([&](Operand *use) {
    // Forward iterate until we find the end of the borrow scope.
    auto *endScope = use->getUser();
    for (auto instIter = beginBorrow->getIterator(),
              endIter = endScope->getIterator();
         instIter != endIter; ++instIter) {
      outerUseInsts.erase(&*instIter);
    }
  });
  if (outerUseInsts.empty()) {
    return;
  }
  // Rewrite the outer uses.
  this->outerCopy = createOuterCopy(beginBorrow);
  for (Operand *use : outerUses) {
    if (!outerUseInsts.count(use->getUser())) {
      continue;
    }
    LLVM_DEBUG(llvm::dbgs() << "  Use of outer copy " << *use->getUser());
    use->set(outerCopy);
  }
}

//===----------------------------------------------------------------------===//
//                    MARK: Step 1. Compute pruned liveness
//===----------------------------------------------------------------------===//

bool CanonicalizeOSSALifetime::computeCanonicalLiveness() {
  defUseWorklist.clear();
  defUseWorklist.insert(currentDef);
  while (!defUseWorklist.empty()) {
    SILValue value = defUseWorklist.pop_back_val();
    for (Operand *use : value->getUses()) {
      auto *user = use->getUser();

      // Recurse through copies.
      if (auto *copy = dyn_cast<CopyValueInst>(user)) {
        defUseWorklist.insert(copy);
        continue;
      }
      // Handle debug_value instructions separately.
      if (pruneDebug) {
        if (auto *dvi = dyn_cast<DebugValueInst>(user)) {
          // Only instructions potentially outside current pruned liveness are
          // interesting.
          if (liveness.getBlockLiveness(dvi->getParent())
              != PrunedLiveBlocks::LiveOut) {
            recordDebugValue(dvi);
          }
          continue;
        }
      }
      switch (use->getOperandOwnership()) {
      case OperandOwnership::NonUse:
        break;
      case OperandOwnership::TrivialUse:
        llvm_unreachable("this operand cannot handle ownership");

      // Conservatively treat a conversion to an unowned value as a pointer
      // escape. Is it legal to canonicalize these?
      case OperandOwnership::ForwardingUnowned:
      case OperandOwnership::PointerEscape:
        return false;
      case OperandOwnership::InstantaneousUse:
      case OperandOwnership::UnownedInstantaneousUse:
      case OperandOwnership::BitwiseEscape:
        liveness.updateForUse(use, /*lifetimeEnding*/ false);
        break;
      case OperandOwnership::ForwardingConsume:
        recordConsumingUse(use);
        liveness.updateForUse(use, /*lifetimeEnding*/ true);
        break;
      case OperandOwnership::DestroyingConsume:
        // destroy_value does not force pruned liveness (but store etc. does).
        if (!isa<DestroyValueInst>(user)) {
          liveness.updateForUse(use, /*lifetimeEnding*/ true);
        }
        recordConsumingUse(use);
        break;
      case OperandOwnership::Borrow:
        // An entire borrow scope is considered a single use that occurs at the
        // point of the end_borrow.
        BorrowingOperand(use).visitLocalEndScopeUses([this](Operand *end) {
          liveness.updateForUse(end, /*lifetimeEnding*/ false);
          return true;
        });
        break;
      case OperandOwnership::InteriorPointer:
      case OperandOwnership::ForwardingBorrow:
      case OperandOwnership::EndBorrow:
      case OperandOwnership::Reborrow:
        llvm_unreachable("operand kind cannot take an owned value");
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
/// `currentDef` is live out of at least one other `predBB` successor.
///
/// Create and record a final destroy_value at the beginning of `succBB`
/// (assuming no critical edges).
static void insertDestroyOnCFGEdge(SILBasicBlock *predBB, SILBasicBlock *succBB,
                                   SILValue def,
                                   CanonicalOSSAConsumeInfo &consumes) {
  assert(succBB->getSinglePredecessorBlock() == predBB
         && "value is live-out on another predBB successor: critical edge?");

  SILBuilderWithScope builder(succBB->begin());
  auto loc = RegularLocation::getAutoGeneratedLocation(
    succBB->begin()->getLoc().getSourceLoc());
  auto *di = builder.createDestroyValue(loc, def);

  consumes.recordFinalConsume(di);

  ++NumDestroysGenerated;
  LLVM_DEBUG(llvm::dbgs() << "  Destroy on edge " << *di);
}

/// This liveness boundary is within a basic block at the given position.
///
/// Create a final destroy, immediately after `pos`.
static void insertDestroyAtInst(SILBasicBlock::iterator pos,
                                DestroyValueInst *existingDestroy, SILValue def,
                                CanonicalOSSAConsumeInfo &consumes) {
  if (existingDestroy) {
    consumes.recordFinalConsume(existingDestroy);
    return;
  }
  SILBuilderWithScope builder(pos);
  auto loc = RegularLocation::getAutoGeneratedLocation(
    (*pos).getLoc().getSourceLoc());
  auto *di = builder.createDestroyValue(loc, def);
  consumes.recordFinalConsume(di);

  ++NumDestroysGenerated;
  LLVM_DEBUG(llvm::dbgs() << "  Destroy at last use " << *di);
}

// The pruned liveness boundary is within the given basic block. Find the
// block's last use. If the last use consumes the value, record it as a
// destroy. Otherwise, insert a new destroy_value.
//
// Return true if a new destroy was inserted.
void CanonicalizeOSSALifetime::findOrInsertDestroyInBlock(SILBasicBlock *bb) {
  auto *defInst = currentDef->getDefiningInstruction();
  DestroyValueInst *existingDestroy = nullptr;
  auto instIter = bb->getTerminator()->getIterator();
  while (true) {
    auto *inst = &*instIter;

    if (pruneDebug) {
      if (auto *dvi = dyn_cast<DebugValueInst>(inst)) {
        if (debugValues.erase(dvi))
          consumes.recordDebugAfterConsume(dvi);
      }
    }
    switch (liveness.isInterestingUser(inst)) {
    case PrunedLiveness::NonUser:
      break;
    case PrunedLiveness::NonLifetimeEndingUse:
      // Insert a destroy after this non-consuming use.
      if (inst == bb->getTerminator()) {
        for (auto &succ : bb->getSuccessors()) {
          insertDestroyOnCFGEdge(bb, succ, currentDef, consumes);
          setChanged();
        }
      } else {
        insertDestroyAtInst(std::next(instIter), existingDestroy, currentDef,
                            consumes);
        setChanged();
      }
      return;
    case PrunedLiveness::LifetimeEndingUse:
      // This consuming use becomes a final destroy.
      consumes.recordFinalConsume(inst);
      return;
    }
    // This is not a potential last user. Keep scanning.
    // Allow lifetimes to be artificially extended up to the next call.
    if (ApplySite::isa(inst)) {
      existingDestroy = nullptr;
    } else if (!existingDestroy) {
      if (auto *destroy = dyn_cast<DestroyValueInst>(inst)) {
        auto destroyDef = CanonicalizeOSSALifetime::getCanonicalCopiedDef(
            destroy->getOperand());
        if (destroyDef == currentDef) {
          existingDestroy = destroy;
        }
      }
    }
    if (instIter == bb->begin()) {
      assert(cast<SILArgument>(currentDef)->getParent() == bb);
      insertDestroyAtInst(instIter, existingDestroy, currentDef, consumes);
      setChanged();
      return;
    }
    --instIter;
    // If the original def is reached, this is a dead live range. Insert a
    // destroy immediately after the def.
    if (&*instIter == defInst) {
      insertDestroyAtInst(std::next(instIter), existingDestroy, currentDef,
                          consumes);
      setChanged();
      return;
    }
  }
}

/// Populate `consumes` with the final destroy points once copies are
/// eliminated. This only applies to owned values.
///
/// Observations:
/// - currentDef must be postdominated by some subset of its
///   consuming uses, including destroys on all return paths.
/// - The postdominating consumes cannot be within nested loops.
/// - Any blocks in nested loops are now marked LiveOut.
void CanonicalizeOSSALifetime::findOrInsertDestroys() {
  // Visit each original consuming use or destroy as the starting point for a
  // backward CFG traversal.
  blockWorklist.clear();
  blockWorklist.insert(consumingBlocks.begin(), consumingBlocks.end());
  // This worklist is also a visited set, so we never pop the entries.
  for (unsigned blockIdx = 0; blockIdx < blockWorklist.size(); ++blockIdx) {
    // Process each block that has not been visited and is not LiveOut.
    SILBasicBlock *bb = blockWorklist[blockIdx];
    switch (liveness.getBlockLiveness(bb)) {
    case PrunedLiveBlocks::LiveOut:
      // A lifetimeEndBlock may be determined to be LiveOut after analyzing the
      // extended  It is irrelevent for finding the boundary.
      break;
    case PrunedLiveBlocks::LiveWithin: {
      // The liveness boundary is inside this block. Insert a final destroy
      // inside the block if it doesn't already have one.
      findOrInsertDestroyInBlock(bb);
      break;
    }
    case PrunedLiveBlocks::Dead:
      // Continue searching upward to find the pruned liveness boundary.
      for (auto *predBB : bb->getPredecessorBlocks()) {
        if (liveness.getBlockLiveness(predBB) == PrunedLiveBlocks::LiveOut) {
          insertDestroyOnCFGEdge(predBB, bb, currentDef, consumes);
          setChanged();
        } else {
          blockWorklist.insert(predBB);
        }
      }
      break;
    }
  }
  // Add any debug_values from Dead blocks into the debugAfterConsume set.
  for (auto *dvi : debugValues) {
    if (liveness.getBlockLiveness(dvi->getParent()) == PrunedLiveBlocks::Dead) {
      consumes.recordDebugAfterConsume(dvi);
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: Step 3. Rewrite copies and destroys
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
  LLVM_DEBUG(llvm::dbgs() << "  Copying at last use " << *copy);
}

/// Revisit the def-use chain of currentDef. Mark unneeded original
/// copies and destroys for deletion. Insert new copies for interior uses that
/// require ownership of the used operand.
void CanonicalizeOSSALifetime::rewriteCopies() {
  bool isOwned = currentDef.getOwnershipKind() == OwnershipKind::Owned;
  assert((!isOwned || !consumes.hasPersistentCopies())
         && "persistent copies use borrowed values");

  SmallSetVector<SILInstruction *, 8> instsToDelete;
  defUseWorklist.clear();

  // Visit each operand in the def-use chain.
  //
  // Return true if the operand can use the current definition. Return false if
  // it requires a copy.
  auto visitUse = [&](Operand *use) {
    auto *user = use->getUser();
    // Recurse through copies.
    if (auto *copy = dyn_cast<CopyValueInst>(user)) {
      if (!consumes.isPersistentCopy(copy)) {
        defUseWorklist.insert(copy);
        return true;
      }
    }
    if (auto *destroy = dyn_cast<DestroyValueInst>(user)) {
      // If this destroy was marked as a final destroy, ignore it; otherwise,
      // delete it.
      if (!consumes.claimConsume(destroy)) {
        instsToDelete.insert(destroy);
        LLVM_DEBUG(llvm::dbgs() << "  Removing " << *destroy);
        ++NumDestroysEliminated;
      }
      return true;
    }

    // Nonconsuming uses do not need copies and cannot be marked as destroys.
    // A lifetime-ending use here must be a consume because EndBorrow/Reborrow
    // uses have been filtered out.
    if (!use->isLifetimeEnding())
      return true;

    // If this use was marked as a final destroy *and* this is the first
    // consumed operand we have visited, then ignore it. Otherwise, treat it as
    // an "interior" consuming use and insert a copy.
    return consumes.claimConsume(user);
  };

  // Perform a def-use traversal, visiting each use operand.
  for (auto useIter = currentDef->use_begin(), endIter = currentDef->use_end();
       useIter != endIter;) {
    Operand *use = *useIter++;
    // A direct lifetime-ending use of a guaranteed value (EndBorrow or
    // Reborrow), never needs a copy.
    if (!isOwned && use->isLifetimeEnding()) {
      continue;
    }
    if (!visitUse(use)) {
      copyLiveUse(use);
      setChanged();
    }
  }
  while (!defUseWorklist.empty()) {
    CopyValueInst *srcCopy = cast<CopyValueInst>(defUseWorklist.pop_back_val());
    // Recurse through copies while replacing their uses.
    Operand *reusedCopyOp = nullptr;
    for (auto useIter = srcCopy->use_begin(); useIter != srcCopy->use_end();) {
      Operand *use = *useIter++;
      if (!visitUse(use)) {
        if (!reusedCopyOp && srcCopy->getParent() == use->getParentBlock()) {
          reusedCopyOp = use;
        } else {
          copyLiveUse(use);
          setChanged();
        }
      }
    }
    if (!(reusedCopyOp && srcCopy->hasOneUse())) {
      setChanged();
      srcCopy->replaceAllUsesWith(srcCopy->getOperand());
      if (reusedCopyOp) {
        reusedCopyOp->set(srcCopy);
      } else {
        instsToDelete.insert(srcCopy);
        LLVM_DEBUG(llvm::dbgs() << "  Removing " << *srcCopy);
        ++NumCopiesEliminated;
      }
    }
  }
  assert(!consumes.hasUnclaimedConsumes());

  // Remove any dead debug_values.
  for (auto *dvi : consumes.getDebugInstsAfterConsume()) {
    LLVM_DEBUG(llvm::dbgs() << "  Removing debug_value: " << *dvi);
    dvi->eraseFromParent();
  }
  consumes.clear();

  // Remove the leftover copy_value and destroy_value instructions.
  if (!instsToDelete.empty()) {
    recursivelyDeleteTriviallyDeadInstructions(instsToDelete.takeVector(),
                                               /*force=*/true);
    setChanged();
  }
}

//===----------------------------------------------------------------------===//
//                            MARK: Top-Level API
//===----------------------------------------------------------------------===//

bool CanonicalizeOSSALifetime::canonicalizeValueLifetime(SILValue def) {
  switch (def.getOwnershipKind()) {
  case OwnershipKind::None:
  case OwnershipKind::Unowned:
  case OwnershipKind::Any:
    return false;
  case OwnershipKind::Guaranteed:
    initDef(def);
    if (!computeBorrowLiveness()) {
      clearLiveness();
      return false;
    }
    // Set outerCopy and persistentCopies and rewrite uses
    // outside the scope.
    consolidateBorrowScope();
    // Invalidate book-keeping before deleting instructions.
    clearLiveness();
    // Rewrite copies and delete extra destroys within the scope.
    assert(!consumes.hasFinalConsumes());
    rewriteCopies();
    return true;
  case OwnershipKind::Owned:
    initDef(def);
    // Step 1: compute liveness
    if (!computeCanonicalLiveness()) {
      clearLiveness();
      return false;
    }
    // Step 2: record final destroys
    findOrInsertDestroys();
    // Invalidate book-keeping before deleting instructions.
    clearLiveness();
    // Step 3: rewrite copies and delete extra destroys
    rewriteCopies();
    return true;
  }
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
