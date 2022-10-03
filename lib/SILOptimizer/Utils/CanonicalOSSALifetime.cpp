//===--- CanonicalOSSALifetime.cpp - Canonicalize OSSA value lifetimes ----===//
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
/// This top-level API rewrites the extended lifetime of a SILValue:
///
///     bool CanonicalizeOSSALifetime::canonicalizeValueLifetime(SILValue def)
///
/// Each time it's called on a single OSSA value, `def`, it performs four
/// steps:
///
/// 1. Compute "pruned" liveness of def and its copies, ignoring original
///    destroys. Initializes `liveness`.
///
/// 2. Find the "extended" boundary of liveness by walking out from the boundary
///    computed by PrunedLiveness out to destroys which aren't separated from
///    the original destory by "interesting" instructions.
///
/// 3. Initializes `consumes` and inserts new destroy_value instructions.
///
/// 4. Rewrite `def`s original copies and destroys, inserting new copies where
///    needed. Deletes original copies and destroys and inserts new copies.
///
/// See CanonicalOSSALifetime.h for examples.
///
/// TODO: Canonicalization currently bails out if any uses of the def has
/// OperandOwnership::PointerEscape. Once project_box is protected by a borrow
/// scope and mark_dependence is associated with an end_dependence, those will
/// no longer be represented as PointerEscapes, and canonicalization will
/// naturally work everywhere as intended. The intention is to keep the
/// canonicalization algorithm as simple and robust, leaving the remaining
/// performance opportunities contingent on fixing the SIL representation.
///
/// TODO: Replace BasicBlock SmallDenseMaps with inlined bits;
/// see BasicBlockDataStructures.h.
///
/// TODO: This algorithm would be extraordinarily simple and cheap except for
/// the following issues:
///
/// 1. Liveness is extended by any overlapping begin/end_access scopes. This
/// avoids calling a destructor within an exclusive access. A simpler
/// alternative would be to model all end_access instructions as deinit
/// barriers, but that may significantly limit optimization.
///
/// 2. Liveness is extended out to original destroys to avoid spurious changes.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "copy-propagation"

#include "swift/SILOptimizer/Utils/CanonicalOSSALifetime.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/DebugOptUtils.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;
using llvm::SmallSetVector;

llvm::Statistic swift::NumCopiesEliminated = {
    DEBUG_TYPE, "NumCopiesEliminated",
    "number of copy_value instructions removed"};

llvm::Statistic swift::NumCopiesGenerated = {
    DEBUG_TYPE, "NumCopiesGenerated",
    "number of copy_value instructions created"};

STATISTIC(NumDestroysEliminated,
          "number of destroy_value instructions removed");
STATISTIC(NumDestroysGenerated, "number of destroy_value instructions created");

//===----------------------------------------------------------------------===//
//                           MARK: General utilities
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

static DestroyValueInst *dynCastToDestroyOf(SILInstruction *instruction,
                                            SILValue def) {
  auto *destroy = dyn_cast<DestroyValueInst>(instruction);
  if (!destroy)
    return nullptr;
  auto originalDestroyedDef = destroy->getOperand();
  if (originalDestroyedDef == def)
    return destroy;
  auto underlyingDestroyedDef =
      CanonicalizeOSSALifetime::getCanonicalCopiedDef(originalDestroyedDef);
  if (underlyingDestroyedDef != def)
    return nullptr;
  return destroy;
}

//===----------------------------------------------------------------------===//
//                    MARK: Step 1. Compute pruned liveness
//===----------------------------------------------------------------------===//

bool CanonicalizeOSSALifetime::computeCanonicalLiveness() {
  defUseWorklist.initialize(getCurrentDef());
  while (SILValue value = defUseWorklist.pop()) {
    SILPhiArgument *arg;
    if ((arg = dyn_cast<SILPhiArgument>(value)) && arg->isPhi()) {
      visitAdjacentReborrowsOfPhi(arg, [&](SILPhiArgument *reborrow) {
        defUseWorklist.insert(reborrow);
        return true;
      });
    }
    for (Operand *use : value->getUses()) {
      auto *user = use->getUser();

      // Recurse through copies.
      if (auto *copy = dyn_cast<CopyValueInst>(user)) {
        defUseWorklist.insert(copy);
        continue;
      }
      // Handle debug_value instructions separately.
      if (pruneDebugMode) {
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
      // escape. Is it legal to canonicalize ForwardingUnowned?
      case OperandOwnership::ForwardingUnowned:
      case OperandOwnership::PointerEscape:
        return false;
      case OperandOwnership::InstantaneousUse:
      case OperandOwnership::UnownedInstantaneousUse:
      case OperandOwnership::BitwiseEscape:
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        break;
      case OperandOwnership::ForwardingConsume:
        recordConsumingUse(use);
        liveness.updateForUse(user, /*lifetimeEnding*/ true);
        break;
      case OperandOwnership::DestroyingConsume:
        if (isa<DestroyValueInst>(user)) {
          destroys.insert(user);
        } else {
          // destroy_value does not force pruned liveness (but store etc. does).
          liveness.updateForUse(user, /*lifetimeEnding*/ true);
        }
        recordConsumingUse(use);
        break;
      case OperandOwnership::Borrow:
        if (liveness.updateForBorrowingOperand(use)
            != InnerBorrowKind::Contained) {
          return false;
        }
        break;
      case OperandOwnership::InteriorPointer:
      case OperandOwnership::ForwardingBorrow:
      case OperandOwnership::EndBorrow:
        // Guaranteed values are considered uses of the value when the value is
        // an owned phi and the guaranteed values are adjacent reborrow phis or
        // reborrow of such.
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        break;
      case OperandOwnership::Reborrow:
        BranchInst *branch;
        if (!(branch = dyn_cast<BranchInst>(user))) {
          // Non-phi reborrows (tuples, etc) never end the lifetime of the owned
          // value.
          liveness.updateForUse(user, /*lifetimeEnding*/ false);
          defUseWorklist.insert(cast<SingleValueInstruction>(user));
          break;
        }
        if (is_contained(user->getOperandValues(), getCurrentDef())) {
          // An adjacent phi consumes the value being reborrowed.  Although this
          // use doesn't end the lifetime, this user does.
          liveness.updateForUse(user, /*lifetimeEnding*/ true);
          break;
        }
        // No adjacent phi consumes the value.  This use is not lifetime ending.
        liveness.updateForUse(user, /*lifetimeEnding*/ false);
        // This branch reborrows a guaranteed phi whose lifetime is dependent on
        // currentDef.  Uses of the reborrowing phi extend liveness.
        auto *reborrow = branch->getArgForOperand(use);
        defUseWorklist.insert(reborrow);
        break;
      }
    }
  }
  return true;
}

// Return true if \p inst is an end_access whose access scope overlaps the end
// of the pruned live range. This means that a hoisted destroy might execute
// within the access scope which previously executed outside the access scope.
//
// Not overlapping (ignored):
//
//     %def
//     use %def     // pruned liveness ends here
//     begin_access // access scope unrelated to def
//     end_access
//
// Overlapping (must extend pruned liveness):
//
//     %def
//     begin_access // access scope unrelated to def
//     use %def     // pruned liveness ends here
//     end_access
//
// Overlapping (must extend pruned liveness):
//
//     begin_access // access scope unrelated to def
//     %def
//     use %def     // pruned liveness ends here
//     end_access
//
bool CanonicalizeOSSALifetime::
endsAccessOverlappingPrunedBoundary(SILInstruction *inst) {
  if (isa<EndUnpairedAccessInst>(inst)) {
    return true;
  }
  auto *endAccess = dyn_cast<EndAccessInst>(inst);
  if (!endAccess) {
    return false;
  }
  auto *beginAccess = endAccess->getBeginAccess();
  SILBasicBlock *beginBB = beginAccess->getParent();
  switch (liveness.getBlockLiveness(beginBB)) {
  case PrunedLiveBlocks::LiveOut:
    // Found partial overlap of the form:
    //     currentDef
    //     beginAccess
    //     br...
    //   bb...
    //     use
    //     endAccess
    return true;
  case PrunedLiveBlocks::LiveWithin:
    // Check for partial overlap of this form where beginAccess and the last use
    // are in the same block:
    //     currentDef
    //     beginAccess
    //     use
    //     endAccess
    if (std::find_if(std::next(beginAccess->getIterator()), beginBB->end(),
                     [this](SILInstruction &nextInst) {
                       return liveness.isInterestingUser(&nextInst)
                         != PrunedLiveness::NonUser;
                     }) != beginBB->end()) {
      // An interesting use after the beginAccess means overlap.
      return true;
    }
    return false;
  case PrunedLiveBlocks::Dead:
    // Check for partial overlap of this form where beginAccess and currentDef
    // are in different blocks:
    //     beginAccess
    //     br...
    //  bb...
    //     currentDef
    //     endAccess
    //
    // Since beginAccess is not within the canonical live range, its access
    // scope overlaps only if there is a path from beginAccess to currentDef
    // that does not pass through endAccess. endAccess is dominated by
    // both currentDef and begin_access. Therefore, such a path only exists if
    // beginAccess dominates currentDef.
    return domTree->properlyDominates(beginAccess->getParent(),
                                      getCurrentDef()->getParentBlock());
  }
  llvm_unreachable("covered switch");
}

// Find all overlapping access scopes and extend pruned liveness to cover them:
//
// This may also unnecessarily, but conservatively extend liveness over some
// originally overlapping access, such as:
//
//     begin_access // access scope unrelated to def
//     %def
//     use %def
//     destroy %def
//     end_access
//
// Or:
//
//     %def
//     begin_access // access scope unrelated to def
//     use %def
//     destroy %def
//     end_access
//
// To minimize unnecessary lifetime extension, only search for end_access
// within dead blocks that are backward reachable from an original destroy.
//
// Note that lifetime extension is iterative because adding a new liveness use
// may create new overlapping access scopes. This can happen because there is no
// guarantee of strict stack discipline across unrelated access. For example:
//
//     %def
//     begin_access A
//     use %def        // Initial pruned lifetime boundary
//     begin_access B
//     end_access A    // Lifetime boundary after first extension
//     end_access B    // Lifetime boundary after second extension
//     destroy %def
//
// If the lifetime extension did not iterate, then def would be destroyed within
// B's access scope when originally it was destroyed outside that scope.
void CanonicalizeOSSALifetime::extendLivenessThroughOverlappingAccess() {
  this->accessBlocks = accessBlockAnalysis->get(getCurrentDef()->getFunction());

  // Visit each original consuming use or destroy as the starting point for a
  // backward CFG traversal. This traversal must only visit blocks within the
  // original extended lifetime.
  bool changed = true;
  while (changed) {
    changed = false;
    // The blocks in which we may have to extend liveness over access scopes.
    //
    // It must be populated first so that we can test membership during the loop
    // (see findLastConsume).
    BasicBlockSetVector blocksToVisit(getCurrentDef()->getFunction());
    for (auto *block : consumingBlocks) {
      blocksToVisit.insert(block);
    }
    for (auto iterator = blocksToVisit.begin(); iterator != blocksToVisit.end();
         ++iterator) {
      auto *bb = *iterator;
      // If the block isn't dead, then we won't need to extend liveness within
      // any of its predecessors (though we may within it).
      if (liveness.getBlockLiveness(bb) != PrunedLiveBlocks::Dead)
        continue;
      // Continue searching upward to find the pruned liveness boundary.
      for (auto *predBB : bb->getPredecessorBlocks()) {
        blocksToVisit.insert(predBB);
      }
    }
    for (auto *bb : blocksToVisit) {
      auto blockLiveness = liveness.getBlockLiveness(bb);
      // Ignore blocks within pruned liveness.
      if (blockLiveness == PrunedLiveBlocks::LiveOut) {
        continue;
      }
      if (blockLiveness == PrunedLiveBlocks::Dead) {
        // Otherwise, ignore dead blocks with no nonlocal end_access.
        if (!accessBlocks->containsNonLocalEndAccess(bb)) {
          continue;
        }
      }
      bool blockHasUse = (blockLiveness == PrunedLiveBlocks::LiveWithin);
      // Find the latest partially overlapping access scope, if one exists:
      //     use %def // pruned liveness ends here
      //     end_access

      // Whether to look for the last consume in the block.
      //
      // We need to avoid extending liveness over end_accesses that occur after
      // original liveness ended.
      bool findLastConsume =
          consumingBlocks.contains(bb) &&
          llvm::none_of(bb->getSuccessorBlocks(), [&](auto *successor) {
            return blocksToVisit.contains(successor) &&
                   liveness.getBlockLiveness(successor) ==
                       PrunedLiveBlocks::Dead;
          });
      for (auto &inst : llvm::reverse(*bb)) {
        if (findLastConsume) {
          findLastConsume = !destroys.contains(&inst);
          continue;
        }
        // Stop at the latest use. An earlier end_access does not overlap.
        if (blockHasUse && liveness.isInterestingUser(&inst)) {
          break;
        }
        if (endsAccessOverlappingPrunedBoundary(&inst)) {
          liveness.updateForUse(&inst, /*lifetimeEnding*/ false);
          changed = true;
          break;
        }
      }
      // If liveness changed, might as well restart CFG traversal.
      if (changed) {
        break;
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: Step 2. Find the destroy points of the current def based on the pruned
// liveness computed in Step 1.
//===----------------------------------------------------------------------===//

namespace swift {
/// Extends the boundary from PrunedLiveness down to preexisting destroys of the
/// def which aren't separated from the original boundary by "interesting"
/// instructions.
///
/// The motivation for extending the boundary is to avoid "churning" when
/// iterating to a fixed point by canonicalizing the lifetimes of several
/// values with overlapping live ranges and failing to find a fixed point
/// because their destroys are repeatedly hoisted over one another.
class CanonicalizeOSSALifetimeBoundaryExtender {
  SSAPrunedLiveness &liveness;
  PrunedLivenessBoundary &originalBoundary;
  SILValue currentDef;

public:
  CanonicalizeOSSALifetimeBoundaryExtender(
      SSAPrunedLiveness &liveness, PrunedLivenessBoundary &originalBoundary,
      SILValue currentDef)
      : liveness(liveness), originalBoundary(originalBoundary),
        currentDef(currentDef){};

  /// Compute the extended boundary by walking out from the original boundary
  /// (from PrunedLiveness::computeBoundary) down to any destroys that appear
  /// later but which aren't separated from the original boundary by
  /// "interesting" users.
  void extend(PrunedLivenessBoundary &boundary) {
    for (auto *def : originalBoundary.deadDefs) {
      if (auto *argument = dyn_cast<SILArgument>(def)) {
        extendBoundaryFromCFGEdge(argument->getParent(), boundary);
      } else {
        extendBoundaryFromInstruction(cast<SILInstruction>(def), boundary);
      }
    }
    for (auto *destination : originalBoundary.boundaryEdges) {
      extendBoundaryFromCFGEdge(destination, boundary);
    }
    for (auto *user : originalBoundary.lastUsers) {
      extendBoundaryFromInstruction(user, boundary);
    }
  }

private:
  /// Look past ignoreable instructions to find the _last_ destroy after the
  /// specified instruction that destroys \p def.
  static DestroyValueInst *findDestroyAfter(SILInstruction *previous,
                                            SILValue def) {
    DestroyValueInst *retval = nullptr;
    for (auto *instruction = previous->getNextInstruction(); instruction;
         instruction = instruction->getNextInstruction()) {
      if (!CanonicalizeOSSALifetime::ignoredByDestroyHoisting(
              instruction->getKind()))
        break;
      if (auto destroy = dynCastToDestroyOf(instruction, def))
        retval = destroy;
    }
    return retval;
  }

  /// Look past ignoreable instructions to find the _last_ destroy at or after
  /// the specified instruction that destroys \p def.
  static DestroyValueInst *findDestroyAtOrAfter(SILInstruction *start,
                                                SILValue def) {
    if (auto *dvi = dynCastToDestroyOf(start, def))
      return dvi;
    return findDestroyAfter(start, def);
  }

  /// Look past ignoreable instructions to find the _last_ destroy "on" the edge
  /// that ends at \p destination (i.e. looking from the beginning of \p
  /// destination) that destroys \p def.
  static DestroyValueInst *findDestroyOnCFGEdge(SILBasicBlock *destination,
                                                SILValue def) {
    return findDestroyAtOrAfter(&*destination->begin(), def);
  }

  /// Compute the points on the extended boundary found by walking down from the
  /// boundary edge in the original boundary (uniquely determined by the
  /// specified destination edge) down to any destroys that appear later but
  /// which aren't separated from the original boundary by "interesting" users.
  void extendBoundaryFromCFGEdge(SILBasicBlock *destination,
                                 PrunedLivenessBoundary &boundary) {
    if (auto *dvi = findDestroyOnCFGEdge(destination, currentDef)) {
      boundary.lastUsers.push_back(dvi);
    } else {
      boundary.boundaryEdges.push_back(destination);
    }
  }

  /// Compute the points on the extended boundary found by walking down from the
  /// specified instruction in the original boundary down to any destroys that
  /// appear later but which aren't separated from the original boundary by
  /// "interesting" users.
  void extendBoundaryFromInstruction(SILInstruction *user,
                                     PrunedLivenessBoundary &boundary) {
    if (auto *dvi = dynCastToDestroyOf(user, currentDef)) {
      auto *existingDestroy = findDestroyAtOrAfter(dvi, currentDef);
      assert(existingDestroy && "couldn't find a destroy at or after one!?");
      boundary.lastUsers.push_back(existingDestroy);
      return;
    }
    switch (liveness.isInterestingUser(user)) {
    case PrunedLiveness::IsInterestingUser::LifetimeEndingUse:
      // Even if we saw a destroy after this consuming use, we don't want to
      // add it to the boundary.  We will rewrite copies so that this user is
      // the final consuming user on this path.
      boundary.lastUsers.push_back(user);
      return;
    case PrunedLiveness::IsInterestingUser::NonLifetimeEndingUse:
    case PrunedLiveness::IsInterestingUser::NonUser:
      if (isa<TermInst>(user)) {
        auto *block = user->getParent();
        for (auto *successor : block->getSuccessorBlocks()) {
          extendBoundaryFromCFGEdge(successor, boundary);
        }
        return;
      }
      if (auto *existingDestroy = findDestroyAfter(user, currentDef)) {
        boundary.lastUsers.push_back(existingDestroy);
        return;
      }
      boundary.lastUsers.push_back(user);
    }
  }
};
} // namespace swift

void CanonicalizeOSSALifetime::findExtendedBoundary(
    PrunedLivenessBoundary &boundary) {
  PrunedLivenessBoundary originalBoundary;
  liveness.computeBoundary(originalBoundary, consumingBlocks.getArrayRef());

  CanonicalizeOSSALifetimeBoundaryExtender extender(liveness, originalBoundary,
                                                    getCurrentDef());
  extender.extend(boundary);
  assert(boundary.deadDefs.empty() && "dead defs in extended boundary!?");
}

//===----------------------------------------------------------------------===//
// MARK: Step 3. Insert destroys onto the boundary found in step 2 where needed.
//===----------------------------------------------------------------------===//

/// Create a new destroy_value instruction before the specified instruction and
/// record it as a final consume.
static void insertDestroyBeforeInstruction(SILInstruction *nextInstruction,
                                           SILValue currentDef,
                                           CanonicalOSSAConsumeInfo &consumes,
                                           InstModCallbacks &callbacks) {
  SILBuilderWithScope builder(nextInstruction);
  auto loc =
      RegularLocation::getAutoGeneratedLocation(nextInstruction->getLoc());
  auto *dvi = builder.createDestroyValue(loc, currentDef);
  callbacks.createdNewInst(dvi);
  consumes.recordFinalConsume(dvi);
  ++NumDestroysGenerated;
}

/// Populate `consumes` with the final destroy points once copies are
/// eliminated. This only applies to owned values.
///
/// Observations:
/// - currentDef must be postdominated by some subset of its
///   consuming uses, including destroys on all return paths.
/// - The postdominating consumes cannot be within nested loops.
/// - Any blocks in nested loops are now marked LiveOut.
void CanonicalizeOSSALifetime::insertDestroysOnBoundary(
    PrunedLivenessBoundary &boundary) {
  for (auto *instruction : boundary.lastUsers) {
    if (auto *dvi = dynCastToDestroyOf(instruction, getCurrentDef())) {
      consumes.recordFinalConsume(dvi);
      continue;
    }
    switch (liveness.isInterestingUser(instruction)) {
    case PrunedLiveness::IsInterestingUser::LifetimeEndingUse:
      consumes.recordFinalConsume(instruction);
      continue;
    case PrunedLiveness::IsInterestingUser::NonLifetimeEndingUse:
    case PrunedLiveness::IsInterestingUser::NonUser:
      auto *insertionPoint = instruction->getNextInstruction();
      // If there were no next instruction, it would be a TermInst and we would
      // have added the successors as boundary edge destinations (or found
      // destroys in their bodies).
      assert(insertionPoint);
      insertDestroyBeforeInstruction(insertionPoint, getCurrentDef(), consumes,
                                     getCallbacks());
      LLVM_DEBUG(llvm::dbgs()
                 << "  Destroy at last use " << insertionPoint << "\n");
      continue;
    }
  }
  for (auto *edgeDestination : boundary.boundaryEdges) {
    auto *insertionPoint = &*edgeDestination->begin();
    insertDestroyBeforeInstruction(insertionPoint, getCurrentDef(), consumes,
                                   getCallbacks());
    LLVM_DEBUG(llvm::dbgs() << "  Destroy on edge " << edgeDestination << "\n");
  }
}

//===----------------------------------------------------------------------===//
// MARK: Step 4. Rewrite copies and destroys
//===----------------------------------------------------------------------===//

/// The lifetime extends beyond given consuming use. Copy the value.
///
/// This can set the operand value, but cannot invalidate the use iterator.
void swift::copyLiveUse(Operand *use, InstModCallbacks &instModCallbacks) {
  SILInstruction *user = use->getUser();
  SILBuilderWithScope builder(user->getIterator());

  auto loc = RegularLocation::getAutoGeneratedLocation(user->getLoc());
  auto *copy = builder.createCopyValue(loc, use->get());
  instModCallbacks.createdNewInst(copy);
  use->set(copy);

  ++NumCopiesGenerated;
  LLVM_DEBUG(llvm::dbgs() << "  Copying at last use " << *copy);
}

/// Revisit the def-use chain of currentDef. Mark unneeded original
/// copies and destroys for deletion. Insert new copies for interior uses that
/// require ownership of the used operand.
void CanonicalizeOSSALifetime::rewriteCopies() {
  assert(getCurrentDef()->getOwnershipKind() == OwnershipKind::Owned);

  InstructionSetVector instsToDelete(getCurrentDef()->getFunction());
  defUseWorklist.clear();

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
        LLVM_DEBUG(llvm::dbgs() << "  Removing " << *destroy);
        ++NumDestroysEliminated;
      } else if (pruneDebugMode) {
        // If this destroy was marked as a final destroy, add it to liveness so
        // that we don't delete any debug instructions that occur before it.
        // (Only relevant in pruneDebugMode).
        liveness.updateForUse(destroy, /*lifetimeEnding*/ true);
      }
      return true;
    }

    // Nonconsuming uses do not need copies and cannot be marked as destroys.
    // A lifetime-ending use here must be a consume because EndBorrow/Reborrow
    // uses have been filtered out.
    if (!use->isLifetimeEnding())
      return true;

    // If this use was not marked as a final destroy *or* this is not the first
    // consumed operand we visited, then it needs a copy.
    if (!consumes.claimConsume(user)) {
      maybeNotifyMoveOnlyCopy(use);
      return false;
    }

    // Ok, this is a final user that isn't a destroy_value. Notify our caller if
    // we were asked to.
    //
    // If we need this for diagnostics, we will only use it if we found actual
    // uses that required copies.
    maybeNotifyFinalConsumingUse(use);

    return true;
  };

  // Perform a def-use traversal, visiting each use operand.
  for (auto useIter = getCurrentDef()->use_begin(),
         endIter = getCurrentDef()->use_end(); useIter != endIter;) {
    Operand *use = *useIter++;
    if (!visitUse(use)) {
      copyLiveUse(use, getCallbacks());
    }
  }
  while (SILValue value = defUseWorklist.pop()) {
    CopyValueInst *srcCopy = cast<CopyValueInst>(value);
    // Recurse through copies while replacing their uses.
    Operand *reusedCopyOp = nullptr;
    for (auto useIter = srcCopy->use_begin(); useIter != srcCopy->use_end();) {
      Operand *use = *useIter++;
      if (!visitUse(use)) {
        if (!reusedCopyOp && srcCopy->getParent() == use->getParentBlock()) {
          reusedCopyOp = use;
        } else {
          copyLiveUse(use, getCallbacks());
        }
      }
    }
    if (!(reusedCopyOp && srcCopy->hasOneUse())) {
      getCallbacks().replaceValueUsesWith(srcCopy, srcCopy->getOperand());
      if (reusedCopyOp) {
        reusedCopyOp->set(srcCopy);
      } else {
        if (instsToDelete.insert(srcCopy)) {
          LLVM_DEBUG(llvm::dbgs() << "  Removing " << *srcCopy);
          ++NumCopiesEliminated;
        }
      }
    }
  }
  assert(!consumes.hasUnclaimedConsumes());

  if (pruneDebugMode) {
    for (auto *dvi : debugValues) {
      if (!liveness.isWithinBoundary(dvi)) {
        LLVM_DEBUG(llvm::dbgs() << "  Removing debug_value: " << *dvi);
        deleter.forceDelete(dvi);
      }
    }
  }

  // Remove the leftover copy_value and destroy_value instructions.
  for (auto iter = instsToDelete.begin(), end = instsToDelete.end();
       iter != end; ++iter) {
    deleter.forceDelete(*iter);
  }
}

//===----------------------------------------------------------------------===//
//                            MARK: Top-Level API
//===----------------------------------------------------------------------===//

/// Canonicalize a single extended owned lifetime.
bool CanonicalizeOSSALifetime::canonicalizeValueLifetime(SILValue def) {
  if (def->getOwnershipKind() != OwnershipKind::Owned)
    return false;

  if (def->isLexical())
    return false;

  LLVM_DEBUG(llvm::dbgs() << "  Canonicalizing: " << def);

  // Note: There is no need to register callbacks with this utility. 'onDelete'
  // is the only one in use to handle dangling pointers, which could be done
  // instead be registering a temporary handler with the pass. Canonicalization
  // is only allowed to create and delete instructions that are associated with
  // this canonical def (copies and destroys). Each canonical def has a disjoint
  // extended lifetime. Any pass calling this utility should work at the level
  // canonical defs, not individual instructions.
  //
  // NotifyWillBeDeleted will not work because copy rewriting removes operands
  // before deleting instructions. Also prohibit setUse callbacks just because
  // that would simply be insane.
  assert(!getCallbacks().notifyWillBeDeletedFunc
         && !getCallbacks().setUseValueFunc && "unsupported");

  initDef(def);
  // Step 1: compute liveness
  if (!computeCanonicalLiveness()) {
    LLVM_DEBUG(llvm::errs() << "Failed to compute canonical liveness?!\n");
    clearLiveness();
    return false;
  }
  extendLivenessThroughOverlappingAccess();
  // Step 2: compute boundary
  PrunedLivenessBoundary boundary;
  findExtendedBoundary(boundary);
  // Step 3: insert destroys and record consumes
  insertDestroysOnBoundary(boundary);
  // Step 4: rewrite copies and delete extra destroys
  rewriteCopies();

  clearLiveness();
  consumes.clear();
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
