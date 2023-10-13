//===--- PhiStorageOptimizer.cpp - Phi storage optimizer ------------------===//
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
/// PhiStorageOptimizer implements an analysis used by AddressLowering
/// to reuse storage across block arguments.
///
/// In OSSA, phi operands can often be coalesced because they are
/// consuming--they end the lifetime of their operand. This optimization may
/// fail to coalesce an operand for two major reasons:
///
/// 1. This phi operand is already coalesced with other storage, possibly of a
/// different type:
///
///     %field = struct_extract %struct : $Struct<T>, #field
///     br bb(%field : $T)
///
///   bb(%phi : @owned $T):
///     ...
///
/// 2. This phi operand interferes with another coalesced phi operand.
///
/// Only one of the call results below, either %get0 or %get1, can be coalesced
/// with %phi. The %phi will itself be coalesced with this function's indirect
/// @out argument.
///
///   sil [ossa] @function : $@convention(thin) <T> () -> @out T {
///   bb0:
///     %get0 = apply %get<T>() : $@convention(thin) <τ_0_0>() -> @out τ_0_0
///     %get1 = apply %get<T>() : $@convention(thin) <τ_0_0>() -> @out τ_0_0
///     cond_br undef, bb2, bb1
///
///   bb1:
///     destroy_value %get0 : $T
///     br bb3(%get1 : $T)
///
///   bb2:
///     destroy_value %get1 : $T
///     br bb3(%get0 : $T)
///
///   bb3(%phi : @owned $T):
///     return %phi : $T
///
/// TODO: Liveness is currently recorded at the block level. This could be
/// extended to handle operand with nonoverlapping liveness in the same
/// block. In this case, %get0 and %get1 could both be coalesced with a bit of
/// extra book-keeping:
///
///   bb0:
///     %get0 = apply %get<T>() : $@convention(thin) <τ_0_0>() -> @out τ_0_0
///
///   bb1:
///     destroy_value %get0 : $T
///     %get1 = apply %get<T>() : $@convention(thin) <τ_0_0>() -> @out τ_0_0
///     br bb3(%get1 : $T)
///
///   bb2:
///     br bb3(%get0 : $T)
///
///   bb3(%phi : @owned $T):
///
/// TODO: This does not yet coalesce the copy_value instructions that produce a
/// phi operand. Such a copy implies that both the operand and phi value are
/// live past the phi. Nonetheless, they could still be coalesced as
/// follows... First coalesce all direct phi operands. Then transitively
/// coalesce copies by checking if the copy's source is coalesceable, then
/// redoing the liveness traversal from the uses of the copy.
///
/// TODO: This approach uses on-the-fly liveness discovery for all incoming
/// values at once. It requires no storage for liveness. Hopefully this is
/// sufficient for -Onone. At -O, we could explore implementing strong phi
/// elimination. However, that depends the ability to perform interference
/// checks between arbitrary storage locations, which requires computing and
/// storing liveness per-storage location.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "address-lowering"

#include "PhiStorageOptimizer.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

namespace swift {

/// An analysis used by AddressLowering to reuse phi storage.
///
/// Populates CoalescedPhi::coalescedOperands with all phi operands that can
/// reuse the phi's storage.
class PhiStorageOptimizer {
  PhiValue phi;
  const ValueStorageMap &valueStorageMap;

  CoalescedPhi &coalescedPhi;

  BasicBlockSet occupiedBlocks;

public:
  PhiStorageOptimizer(PhiValue phi, const ValueStorageMap &valueStorageMap,
                      CoalescedPhi &coalescedPhi)
      : phi(phi), valueStorageMap(valueStorageMap), coalescedPhi(coalescedPhi),
        occupiedBlocks(getFunction()) {}

  SILFunction *getFunction() const { return phi.phiBlock->getParent(); }

  void optimize();

protected:
  bool hasUseProjection(SILInstruction *defInst);
  bool canCoalesceValue(SILValue incomingVal);
  void tryCoalesceOperand(SILBasicBlock *incomingPred);
  bool recordUseLiveness(SILValue incomingVal, BasicBlockSetVector &liveBlocks);
};

} // namespace swift

void CoalescedPhi::coalesce(PhiValue phi,
                            const ValueStorageMap &valueStorageMap) {
  assert(empty() && "attempt to recoalesce the same phi");

  PhiStorageOptimizer(phi, valueStorageMap, *this).optimize();
}

/// Optimize phi storage by coalescing phi operands.
///
/// Finds all non-interfering phi operands and adds them to the result's
/// coalescedOperands. The algorithm can be described in the abstract as follows
/// (assuming no critical edges):
///
/// All blocks are in one of three states at any point:
/// - clean (not present in the live or occupied set)
/// - live
/// - occupied
///
/// All blocks start clean.
///
/// For each incoming value:
///
///   For all uses of the current incoming value:
///
///     Scan the CFG backward following predecessors.
///     If the current block is:
///
///       Clean: mark it live and continue scanning.
///
///       Live: stop scanning and continue with the next use.
///
///       Occupied: record interference, stop scanning, continue to next use.
///
///   If no occupied blocks were reached, mark this phi operand coalesced. It's
///   storage can be projected from the phi storage.
///
///   Mark all live blocks occupied.
///
/// In the end, we have a set of non-interfering incoming values that can reuse
/// the phi's storage.
void PhiStorageOptimizer::optimize() {
  // The single incoming value case always projects storage.
  if (auto *predecessor = phi.phiBlock->getSinglePredecessorBlock()) {
    if (canCoalesceValue(phi.getValue()->getIncomingPhiValue(predecessor))) {
      // Storage will always be allocated for the phi.  The optimization
      // attempts to let incoming values reuse the phi's storage.  This isn't
      // always possible, even in the single incoming value case.  For example,
      // it isn't possible when the incoming value is a function argument or
      // when the incoming value is already a projection.
      coalescedPhi.coalescedOperands.push_back(phi.getOperand(predecessor));
    }
    return;
  }
  for (auto *incomingPred : phi.phiBlock->getPredecessorBlocks()) {
    tryCoalesceOperand(incomingPred);
  }
}

// Return true if any of \p defInst's operands are composing use projections
// into \p defInst's storage.
bool PhiStorageOptimizer::hasUseProjection(SILInstruction *defInst) {
  for (Operand &oper : defInst->getAllOperands()) {
    if (valueStorageMap.isComposingUseProjection(&oper))
      return true;
  }
  return false;
}

// Return true in \p incomingVal can be coalesced with this phi ignoring
// possible interference. Simply determine whether storage reuse is possible.
//
// Precondition: \p incomingVal is an operand of this phi.
bool PhiStorageOptimizer::canCoalesceValue(SILValue incomingVal) {
  // A Phi must not project from storage that was initialized on a path that
  // reaches the phi because other uses of the storage may interfere with the
  // phi. A phi may, however, be a composing use projection.
  assert(!valueStorageMap.getStorage(phi.getValue()).isDefProjection
         && !valueStorageMap.getStorage(phi.getValue()).isPhiProjection());

  auto &incomingStorage = valueStorageMap.getStorage(incomingVal);

  // If the incoming use directly reuses its def storage, projects out of its
  // def storage, or is pre-allocated, then it can't be coalesced. When incoming
  // storage is directly reused, isAllocated() is false. isProjection() covers
  // the other cases.
  //
  // Coalescing use projections from incomingVal into its other non-phi uses
  // could be handled, but would require by recursively following uses across
  // projections when computing liveness.
  if (!incomingStorage.isAllocated() || incomingStorage.isProjection())
    return false;

  auto *defInst = incomingVal->getDefiningInstruction();
  if (!defInst) {
    // Indirect function arguments were replaced by loads.
    assert(!isa<SILFunctionArgument>(incomingVal));
    // Do not coalesce a phi with other phis. This would require liveness
    // analysis of the whole phi web before coalescing phi operands.
    return false;
  }

  // Don't coalesce an incoming value unless it's storage is from a stack
  // allocation, which can be replaced with another alloc_stack.
  if (!isa<AllocStackInst>(incomingStorage.storageAddress))
    return false;

  // Make sure that the incomingVal is not coalesced with any of its operands.
  //
  // Handling incomingValues whose operands project into them would require by
  // recursively finding the set of value definitions and their dominating defBB
  // instead of simply incomingVal->getParentBlock().
  if (hasUseProjection(defInst))
    return false;

  return true;
}

// Process a single incoming phi operand. Compute the value's liveness while
// checking for interference. If no interference exists, mark it coalesced.
void PhiStorageOptimizer::tryCoalesceOperand(SILBasicBlock *incomingPred) {
  Operand *incomingOper = phi.getOperand(incomingPred);
  SILValue incomingVal = incomingOper->get();

  if (!canCoalesceValue(incomingVal))
    return;

  BasicBlockSetVector liveBlocks(getFunction());
  if (!recordUseLiveness(incomingVal, liveBlocks))
    return;

  for (auto *block : liveBlocks) {
    occupiedBlocks.insert(block);
  }
  assert(occupiedBlocks.contains(incomingPred));
  coalescedPhi.coalescedOperands.push_back(incomingOper);
}

// Record liveness generated by uses of \p incomingVal.
//
// Return true if no interference was detected along the way.
bool PhiStorageOptimizer::recordUseLiveness(SILValue incomingVal,
                                            BasicBlockSetVector &liveBlocks) {
  assert(liveBlocks.empty());

  // Stop liveness traversal at defBB.
  SILBasicBlock *defBB = incomingVal->getParentBlock();
  for (auto *use : incomingVal->getUses()) {
    StackList<SILBasicBlock *> liveBBWorklist(getFunction());

    // If \p liveBB is already occupied by another value, return
    // false. Otherwise, mark \p liveBB live and push it onto liveBBWorklist.
    auto visitLiveBlock = [&](SILBasicBlock *liveBB) {
      assert(liveBB != phi.phiBlock && "phi operands are consumed");

      if (occupiedBlocks.contains(liveBB))
        return false;

      if (liveBlocks.insert(liveBB) && liveBB != defBB) {
        liveBBWorklist.push_back(liveBB);
      }
      return true;
    };
    if (!visitLiveBlock(use->getUser()->getParent()))
      return false;

    while (!liveBBWorklist.empty()) {
      auto *succBB = liveBBWorklist.pop_back_val();
      for (auto *predBB : succBB->getPredecessorBlocks()) {
        if (!visitLiveBlock(predBB))
          return false;
      }
    }
  }
  return true;
}
