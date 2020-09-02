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
//                        Address Written To Analysis
//===----------------------------------------------------------------------===//

/// Returns true if we were able to ascertain that either the initialValue has
/// no write uses or all of the write uses were writes that we could understand.
bool swift::semanticarc::constructCacheValue(
    SILValue initialValue,
    SmallVectorImpl<Operand *> &wellBehavedWriteAccumulator) {
  SmallVector<Operand *, 8> worklist(initialValue->getUses());

  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();
    SILInstruction *user = op->getUser();

    if (Projection::isAddressProjection(user) ||
        isa<ProjectBlockStorageInst>(user)) {
      for (SILValue r : user->getResults()) {
        llvm::copy(r->getUses(), std::back_inserter(worklist));
      }
      continue;
    }

    if (auto *oeai = dyn_cast<OpenExistentialAddrInst>(user)) {
      // Mutable access!
      if (oeai->getAccessKind() != OpenedExistentialAccess::Immutable) {
        wellBehavedWriteAccumulator.push_back(op);
      }

      //  Otherwise, look through it and continue.
      llvm::copy(oeai->getUses(), std::back_inserter(worklist));
      continue;
    }

    if (auto *si = dyn_cast<StoreInst>(user)) {
      // We must be the dest since addresses can not be stored.
      assert(si->getDest() == op->get());
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    // Add any destroy_addrs to the resultAccumulator.
    if (isa<DestroyAddrInst>(user)) {
      wellBehavedWriteAccumulator.push_back(op);
      continue;
    }

    // load_borrow and incidental uses are fine as well.
    if (isa<LoadBorrowInst>(user) || isIncidentalUse(user)) {
      continue;
    }

    // Look through begin_access and mark them/their end_borrow as users.
    if (auto *bai = dyn_cast<BeginAccessInst>(user)) {
      // If we do not have a read, mark this as a write. Also, insert our
      // end_access as well.
      if (bai->getAccessKind() != SILAccessKind::Read) {
        wellBehavedWriteAccumulator.push_back(op);
        transform(bai->getUsersOfType<EndAccessInst>(),
                  std::back_inserter(wellBehavedWriteAccumulator),
                  [](EndAccessInst *eai) { return &eai->getAllOperands()[0]; });
      }

      // And then add the users to the worklist and continue.
      llvm::copy(bai->getUses(), std::back_inserter(worklist));
      continue;
    }

    // If we have a load, we just need to mark the load [take] as a write.
    if (auto *li = dyn_cast<LoadInst>(user)) {
      if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
        wellBehavedWriteAccumulator.push_back(op);
      }
      continue;
    }

    // If we have a FullApplySite, we need to do per convention/inst logic.
    if (auto fas = FullApplySite::isa(user)) {
      // Begin by seeing if we have an in_guaranteed use. If we do, we are done.
      if (fas.getArgumentConvention(*op) ==
          SILArgumentConvention::Indirect_In_Guaranteed) {
        continue;
      }

      // Then see if we have an apply site that is not a coroutine apply
      // site. In such a case, without further analysis, we can treat it like an
      // instantaneous write and validate that it doesn't overlap with our load
      // [copy].
      if (!fas.beginsCoroutineEvaluation() &&
          fas.getArgumentConvention(*op).isInoutConvention()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      // Otherwise, be conservative and return that we had a write that we did
      // not understand.
      LLVM_DEBUG(llvm::dbgs()
                 << "Function: " << user->getFunction()->getName() << "\n");
      LLVM_DEBUG(llvm::dbgs() << "Value: " << op->get());
      LLVM_DEBUG(llvm::dbgs() << "Unhandled apply site!: " << *user);

      return false;
    }

    // Copy addr that read are just loads.
    if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
      // If our value is the destination, this is a write.
      if (cai->getDest() == op->get()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      // Ok, so we are Src by process of elimination. Make sure we are not being
      // taken.
      if (cai->isTakeOfSrc()) {
        wellBehavedWriteAccumulator.push_back(op);
        continue;
      }

      // Otherwise, we are safe and can continue.
      continue;
    }

    // If we did not recognize the user, just return conservatively that it was
    // written to in a way we did not understand.
    LLVM_DEBUG(llvm::dbgs()
               << "Function: " << user->getFunction()->getName() << "\n");
    LLVM_DEBUG(llvm::dbgs() << "Value: " << op->get());
    LLVM_DEBUG(llvm::dbgs() << "Unknown instruction!: " << *user);
    return false;
  }

  // Ok, we finished our worklist and this address is not being written to.
  return true;
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

namespace {

} // end anonymous namespace

static llvm::cl::opt<bool>
    VerifyAfterTransform("sil-semantic-arc-opts-verify-after-transform",
                         llvm::cl::init(false), llvm::cl::Hidden);

static bool canEliminatePhi(
    SemanticARCOptVisitor::FrozenMultiMapRange optimizableIntroducerRange,
    ArrayRef<OwnershipPhiOperand> incomingValueOperandList,
    SmallVectorImpl<OwnedValueIntroducer> &ownedValueIntroducerAccumulator) {
  for (auto incomingValueOperand : incomingValueOperandList) {
    SILValue incomingValue = incomingValueOperand.getValue();

    // Before we do anything, see if we have an incoming value with trivial
    // ownership. This can occur in the case where we are working with enums due
    // to trivial non-payloaded cases. Skip that.
    if (incomingValue.getOwnershipKind() == ValueOwnershipKind::None) {
      continue;
    }

    // Then see if this is an introducer that we actually saw as able to be
    // optimized if we could flip this joined live range.
    //
    // NOTE: If this linear search is too slow, we can change the multimap to
    // sort the mapped to list by pointer instead of insertion order. In such a
    // case, we could then bisect.
    if (llvm::find(optimizableIntroducerRange,
                   incomingValueOperand.getOperand()) ==
        optimizableIntroducerRange.end()) {
      return false;
    }

    // Now that we know it is an owned value that we saw before, check for
    // introducers of the owned value which are the copies that we may be able
    // to eliminate. Since we do not look through joined live ranges, we must
    // only have a single introducer. So look for that one and if not, bail.
    auto singleIntroducer = getSingleOwnedValueIntroducer(incomingValue);
    if (!singleIntroducer.hasValue()) {
      return false;
    }

    // Then make sure that our owned value introducer is able to be converted to
    // guaranteed and that we found it to have a LiveRange that we could have
    // eliminated /if/ we were to get rid of this phi.
    if (!singleIntroducer->isConvertableToGuaranteed()) {
      return false;
    }

    // Otherwise, add the introducer to our result array.
    ownedValueIntroducerAccumulator.push_back(*singleIntroducer);
  }

#ifndef NDEBUG
  // Other parts of the pass ensure that we only add values to the list if their
  // owned value introducer is not used by multiple live ranges. That being
  // said, lets assert that.
  {
    SmallVector<OwnedValueIntroducer, 32> uniqueCheck;
    llvm::copy(ownedValueIntroducerAccumulator,
               std::back_inserter(uniqueCheck));
    sortUnique(uniqueCheck);
    assert(
        uniqueCheck.size() == ownedValueIntroducerAccumulator.size() &&
        "multiple joined live range operands are from the same live range?!");
  }
#endif

  return true;
}

static bool getIncomingJoinedLiveRangeOperands(
    SILValue joinedLiveRange,
    SmallVectorImpl<OwnershipPhiOperand> &resultingOperands) {
  if (auto *phi = dyn_cast<SILPhiArgument>(joinedLiveRange)) {
    return phi->visitIncomingPhiOperands([&](Operand *op) {
      if (auto phiOp = OwnershipPhiOperand::get(op)) {
        resultingOperands.push_back(*phiOp);
        return true;
      }
      return false;
    });
  }

  if (auto *svi = dyn_cast<SingleValueInstruction>(joinedLiveRange)) {
    return llvm::all_of(svi->getAllOperands(), [&](const Operand &op) {
      // skip type dependent operands.
      if (op.isTypeDependent())
        return true;

      auto phiOp = OwnershipPhiOperand::get(&op);
      if (!phiOp)
        return false;
      resultingOperands.push_back(*phiOp);
      return true;
    });
  }

  llvm_unreachable("Unhandled joined live range?!");
}

bool SemanticARCOptVisitor::performPostPeepholeOwnedArgElimination() {
  bool madeChange = false;

  // First freeze our multi-map so we can use it for map queries. Also, setup a
  // defer of the reset so we do not forget to reset the map when we are done.
  joinedOwnedIntroducerToConsumedOperands.setFrozen();
  SWIFT_DEFER { joinedOwnedIntroducerToConsumedOperands.reset(); };

  // Now for each phi argument that we have in our multi-map...
  SmallVector<OwnershipPhiOperand, 4> incomingValueOperandList;
  SmallVector<OwnedValueIntroducer, 4> ownedValueIntroducers;
  for (auto pair : joinedOwnedIntroducerToConsumedOperands.getRange()) {
    SWIFT_DEFER {
      incomingValueOperandList.clear();
      ownedValueIntroducers.clear();
    };

    // First compute the LiveRange for ownershipPhi value. For simplicity, we
    // only handle cases now where the result does not have any additional
    // ownershipPhi uses.
    SILValue joinedIntroducer = pair.first;
    OwnershipLiveRange joinedLiveRange(joinedIntroducer);
    if (bool(joinedLiveRange.hasUnknownConsumingUse())) {
      continue;
    }

    // Ok, we know that our phi argument /could/ be converted to guaranteed if
    // our incoming values are able to be converted to guaranteed. Now for each
    // incoming value, compute the incoming values ownership roots and see if
    // all of the ownership roots are in our owned incoming value array.
    if (!getIncomingJoinedLiveRangeOperands(joinedIntroducer,
                                            incomingValueOperandList)) {
      continue;
    }

    // Grab our list of introducer values paired with this SILArgument. See if
    // all of these introducer values were ones that /could/ have been
    // eliminated if it was not for the given phi. If all of them are, we can
    // optimize!
    {
      auto rawFoundOptimizableIntroducerArray = pair.second;
      if (!canEliminatePhi(rawFoundOptimizableIntroducerArray,
                           incomingValueOperandList, ownedValueIntroducers)) {
        continue;
      }
    }

    // Ok, at this point we know that we can eliminate this phi. First go
    // through the list of incomingValueOperandList and stash the value/set the
    // operand's stored value to undef. We will hook them back up later.
    SmallVector<SILValue, 8> originalIncomingValues;
    for (auto &incomingValueOperand : incomingValueOperandList) {
      originalIncomingValues.push_back(incomingValueOperand.getValue());
      incomingValueOperand.markUndef();
    }

    // Then go through all of our owned value introducers, compute their live
    // ranges, and eliminate them. We know it is safe to remove them from our
    // previous proofs.
    //
    // NOTE: If our introducer is a copy_value that is one of our
    // originalIncomingValues, we need to update the originalIncomingValue array
    // with that value since we are going to delete the copy_value here. This
    // creates a complication since we want to binary_search on
    // originalIncomingValues to detect this same condition! So, we create a
    // list of updates that we apply after we no longer need to perform
    // binary_search, but before we start RAUWing things.
    SmallVector<std::pair<SILValue, unsigned>, 8> incomingValueUpdates;
    for (auto introducer : ownedValueIntroducers) {
      SILValue v = introducer.value;
      OwnershipLiveRange lr(v);

      // For now, we only handle copy_value for simplicity.
      //
      // TODO: Add support for load [copy].
      if (introducer.kind == OwnedValueIntroducerKind::Copy) {
        auto *cvi = cast<CopyValueInst>(v);
        // Before we convert from owned to guaranteed, we need to first see if
        // cvi is one of our originalIncomingValues. If so, we need to set
        // originalIncomingValues to be cvi->getOperand(). Otherwise, weirdness
        // results since we are deleting one of our stashed values.
        auto iter = find(originalIncomingValues, cvi);
        if (iter != originalIncomingValues.end()) {
          // We use an auxillary array here so we can continue to bisect on
          // original incoming values. Once we are done processing here, we will
          // not need that property anymore.
          unsigned updateOffset =
              std::distance(originalIncomingValues.begin(), iter);
          incomingValueUpdates.emplace_back(cvi->getOperand(), updateOffset);
        }
        std::move(lr).convertToGuaranteedAndRAUW(cvi->getOperand(),
                                                 getCallbacks());
        continue;
      }
      llvm_unreachable("Unhandled optimizable introducer!");
    }

    // Now go through and update our original incoming value array now that we
    // do not need it to be sorted for bisection purposes.
    while (!incomingValueUpdates.empty()) {
      auto pair = incomingValueUpdates.pop_back_val();
      originalIncomingValues[pair.second] = pair.first;
    }

    // Then convert the phi's live range to be guaranteed.
    std::move(joinedLiveRange)
        .convertJoinedLiveRangePhiToGuaranteed(
            getDeadEndBlocks(), lifetimeFrontier, getCallbacks());

    // Now if our phi operand consumes/forwards its guaranteed input, insert a
    // begin_borrow along the incoming value edges. We have to do this after
    // converting the incoming values to be guaranteed to avoid tripping
    // SILBuilder checks around simple ownership invariants (namely that def/use
    // line up) when creating instructions.
    assert(incomingValueOperandList.size() == originalIncomingValues.size());
    while (!incomingValueOperandList.empty()) {
      auto incomingValueOperand = incomingValueOperandList.pop_back_val();
      SILValue originalValue = originalIncomingValues.pop_back_val();
      if (incomingValueOperand.isGuaranteedConsuming() &&
          originalValue.getOwnershipKind() != ValueOwnershipKind::None) {
        auto loc = RegularLocation::getAutoGeneratedLocation();
        SILBuilderWithScope builder(incomingValueOperand.getInst());
        originalValue = builder.createBeginBorrow(loc, originalValue);
      }
      incomingValueOperand.getOperand()->set(originalValue);
    }

    madeChange = true;
    if (VerifyAfterTransform) {
      F.verify();
    }
  }

  return madeChange;
}

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
//                     Redundant Borrow Scope Elimination
//===----------------------------------------------------------------------===//

bool SemanticARCOptVisitor::visitBeginBorrowInst(BeginBorrowInst *bbi) {
  auto kind = bbi->getOperand().getOwnershipKind();
  SmallVector<EndBorrowInst *, 16> endBorrows;
  for (auto *op : bbi->getUses()) {
    if (!op->isConsumingUse()) {
      // Make sure that this operand can accept our arguments kind.
      auto map = op->getOwnershipKindMap();
      if (map.canAcceptKind(kind))
        continue;
      return false;
    }

    // Otherwise, this borrow is being consumed. See if our consuming inst is an
    // end_borrow. If it isn't, then return false, this scope is
    // needed. Otherwise, add the end_borrow to our list of end borrows.
    auto *ebi = dyn_cast<EndBorrowInst>(op->getUser());
    if (!ebi) {
      return false;
    }
    endBorrows.push_back(ebi);
  }

  // At this point, we know that the begin_borrow's operand can be
  // used as an argument to all non-end borrow uses. Eliminate the
  // begin borrow and end borrows.
  while (!endBorrows.empty()) {
    auto *ebi = endBorrows.pop_back_val();
    eraseInstruction(ebi);
  }

  eraseAndRAUWSingleValueInstruction(bbi, bbi->getOperand());
  return true;
}

//===----------------------------------------------------------------------===//
//                    CopyValue Optimizations Elimination
//===----------------------------------------------------------------------===//

// Eliminate a copy of a borrowed value, if:
//
// 1. All of the copies users do not consume the copy (and thus can accept a
//    borrowed value instead).
// 2. The copies's non-destroy_value users are strictly contained within the
//    scope of the borrowed value.
//
// Example:
//
//   %0 = @guaranteed (argument or instruction)
//   %1 = copy_value %0
//   apply %f(%1) : $@convention(thin) (@guaranteed ...) ...
//   other_non_consuming_use %1
//   destroy_value %1
//   end_borrow %0 (if an instruction)
//
// =>
//
//   %0 = @guaranteed (argument or instruction)
//   apply %f(%0) : $@convention(thin) (@guaranteed ...) ...
//   other_non_consuming_use %0
//   end_borrow %0 (if an instruction)
//
// NOTE: This means that the destroy_value technically can be after the
// end_borrow. In practice, this will not be the case but we use this to avoid
// having to reason about the ordering of the end_borrow and destroy_value.
//
// NOTE: Today we only perform this for guaranteed parameters since this enables
// us to avoid doing the linear lifetime check to make sure that all destroys
// are within the borrow scope.
//
// TODO: This needs a better name.
bool SemanticARCOptVisitor::performGuaranteedCopyValueOptimization(
    CopyValueInst *cvi) {
  // For now, do not run this optimization. This is just to be careful.
  if (onlyGuaranteedOpts)
    return false;

  SmallVector<BorrowedValue, 4> borrowScopeIntroducers;

  // Find all borrow introducers for our copy operand. If we are unable to find
  // all of the reproducers (due to pattern matching failure), conservatively
  // return false. We can not optimize.
  //
  // NOTE: We can get multiple introducers if our copy_value's operand
  // value runs through a phi or an aggregate forming instruction.
  if (!getAllBorrowIntroducingValues(cvi->getOperand(), borrowScopeIntroducers))
    return false;

  // Then go over all of our uses and see if the value returned by our copy
  // value forms a dead live range or a live range that would be dead if it was
  // not consumed by phi nodes. If we do not have such a live range, there must
  // be some consuming use that we either do not understand is /actually/
  // forwarding or a user that truly represents a necessary consume of the value
  // (e.x. storing into memory).
  OwnershipLiveRange lr(cvi);
  auto hasUnknownConsumingUseState =
      lr.hasUnknownConsumingUse(assumingAtFixedPoint);
  if (hasUnknownConsumingUseState ==
      OwnershipLiveRange::HasConsumingUse_t::Yes) {
    return false;
  }

  // Next check if we do not have any destroys of our copy_value and are
  // processing a local borrow scope. In such a case, due to the way we ignore
  // dead end blocks, we may eliminate the copy_value, creating a use of the
  // borrowed value after the end_borrow. To avoid this, in such cases we
  // bail. In contrast, a non-local borrow scope does not have any end scope
  // instructions, implying we can avoid this hazard and still optimize in such
  // a case.
  //
  // DISCUSSION: Consider the following SIL:
  //
  // ```
  //   %1 = begin_borrow %0 : $KlassPair                            (1)
  //   %2 = struct_extract %1 : $KlassPair, #KlassPair.firstKlass
  //   %3 = copy_value %2 : $Klass
  //   ...
  //   end_borrow %1 : $LintCommand                                 (2)
  //   cond_br ..., bb1, bb2
  //
  //   ...
  //
  //   bbN:
  //     // Never return type implies dead end block.
  //     apply %f(%3) : $@convention(thin) (@guaranteed Klass) -> Never (3)
  //     unreachable
  // ```
  //
  // For simplicity, note that if bbN post-dominates %3, given that when we
  // compute linear lifetime errors we ignore dead end blocks, we would not
  // register that the copy_values only use is outside of the begin_borrow
  // region defined by (1), (2) and thus would eliminate the copy. This would
  // result in %2 being used by %f, causing the linear lifetime checker to
  // error.
  //
  // Naively one may assume that the solution to this is to just check if %3 has
  // /any/ destroy_values at all and if it doesn't have any reachable
  // destroy_values, then we are in this case. But is this correct in
  // general. We prove this below:
  //
  // The only paths along which the copy_value can not be destroyed or consumed
  // is along paths to dead end blocks. Trivially, we know that such a dead end
  // block, can not be reachable from the end_borrow since by their nature dead
  // end blocks end in unreachables.
  //
  // So we know that we can only run into this bug if we have a dead end block
  // reachable from the end_borrow, meaning that the bug can not occur if we
  // branch before the end_borrow since in that case, the borrow scope would
  // last over the dead end block's no return meaning that we will not use the
  // borrowed value after its lifetime is ended by the end_borrow.
  //
  // With that in hand, we note again that if we have exactly one consumed,
  // destroy_value /after/ the end_borrow we will not optimize here. This means
  // that this bug can only occur if the copy_value is only post-dominated by
  // dead end blocks that use the value in a non-consuming way.
  //
  // TODO: There may be some way of sinking this into the loop below.
  bool haveAnyLocalScopes =
      llvm::any_of(borrowScopeIntroducers, [](BorrowedValue borrowScope) {
        return borrowScope.isLocalScope();
      });

  auto destroys = lr.getDestroyingUses();
  if (destroys.empty() && haveAnyLocalScopes) {
    return false;
  }

  // If we reached this point, then we know that all of our users can accept a
  // guaranteed value and our owned value is destroyed only by a set of
  // destroy_values. Check if:
  //
  // 1. All of our destroys are joint post-dominated by our end borrow scope
  //    set. If they do not, then the copy_value is lifetime extending the
  //    guaranteed value, we can not eliminate it.
  //
  // 2. If all of our destroy_values are dead end. In such a case, the linear
  //    lifetime checker will not perform any checks since it assumes that dead
  //    end destroys can be ignored. Since we are going to end the program
  //    anyways, we want to be conservative here and optimize only if we do not
  //    need to insert an end_borrow since all of our borrow introducers are
  //    non-local scopes.
  {
    bool foundNonDeadEnd = false;
    for (auto *d : destroys) {
      foundNonDeadEnd |= !getDeadEndBlocks().isDeadEnd(d->getParentBlock());
    }
    if (!foundNonDeadEnd && haveAnyLocalScopes)
      return false;
    SmallVector<Operand *, 8> scratchSpace;
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    if (llvm::any_of(borrowScopeIntroducers, [&](BorrowedValue borrowScope) {
          return !borrowScope.areUsesWithinScope(lr.getAllConsumingUses(),
                                                 scratchSpace, visitedBlocks,
                                                 getDeadEndBlocks());
        })) {
      return false;
    }
  }

  // Otherwise, we know that our copy_value/destroy_values are all completely
  // within the guaranteed value scope. So we /could/ optimize it. Now check if
  // we were truly dead or if we are dead if we can eliminate phi arg uses. If
  // we need to handle the phi arg uses, we bail. After we reach a fixed point,
  // we will try to eliminate this value then if we can find a complete set of
  // all incoming values to our phi argument.
  if (hasUnknownConsumingUseState ==
      OwnershipLiveRange::HasConsumingUse_t::YesButAllPhiArgs) {
    auto opPhi = *OwnershipPhiOperand::get(lr.getSingleUnknownConsumingUse());
    SmallVector<Operand *, 8> scratchSpace;
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;

    bool canOptimizePhi = opPhi.visitResults([&](SILValue value) {
      SWIFT_DEFER {
        scratchSpace.clear();
        visitedBlocks.clear();
      };

      OwnershipLiveRange phiArgLR(value);
      if (bool(phiArgLR.hasUnknownConsumingUse())) {
        return false;
      }

      if (llvm::any_of(borrowScopeIntroducers, [&](BorrowedValue borrowScope) {
            return !borrowScope.areUsesWithinScope(
                phiArgLR.getAllConsumingUses(), scratchSpace, visitedBlocks,
                getDeadEndBlocks());
          })) {
        return false;
      }

      return true;
    });

    if (canOptimizePhi) {
      opPhi.visitResults([&](SILValue value) {
        joinedOwnedIntroducerToConsumedOperands.insert(value,
                                                       opPhi.getOperand());
        return true;
      });
    }

    return false;
  }

  // Otherwise, our copy must truly not be needed, o RAUW and convert to
  // guaranteed!
  std::move(lr).convertToGuaranteedAndRAUW(cvi->getOperand(), getCallbacks());
  return true;
}

/// If cvi only has destroy value users, then cvi is a dead live range. Lets
/// eliminate all such dead live ranges.
bool SemanticARCOptVisitor::eliminateDeadLiveRangeCopyValue(
    CopyValueInst *cvi) {
  // This is a cheap optimization generally.

  // See if we are lucky and have a simple case.
  if (auto *op = cvi->getSingleUse()) {
    if (auto *dvi = dyn_cast<DestroyValueInst>(op->getUser())) {
      eraseInstruction(dvi);
      eraseInstructionAndAddOperandsToWorklist(cvi);
      return true;
    }
  }

  // If all of our copy_value users are destroy_value, zap all of the
  // instructions. We begin by performing that check and gathering up our
  // destroy_value.
  SmallVector<DestroyValueInst *, 16> destroys;
  if (!all_of(cvi->getUses(), [&](Operand *op) {
        auto *dvi = dyn_cast<DestroyValueInst>(op->getUser());
        if (!dvi)
          return false;

        // Stash dvi in destroys so we can easily eliminate it later.
        destroys.push_back(dvi);
        return true;
      })) {
    return false;
  }

  // Now that we have a truly dead live range copy value, eliminate it!
  while (!destroys.empty()) {
    eraseInstruction(destroys.pop_back_val());
  }
  eraseInstructionAndAddOperandsToWorklist(cvi);
  return true;
}

// Handle simple checking where we do not need to form live ranges and visit a
// bunch of instructions.
static bool canSafelyJoinSimpleRange(SILValue cviOperand,
                                     DestroyValueInst *cviOperandDestroy,
                                     CopyValueInst *cvi) {
  // We only handle cases where our copy_value has a single consuming use that
  // is not a forwarding use. We need to use the LiveRange functionality to
  // guarantee correctness in the presence of forwarding uses.
  //
  // NOTE: This use may be any type of consuming use and may not be a
  // destroy_value.
  auto *cviConsumer = cvi->getSingleConsumingUse();
  if (!cviConsumer || isOwnedForwardingInstruction(cviConsumer->getUser())) {
    return false;
  }

  // Ok, we may be able to eliminate this. The main thing we need to be careful
  // of here is that if the destroy_value is /after/ the consuming use of the
  // operand of copy_value, we may have normal uses of the copy_value's operand
  // that would become use-after-frees since we would be shrinking the lifetime
  // of the object potentially. Consider the following SIL:
  //
  //   %0 = ...
  //   %1 = copy_value %0
  //   apply %cviConsumer(%1)
  //   apply %guaranteedUser(%0)
  //   destroy_value %0
  //
  // Easily, if we were to eliminate the copy_value, destroy_value, the object's
  // lifetime could potentially be shrunk before guaranteedUser is executed,
  // causing guaranteedUser to be a use-after-free.
  //
  // As an extra wrinkle, until all interior pointer constructs (e.x.:
  // project_box) are guaranteed to be guaranted by a begin_borrow, we can not
  // in general safely shrink lifetimes. So even if we think we can prove that
  // all non-consuming uses of %0 are before apply %cviConsumer, we may miss
  // implicit uses that are not guarded yet by a begin_borrow, resulting in
  // use-after-frees.
  //
  // With that in mind, we only handle cases today where we can prove that
  // destroy_value is strictly before the consuming use of the operand. This
  // guarantees that we are not shrinking the lifetime of the underlying object.
  //
  // First we handle the simple case: where the cviConsumer is a return inst. In
  // such a case, we know for sure that cviConsumer post-dominates the
  // destroy_value.
  auto cviConsumerIter = cviConsumer->getUser()->getIterator();
  if (isa<ReturnInst>(cviConsumerIter)) {
    return true;
  }

  // Then see if our cviConsumer is in the same block as a return inst and the
  // destroy_value is not. In that case, we know that the cviConsumer must
  // post-dominate the destroy_value.
  auto *cviConsumingBlock = cviConsumerIter->getParent();
  if (isa<ReturnInst>(cviConsumingBlock->getTerminator()) &&
      cviConsumingBlock != cviOperandDestroy->getParent()) {
    return true;
  }

  // Otherwise, we only support joining live ranges where the cvi and the cvi's
  // operand's destroy are in the same block with the destroy_value of cvi
  // operand needing to be strictly after the copy_value. This analysis can be
  // made significantly stronger by using LiveRanges, but this is simple for
  // now.
  auto cviOperandDestroyIter = cviOperandDestroy->getIterator();
  if (cviConsumingBlock != cviOperandDestroyIter->getParent()) {
    return false;
  }

  // TODO: This should really be llvm::find, but for some reason, the templates
  // do not match up given the current state of the iterators. This impl works
  // in a pinch though.
  return llvm::any_of(
      llvm::make_range(cviOperandDestroyIter,
                       cviOperandDestroyIter->getParent()->end()),
      [&](const SILInstruction &val) { return &*cviConsumerIter == &val; });
}

// # The Problem We Are Solving
//
// The main idea here is that we are trying to eliminate the simplest, easiest
// form of live range joining. Consider the following SIL:
//
//   ```
//   %cviOperand = ...                // @owned value
//   %cvi = copy_value %cviOperand    // copy of @owned value
//   ...
//   destroy_value %cviOperandDestroy // destruction of @owned value
//   ...
//   apply %consumingUser(%cvi)       // destruction of copy of @owned value
//   ```
//
// We want to reduce reference count traffic by eliminating the middle
// copy/destroy yielding:
//
//   ```
//   %cviOperand = ...                // @owned value
//   // *eliminated copy_value*
//   ...
//   // *eliminated destroy_value*
//   ...
//   apply %consumingUser(%cviOperand)       // destruction of copy of @owned
//   value
//   ```
//
// # Safety
//
// In order to do this safely, we need to take the union of the two objects
// lifetimes since we are only joining lifetimes. This ensures that we can rely
// on SILGen's correctness on inserting safe lifetimes. To keep this simple
// today we only optimize if the destroy_value and consuming user are in the
// same block and the consuming user is later in the block than the
// destroy_value.
//
// DISCUSSION: The reason why we do not shrink lifetimes today is that all
// interior pointers (e.x. project_box) are properly guarded by
// begin_borrow. Because of that we can not shrink lifetimes and instead rely on
// SILGen's correctness.
bool SemanticARCOptVisitor::tryJoiningCopyValueLiveRangeWithOperand(
    CopyValueInst *cvi) {
  // First do a quick check if our operand is owned. If it is not owned, we can
  // not join live ranges.
  SILValue operand = cvi->getOperand();
  if (operand.getOwnershipKind() != ValueOwnershipKind::Owned) {
    return false;
  }

  // Then check if our operand has a single destroy_value. If it does and that
  // destroy_value is strictly before the consumer of our copy_value in the same
  // block as the consumer of said copy_value then we can always join the live
  // ranges.
  //
  // Example:
  //
  //   ```
  //   %1 = copy_value %0
  //   ...
  //   destroy_value %0
  //   apply %consumingUser(%1)
  //   ```
  // ->
  //
  //   ```
  //   apply %consumingUser(%0)
  //   ```
  //
  // DISCUSSION: We need to ensure that the consuming use of the copy_value is
  // strictly after the destroy_value to ensure that we do not shrink the live
  // range of the operand if the operand has any normal uses beyond our copy
  // value. Otherwise, we could have normal uses /after/ the consuming use of
  // our copy_value.
  if (auto *dvi = operand->getSingleConsumingUserOfType<DestroyValueInst>()) {
    if (canSafelyJoinSimpleRange(operand, dvi, cvi)) {
      eraseInstruction(dvi);
      eraseAndRAUWSingleValueInstruction(cvi, operand);
      return true;
    }
  }

  // Otherwise, we couldn't handle this case, so return false.
  //
  // NOTE: We would generally do a more complex analysis here to handle the more
  // general case. That would most likely /not/ be a guaranteed optimization
  // until we investigate/measure.
  return false;
}

bool SemanticARCOptVisitor::visitCopyValueInst(CopyValueInst *cvi) {
  // If our copy value inst has only destroy_value users, it is a dead live
  // range. Try to eliminate them.
  if (eliminateDeadLiveRangeCopyValue(cvi)) {
    return true;
  }

  // Then see if copy_value operand's lifetime ends after our copy_value via a
  // destroy_value. If so, we can join their lifetimes.
  if (tryJoiningCopyValueLiveRangeWithOperand(cvi)) {
    return true;
  }

  // Then try to perform the guaranteed copy value optimization.
  if (performGuaranteedCopyValueOptimization(cvi)) {
    return true;
  }

  return false;
}

//===----------------------------------------------------------------------===//
//                         load [copy] Optimizations
//===----------------------------------------------------------------------===//

namespace {

/// A class that computes in a flow insensitive way if we can prove that our
/// storage is either never written to, or is initialized exactly once and never
/// written to again. In both cases, we can convert load [copy] -> load_borrow
/// safely.
class StorageGuaranteesLoadVisitor
    : public AccessUseDefChainVisitor<StorageGuaranteesLoadVisitor> {
  // The outer SemanticARCOptVisitor.
  SemanticARCOptVisitor &ARCOpt;

  // The live range of the original load.
  const OwnershipLiveRange &liveRange;

  // The current address being visited.
  SILValue currentAddress;

  Optional<bool> isWritten;

public:
  StorageGuaranteesLoadVisitor(SemanticARCOptVisitor &arcOpt, LoadInst *load,
                               const OwnershipLiveRange &liveRange)
      : ARCOpt(arcOpt), liveRange(liveRange),
        currentAddress(load->getOperand()) {}

  void answer(bool written) {
    currentAddress = nullptr;
    isWritten = written;
  }

  void next(SILValue address) { currentAddress = address; }

  void visitNestedAccess(BeginAccessInst *access) {
    // First see if we have read/modify. If we do not, just look through the
    // nested access.
    switch (access->getAccessKind()) {
    case SILAccessKind::Init:
    case SILAccessKind::Deinit:
      return next(access->getOperand());
    case SILAccessKind::Read:
    case SILAccessKind::Modify:
      break;
    }

    // Next check if our live range is completely in the begin/end access
    // scope. If so, we may be able to use a load_borrow here!
    SmallVector<Operand *, 8> endScopeUses;
    transform(access->getEndAccesses(), std::back_inserter(endScopeUses),
              [](EndAccessInst *eai) { return &eai->getAllOperands()[0]; });
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
    if (!checker.validateLifetime(access, endScopeUses,
                                  liveRange.getAllConsumingUses())) {
      // If we fail the linear lifetime check, then just recur:
      return next(access->getOperand());
    }

    // Otherwise, if we have read, then we are done!
    if (access->getAccessKind() == SILAccessKind::Read) {
      return answer(false);
    }

    // If we have a modify, check if our value is /ever/ written to. If it is
    // never actually written to, then we convert to a load_borrow.
    auto result = ARCOpt.addressToExhaustiveWriteListCache.get(access);
    if (!result.hasValue()) {
      return answer(true);
    }

    if (result.getValue().empty()) {
      return answer(false);
    }

    return answer(true);
  }

  void visitArgumentAccess(SILFunctionArgument *arg) {
    // If this load_copy is from an indirect in_guaranteed argument, then we
    // know for sure that it will never be written to.
    if (arg->hasConvention(SILArgumentConvention::Indirect_In_Guaranteed)) {
      return answer(false);
    }

    // If we have an inout parameter that isn't ever actually written to, return
    // false.
    if (arg->getKnownParameterInfo().isIndirectMutating()) {
      auto wellBehavedWrites =
          ARCOpt.addressToExhaustiveWriteListCache.get(arg);
      if (!wellBehavedWrites.hasValue()) {
        return answer(true);
      }

      // No writes.
      if (wellBehavedWrites->empty()) {
        return answer(false);
      }

      // Ok, we have some writes. See if any of them are within our live
      // range. If any are, we definitely can not promote to load_borrow.
      SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
      SmallVector<BeginAccessInst *, 16> foundBeginAccess;
      LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
      SILValue introducerValue = liveRange.getIntroducer().value;
      if (!checker.usesNotContainedWithinLifetime(introducerValue,
                                                  liveRange.getDestroyingUses(),
                                                  *wellBehavedWrites)) {
        return answer(true);
      }

      // Finally, check if our live range is strictly contained within any of
      // our scoped writes.
      SmallVector<Operand *, 16> endAccessList;
      for (Operand *use : *wellBehavedWrites) {
        auto *bai = dyn_cast<BeginAccessInst>(use->getUser());
        if (!bai) {
          continue;
        }

        endAccessList.clear();
        llvm::transform(
            bai->getUsersOfType<EndAccessInst>(),
            std::back_inserter(endAccessList),
            [](EndAccessInst *eai) { return &eai->getAllOperands()[0]; });
        visitedBlocks.clear();

        // We know that our live range is based on a load [copy], so we know
        // that our value must have a defining inst.
        auto *definingInst =
            cast<LoadInst>(introducerValue->getDefiningInstruction());

        // Then if our defining inst is not in our bai, endAccessList region, we
        // know that the two ranges must be disjoint, so continue.
        if (!checker.validateLifetime(bai, endAccessList,
                                      &definingInst->getAllOperands()[0])) {
          continue;
        }

        // Otherwise, we do have an overlap, return true.
        return answer(true);
      }

      // Otherwise, there isn't an overlap, so we don't write to it.
      return answer(false);
    }

    // TODO: This should be extended:
    //
    // 1. We should be able to analyze in arguments and see if they are only
    //    ever destroyed at the end of the function. In such a case, we may be
    //    able to also to promote load [copy] from such args to load_borrow.
    return answer(true);
  }

  void visitGlobalAccess(SILValue global) {
    return answer(!AccessedStorage(global, AccessedStorage::Global)
                       .isLetAccess(&ARCOpt.F));
  }

  void visitClassAccess(RefElementAddrInst *field) {
    currentAddress = nullptr;

    // We know a let property won't be written to if the base object is
    // guaranteed for the duration of the access.
    // For non-let properties conservatively assume they may be written to.
    if (!field->getField()->isLet()) {
      return answer(true);
    }

    // The lifetime of the `let` is guaranteed if it's dominated by the
    // guarantee on the base. See if we can find a single borrow introducer for
    // this object. If we could not find a single such borrow introducer, assume
    // that our property is conservatively written to.
    SILValue baseObject = field->getOperand();
    auto value = getSingleBorrowIntroducingValue(baseObject);
    if (!value) {
      return answer(true);
    }

    // Ok, we have a single borrow introducing value. First do a quick check if
    // we have a non-local scope that is a function argument. In such a case, we
    // know statically that our let can not be written to in the current
    // function. To be conservative, assume that all other non-local scopes
    // write to memory.
    if (!value->isLocalScope()) {
      if (value->kind == BorrowedValueKind::SILFunctionArgument) {
        return answer(false);
      }

      // TODO: Once we model Coroutine results as non-local scopes, we should be
      // able to return false here for them as well.
      return answer(true);
    }

    // TODO: This is disabled temporarily for guaranteed phi args just for
    // staging purposes. Thus be conservative and assume true in these cases.
    if (value->kind == BorrowedValueKind::Phi) {
      return answer(true);
    }

    // Ok, we now know that we have a local scope whose lifetime we need to
    // analyze. With that in mind, gather up the lifetime ending uses of our
    // borrow scope introducing value and then use the linear lifetime checker
    // to check whether the copied value is dominated by the lifetime of the
    // borrow it's based on.
    SmallVector<Operand *, 4> endScopeInsts;
    value->visitLocalScopeEndingUses(
        [&](Operand *use) { endScopeInsts.push_back(use); });

    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());

    // Returns true on success. So we invert.
    bool foundError = !checker.validateLifetime(
        baseObject, endScopeInsts, liveRange.getAllConsumingUses());
    return answer(foundError);
  }

  // TODO: Handle other access kinds?
  void visitBase(SILValue base, AccessedStorage::Kind kind) {
    return answer(true);
  }

  void visitNonAccess(SILValue addr) { return answer(true); }

  void visitCast(SingleValueInstruction *cast, Operand *parentAddr) {
    return next(parentAddr->get());
  }

  void visitPathComponent(SingleValueInstruction *projectedAddr,
                          Operand *parentAddr) {
    return next(parentAddr->get());
  }

  void visitPhi(SILPhiArgument *phi) {
    // We shouldn't have address phis in OSSA SIL, so we don't need to recur
    // through the predecessors here.
    return answer(true);
  }

  /// See if we have an alloc_stack that is only written to once by an
  /// initializing instruction.
  void visitStackAccess(AllocStackInst *stack) {
    SmallVector<Operand *, 8> destroyAddrOperands;
    bool initialAnswer = isSingleInitAllocStack(stack, destroyAddrOperands);
    if (!initialAnswer)
      return answer(true);

    // Then make sure that all of our load [copy] uses are within the
    // destroy_addr.
    SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
    LinearLifetimeChecker checker(visitedBlocks, ARCOpt.getDeadEndBlocks());
    // Returns true on success. So we invert.
    bool foundError = !checker.validateLifetime(
        stack, destroyAddrOperands /*consuming users*/,
        liveRange.getAllConsumingUses() /*non consuming users*/);
    return answer(foundError);
  }

  bool doIt() {
    while (currentAddress) {
      visit(currentAddress);
    }
    return *isWritten;
  }
};

} // namespace

bool SemanticARCOptVisitor::isWrittenTo(LoadInst *load,
                                        const OwnershipLiveRange &lr) {
  StorageGuaranteesLoadVisitor visitor(*this, load, lr);
  return visitor.doIt();
}

// Convert a load [copy] from unique storage [read] that has all uses that can
// accept a guaranteed parameter to a load_borrow.
bool SemanticARCOptVisitor::visitLoadInst(LoadInst *li) {
  // This optimization can use more complex analysis. We should do some
  // experiments before enabling this by default as a guaranteed optimization.
  if (onlyGuaranteedOpts)
    return false;

  if (li->getOwnershipQualifier() != LoadOwnershipQualifier::Copy)
    return false;

  // Ok, we have our load [copy]. Make sure its value is truly a dead live range
  // implying it is only ever consumed by destroy_value instructions. If it is
  // consumed, we need to pass off a +1 value, so bail.
  //
  // FIXME: We should consider if it is worth promoting a load [copy]
  // -> load_borrow if we can put a copy_value on a cold path and thus
  // eliminate RR traffic on a hot path.
  OwnershipLiveRange lr(li);
  if (bool(lr.hasUnknownConsumingUse()))
    return false;

  // Then check if our address is ever written to. If it is, then we cannot use
  // the load_borrow because the stored value may be released during the loaded
  // value's live range.
  if (isWrittenTo(li, lr))
    return false;

  // Ok, we can perform our optimization. Convert the load [copy] into a
  // load_borrow.
  auto *lbi =
      SILBuilderWithScope(li).createLoadBorrow(li->getLoc(), li->getOperand());

  lr.insertEndBorrowsAtDestroys(lbi, getDeadEndBlocks(), lifetimeFrontier);
  std::move(lr).convertToGuaranteedAndRAUW(lbi, getCallbacks());
  return true;
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
