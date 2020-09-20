//===--- CopyValueOpts.cpp ------------------------------------------------===//
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
/// \file
///
/// Contains optimizations that eliminate redundant copy values.
///
//===----------------------------------------------------------------------===//

#include "OwnershipPhiOperand.h"
#include "SemanticARCOptVisitor.h"

using namespace swift;
using namespace swift::semanticarc;

//===----------------------------------------------------------------------===//
//                     Guaranteed Copy Value Optimization
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
  if (ctx.onlyGuaranteedOpts)
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
      lr.hasUnknownConsumingUse(ctx.assumingAtFixedPoint);
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
        ctx.joinedOwnedIntroducerToConsumedOperands.insert(value,
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

//===----------------------------------------------------------------------===//
//                       Trivial Live Range Elimination
//===----------------------------------------------------------------------===//

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

//===----------------------------------------------------------------------===//
//                             Live Range Joining
//===----------------------------------------------------------------------===//

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

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

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
