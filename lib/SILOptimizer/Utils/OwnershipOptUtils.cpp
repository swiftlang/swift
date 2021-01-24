//===--- OwnershipOptUtils.cpp --------------------------------------------===//
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
/// Ownership Utilities that rely on SILOptimizer functionality.
///
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"

#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                          Utility Helper Functions
//===----------------------------------------------------------------------===//

static void cleanupOperandsBeforeDeletion(SILInstruction *oldValue,
                                          InstModCallbacks &callbacks) {
  SILBuilderWithScope builder(oldValue);
  for (auto &op : oldValue->getAllOperands()) {
    if (!op.isLifetimeEnding()) {
      continue;
    }

    switch (op.get().getOwnershipKind()) {
    case OwnershipKind::Any:
      llvm_unreachable("Invalid ownership for value");
    case OwnershipKind::Owned: {
      auto *dvi = builder.createDestroyValue(oldValue->getLoc(), op.get());
      callbacks.createdNewInst(dvi);
      continue;
    }
    case OwnershipKind::Guaranteed: {
      // Should only happen once we model destructures as true reborrows.
      auto *ebi = builder.createEndBorrow(oldValue->getLoc(), op.get());
      callbacks.createdNewInst(ebi);
      continue;
    }
    case OwnershipKind::None:
      continue;
    case OwnershipKind::Unowned:
      llvm_unreachable("Unowned object can never be consumed?!");
    }
    llvm_unreachable("Covered switch isn't covered");
  }
}

static SILPhiArgument *
insertOwnedBaseValueAlongBranchEdge(BranchInst *bi, SILValue innerCopy,
                                    InstModCallbacks &callbacks) {
  auto *destBB = bi->getDestBB();
  // We need to create the phi argument before calling addNewEdgeValueToBranch
  // since it checks that the destination block has enough arguments for the
  // argument.
  auto *phiArg =
      destBB->createPhiArgument(innerCopy->getType(), OwnershipKind::Owned);
  addNewEdgeValueToBranch(bi, destBB, innerCopy, callbacks);

  // Grab our predecessor blocks, ignoring us, add to the branch edge an
  // undef corresponding to our value.
  //
  // We gather all predecessor blocks in a separate array to avoid
  // iterator invalidation issues as we mess with terminators.
  SmallVector<SILBasicBlock *, 8> predecessorBlocks(
      destBB->getPredecessorBlocks());

  for (auto *predBlock : predecessorBlocks) {
    if (predBlock == innerCopy->getParentBlock())
      continue;
    addNewEdgeValueToBranch(
        predBlock->getTerminator(), destBB,
        SILUndef::get(innerCopy->getType(), *destBB->getParent()), callbacks);
  }

  return phiArg;
}

static bool findTransitiveBorrowedUses(
  SILValue value, SmallVectorImpl<Operand *> &usePoints,
  SmallVectorImpl<BorrowingOperand> &reborrowPoints) {
  assert(value.getOwnershipKind() == OwnershipKind::Guaranteed);

  unsigned firstOffset = usePoints.size();
  for (Operand *use : value->getUses()) {
    if (use->getOperandOwnership() != OperandOwnership::NonUse)
      usePoints.push_back(use);
  }

  // NOTE: Use points resizes in this loop so usePoints.size() may be
  // different every time.
  for (unsigned i = firstOffset; i < usePoints.size(); ++i) {
    Operand *use = usePoints[i];
    switch (use->getOperandOwnership()) {
    case OperandOwnership::NonUse:
    case OperandOwnership::TrivialUse:
    case OperandOwnership::ForwardingConsume:
    case OperandOwnership::DestroyingConsume:
      llvm_unreachable("this operand cannot handle a guaranteed use");

    case OperandOwnership::ForwardingUnowned:
    case OperandOwnership::PointerEscape:
      return false;

    case OperandOwnership::InstantaneousUse:
    case OperandOwnership::UnownedInstantaneousUse:
    case OperandOwnership::BitwiseEscape:
    case OperandOwnership::EndBorrow:
    case OperandOwnership::Reborrow:
      break;

    case OperandOwnership::InteriorPointer:
      // If our base guaranteed value does not have any consuming uses (consider
      // function arguments), we need to be sure to include interior pointer
      // operands since we may not get a use from a end_scope instruction.
      if (!InteriorPointerOperand(use).findTransitiveUses(usePoints)) {
        return false;
      }
      break;

    case OperandOwnership::ForwardingBorrow:
      ForwardingOperand(use).visitForwardedValues(
        [&](SILValue transitiveValue) {
          // Do not include transitive uses with 'none' ownership
          if (transitiveValue.getOwnershipKind() == OwnershipKind::None)
            return true;
          for (auto *transitiveUse : transitiveValue->getUses()) {
            if (transitiveUse->getOperandOwnership()
                != OperandOwnership::NonUse) {
              usePoints.push_back(transitiveUse);
            }
          }
          return true;
        });
      break;

    case OperandOwnership::Borrow:
      // Try to grab additional end scope instructions to find more liveness
      // info. Stash any reborrow uses so that we can eliminate the reborrow
      // before we are done processing.
      BorrowingOperand(use).visitLocalEndScopeUses(
        [&](Operand *scopeEndingUse) {
          if (auto scopeEndingBorrowingOp = BorrowingOperand(scopeEndingUse)) {
            if (scopeEndingBorrowingOp.isReborrow()) {
              reborrowPoints.push_back(scopeEndingUse);
              return true;
            }
          }
          usePoints.push_back(scopeEndingUse);
          return true;
        });
    }
  }
  return true;
}

// Determine whether it is valid to replace \p oldValue with \p newValue by
// directly checking ownership requirements. This does not determine whether the
// scope of the newValue can be fully extended.
static bool hasValidRAUWOwnership(SILValue oldValue, SILValue newValue) {
  auto newOwnershipKind = newValue.getOwnershipKind();

  // If our new kind is ValueOwnershipKind::None, then we are fine. We
  // trivially support that. This check also ensures that we can always
  // replace any value with a ValueOwnershipKind::None value.
  if (newOwnershipKind == OwnershipKind::None)
    return true;

  // If our old ownership kind is ValueOwnershipKind::None and our new kind is
  // not, we may need to do more work that has not been implemented yet. So
  // bail.
  //
  // Due to our requirement that types line up, this can only occur given a
  // non-trivial typed value with None ownership. This can only happen when
  // oldValue is a trivial payloaded or no-payload non-trivially typed
  // enum. That doesn't occur that often so we just bail on it today until we
  // implement this functionality.
  if (oldValue.getOwnershipKind() == OwnershipKind::None)
    return false;

  // First check if oldValue is SILUndef. If it is, then we know that:
  //
  // 1. SILUndef (and thus oldValue) must have OwnershipKind::None.
  // 2. newValue is not OwnershipKind::None due to our check above.
  //
  // Thus we know that we would be replacing a value with OwnershipKind::None
  // with a value with non-None ownership. This is a case we don't support, so
  // we can bail now.
  if (isa<SILUndef>(oldValue))
    return false;

  // Ok, we now know that we do not have SILUndef implying that we must be able
  // to get a module from our value since we must have an argument or an
  // instruction.
  auto *m = oldValue->getModule();
  assert(m);

  // If we are in Raw SIL, just bail at this point. We do not support
  // ownership fixups.
  if (m->getStage() == SILStage::Raw)
    return false;

  return true;
}

// Determine whether it is valid to replace \p oldValue with \p newValue and
// extend the lifetime of \p oldValue to cover the new uses.
//
// This updates the OwnershipFixupContext, populating transitiveBorrowedUses and
// recursiveReborrows.
static bool canFixUpOwnershipForRAUW(SILValue oldValue, SILValue newValue,
                                     OwnershipFixupContext &context) {
  if (!hasValidRAUWOwnership(oldValue, newValue))
    return false;

  if (oldValue.getOwnershipKind() == OwnershipKind::Guaranteed) {
    // Check that the old lifetime can be extended and record the necessary
    // book-keeping in the OwnershipFixupContext.
    return findTransitiveBorrowedUses(oldValue, context.transitiveBorrowedUses,
                                      context.recursiveReborrows);
  }
  return true;
}

//===----------------------------------------------------------------------===//
//                        Ownership Lifetime Extender
//===----------------------------------------------------------------------===//

namespace {

struct OwnershipLifetimeExtender {
  OwnershipFixupContext &ctx;

  /// Create a new copy of \p value assuming that our caller will clean up the
  /// copy along all paths that go through consuming point. Operationally this
  /// means that the API will insert compensating destroy_value on the copy
  /// along all paths that do not go through consuming point.
  ///
  /// DISCUSSION: If \p consumingPoint is an instruction that forwards \p value,
  /// calling this and then RAUWing with \p value guarantee that \p value will
  /// be consumed by the forwarding instruction's results consuming uses.
  CopyValueInst *createPlusOneCopy(SILValue value,
                                   SILInstruction *consumingPoint);

  /// Create a copy of \p value that covers all of \p range and insert all
  /// needed destroy_values. We assume that no uses in \p range consume \p
  /// value.
  CopyValueInst *createPlusZeroCopy(SILValue value, ArrayRef<Operand *> range) {
    return createPlusZeroCopy<ArrayRef<Operand *>>(value, range);
  }

  /// Create a copy of \p value that covers all of \p range and insert all
  /// needed destroy_values. We assume that all uses in \p range do not consume
  /// \p value.
  ///
  /// We return our copy_value to the user at +0 to show that they do not need
  /// to insert cleanup destroys.
  template <typename RangeTy>
  CopyValueInst *createPlusZeroCopy(SILValue value, const RangeTy &range);

  /// Create a new borrow scope for \p newValue that contains all uses in \p
  /// useRange. We assume that \p useRange does not contain any lifetime ending
  /// uses.
  template <typename RangeTy>
  BeginBorrowInst *createPlusZeroBorrow(SILValue newValue, RangeTy useRange);
};

} // end anonymous namespace

// Lifetime extend newValue over owned oldValue assuming that our copy will have
// its lifetime ended by oldValue's lifetime ending uses after RAUWing by our
// caller.
CopyValueInst *
OwnershipLifetimeExtender::createPlusOneCopy(SILValue value,
                                             SILInstruction *consumingPoint) {
  auto *newValInsertPt = value->getDefiningInsertionPoint();
  assert(newValInsertPt);
  CopyValueInst *copy;
  if (!isa<SILArgument>(value)) {
    SILBuilderWithScope::insertAfter(newValInsertPt, [&](SILBuilder &builder) {
      copy = builder.createCopyValue(builder.getInsertionPointLoc(), value);
    });
  } else {
    SILBuilderWithScope builder(newValInsertPt);
    copy = builder.createCopyValue(newValInsertPt->getLoc(), value);
  }

  auto &callbacks = ctx.callbacks;
  callbacks.createdNewInst(copy);

  auto *result = copy;
  ctx.jointPostDomSetComputer.findJointPostDominatingSet(
      newValInsertPt->getParent(), consumingPoint->getParent(),
      // inputBlocksFoundDuringWalk.
      [&](SILBasicBlock *loopBlock) {
        // This must be consumingPoint->getParent() since we only have one
        // consuming use. In this case, we know that this is the consuming
        // point where we will need a control equivalent copy_value (and that
        // destroy_value will be put for the out of loop value as appropriate.
        assert(loopBlock == consumingPoint->getParent());
        auto front = loopBlock->begin();
        SILBuilderWithScope newBuilder(front);
        result = newBuilder.createCopyValue(front->getLoc(), copy);
        callbacks.createdNewInst(result);

        llvm_unreachable("Should never visit this!");
      },
      // Input blocks in joint post dom set. We don't care about thse.
      [&](SILBasicBlock *postDomBlock) {
        auto front = postDomBlock->begin();
        SILBuilderWithScope newBuilder(front);
        auto *dvi = newBuilder.createDestroyValue(front->getLoc(), copy);
        callbacks.createdNewInst(dvi);
      });
  return result;
}

// A copy_value that we lifetime extend with destroy_value over range. We assume
// all instructions passed into range do not consume value.
template <typename RangeTy>
CopyValueInst *
OwnershipLifetimeExtender::createPlusZeroCopy(SILValue value,
                                              const RangeTy &range) {
  auto *newValInsertPt = value->getDefiningInsertionPoint();
  assert(newValInsertPt);

  CopyValueInst *copy;

  if (!isa<SILArgument>(value)) {
    SILBuilderWithScope::insertAfter(newValInsertPt, [&](SILBuilder &builder) {
      copy = builder.createCopyValue(builder.getInsertionPointLoc(), value);
    });
  } else {
    SILBuilderWithScope builder(newValInsertPt);
    copy = builder.createCopyValue(newValInsertPt->getLoc(), value);
  }

  auto &callbacks = ctx.callbacks;
  callbacks.createdNewInst(copy);

  auto opRange = makeUserRange(range);
  ValueLifetimeAnalysis lifetimeAnalysis(copy, opRange);
  ValueLifetimeAnalysis::Frontier frontier;
  bool result = lifetimeAnalysis.computeFrontier(
      frontier, ValueLifetimeAnalysis::DontModifyCFG, &ctx.deBlocks);
  assert(result);

  while (!frontier.empty()) {
    auto *insertPt = frontier.pop_back_val();
    SILBuilderWithScope frontierBuilder(insertPt);
    auto *dvi = frontierBuilder.createDestroyValue(insertPt->getLoc(), copy);
    callbacks.createdNewInst(dvi);
  }

  return copy;
}

template <typename RangeTy>
BeginBorrowInst *
OwnershipLifetimeExtender::createPlusZeroBorrow(SILValue newValue,
                                                RangeTy useRange) {
  auto *newValInsertPt = newValue->getDefiningInsertionPoint();
  assert(newValInsertPt);

  CopyValueInst *copy = nullptr;
  BeginBorrowInst *borrow = nullptr;
  if (!isa<SILArgument>(newValue)) {
    SILBuilderWithScope::insertAfter(newValInsertPt, [&](SILBuilder &builder) {
      auto loc = builder.getInsertionPointLoc();
      copy = builder.createCopyValue(loc, newValue);
      borrow = builder.createBeginBorrow(loc, copy);
    });
  } else {
    SILBuilderWithScope builder(newValInsertPt);
    auto loc = newValInsertPt->getLoc();
    copy = builder.createCopyValue(loc, newValue);
    borrow = builder.createBeginBorrow(loc, copy);
  }
  assert(copy && borrow);

  auto opRange = makeUserRange(useRange);
  ValueLifetimeAnalysis lifetimeAnalysis(copy, opRange);
  ValueLifetimeAnalysis::Frontier frontier;
  bool result = lifetimeAnalysis.computeFrontier(
      frontier, ValueLifetimeAnalysis::DontModifyCFG, &ctx.deBlocks);
  assert(result);

  auto &callbacks = ctx.callbacks;
  while (!frontier.empty()) {
    auto *insertPt = frontier.pop_back_val();
    SILBuilderWithScope frontierBuilder(insertPt);
    // Use an auto-generated location here, because insertPt may have an
    // incompatible LocationKind
    auto loc = RegularLocation::getAutoGeneratedLocation(
        insertPt->getLoc().getSourceLoc());
    auto *ebi = frontierBuilder.createEndBorrow(loc, borrow);
    auto *dvi = frontierBuilder.createDestroyValue(loc, copy);
    callbacks.createdNewInst(ebi);
    callbacks.createdNewInst(dvi);
  }

  return borrow;
}

//===----------------------------------------------------------------------===//
//                            Reborrow Elimination
//===----------------------------------------------------------------------===//

static void eliminateReborrowsOfRecursiveBorrows(
    ArrayRef<BorrowingOperand> transitiveReborrows,
    SmallVectorImpl<Operand *> &usePoints, InstModCallbacks &callbacks) {
  SmallVector<std::pair<SILPhiArgument *, SILPhiArgument *>, 8>
      baseBorrowedValuePair;
  // Ok, we have transitive reborrows.
  for (auto borrowingOperand : transitiveReborrows) {
    // We eliminate the reborrow by creating a new copy+borrow at the reborrow
    // edge from the base value and using that for the reborrow instead of the
    // actual value. We of course insert an end_borrow for our original incoming
    // value.
    SILValue value = borrowingOperand->get();
    auto *bi = cast<BranchInst>(borrowingOperand->getUser());
    SILBuilderWithScope reborrowBuilder(bi);
    // Use an auto-generated location here, because the branch may have an
    // incompatible LocationKind
    auto loc =
        RegularLocation::getAutoGeneratedLocation(bi->getLoc().getSourceLoc());
    auto *innerCopy = reborrowBuilder.createCopyValue(loc, value);
    auto *innerBorrow = reborrowBuilder.createBeginBorrow(loc, innerCopy);
    auto *outerEndBorrow = reborrowBuilder.createEndBorrow(loc, value);

    callbacks.createdNewInst(innerCopy);
    callbacks.createdNewInst(innerBorrow);
    callbacks.createdNewInst(outerEndBorrow);

    // Then set our borrowing operand to take our innerBorrow instead of value
    // (whose lifetime we just ended).
    callbacks.setUseValue(*borrowingOperand, innerBorrow);
    // Add our outer end borrow as a use point to make sure that we extend our
    // base value to this point.
    usePoints.push_back(&outerEndBorrow->getAllOperands()[0]);

    // Then check if in our destination block, we have further reborrows. If we
    // do, we need to recursively process them.
    auto *borrowedArg =
        const_cast<SILPhiArgument *>(bi->getArgForOperand(borrowingOperand));
    auto *baseArg =
        insertOwnedBaseValueAlongBranchEdge(bi, innerCopy, callbacks);
    baseBorrowedValuePair.emplace_back(baseArg, borrowedArg);
  }

  // Now recursively update all further reborrows...
  while (!baseBorrowedValuePair.empty()) {
    SILPhiArgument *baseArg;
    SILPhiArgument *borrowedArg;
    std::tie(baseArg, borrowedArg) = baseBorrowedValuePair.pop_back_val();

    for (auto *use : borrowedArg->getConsumingUses()) {
      // If our consuming use is an end of scope marker, we need to end
      // the lifetime of our base arg.
      if (isEndOfScopeMarker(use->getUser())) {
        SILBuilderWithScope::insertAfter(use->getUser(), [&](SILBuilder &b) {
          auto *dvi = b.createDestroyValue(b.getInsertionPointLoc(), baseArg);
          callbacks.createdNewInst(dvi);
        });
        continue;
      }

      // Otherwise, we have a reborrow. For now our reborrows must be
      // phis. Add our owned value as a new argument of that phi along our
      // edge and undef along all other edges.
      auto borrowingOp = BorrowingOperand::get(use);
      auto *brInst = cast<BranchInst>(borrowingOp.op->getUser());
      auto *newBorrowedPhi = brInst->getArgForOperand(borrowingOp);
      auto *newBasePhi =
          insertOwnedBaseValueAlongBranchEdge(brInst, baseArg, callbacks);
      baseBorrowedValuePair.emplace_back(newBasePhi, newBorrowedPhi);
    }
  }
}

static void rewriteReborrows(SILValue newBorrowedValue,
                             ArrayRef<BorrowingOperand> foundReborrows,
                             InstModCallbacks &callbacks) {
  // Each initial reborrow that we have is a use of oldValue, so we know
  // that copy should be valid at the reborrow.
  SmallVector<std::pair<SILPhiArgument *, SILPhiArgument *>, 8>
      baseBorrowedValuePair;
  for (auto reborrow : foundReborrows) {
    auto *bi = cast<BranchInst>(reborrow.op->getUser());
    SILBuilderWithScope reborrowBuilder(bi);
    // Use an auto-generated location here, because the branch may have an
    // incompatible LocationKind
    auto loc =
        RegularLocation::getAutoGeneratedLocation(bi->getLoc().getSourceLoc());
    auto *innerCopy = reborrowBuilder.createCopyValue(loc, newBorrowedValue);
    auto *innerBorrow = reborrowBuilder.createBeginBorrow(loc, innerCopy);
    auto *outerEndBorrow =
        reborrowBuilder.createEndBorrow(loc, reborrow.op->get());
    callbacks.createdNewInst(innerCopy);
    callbacks.createdNewInst(innerBorrow);
    callbacks.createdNewInst(outerEndBorrow);

    callbacks.setUseValue(*reborrow, innerBorrow);

    auto *borrowedArg =
        const_cast<SILPhiArgument *>(bi->getArgForOperand(reborrow.op));
    auto *baseArg =
        insertOwnedBaseValueAlongBranchEdge(bi, innerCopy, callbacks);
    baseBorrowedValuePair.emplace_back(baseArg, borrowedArg);
  }

  // Now, follow through all chains of reborrows.
  while (!baseBorrowedValuePair.empty()) {
    SILPhiArgument *baseArg;
    SILPhiArgument *borrowedArg;
    std::tie(baseArg, borrowedArg) = baseBorrowedValuePair.pop_back_val();

    for (auto *use : borrowedArg->getConsumingUses()) {
      // If our consuming use is an end of scope marker, we need to end
      // the lifetime of our base arg.
      if (isEndOfScopeMarker(use->getUser())) {
        SILBuilderWithScope::insertAfter(use->getUser(), [&](SILBuilder &b) {
          auto *dvi = b.createDestroyValue(b.getInsertionPointLoc(), baseArg);
          callbacks.createdNewInst(dvi);
        });
        continue;
      }

      // Otherwise, we have a reborrow. For now our reborrows must be
      // phis. Add our owned value as a new argument of that phi along our
      // edge and undef along all other edges.
      auto borrowingOp = BorrowingOperand::get(use);
      auto *brInst = cast<BranchInst>(borrowingOp.op->getUser());
      auto *newBorrowedPhi = brInst->getArgForOperand(borrowingOp);
      auto *newBasePhi =
          insertOwnedBaseValueAlongBranchEdge(brInst, baseArg, callbacks);
      baseBorrowedValuePair.emplace_back(newBasePhi, newBorrowedPhi);
    }
  }
}

//===----------------------------------------------------------------------===//
//                OwnershipRAUWUtility - RAUW + fix ownership
//===----------------------------------------------------------------------===//

/// Given an old value and a new value, lifetime extend new value as appropriate
/// so we can RAUW new value with old value and preserve ownership
/// invariants. We leave fixing up the lifetime of old value to our caller.
namespace {

struct OwnershipRAUWUtility {
  SingleValueInstruction *oldValue;
  SILValue newValue;
  OwnershipFixupContext &ctx;

  SILBasicBlock::iterator handleUnowned();

  SILBasicBlock::iterator handleGuaranteed();

  SILBasicBlock::iterator perform();

  /// Insert copies/borrows as appropriate to eliminate any reborrows of
  /// borrowed value, given we are going to replace it with newValue.
  void eliminateReborrows(BorrowedValue oldBorrowedValue, SILValue newValue);

  OwnershipLifetimeExtender getLifetimeExtender() { return {ctx}; }

  const InstModCallbacks &getCallbacks() const { return ctx.callbacks; }
};

} // anonymous namespace

SILBasicBlock::iterator OwnershipRAUWUtility::handleUnowned() {
  auto &callbacks = ctx.callbacks;
  switch (newValue.getOwnershipKind()) {
  case OwnershipKind::None:
    llvm_unreachable("Should have been handled elsewhere");
  case OwnershipKind::Any:
    llvm_unreachable("Invalid for values");
  case OwnershipKind::Unowned:
    // An unowned value can always be RAUWed with another unowned value.
    return replaceAllUsesAndErase(oldValue, newValue, callbacks);
  case OwnershipKind::Guaranteed: {
    // If we have an unowned value that we want to replace with a guaranteed
    // value, we need to ensure that the guaranteed value is live at all use
    // points of the unowned value. If so, just replace and continue.
    //
    // TODO: Implement this.

    // Otherwise, we need to lifetime extend the borrow over all of the use
    // points. To do so, we copy the value, borrow it, insert an unchecked
    // ownership conversion to unowned at oldValue and then RAUW.
    //
    // We need to insert the conversion to ensure that we do not violate
    // ownership propagation rules of forwarding insts.
    SmallVector<Operand *, 8> oldValueUses(oldValue->getUses());
    for (auto *use : oldValueUses) {
      if (auto *ti = dyn_cast<TermInst>(use->getUser())) {
        if (ti->isFunctionExiting()) {
          SILBuilderWithScope builder(ti);
          auto *newInst = builder.createUncheckedOwnershipConversion(
              ti->getLoc(), use->get(), OwnershipKind::Unowned);
          callbacks.createdNewInst(newInst);
          callbacks.setUseValue(use, newInst);
        }
      }
    }
    auto extender = getLifetimeExtender();
    SILValue borrow =
        extender.createPlusZeroBorrow(newValue, oldValue->getUses());
    SILBuilderWithScope builder(oldValue);
    auto *newInst = builder.createUncheckedOwnershipConversion(
        oldValue->getLoc(), borrow, OwnershipKind::Unowned);
    callbacks.createdNewInst(newInst);
    return replaceAllUsesAndErase(oldValue, newInst, callbacks);
  }
  case OwnershipKind::Owned: {
    // If we have an unowned value that we want to replace with an owned value,
    // we first check if the owned value is live over all use points of the old
    // value. If so, just RAUW and continue.
    //
    // TODO: Implement this.

    // Otherwise, insert a copy of the owned value and lifetime extend that over
    // all uses of the value and then RAUW.
    SmallVector<Operand *, 8> oldValueUses(oldValue->getUses());
    for (auto *use : oldValueUses) {
      if (auto *ti = dyn_cast<TermInst>(use->getUser())) {
        if (ti->isFunctionExiting()) {
          SILBuilderWithScope builder(ti);
          // We insert this to ensure that we can extend our owned value's
          // lifetime to before the function end point.
          auto *newInst = builder.createUncheckedOwnershipConversion(
              ti->getLoc(), use->get(), OwnershipKind::Unowned);
          callbacks.createdNewInst(newInst);
          callbacks.setUseValue(use, newInst);
        }
      }
    }
    auto extender = getLifetimeExtender();
    SILValue copy = extender.createPlusZeroCopy(newValue, oldValue->getUses());
    SILBuilderWithScope builder(oldValue);
    auto *newInst = builder.createUncheckedOwnershipConversion(
        oldValue->getLoc(), copy, OwnershipKind::Unowned);
    callbacks.createdNewInst(newInst);
    auto result = replaceAllUsesAndErase(oldValue, newInst, callbacks);
    return result;
  }
  }
  llvm_unreachable("covered switch isn't covered?!");
}

SILBasicBlock::iterator OwnershipRAUWUtility::handleGuaranteed() {
  // If we want to replace a guaranteed value with a value of some other
  // ownership whose def dominates our guaranteed value. We first see if all
  // uses of the old guaranteed value are within the lifetime of the new
  // guaranteed value. If so, we can just RAUW and move on.
  //
  // TODO: Implement this.
  //
  // Otherwise, we need to actually modify the IR. We first always first
  // lifetime extend newValue to oldValue's transitive uses to set our
  // workspace.

  // If we have any transitive reborrows on sub-borrows.
  if (ctx.recursiveReborrows.size())
    eliminateReborrowsOfRecursiveBorrows(ctx.recursiveReborrows,
                                         ctx.transitiveBorrowedUses,
                                         ctx.callbacks);

  auto extender = getLifetimeExtender();
  SILValue newBorrowedValue =
      extender.createPlusZeroBorrow<ArrayRef<Operand *>>(
          newValue, ctx.transitiveBorrowedUses);

  // Now we need to handle reborrows by eliminating the reborrows from any
  // borrowing operands that use old value as well as from oldvalue itself. We
  // take advantage of a few properties of reborrows:
  //
  // 1. A reborrow has to be on a BorrowedValue. This ensures that the same
  //    base value is propagated through chains of reborrows. (In the future
  //    this may not be true when destructures are introduced as reborrow
  //    instructions).
  //
  // 2. Given that, we change each reborrows into new copy+borrow from the
  //    owned value that we perform at the reborrow use. What is nice about
  //    this formulation is that it ensures that we are always working with a
  //    non-dominating copy value, allowing us to force our borrowing value to
  //    need a base phi argument (the one of our choosing).
  if (auto oldValueBorrowedVal = BorrowedValue::get(oldValue)) {
    SmallVector<BorrowingOperand, 8> foundReborrows;
    if (oldValueBorrowedVal.gatherReborrows(foundReborrows)) {
      rewriteReborrows(newBorrowedValue, foundReborrows, ctx.callbacks);
    }
  }

  // Then we need to look and see if our oldValue had any transitive uses that

  // Ok, we now have eliminated any reborrows if we had any. That means that
  // the uses of oldValue should be completely within the lifetime of our new
  // borrow.
  return replaceAllUsesAndErase(oldValue, newBorrowedValue, ctx.callbacks);
}

SILBasicBlock::iterator OwnershipRAUWUtility::perform() {
  assert(oldValue->getFunction()->hasOwnership());
  assert(hasValidRAUWOwnership(oldValue, newValue) &&
      "Should have checked if can perform this operation before calling it?!");
  // If our new value is just none, we can pass anything to do it so just RAUW
  // and return.
  //
  // NOTE: This handles RAUWing with undef.
  if (newValue.getOwnershipKind() == OwnershipKind::None)
    return replaceAllUsesAndErase(oldValue, newValue, ctx.callbacks);
  assert(SILValue(oldValue).getOwnershipKind() != OwnershipKind::None);

  switch (SILValue(oldValue).getOwnershipKind()) {
  case OwnershipKind::None:
    // If our old value was none and our new value is not, we need to do
    // something more complex that we do not support yet, so bail. We should
    // have not called this function in such a case.
    llvm_unreachable("Should have been handled elsewhere");
  case OwnershipKind::Any:
    llvm_unreachable("Invalid for values");
  case OwnershipKind::Guaranteed: {
    return handleGuaranteed();
  }
  case OwnershipKind::Owned: {
    // If we have an owned value that we want to replace with a value with any
    // other non-None ownership, we need to copy the other value for a
    // lifetimeEnding RAUW, then RAUW the value, and insert a destroy_value on
    // the original value.
    auto extender = getLifetimeExtender();
    SILValue copy = extender.createPlusOneCopy(newValue, oldValue);
    cleanupOperandsBeforeDeletion(oldValue, ctx.callbacks);
    auto result = replaceAllUsesAndErase(oldValue, copy, ctx.callbacks);
    return result;
  }
  case OwnershipKind::Unowned: {
    return handleUnowned();
  }
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

//===----------------------------------------------------------------------===//
//                     Interior Pointer Operand Rebasing
//===----------------------------------------------------------------------===//

namespace {

/// Clone all projections and casts on the access use-def chain until either the
/// specified predicate is true or the access base is reached.
///
/// This will not clone ref_element_addr or ref_tail_addr because those aren't
/// part of the access chain.
class InteriorPointerAddressRebaseUseDefChainCloner
    : public AccessUseDefChainVisitor<
          InteriorPointerAddressRebaseUseDefChainCloner, SILValue> {
  SILValue oldAddressValue;
  SingleValueInstruction *oldIntPtr;
  SingleValueInstruction *newIntPtr;
  SILInstruction *insertPt;

public:
  InteriorPointerAddressRebaseUseDefChainCloner(
      SILValue oldAddressValue, SingleValueInstruction *oldIntPtr,
      SingleValueInstruction *newIntPtr, SILInstruction *insertPt)
      : oldAddressValue(oldAddressValue), oldIntPtr(oldIntPtr),
        newIntPtr(newIntPtr), insertPt(insertPt) {}

  // Recursive main entry point
  SILValue cloneUseDefChain(SILValue currentOldAddr) {
    // If we have finally hit oldIntPtr, we are done.
    if (currentOldAddr == oldIntPtr)
      return currentOldAddr;
    return this->visit(currentOldAddr);
  }

  // Recursively clone an address on the use-def chain.
  SingleValueInstruction *cloneProjection(SingleValueInstruction *projectedAddr,
                                          Operand *sourceOper) {
    SILValue sourceOperVal = sourceOper->get();
    SILValue projectedSource = cloneUseDefChain(sourceOperVal);
    // If we hit the end of our chain, then make newIntPtr the operand so that
    // we have successfully rebased.
    if (sourceOperVal == projectedSource)
      projectedSource = newIntPtr;
    SILInstruction *clone = projectedAddr->clone(insertPt);
    clone->setOperand(sourceOper->getOperandNumber(), projectedSource);
    return cast<SingleValueInstruction>(clone);
  }

  SILValue visitBase(SILValue base, AccessedStorage::Kind kind) {
    assert(false && "access base cannot be cloned");
    return SILValue();
  }

  SILValue visitNonAccess(SILValue base) {
    assert(false && "unknown address root cannot be cloned");
    return SILValue();
  }

  SILValue visitPhi(SILPhiArgument *phi) {
    assert(false && "unexpected phi on access path");
    return SILValue();
  }

  SILValue visitStorageCast(SingleValueInstruction *cast, Operand *sourceOper) {
    assert(false && "unexpected storage cast on access path");
    return SILValue();
  }

  SILValue visitAccessProjection(SingleValueInstruction *projectedAddr,
                                 Operand *sourceOper) {
    return cloneProjection(projectedAddr, sourceOper);
  }
};

} // namespace

/// \p oldAddressValue is an address rooted in \p oldIntPtr. Clone the use-def
/// chain from \p oldAddressValue to \p oldIntPtr, but starting from \p
/// newAddressValue.
static SILValue cloneInteriorProjectionUseDefChain(
    SILValue oldAddressValue, SingleValueInstruction *oldIntPtr,
    SingleValueInstruction *newIntPtr, SILInstruction *insertPt) {
  InteriorPointerAddressRebaseUseDefChainCloner cloner(
      oldAddressValue, oldIntPtr, newIntPtr, insertPt);
  return cloner.cloneUseDefChain(oldAddressValue);
}

SILBasicBlock::iterator
OwnershipRAUWHelper::replaceAddressUses(SingleValueInstruction *oldValue,
                                        SILValue newValue) {
  assert(oldValue->getType().isAddress() &&
         oldValue->getType() == newValue->getType());

  // If we are replacing addresses, see if we need to handle interior pointer
  // fixups. If we don't have any extra info, then we know that we can just RAUW
  // without any further work.
  if (!ctx->extraAddressFixupInfo.intPtrOp)
    return replaceAllUsesAndErase(oldValue, newValue, ctx->callbacks);

  // We are RAUWing two addresses and we found that:
  //
  // 1. newValue is an address associated with an interior pointer instruction.
  // 2. oldValue has uses that are outside of newValue's borrow scope.
  //
  // So, we need to copy/borrow the base value of the interior pointer to
  // lifetime extend the base value over the new uses. Then we clone the
  // interior pointer instruction and change the clone to use our new borrowed
  // value. Then we RAUW as appropriate.
  OwnershipLifetimeExtender extender{*ctx};
  auto &extraInfo = ctx->extraAddressFixupInfo;
  auto intPtr = *extraInfo.intPtrOp;
  BeginBorrowInst *bbi = extender.createPlusZeroBorrow(
      intPtr->get(), llvm::makeArrayRef(extraInfo.allAddressUsesFromOldValue));
  auto bbiNext = &*std::next(bbi->getIterator());
  auto *newIntPtrUser =
      cast<SingleValueInstruction>(intPtr->getUser()->clone(bbiNext));
  ctx->callbacks.createdNewInst(newIntPtrUser);
  newIntPtrUser->setOperand(0, bbi);

  // Now that we have extended our lifetime as appropriate, we need to recreate
  // the access path from newValue to intPtr but upon newIntPtr. Then we make it
  // use newIntPtr.
  auto *intPtrUser = cast<SingleValueInstruction>(intPtr->getUser());
  SILValue initialAddr = cloneInteriorProjectionUseDefChain(
      newValue /*address we originally wanted to replace*/,
      intPtrUser /*the interior pointer of that value*/,
      newIntPtrUser /*the interior pointer we need to recreate the chain upon*/,
      oldValue /*insert point*/);

  // If we got back newValue, then we need to set initialAddr to be int ptr
  // user.
  if (initialAddr == newValue)
    initialAddr = newIntPtrUser;

  // Now that we have an addr that is setup appropriately, RAUW!
  return replaceAllUsesAndErase(oldValue, initialAddr, ctx->callbacks);
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoints
//===----------------------------------------------------------------------===//

OwnershipRAUWHelper::OwnershipRAUWHelper(OwnershipFixupContext &inputCtx,
                                         SingleValueInstruction *inputOldValue,
                                         SILValue inputNewValue)
    : ctx(&inputCtx), oldValue(inputOldValue), newValue(inputNewValue) {
  // If we are already not valid, just bail.
  if (!isValid())
    return;

  // Otherwise, lets check if we can perform this RAUW operation. If we can't,
  // set ctx to nullptr to invalidate the helper and return.
  if (!canFixUpOwnershipForRAUW(oldValue, newValue, inputCtx)) {
    ctx = nullptr;
    return;
  }

  // If we have an object, at this point we are good to go so we can just
  // return.
  if (newValue->getType().isObject())
    return;

  // But if we have an address, we need to check if new value is from an
  // interior pointer or not in a way that the pass understands. What we do is:
  //
  // 1. Early exit some cases that we know can never have interior pointers.
  //
  // 2. Compute the AccessPathWithBase of newValue. If we do not get back a
  //    valid such object, invalidate and then bail.
  //
  // 3. Then we check if the base address is the result of an interior pointer
  //    instruction. If we do not find one we bail.
  //
  // 4. Then grab the base value of the interior pointer operand. We only
  //    support cases where we have a single BorrowedValue as our base. This is
  //    a safe future proof assumption since one reborrows are on
  //    structs/tuple/destructures, a guaranteed value will always be associated
  //    with a single BorrowedValue, so this will never fail (and the code will
  //    probably be DCEed).
  //
  // 5. Then we compute an AccessPathWithBase for oldValue and then find its
  //    derived uses. If we fail, we bail.
  //
  // 6. At this point, we know that we can perform this RAUW. The only question
  //    is if we need to when we RAUW copy the interior pointer base value. We
  //    perform this check by making sure all of the old value's derived uses
  //    are within our BorrowedValue's scope. If so, we clear the extra state we
  //    were tracking (the interior pointer/oldValue's transitive uses), so we
  //    perform just a normal RAUW (without inserting the copy) when we RAUW.
  //
  // We can always RAUW an address with a pointer_to_address since if there
  // were any interior pointer constraints on whatever address pointer came
  // from, the address_to_pointer producing that value erases that
  // information, so we can RAUW without worrying.
  //
  // NOTE: We also need to handle this here since a pointer_to_address is not a
  // valid base value for an access path since it doesn't refer to any storage.
  {
    auto baseProj =
        getUnderlyingObjectStoppingAtObjectToAddrProjections(newValue);
    if (isa<PointerToAddressInst>(baseProj)) {
      return;
    }
  }

  auto accessPathWithBase = AccessPathWithBase::compute(newValue);
  if (!accessPathWithBase.base) {
    // Invalidate!
    ctx = nullptr;
    return;
  }

  auto &intPtr = ctx->extraAddressFixupInfo.intPtrOp;
  intPtr = InteriorPointerOperand::inferFromResult(accessPathWithBase.base);
  if (!intPtr) {
    // We can optimize! Do not invalidate!
    return;
  }

  auto borrowedValue = intPtr.getSingleBaseValue();
  if (!borrowedValue) {
    // Invalidate!
    ctx = nullptr;
    return;
  }

  // For now, just gather up uses
  auto &oldValueUses = ctx->extraAddressFixupInfo.allAddressUsesFromOldValue;
  if (InteriorPointerOperand::findTransitiveUsesForAddress(oldValue,
                                                           oldValueUses)) {
    // If we found an error, invalidate and return!
    ctx = nullptr;
    return;
  }

  // Ok, at this point we know that we can optimize. The only question is if we
  // need to perform the copy or not when we actually RAUW. So perform the is
  // within region check. If we succeed, clear our extra state so we perform a
  // normal RAUW.
  SmallVector<Operand *, 8> scratchSpace;
  SmallPtrSet<SILBasicBlock *, 8> visitedBlocks;
  if (borrowedValue.areUsesWithinScope(oldValueUses, scratchSpace,
                                       visitedBlocks, ctx->deBlocks)) {
    // We do not need to copy the base value! Clear the extra info we have.
    ctx->extraAddressFixupInfo.clear();
  }
}

SILBasicBlock::iterator OwnershipRAUWHelper::perform() {
  assert(isValid() && "OwnershipRAUWHelper invalid?!");

  // Make sure to always clear our context after we transform.
  SWIFT_DEFER { ctx->clear(); };

  if (oldValue->getType().isAddress())
    return replaceAddressUses(oldValue, newValue);

  OwnershipRAUWUtility utility{oldValue, newValue, *ctx};
  return utility.perform();
}
