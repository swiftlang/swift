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
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                           Low Level RAUW Utility
//===----------------------------------------------------------------------===//

static SILBasicBlock::iterator
replaceAllUsesAndEraseInner(SingleValueInstruction *svi, SILValue newValue,
                            std::function<void(SILInstruction *)> eraseNotify) {
  assert(svi != newValue && "Cannot RAUW a value with itself");
  SILBasicBlock::iterator nextii = std::next(svi->getIterator());

  // Only SingleValueInstructions are currently simplified.
  while (!svi->use_empty()) {
    Operand *use = *svi->use_begin();
    SILInstruction *user = use->getUser();
    // Erase the end of scope marker.
    if (isEndOfScopeMarker(user)) {
      if (&*nextii == user)
        ++nextii;
      if (eraseNotify)
        eraseNotify(user);
      else
        user->eraseFromParent();
      continue;
    }
    use->set(newValue);
  }
  if (eraseNotify)
    eraseNotify(svi);
  else
    svi->eraseFromParent();

  return nextii;
}

//===----------------------------------------------------------------------===//
//                        Ownership Lifetime Extender
//===----------------------------------------------------------------------===//

namespace {

struct OwnershipLifetimeExtender {
  OwnershipFixupContext &ctx;

  /// Lifetime extend newValue over owned oldValue assuming that our copy will
  /// have its lifetime ended by oldValue's lifetime ending uses after RAUWing.
  CopyValueInst *
  copyAndExtendForLifetimeEndingRAUW(SILValue value,
                                     SILInstruction *consumingPoint);

  CopyValueInst *
  copyAndExtendForNonLifetimeEndingRAUW(SILValue value,
                                        ArrayRef<Operand *> range) {
    return copyAndExtendForNonLifetimeEndingRAUW<ArrayRef<Operand *>>(value,
                                                                      range);
  }

  template <typename RangeTy>
  CopyValueInst *copyAndExtendForNonLifetimeEndingRAUW(SILValue value,
                                                       const RangeTy &range);

  template <typename RangeTy>
  BeginBorrowInst *copyBorrowAndExtendForRAUW(SILValue newValue,
                                              RangeTy useRange);

  /// We are copy/borrowing new value to be over the same lifetime as old
  /// value. We know that oldValue is dominated by newValue.
  BeginBorrowInst *copyBorrowAndExtendForLifetimeEndingRAUW(SILValue newValue,
                                                            SILValue oldValue);
};

} // end anonymous namespace

/// Lifetime extend newValue over owned oldValue assuming that our copy will
/// have its lifetime ended by oldValue's lifetime ending uses after RAUWing.
CopyValueInst *OwnershipLifetimeExtender::copyAndExtendForLifetimeEndingRAUW(
    SILValue value, SILInstruction *consumingPoint) {
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
  if (ctx.newInstNotify)
    ctx.newInstNotify(copy);

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
        if (ctx.newInstNotify)
          ctx.newInstNotify(result);

        llvm_unreachable("Should never visit this!");
      },
      // Input blocks in joint post dom set. We don't care about thse.
      [&](SILBasicBlock *postDomBlock) {
        auto front = postDomBlock->begin();
        SILBuilderWithScope newBuilder(front);
        auto *dvi = newBuilder.createDestroyValue(front->getLoc(), copy);
        if (ctx.newInstNotify)
          ctx.newInstNotify(dvi);
      });
  return result;
}

template <typename RangeTy>
CopyValueInst *OwnershipLifetimeExtender::copyAndExtendForNonLifetimeEndingRAUW(
    SILValue value, const RangeTy &range) {
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
  if (ctx.newInstNotify)
    ctx.newInstNotify(copy);

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
    if (ctx.newInstNotify)
      ctx.newInstNotify(dvi);
  }

  return copy;
}

template <typename RangeTy>
BeginBorrowInst *
OwnershipLifetimeExtender::copyBorrowAndExtendForRAUW(SILValue newValue,
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

  while (!frontier.empty()) {
    auto *insertPt = frontier.pop_back_val();
    SILBuilderWithScope frontierBuilder(insertPt);
    // Use an auto-generated location here, because insertPt may have an
    // incompatible LocationKind
    auto loc = RegularLocation::getAutoGeneratedLocation(
        insertPt->getLoc().getSourceLoc());
    auto *ebi = frontierBuilder.createEndBorrow(loc, borrow);
    auto *dvi = frontierBuilder.createDestroyValue(loc, copy);
    if (ctx.newInstNotify) {
      ctx.newInstNotify(ebi);
      ctx.newInstNotify(dvi);
    }
  }

  return borrow;
}

/// We are copy/borrowing new value to be over the same lifetime as old
/// value. We know that oldValue is dominated by newValue.
BeginBorrowInst *
OwnershipLifetimeExtender::copyBorrowAndExtendForLifetimeEndingRAUW(
    SILValue newValue, SILValue oldValue) {
  auto *newValInsertPt = newValue->getDefiningInsertionPoint();
  assert(newValInsertPt);

  CopyValueInst *copy = nullptr;
  if (!isa<SILArgument>(newValue)) {
    SILBuilderWithScope::insertAfter(newValInsertPt, [&](SILBuilder &builder) {
      auto loc = builder.getInsertionPointLoc();
      copy = builder.createCopyValue(loc, newValue);
    });
  } else {
    SILBuilderWithScope builder(newValInsertPt);
    auto loc = newValInsertPt->getLoc();
    copy = builder.createCopyValue(loc, newValue);
  }

  // Then insert the begin_borrow at the old value point. We are going to RAUW
  // this in our caller.
  auto *oldValInsertPt = oldValue->getDefiningInsertionPoint();
  assert(oldValInsertPt);
  auto *borrow = SILBuilderWithScope(oldValInsertPt)
                     .createBeginBorrow(oldValInsertPt->getLoc(), copy);
  if (ctx.newInstNotify) {
    ctx.newInstNotify(borrow);
  }

  ValueLifetimeAnalysis lifetimeAnalysis(copy, oldValue->getUses());
  decltype(lifetimeAnalysis)::Frontier frontier;
  bool result = lifetimeAnalysis.computeFrontier(
      frontier, ValueLifetimeAnalysis::DontModifyCFG, &ctx.deBlocks);
  assert(result);

  while (!frontier.empty()) {
    auto *insertPt = frontier.pop_back_val();
    SILBuilderWithScope frontierBuilder(insertPt);
    auto *dvi = frontierBuilder.createDestroyValue(insertPt->getLoc(), copy);
    if (ctx.newInstNotify) {
      ctx.newInstNotify(dvi);
    }
  }

  return borrow;
}

//===----------------------------------------------------------------------===//
//                            Ownership Fixup RAUW
//===----------------------------------------------------------------------===//

/// Given an old value and a new value, lifetime extend new value as appropriate
/// so we can RAUW new value with old value and preserve ownership
/// invariants. We leave fixing up the lifetime of old value to our caller.
namespace {

struct OwnershipRAUWUtility {
  SingleValueInstruction *oldValue;
  SILValue newValue;
  OwnershipFixupContext &ctx;

  void rewriteReborrows(SILValue borrow, ArrayRef<BorrowingOperand> reborrows);
  void eliminateReborrowsOfRecursiveBorrows(
      ArrayRef<BorrowingOperand> transitiveReborrows,
      SmallVectorImpl<Operand *> &usePoints);

  SILBasicBlock::iterator handleUnowned();

  SILBasicBlock::iterator handleGuaranteed();

  SILBasicBlock::iterator perform();

  /// Insert copies/borrows as appropriate to eliminate any reborrows of
  /// borrowed value, given we are going to replace it with newValue.
  void eliminateReborrows(BorrowedValue oldBorrowedValue, SILValue newValue);

  OwnershipLifetimeExtender getLifetimeExtender() { return {ctx}; }

  InstModCallbacks getCallbacks() const {
    return InstModCallbacks(
        ctx.eraseNotify, ctx.newInstNotify,
        [](SILValue, SILValue) { llvm_unreachable("unhandled"); },
        [](SingleValueInstruction *, SILValue) {
          llvm_unreachable("unhandled");
        });
  }
};

} // anonymous namespace

static void cleanupOperandsBeforeDeletion(
    SILInstruction *oldValue,
    std::function<void(SILInstruction *)> newNotifyInst) {
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
      if (newNotifyInst)
        newNotifyInst(dvi);
      continue;
    }
    case OwnershipKind::Guaranteed: {
      // Should only happen once we model destructures as true reborrows.
      auto *ebi = builder.createEndBorrow(oldValue->getLoc(), op.get());
      if (newNotifyInst)
        newNotifyInst(ebi);
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
                                    const InstModCallbacks &callbacks) {
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

static void getAllNonTrivialUsePointsOfBorrowedValue(
    SILValue value, SmallVectorImpl<Operand *> &usePoints,
    SmallVectorImpl<BorrowingOperand> &reborrowPoints) {
  assert(value.getOwnershipKind() == OwnershipKind::Guaranteed);

  unsigned firstOffset = usePoints.size();
  llvm::copy(value->getUses(), std::back_inserter(usePoints));

  if (usePoints.size() == firstOffset)
    return;

  // NOTE: Use points resizes in this loop so usePoints.size() may be
  // different every time.
  for (unsigned i = firstOffset; i < usePoints.size(); ++i) {
    if (auto fOperand = ForwardingOperand::get(usePoints[i])) {
      fOperand->visitForwardedValues([&](SILValue transitiveValue) {
        // Do not include transitive uses with 'none' ownership
        if (transitiveValue.getOwnershipKind() == OwnershipKind::None)
          return true;
        for (auto *transitiveUse : transitiveValue->getUses())
          usePoints.push_back(transitiveUse);
        return true;
      });
      continue;
    }

    if (auto borrowingOp = BorrowingOperand::get(usePoints[i])) {
      // If we have a reborrow, we have no further work to do, our reborrow is
      // already a use and we will handle the reborrow separately.
      if (borrowingOp->isReborrow())
        continue;

      // Otherwise, try to grab additional end scope instructions to find more
      // liveness info. Stash any reborrow uses so that we can eliminate the
      // reborrow before we are done processing.
      borrowingOp->visitLocalEndScopeUses([&](Operand *scopeEndingUse) {
        if (auto scopeEndingBorrowingOp =
                BorrowingOperand::get(scopeEndingUse)) {
          if (scopeEndingBorrowingOp->isReborrow()) {
            reborrowPoints.push_back(scopeEndingUse);
            return true;
          }
        }
        usePoints.push_back(scopeEndingUse);
        return true;
      });

      // Now break up all of the reborrows
      continue;
    }

    // If our base guaranteed value does not have any consuming uses (consider
    // function arguments), we need to be sure to include interior pointer
    // operands since we may not get a use from a end_scope instruction.
    if (auto intPtrOperand = InteriorPointerOperand::get(usePoints[i])) {
      intPtrOperand->getImplicitUses(usePoints);
      continue;
    }
  }
}

void OwnershipRAUWUtility::eliminateReborrowsOfRecursiveBorrows(
    ArrayRef<BorrowingOperand> transitiveReborrows,
    SmallVectorImpl<Operand *> &usePoints) {
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
    if (ctx.newInstNotify) {
      ctx.newInstNotify(innerCopy);
      ctx.newInstNotify(innerBorrow);
      ctx.newInstNotify(outerEndBorrow);
    }

    // Then set our borrowing operand to take our innerBorrow instead of value
    // (whose lifetime we just ended).
    borrowingOperand->set(innerBorrow);
    // Add our outer end borrow as a use point to make sure that we extend our
    // base value to this point.
    usePoints.push_back(&outerEndBorrow->getAllOperands()[0]);

    // Then check if in our destination block, we have further reborrows. If we
    // do, we need to recursively process them.
    auto *borrowedArg =
        const_cast<SILPhiArgument *>(bi->getArgForOperand(borrowingOperand));
    auto *baseArg =
        insertOwnedBaseValueAlongBranchEdge(bi, innerCopy, getCallbacks());
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
          if (ctx.newInstNotify)
            ctx.newInstNotify(dvi);
        });
        continue;
      }

      // Otherwise, we have a reborrow. For now our reborrows must be
      // phis. Add our owned value as a new argument of that phi along our
      // edge and undef along all other edges.
      auto borrowingOp = *BorrowingOperand::get(use);
      auto *brInst = cast<BranchInst>(borrowingOp.op->getUser());
      auto *newBorrowedPhi = brInst->getArgForOperand(borrowingOp);
      auto *newBasePhi =
          insertOwnedBaseValueAlongBranchEdge(brInst, baseArg, getCallbacks());
      baseBorrowedValuePair.emplace_back(newBasePhi, newBorrowedPhi);
    }
  }
}

void OwnershipRAUWUtility::rewriteReborrows(
    SILValue newBorrowedValue, ArrayRef<BorrowingOperand> foundReborrows) {
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
    if (ctx.newInstNotify) {
      ctx.newInstNotify(innerCopy);
      ctx.newInstNotify(innerBorrow);
      ctx.newInstNotify(outerEndBorrow);
    }

    reborrow->set(innerBorrow);

    auto *borrowedArg =
        const_cast<SILPhiArgument *>(bi->getArgForOperand(reborrow.op));
    auto *baseArg =
        insertOwnedBaseValueAlongBranchEdge(bi, innerCopy, getCallbacks());
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
          if (ctx.newInstNotify)
            ctx.newInstNotify(dvi);
        });
        continue;
      }

      // Otherwise, we have a reborrow. For now our reborrows must be
      // phis. Add our owned value as a new argument of that phi along our
      // edge and undef along all other edges.
      auto borrowingOp = *BorrowingOperand::get(use);
      auto *brInst = cast<BranchInst>(borrowingOp.op->getUser());
      auto *newBorrowedPhi = brInst->getArgForOperand(borrowingOp);
      auto *newBasePhi =
          insertOwnedBaseValueAlongBranchEdge(brInst, baseArg, getCallbacks());
      baseBorrowedValuePair.emplace_back(newBasePhi, newBorrowedPhi);
    }
  }
}

SILBasicBlock::iterator OwnershipRAUWUtility::handleUnowned() {
  switch (newValue.getOwnershipKind()) {
  case OwnershipKind::None:
    llvm_unreachable("Should have been handled elsewhere");
  case OwnershipKind::Any:
    llvm_unreachable("Invalid for values");
  case OwnershipKind::Unowned:
    // An unowned value can always be RAUWed with another unowned value.
    return replaceAllUsesAndEraseInner(oldValue, newValue, ctx.eraseNotify);
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
          if (ctx.newInstNotify)
            ctx.newInstNotify(newInst);
          use->set(newInst);
        }
      }
    }
    auto extender = getLifetimeExtender();
    SILValue borrow =
        extender.copyBorrowAndExtendForRAUW(newValue, oldValue->getUses());
    SILBuilderWithScope builder(oldValue);
    auto *newInst = builder.createUncheckedOwnershipConversion(
        oldValue->getLoc(), borrow, OwnershipKind::Unowned);
    if (ctx.newInstNotify)
      ctx.newInstNotify(newInst);
    return replaceAllUsesAndEraseInner(oldValue, newInst, ctx.eraseNotify);
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
          if (ctx.newInstNotify)
            ctx.newInstNotify(newInst);
          use->set(newInst);
        }
      }
    }
    auto extender = getLifetimeExtender();
    SILValue copy = extender.copyAndExtendForNonLifetimeEndingRAUW(
        newValue, oldValue->getUses());
    SILBuilderWithScope builder(oldValue);
    auto *newInst = builder.createUncheckedOwnershipConversion(
        oldValue->getLoc(), copy, OwnershipKind::Unowned);
    if (ctx.newInstNotify)
      ctx.newInstNotify(newInst);
    auto result =
        replaceAllUsesAndEraseInner(oldValue, newInst, ctx.eraseNotify);
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
  SmallVector<Operand *, 32> usePoints;
  SmallVector<BorrowingOperand, 8> recursiveBorrowScopeReborrows;
  getAllNonTrivialUsePointsOfBorrowedValue(oldValue, usePoints,
                                           recursiveBorrowScopeReborrows);

  // If we have any transitive reborrows on sub-borrows.
  if (recursiveBorrowScopeReborrows.size())
    eliminateReborrowsOfRecursiveBorrows(recursiveBorrowScopeReborrows,
                                         usePoints);

  auto extender = getLifetimeExtender();
  SILValue newBorrowedValue =
      extender.copyBorrowAndExtendForRAUW<ArrayRef<Operand *>>(newValue,
                                                               usePoints);

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
    if (oldValueBorrowedVal->gatherReborrows(foundReborrows)) {
      rewriteReborrows(newBorrowedValue, foundReborrows);
    }
  }

  // Then we need to look and see if our oldValue had any transitive uses that

  // Ok, we now have eliminated any reborrows if we had any. That means that
  // the uses of oldValue should be completely within the lifetime of our new
  // borrow.
  return replaceAllUsesAndEraseInner(oldValue, newBorrowedValue,
                                     ctx.eraseNotify);
}

SILBasicBlock::iterator OwnershipRAUWUtility::perform() {
  assert(oldValue->getFunction()->hasOwnership());
  assert(
      OwnershipFixupContext::canFixUpOwnershipForRAUW(oldValue, newValue) &&
      "Should have checked if can perform this operation before calling it?!");
  // If our new value is just none, we can pass anything to do it so just RAUW
  // and return.
  //
  // NOTE: This handles RAUWing with undef.
  if (newValue.getOwnershipKind() == OwnershipKind::None)
    return replaceAllUsesAndEraseInner(oldValue, newValue, ctx.eraseNotify);
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
    SILValue copy =
        extender.copyAndExtendForLifetimeEndingRAUW(newValue, oldValue);
    cleanupOperandsBeforeDeletion(oldValue, ctx.newInstNotify);
    auto result = replaceAllUsesAndEraseInner(oldValue, copy, ctx.eraseNotify);
    return result;
  }
  case OwnershipKind::Unowned: {
    return handleUnowned();
  }
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

//===----------------------------------------------------------------------===//
//                          Ownership Fixup Context
//===----------------------------------------------------------------------===//
//
// Top level entry points to RAUW code.
//
bool OwnershipFixupContext::canFixUpOwnershipForRAUW(
    const SingleValueInstruction *oldValue, SILValue newValue) {
  auto newOwnershipKind = newValue.getOwnershipKind();

  // If our new kind is ValueOwnershipKind::None, then we are fine. We
  // trivially support that. This check also ensures that we can always
  // replace any value with a ValueOwnershipKind::None value.
  if (newOwnershipKind == OwnershipKind::None)
    return true;

  // If we are in Raw SIL, just bail at this point. We do not support
  // ownership fixups.
  if (oldValue->getModule().getStage() == SILStage::Raw)
    return false;

  // If our old ownership kind is ValueOwnershipKind::None and our new kind is
  // not, we may need to do more work that has not been implemented yet. So
  // bail.
  //
  // Due to our requirement that types line up, this can only occur given a
  // non-trivial typed value with None ownership. This can only happen when
  // oldValue is a trivial payloaded or no-payload non-trivially typed
  // enum. That doesn't occur that often so we just bail on it today until we
  // implement this functionality.
  auto oldOwnershipKind = SILValue(oldValue).getOwnershipKind();
  if (oldOwnershipKind != OwnershipKind::None)
    return true;

  // Ok, we have an old ownership kind that is OwnershipKind::None and a new
  // ownership kind that is not OwnershipKind::None. In that case, for now, do
  // not perform this transform.
  return false;
}

SILBasicBlock::iterator
OwnershipFixupContext::replaceAllUsesAndEraseFixingOwnership(
    SingleValueInstruction *oldValue, SILValue newValue) {
  OwnershipRAUWUtility utility{oldValue, newValue, *this};
  return utility.perform();
}
