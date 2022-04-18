//===--- OwnershipUtils.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/OwnershipUtils.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/GraphNodeWorklist.h"
#include "swift/Basic/SmallPtrSetVector.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

bool swift::isValueAddressOrTrivial(SILValue v) {
  return v->getType().isAddress() ||
         v.getOwnershipKind() == OwnershipKind::None;
}

bool swift::canOpcodeForwardGuaranteedValues(SILValue value) {
  // If we have an argument from a transforming terminator, we can forward
  // guaranteed.
  if (auto *arg = dyn_cast<SILArgument>(value))
    if (auto *ti = arg->getSingleTerminator())
      if (ti->isTransformationTerminator())
        return OwnershipForwardingMixin::get(ti)->preservesOwnership();

  if (auto *inst = value->getDefiningInstruction())
    if (auto *mixin = OwnershipForwardingMixin::get(inst))
      return mixin->preservesOwnership() &&
             !isa<OwnedFirstArgForwardingSingleValueInst>(inst);

  return false;
}

bool swift::canOpcodeForwardGuaranteedValues(Operand *use) {
  if (auto *mixin = OwnershipForwardingMixin::get(use->getUser()))
    return mixin->preservesOwnership() &&
           !isa<OwnedFirstArgForwardingSingleValueInst>(use->getUser());
  return false;
}

bool swift::canOpcodeForwardOwnedValues(SILValue value) {
  // If we have a SILArgument and we are the successor block of a transforming
  // terminator, we are fine.
  if (auto *arg = dyn_cast<SILPhiArgument>(value))
    if (auto *predTerm = arg->getSingleTerminator())
      if (predTerm->isTransformationTerminator())
        return OwnershipForwardingMixin::get(predTerm)->preservesOwnership();

  if (auto *inst = value->getDefiningInstruction())
    if (auto *mixin = OwnershipForwardingMixin::get(inst))
      return mixin->preservesOwnership() &&
             !isa<GuaranteedFirstArgForwardingSingleValueInst>(inst);

  return false;
}

bool swift::canOpcodeForwardOwnedValues(Operand *use) {
  auto *user = use->getUser();
  if (auto *mixin = OwnershipForwardingMixin::get(user))
    return mixin->preservesOwnership() &&
           !isa<GuaranteedFirstArgForwardingSingleValueInst>(user);
  return false;
}

//===----------------------------------------------------------------------===//
//                 Guaranteed Use-Point (Lifetime) Discovery
//===----------------------------------------------------------------------===//

// Find all use points of \p guaranteedValue within its borrow scope. All uses
// are naturally dominated by \p guaranteedValue. If a PointerEscape is found,
// then no assumption can be made about \p guaranteedValue's lifetime. Therefore
// the use points are incomplete and this returns false.
//
// Accumulate results in \p usePoints, ignoring existing elements.
//
// Skip over nested borrow scopes. Their scope-ending instructions are their use
// points. Transitively find all nested scope-ending instructions by looking
// through nested reborrows. Nested reborrows are not use points.
//
// FIXME: handle inner reborrows, which aren't dominated by
// guaranteedValue. Audit all users to handle reborrows.
bool swift::findInnerTransitiveGuaranteedUses(
  SILValue guaranteedValue, SmallVectorImpl<Operand *> *usePoints) {

  bool foundPointerEscape = false;

  auto leafUse = [&](Operand *use) {
    if (usePoints && use->getOperandOwnership() != OperandOwnership::NonUse) {
      usePoints->push_back(use);
    }
    return true;
  };

  // Push the value's immediate uses.
  //
  // TODO: The worklist can be a simple vector without any a membership check if
  // destructures are changed to be represented as reborrows. Currently a
  // destructure forwards multiple results! This means that the worklist could
  // grow exponentially without the membership check. It's fine to do this
  // membership check locally in this function (within a borrow scope) because
  // it isn't needed for the immediate uses, only the transitive uses.
  GraphNodeWorklist<Operand *, 8> worklist;
  for (Operand *use : guaranteedValue->getUses()) {
    if (use->getOperandOwnership() != OperandOwnership::NonUse)
      worklist.insert(use);
  }

  // --- Transitively follow forwarded uses and look for escapes.

  // usePoints grows in this loop.
  while (Operand *use = worklist.pop()) {
    switch (use->getOperandOwnership()) {
    case OperandOwnership::NonUse:
    case OperandOwnership::TrivialUse:
    case OperandOwnership::ForwardingConsume:
    case OperandOwnership::DestroyingConsume:
      llvm_unreachable("this operand cannot handle an inner guaranteed use");

    case OperandOwnership::ForwardingUnowned:
    case OperandOwnership::PointerEscape:
      leafUse(use);
      foundPointerEscape = true;
      break;

    case OperandOwnership::InstantaneousUse:
    case OperandOwnership::UnownedInstantaneousUse:
    case OperandOwnership::BitwiseEscape:
    // Reborrow only happens when this is called on a value that creates a
    // borrow scope.
    case OperandOwnership::Reborrow:
    // EndBorrow either happens when this is called on a value that creates a
    // borrow scope, or when it is pushed as a use when processing a nested
    // borrow.
    case OperandOwnership::EndBorrow:
      leafUse(use);
      break;

    case OperandOwnership::InteriorPointer:
#if 0 // FIXME!!! Enable in a following commit that fixes RAUW
      // If our base guaranteed value does not have any consuming uses
      // (consider function arguments), we need to be sure to include interior
      // pointer operands since we may not get a use from a end_scope
      // instruction.
      if (InteriorPointerOperand(use).findTransitiveUses(usePoints)
          != AddressUseKind::NonEscaping) {
        foundPointerEscape = true;
      }
#endif
      leafUse(use);
      foundPointerEscape = true;
      break;

    case OperandOwnership::ForwardingBorrow: {
      bool nonLeaf = false;
      ForwardingOperand(use).visitForwardedValues([&](SILValue result) {
        // Do not include transitive uses with 'none' ownership
        if (result.getOwnershipKind() == OwnershipKind::None)
          return true;
        for (auto *resultUse : result->getUses()) {
          if (resultUse->getOperandOwnership() != OperandOwnership::NonUse) {
            nonLeaf = true;
            worklist.insert(resultUse);
          }
        }
        return true;
      });
      // e.g. A dead forwarded value, e.g. a switch_enum with only trivial uses,
      // must itself be a leaf use.
      if (!nonLeaf) {
        leafUse(use);
      }
      break;
    }
    case OperandOwnership::Borrow:
      // FIXME: Use visitExtendedScopeEndingUses and audit all clients to handle
      // reborrows.
      //
      // FIXME: visit[Extended]ScopeEndingUses can't return false here once dead
      // borrows are disallowed.
      if (!BorrowingOperand(use).visitScopeEndingUses([&](Operand *endUse) {
            if (endUse->getOperandOwnership() == OperandOwnership::Reborrow) {
              foundPointerEscape = true;
            }
            leafUse(endUse);
            return true;
          })) {
        // Special case for dead borrows. This is dangerous because clients
        // don't expect a begin_borrow to be in the use list.
        leafUse(use);
      }
      break;
    }
  }
  return !foundPointerEscape;
}

/// Find all uses in the extended lifetime (i.e. including copies) of a simple
/// (i.e. not reborrowed) borrow scope and its transitive uses.
bool swift::findExtendedUsesOfSimpleBorrowedValue(
    BorrowedValue borrowedValue, SmallVectorImpl<Operand *> *usePoints) {

  auto recordUse = [&](Operand *use) {
    if (usePoints && use->getOperandOwnership() != OperandOwnership::NonUse) {
      usePoints->push_back(use);
    }
  };

  // Push the value's immediate uses.
  //
  // TODO: The worklist can be a simple vector without any a membership check if
  // destructures are changed to be represented as reborrows. Currently a
  // destructure forwards multiple results! This means that the worklist could
  // grow exponentially without the membership check. It's fine to do this
  // membership check locally in this function (within a borrow scope) because
  // it isn't needed for the immediate uses, only the transitive uses.
  GraphNodeWorklist<Operand *, 8> worklist;
  auto addUsesToWorklist = [&worklist](SILValue value) {
    for (Operand *use : value->getUses()) {
      if (use->getOperandOwnership() != OperandOwnership::NonUse)
        worklist.insert(use);
    }
  };

  addUsesToWorklist(borrowedValue.value);

  // --- Transitively follow forwarded uses and look for escapes.

  // usePoints grows in this loop.
  while (Operand *use = worklist.pop()) {
    if (auto *cvi = dyn_cast<CopyValueInst>(use->getUser())) {
      addUsesToWorklist(cvi);
    }
    switch (use->getOperandOwnership()) {
    case OperandOwnership::NonUse:
      break;

    case OperandOwnership::TrivialUse:
    case OperandOwnership::ForwardingConsume:
    case OperandOwnership::DestroyingConsume:
      recordUse(use);
      break;

    case OperandOwnership::ForwardingUnowned:
    case OperandOwnership::PointerEscape:
      return false;

    case OperandOwnership::InstantaneousUse:
    case OperandOwnership::UnownedInstantaneousUse:
    case OperandOwnership::BitwiseEscape:
    // Reborrow only happens when this is called on a value that creates a
    // borrow scope.
    case OperandOwnership::Reborrow:
    // EndBorrow either happens when this is called on a value that creates a
    // borrow scope, or when it is pushed as a use when processing a nested
    // borrow.
    case OperandOwnership::EndBorrow:
      recordUse(use);
      break;

    case OperandOwnership::InteriorPointer:
      if (InteriorPointerOperandKind::get(use) ==
          InteriorPointerOperandKind::Invalid)
        return false;
      // If our base guaranteed value does not have any consuming uses (consider
      // function arguments), we need to be sure to include interior pointer
      // operands since we may not get a use from a end_scope instruction.
      if (InteriorPointerOperand(use).findTransitiveUses(usePoints) !=
          AddressUseKind::NonEscaping) {
        return false;
      }
      recordUse(use);
      break;

    case OperandOwnership::ForwardingBorrow: {
      ForwardingOperand(use).visitForwardedValues([&](SILValue result) {
        // Do not include transitive uses with 'none' ownership
        if (result.getOwnershipKind() == OwnershipKind::None)
          return true;
        for (auto *resultUse : result->getUses()) {
          if (resultUse->getOperandOwnership() != OperandOwnership::NonUse) {
            worklist.insert(resultUse);
          }
        }
        return true;
      });
      recordUse(use);
      break;
    }
    case OperandOwnership::Borrow:
      // FIXME: visitExtendedScopeEndingUses can't return false here once dead
      // borrows are disallowed.
      if (!BorrowingOperand(use).visitExtendedScopeEndingUses(
            [&](Operand *endUse) {
              recordUse(endUse);
              return true;
            })) {
        // Special case for dead borrows. This is dangerous because clients
        // don't expect a begin_borrow to be in the use list.
        recordUse(use);
      }
      break;
    }
  }
  return true;
}

bool swift::findUsesOfSimpleValue(SILValue value,
                                  SmallVectorImpl<Operand *> *usePoints) {
  for (auto *use : value->getUses()) {
    if (use->getOperandOwnership() == OperandOwnership::Borrow) {
      if (!BorrowingOperand(use).visitScopeEndingUses([&](Operand *end) {
            if (end->getOperandOwnership() == OperandOwnership::Reborrow) {
              return false;
            }
            usePoints->push_back(end);
            return true;
          })) {
        return false;
      }
    }
    usePoints->push_back(use);
  }
  return true;
}

// Find all use points of \p guaranteedValue within its borrow scope. All use
// points will be dominated by \p guaranteedValue.
//
// Record (non-nested) reborrows as uses.
//
// BorrowedValues (which introduce a borrow scope) are fundamentally different
// than "inner" guaranteed values. Their only use points are their scope-ending
// uses. There is no need to transitively process uses. However, unlike inner
// guaranteed values, they can have reborrows. To transitively process
// reborrows, use findExtendedTransitiveBorrowedUses.
bool swift::findTransitiveGuaranteedUses(
    SILValue guaranteedValue, SmallVectorImpl<Operand *> &usePoints,
    function_ref<void(Operand *)> visitReborrow) {

  // Handle local borrow introducers without following uses.
  // SILFunctionArguments are *not* borrow introducers in this context--we're
  // trying to find lifetime of values within a function.
  if (auto borrowedValue = BorrowedValue(guaranteedValue)) {
    if (borrowedValue.isLocalScope()) {
      borrowedValue.visitLocalScopeEndingUses([&](Operand *scopeEnd) {
        // Initially push the reborrow as a use point. visitReborrow may pop it
        // if it only wants to compute the extended lifetime's use points.
        usePoints.push_back(scopeEnd);
        if (scopeEnd->getOperandOwnership() == OperandOwnership::Reborrow)
          visitReborrow(scopeEnd);
        return true;
      });
    }
    return true;
  }
  return findInnerTransitiveGuaranteedUses(guaranteedValue, &usePoints);
}

// Find all use points of \p guaranteedValue within its borrow scope. If the
// guaranteed value introduces a borrow scope, then this includes the extended
// borrow scope by following reborrows.
bool swift::
findExtendedTransitiveGuaranteedUses(SILValue guaranteedValue,
                                     SmallVectorImpl<Operand *> &usePoints) {
  // Multiple paths may reach the same reborrows, and reborrow may even be
  // recursive, so the working set requires a membership check.
  SmallPtrSetVector<SILValue, 4> reborrows;
  auto visitReborrow = [&](Operand *reborrow) {
    // Pop the reborrow. It should not appear in the use points of the
    // extend lifetime.
    assert(reborrow == usePoints.back());
    usePoints.pop_back();
    auto borrowedPhi =
      BorrowingOperand(reborrow).getBorrowIntroducingUserResult();
    reborrows.insert(borrowedPhi.value);
  };
  if (!findTransitiveGuaranteedUses(guaranteedValue, usePoints, visitReborrow))
    return false;

  // For guaranteed values that do not introduce a borrow scope, reborrows will
  // be empty at this point.
  for (unsigned idx = 0; idx < reborrows.size(); ++idx) {
    bool result =
      findTransitiveGuaranteedUses(reborrows[idx], usePoints, visitReborrow);
    // It is impossible to find a Pointer escape while traversing reborrows.
    assert(result && "visiting reborrows always succeeds");
    (void)result;
  }
  return true;
}

//===----------------------------------------------------------------------===//
//                           Borrowing Operand
//===----------------------------------------------------------------------===//

void BorrowingOperandKind::print(llvm::raw_ostream &os) const {
  switch (value) {
  case Kind::Invalid:
    llvm_unreachable("Using an unreachable?!");
  case Kind::BeginBorrow:
    os << "BeginBorrow";
    return;
  case Kind::BeginApply:
    os << "BeginApply";
    return;
  case Kind::Branch:
    os << "Branch";
    return;
  case Kind::Apply:
    os << "Apply";
    return;
  case Kind::TryApply:
    os << "TryApply";
    return;
  case Kind::Yield:
    os << "Yield";
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     BorrowingOperandKind kind) {
  kind.print(os);
  return os;
}

void BorrowingOperand::print(llvm::raw_ostream &os) const {
  os << "BorrowScopeOperand:\n"
        "Kind: " << kind << "\n"
        "Value: " << op->get()
     << "User: " << *op->getUser();
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     const BorrowingOperand &operand) {
  operand.print(os);
  return os;
}

bool BorrowingOperand::hasEmptyRequiredEndingUses() const {
  switch (kind) {
  case BorrowingOperandKind::Invalid:
    llvm_unreachable("Using invalid case");
  case BorrowingOperandKind::BeginBorrow:
  case BorrowingOperandKind::BeginApply: {
    return op->getUser()->hasUsesOfAnyResult();
  }
  case BorrowingOperandKind::Branch: {
    auto *br = cast<BranchInst>(op->getUser());
    return br->getArgForOperand(op)->use_empty();
  }
  // These are instantaneous borrow scopes so there aren't any special end
  // scope instructions.
  case BorrowingOperandKind::Apply:
  case BorrowingOperandKind::TryApply:
  case BorrowingOperandKind::Yield:
    return false;
  }
  llvm_unreachable("Covered switch isn't covered");
}

bool BorrowingOperand::visitScopeEndingUses(
    function_ref<bool(Operand *)> func) const {
  switch (kind) {
  case BorrowingOperandKind::Invalid:
    llvm_unreachable("Using invalid case");
  case BorrowingOperandKind::BeginBorrow: {
    bool deadBorrow = true;
    for (auto *use : cast<BeginBorrowInst>(op->getUser())->getUses()) {
      if (use->isLifetimeEnding()) {
        deadBorrow = false;
        if (!func(use))
          return false;
      }
    }
    // FIXME: special case for dead borrows. This is dangerous because clients
    // only expect visitScopeEndingUses to return false if the visitor returned
    // false.
    return !deadBorrow;
  }
  case BorrowingOperandKind::BeginApply: {
    bool deadApply = true;
    auto *user = cast<BeginApplyInst>(op->getUser());
    for (auto *use : user->getTokenResult()->getUses()) {
      deadApply = false;
      if (!func(use))
        return false;
    }
    return !deadApply;
  }
  // These are instantaneous borrow scopes so there aren't any special end
  // scope instructions.
  case BorrowingOperandKind::Apply:
  case BorrowingOperandKind::TryApply:
  case BorrowingOperandKind::Yield:
    return true;
  case BorrowingOperandKind::Branch: {
    bool deadBranch = true;
    auto *br = cast<BranchInst>(op->getUser());
    for (auto *use : br->getArgForOperand(op)->getUses()) {
      if (use->isLifetimeEnding()) {
        deadBranch = false;
        if (!func(use))
          return false;
      }
    }
    return !deadBranch;
  }
  }
  llvm_unreachable("Covered switch isn't covered");
}

bool BorrowingOperand::visitExtendedScopeEndingUses(
    function_ref<bool(Operand *)> visitor) const {

  if (hasBorrowIntroducingUser()) {
    return visitBorrowIntroducingUserResults(
        [visitor](BorrowedValue borrowedValue) {
          return borrowedValue.visitExtendedScopeEndingUses(visitor);
        });
  }
  return visitScopeEndingUses(visitor);
}

bool BorrowingOperand::visitBorrowIntroducingUserResults(
    function_ref<bool(BorrowedValue)> visitor) const {
  switch (kind) {
  case BorrowingOperandKind::Invalid:
    llvm_unreachable("Using invalid case");
  case BorrowingOperandKind::Apply:
  case BorrowingOperandKind::TryApply:
  case BorrowingOperandKind::BeginApply:
  case BorrowingOperandKind::Yield:
    llvm_unreachable("Never has borrow introducer results!");
  case BorrowingOperandKind::BeginBorrow: {
    auto value = BorrowedValue(cast<BeginBorrowInst>(op->getUser()));
    assert(value);
    return visitor(value);
  }
  case BorrowingOperandKind::Branch: {
    auto *bi = cast<BranchInst>(op->getUser());
    auto value = BorrowedValue(
        bi->getDestBB()->getArgument(op->getOperandNumber()));
    assert(value && "guaranteed-to-unowned conversion not allowed on branches");
    return visitor(value);
  }
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

BorrowedValue BorrowingOperand::getBorrowIntroducingUserResult() {
  switch (kind) {
  case BorrowingOperandKind::Invalid:
  case BorrowingOperandKind::Apply:
  case BorrowingOperandKind::TryApply:
  case BorrowingOperandKind::BeginApply:
  case BorrowingOperandKind::Yield:
    return BorrowedValue();

  case BorrowingOperandKind::BeginBorrow:
    return BorrowedValue(cast<BeginBorrowInst>(op->getUser()));

  case BorrowingOperandKind::Branch: {
    auto *bi = cast<BranchInst>(op->getUser());
    return BorrowedValue(bi->getDestBB()->getArgument(op->getOperandNumber()));
  }
  }
  llvm_unreachable("covered switch");
}

void BorrowingOperand::getImplicitUses(
    SmallVectorImpl<Operand *> &foundUses,
    std::function<void(Operand *)> *errorFunction) const {
  // FIXME: this visitScopeEndingUses should never return false once dead
  // borrows are disallowed.
  if (!visitScopeEndingUses([&](Operand *endOp) {
    foundUses.push_back(endOp);
    return true;
  })) {
    // Special-case for dead borrows.
    foundUses.push_back(op);
  }
}

//===----------------------------------------------------------------------===//
//                             Borrow Introducers
//===----------------------------------------------------------------------===//

void BorrowedValueKind::print(llvm::raw_ostream &os) const {
  switch (value) {
  case BorrowedValueKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  case BorrowedValueKind::SILFunctionArgument:
    os << "SILFunctionArgument";
    return;
  case BorrowedValueKind::BeginBorrow:
    os << "BeginBorrowInst";
    return;
  case BorrowedValueKind::LoadBorrow:
    os << "LoadBorrowInst";
    return;
  case BorrowedValueKind::Phi:
    os << "Phi";
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

void BorrowedValue::print(llvm::raw_ostream &os) const {
  os << "BorrowScopeIntroducingValue:\n"
    "Kind: " << kind << "\n"
    "Value: " << value;
}

void BorrowedValue::getLocalScopeEndingInstructions(
    SmallVectorImpl<SILInstruction *> &scopeEndingInsts) const {
  assert(isLocalScope() && "Should only call this given a local scope");

  switch (kind) {
  case BorrowedValueKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  case BorrowedValueKind::SILFunctionArgument:
    llvm_unreachable("Should only call this with a local scope");
  case BorrowedValueKind::BeginBorrow:
  case BorrowedValueKind::LoadBorrow:
  case BorrowedValueKind::Phi:
    for (auto *use : value->getUses()) {
      if (use->isLifetimeEnding()) {
        scopeEndingInsts.push_back(use->getUser());
      }
    }
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

// Note: BorrowedLifetimeExtender assumes no intermediate values between a
// borrow introducer and its reborrow. The borrowed value must be an operand of
// the reborrow.
bool BorrowedValue::visitLocalScopeEndingUses(
    function_ref<bool(Operand *)> visitor) const {
  assert(isLocalScope() && "Should only call this given a local scope");
  switch (kind) {
  case BorrowedValueKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  case BorrowedValueKind::SILFunctionArgument:
    llvm_unreachable("Should only call this with a local scope");
  case BorrowedValueKind::LoadBorrow:
  case BorrowedValueKind::BeginBorrow:
  case BorrowedValueKind::Phi:
    for (auto *use : value->getUses()) {
      if (use->isLifetimeEnding()) {
        if (!visitor(use))
          return false;
      }
    }
    return true;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     BorrowedValueKind kind) {
  kind.print(os);
  return os;
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     const BorrowedValue &value) {
  value.print(os);
  return os;
}

/// Add this scopes live blocks into the PrunedLiveness result.
void BorrowedValue::computeLiveness(PrunedLiveness &liveness) const {
  liveness.initializeDefBlock(value->getParentBlock());
  visitLocalScopeEndingUses([&](Operand *endOp) {
    liveness.updateForUse(endOp->getUser(), true);
    return true;
  });
}

bool BorrowedValue::areUsesWithinLocalScope(
    ArrayRef<Operand *> uses, DeadEndBlocks *deadEndBlocks) const {
  // First make sure that we actually have a local scope. If we have a non-local
  // scope, then we have something (like a SILFunctionArgument) where a larger
  // semantic construct (in the case of SILFunctionArgument, the function
  // itself) acts as the scope. So we already know that our passed in
  // instructions must be in the same scope.
  if (!isLocalScope())
    return true;

  // Compute the local scope's liveness.
  PrunedLiveness liveness;
  computeLiveness(liveness);
  return liveness.areUsesWithinBoundary(uses, deadEndBlocks);
}

// The visitor \p func is only called on final scope-ending uses, not reborrows.
bool BorrowedValue::visitExtendedScopeEndingUses(
    function_ref<bool(Operand *)> visitor) const {
  assert(isLocalScope());

  SmallPtrSetVector<SILValue, 4> reborrows;

  auto visitEnd = [&](Operand *scopeEndingUse) {
    if (scopeEndingUse->getOperandOwnership() == OperandOwnership::Reborrow) {
      BorrowingOperand(scopeEndingUse).visitBorrowIntroducingUserResults(
        [&](BorrowedValue borrowedValue) {
          reborrows.insert(borrowedValue.value);
          return true;
        });
      return true;
    }
    return visitor(scopeEndingUse);
  };

  if (!visitLocalScopeEndingUses(visitEnd))
    return false;

  // reborrows grows in this loop.
  for (unsigned idx = 0; idx < reborrows.size(); ++idx) {
    if (!BorrowedValue(reborrows[idx]).visitLocalScopeEndingUses(visitEnd))
      return false;
  }
  return true;
}

bool BorrowedValue::visitInteriorPointerOperandHelper(
    function_ref<void(InteriorPointerOperand)> func,
    BorrowedValue::InteriorPointerOperandVisitorKind kind) const {
  using Kind = BorrowedValue::InteriorPointerOperandVisitorKind;

  SmallVector<Operand *, 32> worklist(value->getUses());
  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();

    if (auto interiorPointer = InteriorPointerOperand(op)) {
      func(interiorPointer);
      continue;
    }

    if (auto borrowingOperand = BorrowingOperand(op)) {
      switch (kind) {
      case Kind::NoNestedNoReborrows:
        // We do not look through nested things and or reborrows, so just
        // continue.
        continue;
      case Kind::YesNestedNoReborrows:
        // We only look through nested borrowing operands, we never look through
        // reborrows though.
        if (borrowingOperand.isReborrow())
          continue;
        break;
      case Kind::YesNestedYesReborrows:
        // Look through everything!
        break;
      }

      borrowingOperand.visitBorrowIntroducingUserResults([&](auto bv) {
        for (auto *use : bv->getUses()) {
          if (auto intPtrOperand = InteriorPointerOperand(use)) {
            func(intPtrOperand);
            continue;
          }
          worklist.push_back(use);
        }
        return true;
      });
      continue;
    }

    auto *user = op->getUser();
    if (isa<DebugValueInst>(user) || isa<SuperMethodInst>(user) ||
        isa<ClassMethodInst>(user) || isa<CopyValueInst>(user) ||
        isa<EndBorrowInst>(user) || isa<ApplyInst>(user) ||
        isa<StoreInst>(user) || isa<PartialApplyInst>(user) ||
        isa<UnmanagedRetainValueInst>(user) ||
        isa<UnmanagedReleaseValueInst>(user) ||
        isa<UnmanagedAutoreleaseValueInst>(user)) {
      continue;
    }

    // These are interior pointers that have not had support yet added for them.
    if (isa<ProjectExistentialBoxInst>(user)) {
      continue;
    }

    // Look through object.
    if (auto *svi = dyn_cast<SingleValueInstruction>(user)) {
      if (Projection::isObjectProjection(svi)) {
        for (SILValue result : user->getResults()) {
          llvm::copy(result->getUses(), std::back_inserter(worklist));
        }
        continue;
      }
    }

    return false;
  }

  return true;
}

AddressUseKind
swift::findTransitiveUsesForAddress(SILValue projectedAddress,
                                    SmallVectorImpl<Operand *> *foundUses,
                                    std::function<void(Operand *)> *onError) {
  // If the projectedAddress is dead, it is itself a leaf use. Since we don't
  // have an operand for it, simply bail. Dead projectedAddress is unexpected.
  //
  // TODO: store_borrow is currently an InteriorPointer with no uses, so we end
  // up bailing. It should be in a dependence scope instead. It's not clear why
  // it produces an address at all.
  if (projectedAddress->use_empty())
    return AddressUseKind::PointerEscape;

  SmallVector<Operand *, 8> worklist(projectedAddress->getUses());

  AddressUseKind result = AddressUseKind::NonEscaping;

  auto leafUse = [foundUses](Operand *use) {
    if (foundUses)
      foundUses->push_back(use);
  };
  auto transitiveResultUses = [&](Operand *use) {
    auto *svi = cast<SingleValueInstruction>(use->getUser());
    if (svi->use_empty()) {
      leafUse(use);
    } else {
      worklist.append(svi->use_begin(), svi->use_end());
    }
  };

  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();

    // Skip type dependent operands.
    if (op->isTypeDependent())
      continue;

    // Then update the worklist with new things to find if we recognize this
    // inst and then continue. If we fail, we emit an error at the bottom of the
    // loop that we didn't recognize the user.
    auto *user = op->getUser();

    // TODO: Partial apply should be NonEscaping, but then we need to consider
    // the apply to be a use point.
    if (isa<PartialApplyInst>(user) || isa<AddressToPointerInst>(user)) {
      result = meet(result, AddressUseKind::PointerEscape);
      continue;
    }
    // First, eliminate "end point uses" that we just need to check liveness at
    // and do not need to check transitive uses of.
    if (isa<LoadInst>(user) || isa<CopyAddrInst>(user) ||
        isa<MarkUnresolvedMoveAddrInst>(user) || isIncidentalUse(user) ||
        isa<StoreInst>(user) || isa<DestroyAddrInst>(user) ||
        isa<AssignInst>(user) || isa<YieldInst>(user) ||
        isa<LoadUnownedInst>(user) || isa<StoreUnownedInst>(user) ||
        isa<EndApplyInst>(user) || isa<LoadWeakInst>(user) ||
        isa<StoreWeakInst>(user) || isa<AssignByWrapperInst>(user) ||
        isa<BeginUnpairedAccessInst>(user) ||
        isa<EndUnpairedAccessInst>(user) || isa<WitnessMethodInst>(user) ||
        isa<SwitchEnumAddrInst>(user) || isa<CheckedCastAddrBranchInst>(user) ||
        isa<SelectEnumAddrInst>(user) || isa<InjectEnumAddrInst>(user) ||
        isa<IsUniqueInst>(user)) {
      leafUse(op);
      continue;
    }

    if (isa<UnconditionalCheckedCastAddrInst>(user)
        || isa<MarkFunctionEscapeInst>(user)) {
      assert(!user->hasResults());
      continue;
    }

    // Then handle users that we need to look at transitive uses of.
    if (Projection::isAddressProjection(user) ||
        isa<ProjectBlockStorageInst>(user) ||
        isa<OpenExistentialAddrInst>(user) ||
        isa<InitExistentialAddrInst>(user) || isa<InitEnumDataAddrInst>(user) ||
        isa<BeginAccessInst>(user) || isa<TailAddrInst>(user) ||
        isa<IndexAddrInst>(user) || isa<StoreBorrowInst>(user) ||
        isa<UncheckedAddrCastInst>(user)) {
      transitiveResultUses(op);
      continue;
    }

    if (auto *builtin = dyn_cast<BuiltinInst>(user)) {
      if (auto kind = builtin->getBuiltinKind()) {
        if (*kind == BuiltinValueKind::TSanInoutAccess) {
          leafUse(op);
          continue;
        }
      }
    }

    // If we have a load_borrow, add it's end scope to the liveness requirement.
    if (auto *lbi = dyn_cast<LoadBorrowInst>(user)) {
      if (foundUses) {
        for (Operand *use : lbi->getUses()) {
          if (use->endsLocalBorrowScope()) {
            leafUse(use);
          }
        }
      }
      continue;
    }

    // TODO: Merge this into the full apply site code below.
    if (auto *beginApply = dyn_cast<BeginApplyInst>(user)) {
      if (foundUses) {
        // TODO: the empty check should not be needed when dead begin_apply is
        // disallowed.
        if (beginApply->getTokenResult()->use_empty()) {
          leafUse(op);
        } else {
          llvm::copy(beginApply->getTokenResult()->getUses(),
                     std::back_inserter(*foundUses));
        }
      }
      continue;
    }

    if (auto fas = FullApplySite::isa(user)) {
      leafUse(op);
      continue;
    }

    if (auto *mdi = dyn_cast<MarkDependenceInst>(user)) {
      // If this is the base, just treat it as a liveness use.
      if (op->get() == mdi->getBase()) {
        leafUse(op);
        continue;
      }

      // If we are the value use, look through it.
      transitiveResultUses(op);
      continue;
    }

    // We were unable to recognize this user, so return true that we failed.
    if (onError) {
      (*onError)(op);
    }
    result = AddressUseKind::Unknown;
  }
  return result;
}

//===----------------------------------------------------------------------===//
//                              AddressOwnership
//===----------------------------------------------------------------------===//

bool AddressOwnership::areUsesWithinLifetime(
    ArrayRef<Operand *> uses, DeadEndBlocks &deadEndBlocks) const {
  if (!base.hasLocalOwnershipLifetime())
    return true;

  SILValue root = base.getOwnershipReferenceRoot();
  BorrowedValue borrow(root);
  if (borrow)
    return borrow.areUsesWithinLocalScope(uses, &deadEndBlocks);

  // --- A reference no borrow scope. Currently happens for project_box.

  // Compute the reference value's liveness.
  PrunedLiveness liveness;
  liveness.computeSSALiveness(root);
  return liveness.areUsesWithinBoundary(uses, &deadEndBlocks);
}

//===----------------------------------------------------------------------===//
//                          Owned Value Introducers
//===----------------------------------------------------------------------===//

void OwnedValueIntroducerKind::print(llvm::raw_ostream &os) const {
  switch (value) {
  case OwnedValueIntroducerKind::Invalid:
    llvm_unreachable("Using invalid case?!");
  case OwnedValueIntroducerKind::Apply:
    os << "Apply";
    return;
  case OwnedValueIntroducerKind::BeginApply:
    os << "BeginApply";
    return;
  case OwnedValueIntroducerKind::TryApply:
    os << "TryApply";
    return;
  case OwnedValueIntroducerKind::Copy:
    os << "Copy";
    return;
  case OwnedValueIntroducerKind::LoadCopy:
    os << "LoadCopy";
    return;
  case OwnedValueIntroducerKind::LoadTake:
    os << "LoadTake";
    return;
  case OwnedValueIntroducerKind::Phi:
    os << "Phi";
    return;
  case OwnedValueIntroducerKind::Struct:
    os << "Struct";
    return;
  case OwnedValueIntroducerKind::Tuple:
    os << "Tuple";
    return;
  case OwnedValueIntroducerKind::FunctionArgument:
    os << "FunctionArgument";
    return;
  case OwnedValueIntroducerKind::PartialApplyInit:
    os << "PartialApplyInit";
    return;
  case OwnedValueIntroducerKind::AllocBoxInit:
    os << "AllocBoxInit";
    return;
  case OwnedValueIntroducerKind::AllocRefInit:
    os << "AllocRefInit";
    return;
  }
  llvm_unreachable("Covered switch isn't covered");
}

//===----------------------------------------------------------------------===//
//                       Introducer Searching Routines
//===----------------------------------------------------------------------===//

bool swift::getAllBorrowIntroducingValues(SILValue inputValue,
                                          SmallVectorImpl<BorrowedValue> &out) {
  if (inputValue.getOwnershipKind() != OwnershipKind::Guaranteed)
    return false;

  SmallVector<SILValue, 32> worklist;
  worklist.emplace_back(inputValue);

  while (!worklist.empty()) {
    SILValue value = worklist.pop_back_val();

    // First check if v is an introducer. If so, stash it and continue.
    if (auto scopeIntroducer = BorrowedValue(value)) {
      out.push_back(scopeIntroducer);
      continue;
    }

    // If v produces .none ownership, then we can ignore it. It is important
    // that we put this before checking for guaranteed forwarding instructions,
    // since we want to ignore guaranteed forwarding instructions that in this
    // specific case produce a .none value.
    if (value.getOwnershipKind() == OwnershipKind::None)
      continue;

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isForwardingBorrow(value)) {
      if (auto *i = value->getDefiningInstruction()) {
        llvm::copy(i->getNonTypeDependentOperandValues(),
                   std::back_inserter(worklist));
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator.
      auto *arg = cast<SILPhiArgument>(value);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator() &&
             OwnershipForwardingMixin::get(termInst)->preservesOwnership());
      assert(termInst->getNumOperands() == 1 &&
             "Transforming terminators should always have a single operand");
      worklist.push_back(termInst->getAllOperands()[0].get());
      continue;
    }

    // Otherwise, this is an introducer we do not understand. Bail and return
    // false.
    return false;
  }

  return true;
}

// FIXME: replace this logic with AccessBase::findOwnershipReferenceRoot.
BorrowedValue swift::getSingleBorrowIntroducingValue(SILValue inputValue) {
  if (inputValue.getOwnershipKind() != OwnershipKind::Guaranteed)
    return {};

  SILValue currentValue = inputValue;
  while (true) {
    // First check if our initial value is an introducer. If we have one, just
    // return it.
    if (auto scopeIntroducer = BorrowedValue(currentValue)) {
      return scopeIntroducer;
    }

    if (currentValue.getOwnershipKind() == OwnershipKind::None)
      return {};

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isForwardingBorrow(currentValue)) {
      if (auto *i = currentValue->getDefiningInstruction()) {
        auto instOps = i->getNonTypeDependentOperandValues();
        // If we have multiple incoming values, return .None. We can't handle
        // this.
        auto begin = instOps.begin();
        if (std::next(begin) != instOps.end()) {
          return {};
        }
        // Otherwise, set currentOp to the single operand and continue.
        currentValue = *begin;
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator.
      auto *arg = cast<SILPhiArgument>(currentValue);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator() &&
             OwnershipForwardingMixin::get(termInst)->preservesOwnership());
      assert(termInst->getNumOperands() == 1 &&
             "Transformation terminators should only have single operands");
      currentValue = termInst->getAllOperands()[0].get();
      continue;
    }

    // Otherwise, this is an introducer we do not understand. Bail and return
    // None.
    return {};
  }

  llvm_unreachable("Should never hit this");
}

bool swift::getAllOwnedValueIntroducers(
    SILValue inputValue, SmallVectorImpl<OwnedValueIntroducer> &out) {
  if (inputValue.getOwnershipKind() != OwnershipKind::Owned)
    return false;

  SmallVector<SILValue, 32> worklist;
  worklist.emplace_back(inputValue);

  while (!worklist.empty()) {
    SILValue value = worklist.pop_back_val();

    // First check if v is an introducer. If so, stash it and continue.
    if (auto introducer = OwnedValueIntroducer::get(value)) {
      out.push_back(introducer);
      continue;
    }

    // If v produces .none ownership, then we can ignore it. It is important
    // that we put this before checking for guaranteed forwarding instructions,
    // since we want to ignore guaranteed forwarding instructions that in this
    // specific case produce a .none value.
    if (value.getOwnershipKind() == OwnershipKind::None)
      continue;

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isForwardingConsume(value)) {
      if (auto *i = value->getDefiningInstruction()) {
        llvm::copy(i->getNonTypeDependentOperandValues(),
                   std::back_inserter(worklist));
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator.
      auto *arg = cast<SILPhiArgument>(value);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator() &&
             OwnershipForwardingMixin::get(termInst)->preservesOwnership());
      assert(termInst->getNumOperands() == 1 &&
             "Transforming terminators should always have a single operand");
      worklist.push_back(termInst->getAllOperands()[0].get());
      continue;
    }

    // Otherwise, this is an introducer we do not understand. Bail and return
    // false.
    return false;
  }

  return true;
}

OwnedValueIntroducer swift::getSingleOwnedValueIntroducer(SILValue inputValue) {
  if (inputValue.getOwnershipKind() != OwnershipKind::Owned)
    return {};

  SILValue currentValue = inputValue;
  while (true) {
    // First check if our initial value is an introducer. If we have one, just
    // return it.
    if (auto introducer = OwnedValueIntroducer::get(currentValue)) {
      return introducer;
    }

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isForwardingConsume(currentValue)) {
      if (auto *i = currentValue->getDefiningInstruction()) {
        auto instOps = i->getNonTypeDependentOperandValues();
        // If we have multiple incoming values, return .None. We can't handle
        // this.
        auto begin = instOps.begin();
        if (std::next(begin) != instOps.end()) {
          return {};
        }
        // Otherwise, set currentOp to the single operand and continue.
        currentValue = *begin;
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator and is directly forwarding.
      auto *arg = cast<SILPhiArgument>(currentValue);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator() &&
             OwnershipForwardingMixin::get(termInst)->preservesOwnership());
      assert(termInst->getNumOperands()
             - termInst->getNumTypeDependentOperands() == 1 &&
             "Transformation terminators should only have single operands");
      currentValue = termInst->getAllOperands()[0].get();
      continue;
    }

    // Otherwise, this is an introducer we do not understand. Bail and return
    // None.
    return {};
  }

  llvm_unreachable("Should never hit this");
}

//===----------------------------------------------------------------------===//
//                             Forwarding Operand
//===----------------------------------------------------------------------===//

ForwardingOperand::ForwardingOperand(Operand *use) {
  if (use->isTypeDependent())
    return;

  if (!OwnershipForwardingMixin::isa(use->getUser())) {
    return;
  }
#ifndef NDEBUG
  switch (use->getOperandOwnership()) {
  case OperandOwnership::ForwardingUnowned:
  case OperandOwnership::ForwardingConsume:
  case OperandOwnership::ForwardingBorrow:
    break;
  case OperandOwnership::NonUse:
  case OperandOwnership::TrivialUse:
  case OperandOwnership::InstantaneousUse:
  case OperandOwnership::UnownedInstantaneousUse:
  case OperandOwnership::PointerEscape:
  case OperandOwnership::BitwiseEscape:
  case OperandOwnership::Borrow:
  case OperandOwnership::DestroyingConsume:
  case OperandOwnership::InteriorPointer:
  case OperandOwnership::EndBorrow:
  case OperandOwnership::Reborrow:
    llvm_unreachable("this isn't the operand being forwarding!");
  }
#endif
  this->use = use;
}

ValueOwnershipKind ForwardingOperand::getForwardingOwnershipKind() const {
  auto *user = use->getUser();

  // NOTE: This if chain is meant to be a covered switch, so make sure to return
  // in each if itself since we have an unreachable at the bottom to ensure if a
  // new subclass of OwnershipForwardingInst is added
  if (auto *ofsvi = dyn_cast<AllArgOwnershipForwardingSingleValueInst>(user))
    return ofsvi->getForwardingOwnershipKind();

  if (auto *ofsvi = dyn_cast<FirstArgOwnershipForwardingSingleValueInst>(user))
    return ofsvi->getForwardingOwnershipKind();

  if (auto *ofci = dyn_cast<OwnershipForwardingConversionInst>(user))
    return ofci->getForwardingOwnershipKind();

  if (auto *ofseib = dyn_cast<OwnershipForwardingSelectEnumInstBase>(user))
    return ofseib->getForwardingOwnershipKind();

  if (auto *ofmvi =
          dyn_cast<OwnershipForwardingMultipleValueInstruction>(user)) {
    assert(ofmvi->getNumOperands() == 1);
    return ofmvi->getForwardingOwnershipKind();
  }

  if (auto *ofti = dyn_cast<OwnershipForwardingTermInst>(user)) {
    assert(ofti->getNumOperands() == 1);
    return ofti->getForwardingOwnershipKind();
  }

  llvm_unreachable("Unhandled forwarding inst?!");
}

void ForwardingOperand::setForwardingOwnershipKind(
    ValueOwnershipKind newKind) const {
  auto *user = use->getUser();
  // NOTE: This if chain is meant to be a covered switch, so make sure to return
  // in each if itself since we have an unreachable at the bottom to ensure if a
  // new subclass of OwnershipForwardingInst is added
  if (auto *ofsvi = dyn_cast<AllArgOwnershipForwardingSingleValueInst>(user))
    return ofsvi->setForwardingOwnershipKind(newKind);
  if (auto *ofsvi = dyn_cast<FirstArgOwnershipForwardingSingleValueInst>(user))
    return ofsvi->setForwardingOwnershipKind(newKind);
  if (auto *ofci = dyn_cast<OwnershipForwardingConversionInst>(user))
    return ofci->setForwardingOwnershipKind(newKind);
  if (auto *ofseib = dyn_cast<OwnershipForwardingSelectEnumInstBase>(user))
    return ofseib->setForwardingOwnershipKind(newKind);
  if (auto *ofmvi = dyn_cast<OwnershipForwardingMultipleValueInstruction>(user)) {
    assert(ofmvi->getNumOperands() == 1);
    if (!ofmvi->getOperand(0)->getType().isTrivial(*ofmvi->getFunction())) {
      ofmvi->setForwardingOwnershipKind(newKind);
      // TODO: Refactor this better.
      if (auto *dsi = dyn_cast<DestructureStructInst>(ofmvi)) {
        for (auto &result : dsi->getAllResultsBuffer()) {
          if (result.getType().isTrivial(*dsi->getFunction()))
            continue;
          result.setOwnershipKind(newKind);
        }
      } else {
        auto *dti = cast<DestructureTupleInst>(ofmvi);
        for (auto &result : dti->getAllResultsBuffer()) {
          if (result.getType().isTrivial(*dti->getFunction()))
            continue;
          result.setOwnershipKind(newKind);
        }
      }
    }
    return;
  }

  if (auto *ofti = dyn_cast<OwnershipForwardingTermInst>(user)) {
    assert(ofti->getNumOperands() == 1);
    if (!ofti->getOperand()->getType().isTrivial(*ofti->getFunction())) {
      ofti->setForwardingOwnershipKind(newKind);

      // Then convert all of its incoming values that are owned to be guaranteed.
      for (auto &succ : ofti->getSuccessors()) {
        auto *succBlock = succ.getBB();

        // If we do not have any arguments, then continue.
        if (succBlock->args_empty())
          continue;

        for (auto *succArg : succBlock->getSILPhiArguments()) {
          // If we have an any value, just continue.
          if (!succArg->getType().isTrivial(*ofti->getFunction()))
            continue;
          succArg->setOwnershipKind(newKind);
        }
      }
    }
    return;
  }

  llvm_unreachable("Out of sync with OperandOwnership");
}

void ForwardingOperand::replaceOwnershipKind(ValueOwnershipKind oldKind,
                                             ValueOwnershipKind newKind) const {
  auto *user = use->getUser();

  if (auto *fInst = dyn_cast<AllArgOwnershipForwardingSingleValueInst>(user))
    if (fInst->getForwardingOwnershipKind() == oldKind)
      return fInst->setForwardingOwnershipKind(newKind);

  if (auto *fInst = dyn_cast<FirstArgOwnershipForwardingSingleValueInst>(user))
    if (fInst->getForwardingOwnershipKind() == oldKind)
      return fInst->setForwardingOwnershipKind(newKind);

  if (auto *ofci = dyn_cast<OwnershipForwardingConversionInst>(user))
    if (ofci->getForwardingOwnershipKind() == oldKind)
      return ofci->setForwardingOwnershipKind(newKind);

  if (auto *ofseib = dyn_cast<OwnershipForwardingSelectEnumInstBase>(user))
    if (ofseib->getForwardingOwnershipKind() == oldKind)
      return ofseib->setForwardingOwnershipKind(newKind);

  if (auto *ofmvi = dyn_cast<OwnershipForwardingMultipleValueInstruction>(user)) {
    if (ofmvi->getForwardingOwnershipKind() == oldKind) {
      ofmvi->setForwardingOwnershipKind(newKind);
    }
    // TODO: Refactor this better.
    if (auto *dsi = dyn_cast<DestructureStructInst>(ofmvi)) {
      for (auto &result : dsi->getAllResultsBuffer()) {
        if (result.getOwnershipKind() != oldKind)
          continue;
        result.setOwnershipKind(newKind);
      }
    } else {
      auto *dti = cast<DestructureTupleInst>(ofmvi);
      for (auto &result : dti->getAllResultsBuffer()) {
        if (result.getOwnershipKind() != oldKind)
          continue;
        result.setOwnershipKind(newKind);
      }
    }
    return;
  }

  if (auto *ofti = dyn_cast<OwnershipForwardingTermInst>(user)) {
    if (ofti->getForwardingOwnershipKind() == oldKind) {
      ofti->setForwardingOwnershipKind(newKind);
      // Then convert all of its incoming values that are owned to be guaranteed.
      for (auto &succ : ofti->getSuccessors()) {
        auto *succBlock = succ.getBB();

        // If we do not have any arguments, then continue.
        if (succBlock->args_empty())
          continue;

        for (auto *succArg : succBlock->getSILPhiArguments()) {
          // If we have an any value, just continue.
          if (succArg->getOwnershipKind() == oldKind) {
            succArg->setOwnershipKind(newKind);
          }
        }
      }
    }
    return;
  }

  llvm_unreachable("Missing Case! Out of sync with OperandOwnership");
}

SILValue ForwardingOperand::getSingleForwardedValue() const {
  if (auto *svi = dyn_cast<SingleValueInstruction>(use->getUser()))
    return svi;
  return SILValue();
}

bool ForwardingOperand::visitForwardedValues(
    function_ref<bool(SILValue)> visitor) {
  auto *user = use->getUser();

  // See if we have a single value instruction... if we do that is always the
  // transitive result.
  if (auto *svi = dyn_cast<SingleValueInstruction>(user)) {
    return visitor(svi);
  }

  if (auto *mvri = dyn_cast<MultipleValueInstruction>(user)) {
    return llvm::all_of(mvri->getResults(), [&](SILValue value) {
      if (value.getOwnershipKind() == OwnershipKind::None)
        return true;
      return visitor(value);
    });
  }

  // This is an instruction like switch_enum and checked_cast_br that are
  // "transforming terminators"... We know that this means that we should at
  // most have a single phi argument.
  auto *ti = cast<TermInst>(user);
  return llvm::all_of(ti->getSuccessorBlocks(), [&](SILBasicBlock *succBlock) {
    // If we do not have any arguments, then continue.
    if (succBlock->args_empty())
      return true;

    auto args = succBlock->getSILPhiArguments();
    assert(args.size() == 1 && "Transforming terminator with multiple args?!");
    return visitor(args[0]);
  });
}

void swift::findTransitiveReborrowBaseValuePairs(
    BorrowingOperand initialScopedOperand, SILValue origBaseValue,
    function_ref<void(SILPhiArgument *, SILValue)> visitReborrowBaseValuePair) {
  // We need a SetVector to make sure we don't revisit the same reborrow operand
  // again.
  SmallSetVector<std::tuple<Operand *, SILValue>, 4> worklist;

  // Populate the worklist with reborrow and the base value
  initialScopedOperand.visitScopeEndingUses([&](Operand *op) {
    if (op->getOperandOwnership() == OperandOwnership::Reborrow) {
      worklist.insert(std::make_tuple(op, origBaseValue));
    }
    return true;
  });

  // Size of worklist changes in this loop
  for (unsigned idx = 0; idx < worklist.size(); idx++) {
    Operand *reborrowOp;
    SILValue baseValue;
    std::tie(reborrowOp, baseValue) = worklist[idx];

    BorrowingOperand borrowingOperand(reborrowOp);
    assert(borrowingOperand.isReborrow());

    auto *branchInst = cast<BranchInst>(reborrowOp->getUser());
    auto *succBlock = branchInst->getDestBB();
    auto *phiArg = cast<SILPhiArgument>(
        succBlock->getArgument(reborrowOp->getOperandNumber()));

    SILValue newBaseVal = baseValue;
    // If the previous base value was also passed as a phi arg, that will be
    // the new base value.
    for (auto *arg : succBlock->getArguments()) {
      if (arg->getIncomingPhiValue(branchInst->getParent()) == baseValue) {
        newBaseVal = arg;
        break;
      }
    }

    // Call the visitor function
    visitReborrowBaseValuePair(phiArg, newBaseVal);

    BorrowedValue scopedValue(phiArg);
    scopedValue.visitLocalScopeEndingUses([&](Operand *op) {
      if (op->getOperandOwnership() == OperandOwnership::Reborrow) {
        worklist.insert(std::make_tuple(op, newBaseVal));
      }
      return true;
    });
  }
}

void swift::visitTransitiveEndBorrows(
    SILValue value,
    function_ref<void(EndBorrowInst *)> visitEndBorrow) {
  GraphNodeWorklist<SILValue, 4> worklist;
  worklist.insert(value);

  while (!worklist.empty()) {
    auto val = worklist.pop();
    for (auto *consumingUse : val->getConsumingUses()) {
      auto *consumingUser = consumingUse->getUser();
      if (auto *branch = dyn_cast<BranchInst>(consumingUser)) {
        auto *succBlock = branch->getSingleSuccessorBlock();
        auto *phiArg = cast<SILPhiArgument>(
            succBlock->getArgument(consumingUse->getOperandNumber()));
        worklist.insert(phiArg);
      } else {
        visitEndBorrow(cast<EndBorrowInst>(consumingUser));
      }
    }
  }
}

/// Whether the specified lexical begin_borrow instruction is nested.
///
/// A begin_borrow [lexical] is nested if the borrowed value's lifetime is
/// guaranteed by another lexical scope.  That happens if:
/// - the value is a guaranteed argument to the function
/// - the value is itself a begin_borrow [lexical]
bool swift::isNestedLexicalBeginBorrow(BeginBorrowInst *bbi) {
  assert(bbi->isLexical());
  auto value = bbi->getOperand();
  if (auto *outerBBI = dyn_cast<BeginBorrowInst>(value)) {
    return outerBBI->isLexical();
  }
  if (auto *arg = dyn_cast<SILFunctionArgument>(value)) {
    return arg->getOwnershipKind() == OwnershipKind::Guaranteed;
  }
  return false;
}
