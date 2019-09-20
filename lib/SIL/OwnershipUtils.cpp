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
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

bool swift::isValueAddressOrTrivial(SILValue v) {
  return v->getType().isAddress() ||
         v.getOwnershipKind() == ValueOwnershipKind::Any;
}

// These operations forward both owned and guaranteed ownership.
bool swift::isOwnershipForwardingValueKind(SILNodeKind kind) {
  switch (kind) {
  case SILNodeKind::TupleInst:
  case SILNodeKind::StructInst:
  case SILNodeKind::EnumInst:
  case SILNodeKind::OpenExistentialRefInst:
  case SILNodeKind::UpcastInst:
  case SILNodeKind::UncheckedRefCastInst:
  case SILNodeKind::ConvertFunctionInst:
  case SILNodeKind::RefToBridgeObjectInst:
  case SILNodeKind::BridgeObjectToRefInst:
  case SILNodeKind::UnconditionalCheckedCastInst:
  case SILNodeKind::UncheckedEnumDataInst:
  case SILNodeKind::MarkUninitializedInst:
  case SILNodeKind::SelectEnumInst:
  case SILNodeKind::SwitchEnumInst:
  case SILNodeKind::CheckedCastBranchInst:
  case SILNodeKind::BranchInst:
  case SILNodeKind::CondBranchInst:
  case SILNodeKind::DestructureStructInst:
  case SILNodeKind::DestructureTupleInst:
    return true;
  default:
    return false;
  }
}

// These operations forward guaranteed ownership, but don't necessarily forward
// owned values.
bool swift::isGuaranteedForwardingValueKind(SILNodeKind kind) {
  switch (kind) {
  case SILNodeKind::TupleExtractInst:
  case SILNodeKind::StructExtractInst:
  case SILNodeKind::OpenExistentialValueInst:
  case SILNodeKind::OpenExistentialBoxValueInst:
    return true;
  default:
    return isOwnershipForwardingValueKind(kind);
  }
}

bool swift::isGuaranteedForwardingValue(SILValue value) {
  return isGuaranteedForwardingValueKind(
      value->getKindOfRepresentativeSILNodeInObject());
}

bool swift::isGuaranteedForwardingInst(SILInstruction *i) {
  return isGuaranteedForwardingValueKind(SILNodeKind(i->getKind()));
}

bool swift::isOwnershipForwardingInst(SILInstruction *i) {
  return isOwnershipForwardingValueKind(SILNodeKind(i->getKind()));
}

//===----------------------------------------------------------------------===//
//                             Borrow Introducers
//===----------------------------------------------------------------------===//

void BorrowScopeIntroducerKind::print(llvm::raw_ostream &os) const {
  switch (value) {
  case BorrowScopeIntroducerKind::SILFunctionArgument:
    os << "SILFunctionArgument";
    return;
  case BorrowScopeIntroducerKind::BeginBorrow:
    os << "BeginBorrowInst";
    return;
  case BorrowScopeIntroducerKind::LoadBorrow:
    os << "LoadBorrowInst";
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

void BorrowScopeIntroducerKind::dump() const {
#ifndef NDEBUG
  print(llvm::dbgs());
#endif
}

void BorrowScopeIntroducingValue::getLocalScopeEndingInstructions(
    SmallVectorImpl<SILInstruction *> &scopeEndingInsts) const {
  assert(isLocalScope() && "Should only call this given a local scope");

  switch (kind) {
  case BorrowScopeIntroducerKind::SILFunctionArgument:
    llvm_unreachable("Should only call this with a local scope");
  case BorrowScopeIntroducerKind::BeginBorrow:
    llvm::copy(cast<BeginBorrowInst>(value)->getEndBorrows(),
               std::back_inserter(scopeEndingInsts));
    return;
  case BorrowScopeIntroducerKind::LoadBorrow:
    llvm::copy(cast<LoadBorrowInst>(value)->getEndBorrows(),
               std::back_inserter(scopeEndingInsts));
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

void BorrowScopeIntroducingValue::visitLocalScopeEndingInstructions(
    function_ref<void(SILInstruction *)> visitor) const {
  assert(isLocalScope() && "Should only call this given a local scope");
  switch (kind) {
  case BorrowScopeIntroducerKind::SILFunctionArgument:
    llvm_unreachable("Should only call this with a local scope");
  case BorrowScopeIntroducerKind::BeginBorrow:
    for (auto *inst : cast<BeginBorrowInst>(value)->getEndBorrows()) {
      visitor(inst);
    }
    return;
  case BorrowScopeIntroducerKind::LoadBorrow:
    for (auto *inst : cast<LoadBorrowInst>(value)->getEndBorrows()) {
      visitor(inst);
    }
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

bool swift::getUnderlyingBorrowIntroducingValues(
    SILValue inputValue, SmallVectorImpl<BorrowScopeIntroducingValue> &out) {
  if (inputValue.getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return false;

  SmallVector<SILValue, 32> worklist;
  worklist.emplace_back(inputValue);

  while (!worklist.empty()) {
    SILValue v = worklist.pop_back_val();

    // First check if v is an introducer. If so, stash it and continue.
    if (auto scopeIntroducer = BorrowScopeIntroducingValue::get(v)) {
      out.push_back(*scopeIntroducer);
      continue;
    }

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isGuaranteedForwardingValue(v)) {
      auto *i = v->getDefiningInstruction();
      assert(i);
      llvm::transform(i->getAllOperands(), std::back_inserter(worklist),
                      [](const Operand &op) -> SILValue { return op.get(); });
      continue;
    }

    // If v produces any ownership, then we can ignore it. Otherwise, we need to
    // return false since this is an introducer we do not understand.
    if (v.getOwnershipKind() != ValueOwnershipKind::Any)
      return false;
  }

  return true;
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     BorrowScopeIntroducerKind kind) {
  kind.print(os);
  return os;
}

bool BorrowScopeIntroducingValue::areInstructionsWithinScope(
    ArrayRef<BranchPropagatedUser> instructions,
    SmallVectorImpl<BranchPropagatedUser> &scratchSpace,
    SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks,
    DeadEndBlocks &deadEndBlocks) const {
  // Make sure that we clear our scratch space/utilities before we exit.
  SWIFT_DEFER {
    scratchSpace.clear();
    visitedBlocks.clear();
  };

  // First make sure that we actually have a local scope. If we have a non-local
  // scope, then we have something (like a SILFunctionArgument) where a larger
  // semantic construct (in the case of SILFunctionArgument, the function
  // itself) acts as the scope. So we already know that our passed in
  // instructions must be in the same scope.
  if (!isLocalScope())
    return true;

  // Otherwise, gather up our local scope ending instructions.
  visitLocalScopeEndingInstructions(
      [&scratchSpace](SILInstruction *i) { scratchSpace.emplace_back(i); });

  LinearLifetimeChecker checker(visitedBlocks, deadEndBlocks);
  return checker.validateLifetime(value, scratchSpace, instructions);
}
