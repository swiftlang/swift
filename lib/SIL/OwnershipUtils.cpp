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
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

bool swift::isValueAddressOrTrivial(SILValue v, SILModule &m) {
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

bool swift::getUnderlyingBorrowIntroducers(SILValue inputValue,
                                           SmallVectorImpl<SILValue> &out) {
  if (inputValue.getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return false;

  SmallVector<SILValue, 32> worklist;
  worklist.emplace_back(inputValue);

  while (!worklist.empty()) {
    SILValue v = worklist.pop_back_val();

    // First check if v is an introducer. If so, stash it and continue.
    if (isa<LoadBorrowInst>(v) ||
        isa<BeginBorrowInst>(v)) {
      out.push_back(v);
      continue;
    }

    // If we have a function argument with guaranteed convention, it is also an
    // introducer.
    if (auto *arg = dyn_cast<SILFunctionArgument>(v)) {
      if (arg->getOwnershipKind() == ValueOwnershipKind::Guaranteed) {
        out.push_back(v);
        continue;
      }

      // Otherwise, we do not know how to handle this function argument, so
      // bail.
      return false;
    }

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isGuaranteedForwardingValue(v)) {
      auto *i = v->getDefiningInstruction();
      assert(i);
      transform(i->getAllOperands(), std::back_inserter(worklist),
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
