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
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

bool swift::isValueAddressOrTrivial(SILValue v) {
  return v->getType().isAddress() ||
         v.getOwnershipKind() == ValueOwnershipKind::None;
}

// These operations forward both owned and guaranteed ownership.
bool swift::isOwnershipForwardingValueKind(SILNodeKind kind) {
  switch (kind) {
  case SILNodeKind::TupleInst:
  case SILNodeKind::StructInst:
  case SILNodeKind::EnumInst:
  case SILNodeKind::DifferentiableFunctionInst:
  case SILNodeKind::LinearFunctionInst:
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
  case SILNodeKind::DestructureStructInst:
  case SILNodeKind::DestructureTupleInst:
  case SILNodeKind::MarkDependenceInst:
  case SILNodeKind::InitExistentialRefInst:
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
  case SILNodeKind::DifferentiableFunctionExtractInst:
  case SILNodeKind::LinearFunctionExtractInst:
  case SILNodeKind::OpenExistentialValueInst:
  case SILNodeKind::OpenExistentialBoxValueInst:
    return true;
  default:
    return isOwnershipForwardingValueKind(kind);
  }
}

bool swift::isOwnedForwardingValueKind(SILNodeKind kind) {
  switch (kind) {
  case SILNodeKind::BranchInst:
    return true;
  default:
    return isOwnershipForwardingValueKind(kind);
  }
}

bool swift::isOwnedForwardingInstruction(SILInstruction *inst) {
  auto kind = inst->getKind();
  switch (kind) {
  case SILInstructionKind::BranchInst:
    return true;
  default:
    return isOwnershipForwardingValueKind(SILNodeKind(kind));
  }
}

bool swift::isOwnedForwardingValue(SILValue value) {
  switch (value->getKind()) {
  // Phi arguments always forward ownership.
  case ValueKind::SILPhiArgument:
    return true;
  default:
    return isOwnedForwardingValueKind(
        value->getKindOfRepresentativeSILNodeInObject());
  }
}

bool swift::isGuaranteedForwardingValue(SILValue value) {
  // If we have an argument from a transforming terminator, we can forward
  // guaranteed.
  if (auto *arg = dyn_cast<SILArgument>(value)) {
    if (auto *ti = arg->getSingleTerminator()) {
      if (ti->isTransformationTerminator()) {
        return true;
      }
    }
  }

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
//                           Borrowing Operand
//===----------------------------------------------------------------------===//

void BorrowingOperandKind::print(llvm::raw_ostream &os) const {
  switch (value) {
  case Kind::BeginBorrow:
    os << "BeginBorrow";
    return;
  case Kind::BeginApply:
    os << "BeginApply";
    return;
  case Kind::Branch:
    os << "Branch";
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

void BorrowingOperand::visitEndScopeInstructions(
    function_ref<void(Operand *)> func) const {
  switch (kind) {
  case BorrowingOperandKind::BeginBorrow:
    for (auto *use : cast<BeginBorrowInst>(op->getUser())->getUses()) {
      if (use->isConsumingUse()) {
        func(use);
      }
    }
    return;
  case BorrowingOperandKind::BeginApply: {
    auto *user = cast<BeginApplyInst>(op->getUser());
    for (auto *use : user->getTokenResult()->getUses()) {
      func(use);
    }
    return;
  }
  case BorrowingOperandKind::Branch:
    for (auto *succBlock :
         cast<BranchInst>(op->getUser())->getSuccessorBlocks()) {
      auto *arg = succBlock->getArgument(op->getOperandNumber());
      for (auto *use : arg->getUses()) {
        if (use->isConsumingUse()) {
          func(use);
        }
      }
    }
    return;
  }
  llvm_unreachable("Covered switch isn't covered");
}

void BorrowingOperand::visitBorrowIntroducingUserResults(
    function_ref<void(BorrowedValue)> visitor) const {
  switch (kind) {
  case BorrowingOperandKind::BeginApply:
    llvm_unreachable("Never has borrow introducer results!");
  case BorrowingOperandKind::BeginBorrow: {
    auto value = *BorrowedValue::get(cast<BeginBorrowInst>(op->getUser()));
    return visitor(value);
  }
  case BorrowingOperandKind::Branch: {
    auto *bi = cast<BranchInst>(op->getUser());
    for (auto *succBlock : bi->getSuccessorBlocks()) {
      auto value =
          *BorrowedValue::get(succBlock->getArgument(op->getOperandNumber()));
      visitor(value);
    }
    return;
  }
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

void BorrowingOperand::visitConsumingUsesOfBorrowIntroducingUserResults(
    function_ref<void(Operand *)> func) const {
  // First visit all of the results of our user that are borrow introducing
  // values.
  visitBorrowIntroducingUserResults([&](BorrowedValue value) {
    // Visit the scope ending instructions of this value. If any of them are
    // consuming borrow scope operands, visit the consuming uses of the
    // results or successor arguments.
    //
    // This enables one to walk the def-use chain of guaranteed phis for a
    // single guaranteed scope.
    value.visitLocalScopeEndingUses([&](Operand *valueUser) {
      if (auto subBorrowScopeOp = BorrowingOperand::get(valueUser)) {
        if (subBorrowScopeOp->consumesGuaranteedValues()) {
          subBorrowScopeOp->visitUserResultConsumingUses(func);
          return;
        }
      }

      // Otherwise, if we don't have a borrow scope operand that consumes
      // guaranteed values, just visit value user.
      func(valueUser);
    });
  });
}

void BorrowingOperand::visitUserResultConsumingUses(
    function_ref<void(Operand *)> visitor) const {
  auto *ti = dyn_cast<TermInst>(op->getUser());
  if (!ti) {
    for (SILValue result : op->getUser()->getResults()) {
      for (auto *use : result->getUses()) {
        if (use->isConsumingUse()) {
          visitor(use);
        }
      }
    }
    return;
  }

  for (auto *succBlock : ti->getSuccessorBlocks()) {
    auto *arg = succBlock->getArgument(op->getOperandNumber());
    for (auto *use : arg->getUses()) {
      if (use->isConsumingUse()) {
        visitor(use);
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//                             Borrow Introducers
//===----------------------------------------------------------------------===//

void BorrowedValueKind::print(llvm::raw_ostream &os) const {
  switch (value) {
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
  case BorrowedValueKind::SILFunctionArgument:
    llvm_unreachable("Should only call this with a local scope");
  case BorrowedValueKind::BeginBorrow:
  case BorrowedValueKind::LoadBorrow:
  case BorrowedValueKind::Phi:
    for (auto *use : value->getUses()) {
      if (use->isConsumingUse()) {
	scopeEndingInsts.push_back(use->getUser());
      }
    }
    return;
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

void BorrowedValue::visitLocalScopeEndingUses(
    function_ref<void(Operand *)> visitor) const {
  assert(isLocalScope() && "Should only call this given a local scope");
  switch (kind) {
  case BorrowedValueKind::SILFunctionArgument:
    llvm_unreachable("Should only call this with a local scope");
  case BorrowedValueKind::LoadBorrow:
  case BorrowedValueKind::BeginBorrow:
  case BorrowedValueKind::Phi:
    for (auto *use : value->getUses()) {
      if (use->isConsumingUse()) {
        visitor(use);
      }
    }
    return;
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

bool BorrowedValue::areUsesWithinScope(
    ArrayRef<Operand *> uses, SmallVectorImpl<Operand *> &scratchSpace,
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

  // Otherwise, gather up our local scope ending instructions, looking through
  // guaranteed phi nodes.
  visitLocalScopeTransitiveEndingUses(
      [&scratchSpace](Operand *op) { scratchSpace.emplace_back(op); });

  LinearLifetimeChecker checker(visitedBlocks, deadEndBlocks);
  return checker.validateLifetime(value, scratchSpace, uses);
}

bool BorrowedValue::visitLocalScopeTransitiveEndingUses(
    function_ref<void(Operand *)> visitor) const {
  assert(isLocalScope());

  SmallVector<Operand *, 32> worklist;
  SmallPtrSet<Operand *, 16> beenInWorklist;
  for (auto *use : value->getUses()) {
    if (!use->isConsumingUse())
      continue;
    worklist.push_back(use);
    beenInWorklist.insert(use);
  }

  bool foundError = false;
  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();
    assert(op->isConsumingUse() && "Expected only consuming uses");

    // See if we have a borrow scope operand. If we do not, then we know we are
    // a final consumer of our borrow scope introducer. Visit it and continue.
    auto scopeOperand = BorrowingOperand::get(op);
    if (!scopeOperand) {
      visitor(op);
      continue;
    }

    scopeOperand->visitConsumingUsesOfBorrowIntroducingUserResults(
        [&](Operand *op) {
          assert(op->isConsumingUse() && "Expected only consuming uses");
          // Make sure we haven't visited this consuming operand yet. If we
          // have, signal an error and bail without re-visiting the operand.
          if (!beenInWorklist.insert(op).second) {
            foundError = true;
            return;
          }
          worklist.push_back(op);
        });
  }

  return foundError;
}

bool BorrowedValue::visitInteriorPointerOperands(
    function_ref<void(const InteriorPointerOperand &)> func) const {
  SmallVector<Operand *, 32> worklist(value->getUses());
  while (!worklist.empty()) {
    auto *op = worklist.pop_back_val();

    if (auto interiorPointer = InteriorPointerOperand::get(op)) {
      func(*interiorPointer);
      continue;
    }

    auto *user = op->getUser();
    if (isa<BeginBorrowInst>(user) || isa<DebugValueInst>(user) ||
        isa<SuperMethodInst>(user) || isa<ClassMethodInst>(user) ||
        isa<CopyValueInst>(user) || isa<EndBorrowInst>(user) ||
        isa<ApplyInst>(user) || isa<StoreBorrowInst>(user) ||
        isa<StoreInst>(user) || isa<PartialApplyInst>(user) ||
        isa<UnmanagedRetainValueInst>(user) ||
        isa<UnmanagedReleaseValueInst>(user) ||
        isa<UnmanagedAutoreleaseValueInst>(user)) {
      continue;
    }

    // These are interior pointers that have not had support yet added for them.
    if (isa<OpenExistentialBoxInst>(user) ||
        isa<ProjectExistentialBoxInst>(user)) {
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

//===----------------------------------------------------------------------===//
//                          Owned Value Introducers
//===----------------------------------------------------------------------===//

void OwnedValueIntroducerKind::print(llvm::raw_ostream &os) const {
  switch (value) {
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
  if (inputValue.getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return false;

  SmallVector<SILValue, 32> worklist;
  worklist.emplace_back(inputValue);

  while (!worklist.empty()) {
    SILValue value = worklist.pop_back_val();

    // First check if v is an introducer. If so, stash it and continue.
    if (auto scopeIntroducer = BorrowedValue::get(value)) {
      out.push_back(*scopeIntroducer);
      continue;
    }

    // If v produces .none ownership, then we can ignore it. It is important
    // that we put this before checking for guaranteed forwarding instructions,
    // since we want to ignore guaranteed forwarding instructions that in this
    // specific case produce a .none value.
    if (value.getOwnershipKind() == ValueOwnershipKind::None)
      continue;

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isGuaranteedForwardingValue(value)) {
      if (auto *i = value->getDefiningInstruction()) {
        llvm::copy(i->getOperandValues(true /*skip type dependent ops*/),
                   std::back_inserter(worklist));
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator.
      auto *arg = cast<SILPhiArgument>(value);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator());
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

Optional<BorrowedValue>
swift::getSingleBorrowIntroducingValue(SILValue inputValue) {
  if (inputValue.getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return None;

  SILValue currentValue = inputValue;
  while (true) {
    // First check if our initial value is an introducer. If we have one, just
    // return it.
    if (auto scopeIntroducer = BorrowedValue::get(currentValue)) {
      return scopeIntroducer;
    }

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isGuaranteedForwardingValue(currentValue)) {
      if (auto *i = currentValue->getDefiningInstruction()) {
        auto instOps = i->getOperandValues(true /*ignore type dependent ops*/);
        // If we have multiple incoming values, return .None. We can't handle
        // this.
        auto begin = instOps.begin();
        if (std::next(begin) != instOps.end()) {
          return None;
        }
        // Otherwise, set currentOp to the single operand and continue.
        currentValue = *begin;
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator.
      auto *arg = cast<SILPhiArgument>(currentValue);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator());
      assert(termInst->getNumOperands() == 1 &&
             "Transformation terminators should only have single operands");
      currentValue = termInst->getAllOperands()[0].get();
      continue;
    }

    // Otherwise, this is an introducer we do not understand. Bail and return
    // None.
    return None;
  }

  llvm_unreachable("Should never hit this");
}

bool swift::getAllOwnedValueIntroducers(
    SILValue inputValue, SmallVectorImpl<OwnedValueIntroducer> &out) {
  if (inputValue.getOwnershipKind() != ValueOwnershipKind::Owned)
    return false;

  SmallVector<SILValue, 32> worklist;
  worklist.emplace_back(inputValue);

  while (!worklist.empty()) {
    SILValue value = worklist.pop_back_val();

    // First check if v is an introducer. If so, stash it and continue.
    if (auto introducer = OwnedValueIntroducer::get(value)) {
      out.push_back(*introducer);
      continue;
    }

    // If v produces .none ownership, then we can ignore it. It is important
    // that we put this before checking for guaranteed forwarding instructions,
    // since we want to ignore guaranteed forwarding instructions that in this
    // specific case produce a .none value.
    if (value.getOwnershipKind() == ValueOwnershipKind::None)
      continue;

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isOwnedForwardingValue(value)) {
      if (auto *i = value->getDefiningInstruction()) {
        llvm::copy(i->getOperandValues(true /*skip type dependent ops*/),
                   std::back_inserter(worklist));
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator.
      auto *arg = cast<SILPhiArgument>(value);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator());
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

Optional<OwnedValueIntroducer>
swift::getSingleOwnedValueIntroducer(SILValue inputValue) {
  if (inputValue.getOwnershipKind() != ValueOwnershipKind::Owned)
    return None;

  SILValue currentValue = inputValue;
  while (true) {
    // First check if our initial value is an introducer. If we have one, just
    // return it.
    if (auto introducer = OwnedValueIntroducer::get(currentValue)) {
      return introducer;
    }

    // Otherwise if v is an ownership forwarding value, add its defining
    // instruction
    if (isOwnedForwardingValue(currentValue)) {
      if (auto *i = currentValue->getDefiningInstruction()) {
        auto instOps = i->getOperandValues(true /*ignore type dependent ops*/);
        // If we have multiple incoming values, return .None. We can't handle
        // this.
        auto begin = instOps.begin();
        if (std::next(begin) != instOps.end()) {
          return None;
        }
        // Otherwise, set currentOp to the single operand and continue.
        currentValue = *begin;
        continue;
      }

      // Otherwise, we should have a block argument that is defined by a single
      // predecessor terminator.
      auto *arg = cast<SILPhiArgument>(currentValue);
      auto *termInst = arg->getSingleTerminator();
      assert(termInst && termInst->isTransformationTerminator());
      assert(termInst->getNumOperands() == 1 &&
             "Transformation terminators should only have single operands");
      currentValue = termInst->getAllOperands()[0].get();
      continue;
    }

    // Otherwise, this is an introducer we do not understand. Bail and return
    // None.
    return None;
  }

  llvm_unreachable("Should never hit this");
}
