//===--- SILArgument.cpp - Arguments for high-level SIL code --------------===//
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

#include "llvm/ADT/STLExtras.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// SILArgument Implementation
//===----------------------------------------------------------------------===//

SILArgument::SILArgument(ValueKind ChildKind, SILBasicBlock *ParentBB,
                         SILType Ty, ValueOwnershipKind OwnershipKind,
                         const ValueDecl *D)
    : ValueBase(ChildKind, Ty, IsRepresentative::Yes), ParentBB(ParentBB),
      Decl(D) {
  Bits.SILArgument.VOKind = static_cast<unsigned>(OwnershipKind);
  ParentBB->insertArgument(ParentBB->args_end(), this);
}

SILArgument::SILArgument(ValueKind ChildKind, SILBasicBlock *ParentBB,
                         SILBasicBlock::arg_iterator Pos, SILType Ty,
                         ValueOwnershipKind OwnershipKind, const ValueDecl *D)
    : ValueBase(ChildKind, Ty, IsRepresentative::Yes), ParentBB(ParentBB),
      Decl(D) {
  Bits.SILArgument.VOKind = static_cast<unsigned>(OwnershipKind);
  // Function arguments need to have a decl.
  assert(
    !ParentBB->getParent()->isBare() &&
    ParentBB->getParent()->size() == 1
          ? D != nullptr
          : true);
  ParentBB->insertArgument(Pos, this);
}


SILFunction *SILArgument::getFunction() {
  return getParent()->getParent();
}

const SILFunction *SILArgument::getFunction() const {
  return getParent()->getParent();
}

SILModule &SILArgument::getModule() const {
  return getFunction()->getModule();
}

//===----------------------------------------------------------------------===//
//                              SILBlockArgument
//===----------------------------------------------------------------------===//

static SILValue getIncomingValueForPred(const SILBasicBlock *BB,
                                        const SILBasicBlock *Pred,
                                        unsigned Index) {
  const TermInst *TI = Pred->getTerminator();

  switch (TI->getTermKind()) {
  // TODO: This list is conservative. I think we can probably handle more of
  // these.
  case TermKind::UnreachableInst:
  case TermKind::ReturnInst:
  case TermKind::ThrowInst:
  case TermKind::UnwindInst:
    llvm_unreachable("Have terminator that implies no successors?!");
  case TermKind::TryApplyInst:
  case TermKind::SwitchValueInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::CheckedCastAddrBranchInst:
  case TermKind::DynamicMethodBranchInst:
  case TermKind::YieldInst:
    return SILValue();
  case TermKind::BranchInst:
    return cast<const BranchInst>(TI)->getArg(Index);
  case TermKind::CondBranchInst:
    return cast<const CondBranchInst>(TI)->getArgForDestBB(BB, Index);
  case TermKind::CheckedCastBranchInst:
    return cast<const CheckedCastBranchInst>(TI)->getOperand();
  case TermKind::CheckedCastValueBranchInst:
    return cast<const CheckedCastValueBranchInst>(TI)->getOperand();
  case TermKind::SwitchEnumInst:
    return cast<const SwitchEnumInst>(TI)->getOperand();
  }
  llvm_unreachable("Unhandled TermKind?!");
}

SILValue SILPHIArgument::getSingleIncomingValue() const {
  const SILBasicBlock *Parent = getParent();
  const SILBasicBlock *PredBB = Parent->getSinglePredecessorBlock();
  if (!PredBB)
    return SILValue();
  return getIncomingValueForPred(Parent, PredBB, getIndex());
}

bool SILPHIArgument::getIncomingValues(
    llvm::SmallVectorImpl<SILValue> &OutArray) {
  SILBasicBlock *Parent = getParent();

  if (Parent->pred_empty())
    return false;

  unsigned Index = getIndex();
  for (SILBasicBlock *Pred : getParent()->getPredecessorBlocks()) {
    SILValue Value = getIncomingValueForPred(Parent, Pred, Index);
    if (!Value)
      return false;
    OutArray.push_back(Value);
  }

  return true;
}

bool SILPHIArgument::getIncomingValues(
    llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray) {
  SILBasicBlock *Parent = getParent();

  if (Parent->pred_empty())
    return false;

  unsigned Index = getIndex();
  for (SILBasicBlock *Pred : getParent()->getPredecessorBlocks()) {
    SILValue Value = getIncomingValueForPred(Parent, Pred, Index);
    if (!Value)
      return false;
    OutArray.push_back({Pred, Value});
  }

  return true;
}

SILValue SILPHIArgument::getIncomingValue(unsigned BBIndex) {
  SILBasicBlock *Parent = getParent();

  if (Parent->pred_empty())
    return SILValue();

  unsigned Index = getIndex();

  // We could do an early check if the size of the pred list is <= BBIndex, but
  // that would involve walking the linked list anyways, so we just iterate once
  // over the loop.

  // We use this funky loop since predecessors are stored in a linked list but
  // we want array like semantics.
  unsigned BBCount = 0;
  for (SILBasicBlock *Pred : Parent->getPredecessorBlocks()) {
    // If BBCount is not BBIndex, continue.
    if (BBCount < BBIndex) {
      BBCount++;
      continue;
    }

    // This will return an empty SILValue if we found something we do not
    // understand.
    return getIncomingValueForPred(Parent, Pred, Index);
  }

  return SILValue();
}

SILValue SILPHIArgument::getIncomingValue(SILBasicBlock *BB) {
  SILBasicBlock *Parent = getParent();

  assert(!Parent->pred_empty() && "Passed in non-predecessor BB!");
  unsigned Index = getIndex();

  // We could do an early check if the size of the pred list is <= BBIndex, but
  // that would involve walking the linked list anyways, so we just iterate once
  // over the loop.

  auto Target = std::find(Parent->pred_begin(), Parent->pred_end(), BB);
  if (Target == Parent->pred_end())
    return SILValue();
  return getIncomingValueForPred(Parent, BB, Index);
}

const SILPHIArgument *BranchInst::getArgForOperand(const Operand *oper) const {
  assert(oper->getUser() == this);
  return cast<SILPHIArgument>(
      getDestBB()->getArgument(oper->getOperandNumber()));
}

const SILPHIArgument *
CondBranchInst::getArgForOperand(const Operand *oper) const {
  assert(oper->getUser() == this);

  unsigned operIdx = oper->getOperandNumber();
  if (isTrueOperandIndex(operIdx)) {
    return cast<SILPHIArgument>(getTrueBB()->getArgument(
        operIdx - getTrueOperands().front().getOperandNumber()));
  }
  if (isFalseOperandIndex(operIdx)) {
    return cast<SILPHIArgument>(getFalseBB()->getArgument(
        operIdx - getFalseOperands().front().getOperandNumber()));
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                            SILFunctionArgument
//===----------------------------------------------------------------------===//

bool SILFunctionArgument::isSelf() const {
  // Return true if we are the last argument of our BB and that our parent
  // function has a call signature with self.
  return getFunction()->hasSelfParam() &&
         getParent()->getArguments().back() == this;
}
