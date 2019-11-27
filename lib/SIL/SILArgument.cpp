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

SILArgument::SILArgument(ValueKind subClassKind,
                         SILBasicBlock *inputParentBlock, SILType type,
                         ValueOwnershipKind ownershipKind,
                         const ValueDecl *inputDecl)
    : ValueBase(subClassKind, type, IsRepresentative::Yes),
      parentBlock(inputParentBlock), decl(inputDecl) {
  Bits.SILArgument.VOKind = static_cast<unsigned>(ownershipKind);
  inputParentBlock->insertArgument(inputParentBlock->args_end(), this);
}

SILArgument::SILArgument(ValueKind subClassKind,
                         SILBasicBlock *inputParentBlock,
                         SILBasicBlock::arg_iterator argArrayInsertPt,
                         SILType type, ValueOwnershipKind ownershipKind,
                         const ValueDecl *inputDecl)
    : ValueBase(subClassKind, type, IsRepresentative::Yes),
      parentBlock(inputParentBlock), decl(inputDecl) {
  Bits.SILArgument.VOKind = static_cast<unsigned>(ownershipKind);
  // Function arguments need to have a decl.
  assert(!inputParentBlock->getParent()->isBare() &&
                 inputParentBlock->getParent()->size() == 1
             ? decl != nullptr
             : true);
  inputParentBlock->insertArgument(argArrayInsertPt, this);
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

// FIXME: SILPhiArgument should only refer to branch arguments. They usually
// need to be distinguished from projections and casts. Actual phi block
// arguments are substitutable with their incoming values. It is also needlessly
// expensive to call this helper instead of simply specifying phis with an
// opcode. It results in repeated CFG traversals and repeated, unnecessary
// switching over terminator opcodes.
bool SILPhiArgument::isPhiArgument() const {
  // No predecessors indicates an unreachable block.
  if (getParent()->pred_empty())
    return false;

  // Multiple predecessors require phis.
  auto *predBlock = getParent()->getSinglePredecessorBlock();
  if (!predBlock)
    return true;

  auto *termInst = predBlock->getTerminator();
  return isa<BranchInst>(termInst) || isa<CondBranchInst>(termInst);
}

static SILValue getIncomingPhiValueForPred(const SILBasicBlock *parentBlock,
                                           const SILBasicBlock *predBlock,
                                           unsigned argIndex) {
  const auto *predBlockTermInst = predBlock->getTerminator();
  if (auto *bi = dyn_cast<BranchInst>(predBlockTermInst))
    return bi->getArg(argIndex);

  // FIXME: Disallowing critical edges in SIL would enormously simplify phi and
  // branch handling and reduce expensive analysis invalidation. If that is
  // done, then only BranchInst will participate in phi operands, eliminating
  // the need to search for the appropriate CondBranchInst operand.
  return cast<CondBranchInst>(predBlockTermInst)
      ->getArgForDestBB(parentBlock, argIndex);
}

SILValue SILPhiArgument::getIncomingPhiValue(SILBasicBlock *predBlock) const {
  if (!isPhiArgument())
    return SILValue();

  const auto *parentBlock = getParent();
  assert(!parentBlock->pred_empty());

  unsigned argIndex = getIndex();

  assert(parentBlock->pred_end() != std::find(parentBlock->pred_begin(),
                                              parentBlock->pred_end(),
                                              predBlock));

  return getIncomingPhiValueForPred(parentBlock, predBlock, argIndex);
}

bool SILPhiArgument::getIncomingPhiValues(
    SmallVectorImpl<SILValue> &returnedPhiValues) const {
  if (!isPhiArgument())
    return false;

  const auto *parentBlock = getParent();
  assert(!parentBlock->pred_empty());

  unsigned argIndex = getIndex();
  for (auto *predBlock : getParent()->getPredecessorBlocks()) {
    SILValue incomingValue =
        getIncomingPhiValueForPred(parentBlock, predBlock, argIndex);
    assert(incomingValue);
    returnedPhiValues.push_back(incomingValue);
  }
  return true;
}

bool SILPhiArgument::getIncomingPhiValues(
    SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>>
        &returnedPredBBAndPhiValuePairs) const {
  if (!isPhiArgument())
    return false;

  const auto *parentBlock = getParent();
  assert(!parentBlock->pred_empty());

  unsigned argIndex = getIndex();
  for (auto *predBlock : getParent()->getPredecessorBlocks()) {
    SILValue incomingValue =
        getIncomingPhiValueForPred(parentBlock, predBlock, argIndex);
    assert(incomingValue);
    returnedPredBBAndPhiValuePairs.push_back({predBlock, incomingValue});
  }
  return true;
}

static SILValue
getSingleTerminatorOperandForPred(const SILBasicBlock *parentBlock,
                                  const SILBasicBlock *predBlock,
                                  unsigned argIndex) {
  const auto *predTermInst = predBlock->getTerminator();

  switch (predTermInst->getTermKind()) {
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
    return cast<const BranchInst>(predTermInst)->getArg(argIndex);
  case TermKind::CondBranchInst:
    return cast<const CondBranchInst>(predTermInst)
        ->getArgForDestBB(parentBlock, argIndex);
  case TermKind::CheckedCastBranchInst:
    return cast<const CheckedCastBranchInst>(predTermInst)->getOperand();
  case TermKind::CheckedCastValueBranchInst:
    return cast<const CheckedCastValueBranchInst>(predTermInst)->getOperand();
  case TermKind::SwitchEnumInst:
    return cast<const SwitchEnumInst>(predTermInst)->getOperand();
  }
  llvm_unreachable("Unhandled TermKind?!");
}

bool SILPhiArgument::getSingleTerminatorOperands(
    SmallVectorImpl<SILValue> &returnedSingleTermOperands) const {
  const auto *parentBlock = getParent();

  if (parentBlock->pred_empty())
    return false;

  unsigned argIndex = getIndex();
  for (auto *predBlock : getParent()->getPredecessorBlocks()) {
    SILValue incomingValue =
        getSingleTerminatorOperandForPred(parentBlock, predBlock, argIndex);
    if (!incomingValue)
      return false;
    returnedSingleTermOperands.push_back(incomingValue);
  }

  return true;
}

bool SILPhiArgument::getSingleTerminatorOperands(
    SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>>
        &returnedSingleTermOperands) const {
  const auto *parentBlock = getParent();

  if (parentBlock->pred_empty())
    return false;

  unsigned argIndex = getIndex();
  for (auto *predBlock : getParent()->getPredecessorBlocks()) {
    SILValue incomingValue =
        getSingleTerminatorOperandForPred(parentBlock, predBlock, argIndex);
    if (!incomingValue)
      return false;
    returnedSingleTermOperands.push_back({predBlock, incomingValue});
  }

  return true;
}

SILValue SILPhiArgument::getSingleTerminatorOperand() const {
  const auto *parentBlock = getParent();
  const auto *predBlock = parentBlock->getSinglePredecessorBlock();
  if (!predBlock)
    return SILValue();
  return getSingleTerminatorOperandForPred(parentBlock, predBlock, getIndex());
}

const SILPhiArgument *BranchInst::getArgForOperand(const Operand *oper) const {
  assert(oper->getUser() == this);
  return cast<SILPhiArgument>(
      getDestBB()->getArgument(oper->getOperandNumber()));
}

const SILPhiArgument *
CondBranchInst::getArgForOperand(const Operand *oper) const {
  assert(oper->getUser() == this);

  unsigned operIdx = oper->getOperandNumber();
  if (isTrueOperandIndex(operIdx)) {
    return cast<SILPhiArgument>(getTrueBB()->getArgument(
        operIdx - getTrueOperands().front().getOperandNumber()));
  }
  if (isFalseOperandIndex(operIdx)) {
    return cast<SILPhiArgument>(getFalseBB()->getArgument(
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
