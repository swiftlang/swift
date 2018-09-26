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

// FIXME: SILPhiArgument should only refer to branch arguments. They usually
// need to be distinguished from projections and casts. Actual phi block
// arguments are substitutable with their incoming values. It is also needlessly
// expensive to call this helper instead of simply specifying phis with an
// opcode. It results in repeated CFG traversals and repeated, unnecessary
// switching over terminator opcodes.
bool SILPhiArgument::isPhiArgument() {
  // No predecessors indicates an unreachable block.
  if (getParent()->pred_empty())
    return false;

  // Multiple predecessors require phis.
  auto *predBB = getParent()->getSinglePredecessorBlock();
  if (!predBB)
    return true;

  auto *TI = predBB->getTerminator();
  return isa<BranchInst>(TI) || isa<CondBranchInst>(TI);
}

static SILValue getIncomingPhiValueForPred(const SILBasicBlock *BB,
                                           const SILBasicBlock *Pred,
                                           unsigned Index) {
  const TermInst *TI = Pred->getTerminator();
  if (auto *BI = dyn_cast<BranchInst>(TI))
    return BI->getArg(Index);

  // FIXME: Disallowing critical edges in SIL would enormously simplify phi and
  // branch handling and reduce expensive analysis invalidation. If that is
  // done, then only BranchInst will participate in phi operands, eliminating
  // the need to search for the appropriate CondBranchInst operand.
  return cast<CondBranchInst>(TI)->getArgForDestBB(BB, Index);
}

SILValue SILPhiArgument::getIncomingPhiValue(SILBasicBlock *predBB) {
  if (!isPhiArgument())
    return SILValue();

  SILBasicBlock *Parent = getParent();
  assert(!Parent->pred_empty());

  unsigned Index = getIndex();

  assert(Parent->pred_end()
         != std::find(Parent->pred_begin(), Parent->pred_end(), predBB));

  return getIncomingPhiValueForPred(Parent, predBB, Index);
}

bool SILPhiArgument::getIncomingPhiValues(
    llvm::SmallVectorImpl<SILValue> &ReturnedPhiValues) {
  if (!isPhiArgument())
    return false;

  SILBasicBlock *Parent = getParent();
  assert(!Parent->pred_empty());

  unsigned Index = getIndex();
  for (SILBasicBlock *Pred : getParent()->getPredecessorBlocks()) {
    SILValue Value = getIncomingPhiValueForPred(Parent, Pred, Index);
    assert(Value);
    ReturnedPhiValues.push_back(Value);
  }
  return true;
}

bool SILPhiArgument::getIncomingPhiValues(
    llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>>
        &ReturnedPredBBAndPhiValuePairs) {
  if (!isPhiArgument())
    return false;

  SILBasicBlock *Parent = getParent();
  assert(!Parent->pred_empty());

  unsigned Index = getIndex();
  for (SILBasicBlock *Pred : getParent()->getPredecessorBlocks()) {
    SILValue Value = getIncomingPhiValueForPred(Parent, Pred, Index);
    assert(Value);
    ReturnedPredBBAndPhiValuePairs.push_back({Pred, Value});
  }
  return true;
}

static SILValue getSingleTerminatorOperandForPred(const SILBasicBlock *BB,
                                                  const SILBasicBlock *Pred,
                                                  unsigned Index) {
  const TermInst *TI = Pred->getTerminator();

  switch (TI->getTermKind()) {
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

bool SILPhiArgument::getSingleTerminatorOperands(
    llvm::SmallVectorImpl<SILValue> &OutArray) {
  SILBasicBlock *Parent = getParent();

  if (Parent->pred_empty())
    return false;

  unsigned Index = getIndex();
  for (SILBasicBlock *Pred : getParent()->getPredecessorBlocks()) {
    SILValue Value = getSingleTerminatorOperandForPred(Parent, Pred, Index);
    if (!Value)
      return false;
    OutArray.push_back(Value);
  }

  return true;
}

bool SILPhiArgument::getSingleTerminatorOperands(
    llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray) {
  SILBasicBlock *Parent = getParent();

  if (Parent->pred_empty())
    return false;

  unsigned Index = getIndex();
  for (SILBasicBlock *Pred : getParent()->getPredecessorBlocks()) {
    SILValue Value = getSingleTerminatorOperandForPred(Parent, Pred, Index);
    if (!Value)
      return false;
    OutArray.push_back({Pred, Value});
  }

  return true;
}

SILValue SILPhiArgument::getSingleTerminatorOperand() const {
  const SILBasicBlock *Parent = getParent();
  const SILBasicBlock *PredBB = Parent->getSinglePredecessorBlock();
  if (!PredBB)
    return SILValue();
  return getSingleTerminatorOperandForPred(Parent, PredBB, getIndex());
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
