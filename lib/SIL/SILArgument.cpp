//===--- SILArgument.cpp - Arguments for high-level SIL code ---------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

SILArgument::SILArgument(SILBasicBlock *ParentBB, SILType Ty,
                         const ValueDecl *D)
  : ValueBase(ValueKind::SILArgument, Ty), ParentBB(ParentBB), Decl(D) {
  // Function arguments need to have a decl.
  assert(
    !ParentBB->getParent()->isBare() &&
    ParentBB->getParent()->size() == 1
          ? D != nullptr
          : true );
  ParentBB->insertArgument(ParentBB->bbarg_end(), this);
}

SILArgument::SILArgument(SILBasicBlock *ParentBB,
                         SILBasicBlock::bbarg_iterator Pos,
                         SILType Ty, const ValueDecl *D)
  : ValueBase(ValueKind::SILArgument, Ty), ParentBB(ParentBB), Decl(D) {
  // Function arguments need to have a decl.
  assert(
    !ParentBB->getParent()->isBare() &&
    ParentBB->getParent()->size() == 1
          ? D != nullptr
          : true );
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

bool SILArgument::getIncomingValues(llvm::SmallVectorImpl<SILValue> &OutArray) {
  SILBasicBlock *Parent = getParent();

  if (Parent->pred_empty())
    return false;

  unsigned Index = getIndex();
  for (SILBasicBlock *Pred : getParent()->getPreds()) {
    TermInst *TI = Pred->getTerminator();

    if (auto *BI = dyn_cast<BranchInst>(TI)) {
      OutArray.push_back(BI->getArg(Index));
      continue;
    }

    if (auto *CBI = dyn_cast<CondBranchInst>(TI)) {
      OutArray.push_back(CBI->getArgForDestBB(getParent(), this));
      continue;
    }

    if (auto *CCBI = dyn_cast<CheckedCastBranchInst>(TI)) {
      OutArray.push_back(CCBI->getOperand());
      continue;
    }

    if (auto *SWEI = dyn_cast<SwitchEnumInst>(TI)) {
      OutArray.push_back(SWEI->getOperand());
      continue;
    }
    
    return false;
  }

  return true;
}

SILValue SILArgument::getIncomingValue(unsigned BBIndex) {
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
  for (SILBasicBlock *Pred : Parent->getPreds()) {
    // If BBCount is not BBIndex, continue.
    if (BBCount < BBIndex) {
      BBCount++;
      continue;
    }

    TermInst *TI = Pred->getTerminator();

    if (auto *BI = dyn_cast<BranchInst>(TI))
      return BI->getArg(Index);

    if (auto *CBI = dyn_cast<CondBranchInst>(TI))
      return CBI->getArgForDestBB(Parent, this);

    if (auto *CCBI = dyn_cast<CheckedCastBranchInst>(TI))
      return CCBI->getOperand();

    if (auto *SWEI = dyn_cast<SwitchEnumInst>(TI))
      return SWEI->getOperand();

    // Return an empty SILValue since we ran into something we were unable to
    // understand.
    return SILValue();
  }

  return SILValue();
}

SILValue SILArgument::getIncomingValue(SILBasicBlock *BB) {
  SILBasicBlock *Parent = getParent();

  assert(!Parent->pred_empty() && "Passed in non-predecessor BB!");
  unsigned Index = getIndex();

  // We could do an early check if the size of the pred list is <= BBIndex, but
  // that would involve walking the linked list anyways, so we just iterate once
  // over the loop.

  // We use this funky loop since predecessors are stored in a linked list but
  // we want array like semantics.
  for (SILBasicBlock *Pred : Parent->getPreds()) {
    // If BBCount is not BBIndex, continue.
    if (Pred != BB)
      continue;

    TermInst *TI = Pred->getTerminator();

    if (auto *BI = dyn_cast<BranchInst>(TI))
      return BI->getArg(Index);

    if (auto *CBI = dyn_cast<CondBranchInst>(TI))
      return CBI->getArgForDestBB(Parent, this);

    if (auto *CCBI = dyn_cast<CheckedCastBranchInst>(TI))
      return CCBI->getOperand();

    if (auto *SWEI = dyn_cast<SwitchEnumInst>(TI))
      return SWEI->getOperand();

    // Return an empty SILValue since we ran into something we were unable to
    // understand.
    return SILValue();
  }

  return SILValue();
}

bool SILArgument::isSelf() const {
  // First make sure that we are actually a function argument. We use an assert
  // boolean return here since in release builds we want to conservatively
  // return false and in debug builds assert since this is a logic error.
  bool isArg = isFunctionArg();
  assert(isArg && "Only function arguments can be self");
  if (!isArg)
    return false;

  // Return true if we are the last argument of our BB and that our parent
  // function has a call signature with self.
  return getFunction()->hasSelfArgument() &&
         getParent()->getBBArgs().back() == this;
}
