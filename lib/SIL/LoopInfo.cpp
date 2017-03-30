//===--- LoopInfo.cpp - SIL Loop Analysis ---------------------------------===//
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

#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/CFG.h"
#include "llvm/Analysis/LoopInfoImpl.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// Instantiate template members.
template class llvm::LoopBase<SILBasicBlock, SILLoop>;
template class llvm::LoopInfoBase<SILBasicBlock, SILLoop>;


void SILLoop::dump() const {
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  print(llvm::dbgs());
#endif
}

SILLoopInfo::SILLoopInfo(SILFunction *F, DominanceInfo *DT) {
  LI.analyze(*DT);
}

bool SILLoop::canDuplicate(SILInstruction *I) const {
  // The deallocation of a stack allocation must be in the loop, otherwise the
  // deallocation will be fed by a phi node of two allocations.
  if (I->isAllocatingStack()) {
    for (auto *UI : I->getUses()) {
      if (UI->getUser()->isDeallocatingStack()) {
        if (!contains(UI->getUser()->getParent()))
          return false;
      }
    }
    return true;
  }

  // CodeGen can't build ssa for objc methods.
  if (auto *Method = dyn_cast<MethodInst>(I)) {
    if (Method->getMember().isForeign) {
      for (auto *UI : Method->getUses()) {
        if (!contains(UI->getUser()))
          return false;
      }
    }
    return true;
  }

  // We can't have a phi of two openexistential instructions of different UUID.
  SILInstruction *OEI = dyn_cast<OpenExistentialAddrInst>(I);
  if (OEI || (OEI = dyn_cast<OpenExistentialRefInst>(I)) ||
      (OEI = dyn_cast<OpenExistentialMetatypeInst>(I))) {
    for (auto *UI : OEI->getUses())
      if (!contains(UI->getUser()))
        return false;
    return true;
  }

  if (auto *Dealloc = dyn_cast<DeallocStackInst>(I)) {
    // The matching alloc_stack must be in the loop.
    if (auto *Alloc = dyn_cast<AllocStackInst>(Dealloc->getOperand()))
        return contains(Alloc->getParent());
    return false;
  }

  if (isa<ThrowInst>(I))
    return false;

  assert(I->isTriviallyDuplicatable() &&
    "Code here must match isTriviallyDuplicatable in SILInstruction");
  return true;
}

void SILLoopInfo::verify() const {
  LI.verify();
}
