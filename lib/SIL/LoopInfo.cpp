//===--- LoopInfo.cpp - SIL Loop Analysis ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
  // The dealloc_stack of an alloc_stack must be in the loop, otherwise the
  // dealloc_stack will be fed by a phi node of two alloc_stacks.
  if (auto *Alloc = dyn_cast<AllocStackInst>(I)) {
    for (auto *UI : Alloc->getUses()) {
      if (auto *Dealloc = dyn_cast<DeallocStackInst>(UI->getUser())) {
        if (!contains(Dealloc->getParent()))
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

  assert(I->isTriviallyDuplicatable() &&
         "Code here must match isTriviallyDuplicatable in SILInstruction");
  return true;
}

void SILLoopInfo::verify() const {
  LI.verify();
}
