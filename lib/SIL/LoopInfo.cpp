//===-------------- LoopInfo.cpp - SIL Loop Analysis -----*- C++ -*--------===//
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

#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILFunction.h"
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
  LI.Analyze(*DT);
}


void SILLoopInfo::verify() const {
  llvm::DenseSet<const SILLoop*> Loops;
  for (iterator I = begin(), E = end(); I != E; ++I) {
    assert(!(*I)->getParentLoop() && "Top-level loop has a parent!");
    (*I)->verifyLoopNest(&Loops);
  }

  // We need access to the map for this.
  // Verify that blocks are mapped to valid loops.
  for (llvm::DenseMap<const SILBasicBlock *, SILLoop *>::const_iterator
           I = LI.getBlockMap().begin(),
           E = LI.getBlockMap().end();
       I != E; ++I) {
    assert(Loops.count(I->second) && "orphaned loop");
    assert(I->second->contains(I->first) && "orphaned block");
  }
}
