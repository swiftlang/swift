//===--- SimplifyCFG.cpp - Clean up the SIL CFG ---------------------------===//
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

#define DEBUG_TYPE "sil-simplify-cfg"
#include "swift/Subsystems.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumBlocksDeleted, "Number of unreachable blocks removed");

//===----------------------------------------------------------------------===//
//                           alloc_box Promotion
//===----------------------------------------------------------------------===//

static void removeDeadBlock(SILBasicBlock *BB,
                            llvm::SmallSetVector<SILBasicBlock*, 32> &Worklist){
  // Add successor blocks to the worklist since their predecessor list is about
  // to change.
  for (auto &S : BB->getSuccs())
    Worklist.insert(S);

  // Instructions in the dead block may be used by other dead blocks.  Replace
  // any uses of them with undef values.
  while (!BB->empty()) {
    auto *Inst = &BB->getInstList().back();

    // Replace any non-dead results with SILUndef values.
    for (unsigned i = 0, e = Inst->getNumTypes(); i != e; ++i)
      if (!SILValue(Inst, i).use_empty())
        SILValue(Inst, i).replaceAllUsesWith(SILUndef::get(Inst->getType(i),
                                                           BB->getModule()));
    BB->getInstList().pop_back();
  }

  BB->eraseFromParent();
  ++NumBlocksDeleted;
}


static void simplifyFunction(SILFunction &Fn) {
  llvm::SmallSetVector<SILBasicBlock*, 32> Worklist;
  
  // Add all of the blocks to the function.
  for (auto &BB : Fn)
    Worklist.insert(&BB);
  
  // Iteratively simplify while there is still work to do.
  while (!Worklist.empty()) {
    SILBasicBlock *BB = Worklist.pop_back_val();
  
    // If the block is dead, remove it.
    if (BB->pred_empty() && BB != &*Fn.begin()) {
      removeDeadBlock(BB, Worklist);
      continue;
    }
    
    // Otherwise, try to simplify the terminator.
    TermInst *TI = BB->getTerminator();
    if (auto *BI = dyn_cast<BranchInst>(TI))
      (void)BI;
    else if (auto *CBI = dyn_cast<CondBranchInst>(TI))
      (void)CBI;
    else if (auto *SII = dyn_cast<SwitchIntInst>(TI))
      (void)SII;
  }
}

/// \brief Simplify the CFG of SIL functions.
void swift::performSimplifyCFG(SILModule *M) {
  for (auto &Fn : *M)
    simplifyFunction(Fn);
}
