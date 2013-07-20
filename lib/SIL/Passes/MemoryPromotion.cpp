//===--- MemoryPromotion.cpp - Promote heap memory to registers and stack -===//
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

#define DEBUG_TYPE "memory-promotion"
#include "swift/Subsystems.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"
//#include "swift/SIL/Dominance.h"
using namespace swift;

STATISTIC(NumStackPromoted, "Number of heap allocations promoted to the stack");
STATISTIC(NumRegPromoted, "Number of heap allocations promoted to registers");

/// optimizeAllocBox - Try to promote an alloc_box instruction to an
/// alloc_stack.  On success, this updates the IR and returns true, but does not
/// remove the alloc_box itself.
static bool optimizeAllocBox(AllocBoxInst *ABI) {
  
  // Scan all of the uses of the alloc_box to see if any of them cause the
  // allocated memory to escape.  If so, we can't promote it to the stack.  If
  // not, we can turn it into an alloc_stack.
  for (auto UI : ABI->getUses()) {
    auto *User = dyn_cast<SILInstruction>(UI->getUser());
    
    // These instructions do not cause the box's address to escape.
    if (isa<ReleaseInst>(User) ||
        isa<RetainInst>(User) ||
        isa<CopyAddrInst>(User) ||
        isa<LoadInst>(User) ||
        (isa<StoreInst>(User) && UI->getOperandNumber() == 1))
      continue;
    
    // TODO: [byref] arguments also.
    
    
    //DEBUG(llvm::errs() << "*** Failed to promote alloc_box: " << *ABI
    //      << "\n    Due to user: " << *User << "\n\n");
    // Otherwise, this looks like it escapes.
    return false;
  }
  
  
  // Okay, it looks like this value doesn't escape.  Promote it to an
  // alloc_stack.  Start by inserting the alloc stack after the alloc_box.
  SILBuilder B1(ABI->getParent(), ++SILBasicBlock::iterator(ABI),
                // FIXME: Drop this argument.
                *ABI->getParent()->getParent());
  auto *AllocVar = B1.createAllocStack(ABI->getLoc(), ABI->getElementType());
   
  // Replace all uses of the pointer operand with the spiffy new AllocVar.
  SILValue(ABI, 1).replaceAllUsesWith(AllocVar);
  
  // Remove any retain and release instructions.  Since all uses of result #1
  // are gone, this only walks through uses of result #0 (the retain count
  // pointer).
  while (!ABI->use_empty()) {
    auto *User = cast<SILInstruction>((*ABI->use_begin())->getUser());
    assert(isa<ReleaseInst>(User) || isa<RetainInst>(User));
    
    User->eraseFromParent();
  }
  
  // TODO: Determine where to insert a dealloc_var instruction (at the "last"
  // release).
  
  return true;
}

static bool optimizeAllocStack(AllocStackInst *ASI) {
  return false;
}


void swift::performSILMemoryPromotion(SILModule *M) {
  for (auto &Fn : *M)
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        SILInstruction *Inst = I;

        if (auto *ABI = dyn_cast<AllocBoxInst>(Inst)) {
          if (optimizeAllocBox(ABI)) {
            ++NumStackPromoted;
            // Carefully move iterator to avoid invalidation problems.
            ++I;
            Inst->eraseFromParent();
            continue;
          }
        } else if (auto *ASI = dyn_cast<AllocStackInst>(Inst)) {
          if (optimizeAllocStack(ASI)) {
            ++NumRegPromoted;

            // Carefully move iterator to avoid invalidation problems.
            ++I;
            Inst->eraseFromParent();
            continue;
          }
        }

        // Increment the iterator.
        ++I;
      }
    }
}


