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
#include "llvm/Support/Debug.h"
//#include "swift/SIL/Dominance.h"
using namespace swift;

STATISTIC(NumStackPromoted, "Number of heap allocations promoted to the stack");
STATISTIC(NumRegPromoted, "Number of heap allocations promoted to registers");

/// getLastRelease - Determine if there is a single ReleaseInst that
/// post-dominates all of the uses of the specified AllocBox.  If so, return it.
/// If not, return null.
static ReleaseInst *getLastRelease(AllocBoxInst *ABI,
                                   SmallVectorImpl<ReleaseInst*> &Releases) {
  // If there is a single release, it must be the last release.  The calling
  // conventions used in SIL (at least in the case where the ABI doesn't escape)
  // are such that the value is live until it is explicitly released: there are
  // no calls in this case that pass ownership and release for us.
  if (Releases.size() == 1)
    return Releases.back();
  
  // FIXME: implement this.
  return nullptr;
}


/// optimizeAllocBox - Try to promote an alloc_box instruction to an
/// alloc_stack.  On success, this updates the IR and returns true, but does not
/// remove the alloc_box itself.
static bool optimizeAllocBox(AllocBoxInst *ABI) {
  SmallVector<ReleaseInst*, 4> Releases;
  
  // Scan all of the uses of the alloc_box to see if any of them cause the
  // allocated memory to escape.  If so, we can't promote it to the stack.  If
  // not, we can turn it into an alloc_stack.
  for (auto UI : ABI->getUses()) {
    auto *User = cast<SILInstruction>(UI->getUser());
    
    // These instructions do not cause the box's address to escape.
    if (isa<RetainInst>(User) ||
        isa<CopyAddrInst>(User) ||
        isa<LoadInst>(User) ||
        isa<InitializeVarInst>(User) ||
        (isa<StoreInst>(User) && UI->getOperandNumber() == 1))
      continue;
    
    // Release doesn't either, but we want to keep track of where this value
    // gets released.
    if (auto *RI = dyn_cast<ReleaseInst>(User)) {
      Releases.push_back(cast<ReleaseInst>(RI));
      continue;
    }
    
    // TODO: [byref] arguments also.
    
    // Otherwise, this looks like it escapes.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_box: " << *ABI
          << "    Due to user: " << *User << "\n");
    
    return false;
  }
  
  
  // Okay, the value doesn't escape.  Determine where the last release is.  This
  // code only handles the case where there is a single "last release".  This
  // should work for us, because we don't expect code duplication that can
  // introduce different releases for different codepaths.  If this ends up
  // mattering in the future, this can be generalized.
  ReleaseInst *LastRelease = getLastRelease(ABI, Releases);
  
  bool isTrivial = false;  // FIXME: Dtor required?
  
  if (LastRelease == nullptr && !isTrivial) {
    // If we can't tell where the last release is, we don't know where to insert
    // the destroy_addr for this box.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_box: " << *ABI
          << "    Don't know where the last release is!\n\n");
    return false;
  }

  DEBUG({
    llvm::errs() << "*** Promoting alloc_box to stack: " << *ABI;
    for (auto UI : ABI->getUses())
      llvm::errs() << "    User: " << *UI->getUser();
  });
  
  // Okay, it looks like this value doesn't escape.  Promote it to an
  // alloc_stack.  Start by inserting the alloc stack after the alloc_box.
  SILBuilder B1(ABI->getParent(), ++SILBasicBlock::iterator(ABI));
  auto *AllocVar = B1.createAllocStack(ABI->getLoc(), ABI->getElementType());
   
  // Replace all uses of the pointer operand with the spiffy new AllocVar.
  SILValue(ABI, 1).replaceAllUsesWith(AllocVar);
  
  // If we found a 'last release', insert a dealloc_stack instruction and a
  // destroy_addr if its type is non-trivial.
  if (LastRelease) {
    SILBuilder B2(LastRelease);

    if (!isTrivial) {
      // FIXME: If this is a non-address-only type, use a load and the
      // "emitReleaseRValue" logic.
      B2.createDestroyAddr(ABI->getLoc(), AllocVar);
    }

    B2.createDeallocStack(ABI->getLoc(), AllocVar);
  }
  
  // Remove any retain and release instructions.  Since all uses of result #1
  // are gone, this only walks through uses of result #0 (the retain count
  // pointer).
  while (!ABI->use_empty()) {
    auto *User = cast<SILInstruction>((*ABI->use_begin())->getUser());
    assert(isa<ReleaseInst>(User) || isa<RetainInst>(User));
    
    User->eraseFromParent();
  }
  
  
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


