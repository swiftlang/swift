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
#include "swift/SIL/Dominance.h"
using namespace swift;

STATISTIC(NumStackPromoted, "Number of heap allocations promoted to the stack");
STATISTIC(NumRegPromoted, "Number of heap allocations promoted to registers");

/// getLastRelease - Determine if there is a single ReleaseInst that
/// post-dominates all of the uses of the specified AllocBox.  If so, return it.
/// If not, return null.
static ReleaseInst *getLastRelease(AllocBoxInst *ABI,
                                   SmallVectorImpl<SILInstruction*> &Users,
                                   SmallVectorImpl<ReleaseInst*> &Releases,
                                   llvm::OwningPtr<PostDominanceInfo> &PDI) {
  // If there is a single release, it must be the last release.  The calling
  // conventions used in SIL (at least in the case where the ABI doesn't escape)
  // are such that the value is live until it is explicitly released: there are
  // no calls in this case that pass ownership and release for us.
  if (Releases.size() == 1)
    return Releases.back();
  
  // If there are multiple releases of the value, we only support the case where
  // there is a single ultimate release that post-dominates all of the rest of
  // them.
  
  // Determine the most-post-dominating release by doing a linear scan over all
  // of the releases to find one that post-dominates them all.  Because we don't
  // want to do multiple scans of the block (which could be large) just keep
  // track of whether there are multiple releases in the ultimate block we find.
  bool MultipleReleasesInBlock = false;
  ReleaseInst *LastRelease = Releases[0];
  
  for (unsigned i = 1, e = Releases.size(); i != e; ++i) {
    ReleaseInst *RI = Releases[i];
    
    // If this release is in the same block as our candidate, keep track of the
    // multiple release nature of that block, but don't try to determine an
    // ordering between them yet.
    if (RI->getParent() == LastRelease->getParent()) {
      MultipleReleasesInBlock = true;
      continue;
    }
    
    // Otherwise, we need to order them.  Make sure we've computed PDI.
    if (!PDI.isValid())
      PDI.reset(new PostDominanceInfo(ABI->getParent()->getParent()));
    
    if (PDI->properlyDominates(RI->getParent(), LastRelease->getParent())) {
      // RI post-dom's LastRelease, so it is our new LastRelease.
      LastRelease = RI;
      MultipleReleasesInBlock = false;
    }
  }
  
  // Okay, we found the most-post-dominating release.  If it doesn't postdom
  // all of our uses, it would be unsafe to use it though, so check this.
  for (auto *User : Users) {
    if (User->getParent() == LastRelease->getParent())
      continue;
    
    // Make sure we've computed PDI.
    if (!PDI.isValid())
      PDI.reset(new PostDominanceInfo(ABI->getParent()->getParent()));
    
    if (!PDI->properlyDominates(LastRelease->getParent(), User->getParent()))
      return nullptr;
  }

  // Okay, the LastRelease block postdoms all users.  If there are multiple
  // releases in the block, make sure we're looking at the last one.
  if (MultipleReleasesInBlock) {
    for (auto MBBI = --LastRelease->getParent()->end(); ; --MBBI) {
      auto *RI = dyn_cast<ReleaseInst>(MBBI);
      if (RI == nullptr ||
          RI->getOperand() != SILValue(ABI, 1)) {
        assert(MBBI != LastRelease->getParent()->begin() &&
               "Didn't find any release in this block?");
        continue;
      }
      LastRelease = RI;
      break;
    }
  }
  
  return LastRelease;
}

/// checkAllocBoxUses - Scan all of the uses (recursively) of the specified
/// alloc_box, validating that they don't allow the ABI to escape.
static bool checkAllocBoxUses(AllocBoxInst *ABI, ValueBase *V,
                              SmallVectorImpl<SILInstruction*> &Users,
                              SmallVectorImpl<ReleaseInst*> &Releases) {
  for (auto UI : V->getUses()) {
    auto *User = cast<SILInstruction>(UI->getUser());
    
    // These instructions do not cause the box's address to escape.
    if (isa<RetainInst>(User) ||
        isa<CopyAddrInst>(User) ||
        isa<LoadInst>(User) ||
        isa<InitializeVarInst>(User) ||
        (isa<StoreInst>(User) && UI->getOperandNumber() == 1)) {
      Users.push_back(User);
      continue;
    }
    
    // Release doesn't either, but we want to keep track of where this value
    // gets released.
    if (auto *RI = dyn_cast<ReleaseInst>(User)) {
      Releases.push_back(cast<ReleaseInst>(RI));
      Users.push_back(User);
      continue;
    }

    // struct_element_addr project the address of a struct to the address of an
    // element.  Recursively check that the sub-element doesn't escape and
    // collect all of the uses of the value.
    if (isa<StructElementAddrInst>(User) || isa<TupleElementAddrInst>(User)) {
      Users.push_back(User);
      if (checkAllocBoxUses(ABI, User, Users, Releases))
        return true;
      continue;
    }
    
    // apply and partial_apply instructions do not capture the pointer when
    // it is passed through [byref] arguments or for indirect returns.
    if (isa<ApplyInst>(User) || isa<PartialApplyInst>(User)) {
      unsigned ArgumentNumber = UI->getOperandNumber()-1;
      SILType FnTy = User->getOperand(0).getType();
      SILFunctionTypeInfo *FTI = FnTy.getFunctionTypeInfo(*ABI->getModule());
      
      // If this is an indirect return slot, it isn't captured.
      if (ArgumentNumber == 0 && FTI->hasIndirectReturn())
        continue;
      
      Type ArgTy = FTI->getSwiftArgumentType(ArgumentNumber);
      if (ArgTy->is<LValueType>())
        continue;
    }
    
    // Otherwise, this looks like it escapes.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_box: " << *ABI
          << "    Due to user: " << *User << "\n");
    
    return true;
  }
  
  return false;
}


/// optimizeAllocBox - Try to promote an alloc_box instruction to an
/// alloc_stack.  On success, this updates the IR and returns true, but does not
/// remove the alloc_box itself.
static bool optimizeAllocBox(AllocBoxInst *ABI,
                             llvm::OwningPtr<PostDominanceInfo> &PDI) {
  SmallVector<SILInstruction*, 32> Users;
  SmallVector<ReleaseInst*, 4> Releases;
  
  // Scan all of the uses of the alloc_box to see if any of them cause the
  // allocated memory to escape.  If so, we can't promote it to the stack.  If
  // not, we can turn it into an alloc_stack.
  if (checkAllocBoxUses(ABI, ABI, Users, Releases))
    return false;
  
  // Okay, the value doesn't escape.  Determine where the last release is.  This
  // code only handles the case where there is a single "last release".  This
  // should work for us, because we don't expect code duplication that can
  // introduce different releases for different codepaths.  If this ends up
  // mattering in the future, this can be generalized.
  ReleaseInst *LastRelease = getLastRelease(ABI, Users, Releases, PDI);
  
  bool isTrivial = false;  // FIXME: Dtor required?
  
  if (LastRelease == nullptr && !isTrivial) {
    // If we can't tell where the last release is, we don't know where to insert
    // the destroy_addr for this box.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_box: " << *ABI
          << "    Cannot determine location of the last release!\n"
          << *ABI->getParent()->getParent() << "\n");
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
  
  for (auto &Fn : *M) {
    // PostDomInfo - This is the post dominance information for the specified
    // function.  It is lazily generated only if needed.
    llvm::OwningPtr<PostDominanceInfo> PostDomInfo;

    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        SILInstruction *Inst = I;

        if (auto *ABI = dyn_cast<AllocBoxInst>(Inst)) {
          if (optimizeAllocBox(ABI, PostDomInfo)) {
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
}


