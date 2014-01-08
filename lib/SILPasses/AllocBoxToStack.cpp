//===--- AllocBoxToStack.cpp - Promote alloc_box to alloc_stack -----------===//
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

#define DEBUG_TYPE "allocbox-to-stack"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/Dominance.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumStackPromoted, "Number of alloc_box's promoted to the stack");

//===----------------------------------------------------------------------===//
//                           alloc_box Promotion
//===----------------------------------------------------------------------===//


/// getLastRelease - Determine if there is a single ReleaseInst or
/// DeallocBoxInst that post-dominates all of the uses of the specified
/// AllocBox.  If so, return it.  If not, return null.
static SILInstruction *getLastRelease(AllocBoxInst *ABI,
                                      SmallVectorImpl<SILInstruction*> &Users,
                                    SmallVectorImpl<SILInstruction*> &Releases,
                                      llvm::OwningPtr<PostDominanceInfo> &PDI) {
  // If there are no releases, then the box is leaked.  Don't transform it. This
  // can only happen in hand-written SIL code, not compiler generated code.
  if (Releases.empty())
    return nullptr;
  
  
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
  SILInstruction *LastRelease = Releases[0];
  
  for (unsigned i = 1, e = Releases.size(); i != e; ++i) {
    SILInstruction *RI = Releases[i];
    
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
      auto *RI = dyn_cast<StrongReleaseInst>(MBBI);
      if (RI == nullptr ||
          RI->getOperand() != SILValue(ABI, 0)) {
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
static bool checkAllocBoxUses(AllocBoxInst *ABI, SILValue V,
                              SmallVectorImpl<SILInstruction*> &Users) {
  for (auto UI : V.getUses()) {
    auto *User = UI->getUser();
    
    // These instructions do not cause the box's address to escape.
    if (isa<CopyAddrInst>(User) ||
        isa<LoadInst>(User) ||
        isa<ProtocolMethodInst>(User) ||
        (isa<StoreInst>(User) && UI->getOperandNumber() == 1) ||
        (isa<AssignInst>(User) && UI->getOperandNumber() == 1)) {
      Users.push_back(User);
      continue;
    }
    
    // These instructions only cause the alloc_box to escape if they are used in
    // a way that escapes.  Recursively check that the uses of the instruction
    // don't escape and collect all of the uses of the value.
    if (isa<StructElementAddrInst>(User) || isa<TupleElementAddrInst>(User) ||
        isa<ProjectExistentialInst>(User) || isa<MarkUninitializedInst>(User)) {
      Users.push_back(User);
      if (checkAllocBoxUses(ABI, User, Users))
        return true;
      continue;
    }
    
    // apply and partial_apply instructions do not capture the pointer when
    // it is passed through @inout arguments or for indirect returns.
    if (auto apply = dyn_cast<ApplyInst>(User)) {
      if (apply->getSubstCalleeType()
            ->getInterfaceParameters()[UI->getOperandNumber()-1].isIndirect())
        continue;
    }
    if (auto partialApply = dyn_cast<PartialApplyInst>(User)) {
      if (partialApply->getSubstCalleeType()
            ->getInterfaceParameters()[UI->getOperandNumber()-1].isIndirect())
        continue;
      
    }

    // Otherwise, this looks like it escapes.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_box in @"
          << ABI->getFunction()->getName() << ": " << *ABI
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

  // Scan all of the uses of the alloc_box to see if any of them cause the
  // allocated memory to escape.  If so, we can't promote it to the stack.  If
  // not, we can turn it into an alloc_stack.
  SmallVector<SILInstruction*, 32> Users;
  if (checkAllocBoxUses(ABI, SILValue(ABI, 1), Users))
    return false;

  // Scan all of the uses of the retain count value, collecting all the releases
  // and validating that we don't have an unexpected user.
  SmallVector<SILInstruction*, 4> Releases;
  for (auto UI : SILValue(ABI, 0).getUses()) {
    auto *User = UI->getUser();

    if (isa<StrongRetainInst>(User))
      continue;

    // Release doesn't either, but we want to keep track of where this value
    // gets released.
    if (isa<StrongReleaseInst>(User) || isa<DeallocBoxInst>(User)) {
      Releases.push_back(User);
      Users.push_back(User);
      continue;
    }

    // Otherwise, this looks like it escapes.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_box in @"
          << ABI->getFunction()->getName() << ": " << *ABI
          << "    Due to user: " << *User << "\n");

    return false;
  }

  
  // Okay, the value doesn't escape.  Determine where the last release is.  This
  // code only handles the case where there is a single "last release".  This
  // should work for us, because we don't expect code duplication that can
  // introduce different releases for different codepaths.  If this ends up
  // mattering in the future, this can be generalized.
  SILInstruction *LastRelease = getLastRelease(ABI, Users, Releases, PDI);
  
  // FIXME: If there's no last release, we can't balance the alloc_stack.
  if (!LastRelease)
    return false;

  auto &lowering =
    ABI->getModule().Types.getTypeLowering(ABI->getElementType());
  if (LastRelease == nullptr && !lowering.isTrivial()) {
    // If we can't tell where the last release is, we don't know where to insert
    // the destroy_addr for this box.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_box: " << *ABI
          << "    Cannot determine location of the last release!\n"
          << *ABI->getParent()->getParent() << "\n");
    return false;
  }

  DEBUG(llvm::errs() << "*** Promoting alloc_box to stack: " << *ABI);
  
  // Okay, it looks like this value doesn't escape.  Promote it to an
  // alloc_stack.  Start by inserting the alloc stack after the alloc_box.
  SILBuilder B1(ABI->getParent(), ++SILBasicBlock::iterator(ABI));
  auto *ASI = B1.createAllocStack(ABI->getLoc(), ABI->getElementType());
  ASI->setDebugScope(ABI->getDebugScope());
   
  // Replace all uses of the pointer operand with the spiffy new AllocStack.
  SILValue(ABI, 1).replaceAllUsesWith(ASI->getAddressResult());
  
  // If we found a 'last release', insert a dealloc_stack instruction and a
  // destroy_addr if its type is non-trivial.
  if (LastRelease) {
    SILBuilder B2(LastRelease);

    if (!lowering.isTrivial() && !isa<DeallocBoxInst>(LastRelease))
      B2.emitDestroyAddr(CleanupLocation::getCleanupLocation(ABI->getLoc()),
                         ASI->getAddressResult());

    // Reset the insertion point in case the destroy address expanded to
    // multiple blocks.
    B2.setInsertionPoint(LastRelease);
    B2.createDeallocStack(LastRelease->getLoc(), ASI->getContainerResult());
  }
  
  // Remove any retain and release instructions.  Since all uses of result #1
  // are gone, this only walks through uses of result #0 (the retain count
  // pointer).
  while (!ABI->use_empty()) {
    auto *User = (*ABI->use_begin())->getUser();
    assert(isa<StrongReleaseInst>(User) || isa<StrongRetainInst>(User) ||
           isa<DeallocBoxInst>(User));
    
    User->eraseFromParent();
  }
  
  return true;
}

//===----------------------------------------------------------------------===//
//                             Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILAllocBoxToStackPromotion(SILModule *M) {
  
  for (auto &Fn : *M) {
    // PostDomInfo - This is the post dominance information for the specified
    // function.  It is lazily generated only if needed.
    llvm::OwningPtr<PostDominanceInfo> PostDomInfo;

    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        if (auto *ABI = dyn_cast<AllocBoxInst>(I))
          if (optimizeAllocBox(ABI, PostDomInfo)) {
            ++NumStackPromoted;
            // Carefully move iterator to avoid invalidation problems.
            ++I;
            ABI->eraseFromParent();
            continue;
          }

        ++I;
      }
    }
  }
}


