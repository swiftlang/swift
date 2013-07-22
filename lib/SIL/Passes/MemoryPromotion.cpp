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

#include "llvm/Support/CommandLine.h"
static llvm::cl::opt<bool> EnableStackPromotion("enable-stack-promotion");

STATISTIC(NumStackPromoted, "Number of heap allocations promoted to the stack");
STATISTIC(NumRegPromoted, "Number of heap allocations promoted to registers");

/// isByRefOrIndirectReturn - Return true if the specified apply/partial_apply
/// call operand is a [byref] or indirect return, indicating that the call
/// doesn't capture the pointer.
static bool isByRefOrIndirectReturn(SILInstruction *Apply,
                                    unsigned ArgumentNumber) {
  SILType FnTy = Apply->getOperand(0).getType();
  SILFunctionTypeInfo *FTI = FnTy.getFunctionTypeInfo(*Apply->getModule());

  // If this is an indirect return slot, it isn't captured.
  if (ArgumentNumber == 0 && FTI->hasIndirectReturn())
    return true;

  // Otherwise, check for [byref].
  Type ArgTy = FTI->getSwiftArgumentType(ArgumentNumber);
  return ArgTy->is<LValueType>();
}

//===----------------------------------------------------------------------===//
//                           alloc_box Promotion
//===----------------------------------------------------------------------===//


/// getLastRelease - Determine if there is a single ReleaseInst that
/// post-dominates all of the uses of the specified AllocBox.  If so, return it.
/// If not, return null.
static ReleaseInst *getLastRelease(AllocBoxInst *ABI,
                                   SmallVectorImpl<SILInstruction*> &Users,
                                   SmallVectorImpl<ReleaseInst*> &Releases,
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

    // These instructions only cause the alloc_box to escape if they are used in
    // a way that escapes.  Recursively check that the uses of the instruction
    // don't escape and collect all of the uses of the value.
    if (isa<StructElementAddrInst>(User) || isa<TupleElementAddrInst>(User) ||
        isa<ProjectExistentialInst>(User)) {
      Users.push_back(User);
      if (checkAllocBoxUses(ABI, User, Users, Releases))
        return true;
      continue;
    }
    
    // apply and partial_apply instructions do not capture the pointer when
    // it is passed through [byref] arguments or for indirect returns.
    if ((isa<ApplyInst>(User) || isa<PartialApplyInst>(User)) &&
        isByRefOrIndirectReturn(User, UI->getOperandNumber()-1))
      continue;

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
  
  bool isTrivial = ABI->getModule()->Types.
          getTypeLoweringInfo(ABI->getElementType()).isTrivial();
  
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

    if (!isTrivial)
      B2.emitDestroyAddress(ABI->getLoc(), AllocVar);

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

//===----------------------------------------------------------------------===//
//                          alloc_stack Promotion
//===----------------------------------------------------------------------===//

namespace {
  /// LOV_MultiDef - This is a sentinel used in the LiveOutValues map to keep
  /// track of the case where there are multiple stores in a block.
  static const auto LOV_MultiDef = (SILInstruction*)1U;

  class AllocStackPromotionState {
    llvm::SmallPtrSet<SILInstruction *, 32> Stores;
    llvm::DenseMap<SILBasicBlock*, SILInstruction*> LiveOutValues;

  public:

    /// addStore - Add a (may) store to the set of values we're tracking.
    void addStore(SILInstruction *S);

    /// getLoadedValue - Try to determine the value that will be produced by the
    /// specified load instruction.  This may fail, e.g. when we get to a
    /// may-store like a byref call.
    SILValue getLoadedValue(LoadInst *L);
  };
} // end anonymous namespace.


/// addStore - Add a (may) store to the set of values we're tracking.
void AllocStackPromotionState::addStore(SILInstruction *S) {
  Stores.insert(S);

  // Determine if we already have a store for this block.  If so, store
  // LOV_MultiDef to remember this.  We'll lazily compute the last store in the
  // block if there is a query of the live-out value.
  auto &Entry = LiveOutValues[S->getParent()];
  if (Entry == nullptr)
    Entry = S;
  else
    Entry = LOV_MultiDef;
}



/// getStoredValueFrom - Given a may store to the stack slot we're promoting,
/// return the value being stored.
static SILValue getStoredValueFrom(SILInstruction *I) {
  if (auto *SI = dyn_cast<StoreInst>(I))
    return SI->getOperand(0);
  return SILValue();
}



/// getLoadedValue - Try to determine the value that will be produced by the
/// specified load instruction.
SILValue AllocStackPromotionState::getLoadedValue(LoadInst *L) {
  // If there is a store in the same block as the load, do a local scan to see
  // if the store is before the load or not.
  if (LiveOutValues.count(L->getParent())) {
    for (SILBasicBlock::iterator BBI = L, E = L->getParent()->begin();
         BBI != E;) {
      SILInstruction *TheInst = --BBI;
      if (Stores.count(TheInst))
        return getStoredValueFrom(TheInst);
    }
  }

  // TODO: implement SSA construction.
  return SILValue();
}



/// optimizeAllocStack - Try to promote a loads from an alloc_stack instruction
/// to use the previously stored value.
///
/// Note that, if the variable has location information, this optimization does
/// *not* remove the alloc_stack, nor does it remove any stores to the
/// alloc_stack, since doing so would pessimize debug information.  It is safe
/// to completely promote and remove alloc_stack instructions without location
/// information though (e.g. temporaries for indirect return slots).
static bool optimizeAllocStack(AllocStackInst *ASI) {

  if (!EnableStackPromotion) return false;

  // Keep track of whether we will be able to remove the allocation (and all
  // stores to it) completely.
  bool CanRemoveAlloc = ASI->getLoc().isNull();

  // Scan all of the uses of the alloc_stack to determine if we can promote the
  // uses.  Keep track of loads that we see, along with any (potential) stores
  // due to stores, byref arguments, etc.
  SmallVector<LoadInst*, 8> LoadsToPromote;
  SmallVector<SILInstruction*, 8> Stores;

  for (auto UI : ASI->getUses()) {
    auto *User = cast<SILInstruction>(UI->getUser());

    if (auto *LI = dyn_cast<LoadInst>(User)) {
      LoadsToPromote.push_back(LI);
      continue;
    }

    // These are stores to the alloc_stack.
    if (isa<InitializeVarInst>(User) ||
        (isa<StoreInst>(User) && UI->getOperandNumber() == 1)) {
      Stores.push_back(User);
      continue;
    }

    // apply and partial_apply instructions do not capture the pointer when
    // it is passed through [byref] arguments or for indirect returns, but we
    // need to treat them as a may-store.
    if ((isa<ApplyInst>(User) || isa<PartialApplyInst>(User)) &&
        isByRefOrIndirectReturn(User, UI->getOperandNumber()-1)) {
      Stores.push_back(User);

      // We can't remove the allocation if there is a byref store to it.
      CanRemoveAlloc = false;
      continue;
    }

    // These show up as uses but aren't significant for the analysis.
    if (isa<DeallocStackInst>(User))
      continue;

    // TODO: struct_element_addr / tuple_element_addr.

    // Otherwise, this escapes to another pointer, and may be modified without
    // our knowing about it.
    DEBUG(llvm::errs() << "*** Failed to promote alloc_stack: " << *ASI
          << "    Due to user: " << *User << "\n");

    return false;
  }


  // Now that we've collected all of the loads and stores, and know that no
  // pointers are escaping, build some CFG-centric information.
  AllocStackPromotionState PromotionState;

  for (auto S : Stores)
    PromotionState.addStore(S);

  // Try to promote all loads of the stack slot.
  bool PromotedLoad = false;
  for (auto L : LoadsToPromote) {
    // If we failed to promote a load, we can't remove the allocation.
    SILValue V = PromotionState.getLoadedValue(L);
    if (!V) {
      CanRemoveAlloc = false;
      continue;
    }

    PromotedLoad = true;
    SILValue(L, 0).replaceAllUsesWith(V);
    L->eraseFromParent();
  }

  if (CanRemoveAlloc) {
    // FIXME: remove allocation and all uses.
  }

  return PromotedLoad;
}


//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

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
          // Note, this does not remove the alloc_stack instruction.
          if (optimizeAllocStack(ASI))
            ++NumRegPromoted;

          // Carefully move iterator to avoid invalidation problems.
          ++I;
          if (Inst->use_empty())
            Inst->eraseFromParent();
          continue;
        }

        // Increment the iterator.
        ++I;
      }
    }
  }
}


