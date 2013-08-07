//===--- StackToSSA.cpp - Promote stack allocations to SSA registers ------===//
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

#define DEBUG_TYPE "stack-to-ssa"
#include "swift/Subsystems.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "swift/SIL/Dominance.h"
using namespace swift;

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
  // Keep track of whether we will be able to remove the allocation (and all
  // stores to it) completely.
  bool CanRemoveAlloc = !ASI->getLoc().hasASTLocation();

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


  // If we promoted all the loads, have no byref uses, and have no debug
  // information, we can completely remove the alloca.  The only uses of it left
  // should be the dealloc_stack and stores.
  if (CanRemoveAlloc) {
    while (!ASI->use_empty()) {
      auto *User = cast<SILInstruction>((*ASI->use_begin())->getUser());
      assert(isa<DeallocStackInst>(User) || isa<StoreInst>(User));
      User->eraseFromParent();
    }
  }

  return PromotedLoad;
}


//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILStackToSSAPromotion(SILModule *M) {
  
  for (auto &Fn : *M) {
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        auto *ASI = dyn_cast<AllocStackInst>(I);
        if (ASI == nullptr) {
          ++I;
          continue;
        }

        // Note, this does not remove the alloc_stack instruction.
        if (optimizeAllocStack(ASI))
          ++NumRegPromoted;

        // Carefully move iterator to avoid invalidation problems.
        ++I;
        if (ASI->use_empty())
          ASI->eraseFromParent();
      }
    }
  }
}


