//===--- InOutDeshadowing.cpp - Remove non-escaping inout shadows ---------===//
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
//
// SILGen produces shadow variables for "inout" arguments to provide proper
// semantics for when
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "inout-deshadowing"
#include "swift/Subsystems.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumShadowsRemoved, "Number of inout shadow variables removed");

//===----------------------------------------------------------------------===//
//                          inout Deshadowing
//===----------------------------------------------------------------------===//

/// processAllocation - Given an AllocStackInst that is stored to from an inout
/// argument,
static void processAllocation(AllocStackInst *Alloc, SILArgument *InOutArg) {

++NumShadowsRemoved;

}


//===----------------------------------------------------------------------===//
//                     Candidate Variable Identification
//===----------------------------------------------------------------------===//

/// getStoredAllocStacks - Walk the use list of the specified value, looking for
/// stores of the whole value (or copy_value'd versions of it) to an
/// alloc_stack.
static void getStoredAllocStacks(SILValue V,
                       llvm::SmallSetVector<AllocStackInst*, 4> &StackShadows) {
  for (auto UI : V.getUses()) {
    // We don't care about analyzing uses by basic blocks.
    auto *User = dyn_cast<SILInstruction>(UI->getUser());
    if (!User) continue;

    if (auto *SI = dyn_cast<StoreInst>(User))
      if (auto *ASI = dyn_cast<AllocStackInst>(SI->getDest()))
        StackShadows.insert(ASI);

    if (auto *CV = dyn_cast<CopyValueInst>(User))
      getStoredAllocStacks(CV, StackShadows);
  }
}


/// processInOutValue - Walk the use-def list of the inout argument to find uses
/// of it.  We should only have stores and loads of the argument itself.  The
/// load should be a copy into a stack value, which is the shadow we're trying
/// to eliminate.
static void processInOutValue(SILArgument *InOutArg) {
  assert(InOutArg->getType().isAddress() &&
         "inout arguments should always be addresses");

  llvm::SmallSetVector<AllocStackInst*, 4> StackShadows;

  for (auto UI : InOutArg->getUses()) {
    // We can't promote inout arguments used by basic blocks.
    auto *ArgUser = dyn_cast<SILInstruction>(UI->getUser());
    if (!ArgUser) {
      DEBUG(llvm::errs() << "  inout variable used by basic block!\n");
      return;
    }

    // Ignore any stores that write-back the new value.  These happen before
    // returning from the function.
    if (isa<StoreInst>(ArgUser))
      continue;

    // Otherwise, we can only have loads from the argument, which come in
    // multiple forms.
    // TODO: Support address-only types.
    if (!isa<LoadInst>(ArgUser)) {
      DEBUG(llvm::errs() << "  Unexpected use of inout variable: "
                         << *ArgUser << "\n");
      return;
    }

    // The load may be used by arbitrary user code since we've done some
    // store->load forwarding already.  Look for stores of the loaded value into
    // alloc_stacks.
    getStoredAllocStacks(ArgUser, StackShadows);
  }

  // Now that we have identified and uniqued any candidate variables, try to
  // eliminate each one.  We do this as a post-pass to avoid iterator
  // invalidation complications.
  for (auto *Alloc : StackShadows)
    processAllocation(Alloc, InOutArg);
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performInOutDeshadowing(SILModule *M) {
  DEBUG(llvm::errs() << "*** inout Deshadowing\n");

  for (auto &Fn : *M) {
    if (Fn.empty()) continue;
    SILBasicBlock &EntryBlock = Fn.front();

    // For each function, find any inout arguments and try to optimize each of
    // them.
    SILFunctionTypeInfo *FTI = Fn.getFunctionTypeInfo();
    
    for (unsigned arg = 0, e = FTI->getInputTypes().size(); arg != e; ++arg) {
      if (FTI->isInOutArgument(arg))
        processInOutValue(EntryBlock.getBBArgs()[arg]);
    }
  }
}


