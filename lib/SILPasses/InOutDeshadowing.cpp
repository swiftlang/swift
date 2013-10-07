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

//STATISTIC(NumShadowsRemoved, "Number of inout shadow variables removed");

//===----------------------------------------------------------------------===//
//                          inout Deshadowing
//===----------------------------------------------------------------------===//

/// processAllocation - Given an AllocStackInst that is stored to from an inout
/// argument,
static void processAllocation(AllocStackInst *Alloc, SILArgument *InOutArg) {

  //++NumShadowsRemoved;

}


//===----------------------------------------------------------------------===//
//                     Candidate Variable Identification
//===----------------------------------------------------------------------===//

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

    // Take a look at copy_addrs that initialize alloc_stacks.
    if (auto CAI = dyn_cast<CopyAddrInst>(ArgUser))
      if (CAI->isInitializationOfDest())
        if (auto *ASI = dyn_cast<AllocStackInst>(CAI->getDest()))
          StackShadows.insert(ASI);
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


