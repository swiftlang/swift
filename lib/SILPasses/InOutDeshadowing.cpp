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

#define DEBUG_TYPE "inout-deshadow"
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

/// processAllocation - Given an AllocStackInst that is copy_addr's to/from an
/// inout argument, check to see if it can be completely replaced by that inout
/// argument.  For this to be ok, it cannot escape and must only be initialized
/// and destroyed by copies from/to the inout argument.
static bool processAllocation(AllocStackInst *Alloc, SILArgument *InOutArg) {

  for (auto UI : Alloc->getUses()) {
    // We can't promote inout arguments used by basic blocks.
    auto *ArgUser = dyn_cast<SILInstruction>(UI->getUser());
    if (!ArgUser) return false;

    

  }

  return false;
}


//===----------------------------------------------------------------------===//
//                     Candidate Variable Identification
//===----------------------------------------------------------------------===//

/// processInOutValue - Walk the use-def list of the inout argument to find uses
/// of it.  We should only have copy_addr's into and out of it.  This
/// specifically only matches one pattern that SILGen is producing to simplify
/// the legality checks we'd otherwise need.
static void processInOutValue(SILArgument *InOutArg) {
  assert(InOutArg->getType().isAddress() &&
         "inout arguments should always be addresses");

  AllocStackInst *TheShadow = nullptr;
  bool FoundInit = false;

  for (auto UI : InOutArg->getUses()) {
    // We can't promote inout arguments used by basic blocks.
    auto *ArgUser = dyn_cast<SILInstruction>(UI->getUser());
    if (!ArgUser) {
      DEBUG(llvm::errs() << "  inout variable used by basic block!\n");
      return;
    }

    // TODO: We should eventually support extraneous loads, which will occur
    // in the future (e.g. when rdar://15170149 is implemented).
    auto CAI = dyn_cast<CopyAddrInst>(ArgUser);
    if (!CAI) {
      DEBUG(llvm::errs() << "  unknown inout variable user: " << *ArgUser);
      return;
    }

    if (UI->getOperandNumber() == 0) {
      // This is a copy out of the argument into an alloc_stack.  We only
      // support a single copy out of the argument into the stack, and it must
      // be the initialization of that stack location.
      auto *DestAlloc = dyn_cast<AllocStackInst>(CAI->getDest());
      // TODO: init/take flags don't matter for trivial types.
      if (!CAI->isInitializationOfDest() || CAI->isTakeOfSrc() || !DestAlloc) {
        DEBUG(llvm::errs() << "  unknown copy from inout: " << *ArgUser);
        return;
      }

      if (FoundInit || (TheShadow && TheShadow != DestAlloc)) {
        DEBUG(llvm::errs() << "  multiple copies from inout: " << *ArgUser);
        return;
      }

      TheShadow = DestAlloc;
      FoundInit = true;
      continue;
    }

    // We allow multiple copies out of the shadow variable which can happen due
    // to multiple return paths.
    assert(UI->getOperandNumber() == 1);

    // This should be a take from the source.
    // TODO: init/take flags don't matter for trivial types.
    auto *SrcAlloc = dyn_cast<AllocStackInst>(CAI->getSrc());
    if (CAI->isInitializationOfDest() || !CAI->isTakeOfSrc() || !SrcAlloc) {
      DEBUG(llvm::errs() << "  unknown copy back to inout: " << *ArgUser);
      return;
    }

    // Verify that it is a copy from the one true shadow for this argument.
    if (TheShadow && TheShadow != SrcAlloc) {
      DEBUG(llvm::errs() << "  copies from multiple inout shadows: "<<*ArgUser);
      return;
    }

    TheShadow = SrcAlloc;
  }

  // Now that we identified the candidate alloc_stack to remove, check to see if
  // we can legally do so.
  if (TheShadow) {
    DEBUG(llvm::errs() << "  Attempting to promote shadow variable "
                       << *TheShadow);
    if (processAllocation(TheShadow, InOutArg))
      ++NumShadowsRemoved;
    else
      DEBUG(llvm::errs() << "  promotion failed for: " << *TheShadow);
  }
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


