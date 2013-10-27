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
STATISTIC(NumShadowsKept, "Number of inout shadow variables kept");

//===----------------------------------------------------------------------===//
//                          inout Deshadowing
//===----------------------------------------------------------------------===//

/// promoteShadow - Given an AllocStackInst that is copy_addr's to/from an
/// inout argument, check to see if it can be completely replaced by that inout
/// argument.  For this to be ok, it cannot escape and must only be initialized
/// and destroyed by copies from/to the inout argument.
static void promoteShadow(AllocStackInst *Alloc, SILArgument *InOutArg) {
  // At this point, we know that we have an inout argument that is only copied
  // into and out of for the purposes of this allocation, and that these copies
  // define the lifetime of the alloc_stack (by init'ing and take'ing its bits).

  // Since the allocation has already been promoted to an alloc_stack, we know
  // it doesn't escape.  Eliminate the allocation.
  while (!Alloc->use_empty()) {
    auto Use = *Alloc->use_begin();
    auto *User = Use->getUser();

    // If this is a use of the 0th result, not the address result, just zap the
    // instruction.  It is a dealloc_stack or something similar.
    if (Use->get().getResultNumber() == 0) {
      User->eraseFromParent();
      continue;
    }

    // Otherwise, it is a use of the argument.  If this is a copy_addr that
    // defines or destroys the value, then remove it.
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      if (CAI->getSrc() == InOutArg || CAI->getDest() == InOutArg) {
        User->eraseFromParent();
        continue;
      }
    }

    // Otherwise, this is something else that is using the memory.  Remap this
    // to use the InOutArg directly instead of using the allocation.
    Use->set(InOutArg);
  }

  ++NumShadowsRemoved;
  Alloc->eraseFromParent();
}


//===----------------------------------------------------------------------===//
//                     Candidate Variable Identification
//===----------------------------------------------------------------------===//


static bool hasTrivialElementType(AllocStackInst *Alloc) {
  auto &M = Alloc->getModule();
  return M.Types.getTypeLowering(Alloc->getElementType()).isTrivial();
}

namespace {
struct CopyAddrParts {
  bool isSourceOfCopyAddr;  // True if this is the source, false if this is dest
  SILValue Src, Dest;
  bool isTakeOfSrc, isInitializationOfDest;
};
}

/// isCopyAddrOperation - Check to see if the specied use of an @inout argument
/// is a copy_addr, or if it is part of a sequence of instructions that
/// implements copy_addr semantics.  The later can occur when a copy_addr is
/// exploded to expose the loaded intermediate value.
static bool isCopyAddrOperation(Operand *UI, CopyAddrParts &Result) {
  // We can't promote inout arguments used by basic blocks.
  auto *ArgUser = dyn_cast<SILInstruction>(UI->getUser());
  if (!ArgUser) {
    DEBUG(llvm::errs() << "    inout variable used by basic block!\n");
    return false;
  }
  
  // If this is an explicit copy_addr, collect information and return.
  if (auto CAI = dyn_cast<CopyAddrInst>(ArgUser)) {
    Result.isSourceOfCopyAddr = UI->getOperandNumber() == 0;
    Result.Src = CAI->getSrc();
    Result.Dest = CAI->getDest();
    Result.isTakeOfSrc = CAI->isTakeOfSrc();
    Result.isInitializationOfDest = CAI->isInitializationOfDest();
    return true;
  }

  
  
  // TODO: We should eventually support extraneous loads, which will occur
  // in the future (e.g. when rdar://15170149 is implemented).
  
  
  
  DEBUG(llvm::errs() << "    unknown inout variable user: " << *ArgUser);
  return false;
}


/// processInOutValue - Walk the use-def list of the inout argument to find uses
/// of it.  We should only have copy_addr's into and out of it.  This
/// specifically only matches one pattern that SILGen is producing to simplify
/// the legality checks we'd otherwise need.
///
/// This returns true if it promotes away the shadow variable.
///
static bool processInOutValue(SILArgument *InOutArg) {
  assert(InOutArg->getType().isAddress() &&
         "inout arguments should always be addresses");

  AllocStackInst *TheShadow = nullptr;
  bool FoundInit = false;

  for (auto UI : InOutArg->getUses()) {
    CopyAddrParts CAInfo;
    if (!isCopyAddrOperation(UI, CAInfo))
      return false;
    
    auto ArgUser = UI->getUser();
    
    if (CAInfo.isSourceOfCopyAddr) {
      // This is a copy out of the argument into an alloc_stack.  We only
      // support a single copy out of the argument into the stack, and it must
      // be the initialization of that stack location.
      auto *DestAlloc = dyn_cast<AllocStackInst>(CAInfo.Dest);
      if (!DestAlloc) {
        DEBUG(llvm::errs() << "    copy to non-alloc-stack: " << *ArgUser);
        DEBUG(llvm::errs() << "     dest is: " << *CAInfo.Dest);
        return false;
      }

      if ((!CAInfo.isInitializationOfDest || CAInfo.isTakeOfSrc) &&
          !hasTrivialElementType(DestAlloc)) {
        DEBUG(llvm::errs() << "    unknown copy from inout: " << *ArgUser);
        return false;
      }

      if (FoundInit || (TheShadow && TheShadow != DestAlloc)) {
        DEBUG(llvm::errs() << "    multiple copies from inout: " << *ArgUser);
        return false;
      }

      TheShadow = DestAlloc;
      FoundInit = true;
      continue;
    }

    // We allow multiple copies out of the shadow variable which can happen due
    // to multiple return paths.

    // This should be a take from the source.
    // TODO: init/take flags don't matter for trivial types.
    auto *SrcAlloc = dyn_cast<AllocStackInst>(CAInfo.Src);
    if (!SrcAlloc) {
      DEBUG(llvm::errs() << "    copy back from non-alloc-stack: " << *ArgUser);
      DEBUG(llvm::errs() << "      source is: " << *CAInfo.Src);
      return false;
    }

    if ((CAInfo.isInitializationOfDest || !CAInfo.isTakeOfSrc) &&
        !hasTrivialElementType(SrcAlloc)) {
      DEBUG(llvm::errs() << "    unknown copy back to inout: " << *ArgUser);
      return false;
    }

    // Verify that it is a copy from the one true shadow for this argument.
    if (TheShadow && TheShadow != SrcAlloc) {
      DEBUG(llvm::errs() << "    copies from multiple inout shadows: "<<*ArgUser);
      return false;
    }

    TheShadow = SrcAlloc;
  }

  // Now that we identified the candidate alloc_stack to remove, do it.
  if (TheShadow) {
    DEBUG(llvm::errs() << "    Promoting shadow variable " << *TheShadow);
    promoteShadow(TheShadow, InOutArg);
    return true;
  }

  return false;
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
    SILFunctionType *FTI = Fn.getFunctionTypeInfo();
    
    for (unsigned arg = 0, e = FTI->getParameters().size(); arg != e; ++arg) {
      if (!FTI->getParameters()[arg].isIndirectInOut()) continue;

      DEBUG(llvm::errs() << "  " << Fn.getName() << ": argument #"
                         << arg << "\n");

      if (processInOutValue(EntryBlock.getBBArgs()[arg]))
        ++NumShadowsRemoved;
      else {
        ++NumShadowsKept;
      }
    }
  }
}


