//===--- SILMem2Reg.cpp - Promotes AllocStacks to registers ---------------===//
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

#define DEBUG_TYPE "sil-mem2reg"
#include "swift/SILPasses/Passes.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/ImmutableSet.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumAllocStackRemoved, "Number of AllocStack promoted");
STATISTIC(NumAllocStackFound, "Number of AllocStack found");
STATISTIC(NumAllocStackCaptured, "Number of AllocStack captured");

namespace {

/// Promote memory to registers
class MemoryToRegisters {
  /// The function that we are optimizing.
  SILFunction &F;

public:
  /// C'tor
  MemoryToRegisters(SILFunction &Func) : F(Func) {}

  /// Promote memory to registers.
  void run();
};

} // end anonymous namespace.

/// Returns true if this AllocStacks is captured.
static bool isCaptured(AllocStackInst *ASI) {
  // For all users of the AllocStack instruction.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();

    // Loads are okay.
    if (isa<LoadInst>(II))
      continue;

    // We can store into an AllocStack (but not the pointer).
    if (StoreInst *SI = dyn_cast<StoreInst>(II))
      if (SI->getDest().getDef() == ASI)
        continue;

    // Deallocation is also okay.
    if (isa<DeallocStackInst>(II))
      continue;


    // Other instructions are assumed to capture the AllocStack.
    DEBUG(llvm::errs() << "*** AllocStack is captured by: " << *II << "\n");
    return true;
  }

  // None of the users capture the AllocStack.
  return false;
}

/// Returns true if the AllocStack is only stored into.
static bool isWriteOnlyAllocation(AllocStackInst *ASI) {
  // For all users of the AllocStack:
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();

    // It is okay to store into this AllocStack.
    if (StoreInst *SI = dyn_cast<StoreInst>(II))
      if (!isa<AllocStackInst>(SI->getSrc()))
        continue;

    // It is also okay to deallocate.
    if (isa<DeallocStackInst>(II))
      continue;

    // Can't do anything else with it.
    DEBUG(llvm::errs() << "*** AllocStack is loaded by: " << *II << "\n");
    return false;
  }

  return true;
}

/// Returns true if this AllocStack is only used within a single basic block.
static bool isSingleBlockUsage(AllocStackInst *ASI) {
  assert(!isCaptured(ASI) && "This AllocStack must not be captured");
  SILBasicBlock *BB = ASI->getParent();

  // All of the users of the AllocStack must be in the same block.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI)
    if (UI->getUser()->getParent() != BB)
      return false;

  return true;
}

/// Promote all of the AllocStacks in a single basic block in one linear scan.
/// Note: This function deletes all of the users of the AllocStackInst,
/// including the DeallocStackInst. However, it does not remove the
// AllocStackInst itself!
static void promoteAllocationInBlock(AllocStackInst *ASI) {
  DEBUG(llvm::errs() << "*** Promoting in-block: " << *ASI << "\n");

  SILBasicBlock *BB = ASI->getParent();

  // The default value of the AllocStack is NULL because we don't have
  // unilitialized variables in Swift.
  SILValue RunningVal = SILValue();

  // For all instructions in the block.
  for (auto BBI = BB->begin(), E = BB->end(); BBI != E;) {
    SILInstruction *Inst = BBI++;
    // Remove instructions that we are loading from. Replace the loaded value
    // with our running value.
    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      if (LI->getOperand().getDef() == ASI) {
        assert(RunningVal.isValid() &&
               "The AllocStack must be initialized before usage.");
        SILValue(Inst, 0).replaceAllUsesWith(RunningVal);
        Inst->eraseFromParent();
        continue;
      }
    }

    // Remove stores and record the value that we are saving as the running
    // value.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      if (SI->getDest().getDef() == ASI) {
        RunningVal = SI->getSrc();
        Inst->eraseFromParent();
        continue;
      }
    }

    // Remove deallocation.
    if (DeallocStackInst *DSI = dyn_cast<DeallocStackInst>(Inst)) {
      if (DSI->getOperand() == ASI)
        Inst->eraseFromParent();
    }
  }
}

void MemoryToRegisters::run() {
  for (auto &BB : F) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I;
      AllocStackInst *ASI = dyn_cast<AllocStackInst>(Inst);
      if (!ASI) {
        ++I;
        continue;
      }

      DEBUG(llvm::errs()<< "*** Memory to register looking at: " << *I << "\n");
      NumAllocStackFound++;

      // Don't handle captured AllocStacks.
      if (isCaptured(ASI)) {
        NumAllocStackCaptured++;
        ++I;
        continue;
      }

      // For AllocStacks that are only used within a single basic blocks, use
      // the linear sweep to remove the AllocStack.
      if (isSingleBlockUsage(ASI)) {
        promoteAllocationInBlock(ASI);

        DEBUG(llvm::errs() << "*** Deleting single block AllocStackInst: " <<
              *ASI << "\n");
        I++;
        ASI->eraseFromParent();
        NumAllocStackRemoved++;
        continue;
      }

      // Remove write-only AllocStacks.
      if (isWriteOnlyAllocation(ASI)) {
        eraseUsesOfInstruction(ASI);

        DEBUG(llvm::errs() << "*** Deleting store-only AllocStackInst: " <<
              *ASI << "\n");
        I++;
        ASI->eraseFromParent();
        NumAllocStackRemoved++;
        continue;
      }

      DEBUG(llvm::errs() << "*** Need to insert PHIs for " << *ASI << "\n");
      // TODO: Replace AllocStacks with PHI-nodes ...

      // Move iterator to avoid invalidation.
      ++I;
    }
  }
}

void promoteAllocasInFunction(SILFunction &F) {
  MemoryToRegisters(F).run();
}

void swift::performSILMem2Reg(SILModule *M) {
  for (auto &F : *M)
    promoteAllocasInFunction(F);

}
