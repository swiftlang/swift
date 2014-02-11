//===---- LoadStoreOpts.cpp - SIL Load/Store Optimizations Forwarding  ----===//
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
// This pass eliminates redundent loads, dead stores, and performs load
// forwarding.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-load-store-opts"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumDeadStores,     "Number of dead stores removed");
STATISTIC(NumDupLoads,       "Number of dup loads removed");
STATISTIC(NumForwardedLoads, "Number of loads forwarded");

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

static bool isWriteMemBehavior(SILInstruction::MemoryBehavior B) {
  switch (B) {
  case SILInstruction::MemoryBehavior::MayWrite:
  case SILInstruction::MemoryBehavior::MayReadWrite:
  case SILInstruction::MemoryBehavior::MayHaveSideEffects:
    return true;
  case SILInstruction::MemoryBehavior::None:
  case SILInstruction::MemoryBehavior::MayRead:
    return false;
  }
}

/// Given the already emitted load PrevLI, see if we can find a projection
/// address path to LI. If we can, emit the corresponding aggregate projection
/// insts and return the last such inst.
static SILValue findExtractPathBetweenValues(LoadInst *PrevLI, LoadInst *LI) {
  // Attempt to find the projection path from LI -> PrevLI.
  SILValue PrevLIOp = PrevLI->getOperand();
  SILValue LIOp = LI->getOperand();
  llvm::SmallVector<Projection, 4> ProjectionPath;

  // If we failed to find the path, return an empty value early.
  if (!findAddressProjectionPathBetweenValues(PrevLIOp, LIOp, ProjectionPath))
    return SILValue();

  // If we found a projection path, but there are no projections, then the two
  // loads must be the same, return PrevLI.
  if (ProjectionPath.empty())
    return PrevLI;

  // Ok, at this point we know that we can construct our aggregate projections
  // from our list of address projections.
  SILValue LastExtract = PrevLI;
  SILBuilder Builder(LI);
  while (!ProjectionPath.empty()) {
    auto P = ProjectionPath.pop_back_val();
    if (auto *D = P.getDecl()) {
      assert(P.getNominalType() != Projection::NominalType::Class &&
             "Aggregate projections do not exist for classes.");
      LastExtract = Builder.createStructExtract(LI->getLoc(), LastExtract,
                                                D,
                                                P.getType().getObjectType());
      assert(cast<StructExtractInst>(*LastExtract).getStructDecl() &&
             "Instruction must have a struct decl!");
    } else {
      LastExtract = Builder.createTupleExtract(LI->getLoc(), LastExtract,
                                               P.getIndex(),
                                               P.getType().getObjectType());
      assert(cast<TupleExtractInst>(*LastExtract).getTupleType() &&
             "Instruction must have a tuple type!");
    }
  }

  // Return the last extract we created.
  return LastExtract;
}

static void
invalidateAliasingLoads(SILInstruction *Inst,
                        llvm::SmallPtrSetImpl<LoadInst *> &Loads,
                        AliasAnalysis *AA) {
  llvm::SmallVector<LoadInst *, 4> InvalidatedLoadList;
  for (auto *LI : Loads)
    if (isWriteMemBehavior(AA->getMemoryBehavior(Inst, LI->getOperand())))
      InvalidatedLoadList.push_back(LI);
  for (auto *LI : InvalidatedLoadList) {
    DEBUG(llvm::dbgs() << "    Found an instruction that writes to memory "
          "such that a load is invalidated:" << *LI);
    Loads.erase(LI);
  }
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

/// \brief Promote stored values to loads, remove dead stores and merge
/// duplicated loads.
bool performLoadStoreOptimizations(SILBasicBlock *BB, AliasAnalysis *AA) {
  bool Changed = false;
  StoreInst *PrevStore = 0;
  llvm::SmallPtrSet<LoadInst *, 8> Loads;

  auto II = BB->begin(), E = BB->end();
  while (II != E) {
    SILInstruction *Inst = II++;

    DEBUG(llvm::dbgs() << "Visiting: " << *Inst);

    // This is a StoreInst. Let's see if we can remove the previous stores.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      // Invalidate any load that we can not prove does not read from the stores
      // destination.
      invalidateAliasingLoads(Inst, Loads, AA);

      // If we are storing to the previously stored address then delete the old
      // store.
      if (PrevStore && PrevStore->getDest() == SI->getDest()) {
        DEBUG(llvm::dbgs() << "    Found a dead previous store... Removing...:"
              << *PrevStore);
        Changed = true;
        recursivelyDeleteTriviallyDeadInstructions(PrevStore, true);
        PrevStore = SI;
        NumDeadStores++;
        continue;
      }
      PrevStore = SI;
      continue;
    }

    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      // If we are loading a value that we just saved then use the saved value.
      if (PrevStore && PrevStore->getDest() == LI->getOperand()) {
        DEBUG(llvm::dbgs() << "    Forwarding store from: " << *PrevStore);
        SILValue(LI, 0).replaceAllUsesWith(PrevStore->getSrc());
        recursivelyDeleteTriviallyDeadInstructions(LI, true);
        Changed = true;
        NumForwardedLoads++;
        continue;
      }

      // Search the previous loads and replace the current load with one of the
      // previous loads.
      for (auto PrevLI : Loads) {
        SILValue ForwardingExtract = findExtractPathBetweenValues(PrevLI, LI);
        if (!ForwardingExtract)
          continue;

        DEBUG(llvm::dbgs() << "    Replacing with previous load: "
              << *ForwardingExtract);
        SILValue(LI, 0).replaceAllUsesWith(ForwardingExtract);
        recursivelyDeleteTriviallyDeadInstructions(LI, true);
        Changed = true;
        LI = 0;
        NumDupLoads++;
        break;
      }

      if (LI)
        Loads.insert(LI);
      continue;
    }

    // Retains write to memory but they don't affect loads and stores.
    if (isa<StrongRetainInst>(Inst)) {
      DEBUG(llvm::dbgs() << "    Found strong retain, does not affect loads and"
            " stores.\n");
      continue;
    }

    // Dealloc stack does not affect loads and stores.
    if (isa<DeallocStackInst>(Inst)) {
      DEBUG(llvm::dbgs() << "Found a dealloc stack. Does not affect loads and "
            "stores.\n");
      continue;
    }

    if (auto *AI = dyn_cast<ApplyInst>(Inst))
      if (auto *BI = dyn_cast<BuiltinFunctionRefInst>(&*AI->getCallee()))
        if (isReadNone(BI)) {
          DEBUG(llvm::dbgs() << "    Found readnone builtin, does not affect "
                "loads and stores.\n");
          continue;
        }

    // cond_fail does not read/write memory in a manner that we care about.
    if (isa<CondFailInst>(Inst)) {
      DEBUG(llvm::dbgs() << "    Found a cond fail, does not affect "
            "loads and stores.\n");
      continue;
    }

    // All other instructions that read from memory invalidate the store.
    if (Inst->mayReadFromMemory()) {
      DEBUG(llvm::dbgs() << "    Found an instruction that reads from memory."
            " Invalidating store.\n");
      PrevStore = 0;
    }

    // If we have an instruction that may write to memory and we can not prove
    // that it and its operands can not alias a load we have visited, invalidate
    // that load.
    if (Inst->mayWriteToMemory())
      // Invalidate any load that we can not prove does not read from one of the
      // writing instructions operands.
      invalidateAliasingLoads(Inst, Loads, AA);
  }

  return Changed;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

class LoadStoreOpts : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() {
    SILFunction &F = *getFunction();

    DEBUG(llvm::dbgs() << "***** Load Store Elimination on function: "
          << F.getName() << " *****\n");

    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();

    // Remove dead stores, merge duplicate loads, and forward stores to loads.
    bool Changed = false;
    for (auto &BB : F)
      Changed |= performLoadStoreOptimizations(&BB, AA);

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Load Store Opts"; }
};

} // end anonymous namespace

SILTransform *swift::createLoadStoreOpts() {
  return new LoadStoreOpts();
}
