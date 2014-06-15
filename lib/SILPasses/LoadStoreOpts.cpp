//===---- LoadStoreOpts.cpp - SIL Load/Store Optimizations Forwarding -----===//
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

#define DEBUG_TYPE "load-store-opts"
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

/// Returns true if this is an instruction that may have side effects in a
/// general sense but are inert from a load store perspective.
static bool isLSForwardingInertInstruction(SILInstruction *Inst) {
  switch (Inst->getKind()) {
  case ValueKind::StrongRetainInst:
  case ValueKind::RetainValueInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::CondFailInst:
    return true;
  default:
    return false;
  }
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

namespace {

/// State of the load store forwarder in one basic block.
class LSBBForwarder {

  /// The basic block that we are optimizing.
  SILBasicBlock *BB;

  /// The alias analysis that we are using for alias queries.
  AliasAnalysis *AA;

  /// The current list of store instructions that stored to memory locations
  /// that were not read/written to since the store was executed.
  llvm::SmallPtrSet<StoreInst *, 8> Stores;

  // This is a list of LoadInst instructions that reference memory locations
  // were not clobbered by instructions that write to memory. In other words
  // the SSA value of the load is known to be the same value as the referenced
  // pointer. The values in the list are potentially updated on each iteration
  // of the loop below.
  llvm::SmallPtrSet<LoadInst *, 8> Loads;

  /// During the last run of the forwarder, did we make any changes.
  bool Changed;
public:
  LSBBForwarder(SILBasicBlock *BB, AliasAnalysis *AA) : BB(BB), AA(AA),
                                                        Changed(false) {}

  bool optimize();

  /// Clear all state in the BB optimizer.
  void clear() {
    Stores.clear();
    Loads.clear();
    Changed = false;
  }

  void deleteInstruction(SILInstruction *I) {
    recursivelyDeleteTriviallyDeadInstructions(I, true,
                                               [&](SILInstruction *DeadI) {
      if (LoadInst *LI = dyn_cast<LoadInst>(DeadI))
        Loads.erase(LI);
      if (StoreInst *SI = dyn_cast<StoreInst>(DeadI))
        Stores.erase(SI);
    });
  }

  /// Invalidate any loads that we can not prove that Inst does not write to.
  void invalidateAliasingLoads(SILInstruction *Inst) {
    llvm::SmallVector<LoadInst *, 4> InvalidatedLoadList;
    for (auto *LI : Loads)
      if (AA->mayWriteToMemory(Inst, LI->getOperand()))
        InvalidatedLoadList.push_back(LI);
    for (auto *LI : InvalidatedLoadList) {
      DEBUG(llvm::dbgs() << "    Found an instruction that writes to memory "
                            "such that a load is invalidated:" << *LI);
      Loads.erase(LI);
    }
  }

  void invalidateWriteToStores(SILInstruction *Inst) {
    llvm::SmallVector<StoreInst *, 4> InvalidatedStoreList;
    for (auto *SI : Stores)
      if (AA->mayWriteToMemory(Inst, SI->getDest()))
        InvalidatedStoreList.push_back(SI);
    for (auto *SI : InvalidatedStoreList) {
      DEBUG(llvm::dbgs() << "    Found an instruction that writes to memory "
                            "such that a store is invalidated:" << *SI);
      Stores.erase(SI);
    }
  }

  void invalidateReadFromStores(SILInstruction *Inst) {
    llvm::SmallVector<StoreInst *, 4> InvalidatedStoreList;
    for (auto *SI : Stores)
      if (AA->mayReadFromMemory(Inst, SI->getDest()))
        InvalidatedStoreList.push_back(SI);
    for (auto *SI : InvalidatedStoreList) {
      DEBUG(llvm::dbgs() << "    Found an instruction that reads from memory "
                            "such that a store is invalidated:" << *SI);
      Stores.erase(SI);
    }
  }

  /// Try to prove that SI is a dead store updating all current state. If SI is
  /// dead, eliminate it.
  void tryToEliminateDeadStores(StoreInst *SI);

  /// Try to find a previously known value that we can forward to LI. This
  /// includes from stores and loads.
  void tryToForwardLoad(LoadInst *LI);
};

} // end anonymous namespace

void LSBBForwarder::tryToEliminateDeadStores(StoreInst *SI) {
  // If we are storing a value that is available in the load list then we
  // know that no one clobbered that address and the current store is
  // redundant and we can remove it.
  if (LoadInst *LdSrc = dyn_cast<LoadInst>(SI->getSrc())) {
    // Check that the loaded value is live and that the destination address
    // is the same as the loaded address.
    if (Loads.count(LdSrc) && LdSrc->getOperand() == SI->getDest()) {
      Changed = true;
      deleteInstruction(SI);
      NumDeadStores++;
      return;
    }
  }

  // Invalidate any load that we can not prove does not read from the stores
  // destination.
  invalidateAliasingLoads(SI);

  // If we are storing to a previously stored address then delete the old
  // store.
  for (auto *PrevStore : Stores) {
    if (SI->getDest() != PrevStore->getDest())
      continue;

    DEBUG(llvm::dbgs() << "    Found a dead previous store... Removing...:"
          << *PrevStore);
    Changed = true;
    deleteInstruction(PrevStore);
    NumDeadStores++;
  }

  // Insert SI into our store list.
  Stores.insert(SI);
}

void LSBBForwarder::tryToForwardLoad(LoadInst *LI) {  
  // If we are loading a value that we just saved then use the saved value.
  for (auto *PrevStore : Stores) {
    if (PrevStore->getDest() != LI->getOperand())
      continue;

    DEBUG(llvm::dbgs() << "    Forwarding store from: " << *PrevStore);
    SILValue(LI, 0).replaceAllUsesWith(PrevStore->getSrc());
    deleteInstruction(LI);
    Changed = true;
    NumForwardedLoads++;
    return;
  }

  // Promote partial loads.
  // Check that we are loading a struct element:
  if (auto *SEAI = dyn_cast<StructElementAddrInst>(LI->getOperand())) {
    for (auto *PrevStore : Stores) {
      // And that the previous store stores into that struct.
      if (PrevStore->getDest() != SEAI->getOperand())
        continue;

      // And that the stored value is a struct construction instruction:
      auto *SI = dyn_cast<StructInst>(PrevStore->getSrc());
      if (!SI)
        continue;

      DEBUG(llvm::dbgs() << "    Forwarding element store from: "
            << *PrevStore);
      unsigned FieldNo = SEAI->getFieldNo();
      SILValue(LI, 0).replaceAllUsesWith(SI->getOperand(FieldNo));
      deleteInstruction(LI);
      Changed = true;
      NumForwardedLoads++;
      return;
    }
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
    deleteInstruction(LI);
    Changed = true;
    NumDupLoads++;
    return;
  }

  Loads.insert(LI);
}

/// \brief Promote stored values to loads, remove dead stores and merge
/// duplicated loads.
bool LSBBForwarder::optimize() {
  clear();

  auto II = BB->begin(), E = BB->end();
  while (II != E) {
    SILInstruction *Inst = II++;

    DEBUG(llvm::dbgs() << "Visiting: " << *Inst);

    // This is a StoreInst. Let's see if we can remove the previous stores.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      tryToEliminateDeadStores(SI);
      continue;
    }

    // This is a LoadInst. Let's see if we can find a previous loaded, stored
    // value to use instead of this load.
    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      tryToForwardLoad(LI);
      continue;
    }

    // If this instruction has side effects, but is inert from a load store
    // perspective, skip it.
    if (isLSForwardingInertInstruction(Inst)) {
      DEBUG(llvm::dbgs() << "    Found inert instruction: " << *Inst);
      continue;
    }

    if (auto *AI = dyn_cast<ApplyInst>(Inst))
      if (auto *BI = dyn_cast<BuiltinFunctionRefInst>(&*AI->getCallee()))
        if (isReadNone(BI)) {
          DEBUG(llvm::dbgs() << "    Found readnone builtin, does not affect "
                "loads and stores.\n");
          continue;
        }

    // All other instructions that read from the memory location of the store
    // invalidates the store.
    if (Inst->mayReadFromMemory()) {
      invalidateReadFromStores(Inst);
    }

    // If we have an instruction that may write to memory and we can not prove
    // that it and its operands can not alias a load we have visited, invalidate
    // that load.
    if (Inst->mayWriteToMemory()) {
      // Invalidate any load that we can not prove does not read from one of the
      // writing instructions operands.
      invalidateAliasingLoads(Inst);

      // Invalidate our store if Inst writes to the destination location.
      invalidateWriteToStores(Inst);
    }
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
    for (auto &BB : F) {
      LSBBForwarder Forwarder(&BB, AA);
      while (Forwarder.optimize())
        Changed = true;
    }

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Load Store Opts"; }
};

} // end anonymous namespace

SILTransform *swift::createLoadStoreOpts() {
  return new LoadStoreOpts();
}
