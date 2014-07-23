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
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

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
static SILValue
findExtractPathFromAddressValueToLoad(SILValue Address, SILValue StoredValue,
                                      SILInstruction *Inst, SILValue InstOp) {
  // Attempt to find the projection path from Address -> Load->getOperand().
  llvm::SmallVector<Projection, 4> ProjectionPath;

  // If we failed to find the path, return an empty value early.
  if (!findAddressProjectionPathBetweenValues(Address, InstOp, ProjectionPath))
    return SILValue();

  // If we found a projection path, but there are no projections, then the two
  // loads must be the same, return PrevLI.
  if (ProjectionPath.empty())
    return StoredValue;

  // Ok, at this point we know that we can construct our aggregate projections
  // from our list of address projections.
  SILValue LastExtract = StoredValue;
  SILBuilder Builder(Inst);
  while (!ProjectionPath.empty()) {
    auto P = ProjectionPath.pop_back_val();
    if (ValueDecl *D = P.getDecl()) {
      if (P.getNominalType() == Projection::NominalType::Struct) {
        LastExtract = Builder.createStructExtract(Inst->getLoc(), LastExtract,
                                                  cast<VarDecl>(D),
                                                  P.getType().getObjectType());
        assert(cast<StructExtractInst>(*LastExtract).getStructDecl() &&
               "Instruction must have a struct decl!");
      } else {
        assert(P.getNominalType() == Projection::NominalType::Enum &&
               "Expected an enum decl here. Only other possibility is a "
               "class which we do not support");
        LastExtract = Builder.createUncheckedEnumData(
            Inst->getLoc(), LastExtract, cast<EnumElementDecl>(D),
            P.getType().getObjectType());
        assert(cast<UncheckedEnumDataInst>(*LastExtract).getElement() &&
               "Instruction must have an enum element decl!");
      }
    } else {
      LastExtract = Builder.createTupleExtract(Inst->getLoc(), LastExtract,
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

  /// The post dominance analysis that we use for dead store elimination.
  PostDominanceInfo *PDI;

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
  LSBBForwarder() = default;

  void init(SILBasicBlock *NewBB, AliasAnalysis *NewAA,
            PostDominanceInfo *NewPDI) {
    BB = NewBB;
    AA = NewAA;
    PDI = NewPDI;
    Changed = false;
  }

  bool optimize();

  SILBasicBlock *getBB() const { return BB; }

  void mergePredecessorStates(llvm::DenseMap<SILBasicBlock *,
                                             unsigned> &BBToBBIDMap,
                              std::vector<LSBBForwarder> &BBIDToForwarderMap);

  /// Clear all state in the BB optimizer.
  void clear() {
    Stores.clear();
    Loads.clear();
    Changed = false;
  }

  /// Add this load to our tracking list.
  void startTrackingLoad(LoadInst *LI) {
    DEBUG(llvm::dbgs() << "        Tracking Load: " << *LI);
    Loads.insert(LI);
  }

  void stopTrackingLoad(LoadInst *LI) {
    DEBUG(llvm::dbgs() << "        No Longer Tracking Load: " << *LI);
    Loads.erase(LI);
  }

  /// Add this store to our tracking list.
  void startTrackingStore(StoreInst *SI) {
    DEBUG(llvm::dbgs() << "        Tracking Store: " << *SI);
    Stores.insert(SI);
  }

  void stopTrackingStore(StoreInst *SI) {
    DEBUG(llvm::dbgs() << "        No Longer Store: " << *SI);
    Stores.erase(SI);
  }

  void deleteInstruction(SILInstruction *I) {
    DEBUG(llvm::dbgs() << "        Deleting instruction recursively: " << *I);
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
      DEBUG(llvm::dbgs() << "        Found an instruction that writes to memory"
                            " such that a load is invalidated:" << *LI);
      stopTrackingLoad(LI);
    }
  }

  void invalidateWriteToStores(SILInstruction *Inst) {
    llvm::SmallVector<StoreInst *, 4> InvalidatedStoreList;
    for (auto *SI : Stores)
      if (AA->mayWriteToMemory(Inst, SI->getDest()))
        InvalidatedStoreList.push_back(SI);
    for (auto *SI : InvalidatedStoreList) {
      DEBUG(llvm::dbgs() << "        Found an instruction that writes to memory"
                            " such that a store is invalidated:" << *SI);
      stopTrackingStore(SI);
    }
  }

  void invalidateReadFromStores(SILInstruction *Inst) {
    llvm::SmallVector<StoreInst *, 4> InvalidatedStoreList;
    for (auto *SI : Stores)
      if (AA->mayReadFromMemory(Inst, SI->getDest()))
        InvalidatedStoreList.push_back(SI);
    for (auto *SI : InvalidatedStoreList) {
      DEBUG(llvm::dbgs() << "        Found an instruction that reads from "
                            "memory such that a store is invalidated:"
                         << *SI);
      stopTrackingStore(SI);
    }
  }

  /// Try to prove that SI is a dead store updating all current state. If SI is
  /// dead, eliminate it.
  void tryToEliminateDeadStores(StoreInst *SI);

  /// Try to find a previously known value that we can forward to LI. This
  /// includes from stores and loads.
  void tryToForwardLoad(LoadInst *LI);

private:

  /// Merge in the state of an individual predecessor.
  void mergePredecessorState(LSBBForwarder &OtherState);
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

  // If we are storing to a previously stored address that this store post
  // dominates, delete the old store.
  llvm::SmallVector<StoreInst *, 4> StoresToDelete;
  llvm::SmallVector<StoreInst *, 4> StoresToStopTracking;
  for (auto *PrevStore : Stores) {
    if (SI->getDest() != PrevStore->getDest())
      continue;

    // If this store does not post dominate prev store, we can not eliminate
    // it. But do remove prev store from the store list and start tracking the
    // new store.
    //
    // We are only given this if we are being used for multi-bb load store opts
    // (when this is required). If we are being used for single-bb load store
    // opts, this is not necessary, so skip it.
    if (PDI && !PDI->properlyDominates(SI, PrevStore)) {
      StoresToStopTracking.push_back(PrevStore);
      DEBUG(llvm::dbgs() << "        Found dead store... That we don't "
            "postdominate... Can't remove it but will track it.");
      continue;
    }

    DEBUG(llvm::dbgs() << "        Found a dead previous store... Removing...:"
          << *PrevStore);
    Changed = true;
    StoresToDelete.push_back(PrevStore);
    NumDeadStores++;
  }

  for (StoreInst *I : StoresToDelete)
    deleteInstruction(I);
  for (StoreInst *I : StoresToStopTracking)
    stopTrackingStore(I);

  // Insert SI into our store list.
  startTrackingStore(SI);
}

/// Given an unchecked_addr_cast with various address projections using it,
/// rewrite the forwarding stored value to a bitcast + the relevant extract
/// operations.
static SILValue
tryToForwardAddressValueToUncheckedAddrToLoad(SILValue Address,
                                              SILValue StoredValue,
                                              LoadInst *LI,
                                              UncheckedAddrCastInst *UADCI) {
  assert(LI->getOperand().stripAddressProjections() == UADCI &&
         "We assume that the UADCI is the load's address stripped of "
         "address projections.");

  // First grab the address operand of our UADCI.
  SILValue UADCIOp = UADCI->getOperand();

  // Make sure that this is equal to our address. If not, bail.
  if (UADCIOp != Address)
    return SILValue();

  // Construct the relevant bitcast.
  SILModule &Mod = UADCI->getModule();
  SILType InputTy = UADCI->getOperand().getType();
  SILType OutputTy = UADCI->getType();

  bool InputIsTrivial = InputTy.isTrivial(Mod);
  bool OutputIsTrivial = OutputTy.isTrivial(Mod);

  // If either are generic, bail.
  if (InputTy.hasArchetype() || OutputTy.hasArchetype())
    return SILValue();

  // If we have a trivial input and a non-trivial output bail.
  if (InputIsTrivial && !OutputIsTrivial) {
    return SILValue();
  }

  SILBuilder B(LI);
  SILValue CastValue;

  // If the output is trivial, we have a trivial bit cast.
  if (OutputIsTrivial) {
    CastValue =  B.createUncheckedTrivialBitCast(UADCI->getLoc(), StoredValue,
                                                 OutputTy.getObjectType());
  } else {
    // Otherwise, both must have some sort of reference counts on them. Insert
    // the ref count cast.
    CastValue = B.createUncheckedRefBitCast(UADCI->getLoc(), StoredValue,
                                            OutputTy.getObjectType());
  }

  // Then try to construct an extract path from the UADCI to the Address.
  SILValue ExtractPath =
    findExtractPathFromAddressValueToLoad(UADCI, CastValue,
                                          LI, LI->getOperand());

  // If we can not construct the extract path, bail.
  if (!ExtractPath)
    return SILValue();

  assert(ExtractPath.getType() == LI->getType().getObjectType() &&
         "Must have same types here.");

  return ExtractPath;
}

/// Given an address \p Address and a value \p Value stored there that is then
/// loaded or partially loaded by \p LI, forward the value with the appropriate
/// extracts.
static SILValue tryToForwardAddressValueToLoad(SILValue Address,
                                               SILValue StoredValue,
                                               LoadInst *LI) {
  // First if we have a store + unchecked_addr_cast + load, try to forward the
  // value the store using a bitcast.
  SILValue LIOpWithoutProjs = LI->getOperand().stripAddressProjections();
  if (auto *UADCI = dyn_cast<UncheckedAddrCastInst>(LIOpWithoutProjs))
    return tryToForwardAddressValueToUncheckedAddrToLoad(Address, StoredValue,
                                                         LI, UADCI);

  // Next, try to promote partial loads from stores. If this fails, it will
  // return SILValue(), which is also our failure condition.
  return findExtractPathFromAddressValueToLoad(Address, StoredValue, LI,
                                               LI->getOperand());
}

void LSBBForwarder::tryToForwardLoad(LoadInst *LI) {
  // If we are loading a value that we just stored, forward the stored value.
  for (auto *PrevStore : Stores) {
    SILValue Result = tryToForwardAddressValueToLoad(PrevStore->getDest(),
                                                     PrevStore->getSrc(),
                                                     LI);
    if (!Result)
      continue;

    DEBUG(llvm::dbgs() << "        Forwarding store from: " << *PrevStore);
    SILValue(LI, 0).replaceAllUsesWith(Result);
    deleteInstruction(LI);
    Changed = true;
    NumForwardedLoads++;
    return;
  }

  // Search the previous loads and replace the current load with one of the
  // previous loads.
  for (auto *PrevLI : Loads) {
    SILValue Result = tryToForwardAddressValueToLoad(PrevLI->getOperand(),
                                                     PrevLI, LI);
    if (!Result)
      continue;

    DEBUG(llvm::dbgs() << "        Replacing with previous load: "
                       << *Result);
    SILValue(LI, 0).replaceAllUsesWith(Result);
    deleteInstruction(LI);
    Changed = true;
    NumDupLoads++;
    return;
  }

  startTrackingLoad(LI);
}

/// \brief Promote stored values to loads, remove dead stores and merge
/// duplicated loads.
bool LSBBForwarder::optimize() {
  auto II = BB->begin(), E = BB->end();
  while (II != E) {
    SILInstruction *Inst = II++;

    DEBUG(llvm::dbgs() << "    Visiting: " << *Inst);

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
      DEBUG(llvm::dbgs() << "        Found inert instruction: " << *Inst);
      continue;
    }

    if (auto *AI = dyn_cast<ApplyInst>(Inst))
      if (auto *BI = dyn_cast<BuiltinFunctionRefInst>(&*AI->getCallee()))
        if (isReadNone(BI)) {
          DEBUG(llvm::dbgs() << "        Found readnone builtin, does not "
                "affect loads and stores.\n");
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

  DEBUG(llvm::dbgs() << "    Final State\n");
  DEBUG(llvm::dbgs() << "        Tracking Loads:\n";
        for (auto *LI : Loads) {
          llvm::dbgs() << "            " << *LI;
        });

  DEBUG(llvm::dbgs() << "        Tracking Stores:\n";
        for (auto *SI : Stores) {
          llvm::dbgs() << "            " << *SI;
        });

  return Changed;
}

void
LSBBForwarder::
mergePredecessorState(LSBBForwarder &OtherState) {
  // Merge in the predecessor state.
  DEBUG(llvm::dbgs() << "        Initial Stores:\n");
  llvm::SmallVector<SILInstruction *, 8> DeleteList;
  for (auto *SI : Stores) {
    DEBUG(llvm::dbgs() << "            " << *SI);
    if (OtherState.Stores.count(SI))
      continue;
    DeleteList.push_back(SI);
  }

  if (DeleteList.size()) {
    DEBUG(llvm::dbgs() << "        Stores no longer being tracked:\n");
    for (auto *SI : DeleteList) {
      stopTrackingStore(cast<StoreInst>(SI));
    }
    DeleteList.clear();
  } else {
    DEBUG(llvm::dbgs() << "        All stores still being tracked!\n");
  }

  DEBUG(llvm::dbgs() << "            Initial Loads:\n");
  for (auto *LI : Loads) {
    DEBUG(llvm::dbgs() << "            " << *LI);
    if (OtherState.Loads.count(LI))
      continue;
    DeleteList.push_back(LI);
  }

  if (DeleteList.size()) {
    DEBUG(llvm::dbgs() << "        Loads no longer being tracked:\n");
    for (auto *LI : DeleteList) {
      stopTrackingLoad(cast<LoadInst>(LI));
    }
  } else {
    DEBUG(llvm::dbgs() << "        All loads still being tracked!\n");
  }
}

void
LSBBForwarder::
mergePredecessorStates(llvm::DenseMap<SILBasicBlock *,
                                      unsigned> &BBToBBIDMap,
                       std::vector<LSBBForwarder> &BBIDToForwarderMap) {
  bool HasAtLeastOnePred = false;
  bool HasSelfCycle = false;
  for (auto Pred : BB->getPreds())
    if (Pred == BB) {
      HasSelfCycle = true;
      break;
    }

  // For each predecessor of BB...
  for (auto Pred : BB->getPreds()) {

    // Lookup the BBState associated with the predecessor and merge the
    // predecessor in.
    auto I = BBToBBIDMap.find(Pred);

    // If we can not lookup the BBID then the BB was not in the post order
    // implying that it is unreachable. LLVM will ensure that the BB is removed
    // if we do not reach it at the SIL level. Since it is unreachable, ignore
    // it.
    if (I == BBToBBIDMap.end())
      continue;

    LSBBForwarder &Other = BBIDToForwarderMap[I->second];

    // If we have not had at least one predecessor, initialize LSBBForwarder
    // with the state of the initial predecessor.
    // If BB is also a predecessor of itself, we should not initialize.
    if (!HasAtLeastOnePred && !HasSelfCycle) {
      DEBUG(llvm::dbgs() << "    Initializing with pred: " << I->second
                         << "\n");
      Stores = Other.Stores;
      Loads = Other.Loads;

      DEBUG(llvm::dbgs() << "        Tracking Loads:\n";
            for (auto *LI : Loads) {
              llvm::dbgs() << "            " << *LI;
            });

      DEBUG(llvm::dbgs() << "        Tracking Stores:\n";
            for (auto *SI : Stores) {
              llvm::dbgs() << "            " << *SI;
            });
    } else if (Pred != BB) {
      DEBUG(llvm::dbgs() << "    Merging with pred: " << I->second << "\n");
      mergePredecessorState(Other);
    }
    HasAtLeastOnePred = true;
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

class GlobalLoadStoreOpts : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() {
    SILFunction *F = getFunction();

    DEBUG(llvm::dbgs() << "***** Load Store Elimination on function: "
          << F->getName() << " *****\n");

    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *POTA = PM->getAnalysis<PostOrderAnalysis>();
    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    auto *PDI = DA->getPostDomInfo(F);

    auto ReversePostOrder = POTA->getReversePostOrder(F);
    int PostOrderSize = std::distance(ReversePostOrder.begin(),
                                      ReversePostOrder.end());

    // TODO: Each block does not need its own LSBBForwarder instance. Only
    // the set of reaching loads and stores is specific to the block.
    llvm::DenseMap<SILBasicBlock *, unsigned> BBToBBIDMap;
    std::vector<LSBBForwarder> BBIDToForwarderMap(PostOrderSize);

    for (SILBasicBlock *BB : ReversePostOrder) {
      unsigned count = BBToBBIDMap.size();
      BBToBBIDMap[BB] = count;
      BBIDToForwarderMap[count].init(BB, AA, PDI);
    }

    bool Changed = false;
    bool ChangedDuringIteration = false;
    do {
      ChangedDuringIteration = false;
      for (SILBasicBlock *BB : ReversePostOrder) {
        auto IDIter = BBToBBIDMap.find(BB);
        assert(IDIter != BBToBBIDMap.end() && "We just constructed this!?");
        unsigned ID = IDIter->second;
        LSBBForwarder &Forwarder = BBIDToForwarderMap[ID];
        assert(Forwarder.getBB() == BB && "We just constructed this!?");

        DEBUG(llvm::dbgs() << "Visiting BB #" << ID << "\n");

        // Clear forwarder.
        Forwarder.clear();

        // Merge the predecessors.
        Forwarder.mergePredecessorStates(BBToBBIDMap, BBIDToForwarderMap);

        // Remove dead stores, merge duplicate loads, and forward stores to loads.
        ChangedDuringIteration = Forwarder.optimize();
        Changed |= ChangedDuringIteration;
      }
    } while (ChangedDuringIteration);

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Load Store Opts"; }
};

} // end anonymous namespace

SILTransform *swift::createGlobalLoadStoreOpts() {
  return new GlobalLoadStoreOpts();
}
