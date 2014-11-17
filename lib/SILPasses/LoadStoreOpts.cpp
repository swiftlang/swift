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
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/None.h"
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
//                      Forwarding Feasability Analysis
//===----------------------------------------------------------------------===//

namespace {

enum class ForwardingKind {
  Normal,
  UncheckedAddress
};

/// This is a move-only structure. Thus is has a private default constructor and
/// a deleted copy constructor.
class ForwardingAnalysis {
  ForwardingKind Kind;
  UncheckedAddrCastInst *UADCI;
  Optional<ProjectionPath> Path;

  ForwardingAnalysis(ForwardingKind Kind,
                     UncheckedAddrCastInst *UADCI,
                     Optional<ProjectionPath> &&P)
    : Kind(Kind), UADCI(UADCI), Path(std::move(*P)) {}

  ForwardingAnalysis(ForwardingKind Kind,
                     Optional<ProjectionPath> &&P)
    : Kind(Kind), UADCI(nullptr), Path(std::move(*P)) {}

public:
  ForwardingAnalysis() = delete;
  ForwardingAnalysis(const ForwardingAnalysis &) = delete;
  ForwardingAnalysis(ForwardingAnalysis &&FFA) = default;

  static Optional<ForwardingAnalysis>
  canForwardAddrToUncheckedAddrToLd(SILValue Address, LoadInst *LI,
                                    UncheckedAddrCastInst *UADCI);
  static Optional<ForwardingAnalysis>
  canForwardAddrToLd(SILValue Address, LoadInst *LI);

  SILValue forwardAddr(SILValue Addr, SILValue StoredValue,
                       LoadInst *LI);

private:
  SILValue forwardAddrToLdWithExtractPath(SILValue Address,
                                          SILValue StoredValue,
                                          SILInstruction *Inst,
                                          SILValue InstOp);

  SILValue forwardAddrToUncheckedCastToLd(SILValue Address,
                                          SILValue StoredValue,
                                          LoadInst *LI);
};

} // end anonymous namespace

/// Given an unchecked_addr_cast with various address projections using it,
/// check if we can forward the stored value.
Optional<ForwardingAnalysis>
ForwardingAnalysis::
canForwardAddrToUncheckedAddrToLd(SILValue Address,
                                  LoadInst *LI,
                                  UncheckedAddrCastInst *UADCI) {
  assert(LI->getOperand().stripAddressProjections() == UADCI &&
         "We assume that the UADCI is the load's address stripped of "
         "address projections.");

  // First grab the address operand of our UADCI.
  SILValue UADCIOp = UADCI->getOperand();

  // Make sure that this is equal to our address. If not, bail.
  if (UADCIOp != Address)
    return llvm::NoneType::None;

  // Construct the relevant bitcast.
  SILModule &Mod = UADCI->getModule();
  SILType InputTy = UADCI->getOperand().getType();
  SILType OutputTy = UADCI->getType();

  bool InputIsTrivial = InputTy.isTrivial(Mod);
  bool OutputIsTrivial = OutputTy.isTrivial(Mod);

  // If either are generic, bail.
  if (InputTy.hasArchetype() || OutputTy.hasArchetype())
    return llvm::NoneType::None;

  // If we have a trivial input and a non-trivial output bail.
  if (InputIsTrivial && !OutputIsTrivial) {
    return llvm::NoneType::None;
  }

  SILValue LdAddr = LI->getOperand();
  auto P = ProjectionPath::getAddrProjectionPath(UADCI, LdAddr);
  if (!P)
    return llvm::NoneType::None;
  return ForwardingAnalysis(ForwardingKind::UncheckedAddress,
                            UADCI, std::move(P));
}

Optional<ForwardingAnalysis>
ForwardingAnalysis::
canForwardAddrToLd(SILValue Address, LoadInst *LI) {
  // First if we have a store + unchecked_addr_cast + load, try to forward the
  // value the store using a bitcast.
  SILValue LIOpWithoutProjs = LI->getOperand().stripAddressProjections();
  if (auto *UADCI = dyn_cast<UncheckedAddrCastInst>(LIOpWithoutProjs))
    return ForwardingAnalysis::canForwardAddrToUncheckedAddrToLd(Address, LI,
                                                                 UADCI);

  // Attempt to find the projection path from Address -> Load->getOperand().
  // If we failed to find the path, return an empty value early.
  auto P = ProjectionPath::getAddrProjectionPath(Address, LI->getOperand());
  if (!P)
    return llvm::NoneType::None;
  return ForwardingAnalysis(ForwardingKind::Normal, std::move(P));
}

/// Given an unchecked_addr_cast with various address projections using it,
/// rewrite the forwarding stored value to a bitcast + the relevant extract
/// operations.
SILValue
ForwardingAnalysis::
forwardAddrToUncheckedCastToLd(SILValue Address, SILValue StoredValue,
                               LoadInst *LI) {
  assert(UADCI && "UADCI is assumed to be non-null here");

  // Construct the relevant bitcast.
  SILModule &Mod = UADCI->getModule();
  SILType OutputTy = UADCI->getType();
  bool OutputIsTrivial = OutputTy.isTrivial(Mod);

  SILBuilderWithScope<1> B(LI);
  SILValue CastValue;

  // If the output is trivial, we have a trivial bit cast.
  if (OutputIsTrivial) {

    // The structs could have different size. We have code in the stdlib that
    // casts pointers to differently sized integer types. This code prevents
    // that we bitcast the values.
    SILType InputTy = UADCI->getOperand().getType();
    if (OutputTy.getStructOrBoundGenericStruct() &&
        InputTy.getStructOrBoundGenericStruct())
      return SILValue();

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
    forwardAddrToLdWithExtractPath(UADCI, CastValue,
                                   LI, LI->getOperand());

  assert(ExtractPath && "Already checked the feasibility.");
  assert(ExtractPath.getType() == LI->getType().getObjectType() &&
         "Must have same types here.");

  return ExtractPath;
}

SILValue
ForwardingAnalysis::forwardAddr(SILValue Addr, SILValue StoredValue,
                                LoadInst *LI) {
  // First if we have a store + unchecked_addr_cast + load, try to forward the
  // value the store using a bitcast.
  if (Kind == ForwardingKind::UncheckedAddress)
    return forwardAddrToUncheckedCastToLd(Addr, StoredValue, LI);

  assert(Kind == ForwardingKind::Normal && "The default kind is Normal.");

  // Next, try to promote partial loads from stores. If this fails, it will
  // return SILValue(), which is also our failure condition.
  return forwardAddrToLdWithExtractPath(Addr, StoredValue, LI,
                                        LI->getOperand());
}

/// Given the already emitted load PrevLI, see if we can find a projection
/// address path to LI. If we can, emit the corresponding aggregate projection
/// insts and return the last such inst.
SILValue
ForwardingAnalysis::
forwardAddrToLdWithExtractPath(SILValue Address, SILValue StoredValue,
                               SILInstruction *Inst, SILValue InstOp) {
  // If we found a projection path, but there are no projections, then the two
  // loads must be the same, return PrevLI.
  if (!Path || Path->empty())
    return StoredValue;

  // Ok, at this point we know that we can construct our aggregate projections
  // from our list of address projections.
  SILValue LastExtract = StoredValue;
  SILBuilderWithScope<16> Builder(Inst);

  // Construct the path!
  for (auto PI = Path->rbegin(), PE = Path->rend(); PI != PE; ++PI) {
    LastExtract = PI->createValueProjection(Builder, Inst->getLoc(),
                                            LastExtract).get();
  }

  // Return the last extract we created.
  return LastExtract;
}

/// Given an address \p Address and a value \p Value stored there that is then
/// loaded or partially loaded by \p LI, forward the value with the appropriate
/// extracts. This is the main entry point to the forwarding feasability code.
static SILValue tryToForwardAddressValueToLoad(SILValue Address,
                                               SILValue StoredValue,
                                               LoadInst *LI) {
  auto CheckResult = ForwardingAnalysis::canForwardAddrToLd(Address, LI);
  if (!CheckResult)
    return SILValue();
  return CheckResult->forwardAddr(Address, StoredValue, LI);
}

//===----------------------------------------------------------------------===//
//                     Actual Load Store Forwarding Impl
//===----------------------------------------------------------------------===//

namespace {

using StoreList = SmallVector<StoreInst *, 8>;

/// The predecessor order in StoreList. We have one StoreInst for each
/// predecessor in StoreList.
using PredOrderInStoreList = SmallVector<SILBasicBlock *, 8>;

/// Map an address to a list of StoreInst, one for each predecessor.
using CoveredStoreMap = llvm::DenseMap<SILValue, StoreList>;

/// State of the load store forwarder in one basic block.
class LSBBForwarder {

  /// The basic block that we are optimizing.
  SILBasicBlock *BB;

  /// The current list of store instructions that stored to memory locations
  /// that were not read/written to since the store was executed.
  llvm::SmallPtrSet<StoreInst *, 8> Stores;

  // This is a list of LoadInst instructions that reference memory locations
  // were not clobbered by instructions that write to memory. In other words
  // the SSA value of the load is known to be the same value as the referenced
  // pointer. The values in the list are potentially updated on each iteration
  // of the loop below.
  llvm::SmallPtrSet<LoadInst *, 8> Loads;

public:
  LSBBForwarder() = default;

  void init(SILBasicBlock *NewBB) {
    BB = NewBB;
  }

  bool optimize(AliasAnalysis *AA, PostDominanceInfo *PDI,
                CoveredStoreMap &StoreMap, PredOrderInStoreList &PredOrder);

  SILBasicBlock *getBB() const { return BB; }

  /// Merge in the states of all predecessors.
  void mergePredecessorStates(llvm::DenseMap<SILBasicBlock *,
                                             unsigned> &BBToBBIDMap,
                              std::vector<LSBBForwarder> &BBIDToForwarderMap,
                              CoveredStoreMap &StoreMap,
                              PredOrderInStoreList &PredOrder);

  /// Clear all state in the BB optimizer.
  void clear() {
    Stores.clear();
    Loads.clear();
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

  /// Remove SI from Stores and update StoreMap as well.
  void stopTrackingStore(StoreInst *SI, CoveredStoreMap &StoreMap) {
    DEBUG(llvm::dbgs() << "        No Longer Store: " << *SI);
    Stores.erase(SI);
    StoreMap.erase(SI->getDest());
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
  void invalidateAliasingLoads(SILInstruction *Inst, AliasAnalysis *AA) {
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

  /// Invalidate our store if Inst writes to the destination location.
  void invalidateWriteToStores(SILInstruction *Inst, AliasAnalysis *AA,
                               CoveredStoreMap &StoreMap) {
    llvm::SmallVector<StoreInst *, 4> InvalidatedStoreList;
    for (auto *SI : Stores)
      if (AA->mayWriteToMemory(Inst, SI->getDest()))
        InvalidatedStoreList.push_back(SI);
    for (auto *SI : InvalidatedStoreList) {
      DEBUG(llvm::dbgs() << "        Found an instruction that writes to memory"
                            " such that a store is invalidated:" << *SI);
      stopTrackingStore(SI, StoreMap);
    }
  }

  /// Invalidate our store if Inst reads from the destination location.
  void invalidateReadFromStores(SILInstruction *Inst, AliasAnalysis *AA,
                                CoveredStoreMap &StoreMap) {
    llvm::SmallVector<StoreInst *, 4> InvalidatedStoreList;
    for (auto *SI : Stores)
      if (AA->mayReadFromMemory(Inst, SI->getDest()))
        InvalidatedStoreList.push_back(SI);
    for (auto *SI : InvalidatedStoreList) {
      DEBUG(llvm::dbgs() << "        Found an instruction that reads from "
                            "memory such that a store is invalidated:"
                         << *SI);
      stopTrackingStore(SI, StoreMap);
    }
  }

  /// Try to prove that SI is a dead store updating all current state. If SI is
  /// dead, eliminate it.
  bool tryToEliminateDeadStores(StoreInst *SI, AliasAnalysis *AA,
                                PostDominanceInfo *PDI,
                                CoveredStoreMap &StoreMap);

  /// Try to find a previously known value that we can forward to LI. This
  /// includes from stores and loads.
  bool tryToForwardLoad(LoadInst *LI, AliasAnalysis *AA,
                        CoveredStoreMap &StoreMap,
                        PredOrderInStoreList &PredOrder);

private:

  /// Merge in the state of an individual predecessor.
  void mergePredecessorState(LSBBForwarder &OtherState);

  /// Update StoreMap for a given basic block, if there is a reaching store
  /// for an address from all the predecessors.
  void updateStoreMap(llvm::DenseMap<SILBasicBlock *,
                                     unsigned> &BBToBBIDMap,
                      std::vector<LSBBForwarder> &BBIDToForwarderMap,
                      CoveredStoreMap &StoreMap,
                      PredOrderInStoreList &PredOrder);

  bool tryToSubstitutePartialAliasLoad(LoadInst *LI, LoadInst *PrevLI);
};

} // end anonymous namespace

bool LSBBForwarder::tryToEliminateDeadStores(StoreInst *SI, AliasAnalysis *AA,
                                             PostDominanceInfo *PDI,
                                             CoveredStoreMap &StoreMap) {
  // If we are storing a value that is available in the load list then we
  // know that no one clobbered that address and the current store is
  // redundant and we can remove it.
  if (LoadInst *LdSrc = dyn_cast<LoadInst>(SI->getSrc())) {
    // Check that the loaded value is live and that the destination address
    // is the same as the loaded address.
    if (Loads.count(LdSrc) && LdSrc->getOperand() == SI->getDest()) {
      deleteInstruction(SI);
      NumDeadStores++;
      return true;
    }
  }

  // Invalidate any load that we can not prove does not read from the stores
  // destination.
  invalidateAliasingLoads(SI, AA);

  // If we are storing to a previously stored address that this store post
  // dominates, delete the old store.
  llvm::SmallVector<StoreInst *, 4> StoresToDelete;
  llvm::SmallVector<StoreInst *, 4> StoresToStopTracking;
  bool Changed = false;
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
    if (!PDI->properlyDominates(SI, PrevStore)) {
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
    stopTrackingStore(I, StoreMap);

  // Insert SI into our store list.
  startTrackingStore(SI);
  return Changed;
}

/// See if there is an extract path from LI that we can replace with PrevLI. If
/// we delete all uses of LI this way, delete LI.
bool LSBBForwarder::tryToSubstitutePartialAliasLoad(LoadInst *LI,
                                                    LoadInst *PrevLI) {
  bool Changed = false;

  // Since LI and PrevLI partially alias and we know that PrevLI is smaller than
  // LI due to where we are in the computation, we compute the address
  // projection path from PrevLI's operand to LI's operand.
  SILValue PrevLIOp = PrevLI->getOperand();
  SILValue UnderlyingPrevLIOp = getUnderlyingObject(PrevLIOp);
  auto PrevLIPath = ProjectionPath::getAddrProjectionPath(UnderlyingPrevLIOp,
                                                          PrevLIOp);
  if (!PrevLIPath)
    return false;

  SILValue LIOp = LI->getOperand();
  SILValue UnderlyingLIOp = getUnderlyingObject(LIOp);
  auto LIPath = ProjectionPath::getAddrProjectionPath(UnderlyingLIOp, LIOp);
  if (!LIPath)
    return false;

  // If LIPath matches a prefix of PrevLIPath, return the projection path with
  // the prefix removed.
  auto P = ProjectionPath::subtractPaths(*PrevLIPath, *LIPath);
  if (!P)
    return false;

  // For all uses of LI, if we can traverse the entire projection path P for
  // PrevLI, matching each projection to an extract, replace the final extract
  // with the PrevLI.

  llvm::SmallVector<SILInstruction *, 8> Tails;
  for (auto *Op : LI->getUses()) {
    if (P->findMatchingValueProjectionPaths(Op->getUser(), Tails)) {
      for (auto *FinalExt : Tails) {
        FinalExt->replaceAllUsesWith(PrevLI);
        NumForwardedLoads++;
        Changed = true;
      }
    }
    Tails.clear();
  }

  return Changed;
}

/// Add a BBArgument in Dest to combine sources of Stores.
static SILValue fixPhiPredBlocks(SmallVectorImpl<StoreInst *> &Stores,
                                 SmallVectorImpl<SILBasicBlock *> &PredOrder,
                                 SILBasicBlock *Dest) {
  SILModule &M = Dest->getModule();
  assert(Stores.size() ==
         (unsigned)std::distance(Dest->pred_begin(), Dest->pred_end()) &&
         "Multiple store forwarding size mismatch");
  auto PhiValue = new (M) SILArgument(Stores[0]->getSrc().getType(), Dest);
  unsigned Id = 0;
  for (auto Pred : PredOrder) {
    TermInst *TI = Pred->getTerminator();
    // This can change the order of predecessors in getPreds.
    addArgumentToBranch(Stores[Id++]->getSrc(), Dest, TI);
    TI->eraseFromParent();
  }
  return PhiValue;
}

bool LSBBForwarder::tryToForwardLoad(LoadInst *LI, AliasAnalysis *AA,
                                     CoveredStoreMap &StoreMap,
                                     PredOrderInStoreList &PredOrder) {
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
    NumForwardedLoads++;
    return true;
  }

  // Search the previous loads and replace the current load or one of the
  // current loads uses with one of the previous loads.
  for (auto *PrevLI : Loads) {
    SILValue Address = PrevLI->getOperand();
    SILValue StoredValue = PrevLI;

    // First Check if LI can be completely replaced by PrevLI or if we can
    // construct an extract path from PrevLI's loaded value. The latter occurs
    // if PrevLI is a partially aliasing load that completely subsumes LI.
    if (SILValue Result = tryToForwardAddressValueToLoad(Address, StoredValue,
                                                         LI)) {

      DEBUG(llvm::dbgs() << "        Replacing with previous load: "
            << *Result);
      SILValue(LI, 0).replaceAllUsesWith(Result);
      deleteInstruction(LI);
      NumDupLoads++;
      return true;
    }

    // Otherwise check if LI's operand partially aliases PrevLI's operand. If
    // so, see if LI has any uses which could use PrevLI instead of LI
    // itself. If LI has no uses after this is completed, delete it and return
    // true.
    //
    // We return true at the end of this if we succeeded to find any uses of LI
    // that could be replaced with PrevLI, this means that there could not have
    // been a store to LI in between LI and PrevLI since then the store would
    // have invalidated PrevLI.
    if (AA->isPartialAlias(LI->getOperand(), PrevLI->getOperand())) {
      tryToSubstitutePartialAliasLoad(LI, PrevLI);
    }
  }

  // Check if we can forward from multiple stores.
  for (auto I = StoreMap.begin(), E = StoreMap.end(); I != E; I++) {
    auto CheckResult = ForwardingAnalysis::canForwardAddrToLd(I->first, LI);
    if (!CheckResult)
      continue;

    DEBUG(llvm::dbgs() << "        Checking from: ");
    for (auto *SI : I->second) {
      DEBUG(llvm::dbgs() << "          " << *SI);
      (void) SI;
    }

    // Create a BBargument to merge in multiple stores.
    SILValue PhiValue = fixPhiPredBlocks(I->second, PredOrder, BB);
    SILValue Result = CheckResult->forwardAddr(I->first,
                                               PhiValue,
                                               LI);
    assert(Result && "Forwarding from multiple stores failed!");

    DEBUG(llvm::dbgs() << "        Forwarding from multiple stores: ");
    SILValue(LI, 0).replaceAllUsesWith(Result);
    deleteInstruction(LI);
    NumForwardedLoads++;
    return true;
  }

  startTrackingLoad(LI);
  // No partial aliased loads were successfully forwarded. Return false to
  // indicate no change.
  return false;
}

/// \brief Promote stored values to loads, remove dead stores and merge
/// duplicated loads.
bool LSBBForwarder::optimize(AliasAnalysis *AA, PostDominanceInfo *PDI,
                             CoveredStoreMap &StoreMap,
                             PredOrderInStoreList &PredOrder) {
  auto II = BB->begin(), E = BB->end();
  bool Changed = false;
  while (II != E) {
    SILInstruction *Inst = II++;
    DEBUG(llvm::dbgs() << "    Visiting: " << *Inst);

    // This is a StoreInst. Let's see if we can remove the previous stores.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      Changed |= tryToEliminateDeadStores(SI, AA, PDI, StoreMap);
      continue;
    }

    // This is a LoadInst. Let's see if we can find a previous loaded, stored
    // value to use instead of this load.
    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      Changed |= tryToForwardLoad(LI, AA, StoreMap, PredOrder);
      continue;
    }

    // If this instruction has side effects, but is inert from a load store
    // perspective, skip it.
    if (isLSForwardingInertInstruction(Inst)) {
      DEBUG(llvm::dbgs() << "        Found inert instruction: " << *Inst);
      continue;
    }

    if (auto *BI = dyn_cast<BuiltinInst>(Inst))
      if (isReadNone(BI)) {
        DEBUG(llvm::dbgs() << "        Found readnone builtin, does not "
              "affect loads and stores.\n");
        continue;
      }

    // All other instructions that read from the memory location of the store
    // invalidates the store.
    if (Inst->mayReadFromMemory()) {
      invalidateReadFromStores(Inst, AA, StoreMap);
    }

    // If we have an instruction that may write to memory and we can not prove
    // that it and its operands can not alias a load we have visited, invalidate
    // that load.
    if (Inst->mayWriteToMemory()) {
      // Invalidate any load that we can not prove does not read from one of the
      // writing instructions operands.
      invalidateAliasingLoads(Inst, AA);

      // Invalidate our store if Inst writes to the destination location.
      invalidateWriteToStores(Inst, AA, StoreMap);
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

void LSBBForwarder::mergePredecessorState(LSBBForwarder &OtherState) {
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
    CoveredStoreMap DummyMap;
    for (auto *SI : DeleteList) {
      stopTrackingStore(cast<StoreInst>(SI), DummyMap);
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

/// Update StoreMap for a given basic block, if there is a reaching store
/// for an address from all the predecessors.
void LSBBForwarder::
updateStoreMap(llvm::DenseMap<SILBasicBlock *,
                              unsigned> &BBToBBIDMap,
               std::vector<LSBBForwarder> &BBIDToForwarderMap,
               CoveredStoreMap &StoreMap, PredOrderInStoreList &PredOrder) {
  if (std::distance(BB->pred_begin(), BB->pred_end()) <= 1)
    return;

  bool FirstPred = true;
  for (auto Pred : BB->getPreds()) {
    // Bail out if one of the predecessors has a terminator that we currently
    // do not handle.
    if (!isa<CondBranchInst>(Pred->getTerminator()) &&
        !isa<BranchInst>(Pred->getTerminator())) {
      StoreMap.clear();
      return;
    }

    PredOrder.push_back(Pred);
    auto I = BBToBBIDMap.find(Pred);
    if (I == BBToBBIDMap.end()) {
      StoreMap.clear();
      return;
    }
    auto BBId = I->second;
    (void) BBId;
    LSBBForwarder &Other = BBIDToForwarderMap[I->second];

    // Calculate SILValues that are stored once in this predecessor.
    llvm::DenseMap<SILValue, StoreInst *> StoredMapOfThisPred;
    llvm::SmallSet<SILValue, 16> StoredValuesOfThisPred;
    for (auto *SI : Other.Stores) {
      if (!StoredValuesOfThisPred.count(SI->getDest()))
        StoredMapOfThisPred[SI->getDest()] = SI;
      else
        StoredMapOfThisPred.erase(SI->getDest());
      StoredValuesOfThisPred.insert(SI->getDest());
    }

    if (FirstPred) {
      // Update StoreMap with stores from the first predecessor.
      for (auto I = StoredMapOfThisPred.begin(), E = StoredMapOfThisPred.end();
           I != E; I++) {
        StoreMap[I->first].push_back(I->second);
        DEBUG(llvm::dbgs() << "        Updating StoreMap BB#" << BBId << ": "
                           << I->first << "          " << *I->second);
      }
    } else {
      // Merge in stores from other predecessors.
      for (auto I = StoreMap.begin(), E = StoreMap.end(); I != E;) {
        SILValue Current = I->first;
        if (!StoredMapOfThisPred.count(Current) ||
            I->second[0]->getSrc().getType() !=
            StoredMapOfThisPred[Current]->getSrc().getType()) {
          DEBUG(llvm::dbgs() << "        Removing StoreMap: " << Current);
          I++; // Move to the next before erasing the current.
          StoreMap.erase(Current);
        }
        else {
          I->second.push_back(StoredMapOfThisPred[Current]);
          DEBUG(llvm::dbgs() << "        Updating StoreMap BB#" << BBId << ": "
                             << Current
                             << "          " << *StoredMapOfThisPred[Current]);
          I++;
        }
      }
    }
    FirstPred = false;
  }
}

void
LSBBForwarder::
mergePredecessorStates(llvm::DenseMap<SILBasicBlock *,
                                      unsigned> &BBToBBIDMap,
                       std::vector<LSBBForwarder> &BBIDToForwarderMap,
                       CoveredStoreMap &StoreMap,
                       PredOrderInStoreList &PredOrder) {
  // Clear the state if the basic block has no predecessor.
  if (BB->getPreds().empty()) {
    clear();
    return;
  }

  // Update StoreMap. Do this before updating Stores since we need the states
  // at the end of the basic blocks.
  updateStoreMap(BBToBBIDMap, BBIDToForwarderMap, StoreMap, PredOrder);

  bool HasAtLeastOnePred = false;
  // If we have a self cycle, we keep the old state and merge in states
  // of other predecessors. Otherwise, we initialize the state with the first
  // predecessor's state and merge in states of other predecessors.
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

  for (auto *SI : Stores) {
    DEBUG(llvm::dbgs() << "        Removing StoreMap: " << SI->getDest());
    StoreMap.erase(SI->getDest());
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
      BBIDToForwarderMap[count].init(BB);
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

        CoveredStoreMap StoreMap;
        PredOrderInStoreList PredOrder;
        // Merge the predecessors. After merging, LSBBForwarder now contains
        // lists of stores|loads that reach the beginning of the basic block
        // along all paths.
        Forwarder.mergePredecessorStates(BBToBBIDMap, BBIDToForwarderMap,
                                         StoreMap, PredOrder);

        // Remove dead stores, merge duplicate loads, and forward stores to
        // loads. We also update lists of stores|loads to reflect the end
        // of the basic block.
        ChangedDuringIteration |= Forwarder.optimize(AA, PDI, StoreMap,
                                                     PredOrder);
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
