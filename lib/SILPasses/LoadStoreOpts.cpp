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
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"

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

static SILValue getForwardingValueForLS(const SILInstruction *I) {
  if (auto *SI = dyn_cast<StoreInst>(I))
    return SI->getSrc();
  return cast<LoadInst>(I);
}

static SILValue getAddressForLS(const SILInstruction *I) {
  if (auto *SI = dyn_cast<StoreInst>(I))
    return SI->getDest();
  return cast<LoadInst>(I)->getOperand();
}

static SILType getForwardingTypeForLS(const SILInstruction *I) {
  return getForwardingValueForLS(I).getType();
}

//===----------------------------------------------------------------------===//
//                      Forwarding Feasability Analysis
//===----------------------------------------------------------------------===//

namespace {

enum class ForwardingKind {
  Normal,
  UncheckedAddress
};

/// This is a move-only structure. Thus it has a private default constructor and
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

  // The structs could have different size. We have code in the stdlib that
  // casts pointers to differently sized integer types. This code prevents
  // that we bitcast the values.
  if (OutputTy.getStructOrBoundGenericStruct() &&
      InputTy.getStructOrBoundGenericStruct())
    return llvm::NoneType::None;

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
    CastValue = B.createUncheckedTrivialBitCast(UADCI->getLoc(), StoredValue,
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
//                            LSContext Interface
//===----------------------------------------------------------------------===//

namespace {

class LSBBForwarder;

using StoreList = SmallVector<StoreInst *, 8>;

/// The predecessor order in StoreList. We have one StoreInst for each
/// predecessor in StoreList.
using PredOrderInStoreList = SmallVector<SILBasicBlock *, 8>;

/// Map an address to a list of StoreInst, one for each predecessor.
using CoveredStoreMap = llvm::DenseMap<SILValue, StoreList>;

/// This class stores global state that we use when processing and also drives
/// the computation. We put its interface at the top for use in other parts of
/// the pass which may want to use this global information.
class LSContext {
  /// The alias analysis that we will use during all computations.
  AliasAnalysis *AA;

  /// The post dominance analysis that we use for dead store elimination.
  PostDominanceInfo *PDI;

  /// The range that we use to iterate over the reverse post order of the given
  /// function.
  PostOrderAnalysis::reverse_range ReversePostOrder;

  /// A map from each BasicBlock to its index in the BBIDToForwarderMap.
  ///
  /// TODO: Each block does not need its own LSBBForwarder instance. Only
  /// the set of reaching loads and stores is specific to the block.
  llvm::DenseMap<SILBasicBlock *, unsigned> BBToBBIDMap;

  /// A "map" from a BBID (which is just an index) to an LSBBForwarder.
  std::vector<LSBBForwarder> BBIDToForwarderMap;

public:
  LSContext(AliasAnalysis *AA, PostDominanceInfo *PDI,
            PostOrderAnalysis::reverse_range RPOT);

  LSContext(const LSContext &) = delete;
  LSContext(LSContext &&) = default;
  ~LSContext() = default;

  bool runIteration();

  /// Remove all LSValues from all LSBBForwarders which contain the load/store
  /// instruction \p I.
  void stopTrackingInst(SILInstruction *I, CoveredStoreMap &StoreMap);

  AliasAnalysis *getAA() const { return AA; }
  PostDominanceInfo *getPDI() const { return PDI; }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                                  LSValue
//===----------------------------------------------------------------------===//

namespace {

/// This class represents either a single value or a covering of values that we
/// can load forward from via the introdution of a SILArgument. This enables us
/// to treat the case of having one value or multiple values and load and store
/// cases all at once abstractly and cleanly.
class LSValue {
  /// The "parent" basic block which this LSValue originated in.
  ///
  /// In the case where we are tracking one value this is the BB in which the
  /// actual value originated. In the case in which we are tracking a covering
  /// set of loads, this is the BB where if we forward this load value, we will
  /// need to insert a SILArgument.
  SILBasicBlock *ParentBB;

  /// The individual inst or covering inst set that this LSValue represents.
  llvm::TinyPtrVector<SILInstruction *> Insts;

  /// The lazily computed value that can be used to forward this LSValue.
  ///
  /// In the case where we have a single value this is always initialized. In
  /// the case where we are handling a covering set, this is initially null and
  /// when we insert the PHI node, this is set to the SILArgument which
  /// represents the PHI node.
  ///
  /// In the case where we are dealing with loads this is the loaded value or a
  /// phi derived from a covering set of loaded values. In the case where we are
  /// dealing with stores, this is the value that is stored or a phi of such
  /// values.
  SILValue ForwardingValue;

public:
  LSValue(SILInstruction *NewInst)
      : ParentBB(NewInst->getParent()), Insts(NewInst),
        ForwardingValue(getForwardingValueForLS(NewInst)) {}

  LSValue(SILBasicBlock *NewParentBB, ArrayRef<SILInstruction *> NewInsts);
  LSValue(SILBasicBlock *NewParentBB, ArrayRef<LoadInst *> NewInsts);
  LSValue(SILBasicBlock *NewParentBB, ArrayRef<StoreInst *> NewInsts);

  bool operator==(const LSValue &Other) const;

  /// Return the SILValue necessary for forwarding the given LSValue.
  ///
  /// *NOTE* This will create a PHI node if we have not created one yet if we
  /// have a covering set.
  SILValue getForwardingValue();

  /// Returns true if this LSValue aliases the given instruction.
  bool aliasingWrite(AliasAnalysis *AA, SILInstruction *Inst) const {
    // If we have a single inst, just get the forwarding value and compare if
    // they alias.
    if (isSingleInst())
      return AA->mayWriteToMemory(Inst, getAddressForLS(getInst()));

    // Otherwise, loop over all of our forwaring insts and return true if any of
    // them alias Inst.
    for (auto &I : getInsts())
      if (AA->mayWriteToMemory(Inst, getAddressForLS(I)))
        return true;
    return false;
  }

  bool aliasingRead(AliasAnalysis *AA, SILInstruction *Inst) const {
    // If we have a single inst, just get the forwarding value and compare if
    // they alias.
    if (isSingleInst())
      return AA->mayReadFromMemory(Inst, getAddressForLS(getInst()));

    // Otherwise, loop over all of our forwaring insts and return true if any of
    // them alias Inst.
    for (auto &I : getInsts())
      if (AA->mayReadFromMemory(Inst, getAddressForLS(I)))
        return true;
    return false;
  }

  /// Returns the set of insts represented by this LSValue.
  ArrayRef<SILInstruction *> getInsts() const { return Insts; }

  /// Returns true if the value contains the instruction \p Inst.
  bool containsInst(SILInstruction *Inst) const {
    for (SILInstruction *I : Insts) {
      if (I == Inst)
        return true;
    }
    return false;
  }

#ifndef NDEBUG
  friend raw_ostream &operator<<(raw_ostream &os, const LSValue &Val) {
    os << "value in bb" << Val.ParentBB->getDebugID() << ": " <<
      Val.ForwardingValue;
    for (SILInstruction *I : Val.Insts) {
      os << "             " << *I;
    }
    return os;
  }
#endif

protected:
  /// Returns true if this LSValue represents a singular inst instruction.
  bool isSingleInst() const { return Insts.size() == 1; }

  /// Returns true if this LSValue represents a covering set of insts.
  bool isCoveringInst() const { return Insts.size() > 1; }

  /// Returns a singular inst if we are tracking a singular inst. Asserts
  /// otherwise.
  SILInstruction *getInst() const {
    assert(isSingleInst() && "Can only getLoad() if this is a singular load");
    return Insts[0];
  }
};

} // end anonymous namespace

LSValue::LSValue(SILBasicBlock *NewParentBB,
                 ArrayRef<SILInstruction *> NewInsts)
    : ParentBB(NewParentBB), Insts(), ForwardingValue() {
  std::copy(NewInsts.begin(), NewInsts.end(), Insts.begin());
  // Sort Insts so we can trivially compare two LSValues.
  std::sort(Insts.begin(), Insts.end());
}

LSValue::LSValue(SILBasicBlock *NewParentBB,
                 ArrayRef<LoadInst *> NewInsts)
    : ParentBB(NewParentBB), Insts(), ForwardingValue() {
  std::copy(NewInsts.begin(), NewInsts.end(), Insts.begin());
  // Sort Insts so we can trivially compare two LSValues.
  std::sort(Insts.begin(), Insts.end());
}

LSValue::LSValue(SILBasicBlock *NewParentBB,
                 ArrayRef<StoreInst *> NewInsts)
    : ParentBB(NewParentBB), Insts(), ForwardingValue() {
  std::copy(NewInsts.begin(), NewInsts.end(), Insts.begin());
  // Sort Insts so we can trivially compare two LSValues.
  std::sort(Insts.begin(), Insts.end());
}

/// Return the SILValue necessary for forwarding the given LSValue. *NOTE*
/// This will create a PHI node if we have not created one yet if we have a
/// covering set.
SILValue LSValue::getForwardingValue() {
  // If we already have a forwarding value, just return it.
  if (ForwardingValue)
    return ForwardingValue;

  // Otherwise, we must have a covering set of loads. Create the PHI and set
  // forwarding value to it.
  assert(isCoveringInst() &&
         "Must have a covering inst at this point since "
         "if we have a singular inst ForwardingValue is set in the "
         "constructor.");

  // We only support adding arguments to cond_br and br. If any predecessor
  // does not have such a terminator, return an empty SILValue().
  //
  // *NOTE* There is an assertion in addNewEdgeValueToBranch that will throw
  // if we do not do this early.
  // *NOTE* This is a strong argument in favor of representing PHI nodes
  // separately from SILArguments.
  if (std::any_of(ParentBB->pred_begin(), ParentBB->pred_end(),
                  [](SILBasicBlock *Pred) -> bool {
        TermInst *TI = Pred->getTerminator();
        return !isa<CondBranchInst>(TI) || !isa<BranchInst>(TI);
      }))
    return SILValue();

  // Create the new SILArgument and set ForwardingValue to it.
  ForwardingValue = ParentBB->createBBArg(getForwardingTypeForLS(Insts[0]));

  // Update all edges. We do not create new edges in between BBs so this
  // information should always be correct.
  for (SILInstruction *I : getInsts())
    addNewEdgeValueToBranch(I->getParent()->getTerminator(), ParentBB,
                            getForwardingValueForLS(I));

  /// Return our new forwarding value.
  return ForwardingValue;
}

/// We use the fact that LSValues always have items sorted by pointer address to
/// compare the two instruction lists.
bool LSValue::operator==(const LSValue &Other) const {
  if (Insts.size() != Other.Insts.size())
    return false;

  for (unsigned i : indices(Insts))
    if (Insts[i] != Other.Insts[i])
      return false;

  return true;
}

//===----------------------------------------------------------------------===//
//                                   LSLoad
//===----------------------------------------------------------------------===//

namespace {

/// This class represents either a single value that we can load forward or a
/// covering of values that we could load forward from via the introdution of a
/// SILArgument. This enables us to treat both cases the same during our
/// transformations in an abstract way.
class LSLoad : public LSValue {
public:
  /// TODO: Add constructor to TinyPtrVector that takes in an individual
  LSLoad(LoadInst *NewLoad) : LSValue(NewLoad) {}

  /// TODO: Add constructor to TinyPtrVector that takes in an ArrayRef.
  LSLoad(SILBasicBlock *NewParentBB, ArrayRef<LoadInst *> NewLoads)
      : LSValue(NewParentBB, NewLoads) {}
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                                  LSStore
//===----------------------------------------------------------------------===//

namespace {

/// This structure represents either a single value or a covering of values that
/// we could use in we can dead store elimination or store forward via the
/// introdution of a SILArgument. This enables us to treat both cases the same
/// during our transformations in an abstract way.
class LSStore : public LSValue {
public:
  LSStore(StoreInst *NewStore) : LSValue(NewStore) {}

  LSStore(SILBasicBlock *NewParentBB, ArrayRef<StoreInst *> NewStores)
      : LSValue(NewParentBB, NewStores) {}

  /// Delete the store or set of stores that this LSStore represents.
  void deleteDeadValue() {
    for (SILInstruction *I : getInsts()) {
      I->eraseFromParent();
    }
  }

  /// Returns true if I post dominates all of the stores that we are tracking.
  bool postdominates(PostDominanceInfo *PDI, SILInstruction *I) {
    for (SILInstruction *Stores : getInsts()) {
      if (!PDI->properlyDominates(I, Stores)) {
        return false;
      }
    }
    return true;
  }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                               LSBBForwarder
//===----------------------------------------------------------------------===//

namespace {

/// State of the load store forwarder in one basic block.
///
/// An invariant of this pass is that a SILValue can only be in one of Stores
/// and Loads. This is enforced by assertions in startTrackingLoad() and
/// startTrackingStore().
class LSBBForwarder {

  /// The basic block that we are optimizing.
  SILBasicBlock *BB;

  /// The current list of store instructions that stored to memory locations
  /// that were not read/written to since the store was executed.
  llvm::SmallMapVector<SILValue, LSStore, 8> Stores;

  // This is a list of LoadInst instructions that reference memory locations
  // were not clobbered by instructions that write to memory. In other words
  // the SSA value of the load is known to be the same value as the referenced
  // pointer. The values in the list are potentially updated on each iteration
  // of the loop below.
  llvm::SmallMapVector<SILValue, LSLoad, 8> Loads;

public:
  LSBBForwarder() = default;

  void init(SILBasicBlock *NewBB) {
    BB = NewBB;
  }

  bool optimize(LSContext &Ctx, CoveredStoreMap &StoreMap,
                PredOrderInStoreList &PredOrder);

  SILBasicBlock *getBB() const { return BB; }

  /// Removes an LSStore or LSLoad if it contains instruction \p I.
  /// Returns true if \p I was found and an LSStore/LSLoad was removed.
  bool removeIfContainsInst(SILInstruction *I) {
    if (auto *SI = dyn_cast<StoreInst>(I)) {
      auto StoreIter = Stores.find(SI->getDest());
      if (StoreIter != Stores.end() && StoreIter->second.containsInst(I)) {
        Stores.erase(StoreIter);
        return true;
      }
      return false;
    }
    auto LoadIter = Loads.find(cast<LoadInst>(I)->getOperand());
    if (LoadIter != Loads.end() && LoadIter->second.containsInst(I)) {
      Loads.erase(LoadIter);
      return true;
    }
    return false;
  }

  void eraseValue(SILValue Addr) {
    Stores.erase(Addr);
    Loads.erase(Addr);
  }

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
    assert(Stores.find(LI->getOperand()) == Stores.end() &&
           "Found new address asked to track in store list already!");
    Loads.insert({LI->getOperand(), LSLoad(LI)});
  }

  /// Add this store to our tracking list.
  void startTrackingStore(StoreInst *SI) {
    DEBUG(llvm::dbgs() << "        Tracking Store: " << *SI);
    assert(Loads.find(SI->getDest()) == Loads.end() &&
           "Found new address asked to track in load list already!\n");
    Stores.insert({SI->getDest(), LSStore(SI)});
  }

  /// Stop tracking any state related to the address \p Addr.
  void stopTrackingAddress(SILValue Addr, CoveredStoreMap &StoreMap) {
    DEBUG(llvm::dbgs() << "        No Longer Tracking: " << Addr);
    Loads.erase(Addr);
    Stores.erase(Addr);
    StoreMap.erase(Addr);
  }

  /// Delete the store that we have mapped to Addr, plus other instructions
  /// which get dead due to the removed store.
  void deleteStoreMappedToAddress(SILValue Addr, CoveredStoreMap &StoreMap,
                                  LSContext &Ctx);

  void deleteUntrackedInstruction(SILInstruction *I, CoveredStoreMap &StoreMap,
                                  LSContext &Ctx);

  /// Invalidate any loads that we can not prove that Inst does not write to.
  void invalidateAliasingLoads(LSContext &Ctx, SILInstruction *Inst,
                               CoveredStoreMap &StoreMap);

  /// Invalidate our store if Inst writes to the destination location.
  void invalidateWriteToStores(LSContext &Ctx, SILInstruction *Inst,
                               CoveredStoreMap &StoreMap);

  /// Invalidate our store if Inst reads from the destination location.
  void invalidateReadFromStores(LSContext &Ctx, SILInstruction *Inst,
                                CoveredStoreMap &StoreMap);

  /// Try to prove that SI is a dead store updating all current state. If SI is
  /// dead, eliminate it.
  bool tryToEliminateDeadStores(LSContext &Ctx, StoreInst *SI,
                                CoveredStoreMap &StoreMap);

  /// Try to find a previously known value that we can forward to LI. This
  /// includes from stores and loads.
  bool tryToForwardLoad(LSContext &Ctx, LoadInst *LI,
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

  bool tryToSubstitutePartialAliasLoad(SILValue PrevAddr, SILValue PrevValue,
                                       LoadInst *LI);
};

#ifndef NDEBUG
inline raw_ostream &operator<<(raw_ostream &os,
                               const std::pair<SILValue, LSLoad> &Value) {
  os << "load " << Value.first << "              -> " << Value.second;
  return os;
}

inline raw_ostream &operator<<(raw_ostream &os,
                               const std::pair<SILValue, LSStore> &Value) {
  os << "store " << Value.first << "              -> " << Value.second;
  return os;
}
#endif

} // end anonymous namespace

void LSBBForwarder::deleteStoreMappedToAddress(SILValue Addr,
                                               CoveredStoreMap &StoreMap,
                                               LSContext &Ctx) {
  auto SIIter = Stores.find(Addr);
  if (SIIter == Stores.end())
    return;
  assert((Loads.find(Addr) == Loads.end()) &&
         "An address can never be in both the stores and load lists.");

  llvm::SmallVector<SILInstruction *, 8> InstsToDelete;

  for (auto *SI : SIIter->second.getInsts())
    InstsToDelete.push_back(SI);

  auto UpdateFun = [&](SILInstruction *DeadI) {
    Ctx.stopTrackingInst(DeadI, StoreMap);
  };

  // Delete the instructions.
  for (auto *I : InstsToDelete)
    recursivelyDeleteTriviallyDeadInstructions(I, true, UpdateFun);

  assert(Stores.find(Addr) == Stores.end() &&
         "Addr should be removed during deleting the store instruction");
}

void
LSBBForwarder::
deleteUntrackedInstruction(SILInstruction *I,
                           CoveredStoreMap &StoreMap,
                           LSContext &Ctx) {
  DEBUG(llvm::dbgs() << "        Deleting all instructions recursively from: "
                     << *I);
  auto UpdateFun = [&](SILInstruction *DeadI) {
    Ctx.stopTrackingInst(DeadI, StoreMap);
  };
  recursivelyDeleteTriviallyDeadInstructions(I, true, UpdateFun);
}

void
LSBBForwarder::
invalidateAliasingLoads(LSContext &Ctx, SILInstruction *Inst,
                        CoveredStoreMap &StoreMap) {
  AliasAnalysis *AA = Ctx.getAA();
  llvm::SmallVector<SILValue, 4> InvalidatedLoadList;
  for (auto &P : Loads)
    if (P.second.aliasingWrite(AA, Inst))
      InvalidatedLoadList.push_back(P.first);

  for (SILValue LIOp : InvalidatedLoadList) {
    DEBUG(llvm::dbgs() << "        Found an instruction that writes to memory"
          " such that a load operand is invalidated:"
          << LIOp);
    stopTrackingAddress(LIOp, StoreMap);
  }
}

static bool writeAliasesStoreInList(AliasAnalysis *AA,
                                    SILInstruction *Writer,
                                    ArrayRef<StoreInst *> Stores) {
  for (auto *S : Stores)
    if (LSValue(S).aliasingWrite(AA, Writer))
      return true;

  return false;
}

void
LSBBForwarder::
invalidateWriteToStores(LSContext &Ctx, SILInstruction *Inst,
                        CoveredStoreMap &StoreMap) {
  AliasAnalysis *AA = Ctx.getAA();
  llvm::SmallVector<SILValue, 4> InvalidatedStoreList;
  for (auto &P : Stores)
    if (P.second.aliasingWrite(AA, Inst))
      InvalidatedStoreList.push_back(P.first);

  for (auto &P : StoreMap)
    if (writeAliasesStoreInList(AA, Inst, P.second))
      InvalidatedStoreList.push_back(P.first);

  for (SILValue SIOp : InvalidatedStoreList) {
    DEBUG(llvm::dbgs() << "        Found an instruction that writes to memory"
          " such that a store is invalidated:" << SIOp);
    stopTrackingAddress(SIOp, StoreMap);
  }
}

void LSBBForwarder::invalidateReadFromStores(LSContext &Ctx,
                                             SILInstruction *Inst,
                                             CoveredStoreMap &StoreMap) {
  AliasAnalysis *AA = Ctx.getAA();
  llvm::SmallVector<SILValue, 4> InvalidatedStoreList;
  for (auto &P : Stores)
    if (P.second.aliasingRead(AA, Inst) && !isLetPointer(P.first))
      InvalidatedStoreList.push_back(P.first);

  for (SILValue SIOp : InvalidatedStoreList) {
    DEBUG(llvm::dbgs() << "        Found an instruction that reads from "
          "memory such that a store is invalidated:" << SIOp);
    stopTrackingAddress(SIOp, StoreMap);
  }
}

bool LSBBForwarder::tryToEliminateDeadStores(LSContext &Ctx, StoreInst *SI,
                                             CoveredStoreMap &StoreMap) {
  PostDominanceInfo *PDI = Ctx.getPDI();
  AliasAnalysis *AA = Ctx.getAA();

  // If we are storing a value that is available in the load list then we
  // know that no one clobbered that address and the current store is
  // redundant and we can remove it.
  if (auto *LdSrc = dyn_cast<LoadInst>(SI->getSrc())) {
    // Check that the loaded value is live and that the destination address
    // is the same as the loaded address.
    SILValue LdSrcOp = LdSrc->getOperand();
    auto Iter = Loads.find(LdSrcOp);
    if (Iter != Loads.end() && LdSrcOp == SI->getDest()) {
      deleteUntrackedInstruction(SI, StoreMap, Ctx);
      NumDeadStores++;
      return true;
    }
  }

  // Invalidate any load that we can not prove does not read from the stores
  // destination.
  invalidateAliasingLoads(Ctx, SI, StoreMap);

  // If we are storing to a previously stored address that this store post
  // dominates, delete the old store.
  llvm::SmallVector<SILValue, 4> StoresToDelete;
  llvm::SmallVector<SILValue, 4> StoresToStopTracking;
  bool Changed = false;
  for (auto &P : Stores) {
    if (!P.second.aliasingWrite(AA, SI))
      continue;

    // We know that the locations might alias. Check whether it is a must alias.
    bool IsStoreToSameLocation = AA->isMustAlias(SI->getDest(),  P.first);

    // If this store may alias but is not known to be to the same location, we
    // cannot eliminate it. We need to remove it from the store list and start
    // tracking the new store, though.
    if (!IsStoreToSameLocation) {
      StoresToStopTracking.push_back(P.first);
      DEBUG(llvm::dbgs() << "        Found an aliasing store... But we don't "
            "know that it must alias... Can't remove it but will track it.");
      continue;
    }

    // If this store does not post dominate prev store, we can not eliminate
    // it. But do remove prev store from the store list and start tracking the
    // new store.
    //
    // We are only given this if we are being used for multi-bb load store opts
    // (when this is required). If we are being used for single-bb load store
    // opts, this is not necessary, so skip it.
    if (!P.second.postdominates(PDI, SI)) {
      StoresToStopTracking.push_back(P.first);
      DEBUG(llvm::dbgs() << "        Found dead store... That we don't "
            "postdominate... Can't remove it but will track it.");
      continue;
    }

    DEBUG(llvm::dbgs() << "        Found a dead previous store... Removing...:"
                       << P);
    Changed = true;
    StoresToDelete.push_back(P.first);
    NumDeadStores++;
  }

  for (SILValue SIOp : StoresToDelete)
    deleteStoreMappedToAddress(SIOp, StoreMap, Ctx);
  for (SILValue SIOp : StoresToStopTracking)
    stopTrackingAddress(SIOp, StoreMap);

  // Insert SI into our store list to start tracking.
  startTrackingStore(SI);
  return Changed;
}

/// See if there is an extract path from LI that we can replace with PrevLI. If
/// we delete all uses of LI this way, delete LI.
bool LSBBForwarder::tryToSubstitutePartialAliasLoad(SILValue PrevLIAddr,
                                                    SILValue PrevLIValue,
                                                    LoadInst *LI) {
  bool Changed = false;

  // Since LI and PrevLI partially alias and we know that PrevLI is smaller than
  // LI due to where we are in the computation, we compute the address
  // projection path from PrevLI's operand to LI's operand.
  SILValue UnderlyingPrevLIAddr = getUnderlyingObject(PrevLIAddr);
  auto PrevLIPath =
      ProjectionPath::getAddrProjectionPath(UnderlyingPrevLIAddr, PrevLIAddr);
  if (!PrevLIPath)
    return false;

  SILValue LIAddr = LI->getOperand();
  SILValue UnderlyingLIAddr = getUnderlyingObject(LIAddr);
  auto LIPath = ProjectionPath::getAddrProjectionPath(UnderlyingLIAddr, LIAddr);
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
        assert(FinalExt->getNumTypes() == 1 && "Expecting only unary types");
        SILValue(FinalExt).replaceAllUsesWith(PrevLIValue);
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
  auto PhiValue = new (M) SILArgument(Dest, Stores[0]->getSrc().getType());
  unsigned Id = 0;
  for (auto Pred : PredOrder) {
    TermInst *TI = Pred->getTerminator();
    // This can change the order of predecessors in getPreds.
    addArgumentToBranch(Stores[Id++]->getSrc(), Dest, TI);
    TI->eraseFromParent();
  }
  return PhiValue;
}

bool LSBBForwarder::tryToForwardLoad(LSContext &Ctx, LoadInst *LI,
                                     CoveredStoreMap &StoreMap,
                                     PredOrderInStoreList &PredOrder) {
  // If we are loading a value that we just stored, forward the stored value.
  for (auto &P : Stores) {
    SILValue Addr = P.first;

    auto CheckResult = ForwardingAnalysis::canForwardAddrToLd(Addr, LI);
    if (!CheckResult) {
      if (Addr == LI->getOperand()) {
        // Although the addresses match, we cannot load the stored value.
        stopTrackingAddress(Addr, StoreMap);
        startTrackingLoad(LI);
        return false;
      }
      continue;
    }

    SILValue Value = P.second.getForwardingValue();
    SILValue Result = CheckResult->forwardAddr(Addr, Value, LI);
    assert(Result);

    DEBUG(llvm::dbgs() << "        Forwarding store from: " << *Addr);
    SILValue(LI).replaceAllUsesWith(Result);
    deleteUntrackedInstruction(LI, StoreMap, Ctx);
    NumForwardedLoads++;
    return true;
  }

  // Search the previous loads and replace the current load or one of the
  // current loads uses with one of the previous loads.
  for (auto &P : Loads) {
    SILValue Addr = P.first;
    SILValue Value = P.second.getForwardingValue();

    // First Check if LI can be completely replaced by PrevLI or if we can
    // construct an extract path from PrevLI's loaded value. The latter occurs
    // if PrevLI is a partially aliasing load that completely subsumes LI.
    if (SILValue Result = tryToForwardAddressValueToLoad(Addr, Value, LI)) {
      DEBUG(llvm::dbgs() << "        Replacing with previous load: "
            << *Result);
      SILValue(LI).replaceAllUsesWith(Result);
      deleteUntrackedInstruction(LI, StoreMap, Ctx);
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
    if (Ctx.getAA()->isPartialAlias(LI->getOperand(), Addr)) {
      tryToSubstitutePartialAliasLoad(Addr, Value, LI);
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
    SILValue(LI).replaceAllUsesWith(Result);
    deleteUntrackedInstruction(LI, StoreMap, Ctx);
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
bool LSBBForwarder::optimize(LSContext &Ctx,
                             CoveredStoreMap &StoreMap,
                             PredOrderInStoreList &PredOrder) {
  auto II = BB->begin(), E = BB->end();
  bool Changed = false;
  while (II != E) {
    SILInstruction *Inst = II++;
    DEBUG(llvm::dbgs() << "    Visiting: " << *Inst);

    // This is a StoreInst. Let's see if we can remove the previous stores.
    if (auto *SI = dyn_cast<StoreInst>(Inst)) {
      Changed |= tryToEliminateDeadStores(Ctx, SI, StoreMap);
      continue;
    }

    // This is a LoadInst. Let's see if we can find a previous loaded, stored
    // value to use instead of this load.
    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      Changed |= tryToForwardLoad(Ctx, LI, StoreMap, PredOrder);
      continue;
    }

    // If this instruction has side effects, but is inert from a load store
    // perspective, skip it.
    if (isLSForwardingInertInstruction(Inst)) {
      DEBUG(llvm::dbgs() << "        Found inert instruction: " << *Inst);
      continue;
    }

    if (!Inst->mayReadOrWriteMemory()) {
      DEBUG(llvm::dbgs() << "        Found readnone instruction, does not "
                            "affect loads and stores.\n");
      continue;
    }

    // All other instructions that read from the memory location of the store
    // invalidates the store.
    if (Inst->mayReadFromMemory()) {
      invalidateReadFromStores(Ctx, Inst, StoreMap);
    }

    // If we have an instruction that may write to memory and we can not prove
    // that it and its operands can not alias a load we have visited, invalidate
    // that load.
    if (Inst->mayWriteToMemory()) {
      // Invalidate any load that we can not prove does not read from one of the
      // writing instructions operands.
      invalidateAliasingLoads(Ctx, Inst, StoreMap);

      // Invalidate our store if Inst writes to the destination location.
      invalidateWriteToStores(Ctx, Inst, StoreMap);
    }
  }

  DEBUG(llvm::dbgs() << "    Final State\n");
  DEBUG(llvm::dbgs() << "        Tracking Load Ops:\n";
        for (auto &P : Loads) {
          llvm::dbgs() << "            " << P;
        });

  DEBUG(llvm::dbgs() << "        Tracking Store Ops:\n";
        for (auto &P : Stores) {
          llvm::dbgs() << "            " << P;
        });

  return Changed;
}

void LSBBForwarder::mergePredecessorState(LSBBForwarder &OtherState) {
  // Merge in the predecessor state.
  DEBUG(llvm::dbgs() << "        Initial Stores:\n");
  llvm::SmallVector<SILValue, 8> DeleteList;
  for (auto &P : Stores) {
    DEBUG(llvm::dbgs() << "            " << *P.first);
    auto Iter = OtherState.Stores.find(P.first);
    if (Iter != OtherState.Stores.end() && P.second == Iter->second)
      continue;
    DeleteList.push_back(P.first);
  }

  if (DeleteList.size()) {
    DEBUG(llvm::dbgs() << "        Stores no longer being tracked:\n");
    for (SILValue V : DeleteList) {
      // TLDR: We do not remove stores from StoreMap here.
      //
      // We perform dataflow over self loops. This is done by initializing our
      // merging with the original BB state in a later iteration if we
      // additionally perform some change in the function. Since we only do this
      // with self loops and in all other cases are conservative, this is safe.
      Stores.erase(V);
    }
    DeleteList.clear();
  } else {
    DEBUG(llvm::dbgs() << "        All stores still being tracked!\n");
  }

  DEBUG(llvm::dbgs() << "            Initial Loads:\n");
  for (auto &P : Loads) {
    DEBUG(llvm::dbgs() << "            " << P.first);
    auto Iter = OtherState.Loads.find(P.first);
    if (Iter != OtherState.Loads.end() && P.second == Iter->second)
      continue;
    DeleteList.push_back(P.first);
  }

  if (DeleteList.size()) {
    DEBUG(llvm::dbgs() << "        Loads no longer being tracked:\n");
    for (SILValue V : DeleteList) {
      Loads.erase(V);
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
    LSBBForwarder &Other = BBIDToForwarderMap[I->second];

    // Calculate SILValues that are stored once in this predecessor.
    llvm::DenseMap<SILValue, StoreInst *> StoredMapOfThisPred;
    llvm::SmallSet<SILValue, 16> StoredValuesOfThisPred;
    for (auto &P : Other.Stores) {
      if (!StoredValuesOfThisPred.count(P.first)) {
        // Assert to make sure to update this when we transition covering stores
        // to use LSValues.
        assert(P.second.getInsts().size() == 1);
        auto *SI = cast<StoreInst>(P.second.getInsts()[0]);
        StoredMapOfThisPred[P.first] = SI;
      } else {
        StoredMapOfThisPred.erase(P.first);
      }
      StoredValuesOfThisPred.insert(P.first);
    }

    if (FirstPred) {
      // Update StoreMap with stores from the first predecessor.
      for (auto I = StoredMapOfThisPred.begin(), E = StoredMapOfThisPred.end();
           I != E; I++) {
        StoreMap[I->first].push_back(I->second);
        DEBUG(llvm::dbgs() << "        Updating StoreMap bb" <<
              Pred->getDebugID() << ": " << I->first << "          " <<
              *I->second);
      }
    } else {
      // Merge in stores from other predecessors.
      for (auto I = StoreMap.begin(), E = StoreMap.end(); I != E;) {
        SILValue Current = I->first;
        if (!StoredMapOfThisPred.count(Current) ||
            I->second[0]->getSrc().getType() !=
            StoredMapOfThisPred[Current]->getSrc().getType()) {
          DEBUG(llvm::dbgs() << "        Removing from StoreMap: " << Current);
          I++; // Move to the next before erasing the current.
          StoreMap.erase(Current);
        }
        else {
          I->second.push_back(StoredMapOfThisPred[Current]);
          DEBUG(llvm::dbgs() << "        Updating StoreMap bb" <<
                Pred->getDebugID() << ": " << Current <<
                "          " << *StoredMapOfThisPred[Current]);
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
  //
  // The reason why we need the states at the end of the basic blocks is that
  // the optimization that this is used for is a simple self cycle. If we move
  // this later we will have already cleared this basic blocks's state when we
  // merged in our predecessors.
  //
  // Once we have a fully optimistic iterative dataflow that uses LSValues
  // this should be removed.
  updateStoreMap(BBToBBIDMap, BBIDToForwarderMap, StoreMap, PredOrder);

  bool HasAtLeastOnePred = false;
  // If we have a self cycle, we keep the old state and merge in states
  // of other predecessors. Otherwise, we initialize the state with the first
  // predecessor's state and merge in states of other predecessors.
  //
  // Again, this is why we call updateStoreMap earlier.
  SILBasicBlock *TheBB = getBB();
  bool HasSelfCycle = std::any_of(BB->pred_begin(), BB->pred_end(),
                                  [&TheBB](SILBasicBlock *Pred) -> bool {
                                    return Pred == TheBB;
                                  });

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
            for (auto &P : Loads) {
              llvm::dbgs() << "            " << P;
            });

      DEBUG(llvm::dbgs() << "        Tracking Stores:\n";
            for (auto &P : Stores) {
              llvm::dbgs() << "            " << P;
            });
    } else if (Pred != BB) {
      DEBUG(llvm::dbgs() << "    Merging with pred  bb" << Pred->getDebugID() <<
            "\n");
      mergePredecessorState(Other);
    }
    HasAtLeastOnePred = true;
  }

  // If we updated the StoreMap earlier and later on we discovered that along
  // all predecessors we have a store, remove the store from the store map.
  // Otherwise we will be representing the same store in two places.
  for (auto &P : Stores) {
    DEBUG(llvm::dbgs() << "        Removing from StoreMap: " << P);
    StoreMap.erase(P.first);
  }
}

//===----------------------------------------------------------------------===//
//                          LSContext Implementation
//===----------------------------------------------------------------------===//

static inline unsigned roundPostOrderSize(PostOrderAnalysis::reverse_range R) {
  unsigned PostOrderSize = std::distance(R.begin(), R.end());

  // NextPowerOf2 operates on uint64_t, so we can not overflow since our input
  // is a 32 bit value. But we need to make sure if the next power of 2 is
  // greater than the representable UINT_MAX, we just pass in (1 << 31) if the
  // next power of 2 is (1 << 32).
  uint64_t SizeRoundedToPow2 = llvm::NextPowerOf2(PostOrderSize);
  if (SizeRoundedToPow2 > uint64_t(UINT_MAX))
    return 1 << 31;
  return unsigned(SizeRoundedToPow2);
}

LSContext::LSContext(AliasAnalysis *AA, PostDominanceInfo *PDI,
                     PostOrderAnalysis::reverse_range RPOT)
  : AA(AA), PDI(PDI), ReversePostOrder(RPOT),
    BBToBBIDMap(roundPostOrderSize(RPOT)),
    BBIDToForwarderMap(roundPostOrderSize(RPOT)) {
  for (SILBasicBlock *BB : ReversePostOrder) {
    unsigned count = BBToBBIDMap.size();
    BBToBBIDMap[BB] = count;
    BBIDToForwarderMap[count].init(BB);
  }
}

bool
LSContext::runIteration() {
  bool Changed = false;

  for (SILBasicBlock *BB : ReversePostOrder) {
    auto IDIter = BBToBBIDMap.find(BB);
    assert(IDIter != BBToBBIDMap.end() && "We just constructed this!?");
    unsigned ID = IDIter->second;
    LSBBForwarder &Forwarder = BBIDToForwarderMap[ID];
    assert(Forwarder.getBB() == BB && "We just constructed this!?");

    DEBUG(llvm::dbgs() << "Visiting bb" << BB->getDebugID() << "\n");

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
    Changed |= Forwarder.optimize(*this, StoreMap, PredOrder);
  }

  return Changed;
}

void LSContext::stopTrackingInst(SILInstruction *I, CoveredStoreMap &StoreMap) {
  if (auto *SI = dyn_cast<StoreInst>(I)) {
    StoreMap.erase(SI->getDest());
  } else if (! isa<LoadInst>(I)) {
    return;
  }

  // LSValues may be propagated (= copied) to multiple blocks. Therefore we
  // have to look into successors as well.
  SmallVector<SILBasicBlock *, 8> WorkList;
  SmallPtrSet<SILBasicBlock *, 8> BlocksHandled;

  // Start with the block of the instruction.
  WorkList.push_back(I->getParentBB());

  while (!WorkList.empty()) {
    SILBasicBlock *WorkBB = WorkList.back();
    WorkList.pop_back();
    BlocksHandled.insert(WorkBB);

    auto IDIter = BBToBBIDMap.find(WorkBB);
    if (IDIter == BBToBBIDMap.end())
      continue;
    LSBBForwarder &F = BBIDToForwarderMap[IDIter->second];

    // Remove the LSValue if it contains I. If not, we don't have to continue
    // with the successors.
    if (!F.removeIfContainsInst(I))
      continue;

    // Continue with the successors.
    for (SILBasicBlock *Succ : WorkBB->getSuccessors()) {
      if (BlocksHandled.count(Succ) == 0)
        WorkList.push_back(Succ);
    }
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

class GlobalLoadStoreOpts : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();

    DEBUG(llvm::dbgs() << "***** Load Store Elimination on function: "
          << F->getName() << " *****\n");

    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *POTA = PM->getAnalysis<PostOrderAnalysis>();
    auto *PDA = PM->getAnalysis<PostDominanceAnalysis>();
    auto *PDI = PDA->get(F);

    LSContext Ctx(AA, PDI, POTA->getReversePostOrder(F));

    bool Changed = false;
    while (Ctx.runIteration())
      Changed = true;

    if (Changed)
      invalidateAnalysis(SILAnalysis::PreserveKind::ProgramFlow);
  }

  StringRef getName() override { return "SIL Load Store Opts"; }
};

} // end anonymous namespace

SILTransform *swift::createGlobalLoadStoreOpts() {
  return new GlobalLoadStoreOpts();
}
