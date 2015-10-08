///===---- LoadStoreOpts.cpp - SIL Load/Store Optimizations Forwarding -----===//
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See http://swift.org/LICENSE.txt for license information
/// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===----------------------------------------------------------------------===//
///
/// This pass eliminates redundant loads, dead stores, and performs load
/// forwarding.
///
/// TODO: Plan to implement new redundant load elimination, a.k.a. load
/// forwarding.
///
/// A load can be eliminated if its value has already been held somewhere,
/// i.e. loaded by a previous load, memory location stored by a known
/// value.
///
/// In this case, one can replace the load instruction with the previous
/// results.
///
/// RedudantLoadElimination (RLE) eliminates such loads by:
///
/// 1. Introducing a notion of a MemLocation that is used to model objects
/// fields. (See below for more details).
///
/// 2. Introducing a notion of a MemValue that is used to model the value
/// that currently resides in the associated MemLocation on the particular
/// program path. (See below for more details).
///
/// 3. Performing a RPO walk over the control flow graph, tracking any
/// MemLocations that are read from or stored into in each basic block. The
/// read or stored value, kept in a map (gen-set) from MemLocation <-> MemValue,
/// becomes the avalable value for the MemLocation. 
///
/// 4. An optimistic iterative intersection-based dataflow is performed on the
/// gen sets until convergence.
///
/// At the core of RLE, there is the MemLocation class. a MemLocation is an
/// abstraction of an object field in program. It consists of a base and a 
/// projection path to the field accessed.
///
/// In SIL, one can access an aggregate as a whole, i.e. store to a struct with
/// 2 Int fields. A store like this will generate 2 *indivisible* MemLocations, 
/// 1 for each field and in addition to keeping a list of MemLocation, RLE also
/// keeps their available MemValues. We call it *indivisible* because it can not
/// be broken down to more MemLocations.
///
/// MemValues consists of a base - a SILValue from the load or store inst,
/// as well as a projection path to which the field it represents. So, a
/// store to an 2-field struct as mentioned above will generate 2 MemLocations
/// and 2 MemValues.
///
/// Every basic block keeps a map between MemLocation <-> MemValue. By keeping
/// the MemLocation and MemValue in their indivisible form, one can
/// easily find which part of the load is redundant and how to compute its
/// forwarding value.
///
/// Given the case which the 2 fields of the struct both have available values,
/// RLE can find their MemValues (maybe by struct_extract from a larger value)
/// and then aggregate them.
///
/// However, this may introduce a lot of extraction and aggregation which may
/// not be necessary. i.e. a store the the struct followed by a load from the
/// struct. To solve this problem, when RLE detects that an load instruction
/// can be replaced by forwarded value, it will try to find minimum # of
/// extraction necessary to form the forwarded value. It will group the
/// available value's by the MemValue base, i.e. the MemValues come from the
/// same instruction,  and then use extraction to obtain the needed components
/// of the base.
///
///===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-redundant-load-elim"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/MemLocation.h"
#include "swift/SIL/MemValue.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
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

/// Disable dead store elimination.
static llvm::cl::opt<bool> DisableGDSE("sil-disable-loadstore-dse",
                                      llvm::cl::init(true), llvm::cl::Hidden);

STATISTIC(NumSameValueStores,"Number of same value stores removed");
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
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::RetainValueInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::CondFailInst:
  case ValueKind::IsUniqueInst:
  case ValueKind::IsUniqueOrPinnedInst:
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
//                                  LSValue
//===----------------------------------------------------------------------===//

namespace {

  /// This class represents either a single value or a covering of values that
  /// we can load forward from via the introdution of a SILArgument. This
  /// enables us to treat the case of having one value or multiple values and
  /// load and store cases all at once abstractly and cleanly.
  class LSValue {
    /// The "parent" basic block which this LSValue originated in.
    ///
    /// In the case where we are tracking one value this is the BB in which the
    /// actual value originated. In the case in which we are tracking a covering
    /// set of loads, this is the BB where if we forward this load value, we
    /// will need to insert a SILArgument.
    SILBasicBlock *ParentBB;

    /// The individual inst or covering inst set that this LSValue represents.
    llvm::TinyPtrVector<SILInstruction *> Insts;

    /// The lazily computed value that can be used to forward this LSValue.
    ///
    /// In the case where we have a single value this is always initialized. In
    /// the case where we are handling a covering set, this is initially null
    /// and when we insert the PHI node, this is set to the SILArgument which
    /// represents the PHI node.
    ///
    /// In the case where we are dealing with loads this is the loaded value or
    /// a phi derived from a covering set of loaded values. In the case where we
    /// are dealing with stores, this is the value that is stored or a phi of
    /// such values.
    SILValue ForwardingValue;

  public:
    LSValue(SILInstruction *NewInst)
    : ParentBB(NewInst->getParent()), Insts(NewInst),
    ForwardingValue(getForwardingValueForLS(NewInst)) {}

    LSValue(SILBasicBlock *NewParentBB, ArrayRef<SILInstruction *> NewInsts);
    LSValue(SILBasicBlock *NewParentBB, ArrayRef<LoadInst *> NewInsts);
    LSValue(SILBasicBlock *NewParentBB, ArrayRef<StoreInst *> NewInsts);

    bool operator==(const LSValue &Other) const;

    void addValue(SILInstruction *I) {
      Insts.push_back(I);
    }

    /// Return the SILValue necessary for forwarding the given LSValue.
    ///
    /// *NOTE* This will create a PHI node if we have not created one yet if we
    /// have a covering set.
    SILValue getForwardingValue();

    /// Returns true if Inst may write to the instructions that make up this
    /// LSValue.
    bool aliasingWrite(AliasAnalysis *AA, SILInstruction *Inst) const {
      // If we have a single inst, just get the forwarding value and compare if
      // they alias.
      if (isSingleInst())
        return AA->mayWriteToMemory(Inst, getAddressForLS(getInst()));

      // Otherwise, loop over all of our forwaring insts and return true if any
      // of them alias Inst.
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

      // Otherwise, loop over all of our forwaring insts and return true if any
      // of them alias Inst.
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
  /// covering of values that we could load forward from via the introdution of
  /// a SILArgument. This enables us to treat both cases the same during our
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

  /// This structure represents either a single value or a covering of values
  /// that we could use in we can dead store elimination or store forward via
  /// the introdution of a SILArgument. This enables us to treat both cases the
  /// same during our transformations in an abstract way.
  class LSStore : public LSValue {
    /// Set to true if this LSStore has been read from by some instruction so it
    /// must be live.
    ///
    /// This allows us to know that the LSStore can not be deleted, but can
    /// still be forwarded from.
    bool HasReadDependence = false;

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
    
    void setHasReadDependence() { HasReadDependence = true; }
    bool hasReadDependence() const { return HasReadDependence; }

    bool mayWriteToMemory(AliasAnalysis *AA, SILInstruction *Inst) {
      for (auto &I : getInsts()) {
        if (AA->mayWriteToMemory(I, getAddressForLS(Inst)))
          return true;
      }
      return false;
    }
  };
  
} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                      Forwarding Feasability Analysis
//===----------------------------------------------------------------------===//

namespace {

enum class ForwardingAnalysisResult {
  /// A failure to forward occurred.
  Failure,

  /// Forwarding can occur using a projection path.
  Normal,

  /// Forwarding can occur from a projection path rooted in an unchecked address
  /// cast.
  UncheckedAddress,
};

/// This is a move-only structure. Thus it has a private default constructor and
/// a deleted copy constructor.
class ForwardingAnalysis final {
  ForwardingAnalysisResult Result;
  UncheckedAddrCastInst *UADCI = nullptr;
  Optional<ProjectionPath> Path;

public:
  ForwardingAnalysis(AliasAnalysis *AA, SILValue Address, LoadInst *LI);

  ForwardingAnalysis(const ForwardingAnalysis &) = delete;
  ForwardingAnalysis(ForwardingAnalysis &&FFA) = default;

  ForwardingAnalysis &operator=(const ForwardingAnalysis &) = delete;
  ForwardingAnalysis &operator=(ForwardingAnalysis &&) = delete;

  SILValue forward(SILValue Addr, SILValue StoredValue, LoadInst *LI);

  /// Returns true if this analysis is able to forward the analyzed load.
  bool canForward() const {
    switch (Result) {
    case ForwardingAnalysisResult::Failure:
      return false;
    case ForwardingAnalysisResult::Normal:
    case ForwardingAnalysisResult::UncheckedAddress:
      return true;
    }
  }

  ForwardingAnalysisResult getResult() const { return Result; }

private:
  SILValue forwardAddrToLdWithExtractPath(SILValue Address,
                                          SILValue StoredValue,
                                          SILInstruction *Inst,
                                          SILValue InstOp);

  SILValue forwardAddrToUncheckedCastToLd(SILValue Address,
                                          SILValue StoredValue,
                                          LoadInst *LI);

  bool initializeWithUncheckedAddrCast(SILValue Address, LoadInst *LI,
                                       UncheckedAddrCastInst *InputUADCI);
};

} // end anonymous namespace

bool
ForwardingAnalysis::
initializeWithUncheckedAddrCast(SILValue Address, LoadInst *LI,
                                UncheckedAddrCastInst *InputUADCI) {
  assert(LI->getOperand().stripAddressProjections() == InputUADCI &&
         "We assume that the UADCI is the load's address stripped of "
         "address projections.");

  // First grab the address operand of our UADCI.
  SILValue UADCIOp = InputUADCI->getOperand();

  // Make sure that this is equal to our address. If not, bail.
  if (UADCIOp != Address)
    return false;

  // Construct the relevant bitcast.
  SILModule &Mod = InputUADCI->getModule();
  SILType InputTy = InputUADCI->getOperand().getType();
  SILType OutputTy = InputUADCI->getType();

  bool InputIsTrivial = InputTy.isTrivial(Mod);
  bool OutputIsTrivial = OutputTy.isTrivial(Mod);

  // If either are generic, bail.
  if (InputTy.hasArchetype() || OutputTy.hasArchetype())
    return false;

  // If we have a trivial input and a non-trivial output bail.
  if (InputIsTrivial && !OutputIsTrivial)
    return false;

  // Check that the input type can be value cast to the output type.  It is
  // possible to cast the address of a smaller InputType to the address of a
  // larger OutputType (the actual memory object must be large enough to hold
  // both types). However, such address casts cannot be converted to value
  // casts.
  if (!SILType::canUnsafeCastValue(InputTy, OutputTy, Mod))
    return false;

  SILValue LdAddr = LI->getOperand();
  Path = std::move(ProjectionPath::getAddrProjectionPath(InputUADCI, LdAddr));
  if (!Path)
    return false;

  Result = ForwardingAnalysisResult::UncheckedAddress;
  UADCI = InputUADCI;
  return true;
}

ForwardingAnalysis::ForwardingAnalysis(AliasAnalysis *AA, SILValue Address,
                                       LoadInst *LI)
    : Result(ForwardingAnalysisResult::Failure),
      UADCI(nullptr), Path() {

  // First if we have a store + unchecked_addr_cast + load, try to forward the
  // value the store using a bitcast.
  SILValue LIOpWithoutProjs = LI->getOperand().stripAddressProjections();
  if (auto *InputUADCI = dyn_cast<UncheckedAddrCastInst>(LIOpWithoutProjs))
    if (initializeWithUncheckedAddrCast(Address, LI, InputUADCI))
      return;

  // Attempt to find the projection path from Address -> Load->getOperand().
  // If we failed to find the path, return an empty value early.
  Path = std::move(
    ProjectionPath::getAddrProjectionPath(Address, LI->getOperand()));
  if (!Path)
    return;
  Result = ForwardingAnalysisResult::Normal;
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
  SILType OutputTy = UADCI->getType();

  SILBuilderWithScope<1> B(LI);
  SILValue CastValue;

  CastValue = B.createUncheckedBitCast(UADCI->getLoc(), StoredValue,
                                       OutputTy.getObjectType());

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
ForwardingAnalysis::forward(SILValue Addr, SILValue StoredValue,
                                LoadInst *LI) {
  assert(canForward() && "Can not forward if analysis failed");

  // First if we have a store + unchecked_addr_cast + load, try to forward the
  // value the store using a bitcast.
  if (Result == ForwardingAnalysisResult::UncheckedAddress)
    return forwardAddrToUncheckedCastToLd(Addr, StoredValue, LI);

  assert(Result == ForwardingAnalysisResult::Normal &&
         "The default kind is Normal.");

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

//===----------------------------------------------------------------------===//
//                            RLEContext Interface
//===----------------------------------------------------------------------===//

namespace {

class RLEBBForwarder;
class LSStore;

/// This class stores global state that we use when processing and also drives
/// the computation. We put its interface at the top for use in other parts of
/// the pass which may want to use this global information.
class RLEContext {
  /// Function this context is currently processing.
  SILFunction *F;

  /// The alias analysis that we will use during all computations.
  AliasAnalysis *AA;

  /// The post dominance analysis that we use for dead store elimination.
  PostDominanceInfo *PDI;

  /// The range that we use to iterate over the reverse post order of the given
  /// function.
  PostOrderFunctionInfo::reverse_range ReversePostOrder;

  /// Keeps all the locations for the current function. The BitVector in each
  /// BBState is then laid on top of it to keep track of which MemLocation
  /// has an upward visible store.
  std::vector<MemLocation> MemLocationVault;

  /// Contains a map between location to their index in the MemLocationVault.
  llvm::DenseMap<MemLocation, unsigned> LocToBitIndex;

  /// A map from each BasicBlock to its index in the BBIDToForwarderMap.
  ///
  /// TODO: Each block does not need its own RLEBBForwarder instance. Only
  /// the set of reaching loads and stores is specific to the block.
  llvm::DenseMap<SILBasicBlock *, unsigned> BBToBBIDMap;

  /// A "map" from a BBID (which is just an index) to an RLEBBForwarder.
  std::vector<RLEBBForwarder> BBIDToForwarderMap;

public:
  RLEContext(SILFunction *F, AliasAnalysis *AA, PostDominanceInfo *PDI,
            PostOrderFunctionInfo::reverse_range RPOT);

  RLEContext(const RLEContext &) = delete;
  RLEContext(RLEContext &&) = default;
  ~RLEContext() = default;

  bool runIteration();

  /// Remove all LSValues from all RLEBBForwarders which contain the load/store
  /// instruction \p I.
  void stopTrackingInst(SILInstruction *I);

  AliasAnalysis *getAA() const { return AA; }
  PostDominanceInfo *getPDI() const { return PDI; }

  /// Get the bit representing the location in the MemLocationVault.
  ///
  /// NOTE: Adds the location to the location vault if necessary.
  unsigned getMemLocationBit(const MemLocation &L);

  /// Given the bit, get the memory location from the MemLocationVault.
  MemLocation &getMemLocation(const unsigned index);
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                               RLEBBForwarder
//===----------------------------------------------------------------------===//

namespace {

/// State of the load store forwarder in one basic block which allows for
/// forwarding from loads, stores -> loads and eliminating dead stores by
/// tracking various types of dependencies.
///
/// Discussion: The algorithm tracks data flow as follows:
///
/// 1. A write that aliases a load causes the load to no longer be tracked.
/// 2. Read that aliases a load:
///    a. If the read is a new load and we can forward from the first load to
///       the second, we forward and delete the new load.
///    b. If the read is a new load which we can not forward, we just track it.
///       This may cause us to track multiple "views" of the same available
///       value, but it should be harmless and may allow for further forwarding
///       opportunities.
///    c. If the read is not a load, we ignore it for the purposes of load
///       forwarding.
/// 3. An aliasing read that occurs after a store, causes the store to no longer
///    be dead, but still allows for forwarding to occur from the store. This is
///    modeled by setting the read dependence flag on the store. In the future
///    this should be tracked at a finer level of granularity.
/// 4. An aliasing new store that occurs after a store causes the old store
///    to be eliminated if:
///    a. The new store completely overlaps the old store. In the future, this
///       may be able to be extended to perform partial dead store elimination.
///    b. The new store post dominates the old store.
///    c. The old store does not have a read dependency.
/// 5. An aliasing write that is a store that does not cause the old store to
///    be dead results in the old store no longer being tracked and the new
///    store being tracked. Again in the future this can be extended to
///    partial dead store elimination.
/// 6. An aliasing write that is not a store (for simplicity) invalidates the
///    store. This can be extended in the future to understand invalidation
///    of specific parts of types (i.e. partial dead store elimination).
///
/// With these in mind, we have the following invariants:
/// 1. All pointers that have available stored values should be no-alias.
class RLEBBForwarder {

  /// The basic block that we are optimizing.
  SILBasicBlock *BB;

  /// The current list of store instructions that stored to memory locations
  /// that were not read/written to since the store was executed.
  llvm::SmallMapVector<SILValue, LSStore, 8> Stores;

  /// This is a list of LoadInst instructions that reference memory locations
  /// were not clobbered by instructions that write to memory. In other words
  /// the SSA value of the load is known to be the same value as the referenced
  /// pointer. The values in the list are potentially updated on each iteration
  /// of the loop below.
  llvm::SmallMapVector<SILValue, LSLoad, 8> Loads;

  /// This is a list of memlocations that have available values. Eventually,
  /// AvailLocs should replace Stores and Loads.
  llvm::SmallMapVector<unsigned, SILValue, 8> AvailLocs;

public:
  RLEBBForwarder() = default;

  void init(SILBasicBlock *NewBB) {
    BB = NewBB;
  }

  bool optimize(RLEContext &Ctx);

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
                              std::vector<RLEBBForwarder> &BBIDToForwarderMap);

  /// Clear all state in the BB optimizer.
  void clear() {
    Stores.clear();
    Loads.clear();
  }

  /// Add this load to our tracking list.
  void startTrackingLoad(RLEContext &Ctx, LoadInst *LI) {
    DEBUG(llvm::dbgs() << "        Tracking Load: " << *LI);

#ifndef NDEBUG
    // Make sure that any stores we are tracking that may alias this load have
    // the read dependence bit set.
    auto *AA = Ctx.getAA();
    for (auto &P : Stores) {
      assert((!P.second.aliasingWrite(AA, LI) ||
              P.second.hasReadDependence()) &&
             "Found aliasing store without read dependence");
    }
#endif

    Loads.insert({LI->getOperand(), LSLoad(LI)});
  }

  /// Add this store to our tracking list.
  void startTrackingStore(RLEContext &Ctx, StoreInst *SI) {
    DEBUG(llvm::dbgs() << "        Tracking Store: " << *SI);

#ifndef NDEBUG
    auto *AA = Ctx.getAA();
    // Make sure that we do not have any loads that alias this store's
    // destination. They should all be invalidated.
    for (auto &P : Loads) {
      assert(!AA->mayWriteToMemory(SI, P.first) &&
             "Found aliasing load that can be written to by store that was not "
             "invalidated");
    }
#endif

    // In the case of Stores this will overwrite whatever we have there.
    Stores.insert({SI->getDest(), LSStore(SI)});
  }

  /// Stop tracking any state related to the address \p Addr.
  void stopTrackingAddress(SILValue Addr) {
    DEBUG(llvm::dbgs() << "        No Longer Tracking: " << Addr);
    Loads.erase(Addr);
    Stores.erase(Addr);
  }

  /// Stop tracking any state related to the address \p Addr.
  void setReadDependencyOnStores(SILValue Addr) {
    DEBUG(llvm::dbgs() << "        Adding read dependency: " << Addr);
    {
      auto Iter = Stores.find(Addr);
      if (Iter != Stores.end()) {
        Iter->second.setHasReadDependence();
      }
    }
  }

  /// Delete the store that we have mapped to Addr, plus other instructions
  /// which get dead due to the removed store.
  void deleteStoreMappedToAddress(RLEContext &Ctx, SILValue Addr);

  void deleteUntrackedInstruction(RLEContext &Ctx, SILInstruction *I);

  /// Invalidate any loads that we can not prove that Inst does not write to.
  void invalidateAliasingLoads(RLEContext &Ctx, SILInstruction *Inst);

  /// Invalidate our store if Inst writes to the destination location.
  void invalidateWriteToStores(RLEContext &Ctx, SILInstruction *Inst);

  /// Invalidate our store if Inst reads from the destination location.
  void invalidateReadFromStores(RLEContext &Ctx, SILInstruction *Inst);

  /// Update the load store states w.r.t. the store instruction.
  void trackStoreInst(RLEContext &Ctx, StoreInst *SI);

  /// Try to prove that SI is a dead store updating all current state. If SI is
  /// dead, eliminate it.
  bool tryToEliminateDeadStores(RLEContext &Ctx, StoreInst *SI);

  /// Try to find a previously known value that we can forward to LI. This
  /// includes from stores and loads.
  bool tryToForwardLoad(RLEContext &Ctx, LoadInst *LI);

  /// Process Instruction which writes to memory in an unknown way. 
  void processUnknownWriteInst(RLEContext &Ctx, SILInstruction *I);

  /// Process Instructions. Extract MemLocations from SIL LoadInst.
  void processLoadInst(RLEContext &Ctx, LoadInst *LI);

  /// Process Instructions. Extract MemLocations from SIL StoreInst.
  void processStoreInst(RLEContext &Ctx, StoreInst *SI);

private:

  /// Merge in the state of an individual predecessor.
  void mergePredecessorState(RLEBBForwarder &OtherState);

  bool tryToSubstitutePartialAliasLoad(SILValue PrevAddr, SILValue PrevValue,
                                       LoadInst *LI);

  bool tryToForwardStoresToLoad(RLEContext &Ctx, LoadInst *LI);

  bool tryToForwardLoadsToLoad(RLEContext &Ctx, LoadInst *LI);

  void verify(RLEContext &Ctx);
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

void RLEBBForwarder::deleteStoreMappedToAddress(RLEContext &Ctx,
                                               SILValue Addr) {
  auto SIIter = Stores.find(Addr);
  if (SIIter == Stores.end())
    return;
  assert((Loads.find(Addr) == Loads.end()) &&
         "An address can never be in both the stores and load lists.");

  llvm::SmallVector<SILInstruction *, 8> InstsToDelete;

  for (auto *SI : SIIter->second.getInsts())
    InstsToDelete.push_back(SI);

  auto UpdateFun = [&](SILInstruction *DeadI) {
    Ctx.stopTrackingInst(DeadI);
  };

  // Delete the instructions.
  for (auto *I : InstsToDelete)
    recursivelyDeleteTriviallyDeadInstructions(I, true, UpdateFun);

  assert(Stores.find(Addr) == Stores.end() &&
         "Addr should be removed during deleting the store instruction");
}

void
RLEBBForwarder::
deleteUntrackedInstruction(RLEContext &Ctx, SILInstruction *I) {
  DEBUG(llvm::dbgs() << "        Deleting all instructions recursively from: "
                     << *I);
  auto UpdateFun = [&](SILInstruction *DeadI) {
    Ctx.stopTrackingInst(DeadI);
  };
  recursivelyDeleteTriviallyDeadInstructions(I, true, UpdateFun);
}

void
RLEBBForwarder::
invalidateAliasingLoads(RLEContext &Ctx, SILInstruction *Inst) {
  AliasAnalysis *AA = Ctx.getAA();
  llvm::SmallVector<SILValue, 4> InvalidatedLoadList;
  for (auto &P : Loads)
    if (P.second.aliasingWrite(AA, Inst))
      InvalidatedLoadList.push_back(P.first);

  for (SILValue LIOp : InvalidatedLoadList) {
    DEBUG(llvm::dbgs() << "        Found an instruction that writes to memory"
          " such that a load operand is invalidated:"
          << LIOp);
    stopTrackingAddress(LIOp);
  }
}

void
RLEBBForwarder::
invalidateWriteToStores(RLEContext &Ctx, SILInstruction *Inst) {
  AliasAnalysis *AA = Ctx.getAA();
  llvm::SmallVector<SILValue, 4> InvalidatedStoreList;
  for (auto &P : Stores)
    if (P.second.aliasingWrite(AA, Inst))
      InvalidatedStoreList.push_back(P.first);

  for (SILValue SIOp : InvalidatedStoreList) {
    DEBUG(llvm::dbgs() << "        Found an instruction that writes to memory"
          " such that a store is invalidated:" << SIOp);
    stopTrackingAddress(SIOp);
  }
}

void RLEBBForwarder::invalidateReadFromStores(RLEContext &Ctx,
                                             SILInstruction *Inst) {
  AliasAnalysis *AA = Ctx.getAA();
  for (auto &P : Stores) {
    if (!P.second.aliasingRead(AA, Inst))
      continue;

    DEBUG(llvm::dbgs() << "        Found an instruction that reads from "
                          "memory such that a store has a read dependence:"
                       << P.first);
    setReadDependencyOnStores(P.first);
  }
}

void RLEBBForwarder::trackStoreInst(RLEContext &Ctx, StoreInst *SI) {
  // Invalidate any load that we can not prove does not read from the stores
  // destination.
  invalidateAliasingLoads(Ctx, SI);

  // Invalidate any store that we can not prove does not write to the stored
  // destination.
  invalidateWriteToStores(Ctx, SI);

  // Insert SI into our store list to start tracking.
  startTrackingStore(Ctx, SI);
}

bool RLEBBForwarder::tryToEliminateDeadStores(RLEContext &Ctx, StoreInst *SI) {
  PostDominanceInfo *PDI = Ctx.getPDI();
  AliasAnalysis *AA = Ctx.getAA();

  // If we are storing a value that is available in the load list then we
  // know that no one clobbered that address and the current store is
  // redundant and we can remove it.
  //
  // e.g.
  // %0 = load %A
  // ... nothing happens in middle and the %A contains the value of %0.
  // store %0 to %A  <---- no need to do this store.
  if (auto *LdSrc = dyn_cast<LoadInst>(SI->getSrc())) {
    // Check that the loaded value is live and that the destination address
    // is the same as the loaded address.
    SILValue LdSrcOp = LdSrc->getOperand();
    auto Iter = Loads.find(LdSrcOp);

    // It is important that we do an exact comparison here so that the types
    // match. Otherwise we would need to make sure that that the store is
    // completely contained within the loaded value which we do not currently
    // do.
    if (Iter != Loads.end() && LdSrcOp == SI->getDest()) {
      deleteUntrackedInstruction(Ctx, SI);
      NumSameValueStores++;
      return true;
    }
  }

  // Invalidate any load that we can not prove does not read from the stores
  // destination.
  invalidateAliasingLoads(Ctx, SI);

  // If we are storing to a previously stored address that this store post
  // dominates, delete the old store.
  llvm::SmallVector<SILValue, 4> StoresToDelete;
  llvm::SmallVector<SILValue, 4> StoresToStopTracking;
  bool Changed = false;
  for (auto &P : Stores) {
    if (!P.second.aliasingWrite(AA, SI))
      continue;

    // If this store has a read dependency then it can not be dead. We need to
    // remove it from the store list and start tracking the new store, though.
    if (P.second.hasReadDependence()) {
      StoresToStopTracking.push_back(P.first);
      DEBUG(llvm::dbgs()
            << "        Found an aliasing store... But we don't "
               "know that it must alias... Can't remove it but will track it.");
      continue;
    }

    // We know that the locations might alias. Check whether if they are the
    // exact same location.
    //
    // Some things to note:
    //
    // 1. Our alias analysis is relatively conservative with must alias. We only
    // return must alias for two values V1, V2 if:
    //   a. V1 == V2.
    //   b. getUnderlyingObject(V1) == getUnderlingObject(V2) and the projection
    //      paths from V1.stripCasts() to V2.stripCasts() to the underlying
    //      objects are exactly the same and do not contain any casts.
    // 2. There are FileCheck sil tests that verifies that the correct
    // load store behavior is preserved in case this behavior changes.
    bool IsStoreToSameLocation = AA->isMustAlias(SI->getDest(), P.first);

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
    deleteStoreMappedToAddress(Ctx, SIOp);
  for (SILValue SIOp : StoresToStopTracking)
    stopTrackingAddress(SIOp);

  // Insert SI into our store list to start tracking.
  startTrackingStore(Ctx, SI);
  return Changed;
}

/// See if there is an extract path from LI that we can replace with PrevLI. If
/// we delete all uses of LI this way, delete LI.
bool RLEBBForwarder::tryToSubstitutePartialAliasLoad(SILValue PrevLIAddr,
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
static SILValue fixPhiPredBlocks(ArrayRef<SILInstruction *> Stores,
                                 SILBasicBlock *Dest) {
  assert(!Stores.empty() && "Can not fix phi pred for multiple blocks");
  assert(Stores.size() ==
         (unsigned)std::distance(Dest->pred_begin(), Dest->pred_end()) &&
         "Multiple store forwarding size mismatch");
  SILSSAUpdater Updater;

  // We know that we only have one store per block already so we can use the
  // SSA updater.
  Updater.Initialize(cast<StoreInst>(Stores[0])->getSrc().getType());
  for (auto *I : Stores)
    Updater.AddAvailableValue(I->getParent(), cast<StoreInst>(I)->getSrc());
  return Updater.GetValueInMiddleOfBlock(Dest);
}

/// Attempt to forward available values from stores to this load. If we do not
/// perform store -> load forwarding, all stores which we failed to forward from
/// which may alias the load will have the read dependency bit set on them.
bool RLEBBForwarder::tryToForwardStoresToLoad(RLEContext &Ctx, LoadInst *LI) {
  // The list of stores that this load conservatively depends on. If we do not
  // eliminate the load from some store, we need to set the read dependency bit
  // on all stores that may alias the load.
  //
  // We use a list so that if we see a later store that can be propagated to the
  // load, we do not set the read dependency bit on any stores. I do not think
  // given the current AA this is possible, but I am being conservatively
  // correct. Additionally if we do not remove the dead store now, if we forward
  // the load we will rerun the algorithm allowing us to hit the store the
  // second time through. But modeling memory effects precisely is an
  // imperitive.
  llvm::SmallVector<SILValue, 8> ReadDependencyStores;

  auto *AA = Ctx.getAA();
  // If we are loading a value that we just stored, forward the stored value.
  for (auto &I : Stores) {
    SILValue Addr = I.first;

    ForwardingAnalysis FA(Ctx.getAA(), Addr, LI);
    if (!FA.canForward()) {
      // Although the addresses match, we cannot load the stored value. If we do
      // not forward the load to be conservative, we need to set a read
      // dependency on this store.
      if (I.second.mayWriteToMemory(AA, LI)) {
        ReadDependencyStores.push_back(Addr);
      }
      continue;
    }

    SILValue Value = I.second.getForwardingValue();
    SILValue Result = FA.forward(Addr, Value, LI);
    assert(Result);

    DEBUG(llvm::dbgs() << "        Forwarding store from: " << *Addr);
    SILValue(LI).replaceAllUsesWith(Result);
    deleteUntrackedInstruction(Ctx, LI);
    NumForwardedLoads++;
    return true;
  }

  // If we were unable to eliminate the load, then set the read dependency bit
  // on all of the addresses that we could have a dependency upon.
  for (auto V : ReadDependencyStores) {
    setReadDependencyOnStores(V);
  }

  return false;
}

/// Try to forward a previously seen load to this load. We allow for multiple
/// loads to be tracked from the same value.
bool RLEBBForwarder::tryToForwardLoadsToLoad(RLEContext &Ctx, LoadInst *LI) {
  // Search the previous loads and replace the current load or one of the
  // current loads uses with one of the previous loads.
  for (auto &P : Loads) {
    SILValue Addr = P.first;
    SILValue Value = P.second.getForwardingValue();

    // First Check if LI can be completely replaced by PrevLI or if we can
    // construct an extract path from PrevLI's loaded value. The latter occurs
    // if PrevLI is a partially aliasing load that completely subsumes LI.
    ForwardingAnalysis FA(Ctx.getAA(), Addr, LI);
    if (FA.canForward()) {
      SILValue Result = FA.forward(Addr, Value, LI);
      DEBUG(llvm::dbgs() << "        Replacing with previous load: "
            << *Result);
      SILValue(LI).replaceAllUsesWith(Result);
      deleteUntrackedInstruction(Ctx, LI);
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

  return false;
}

void RLEBBForwarder::processUnknownWriteInst(RLEContext &Ctx, SILInstruction *I){
  auto *AA = Ctx.getAA();
  llvm::SmallVector<unsigned, 8> LocDeleteList;
  for (auto &X : AvailLocs) {
    if (!AA->mayWriteToMemory(I, Ctx.getMemLocation(X.first).getBase()))
      continue;
    LocDeleteList.push_back(X.first);
  }

  if (LocDeleteList.size()) {
    DEBUG(llvm::dbgs() << "        MemLocation no longer being tracked:\n");
    for (unsigned V : LocDeleteList) {
      AvailLocs.erase(V);
    }
  }
}

void RLEBBForwarder::processStoreInst(RLEContext &Ctx, StoreInst *SI) {
  // Initialize the memory location.
  MemLocation L(cast<StoreInst>(SI)->getDest());

  // If we cant figure out the Base or Projection Path for the read instruction,
  // process it as an unknown memory instruction for now.
  if (!L.isValid()) {
    processUnknownWriteInst(Ctx, SI);
    return;
  }

  // Expand the given Mem into individual fields and process them as
  // separate reads.
  MemLocationList Locs;
  MemLocation::expand(L, &SI->getModule(), Locs);
  for (auto &X : Locs) {
    AvailLocs[Ctx.getMemLocationBit(X)] = SILValue();
  } 
}

void RLEBBForwarder::processLoadInst(RLEContext &Ctx, LoadInst *LI) {
  // Initialize the memory location.
  MemLocation L(cast<LoadInst>(LI)->getOperand());

  // If we cant figure out the Base or Projection Path for the read instruction,
  // simply ignore it for now.
  if (!L.isValid())
    return;

  // Expand the given Mem into individual fields and process them as
  // separate reads.
  MemLocationList Locs;
  MemLocation::expand(L, &LI->getModule(), Locs);
  for (auto &X : Locs) {
    AvailLocs[Ctx.getMemLocationBit(X)] = SILValue();
  } 
}

bool RLEBBForwarder::tryToForwardLoad(RLEContext &Ctx, LoadInst *LI) {

  if (tryToForwardLoadsToLoad(Ctx, LI))
    return true;

  if (tryToForwardStoresToLoad(Ctx, LI))
    return true;

  startTrackingLoad(Ctx, LI);

  // No partial aliased loads were successfully forwarded. Return false to
  // indicate no change.
  return false;
}

/// \brief Promote stored values to loads, remove dead stores and merge
/// duplicated loads.
bool RLEBBForwarder::optimize(RLEContext &Ctx) {
  auto II = BB->begin(), E = BB->end();
  bool Changed = false;
  while (II != E) {
    // Make sure that all of our invariants have been maintained. This is a noop
    // when asserts are disabled.
    verify(Ctx);

    SILInstruction *Inst = II++;
    DEBUG(llvm::dbgs() << "    Visiting: " << *Inst);

    // This is a StoreInst. Let's see if we can remove the previous stores.
    if (auto *SI = dyn_cast<StoreInst>(Inst)) {
      // Keep track of the available value this store generates.
      processStoreInst(Ctx, SI);

      // If DSE is disabled, merely update states w.r.t. this store, but do not
      // try to get rid of the store.
      if (DisableGDSE) {
        trackStoreInst(Ctx, SI);
        continue;
      }
      Changed |= tryToEliminateDeadStores(Ctx, SI);
      continue;
    }

    // This is a LoadInst. Let's see if we can find a previous loaded, stored
    // value to use instead of this load.
    if (auto *LI = dyn_cast<LoadInst>(Inst)) {
      // Keep track of the available value this load generates.
      processLoadInst(Ctx, LI);

      Changed |= tryToForwardLoad(Ctx, LI);
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
    // act as a read dependency on the store meaning that the store can no
    // longer be dead.
    if (Inst->mayReadFromMemory()) {
      invalidateReadFromStores(Ctx, Inst);
    }

    // If we have an instruction that may write to memory and we can not prove
    // that it and its operands can not alias a load we have visited, invalidate
    // that load.
    if (Inst->mayWriteToMemory()) {
      // Invalidate all the aliasing location.
      processUnknownWriteInst(Ctx, Inst);

      // Invalidate any load that we can not prove does not read from one of the
      // writing instructions operands.
      invalidateAliasingLoads(Ctx, Inst);

      // Invalidate our store if Inst writes to the destination location.
      invalidateWriteToStores(Ctx, Inst);
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

void RLEBBForwarder::mergePredecessorState(RLEBBForwarder &OtherState) {
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

  llvm::SmallVector<unsigned, 8> LocDeleteList;
  DEBUG(llvm::dbgs() << "            Initial AvailLocs:\n");
  for (auto &P : AvailLocs) {
    auto Iter = OtherState.AvailLocs.find(P.first);
    if (Iter != OtherState.AvailLocs.end())
      continue;
    LocDeleteList.push_back(P.first);
  }

  if (LocDeleteList.size()) {
    DEBUG(llvm::dbgs() << "        MemLocation no longer being tracked:\n");
    for (unsigned V : LocDeleteList) {
      AvailLocs.erase(V);
    }
  } else {
    DEBUG(llvm::dbgs() << "        All loads still being tracked!\n");
  }
}

void
RLEBBForwarder::
mergePredecessorStates(llvm::DenseMap<SILBasicBlock *,
                                      unsigned> &BBToBBIDMap,
                       std::vector<RLEBBForwarder> &BBIDToForwarderMap) {
  // Clear the state if the basic block has no predecessor.
  if (BB->getPreds().begin() == BB->getPreds().end()) {
    clear();
    return;
  }

  bool HasAtLeastOnePred = false;
  // If we have a self cycle, we keep the old state and merge in states
  // of other predecessors. Otherwise, we initialize the state with the first
  // predecessor's state and merge in states of other predecessors.
  //
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

    RLEBBForwarder &Other = BBIDToForwarderMap[I->second];

    // If we have not had at least one predecessor, initialize RLEBBForwarder
    // with the state of the initial predecessor.
    // If BB is also a predecessor of itself, we should not initialize.
    if (!HasAtLeastOnePred && !HasSelfCycle) {
      DEBUG(llvm::dbgs() << "    Initializing with pred: " << I->second
                         << "\n");
      Stores = Other.Stores;
      Loads = Other.Loads;
      AvailLocs = Other.AvailLocs;

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
}

void RLEBBForwarder::verify(RLEContext &Ctx) {
#ifndef NDEBUG
  llvm::SmallVector<SILValue, 8> Values;
  auto *AA = Ctx.getAA();

  for (auto &P : Stores) {
    for (auto V : Values) {
      for (SILInstruction *SI : P.second.getInsts()) {
        assert(!AA->mayWriteToMemory(SI, V) && "Found overlapping stores");
      }
    }
    Values.push_back(P.first);
  }
#endif
}

//===----------------------------------------------------------------------===//
//                          RLEContext Implementation
//===----------------------------------------------------------------------===//

static inline unsigned
roundPostOrderSize(PostOrderFunctionInfo::reverse_range R) {
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

RLEContext::RLEContext(SILFunction *F, AliasAnalysis *AA, PostDominanceInfo *PDI,
                     PostOrderFunctionInfo::reverse_range RPOT)
    : F(F), AA(AA), PDI(PDI), ReversePostOrder(RPOT),
      BBToBBIDMap(roundPostOrderSize(RPOT)),
      BBIDToForwarderMap(roundPostOrderSize(RPOT)) {
  for (SILBasicBlock *BB : ReversePostOrder) {
    unsigned count = BBToBBIDMap.size();
    BBToBBIDMap[BB] = count;
    BBIDToForwarderMap[count].init(BB);
  }
}

MemLocation &RLEContext::getMemLocation(const unsigned index) {
  // Return the bit position of the given Loc in the MemLocationVault. The bit
  // position is then used to set/reset the bitvector kept by each BBState.
  //
  // We should have the location populated by the enumerateMemLocation at this
  // point.
  //
  return MemLocationVault[index];
}

unsigned RLEContext::getMemLocationBit(const MemLocation &Loc) {
  // Return the bit position of the given Loc in the MemLocationVault. The bit
  // position is then used to set/reset the bitvector kept by each BBState.
  //
  // We should have the location populated by the enumerateMemLocation at this
  // point.
  //
  auto Iter = LocToBitIndex.find(Loc);
  assert(Iter != LocToBitIndex.end() && "MemLocation should have been enumerated");
  return Iter->second;
}

bool
RLEContext::runIteration() {
  // Walk over the function and find all the locations accessed by
  // this function.
  MemLocation::enumerateMemLocations(*F, MemLocationVault, LocToBitIndex);

  bool Changed = false;
  for (SILBasicBlock *BB : ReversePostOrder) {
    auto IDIter = BBToBBIDMap.find(BB);
    assert(IDIter != BBToBBIDMap.end() && "We just constructed this!?");
    unsigned ID = IDIter->second;
    RLEBBForwarder &Forwarder = BBIDToForwarderMap[ID];
    assert(Forwarder.getBB() == BB && "We just constructed this!?");

    DEBUG(llvm::dbgs() << "Visiting bb" << BB->getDebugID() << "\n");

    // Merge the predecessors. After merging, RLEBBForwarder now contains
    // lists of stores|loads that reach the beginning of the basic block
    // along all paths.
    Forwarder.mergePredecessorStates(BBToBBIDMap, BBIDToForwarderMap);

    // Remove dead stores, merge duplicate loads, and forward stores to
    // loads. We also update lists of stores|loads to reflect the end
    // of the basic block.
    Changed |= Forwarder.optimize(*this);
  }

  return Changed;
}

void RLEContext::stopTrackingInst(SILInstruction *I) {
  if (auto *SI = dyn_cast<StoreInst>(I)) {
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
    RLEBBForwarder &F = BBIDToForwarderMap[IDIter->second];

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

class GlobalRedundantLoadElimination : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();

    DEBUG(llvm::dbgs() << "***** Redundant Load Elimination on function: "
          << F->getName() << " *****\n");

    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *PO = PM->getAnalysis<PostOrderAnalysis>()->get(F);
    auto *PDT = PM->getAnalysis<PostDominanceAnalysis>()->get(F);

    RLEContext Ctx(F, AA, PDT, PO->getReversePostOrder());

    bool Changed = false;
    while (Ctx.runIteration())
      Changed = true;

    if (Changed)
      invalidateAnalysis(SILAnalysis::PreserveKind::ProgramFlow);
  }

  StringRef getName() override { return "SIL Redundant Load Elimination"; }
};

} // end anonymous namespace

SILTransform *swift::createGlobalRedundantLoadElimination() {
  return new GlobalRedundantLoadElimination();
}
