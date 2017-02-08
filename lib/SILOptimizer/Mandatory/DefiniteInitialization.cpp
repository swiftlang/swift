//===--- DefiniteInitialization.cpp - Perform definite init analysis ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "definite-init"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "DIMemoryUseCollector.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumAssignRewritten, "Number of assigns rewritten");

template<typename ...ArgTypes>
static void diagnose(SILModule &M, SILLocation loc, ArgTypes... args) {
  M.getASTContext().Diags.diagnose(loc.getSourceLoc(), Diagnostic(args...));
}

enum class PartialInitializationKind {
  /// The box contains a fully-initialized value.
  IsNotInitialization,

  /// The box contains a class instance that we own, but the instance has
  /// not been initialized and should be freed with a special SIL
  /// instruction made for this purpose.
  IsReinitialization,

  /// The box contains an undefined value that should be ignored.
  IsInitialization,
};

/// Emit the sequence that an assign instruction lowers to once we know
/// if it is an initialization or an assignment.  If it is an assignment,
/// a live-in value can be provided to optimize out the reload.
static void LowerAssignInstruction(SILBuilder &B, AssignInst *Inst,
                                   PartialInitializationKind isInitialization) {
  DEBUG(llvm::dbgs() << "  *** Lowering [isInit=" << unsigned(isInitialization)
                     << "]: " << *Inst << "\n");

  ++NumAssignRewritten;

  SILValue Src = Inst->getSrc();
  SILLocation Loc = Inst->getLoc();

  if (isInitialization == PartialInitializationKind::IsInitialization ||
      Inst->getDest()->getType().isTrivial(Inst->getModule())) {

    // If this is an initialization, or the storage type is trivial, we
    // can just replace the assignment with a store.
    assert(isInitialization != PartialInitializationKind::IsReinitialization);
    B.createStore(Loc, Src, Inst->getDest(),
                  StoreOwnershipQualifier::Unqualified);
  } else if (isInitialization == PartialInitializationKind::IsReinitialization) {

    // We have a case where a convenience initializer on a class
    // delegates to a factory initializer from a protocol extension.
    // Factory initializers give us a whole new instance, so the existing
    // instance, which has not been initialized and never will be, must be
    // freed using dealloc_partial_ref.
    SILValue Pointer =
        B.createLoad(Loc, Inst->getDest(), LoadOwnershipQualifier::Unqualified);
    B.createStore(Loc, Src, Inst->getDest(),
                  StoreOwnershipQualifier::Unqualified);

    auto MetatypeTy = CanMetatypeType::get(
        Inst->getDest()->getType().getSwiftRValueType(),
        MetatypeRepresentation::Thick);
    auto SILMetatypeTy = SILType::getPrimitiveObjectType(MetatypeTy);
    SILValue Metatype = B.createValueMetatype(Loc, SILMetatypeTy, Pointer);

    B.createDeallocPartialRef(Loc, Pointer, Metatype);
  } else {
    assert(isInitialization == PartialInitializationKind::IsNotInitialization);

    // Otherwise, we need to replace the assignment with the full
    // load/store/release dance. Note that the new value is already
    // considered to be retained (by the semantics of the storage type),
    // and we're transferring that ownership count into the destination.

    // This is basically TypeLowering::emitStoreOfCopy, except that if we have
    // a known incoming value, we can avoid the load.
    SILValue IncomingVal =
        B.createLoad(Loc, Inst->getDest(), LoadOwnershipQualifier::Unqualified);
    B.createStore(Inst->getLoc(), Src, Inst->getDest(),
                  StoreOwnershipQualifier::Unqualified);

    B.emitDestroyValueOperation(Loc, IncomingVal);
  }

  Inst->eraseFromParent();
}


/// InsertCFGDiamond - Insert a CFG diamond at the position specified by the
/// SILBuilder, with a conditional branch based on "Cond".  This returns the
/// true, false, and continuation block.  If createTrueBB or createFalseBB is
/// false, then only one of the two blocks is created - a CFG triangle instead
/// of a diamond.
///
/// The SILBuilder is left at the start of the ContBB block.
static void InsertCFGDiamond(SILValue Cond, SILLocation Loc, SILBuilder &B,
                             bool createTrueBB,
                             bool createFalseBB,
                             SILBasicBlock *&TrueBB,
                             SILBasicBlock *&FalseBB,
                             SILBasicBlock *&ContBB) {
  SILBasicBlock *StartBB = B.getInsertionBB();
  
  // Start by splitting the current block.
  ContBB = StartBB->split(B.getInsertionPoint());

  // Create the true block if requested.
  SILBasicBlock *TrueDest;
  if (!createTrueBB) {
    TrueDest = ContBB;
    TrueBB = nullptr;
  } else {
    TrueDest = StartBB->getParent()->createBasicBlock();
    B.moveBlockTo(TrueDest, ContBB);
    B.setInsertionPoint(TrueDest);
    B.createBranch(Loc, ContBB);
    TrueBB = TrueDest;
  }
  
  // Create the false block if requested.
  SILBasicBlock *FalseDest;
  if (!createFalseBB) {
    FalseDest = ContBB;
    FalseBB = nullptr;
  } else {
    FalseDest = StartBB->getParent()->createBasicBlock();
    B.moveBlockTo(FalseDest, ContBB);
    B.setInsertionPoint(FalseDest);
    B.createBranch(Loc, ContBB);
    FalseBB = FalseDest;
  }
  
  // Now that we have our destinations, insert a conditional branch on the
  // condition.
  B.setInsertionPoint(StartBB);
  B.createCondBranch(Loc, Cond, TrueDest, FalseDest);
  
  B.setInsertionPoint(ContBB, ContBB->begin());
}


//===----------------------------------------------------------------------===//
// Per-Element Promotion Logic
//===----------------------------------------------------------------------===//

namespace {
  enum class DIKind : unsigned char {
    No,
    Yes,
    Partial
  };
} // end anonymous namespace

/// This implements the lattice merge operation for 2 optional DIKinds.
static Optional<DIKind> mergeKinds(Optional<DIKind> OK1, Optional<DIKind> OK2) {
  // If OK1 is unset, ignore it.
  if (!OK1.hasValue())
    return OK2;

  DIKind K1 = OK1.getValue();

  // If "this" is already partial, we won't learn anything.
  if (K1 == DIKind::Partial)
    return K1;

  // If OK2 is unset, take K1.
  if (!OK2.hasValue())
    return K1;

  DIKind K2 = OK2.getValue();

  // If "K1" is yes, or no, then switch to partial if we find a different
  // answer.
  if (K1 != K2)
    return DIKind::Partial;

  // Otherwise, we're still consistently Yes or No.
  return K1;
}


namespace {
  /// AvailabilitySet - This class stores an array of lattice values for tuple
  /// elements being analyzed for liveness computations.  Each element is
  /// represented with two bits in a bitvector, allowing this to represent the
  /// lattice values corresponding to "Unknown" (bottom), "Live" or "Not Live",
  /// which are the middle elements of the lattice, and "Partial" which is the
  /// top element.
  class AvailabilitySet {
    // We store two bits per element, encoded in the following form:
    //   T,T -> Nothing/Unknown
    //   F,F -> No
    //   F,T -> Yes
    //   T,F -> Partial
    llvm::SmallBitVector Data;
  public:
    AvailabilitySet(unsigned NumElts) {
      Data.resize(NumElts*2, true);
    }

    bool empty() const { return Data.empty(); }
    unsigned size() const { return Data.size()/2; }

    DIKind get(unsigned Elt) const {
      return getConditional(Elt).getValue();
    }

    Optional<DIKind> getConditional(unsigned Elt) const {
      bool V1 = Data[Elt*2], V2 = Data[Elt*2+1];
      if (V1 == V2)
        return V1 ? Optional<DIKind>(None) : DIKind::No;
      return V2 ? DIKind::Yes : DIKind::Partial;
    }

    void set(unsigned Elt, DIKind K) {
      switch (K) {
      case DIKind::No:      Data[Elt*2] = false; Data[Elt*2+1] = false; break;
      case DIKind::Yes:     Data[Elt*2] = false, Data[Elt*2+1] = true; break;
      case DIKind::Partial: Data[Elt*2] = true,  Data[Elt*2+1] = false; break;
      }
    }
    
    void set(unsigned Elt, Optional<DIKind> K) {
      if (!K.hasValue())
        Data[Elt*2] = true, Data[Elt*2+1] = true;
      else
        set(Elt, K.getValue());
    }

    /// containsUnknownElements - Return true if there are any elements that are
    /// unknown.
    bool containsUnknownElements() const {
      // Check that we didn't get any unknown values.
      for (unsigned i = 0, e = size(); i != e; ++i)
        if (!getConditional(i).hasValue())
          return true;
      return false;
    }

    bool isAll(DIKind K) const {
      for (unsigned i = 0, e = size(); i != e; ++i) {
        auto Elt = getConditional(i);
        if (!Elt.hasValue() || Elt.getValue() != K)
          return false;
      }
      return true;
    }
    
    bool hasAny(DIKind K) const {
      for (unsigned i = 0, e = size(); i != e; ++i) {
        auto Elt = getConditional(i);
        if (Elt.hasValue() && Elt.getValue() == K)
          return true;
      }
      return false;
    }
    
    bool isAllYes() const { return isAll(DIKind::Yes); }
    bool isAllNo() const { return isAll(DIKind::No); }
    
    /// changeUnsetElementsTo - If any elements of this availability set are not
    /// known yet, switch them to the specified value.
    void changeUnsetElementsTo(DIKind K) {
      for (unsigned i = 0, e = size(); i != e; ++i)
        if (!getConditional(i).hasValue())
          set(i, K);
    }
    
    void mergeIn(const AvailabilitySet &RHS) {
      // Logically, this is an elementwise "this = merge(this, RHS)" operation,
      // using the lattice merge operation for each element.
      for (unsigned i = 0, e = size(); i != e; ++i)
        set(i, mergeKinds(getConditional(i), RHS.getConditional(i)));
    }

    void dump(llvm::raw_ostream &OS) const {
      OS << '(';
      for (unsigned i = 0, e = size(); i != e; ++i) {
        if (Optional<DIKind> Elt = getConditional(i)) {
          switch (Elt.getValue()) {
            case DIKind::No:      OS << 'n'; break;
            case DIKind::Yes:     OS << 'y'; break;
            case DIKind::Partial: OS << 'p'; break;
          }
        } else {
          OS << '.';
        }
      }
      OS << ')';
    }
  };
 
  LLVM_ATTRIBUTE_USED
  inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                       const AvailabilitySet &AS) {
    AS.dump(OS);
    return OS;
  }
} // end anonymous namespace


namespace {
  /// LiveOutBlockState - Keep track of information about blocks that have
  /// already been analyzed.  Since this is a global analysis, we need this to
  /// cache information about different paths through the CFG.
  struct LiveOutBlockState {
    /// Keep track of whether there is a Store, InOutUse, or Escape locally in
    /// this block.
    bool HasNonLoadUse : 1;

    /// Helper flag used during building the worklist for the dataflow analysis.
    bool isInWorkList : 1;
    
    /// Availability of elements within the block.
    /// Not "empty" for all blocks which have non-load uses or contain the
    /// definition of the memory object.
    AvailabilitySet LocalAvailability;
    
    /// The live out information of the block. This is the LocalAvailability
    /// plus the information merged-in from the predecessor blocks.
    AvailabilitySet OutAvailability;
    
    /// Keep track of blocks where the contents of the self box are not valid
    /// because we're in an error path dominated by a self.init or super.init
    /// delegation.
    Optional<DIKind> LocalSelfConsumed;
    
    /// The live out information of the block. This is the LocalSelfConsumed
    /// plus the information merged-in from the predecessor blocks.
    Optional<DIKind> OutSelfConsumed;

    LiveOutBlockState(unsigned NumElements)
      : HasNonLoadUse(false),
        isInWorkList(false),
        LocalAvailability(NumElements),
        OutAvailability(NumElements) {
    }

    /// Sets all unknown elements to not-available.
    void setUnknownToNotAvailable() {
      LocalAvailability.changeUnsetElementsTo(DIKind::No);
      OutAvailability.changeUnsetElementsTo(DIKind::No);
      if (!LocalSelfConsumed.hasValue())
        LocalSelfConsumed = DIKind::No;
      if (!OutSelfConsumed.hasValue())
        OutSelfConsumed = DIKind::No;
    }

    /// Transfer function for dataflow analysis.
    ///
    /// \param pred Value from a predecessor block
    /// \param out Current live-out
    /// \param local Value from current block, overrides predecessor
    /// \param result Out parameter
    ///
    /// \return True if the result was different from the live-out
    bool transferAvailability(const Optional<DIKind> pred,
                              const Optional<DIKind> out,
                              const Optional<DIKind> local,
                              Optional<DIKind> &result) {
      if (local.hasValue()) {
        // A local availability overrides the incoming value.
        result = local;
      } else {
        result = mergeKinds(out, pred);
      }
      if (result.hasValue() &&
          (!out.hasValue() || result.getValue() != out.getValue())) {
        return true;
      }
      return false;
    }

    /// Merge the state from a predecessor block into the OutAvailability.
    /// Returns true if the live out set changed.
    bool mergeFromPred(const LiveOutBlockState &Pred) {
      bool changed = false;
      for (unsigned i = 0, e = OutAvailability.size(); i != e; ++i) {
        Optional<DIKind> result;
        if (transferAvailability(Pred.OutAvailability.getConditional(i),
                                 OutAvailability.getConditional(i),
                                 LocalAvailability.getConditional(i),
                                 result)) {
          changed = true;
          OutAvailability.set(i, result);
        }
      }

      Optional<DIKind> result;
      if (transferAvailability(Pred.OutSelfConsumed,
                               OutSelfConsumed,
                               LocalSelfConsumed,
                               result)) {
        changed = true;
        OutSelfConsumed = result;
      }

      return changed;
    }

    /// Sets the elements of a use to available.
    void markAvailable(const DIMemoryUse &Use) {
      // If the memory object has nothing in it (e.g., is an empty tuple)
      // ignore.
      if (LocalAvailability.empty()) return;
      
      for (unsigned i = 0; i != Use.NumElements; ++i) {
        LocalAvailability.set(Use.FirstElement+i, DIKind::Yes);
        OutAvailability.set(Use.FirstElement+i, DIKind::Yes);
      }
    }

    /// Mark the block as a failure path, indicating the self value has been
    /// consumed.
    void markFailure(bool partial) {
      LocalSelfConsumed = (partial ? DIKind::Partial : DIKind::Yes);
      OutSelfConsumed = LocalSelfConsumed;
    }

    /// If true, we're not done with our dataflow analysis yet.
    bool containsUndefinedValues() {
      return (!OutSelfConsumed.hasValue() ||
              OutAvailability.containsUnknownElements());
    }
  };

  struct ConditionalDestroy {
    unsigned ReleaseID;
    AvailabilitySet Availability;
    DIKind SelfConsumed;
  };

} // end anonymous namespace

namespace {
  /// LifetimeChecker - This is the main heavy lifting for definite
  /// initialization checking of a memory object.
  class LifetimeChecker {
    SILModule &Module;

    /// TheMemory - This holds information about the memory object being
    /// analyzed.
    DIMemoryObjectInfo TheMemory;

    SmallVectorImpl<DIMemoryUse> &Uses;
    SmallVectorImpl<TermInst *> &FailableInits;
    SmallVectorImpl<SILInstruction *> &Releases;
    std::vector<ConditionalDestroy> ConditionalDestroys;

    llvm::SmallDenseMap<SILBasicBlock*, LiveOutBlockState, 32> PerBlockInfo;

    /// This is a map of uses that are not loads (i.e., they are Stores,
    /// InOutUses, and Escapes), to their entry in Uses.
    llvm::SmallDenseMap<SILInstruction*, unsigned, 16> NonLoadUses;

    /// This is true when there is an ambiguous store, which may be an init or
    /// assign, depending on the CFG path.
    bool HasConditionalInitAssign = false;

    /// This is true when there is an ambiguous destroy, which may be a release
    /// of a fully-initialized or a partially-initialized value.
    bool HasConditionalDestroy = false;
    
    /// This is true when there is a destroy on a path where the self value may
    /// have been consumed, in which case there is nothing to do.
    bool HasConditionalSelfConsumed = false;

    // Keep track of whether we've emitted an error.  We only emit one error per
    // location as a policy decision.
    std::vector<SILLocation> EmittedErrorLocs;
    SmallPtrSet<SILBasicBlock*, 16> BlocksReachableFromEntry;
    
  public:
    LifetimeChecker(const DIMemoryObjectInfo &TheMemory,
                    SmallVectorImpl<DIMemoryUse> &Uses,
                    SmallVectorImpl<TermInst *> &FailableInits,
                    SmallVectorImpl<SILInstruction*> &Releases);

    void doIt();

  private:

    LiveOutBlockState &getBlockInfo(SILBasicBlock *BB) {
      return PerBlockInfo.insert({BB,
                     LiveOutBlockState(TheMemory.NumElements)}).first->second;
    }
    
    AvailabilitySet getLivenessAtInst(SILInstruction *Inst, unsigned FirstElt,
                                      unsigned NumElts);
    int getAnyUninitializedMemberAtInst(SILInstruction *Inst, unsigned FirstElt,
                                        unsigned NumElts);

    DIKind getSelfConsumedAtInst(SILInstruction *Inst);

    bool isInitializedAtUse(const DIMemoryUse &Use,
                            bool *SuperInitDone = nullptr,
                            bool *FailedSelfUse = nullptr);
    

    void handleStoreUse(unsigned UseID);
    void handleLoadUse(unsigned UseID);
    void handleInOutUse(const DIMemoryUse &Use);
    void handleEscapeUse(const DIMemoryUse &Use);

    void handleLoadUseFailure(const DIMemoryUse &InstInfo,
                              bool SuperInitDone,
                              bool FailedSelfUse);

    void handleSuperInitUse(const DIMemoryUse &InstInfo);
    void handleSelfInitUse(DIMemoryUse &InstInfo);
    void updateInstructionForInitState(DIMemoryUse &InstInfo);


    void processUninitializedRelease(SILInstruction *Release,
                                     bool consumed,
                                     SILBasicBlock::iterator InsertPt);
    void deleteDeadRelease(unsigned ReleaseID);

    void processNonTrivialRelease(unsigned ReleaseID);

    SILValue handleConditionalInitAssign();
    void handleConditionalDestroys(SILValue ControlVariableAddr);

    typedef SmallVector<SILBasicBlock *, 16> WorkListType;
    void putIntoWorkList(SILBasicBlock *BB, WorkListType &WorkList);
    void computePredsLiveOut(SILBasicBlock *BB);
    void getOutAvailability(SILBasicBlock *BB, AvailabilitySet &Result);
    void getOutSelfConsumed(SILBasicBlock *BB, Optional<DIKind> &Result);

    bool shouldEmitError(SILInstruction *Inst);
    std::string getUninitElementName(const DIMemoryUse &Use);
    void noteUninitializedMembers(const DIMemoryUse &Use);
    void diagnoseInitError(const DIMemoryUse &Use,
                           Diag<StringRef, bool> DiagMessage);
    void diagnoseRefElementAddr(RefElementAddrInst *REI);
    bool diagnoseMethodCall(const DIMemoryUse &Use,
                            bool SuperInitDone);
    
    bool isBlockIsReachableFromEntry(SILBasicBlock *BB);
  };
} // end anonymous namespace


LifetimeChecker::LifetimeChecker(const DIMemoryObjectInfo &TheMemory,
                                 SmallVectorImpl<DIMemoryUse> &Uses,
                                 SmallVectorImpl<TermInst*> &FailableInits,
                                 SmallVectorImpl<SILInstruction*> &Releases)
  : Module(TheMemory.MemoryInst->getModule()), TheMemory(TheMemory), Uses(Uses),
    FailableInits(FailableInits), Releases(Releases) {

  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (unsigned ui = 0, e = Uses.size(); ui != e; ++ui) {
    auto &Use = Uses[ui];
    assert(Use.Inst && "No instruction identified?");

    // Keep track of all the uses that aren't loads or escapes.  These are
    // important uses that we'll visit, but we don't consider them definition
    // points for liveness computation purposes.
    if (Use.Kind == DIUseKind::Load || Use.Kind == DIUseKind::Escape)
      continue;

    NonLoadUses[Use.Inst] = ui;

    auto &BBInfo = getBlockInfo(Use.Inst->getParent());
    BBInfo.HasNonLoadUse = true;

    // Each of the non-load instructions will each be checked to make sure that
    // they are live-in or a full element store.  This means that the block they
    // are in should be treated as a live out for cross-block analysis purposes.
    BBInfo.markAvailable(Use);
  }

  // Mark blocks where the self value has been consumed.
  for (auto *I : FailableInits) {
    auto *bb = I->getSuccessors()[1].getBB();

    // Horrible hack. Failing inits create critical edges, where all
    // 'return nil's end up. We'll split the edge later.
    bool criticalEdge = isCriticalEdge(I, 1);
    getBlockInfo(bb).markFailure(criticalEdge);
  }

  // If isn't really a use, but we account for the alloc_box/mark_uninitialized
  // as a use so we see it in our dataflow walks.
  NonLoadUses[TheMemory.MemoryInst] = ~0U;
  auto &MemBBInfo = getBlockInfo(TheMemory.MemoryInst->getParent());
  MemBBInfo.HasNonLoadUse = true;

  // There is no scanning required (or desired) for the block that defines the
  // memory object itself.  Its live-out properties are whatever are trivially
  // locally inferred by the loop above.  Mark any unset elements as not
  // available.
  MemBBInfo.setUnknownToNotAvailable();
}

/// Determine whether the specified block is reachable from the entry of the
/// containing function's entrypoint.  This allows us to avoid diagnosing DI
/// errors in synthesized code that turns out to be unreachable.
bool LifetimeChecker::isBlockIsReachableFromEntry(SILBasicBlock *BB) {
  // Lazily compute reachability, so we only have to do it in the case of an
  // error.
  if (BlocksReachableFromEntry.empty()) {
    SmallVector<SILBasicBlock*, 128> Worklist;
    Worklist.push_back(&BB->getParent()->front());
    BlocksReachableFromEntry.insert(Worklist.back());
    
    // Collect all reachable blocks by walking the successors.
    while (!Worklist.empty()) {
      SILBasicBlock *BB = Worklist.pop_back_val();
      for (auto &Succ : BB->getSuccessors()) {
        if (BlocksReachableFromEntry.insert(Succ).second)
          Worklist.push_back(Succ);
      }
    }
  }
  
  return BlocksReachableFromEntry.count(BB);
}


/// shouldEmitError - Check to see if we've already emitted an error at the
/// specified instruction.  If so, return false.  If not, remember the
/// instruction and return true.
bool LifetimeChecker::shouldEmitError(SILInstruction *Inst) {
  // If this instruction is in a dead region, don't report the error.  This can
  // occur because we haven't run DCE before DI and this may be a synthesized
  // statement.  If it isn't synthesized, then DCE will report an error on the
  // dead code.
  if (!isBlockIsReachableFromEntry(Inst->getParent()))
    return false;

  // Check to see if we've already emitted an error at this location.  If so,
  // swallow the error.
  SILLocation InstLoc = Inst->getLoc();
  if (llvm::any_of(EmittedErrorLocs, [&](SILLocation L) -> bool {
        return L.getSourceLoc() == InstLoc.getSourceLoc();
      }))
    return false;

  EmittedErrorLocs.push_back(InstLoc);
  return true;
}


/// Emit notes for each uninitialized stored property in a designated
/// initializer.
void LifetimeChecker::noteUninitializedMembers(const DIMemoryUse &Use) {
  assert(TheMemory.isAnyInitSelf() && !TheMemory.isDelegatingInit() &&
         "Not a designated initializer");

  // Root protocol initializers (ones that reassign to self, not delegating to
  // self.init) have no members to initialize and self itself has already been
  // reported to be uninit in the primary diagnostic.
  if (TheMemory.isProtocolInitSelf())
    return;


  // Determine which members, specifically are uninitialized.
  AvailabilitySet Liveness =
    getLivenessAtInst(Use.Inst, Use.FirstElement, Use.NumElements);

  for (unsigned i = Use.FirstElement, e = Use.FirstElement+Use.NumElements;
       i != e; ++i) {
    if (Liveness.get(i) == DIKind::Yes) continue;

    // Ignore a failed super.init requirement.
    if (i == TheMemory.NumElements-1 && TheMemory.isDerivedClassSelf())
      continue;

    std::string Name;
    auto *Decl = TheMemory.getPathStringToElement(i, Name);
    SILLocation Loc = Use.Inst->getLoc();

    // If we found a non-implicit declaration, use its source location.
    if (Decl && !Decl->isImplicit())
      Loc = SILLocation(Decl);

    diagnose(Module, Loc, diag::stored_property_not_initialized, Name);
  }
}

/// Given a use that has at least one uninitialized element in it, produce a
/// nice symbolic name for the element being accessed.
std::string LifetimeChecker::getUninitElementName(const DIMemoryUse &Use) {

  // If the overall memory allocation has multiple elements, then dive in to
  // explain *which* element is being used uninitialized.  Start by rerunning
  // the query, to get a bitmask of exactly which elements are uninitialized.
  // In a multi-element query, the first element may already be defined and
  // we want to point to the second one.
  unsigned firstUndefElement =
    getAnyUninitializedMemberAtInst(Use.Inst, Use.FirstElement,Use.NumElements);
  assert(firstUndefElement != ~0U && "No undef elements found?");
  
  // Verify that it isn't the super.init marker that failed.  The client should
  // handle this, not pass it down to diagnoseInitError.
  assert((!TheMemory.isDerivedClassSelf() ||
          firstUndefElement != TheMemory.NumElements-1) &&
         "super.init failure not handled in the right place");

  // If the definition is a declaration, try to reconstruct a name and
  // optionally an access path to the uninitialized element.
  //
  // TODO: Given that we know the range of elements being accessed, we don't
  // need to go all the way deep into a recursive tuple here.  We could print
  // an error about "v" instead of "v.0" when "v" has tuple type and the whole
  // thing is accessed inappropriately.
  std::string Name;
  TheMemory.getPathStringToElement(firstUndefElement, Name);

  return Name;
}

void LifetimeChecker::diagnoseInitError(const DIMemoryUse &Use,
                                        Diag<StringRef, bool> DiagMessage) {
  auto *Inst = Use.Inst;
  if (!shouldEmitError(Inst))
    return;

  // If the definition is a declaration, try to reconstruct a name and
  // optionally an access path to the uninitialized element.
  std::string Name = getUninitElementName(Use);

  // Figure out the source location to emit the diagnostic to.  If this is null,
  // it is probably implicitly generated code, so we'll adjust it.
  SILLocation DiagLoc = Inst->getLoc();
  if (DiagLoc.isNull() || DiagLoc.getSourceLoc().isInvalid())
    DiagLoc = Inst->getFunction()->getLocation();

  // Determine whether the field we're touching is a let property.
  bool isLet = true;
  for (unsigned i = 0, e = Use.NumElements; i != e; ++i)
    isLet &= TheMemory.isElementLetProperty(i);

  diagnose(Module, DiagLoc, DiagMessage, Name, isLet);

  // As a debugging hack, print the instruction itself if there is no location
  // information.  This should never happen.
  if (Inst->getLoc().isNull())
    llvm::dbgs() << "  the instruction: " << *Inst << "\n";

  // Provide context as note diagnostics.

  // TODO: The QoI could be improved in many different ways here.  For example,
  // We could give some path information where the use was uninitialized, like
  // the static analyzer.
  if (!TheMemory.isAnyInitSelf())
    diagnose(Module, TheMemory.getLoc(), diag::variable_defined_here, isLet);
}

void LifetimeChecker::doIt() {
  // With any escapes tallied up, we can work through all the uses, checking
  // for definitive initialization, promoting loads, rewriting assigns, and
  // performing other tasks.

  // Note that this should not use a for-each loop, as the Uses list can grow
  // and reallocate as we iterate over it.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &Use = Uses[i];
    auto *Inst = Uses[i].Inst;
    // Ignore entries for instructions that got expanded along the way.
    if (Inst == nullptr) continue;
    
    switch (Use.Kind) {
    case DIUseKind::Initialization:
      // We assume that SILGen knows what it is doing when it produces
      // initializations of variables, because it only produces them when it
      // knows they are correct, and this is a super common case for "var x = y"
      // cases.
      continue;
        
    case DIUseKind::Assign:
      // Instructions classified as assign are only generated when lowering
      // InitOrAssign instructions in regions known to be initialized.  Since
      // they are already known to be definitely init, don't reprocess them.
      continue;
    case DIUseKind::InitOrAssign:
      // FIXME: This is a hack because DI is not understanding SILGen's
      // stack values that have multiple init and destroy lifetime cycles with
      // one allocation.  This happens in foreach silgen (see rdar://15532779)
      // and needs to be resolved someday, either by changing silgen or by
      // teaching DI about destroy events.  In the meantime, just assume that
      // all stores of trivial type are ok.
      if (isa<StoreInst>(Inst))
        continue;
        
      SWIFT_FALLTHROUGH;
    case DIUseKind::PartialStore:
      handleStoreUse(i);
      break;

    case DIUseKind::IndirectIn: {
      bool IsSuperInitComplete, FailedSelfUse;
      // If the value is not definitively initialized, emit an error.
      if (!isInitializedAtUse(Use, &IsSuperInitComplete, &FailedSelfUse))
        handleLoadUseFailure(Use, IsSuperInitComplete, FailedSelfUse);
      break;
    }
    case DIUseKind::Load:
      handleLoadUse(i);
      break;
    case DIUseKind::InOutUse:
      handleInOutUse(Use);
      break;
    case DIUseKind::Escape:
      handleEscapeUse(Use);
      break;
    case DIUseKind::SuperInit:
      handleSuperInitUse(Use);
      break;
    case DIUseKind::SelfInit:
      handleSelfInitUse(Use);
      break;
    }
  }

  // If we emitted an error, there is no reason to proceed with load promotion.
  if (!EmittedErrorLocs.empty()) return;

  // If the memory object has nontrivial type, then any destroy/release of the
  // memory object will destruct the memory.  If the memory (or some element
  // thereof) is not initialized on some path, the bad things happen.  Process
  // releases to adjust for this.
  if (!TheMemory.MemorySILType.isTrivial(Module)) {
    for (unsigned i = 0, e = Releases.size(); i != e; ++i)
      processNonTrivialRelease(i);
  }
  
  // If the memory object had any non-trivial stores that are init or assign
  // based on the control flow path reaching them, then insert dynamic control
  // logic and CFG diamonds to handle this.
  SILValue ControlVariable;
  if (HasConditionalInitAssign ||
      HasConditionalDestroy ||
      HasConditionalSelfConsumed)
    ControlVariable = handleConditionalInitAssign();
  if (!ConditionalDestroys.empty())
    handleConditionalDestroys(ControlVariable);
}

void LifetimeChecker::handleLoadUse(unsigned UseID) {
  DIMemoryUse &Use = Uses[UseID];
  SILInstruction *LoadInst = Use.Inst;

  bool IsSuperInitComplete, FailedSelfUse;
  // If the value is not definitively initialized, emit an error.
  if (!isInitializedAtUse(Use, &IsSuperInitComplete, &FailedSelfUse))
    return handleLoadUseFailure(Use, IsSuperInitComplete, FailedSelfUse);

  // If this is an OpenExistentialAddrInst in preparation for applying
  // a witness method, analyze its use to make sure, that no mutation of
  // lvalue let constants occurs.
  auto* OEAI = dyn_cast<OpenExistentialAddrInst>(LoadInst);
  if (OEAI != nullptr && TheMemory.isElementLetProperty(Use.FirstElement)) {
    for (auto OEAUse : OEAI->getUses()) {
      auto* AI = dyn_cast<ApplyInst>(OEAUse->getUser());

      if (AI == nullptr)
        // User is not an ApplyInst
        continue;

      unsigned OperandNumber = OEAUse->getOperandNumber();
      if (OperandNumber < 1 || OperandNumber > AI->getNumCallArguments())
        // Not used as a call argument
        continue;

      unsigned ArgumentNumber = OperandNumber - 1;

      CanSILFunctionType calleeType = AI->getSubstCalleeType();
      SILParameterInfo parameterInfo = calleeType->getParameters()[ArgumentNumber];

      if (!parameterInfo.isIndirectMutating() ||
          parameterInfo.getType().isAnyClassReferenceType())
        continue;

      if (!shouldEmitError(LoadInst))
        continue;

      std::string PropertyName;
      auto *VD = TheMemory.getPathStringToElement(Use.FirstElement, PropertyName);
      diagnose(Module, LoadInst->getLoc(),
               diag::mutating_protocol_witness_method_on_let_constant, PropertyName);

      if (auto *Var = dyn_cast<VarDecl>(VD)) {
        Var->emitLetToVarNoteIfSimple(nullptr);
      }
    }
  }
}

void LifetimeChecker::handleStoreUse(unsigned UseID) {
  DIMemoryUse &InstInfo = Uses[UseID];

  if (getSelfConsumedAtInst(InstInfo.Inst) != DIKind::No) {
    // FIXME: more specific diagnostics here, handle this case gracefully below.
    if (!shouldEmitError(InstInfo.Inst))
      return;

    diagnose(Module, InstInfo.Inst->getLoc(),
             diag::self_inside_catch_superselfinit,
             (unsigned)TheMemory.isDelegatingInit());
    return;
  }

  // Determine the liveness state of the element that we care about.
  auto Liveness = getLivenessAtInst(InstInfo.Inst, InstInfo.FirstElement,
                                    InstInfo.NumElements);

  // Check to see if the stored location is either fully uninitialized or fully
  // initialized.
  bool isFullyInitialized = true;
  bool isFullyUninitialized = true;
  for (unsigned i = InstInfo.FirstElement, e = i+InstInfo.NumElements;
       i != e;++i) {
    auto DI = Liveness.get(i);
    if (DI != DIKind::Yes)
      isFullyInitialized = false;
    if (DI != DIKind::No)
      isFullyUninitialized = false;
  }

  // If this is a partial store into a struct and the whole struct hasn't been
  // initialized, diagnose this as an error.
  if (InstInfo.Kind == DIUseKind::PartialStore && !isFullyInitialized) {
    assert(InstInfo.NumElements == 1 && "partial stores are intra-element");
    diagnoseInitError(InstInfo, diag::struct_not_fully_initialized);
    return;
  }

  // If this is a store to a 'let' property in an initializer, then we only
  // allow the assignment if the property was completely uninitialized.
  // Overwrites are not permitted.
  if (InstInfo.Kind == DIUseKind::PartialStore || !isFullyUninitialized) {
    for (unsigned i = InstInfo.FirstElement, e = i+InstInfo.NumElements;
         i != e; ++i) {
      if (Liveness.get(i) == DIKind::No || !TheMemory.isElementLetProperty(i))
        continue;

      // Don't emit errors for unreachable code, or if we have already emitted
      // a diagnostic.
      if (!shouldEmitError(InstInfo.Inst))
        continue;
      
      std::string PropertyName;
      auto *VD = TheMemory.getPathStringToElement(i, PropertyName);
      diagnose(Module, InstInfo.Inst->getLoc(),
               diag::immutable_property_already_initialized, PropertyName);
      
      if (auto *Var = dyn_cast<VarDecl>(VD)) {
        if (Var->getParentInitializer())
          diagnose(Module, SILLocation(VD),
                   diag::initial_value_provided_in_let_decl);
        Var->emitLetToVarNoteIfSimple(nullptr);
      }
      return;
    }
  }

  // If this is an initialization or a normal assignment, upgrade the store to
  // an initialization or assign in the uses list so that clients know about it.
  if (isFullyUninitialized) {
    InstInfo.Kind = DIUseKind::Initialization;
  } else if (isFullyInitialized) {
    InstInfo.Kind = DIUseKind::Assign;
  } else {
    // If it is initialized on some paths, but not others, then we have an
    // inconsistent initialization, which needs dynamic control logic in the
    // general case.

    // This is classified as InitOrAssign (not PartialStore), so there are only
    // a few instructions that could reach here.
    assert(InstInfo.Kind == DIUseKind::InitOrAssign &&
           "should only have inconsistent InitOrAssign's here");

    // If this access stores something of non-trivial type, then keep track of
    // it for later.   Once we've collected all of the conditional init/assigns,
    // we can insert a single control variable for the memory object for the
    // whole function.
    if (!InstInfo.onlyTouchesTrivialElements(TheMemory))
      HasConditionalInitAssign = true;
    return;
  }
  
  // Otherwise, we have a definite init or assign.  Make sure the instruction
  // itself is tagged properly.
  updateInstructionForInitState(InstInfo);
}

void LifetimeChecker::handleInOutUse(const DIMemoryUse &Use) {
  bool IsSuperInitDone, FailedSelfUse;

  // inout uses are generally straight-forward: the memory must be initialized
  // before the "address" is passed as an l-value.
  if (!isInitializedAtUse(Use, &IsSuperInitDone, &FailedSelfUse)) {
    if (FailedSelfUse) {
      // FIXME: more specific diagnostics here, handle this case gracefully below.
      if (!shouldEmitError(Use.Inst))
        return;
      
      diagnose(Module, Use.Inst->getLoc(),
               diag::self_inside_catch_superselfinit,
               (unsigned)TheMemory.isDelegatingInit());
      return;
    }

    auto diagID = diag::variable_inout_before_initialized;
    
    if (isa<AddressToPointerInst>(Use.Inst))
      diagID = diag::variable_addrtaken_before_initialized;

    diagnoseInitError(Use, diagID);
    return;
  }

  // One additional check: 'let' properties may never be passed inout, because
  // they are only allowed to have their initial value set, not a subsequent
  // overwrite.
  for (unsigned i = Use.FirstElement, e = i+Use.NumElements;
       i != e; ++i) {
    if (!TheMemory.isElementLetProperty(i))
      continue;

    std::string PropertyName;
    (void)TheMemory.getPathStringToElement(i, PropertyName);
    
    // Try to produce a specific error message about the inout use.  If this is
    // a call to a method or a mutating property access, indicate that.
    // Otherwise, we produce a generic error.
    FuncDecl *FD = nullptr;
    bool isAssignment = false;
    
    if (auto *Apply = dyn_cast<ApplyInst>(Use.Inst)) {
      // If this is a method application, produce a nice, specific, error.
      if (auto *WMI = dyn_cast<MethodInst>(Apply->getOperand(0)))
        FD = dyn_cast<FuncDecl>(WMI->getMember().getDecl());
      
      // If this is a direct/devirt method application, check the location info.
      if (auto *Fn = Apply->getReferencedFunction()) {
        if (Fn->hasLocation()) {
          auto SILLoc = Fn->getLocation();
          FD = SILLoc.getAsASTNode<FuncDecl>();
        }
      }
      
      // If we failed to find the decl a clean and principled way, try hacks:
      // map back to the AST and look for some common patterns.
      if (!FD) {
        if (Apply->getLoc().getAsASTNode<AssignExpr>())
          isAssignment = true;
        else if (auto *CE = Apply->getLoc().getAsASTNode<ApplyExpr>()) {
          if (auto *DSCE = dyn_cast<SelfApplyExpr>(CE->getFn()))
            // Normal method calls are curried, so they are:
            // (call_expr (dot_syntax_call_expr (decl_ref_expr METHOD)))
            FD = dyn_cast<FuncDecl>(DSCE->getCalledValue());
          else
            // Operators and normal function calls are just (CallExpr DRE)
            FD = dyn_cast<FuncDecl>(CE->getCalledValue());
        }
      }
    }
    
    // If we were able to find a method or function call, emit a diagnostic
    // about the method.  The magic numbers used by the diagnostic are:
    // 0 -> method, 1 -> property, 2 -> subscript, 3 -> operator.
    unsigned Case = ~0;
    Identifier MethodName;
    if (FD && FD->isAccessor()) {
      MethodName = FD->getAccessorStorageDecl()->getName();
      Case = isa<SubscriptDecl>(FD->getAccessorStorageDecl()) ? 2 : 1;
    } else if (FD && FD->isOperator()) {
      MethodName = FD->getName();
      Case = 3;
    } else if (FD && FD->isInstanceMember()) {
      MethodName = FD->getName();
      Case = 0;
    }
    
    if (Case != ~0U) {
      diagnose(Module, Use.Inst->getLoc(),
               diag::mutating_method_called_on_immutable_value,
               MethodName, Case, PropertyName);
    } else if (isAssignment) {
      diagnose(Module, Use.Inst->getLoc(),
               diag::assignment_to_immutable_value, PropertyName);
    } else {
      diagnose(Module, Use.Inst->getLoc(),
               diag::immutable_value_passed_inout, PropertyName);
    }
    return;
  }
}

void LifetimeChecker::handleEscapeUse(const DIMemoryUse &Use) {
  // The value must be fully initialized at all escape points.  If not, diagnose
  // the error.
  bool SuperInitDone, FailedSelfUse;

  if (isInitializedAtUse(Use, &SuperInitDone, &FailedSelfUse))
    return;

  auto Inst = Use.Inst;

  if (FailedSelfUse) {
    // FIXME: more specific diagnostics here, handle this case gracefully below.
    if (!shouldEmitError(Inst))
      return;
    
    diagnose(Module, Inst->getLoc(),
             diag::self_inside_catch_superselfinit,
             (unsigned)TheMemory.isDelegatingInit());
    return;
  }

  // This is a use of an uninitialized value.  Emit a diagnostic.
  if (TheMemory.isDelegatingInit() || TheMemory.isDerivedClassSelfOnly()) {
    if (diagnoseMethodCall(Use, false))
      return;

    if (!shouldEmitError(Inst)) return;

    auto diagID = diag::self_before_superselfinit;

    // If this is a load with a single user that is a return, then this is
    // a return before self.init.   Emit a specific diagnostic.
    if (auto *LI = dyn_cast<LoadInst>(Inst))
      if (LI->hasOneUse() &&
          isa<ReturnInst>((*LI->use_begin())->getUser())) {
        diagID = diag::superselfinit_not_called_before_return;
      }
    if (isa<ReturnInst>(Inst)) {
      diagID = diag::superselfinit_not_called_before_return;
    }

    diagnose(Module, Inst->getLoc(), diagID,
             (unsigned)TheMemory.isDelegatingInit());
    return;
  }

  if (isa<ApplyInst>(Inst) && TheMemory.isAnyInitSelf() &&
      !TheMemory.isClassInitSelf()) {
    if (!shouldEmitError(Inst)) return;

    auto diagID = diag::use_of_self_before_fully_init;
    if (TheMemory.isProtocolInitSelf())
      diagID = diag::use_of_self_before_fully_init_protocol;

    diagnose(Module, Inst->getLoc(), diagID);
    noteUninitializedMembers(Use);
    return;
  }
  
  if (isa<PartialApplyInst>(Inst) && TheMemory.isClassInitSelf()) {
    if (!shouldEmitError(Inst)) return;
    
    diagnose(Module, Inst->getLoc(), diag::self_closure_use_uninit);
    noteUninitializedMembers(Use);
    return;
  }
 

  Diag<StringRef, bool> DiagMessage;
  if (isa<MarkFunctionEscapeInst>(Inst)) {
    if (Inst->getLoc().isASTNode<AbstractClosureExpr>())
      DiagMessage = diag::variable_closure_use_uninit;
    else
      DiagMessage = diag::variable_function_use_uninit;
  } else if (isa<UncheckedTakeEnumDataAddrInst>(Inst)) {
    DiagMessage = diag::variable_used_before_initialized;
  } else {
    DiagMessage = diag::variable_closure_use_uninit;
  }

  diagnoseInitError(Use, DiagMessage);
}


/// Failable enum initializer produce a CFG for the return that looks like this,
/// where the load is the use of 'self'.  Detect this pattern so we can consider
/// it a 'return' use of self.
///
///   %3 = load %2 : $*Enum
///   %4 = enum $Optional<Enum>, #Optional.Some!enumelt.1, %3 : $Enum
///   br bb2(%4 : $Optional<Enum>)                    // id: %5
/// bb1:
///   %6 = enum $Optional<Enum>, #Optional.None!enumelt // user: %7
///   br bb2(%6 : $Optional<Enum>)                    // id: %7
/// bb2(%8 : $Optional<Enum>):                        // Preds: bb0 bb1
///   dealloc_stack %1 : $*Enum                       // id: %9
///   return %8 : $Optional<Enum>                     // id: %10
///
static bool isFailableInitReturnUseOfEnum(EnumInst *EI) {
  // Only allow enums forming an optional.
  if (!EI->getType().getSwiftRValueType()->getOptionalObjectType())
    return false;

  if (!EI->hasOneUse()) return false;
  auto *BI = dyn_cast<BranchInst>(EI->use_begin()->getUser());
  if (!BI || BI->getNumArgs() != 1) return false;

  auto *TargetArg = BI->getDestBB()->getArgument(0);
  if (!TargetArg->hasOneUse()) return false;
  return isa<ReturnInst>(TargetArg->use_begin()->getUser());
}

enum BadSelfUseKind {
  BeforeStoredPropertyInit,
  BeforeSuperInit,
  BeforeSelfInit
};

void LifetimeChecker::diagnoseRefElementAddr(RefElementAddrInst *REI) {
  if (!shouldEmitError(REI)) return;
  
  auto Kind = (TheMemory.isAnyDerivedClassSelf()
               ? BeforeSuperInit
               : BeforeSelfInit);
  diagnose(Module, REI->getLoc(),
           diag::self_use_before_fully_init,
           REI->getField()->getName(), true, Kind);
}

bool LifetimeChecker::diagnoseMethodCall(const DIMemoryUse &Use,
                                         bool SuperInitDone) {
  SILInstruction *Inst = Use.Inst;

  if (auto *REI = dyn_cast<RefElementAddrInst>(Inst)) {
    diagnoseRefElementAddr(REI);
    return true;
  }

  // Check to see if this is a use of self or super, due to a method call.  If
  // so, emit a specific diagnostic.
  FuncDecl *Method = nullptr;

  // Check for an access to the base class through an Upcast.
  if (auto UCI = dyn_cast<UpcastInst>(Inst)) {
    // If the upcast is used by a ref_element_addr, then it is an access to a
    // base ivar before super.init is called.
    if (UCI->hasOneUse() && !SuperInitDone) {
      if (auto *REI =
          dyn_cast<RefElementAddrInst>((*UCI->use_begin())->getUser())) {
        diagnoseRefElementAddr(REI);
        return true;
      }
    }

    // If the upcast is used by a class_method + apply, then this is a call of a
    // superclass method or property accessor. If we have a guaranteed method,
    // we will have a release due to a missing optimization in SILGen that will
    // be removed.
    //
    // TODO: Implement the SILGen fixes so this can be removed.
    ClassMethodInst *CMI = nullptr;
    ApplyInst *AI = nullptr;
    SILInstruction *Release = nullptr;
    for (auto UI : UCI->getUses()) {
      auto *User = UI->getUser();
      if (auto *TAI = dyn_cast<ApplyInst>(User)) {
        if (!AI) {
          AI = TAI;
          continue;
        }
      }
      if (auto *TCMI = dyn_cast<ClassMethodInst>(User)) {
        if (!CMI) {
          CMI = TCMI;
          continue;
        }
      }

      if (isa<ReleaseValueInst>(User) || isa<StrongReleaseInst>(User)) {
        if (!Release) {
          Release = User;
          continue;
        }
      }

      // Not a pattern we recognize, conservatively generate a generic
      // diagnostic.
      CMI = nullptr;
      break;
    }

    // If we have a release, make sure that AI is guaranteed. If it is not, emit
    // the generic error that we would emit before.
    //
    // That is the only case where we support pattern matching a release.
    if (Release && AI &&
        !AI->getSubstCalleeType()->getExtInfo().hasGuaranteedSelfParam())
      CMI = nullptr;

    if (AI && CMI) {
      // TODO: Could handle many other members more specifically.
      Method = dyn_cast<FuncDecl>(CMI->getMember().getDecl());
    }
  }

  // If this is an apply instruction and we're in a class initializer, we're
  // calling a method on self.
  if (isa<ApplyInst>(Inst) && TheMemory.isClassInitSelf()) {
    // If this is a method application, produce a nice, specific, error.
    if (auto *CMI = dyn_cast<ClassMethodInst>(Inst->getOperand(0)))
      Method = dyn_cast<FuncDecl>(CMI->getMember().getDecl());

    // If this is a direct/devirt method application, check the location info.
    if (auto *Fn = cast<ApplyInst>(Inst)->getReferencedFunction()) {
      if (Fn->hasLocation())
        Method = Fn->getLocation().getAsASTNode<FuncDecl>();
    }
  }
  
  // If this is part of a call to a witness method for a non-class-bound
  // protocol in a root class, then we could have a store to a temporary whose
  // address is passed into an apply.  Look through this pattern.
  if (auto *SI = dyn_cast<StoreInst>(Inst)) {
    if (SI->getSrc() == TheMemory.MemoryInst &&
        isa<AllocStackInst>(SI->getDest()) &&
        TheMemory.isClassInitSelf()) {
      ApplyInst *TheApply = nullptr;
      // Check to see if the address of the alloc_stack is only passed to one
      // apply_inst.
      for (auto UI : SI->getDest()->getUses()) {
        if (auto *ApplyUser = dyn_cast<ApplyInst>(UI->getUser())) {
          if (!TheApply && UI->getOperandNumber() == 1) {
            TheApply = ApplyUser;
          } else {
            TheApply = nullptr;
            break;
          }
        }
      }
      
      if (TheApply) {
        if (auto *Fn = TheApply->getReferencedFunction())
          if (Fn->hasLocation())
            Method = Fn->getLocation().getAsASTNode<FuncDecl>();
      }
    }
  }
  

  // If we were able to find a method call, emit a diagnostic about the method.
  if (Method) {
    if (!shouldEmitError(Inst)) return true;

    Identifier Name;
    if (Method->isAccessor())
      Name = Method->getAccessorStorageDecl()->getName();
    else
      Name = Method->getName();

    // If this is a use of self before super.init was called, emit a diagnostic
    // about *that* instead of about individual properties not being
    // initialized.
    auto Kind = (SuperInitDone
                 ? BeforeStoredPropertyInit
                 : (TheMemory.isAnyDerivedClassSelf()
                    ? BeforeSuperInit
                    : BeforeSelfInit));
    diagnose(Module, Inst->getLoc(), diag::self_use_before_fully_init,
             Name, Method->isAccessor(), Kind);

    if (SuperInitDone)
      noteUninitializedMembers(Use);
    return true;
  }

  return false;
}

/// handleLoadUseFailure - Check and diagnose various failures when a load use
/// is not fully initialized.
///
/// TODO: In the "No" case, we can emit a fixit adding a default
/// initialization of the type.
///
void LifetimeChecker::handleLoadUseFailure(const DIMemoryUse &Use,
                                           bool SuperInitDone,
                                           bool FailedSelfUse) {
  SILInstruction *Inst = Use.Inst;
  
  if (FailedSelfUse) {
    // FIXME: more specific diagnostics here, handle this case gracefully below.
    if (!shouldEmitError(Inst))
      return;
    
    diagnose(Module, Inst->getLoc(),
             diag::self_inside_catch_superselfinit,
             (unsigned)TheMemory.isDelegatingInit());
    return;
  }
  
  // If this is a load with a single user that is a return (and optionally a
  // retain_value for non-trivial structs/enums), then this is a return in the
  // enum/struct init case, and we haven't stored to self.   Emit a specific
  // diagnostic.
  if (auto *LI = dyn_cast<LoadInst>(Inst)) {
    bool hasReturnUse = false, hasUnknownUses = false;
    
    for (auto LoadUse : LI->getUses()) {
      auto *User = LoadUse->getUser();
      
      // Ignore retains of the struct/enum before the return.
      if (isa<RetainValueInst>(User))
        continue;
      if (isa<ReturnInst>(User)) {
        hasReturnUse = true;
        continue;
      }

      if (auto *EI = dyn_cast<EnumInst>(User))
        if (isFailableInitReturnUseOfEnum(EI)) {
          hasReturnUse = true;
          continue;
        }

      hasUnknownUses = true;
      break;
    }
    
    // Okay, this load is part of a return sequence, diagnose it specially.
    if (hasReturnUse && !hasUnknownUses) {
      // The load is probably part of the common epilog for the function, try to
      // find a more useful source location than the syntactic end of the
      // function.
      SILLocation returnLoc = Inst->getLoc();
      auto TermLoc = Inst->getParent()->getTerminator()->getLoc();
      if (TermLoc.getKind() == SILLocation::ReturnKind) {
        // Function has a single return that got merged into the epilog block.
        returnLoc = TermLoc;
      } else {
        // Otherwise, there are multiple paths to the epilog block, scan its
        // predecessors to see if there are any where the value is unavailable.
        // If so, we can use its location information for more precision.
        for (auto pred : LI->getParent()->getPredecessorBlocks()) {
          auto *TI = pred->getTerminator();
          // Check if this is an early return with uninitialized members.
          if (TI->getLoc().getKind() == SILLocation::ReturnKind &&
              getAnyUninitializedMemberAtInst(TI, Use.FirstElement,
                                              Use.NumElements) != -1)
            returnLoc = TI->getLoc();
        }
      }
      
      if (TheMemory.isEnumInitSelf()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, returnLoc,
                 diag::return_from_init_without_initing_self);
        return;
      }
      
      if (TheMemory.isAnyInitSelf() && !TheMemory.isClassInitSelf() &&
                 !TheMemory.isDelegatingInit()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, returnLoc,
                 diag::return_from_init_without_initing_stored_properties);
        noteUninitializedMembers(Use);
        return;
      }
    }
  }
  
  // If this is a copy_addr into the 'self' argument, and the memory object is a
  // rootself struct/enum or a non-delegating initializer, then we're looking at
  // the implicit "return self" in an address-only initializer.  Emit a specific
  // diagnostic.
  if (auto *CA = dyn_cast<CopyAddrInst>(Inst)) {
    if (CA->isInitializationOfDest() &&
        !CA->getFunction()->getArguments().empty() &&
        SILValue(CA->getFunction()->getArgument(0)) == CA->getDest()) {
      if (TheMemory.isEnumInitSelf()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, Inst->getLoc(),
                 diag::return_from_init_without_initing_self);
        return;
      }

      if (TheMemory.isProtocolInitSelf()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, Inst->getLoc(),
                 diag::return_from_protocol_init_without_initing_self);
        return;
      }

      if (TheMemory.isAnyInitSelf() && !TheMemory.isClassInitSelf() &&
          !TheMemory.isDelegatingInit()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, Inst->getLoc(),
                 diag::return_from_init_without_initing_stored_properties);
        noteUninitializedMembers(Use);
        return;
      }
    }
  }

  // Check to see if we're returning self in a class initializer before all the
  // ivars/super.init are set up.
  if (isa<ReturnInst>(Inst) && TheMemory.isAnyInitSelf()) {
    if (!shouldEmitError(Inst)) return;
    if (!SuperInitDone) {
      diagnose(Module, Inst->getLoc(),
               diag::superselfinit_not_called_before_return,
               (unsigned)TheMemory.isDelegatingInit());
    } else {
      diagnose(Module, Inst->getLoc(),
               diag::return_from_init_without_initing_stored_properties);
      noteUninitializedMembers(Use);
    }
    return;
  }

  // Check to see if this is a use of self or super, due to a method call.  If
  // so, emit a specific diagnostic.
  if (diagnoseMethodCall(Use, SuperInitDone))
    return;

  // Otherwise, we couldn't find a specific thing to complain about, so emit a
  // generic error, depending on what kind of failure this is.
  if (!SuperInitDone) {
    if (!shouldEmitError(Inst)) return;
    diagnose(Module, Inst->getLoc(), diag::self_before_superselfinit,
             (unsigned)TheMemory.isDelegatingInit());
    return;
  }

  // If this is a call to a method in a class initializer, then it must be a use
  // of self before the stored properties are set up.
  if (isa<ApplyInst>(Inst) && TheMemory.isClassInitSelf()) {
    if (!shouldEmitError(Inst)) return;
    diagnose(Module, Inst->getLoc(), diag::use_of_self_before_fully_init);
    noteUninitializedMembers(Use);
    return;
  }

  // If this is a load of self in a struct/enum/protocol initializer, then it
  // must be a use of 'self' before all the stored properties are set up.
  if (isa<LoadInst>(Inst) && TheMemory.isAnyInitSelf() &&
      !TheMemory.isClassInitSelf()) {
    if (!shouldEmitError(Inst)) return;

    auto diagID = diag::use_of_self_before_fully_init;
    if (TheMemory.isProtocolInitSelf())
      diagID = diag::use_of_self_before_fully_init_protocol;
    diagnose(Module, Inst->getLoc(), diagID);
    noteUninitializedMembers(Use);
    return;
  }
  
  // If this is a load into a promoted closure capture, diagnose properly as
  // a capture.
  if (isa<LoadInst>(Inst) && Inst->getLoc().isASTNode<AbstractClosureExpr>())
    diagnoseInitError(Use, diag::variable_closure_use_uninit);
  else
    diagnoseInitError(Use, diag::variable_used_before_initialized);
}

/// handleSuperInitUse - When processing a 'self' argument on a class, this is
/// a call to super.init.
void LifetimeChecker::handleSuperInitUse(const DIMemoryUse &InstInfo) {
  // This is an apply or try_apply.
  auto *Inst = InstInfo.Inst;

  if (getSelfConsumedAtInst(Inst) != DIKind::No) {
    // FIXME: more specific diagnostics here, handle this case gracefully below.
    if (!shouldEmitError(Inst))
      return;
    
    diagnose(Module, Inst->getLoc(),
             diag::self_inside_catch_superselfinit,
             (unsigned)TheMemory.isDelegatingInit());
    return;
  }

  // Determine the liveness states of the memory object, including the
  // super.init state.
  AvailabilitySet Liveness = getLivenessAtInst(Inst, 0, TheMemory.NumElements);

  // super.init() calls require that super.init has not already been called. If
  // it has, reject the program.
  switch (Liveness.get(TheMemory.NumElements-1)) {
  case DIKind::No:  // This is good! Keep going.
    break;
  case DIKind::Yes:
  case DIKind::Partial:
    // This is bad, only one super.init call is allowed.
    if (shouldEmitError(Inst))
      diagnose(Module, Inst->getLoc(), diag::selfinit_multiple_times, 0);
    return;
  }

  // super.init also requires that all ivars are initialized before the
  // superclass initializer runs.
  for (unsigned i = 0, e = TheMemory.NumElements-1; i != e; ++i) {
    if (Liveness.get(i) == DIKind::Yes) continue;

    // If the super.init call is implicit generated, produce a specific
    // diagnostic.
    bool isImplicit = InstInfo.Inst->getLoc().getSourceLoc().isInvalid();
    auto diag = isImplicit ? diag::ivar_not_initialized_at_implicit_superinit :
                diag::ivar_not_initialized_at_superinit;
    return diagnoseInitError(InstInfo, diag);
  }

  // Otherwise everything is good!
}

/// handleSelfInitUse - When processing a 'self' argument on a class, this is
/// a call to self.init.
void LifetimeChecker::handleSelfInitUse(DIMemoryUse &InstInfo) {
  auto *Inst = InstInfo.Inst;

  assert(TheMemory.NumElements == 1 && "delegating inits have a single elt");
  
  if (getSelfConsumedAtInst(Inst) != DIKind::No) {
    // FIXME: more specific diagnostics here, handle this case gracefully below.
    if (!shouldEmitError(Inst))
      return;
    
    diagnose(Module, Inst->getLoc(),
             diag::self_inside_catch_superselfinit,
             (unsigned)TheMemory.isDelegatingInit());
    return;
  }

  // Determine the self.init state.  self.init() calls require that self.init
  // has not already been called. If it has, reject the program.
  switch (getLivenessAtInst(Inst, 0, 1).get(0)) {
  case DIKind::No:  // This is good! Keep going.
    break;
  case DIKind::Yes:
  case DIKind::Partial:
    // This is bad, only one self.init call is allowed.
    if (EmittedErrorLocs.empty() && shouldEmitError(Inst))
      diagnose(Module, Inst->getLoc(), diag::selfinit_multiple_times, 1);
    return;
  }

  // If this is a copy_addr, make sure we remember that it is an initialization.
  if (auto *CAI = dyn_cast<CopyAddrInst>(InstInfo.Inst))
    CAI->setIsInitializationOfDest(IsInitialization);

  // Lower Assign instructions if needed.
  if (isa<AssignInst>(InstInfo.Inst))
    updateInstructionForInitState(InstInfo);
}


/// updateInstructionForInitState - When an instruction being analyzed moves
/// from being InitOrAssign to some concrete state, update it for that state.
/// This includes rewriting them from assign instructions into their composite
/// operations.
void LifetimeChecker::updateInstructionForInitState(DIMemoryUse &InstInfo) {
  SILInstruction *Inst = InstInfo.Inst;

  IsInitialization_t InitKind;
  if (InstInfo.Kind == DIUseKind::Initialization ||
      InstInfo.Kind == DIUseKind::SelfInit)
    InitKind = IsInitialization;
  else {
    assert(InstInfo.Kind == DIUseKind::Assign);
    InitKind = IsNotInitialization;
  }

  // If this is a copy_addr or store_weak, we just set the initialization bit
  // depending on what we find.
  if (auto *CA = dyn_cast<CopyAddrInst>(Inst)) {
    assert(!CA->isInitializationOfDest() &&
           "should not modify copy_addr that already knows it is initialized");
    CA->setIsInitializationOfDest(InitKind);
    return;
  }
  
  if (auto *SW = dyn_cast<StoreWeakInst>(Inst)) {
    assert(!SW->isInitializationOfDest() &&
           "should not modify store_weak that already knows it is initialized");

    SW->setIsInitializationOfDest(InitKind);
    return;
  }

  if (auto *SU = dyn_cast<StoreUnownedInst>(Inst)) {
    assert(!SU->isInitializationOfDest() &&
           "should not modify store_unowned that already knows it is an init");

    SU->setIsInitializationOfDest(InitKind);
    return;
  }
  
  // If this is an assign, rewrite it based on whether it is an initialization
  // or not.
  if (auto *AI = dyn_cast<AssignInst>(Inst)) {
    // Remove this instruction from our data structures, since we will be
    // removing it.
    auto Kind = InstInfo.Kind;
    InstInfo.Inst = nullptr;
    NonLoadUses.erase(Inst);

    PartialInitializationKind PartialInitKind;

    if (TheMemory.isClassInitSelf() &&
        Kind == DIUseKind::SelfInit) {
      assert(InitKind == IsInitialization);
      PartialInitKind = PartialInitializationKind::IsReinitialization;
    } else {
      PartialInitKind = (InitKind == IsInitialization
                         ? PartialInitializationKind::IsInitialization
                         : PartialInitializationKind::IsNotInitialization);
    }

    unsigned FirstElement = InstInfo.FirstElement;
    unsigned NumElements = InstInfo.NumElements;

    SmallVector<SILInstruction*, 4> InsertedInsts;
    SILBuilderWithScope B(Inst, &InsertedInsts);
    LowerAssignInstruction(B, AI, PartialInitKind);

    // If lowering of the assign introduced any new loads or stores, keep track
    // of them.
    for (auto I : InsertedInsts) {
      if (isa<StoreInst>(I)) {
        NonLoadUses[I] = Uses.size();
        Uses.push_back(DIMemoryUse(I, Kind, FirstElement, NumElements));
      } else if (isa<LoadInst>(I)) {
        // If we have a re-initialization, the value must be a class,
        // and the load is just there so we can free the uninitialized
        // object husk; it's not an actual use of 'self'.
        if (PartialInitKind != PartialInitializationKind::IsReinitialization)
          Uses.push_back(DIMemoryUse(I, Load, FirstElement, NumElements));
      }
    }
    return;
  }

  // Ignore non-stores for SelfInits.
  assert(isa<StoreInst>(Inst) && "Unknown store instruction!");
}

void LifetimeChecker::processUninitializedRelease(SILInstruction *Release,
                                                  bool consumed,
                                             SILBasicBlock::iterator InsertPt) {
  // If this is an early release of a class instance, we need to emit a
  // dealloc_partial_ref to free the memory.  If this is a derived class, we
  // may have to do a load of the 'self' box to get the class reference.
  if (TheMemory.isClassInitSelf()) {
    auto Loc = Release->getLoc();
    
    SILBuilderWithScope B(Release);
    B.setInsertionPoint(InsertPt);

    SILValue Pointer = Release->getOperand(0);

    // If we see an alloc_box as the pointer, then we're deallocating a 'box'
    // for self.  Make sure we're using its address result, not its refcount
    // result, and make sure that the box gets deallocated (not released)
    // since the pointer it contains will be manually cleaned up.
    auto *ABI = dyn_cast<AllocBoxInst>(Release->getOperand(0));
    if (ABI)
      Pointer = getOrCreateProjectBox(ABI, 0);

    if (!consumed) {
      if (Pointer->getType().isAddress())
        Pointer =
            B.createLoad(Loc, Pointer, LoadOwnershipQualifier::Unqualified);

      auto MetatypeTy = CanMetatypeType::get(
          TheMemory.MemorySILType.getSwiftRValueType(),
          MetatypeRepresentation::Thick);
      auto SILMetatypeTy = SILType::getPrimitiveObjectType(MetatypeTy);
      SILValue Metatype;

      // In an inherited convenience initializer, we must use the dynamic
      // type of the object since nothing is initialized yet.
      if (TheMemory.isDelegatingInit())
        Metatype = B.createValueMetatype(Loc, SILMetatypeTy, Pointer);
      else
        Metatype = B.createMetatype(Loc, SILMetatypeTy);

      // We've already destroyed any instance variables initialized by this
      // constructor, now destroy instance variables initialized by subclass
      // constructors that delegated to us, and finally free the memory.
      B.createDeallocPartialRef(Loc, Pointer, Metatype);
    }
    
    // dealloc_box the self box if necessary.
    if (ABI) {
      auto DB = B.createDeallocBox(Loc, ABI);
      Releases.push_back(DB);
    }
  }
}

void LifetimeChecker::deleteDeadRelease(unsigned ReleaseID) {
  SILInstruction *Release = Releases[ReleaseID];
  if (isa<DestroyAddrInst>(Release)) {
    SILValue Addr = Release->getOperand(0);
    if (auto *AddrI = dyn_cast<SILInstruction>(Addr))
      recursivelyDeleteTriviallyDeadInstructions(AddrI);
  }
  Release->eraseFromParent();
  Releases[ReleaseID] = nullptr;
}

/// processNonTrivialRelease - We handle two kinds of release instructions here:
/// destroy_addr for alloc_stack's and strong_release/dealloc_box for
/// alloc_box's.  By the time that DI gets here, we've validated that all uses
/// of the memory location are valid.  Unfortunately, the uses being valid
/// doesn't mean that the memory is actually initialized on all paths leading to
/// a release.  As such, we have to push the releases up the CFG to where the
/// value is initialized.
///
void LifetimeChecker::processNonTrivialRelease(unsigned ReleaseID) {
  SILInstruction *Release = Releases[ReleaseID];
  
  // If the instruction is a deallocation of uninitialized memory, no action is
  // required (or desired).
  if (isa<DeallocStackInst>(Release) || isa<DeallocBoxInst>(Release) ||
      isa<DeallocRefInst>(Release) || isa<DeallocPartialRefInst>(Release))
    return;
  
  // We only handle strong_release and destroy_addr here.  The former is a
  // release of a class in an initializer, the later is used for local variable
  // destruction.
  assert(isa<StrongReleaseInst>(Release) || isa<DestroyAddrInst>(Release));
  
  auto Availability = getLivenessAtInst(Release, 0, TheMemory.NumElements);
  DIKind SelfConsumed =
    getSelfConsumedAtInst(Release);

  if (SelfConsumed == DIKind::Yes) {
    // We're in an error path after performing a self.init or super.init
    // delegation. The value was already consumed so there's nothing to release.
    processUninitializedRelease(Release, true, Release->getIterator());
    deleteDeadRelease(ReleaseID);
    return;
  }

  // If the memory object is completely initialized, then nothing needs to be
  // done at this release point.
  if (Availability.isAllYes() && SelfConsumed == DIKind::No)
    return;

  // If it is all 'no' then we can handle it specially without conditional code.
  if (Availability.isAllNo() && SelfConsumed == DIKind::No) {
    processUninitializedRelease(Release, false, Release->getIterator());
    deleteDeadRelease(ReleaseID);
    return;
  }
  
  // If any elements or the 'super.init' state are conditionally live, we need
  // to emit conditional logic.
  if (Availability.hasAny(DIKind::Partial))
    HasConditionalDestroy = true;

  // If the self value was conditionally consumed, we need to emit conditional
  // logic.
  if (SelfConsumed == DIKind::Partial)
    HasConditionalSelfConsumed = true;

  // Otherwise, it is partially live, save it for later processing.
  ConditionalDestroys.push_back({ ReleaseID, Availability, SelfConsumed });
}

static Identifier getBinaryFunction(StringRef Name, SILType IntSILTy,
                                    ASTContext &C) {
  CanType IntTy = IntSILTy.getSwiftRValueType();
  unsigned NumBits =
    cast<BuiltinIntegerType>(IntTy)->getWidth().getFixedWidth();
  // Name is something like: add_Int64
  std::string NameStr = Name;
  NameStr += "_Int" + llvm::utostr(NumBits);

  return C.getIdentifier(NameStr);
}
static Identifier getTruncateToI1Function(SILType IntSILTy, ASTContext &C) {
  CanType IntTy = IntSILTy.getSwiftRValueType();
  unsigned NumBits =
    cast<BuiltinIntegerType>(IntTy)->getWidth().getFixedWidth();

  // Name is something like: trunc_Int64_Int8
  std::string NameStr = "trunc_Int" + llvm::utostr(NumBits) + "_Int1";
  return C.getIdentifier(NameStr);
}

/// Set a bit in the control variable at the current insertion point.
static void updateControlVariable(SILLocation Loc,
                                  const APInt &Bitmask,
                                  SILValue ControlVariable,
                                  Identifier &OrFn,
                                  SILBuilder &B) {
  SILType IVType = ControlVariable->getType().getObjectType();

  // Get the integer constant.
  SILValue MaskVal = B.createIntegerLiteral(Loc, IVType, Bitmask);
  
  // If the mask is all ones, do a simple store, otherwise do a
  // load/or/store sequence to mask in the bits.
  if (!Bitmask.isAllOnesValue()) {
    SILValue Tmp =
        B.createLoad(Loc, ControlVariable, LoadOwnershipQualifier::Unqualified);
    if (!OrFn.get())
      OrFn = getBinaryFunction("or", IVType, B.getASTContext());
      
    SILValue Args[] = { Tmp, MaskVal };
    MaskVal = B.createBuiltin(Loc, OrFn, IVType, {}, Args);
  }

  B.createStore(Loc, MaskVal, ControlVariable,
                StoreOwnershipQualifier::Unqualified);
}

/// Test a bit in the control variable at the current insertion point.
static SILValue testControlVariable(SILLocation Loc,
                                    unsigned Elt,
                                    SILValue ControlVariableAddr,
                                    SILValue &ControlVariable,
                                    Identifier &ShiftRightFn,
                                    Identifier &TruncateFn,
                                    SILBuilder &B) {
  if (!ControlVariable)
    ControlVariable = B.createLoad(Loc, ControlVariableAddr,
                                   LoadOwnershipQualifier::Unqualified);

  SILValue CondVal = ControlVariable;
  CanBuiltinIntegerType IVType = CondVal->getType().castTo<BuiltinIntegerType>();

  // If this memory object has multiple tuple elements, we need to make sure
  // to test the right one.
  if (IVType->getFixedWidth() == 1)
    return CondVal;

  // Shift the mask down to this element.
  if (Elt != 0) {
    if (!ShiftRightFn.get())
      ShiftRightFn = getBinaryFunction("lshr", CondVal->getType(),
                                       B.getASTContext());
    SILValue Amt = B.createIntegerLiteral(Loc, CondVal->getType(), Elt);
    SILValue Args[] = { CondVal, Amt };
    
    CondVal = B.createBuiltin(Loc, ShiftRightFn,
                              CondVal->getType(), {},
                              Args);
  }
  
  if (!TruncateFn.get())
    TruncateFn = getTruncateToI1Function(CondVal->getType(),
                                         B.getASTContext());
  return B.createBuiltin(Loc, TruncateFn,
                         SILType::getBuiltinIntegerType(1, B.getASTContext()),
                         {}, CondVal);
}

/// handleConditionalInitAssign - This memory object has some stores
/// into (some element of) it that is either an init or an assign based on the
/// control flow path through the function, or have a destroy event that happens
/// when the memory object may or may not be initialized.  Handle this by
/// inserting a bitvector that tracks the liveness of each tuple element
/// independently.
SILValue LifetimeChecker::handleConditionalInitAssign() {
  SILLocation Loc = TheMemory.getLoc();
  Loc.markAutoGenerated();

  unsigned NumMemoryElements = TheMemory.NumElements;

  // We might need an extra bit to check if self was consumed.
  if (HasConditionalSelfConsumed)
    NumMemoryElements++;

  // Create the control variable as the first instruction in the function (so
  // that it is easy to destroy the stack location.
  SILBuilder B(TheMemory.getFunctionEntryPoint());
  B.setCurrentDebugScope(TheMemory.getFunction().getDebugScope());
  SILType IVType =
    SILType::getBuiltinIntegerType(NumMemoryElements, Module.getASTContext());
  // Use an empty location for the alloc_stack. If Loc is variable declaration
  // the alloc_stack would look like the storage of that variable.
  auto *ControlVariableBox =
      B.createAllocStack(RegularLocation(SourceLoc()), IVType);
  
  // Find all the return blocks in the function, inserting a dealloc_stack
  // before the return.
  for (auto &BB : TheMemory.getFunction()) {
    auto *Term = BB.getTerminator();
    if (Term->isFunctionExiting()) {
      B.setInsertionPoint(Term);
      B.createDeallocStack(Loc, ControlVariableBox);
    }
  }
  
  // Before the memory allocation, store zero in the control variable.
  B.setInsertionPoint(&*std::next(TheMemory.MemoryInst->getIterator()));
  SILValue ControlVariableAddr = ControlVariableBox;
  auto Zero = B.createIntegerLiteral(Loc, IVType, 0);
  B.createStore(Loc, Zero, ControlVariableAddr,
                StoreOwnershipQualifier::Unqualified);

  Identifier OrFn;

  // At each initialization, mark the initialized elements live.  At each
  // conditional assign, resolve the ambiguity by inserting a CFG diamond.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &Use = Uses[i];
    
    // Ignore deleted uses.
    if (Use.Inst == nullptr) continue;
    
    B.setInsertionPoint(Use.Inst);
    
    // Only full initializations make something live.  inout uses, escapes, and
    // assignments only happen when some kind of init made the element live.
    switch (Use.Kind) {
    default:
      // We can ignore most use kinds here.
      continue;
    case DIUseKind::InitOrAssign:
      // The dynamically unknown case is the interesting one, handle it below.
      break;

    case DIUseKind::SelfInit:
    case DIUseKind::SuperInit:
    case DIUseKind::Initialization:
      // If this is an initialization of only trivial elements, then we don't
      // need to update the bitvector.
      if (Use.onlyTouchesTrivialElements(TheMemory))
        continue;

      APInt Bitmask = Use.getElementBitmask(NumMemoryElements);
      updateControlVariable(Loc, Bitmask, ControlVariableAddr, OrFn, B);
      continue;
    }

    assert(!TheMemory.isDelegatingInit() &&
           "re-assignment of self in delegating init?");

    // If this ambiguous store is only of trivial types, then we don't need to
    // do anything special.  We don't even need keep the init bit for the
    // element precise.
    if (Use.onlyTouchesTrivialElements(TheMemory))
      continue;

    // If this is the interesting case, we need to generate a CFG diamond for
    // each element touched, destroying any live elements so that the resulting
    // store is always an initialize.  This disambiguates the dynamic
    // uncertainty with a runtime check.
    SILValue ControlVariable;

    // If we have multiple tuple elements, we'll have to do some shifting and
    // truncating of the mask value.  These values cache the function_ref so we
    // don't emit multiple of them.
    Identifier ShiftRightFn, TruncateFn;
    
    // If the memory object has multiple tuple elements, we need to destroy any
    // live subelements, since they can each be in a different state of
    // initialization.
    for (unsigned Elt = Use.FirstElement, e = Elt+Use.NumElements;
         Elt != e; ++Elt) {
      auto CondVal = testControlVariable(Loc, Elt, ControlVariableAddr,
                                         ControlVariable, ShiftRightFn, TruncateFn,
                                         B);
      
      SILBasicBlock *TrueBB, *FalseBB, *ContBB;
      InsertCFGDiamond(CondVal, Loc, B,
                       /*createTrueBB=*/true,
                       /*createFalseBB=*/false,
                       TrueBB, FalseBB, ContBB);

      // Emit a destroy_addr in the taken block.
      B.setInsertionPoint(TrueBB->begin());
      SILValue EltPtr = TheMemory.emitElementAddress(Elt, Loc, B);
      if (auto *DA = B.emitDestroyAddrAndFold(Loc, EltPtr))
        Releases.push_back(DA);

      B.setInsertionPoint(ContBB->begin());
    }
    
    // Finally, now that we know the value is uninitialized on all paths, it is
    // safe to do an unconditional initialization.
    Use.Kind = DIUseKind::Initialization;
    
    // Now that the instruction has a concrete "init" form, update it to reflect
    // that.  Note that this can invalidate the Uses vector and delete
    // the instruction.
    updateInstructionForInitState(Use);

    // Revisit the instruction on the next pass through the loop, so that we
    // emit a mask update as appropriate.
    --i;
  }

  // At each failure block, mark the self value as having been consumed.
  if (HasConditionalSelfConsumed) {
    for (auto *I : FailableInits) {
      auto *bb = I->getSuccessors()[1].getBB();

      bool criticalEdge = isCriticalEdge(I, 1);
      if (criticalEdge)
        bb = splitEdge(I, 1);

      B.setInsertionPoint(bb->begin());

      // Set the most significant bit.
      APInt Bitmask = APInt::getHighBitsSet(NumMemoryElements, 1);
      updateControlVariable(Loc, Bitmask, ControlVariableAddr, OrFn, B);
    }
  }

  return ControlVariableAddr;
}

/// Process any destroy_addr and strong_release instructions that are invoked on
/// a partially initialized value.  This generates code to destroy the elements
/// that are known to be alive, ignore the ones that are known to be dead, and
/// to emit branching logic when an element may or may not be initialized.
void LifetimeChecker::
handleConditionalDestroys(SILValue ControlVariableAddr) {
  SILBuilderWithScope B(TheMemory.MemoryInst);
  Identifier ShiftRightFn, TruncateFn;

  unsigned NumMemoryElements = TheMemory.NumElements;
  
  unsigned SelfConsumedElt = TheMemory.NumElements;
  unsigned SuperInitElt = TheMemory.NumElements - 1;

  // We might need an extra bit to check if self was consumed.
  if (HasConditionalSelfConsumed)
    NumMemoryElements++;

  // After handling any conditional initializations, check to see if we have any
  // cases where the value is only partially initialized by the time its
  // lifetime ends.  In this case, we have to make sure not to destroy an
  // element that wasn't initialized yet.
  for (auto &CDElt : ConditionalDestroys) {
    auto *Release = Releases[CDElt.ReleaseID];
    auto Loc = Release->getLoc();
    auto &Availability = CDElt.Availability;
    SILValue ControlVariable;
    
    B.setInsertionPoint(Release);

    // If the self value may have been consumed, we need to check for this
    // before doing anything else.
    switch (CDElt.SelfConsumed) {
    case DIKind::No:
      break;

    case DIKind::Partial: {
      auto CondVal = testControlVariable(Loc, SelfConsumedElt, ControlVariableAddr,
                                         ControlVariable, ShiftRightFn, TruncateFn,
                                         B);
      
      SILBasicBlock *ConsumedBlock, *DeallocBlock, *ContBlock;
      
      InsertCFGDiamond(CondVal, Loc, B,
                       /*createTrueBB=*/true,
                       /*createFalseBB=*/true,
                       ConsumedBlock, DeallocBlock, ContBlock);

      // Boxes have to be deallocated even if the payload was consumed.
      processUninitializedRelease(Release, true, ConsumedBlock->begin());

      B.setInsertionPoint(DeallocBlock->begin());
      break;
    }

    case DIKind::Yes:
      // We should have skipped this conditional destroy by now, oops
      llvm_unreachable("Destroy of consumed self is not conditional");
    }

    // If we're in a designated initializer the object might already be fully
    // initialized.
    if (TheMemory.isDerivedClassSelf()) {
      switch (Availability.get(SuperInitElt)) {
      case DIKind::No:
        // super.init() has not been called yet, proceed below.
        break;

      case DIKind::Partial: {
        // super.init() may or may not have been called yet, we have to check.
        auto CondVal = testControlVariable(Loc, SuperInitElt, ControlVariableAddr,
                                           ControlVariable, ShiftRightFn, TruncateFn,
                                           B);
        
        SILBasicBlock *ReleaseBlock, *DeallocBlock, *ContBlock;
        
        InsertCFGDiamond(CondVal, Loc, B,
                         /*createTrueBB=*/true,
                         /*createFalseBB=*/true,
                         ReleaseBlock, DeallocBlock, ContBlock);

        // Set up the initialized release block.
        B.setInsertionPoint(ReleaseBlock->begin());
        if (isa<StrongReleaseInst>(Release))
          Releases.push_back(B.emitStrongReleaseAndFold(Loc, Release->getOperand(0)));
        else
          Releases.push_back(B.emitDestroyAddrAndFold(Loc, Release->getOperand(0)));
        
        B.setInsertionPoint(DeallocBlock->begin());
        break;
      }
      case DIKind::Yes:
        // super.init() already called, just release the value.
        Release->removeFromParent();
        B.getInsertionBB()->insert(B.getInsertionPoint(), Release);
        continue;
      }
    }

    // If the memory is not-fully initialized at the destroy_addr, then there
    // can be multiple issues: we could have some tuple elements initialized and
    // some not, or we could have a control flow sensitive situation where the
    // elements are only initialized on some paths.  We handle this by splitting
    // the multi-element case into its component parts and treating each
    // separately.
    //
    // Classify each element into three cases: known initialized, known
    // uninitialized, or partially initialized.  The first two cases are simple
    // to handle, whereas the partial case requires dynamic codegen based on the
    // liveness bitmask.
    for (unsigned Elt = 0; Elt != TheMemory.getNumMemoryElements(); ++Elt) {
      switch (Availability.get(Elt)) {
      case DIKind::No:
        // If an element is known to be uninitialized, then we know we can
        // completely ignore it.
        if (TheMemory.isDelegatingInit()) {
          // In a convenience initializer, the sole element of the memory
          // represents the self instance itself, so if self is neither
          // initialized nor consumed, we have to free the memory.
          assert(TheMemory.getNumMemoryElements() == 1);
          processUninitializedRelease(Release, false, B.getInsertionPoint());
        }

        continue;
      case DIKind::Partial:
        // In the partially live case, we have to check our control variable to
        // destroy it.  Handle this below.
        break;

      case DIKind::Yes:
        // If an element is known to be initialized, then we can strictly
        // destroy its value at releases position.
        SILValue EltPtr = TheMemory.emitElementAddress(Elt, Loc, B);
        if (auto *DA = B.emitDestroyAddrAndFold(Release->getLoc(), EltPtr))
          Releases.push_back(DA);
        continue;
      }
      
      // Note - in some partial liveness cases, we can push the destroy_addr up
      // the CFG, instead of immediately generating dynamic control flow checks.
      // This could be handled in processNonTrivialRelease some day.
      
      // Insert a load of the liveness bitmask and split the CFG into a diamond
      // right before the destroy_addr, if we haven't already loaded it.
      auto CondVal = testControlVariable(Loc, Elt, ControlVariableAddr,
                                         ControlVariable, ShiftRightFn, TruncateFn,
                                         B);
      
      SILBasicBlock *ReleaseBlock, *DeallocBlock, *ContBlock;

      InsertCFGDiamond(CondVal, Loc, B,
                       /*createTrueBB=*/true,
                       /*createFalseBB=*/TheMemory.isDelegatingInit(),
                       ReleaseBlock, DeallocBlock, ContBlock);

      // Set up the initialized release block.
      B.setInsertionPoint(ReleaseBlock->begin());
      SILValue EltPtr = TheMemory.emitElementAddress(Elt, Loc, B);
      if (auto *DA = B.emitDestroyAddrAndFold(Loc, EltPtr))
        Releases.push_back(DA);

      // Set up the uninitialized release block. Free the self value in
      // convenience initializers, otherwise there's nothing to do.
      if (TheMemory.isDelegatingInit()) {
        assert(TheMemory.getNumMemoryElements() == 1);
        processUninitializedRelease(Release, false, DeallocBlock->begin());
      }

      B.setInsertionPoint(ContBlock->begin());
    }

    // If we're in a designated initializer, the elements of the memory
    // represent instance variables -- after destroying them, we have to
    // destroy the class instance itself.
    if (!TheMemory.isDelegatingInit())
      processUninitializedRelease(Release, false, B.getInsertionPoint());

    // We've split up the release into zero or more primitive operations,
    // delete it now.
    deleteDeadRelease(CDElt.ReleaseID);
  }
}

void LifetimeChecker::
putIntoWorkList(SILBasicBlock *BB, WorkListType &WorkList) {
  LiveOutBlockState &State = getBlockInfo(BB);
  if (!State.isInWorkList && State.containsUndefinedValues()) {
    DEBUG(llvm::dbgs() << "    add block " << BB->getDebugID()
          << " to worklist\n");
    WorkList.push_back(BB);
    State.isInWorkList = true;
  }
}

void LifetimeChecker::
computePredsLiveOut(SILBasicBlock *BB) {
  DEBUG(llvm::dbgs() << "  Get liveness for block " << BB->getDebugID() << "\n");
  
  // Collect blocks for which we have to calculate the out-availability.
  // These are the paths from blocks with known out-availability to the BB.
  WorkListType WorkList;
  for (auto Pred : BB->getPredecessorBlocks()) {
    putIntoWorkList(Pred, WorkList);
  }
  size_t idx = 0;
  while (idx < WorkList.size()) {
    SILBasicBlock *WorkBB = WorkList[idx++];
    for (auto Pred : WorkBB->getPredecessorBlocks()) {
      putIntoWorkList(Pred, WorkList);
    }
  }

  // Solve the dataflow problem.
#ifndef NDEBUG
  int iteration = 0;
  int upperIterationLimit = WorkList.size() * 2 + 10; // More than enough.
#endif
  bool changed;
  do {
    assert(iteration < upperIterationLimit &&
           "Infinite loop in dataflow analysis?");
    DEBUG(llvm::dbgs() << "    Iteration " << iteration++ << "\n");
    
    changed = false;
    // We collected the blocks in reverse order. Since it is a forward dataflow-
    // problem, it is faster to go through the worklist in reverse order.
    for (auto iter = WorkList.rbegin(); iter != WorkList.rend(); ++iter) {
      SILBasicBlock *WorkBB = *iter;
      LiveOutBlockState &BBState = getBlockInfo(WorkBB);

      // Merge from the predecessor blocks.
      for (auto Pred : WorkBB->getPredecessorBlocks()) {
        changed |= BBState.mergeFromPred(getBlockInfo(Pred));
      }
      DEBUG(llvm::dbgs() << "      Block " << WorkBB->getDebugID() << " out: "
            << BBState.OutAvailability << "\n");

      // Clear the worklist-flag for the next call to computePredsLiveOut().
      // This could be moved out of the outer loop, but doing it here avoids
      // another loop with getBlockInfo() calls.
      BBState.isInWorkList = false;
    }
  } while (changed);
}

void LifetimeChecker::
getOutAvailability(SILBasicBlock *BB, AvailabilitySet &Result) {
  computePredsLiveOut(BB);

  for (auto *Pred : BB->getPredecessorBlocks()) {
    // If self was consumed in a predecessor P, don't look at availability
    // at all, because there's no point in making things more conditional
    // than they are. If we enter the current block through P, the self value
    // will be null and we don't have to destroy anything.
    auto &BBInfo = getBlockInfo(Pred);
    if (BBInfo.OutSelfConsumed.hasValue() &&
        *BBInfo.OutSelfConsumed == DIKind::Yes)
      continue;

    Result.mergeIn(BBInfo.OutAvailability);
  }
  DEBUG(llvm::dbgs() << "    Result: " << Result << "\n");
}

void LifetimeChecker::
getOutSelfConsumed(SILBasicBlock *BB, Optional<DIKind> &Result) {
  computePredsLiveOut(BB);

  for (auto *Pred : BB->getPredecessorBlocks())
    Result = mergeKinds(Result, getBlockInfo(Pred).OutSelfConsumed);
}

/// getLivenessAtInst - Compute the liveness state for any number of tuple
/// elements at the specified instruction.  The elements are returned as an
/// AvailabilitySet.  Elements outside of the range specified may not be
/// computed correctly.
AvailabilitySet LifetimeChecker::
getLivenessAtInst(SILInstruction *Inst, unsigned FirstElt, unsigned NumElts) {
  DEBUG(llvm::dbgs() << "Get liveness " << FirstElt << ", #" << NumElts <<
        " at " << *Inst);

  AvailabilitySet Result(TheMemory.NumElements);

  // Empty tuple queries return a completely "unknown" vector, since they don't
  // care about any of the elements.
  if (NumElts == 0)
    return Result;
  
  SILBasicBlock *InstBB = Inst->getParent();
  
  // The vastly most common case is memory allocations that are not tuples,
  // so special case this with a more efficient algorithm.
  if (TheMemory.NumElements == 1) {
    
    // If there is a store in the current block, scan the block to see if the
    // store is before or after the load.  If it is before, it produces the value
    // we are looking for.
    if (getBlockInfo(InstBB).HasNonLoadUse) {
      for (auto BBI = Inst->getIterator(), E = InstBB->begin(); BBI != E;) {
        --BBI;
        SILInstruction *TheInst = &*BBI;

        // If this instruction is unrelated to the memory, ignore it.
        if (!NonLoadUses.count(TheInst))
          continue;
        
        // If we found the allocation itself, then we are loading something that
        // is not defined at all yet.  Otherwise, we've found a definition, or
        // something else that will require that the memory is initialized at
        // this point.
        Result.set(0,
                   TheInst == TheMemory.MemoryInst ? DIKind::No : DIKind::Yes);
        return Result;
      }
    }

    getOutAvailability(InstBB, Result);

    // If the result element wasn't computed, we must be analyzing code within
    // an unreachable cycle that is not dominated by "TheMemory".  Just force
    // the unset element to yes so that clients don't have to handle this.
    if (!Result.getConditional(0))
      Result.set(0, DIKind::Yes);

    return Result;
  }

  // Check locally to see if any elements are satisfied within the block, and
  // keep track of which ones are still needed in the NeededElements set.
  llvm::SmallBitVector NeededElements(TheMemory.NumElements);
  NeededElements.set(FirstElt, FirstElt+NumElts);
  
  // If there is a store in the current block, scan the block to see if the
  // store is before or after the load.  If it is before, it may produce some of
  // the elements we are looking for.
  if (getBlockInfo(InstBB).HasNonLoadUse) {
    for (auto BBI = Inst->getIterator(), E = InstBB->begin(); BBI != E;) {
      --BBI;
      SILInstruction *TheInst = &*BBI;

      // If this instruction is unrelated to the memory, ignore it.
      auto It = NonLoadUses.find(TheInst);
      if (It == NonLoadUses.end())
        continue;
      
      // If we found the allocation itself, then we are loading something that
      // is not defined at all yet.  Scan no further.
      if (TheInst == TheMemory.MemoryInst) {
        // The result is perfectly decided locally.
        for (unsigned i = FirstElt, e = i+NumElts; i != e; ++i)
          Result.set(i, NeededElements[i] ? DIKind::No : DIKind::Yes);
        return Result;
      }
      
      // Check to see which tuple elements this instruction defines.  Clear them
      // from the set we're scanning from.
      auto &TheInstUse = Uses[It->second];
      NeededElements.reset(TheInstUse.FirstElement,
                           TheInstUse.FirstElement+TheInstUse.NumElements);
      // If that satisfied all of the elements we're looking for, then we're
      // done.  Otherwise, keep going.
      if (NeededElements.none()) {
        Result.changeUnsetElementsTo(DIKind::Yes);
        return Result;
      }
    }
  }

  // Compute the liveness of each element according to our predecessors.
  getOutAvailability(InstBB, Result);
  
  // If any of the elements was locally satisfied, make sure to mark them.
  for (unsigned i = FirstElt, e = i+NumElts; i != e; ++i) {
    if (!NeededElements[i] || !Result.getConditional(i)) {
      // If the result element wasn't computed, we must be analyzing code within
      // an unreachable cycle that is not dominated by "TheMemory".  Just force
      // the unset element to yes so that clients don't have to handle this.
      Result.set(i, DIKind::Yes);
    }
  }
  return Result;
}

/// If any of the elements in the specified range are uninitialized at the
/// specified instruction, return the first element that is uninitialized.  If
/// they are all initialized, return -1.
int LifetimeChecker::getAnyUninitializedMemberAtInst(SILInstruction *Inst,
                                                     unsigned FirstElt,
                                                     unsigned NumElts) {
  // Determine the liveness states of the elements that we care about.
  auto Liveness = getLivenessAtInst(Inst, FirstElt, NumElts);
  
  // Find uninitialized member.
  for (unsigned i = FirstElt, e = i+NumElts; i != e; ++i)
    if (Liveness.get(i) != DIKind::Yes)
      return i;
  
  return -1;
}

/// getSelfConsumedAtInst - Compute the liveness state for any number of tuple
/// elements at the specified instruction.  The elements are returned as an
/// AvailabilitySet.  Elements outside of the range specified may not be
/// computed correctly.
DIKind LifetimeChecker::
getSelfConsumedAtInst(SILInstruction *Inst) {
  DEBUG(llvm::dbgs() << "Get self consumed at " << *Inst);

  Optional<DIKind> Result;

  SILBasicBlock *InstBB = Inst->getParent();
  auto &BlockInfo = getBlockInfo(InstBB);

  if (BlockInfo.LocalSelfConsumed.hasValue())
    return *BlockInfo.LocalSelfConsumed;

  getOutSelfConsumed(InstBB, Result);

  // If the result wasn't computed, we must be analyzing code within
  // an unreachable cycle that is not dominated by "TheMemory".  Just force
  // the result to unconsumed so that clients don't have to handle this.
  if (!Result.hasValue())
    Result = DIKind::No;

  return *Result;
}

/// The specified instruction is a use of some number of elements.  Determine
/// whether all of the elements touched by the instruction are definitely
/// initialized at this point or not.
bool LifetimeChecker::isInitializedAtUse(const DIMemoryUse &Use,
                                         bool *SuperInitDone,
                                         bool *FailedSelfUse) {
  if (FailedSelfUse) *FailedSelfUse = false;
  if (SuperInitDone) *SuperInitDone = true;
  
  // If the self.init() or super.init() call threw an error and
  // we caught it, self is no longer available.
  if (getSelfConsumedAtInst(Use.Inst) != DIKind::No) {
    if (FailedSelfUse) *FailedSelfUse = true;
    return false;
  }

  // Determine the liveness states of the elements that we care about.
  AvailabilitySet Liveness =
    getLivenessAtInst(Use.Inst, Use.FirstElement, Use.NumElements);

  // If the client wants to know about super.init, check to see if we failed
  // it or some other element.
  if (Use.FirstElement+Use.NumElements == TheMemory.NumElements &&
      TheMemory.isAnyDerivedClassSelf() &&
      Liveness.get(Liveness.size()-1) != DIKind::Yes) {
    if (SuperInitDone) *SuperInitDone = false;
  }

  // Check all the results.
  for (unsigned i = Use.FirstElement, e = i+Use.NumElements;
       i != e; ++i)
    if (Liveness.get(i) != DIKind::Yes)
      return false;

  return true;
}






//===----------------------------------------------------------------------===//
//                           Top Level Driver
//===----------------------------------------------------------------------===//

static bool processMemoryObject(SILInstruction *I) {
  DEBUG(llvm::dbgs() << "*** Definite Init looking at: " << *I << "\n");
  DIMemoryObjectInfo MemInfo(I);

  // Set up the datastructure used to collect the uses of the allocation.
  SmallVector<DIMemoryUse, 16> Uses;
  SmallVector<TermInst*, 1> FailableInits;
  SmallVector<SILInstruction*, 4> Releases;

  // Walk the use list of the pointer, collecting them into the Uses array.
  collectDIElementUsesFrom(MemInfo, Uses, FailableInits, Releases, false,
                           /*TreatAddressToPointerAsInout*/ true);

  LifetimeChecker(MemInfo, Uses, FailableInits, Releases).doIt();
  return true;
}

/// checkDefiniteInitialization - Check that all memory objects that require
/// initialization before use are properly set and transform the code as
/// required for flow-sensitive properties.
static bool checkDefiniteInitialization(SILFunction &Fn) {
  DEBUG(llvm::dbgs() << "*** Definite Init visiting function: "
                     <<  Fn.getName() << "\n");
  bool Changed = false;
  for (auto &BB : Fn) {
    for (auto I = BB.begin(), E = BB.end(); I != E; ++I) {
      SILInstruction *Inst = &*I;
      if (isa<MarkUninitializedInst>(Inst))
        Changed |= processMemoryObject(Inst);
    }
  }
  return Changed;
}

/// lowerRawSILOperations - There are a variety of raw-sil instructions like
/// 'assign' that are only used by this pass.  Now that definite initialization
/// checking is done, remove them.
static bool lowerRawSILOperations(SILFunction &Fn) {
  bool Changed = false;
  for (auto &BB : Fn) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = &*I;
      ++I;

      // Unprocessed assigns just lower into assignments, not initializations.
      if (auto *AI = dyn_cast<AssignInst>(Inst)) {
        SILBuilderWithScope B(AI);
        LowerAssignInstruction(B, AI,
            PartialInitializationKind::IsNotInitialization);
        // Assign lowering may split the block. If it did,
        // reset our iteration range to the block after the insertion.
        if (B.getInsertionBB() != &BB)
          I = E;
        Changed = true;
        continue;
      }

      // mark_uninitialized just becomes a noop, resolving to its operand.
      if (auto *MUI = dyn_cast<MarkUninitializedInst>(Inst)) {
        MUI->replaceAllUsesWith(MUI->getOperand());
        MUI->eraseFromParent();
        Changed = true;
        continue;
      }
      
      // mark_function_escape just gets zapped.
      if (isa<MarkFunctionEscapeInst>(Inst)) {
        Inst->eraseFromParent();
        Changed = true;
        continue;
      }
    }
  }
  return Changed;
}


namespace {
/// Perform definitive initialization analysis and promote alloc_box uses into
/// SSA registers for later SSA-based dataflow passes.
class DefiniteInitialization : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    // Walk through and promote all of the alloc_box's that we can.
    if (checkDefiniteInitialization(*getFunction())) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
    }

    DEBUG(getFunction()->verify());

    // Lower raw-sil only instructions used by this pass, like "assign".
    if (lowerRawSILOperations(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
  }

  StringRef getName() override { return "Definite Initialization"; }
};
} // end anonymous namespace

SILTransform *swift::createDefiniteInitialization() {
  return new DefiniteInitialization();
}
