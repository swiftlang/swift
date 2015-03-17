//===--- DefiniteInitialization.cpp - Perform definite init analysis ------===//
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

#define DEBUG_TYPE "definite-init"
#include "swift/SILPasses/Passes.h"
#include "DIMemoryUseCollector.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

STATISTIC(NumAssignRewritten, "Number of assigns rewritten");

template<typename ...ArgTypes>
static void diagnose(SILModule &M, SILLocation loc, ArgTypes... args) {
  M.getASTContext().Diags.diagnose(loc.getSourceLoc(), Diagnostic(args...));
}

/// Emit the sequence that an assign instruction lowers to once we know
/// if it is an initialization or an assignment.  If it is an assignment,
/// a live-in value can be provided to optimize out the reload.
static void LowerAssignInstruction(SILBuilder &B, AssignInst *Inst,
                                   IsInitialization_t isInitialization) {
  DEBUG(llvm::dbgs() << "  *** Lowering [isInit=" << (bool)isInitialization
                     << "]: " << *Inst << "\n");

  ++NumAssignRewritten;

  auto &M = Inst->getModule();
  SILValue Src = Inst->getSrc();

  // If this is an initialization, or the storage type is trivial, we
  // can just replace the assignment with a store.

  // Otherwise, if it has trivial type, we can always just replace the
  // assignment with a store.  If it has non-trivial type and is an
  // initialization, we can also replace it with a store.
  if (isInitialization == IsInitialization ||
      Inst->getDest().getType().isTrivial(M)) {
    B.createStore(Inst->getLoc(), Src, Inst->getDest());
  } else {
    // Otherwise, we need to replace the assignment with the full
    // load/store/release dance.  Note that the new value is already
    // considered to be retained (by the semantics of the storage type),
    // and we're transfering that ownership count into the destination.

    // This is basically TypeLowering::emitStoreOfCopy, except that if we have
    // a known incoming value, we can avoid the load.
    SILValue IncomingVal = B.createLoad(Inst->getLoc(), Inst->getDest());
    B.createStore(Inst->getLoc(), Src, Inst->getDest());

    B.emitReleaseValueOperation(Inst->getLoc(), IncomingVal);
  }

  Inst->eraseFromParent();
}


/// InsertCFGDiamond - Insert a CFG diamond at the position specified by the
/// SILBuilder, with a conditional branch based on "Cond".  This returns the
/// true, false, and continuation block.  If FalseBB is passed in as a null
/// pointer, then only the true block is created - a CFG triangle instead of a
/// diamond.
///
/// The SILBuilder is left at the start of the ContBB block.
static void InsertCFGDiamond(SILValue Cond, SILLocation Loc, SILBuilder &B,
                             SILBasicBlock *&TrueBB,
                             SILBasicBlock **FalseBB,
                             SILBasicBlock *&ContBB) {
  SILBasicBlock *StartBB = B.getInsertionBB();
  SILModule &Module = StartBB->getModule();
  
  // Start by splitting the current block.
  ContBB = StartBB->splitBasicBlock(B.getInsertionPoint());
  
  // Create the true block.
  TrueBB = new (Module) SILBasicBlock(StartBB->getParent());
  B.moveBlockTo(TrueBB, ContBB);
  B.setInsertionPoint(TrueBB);
  B.createBranch(Loc, ContBB);
  
  // If the client wanted a false BB, create it too.
  SILBasicBlock *FalseDest;
  if (!FalseBB) {
    FalseDest = ContBB;
  } else {
    FalseDest = new (Module) SILBasicBlock(StartBB->getParent());
    B.moveBlockTo(FalseDest, ContBB);
    B.setInsertionPoint(FalseDest);
    B.createBranch(Loc, ContBB);
    *FalseBB = FalseDest;
  }
  
  // Now that we have our destinations, insert a conditional branch on the
  // condition.
  B.setInsertionPoint(StartBB);
  B.createCondBranch(Loc, Cond, TrueBB, FalseDest);
  
  B.setInsertionPoint(ContBB, ContBB->begin());
}


//===----------------------------------------------------------------------===//
// Per-Element Promotion Logic
//===----------------------------------------------------------------------===//

namespace {
  enum class DIKind {
    No,
    Yes,
    Partial
  };
}

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
  
  inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                       const AvailabilitySet &AS) {
    AS.dump(OS);
    return OS;
  }
}


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
    }

    /// Merge the state from a predecessor block into the OutAvailability.
    /// Returns true if the l
    bool mergeFromPred(const LiveOutBlockState &Pred) {
      bool changed = false;
      for (unsigned i = 0, e = OutAvailability.size(); i != e; ++i) {
        const Optional<DIKind> out = OutAvailability.getConditional(i);
        const Optional<DIKind> local = LocalAvailability.getConditional(i);
        Optional<DIKind> result;
        if (local.hasValue()) {
          // A local availibility overrides the incoming value.
          result = local;
        } else {
          result = mergeKinds(out, Pred.OutAvailability.getConditional(i));
        }
        if (result.hasValue() &&
            (!out.hasValue() || result.getValue() != out.getValue())) {
          changed = true;
          OutAvailability.set(i, result);
        }
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
    SmallVectorImpl<SILInstruction*> &Releases;
    std::vector<std::pair<unsigned, AvailabilitySet>> ConditionalDestroys;

    llvm::SmallDenseMap<SILBasicBlock*, LiveOutBlockState, 32> PerBlockInfo;

    /// This is a map of uses that are not loads (i.e., they are Stores,
    /// InOutUses, and Escapes), to their entry in Uses.
    llvm::SmallDenseMap<SILInstruction*, unsigned, 16> NonLoadUses;

    /// This is true when there is an ambiguous store, which may be an init or
    /// assign, depending on the CFG path.
    bool HasConditionalInitAssignOrDestroys = false;
    
    // Keep track of whether we've emitted an error.  We only emit one error per
    // location as a policy decision.
    std::vector<SourceLoc> EmittedErrorLocs;
    SmallPtrSet<SILBasicBlock*, 16> BlocksReachableFromEntry;
    
  public:
    LifetimeChecker(const DIMemoryObjectInfo &TheMemory,
                    SmallVectorImpl<DIMemoryUse> &Uses,
                    SmallVectorImpl<SILInstruction*> &Releases);

    void doIt();

  private:

    LiveOutBlockState &getBlockInfo(SILBasicBlock *BB) {
      return PerBlockInfo.insert({BB,
                     LiveOutBlockState(TheMemory.NumElements)}).first->second;
    }
    
    AvailabilitySet getLivenessAtInst(SILInstruction *Inst, unsigned FirstElt,
                                      unsigned NumElts);

    bool isInitializedAtUse(const DIMemoryUse &Use, bool *SuperInitDone = 0);

    void handleStoreUse(unsigned UseID);
    void handleInOutUse(const DIMemoryUse &Use);

    void handleLoadUseFailure(const DIMemoryUse &InstInfo,
                              bool IsSuperInitComplete);
    void handleSuperInitUse(const DIMemoryUse &InstInfo);
    void handleSelfInitUse(DIMemoryUse &InstInfo);
    void updateInstructionForInitState(DIMemoryUse &InstInfo);


    void processNonTrivialRelease(unsigned ReleaseID);

    SILValue handleConditionalInitAssign();
    void handleConditionalDestroys(SILValue ControlVariableAddr);

    typedef SmallVector<SILBasicBlock *, 16> WorkListType;
    void putIntoWorkList(SILBasicBlock *BB, WorkListType &WorkList);
    void getPredsLiveOut(SILBasicBlock *BB, AvailabilitySet &Result);

    bool shouldEmitError(SILInstruction *Inst);
    std::string getUninitElementName(const DIMemoryUse &Use);
    void noteUninitializedMembers(const DIMemoryUse &Use);
    void diagnoseInitError(const DIMemoryUse &Use, Diag<StringRef> DiagMessage);
    
    bool isBlockIsReachableFromEntry(SILBasicBlock *BB);
  };
} // end anonymous namespace


LifetimeChecker::LifetimeChecker(const DIMemoryObjectInfo &TheMemory,
                                 SmallVectorImpl<DIMemoryUse> &Uses,
                                 SmallVectorImpl<SILInstruction*> &Releases)
  : Module(TheMemory.MemoryInst->getModule()), TheMemory(TheMemory), Uses(Uses),
    Releases(Releases) {

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
  for (auto L : EmittedErrorLocs)
    if (L == Inst->getLoc().getSourceLoc())
      return false;

  EmittedErrorLocs.push_back(Inst->getLoc().getSourceLoc());
  return true;
}


/// Emit notes for each uninitialized stored property in a designated
/// initializer.
void LifetimeChecker::noteUninitializedMembers(const DIMemoryUse &Use) {
  assert(TheMemory.isAnyInitSelf() && !TheMemory.isDelegatingInit() &&
         "Not an designated initializer");

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
  AvailabilitySet Liveness =
    getLivenessAtInst(Use.Inst, Use.FirstElement, Use.NumElements);

  unsigned FirstUndefElement = Use.FirstElement;
  while (Liveness.get(FirstUndefElement) == DIKind::Yes) {
    ++FirstUndefElement;
    assert(FirstUndefElement != Use.FirstElement+Use.NumElements &&
           "No undef elements found?");
  }

  // Verify that it isn't the super.init marker that failed.  The client should
  // handle this, not pass it down to diagnoseInitError.
  assert((!TheMemory.isDerivedClassSelf() ||
          FirstUndefElement != TheMemory.NumElements-1) &&
         "super.init failure not handled in the right place");

  // If the definition is a declaration, try to reconstruct a name and
  // optionally an access path to the uninitialized element.
  //
  // TODO: Given that we know the range of elements being accessed, we don't
  // need to go all the way deep into a recursive tuple here.  We could print
  // an error about "v" instead of "v.0" when "v" has tuple type and the whole
  // thing is accessed inappropriately.
  std::string Name;
  TheMemory.getPathStringToElement(FirstUndefElement, Name);

  return Name;
}

void LifetimeChecker::diagnoseInitError(const DIMemoryUse &Use,
                                        Diag<StringRef> DiagMessage) {
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

  diagnose(Module, DiagLoc, DiagMessage, Name);

  // As a debugging hack, print the instruction itself if there is no location
  // information.  This should never happen.
  if (Inst->getLoc().isNull())
    llvm::dbgs() << "  the instruction: " << *Inst << "\n";

  // Provide context as note diagnostics.

  // TODO: The QoI could be improved in many different ways here.  For example,
  // We could give some path information where the use was uninitialized, like
  // the static analyzer.
  if (!TheMemory.isAnyInitSelf())
    diagnose(Module, TheMemory.getLoc(), diag::variable_defined_here);
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

    case DIUseKind::IndirectIn:
    case DIUseKind::Load: {
      bool IsSuperInitComplete;
      // If the value is not definitively initialized, emit an error.
      if (!isInitializedAtUse(Use, &IsSuperInitComplete))
        handleLoadUseFailure(Use, IsSuperInitComplete);
      break;
    }

    case DIUseKind::InOutUse:
      handleInOutUse(Use);
      break;

    case DIUseKind::Escape:
      if (!isInitializedAtUse(Use)) {
        Diag<StringRef> DiagMessage;

        // This is a use of an uninitialized value.  Emit a diagnostic.
        if (TheMemory.isDelegatingInit()) {
          DiagMessage = diag::self_use_before_init_in_delegatinginit;
          
          // If this is a load with a single user that is a return, then this is
          // a return before self.init.   Emit a specific diagnostic.
          if (auto *LI = dyn_cast<LoadInst>(Inst))
            if (LI->hasOneUse() &&
                isa<ReturnInst>((*LI->use_begin())->getUser())) {
              if (shouldEmitError(Inst))
                diagnose(Module, Inst->getLoc(),
                         diag::return_from_init_without_self_init);
              break;
            }
          if (isa<ReturnInst>(Inst)) {
            if (shouldEmitError(Inst))
              diagnose(Module, Inst->getLoc(),
                       diag::return_from_init_without_self_init);
            break;
          }
        } else if (isa<ApplyInst>(Inst) && TheMemory.isStructInitSelf()) {
          if (shouldEmitError(Inst)) {
            diagnose(Module, Inst->getLoc(),
                     diag::use_of_self_before_fully_init);
            noteUninitializedMembers(Use);
          }
          break;
        } else if (isa<MarkFunctionEscapeInst>(Inst))
          DiagMessage = diag::global_variable_function_use_uninit;
        else if (isa<AddressToPointerInst>(Inst))
          DiagMessage = diag::variable_addrtaken_before_initialized;
        else
          DiagMessage = diag::variable_escape_before_initialized;

        diagnoseInitError(Use, DiagMessage);
      }
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
  if (HasConditionalInitAssignOrDestroys)
    ControlVariable = handleConditionalInitAssign();
  if (!ConditionalDestroys.empty())
    handleConditionalDestroys(ControlVariable);
}

void LifetimeChecker::handleStoreUse(unsigned UseID) {
  DIMemoryUse &InstInfo = Uses[UseID];

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

      std::string PropertyName;
      auto *VD = TheMemory.getPathStringToElement(i, PropertyName);
      diagnose(Module, InstInfo.Inst->getLoc(),
               diag::immutable_property_already_initialized, PropertyName);
      if (auto *Var = dyn_cast<VarDecl>(VD))
        if (Var->getParentInitializer())
          diagnose(Module, SILLocation(VD),
                   diag::initial_value_provided_in_let_decl);
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
      HasConditionalInitAssignOrDestroys = true;
    return;
  }
  
  // Otherwise, we have a definite init or assign.  Make sure the instruction
  // itself is tagged properly.
  updateInstructionForInitState(InstInfo);
}

void LifetimeChecker::handleInOutUse(const DIMemoryUse &Use) {
  // inout uses are generally straight-forward: the memory must be initialized
  // before the "address" is passed as an l-value.
  if (!isInitializedAtUse(Use)) {
    diagnoseInitError(Use, diag::variable_inout_before_initialized);
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
      if (auto *FRI = dyn_cast<FunctionRefInst>(Apply->getOperand(0))) {
        if (FRI->getReferencedFunction()->hasLocation()) {
          auto SILLoc = FRI->getReferencedFunction()->getLocation();
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
///   dealloc_stack %1#0 : $*@local_storage Enum      // id: %9
///   return %8 : $Optional<Enum>                     // id: %10
///
static bool isFailableInitReturnUseOfEnum(EnumInst *EI) {
  // Only allow enums forming an optional.
  if (!EI->getType().getSwiftRValueType()->getOptionalObjectType())
    return false;

  if (!EI->hasOneUse()) return false;
  auto *BI = dyn_cast<BranchInst>(EI->use_begin()->getUser());
  if (!BI || BI->getNumArgs() != 1) return false;

  auto *TargetArg = BI->getDestBB()->getBBArg(0);
  if (!TargetArg->hasOneUse()) return false;
  return isa<ReturnInst>(TargetArg->use_begin()->getUser());
}

/// handleLoadUseFailure - Check and diagnose various failures when a load use
/// is not fully initialized.
///
/// TODO: In the "No" case, we can emit a fixit adding a default
/// initialization of the type.
///
void LifetimeChecker::handleLoadUseFailure(const DIMemoryUse &Use,
                                           bool IsSuperInitComplete) {
  SILInstruction *Inst = Use.Inst;

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
    
    if (hasReturnUse && !hasUnknownUses) {
      if (TheMemory.isEnumInitSelf()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, Inst->getLoc(),
                 diag::return_from_init_without_initing_self);
        return;
      } else if (TheMemory.isAnyInitSelf() && !TheMemory.isClassInitSelf() &&
                 !TheMemory.isDelegatingInit()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, Inst->getLoc(),
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
        CA->getFunction()->getArgument(0) && CA->getDest()) {
      if (TheMemory.isEnumInitSelf()) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, Inst->getLoc(),
                 diag::return_from_init_without_initing_self);
        return;
      } else if (TheMemory.isAnyInitSelf() && !TheMemory.isClassInitSelf() &&
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
    if (!IsSuperInitComplete) {
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
  FuncDecl *Method = nullptr;

  // Check for an access to the base class through an Upcast.
  if (auto UCI = dyn_cast<UpcastInst>(Inst)) {
    // If the upcast is used by a ref_element_addr, then it is an access to a
    // base ivar before super.init is called.
    if (UCI->hasOneUse() && !IsSuperInitComplete) {
      if (auto *REI =
          dyn_cast<RefElementAddrInst>((*UCI->use_begin())->getUser())) {
        if (!shouldEmitError(Inst)) return;
        diagnose(Module, Inst->getLoc(),
                 diag::self_use_before_fully_init,
                 REI->getField()->getName(), true, true);
        return;
      }
    }

    // If the upcast is used by a class_method + apply, then this is a call of
    // a superclass method or property accessor.
    ClassMethodInst *CMI = nullptr;
    ApplyInst *AI = nullptr;
    for (auto UI : SILValue(UCI, 0).getUses()) {
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

      // Not a pattern we recognize, conservatively generate a generic
      // diagnostic.
      CMI = nullptr;
      break;
    }

    if (AI && CMI) {
      // TODO: Could handle many other members more specifically.
      auto *Decl = CMI->getMember().getDecl();
      Method = dyn_cast<FuncDecl>(Decl);
    }
  }

  // If this is an apply instruction and we're in an class initializer, we're
  // calling a method on self.
  if (isa<ApplyInst>(Inst) && TheMemory.isClassInitSelf()) {
    // If this is a method application, produce a nice, specific, error.
    if (auto *CMI = dyn_cast<ClassMethodInst>(Inst->getOperand(0)))
      Method = dyn_cast<FuncDecl>(CMI->getMember().getDecl());

    // If this is a direct/devirt method application, check the location info.
    if (auto *FRI = dyn_cast<FunctionRefInst>(Inst->getOperand(0))) {
      if (FRI->getReferencedFunction()->hasLocation()) {
        auto SILLoc = FRI->getReferencedFunction()->getLocation();
        Method = SILLoc.getAsASTNode<FuncDecl>();
      }
    }
  }

  // If we were able to find a method call, emit a diagnostic about the method.
  if (Method) {
    Identifier Name;
    if (Method->isAccessor())
      Name = Method->getAccessorStorageDecl()->getName();
    else
      Name = Method->getName();

    // If this is a use of self before super.init was called, emit a diagnostic
    // about *that* instead of about individual properties not being
    // initialized.
    if (!shouldEmitError(Inst)) return;
    diagnose(Module, Inst->getLoc(), diag::self_use_before_fully_init,
             Name, Method->isAccessor(), !IsSuperInitComplete);

    if (IsSuperInitComplete)
      noteUninitializedMembers(Use);
    return;
  }

  // Otherwise, we couldn't find a specific thing to complain about, so emit a
  // generic error, depending on what kind of failure this is.
  if (!IsSuperInitComplete) {
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

  // If this is a load of self in a struct/enum initializer, then it must be a
  // use of 'self' before all the stored properties are set up.
  if (isa<LoadInst>(Inst) && TheMemory.isAnyInitSelf() &&
      !TheMemory.isClassInitSelf()) {
    if (!shouldEmitError(Inst)) return;
    diagnose(Module, Inst->getLoc(), diag::use_of_self_before_fully_init);
    noteUninitializedMembers(Use);
    return;
  }

  diagnoseInitError(Use, diag::variable_used_before_initialized);
}

/// handleSuperInitUse - When processing a 'self' argument on a class, this is
/// a call to super.init.
void LifetimeChecker::handleSuperInitUse(const DIMemoryUse &InstInfo) {
  auto *Inst = cast<ApplyInst>(InstInfo.Inst);

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

/// handleSuperInitUse - When processing a 'self' argument on a class, this is
/// a call to self.init.
void LifetimeChecker::handleSelfInitUse(DIMemoryUse &InstInfo) {
  auto *Inst = InstInfo.Inst;

  assert(TheMemory.NumElements == 1 && "delegating inits have a single elt");

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
  
  // If this is an assign, rewrite it based on whether it is an initialization
  // or not.
  if (auto *AI = dyn_cast<AssignInst>(Inst)) {
    // Remove this instruction from our data structures, since we will be
    // removing it.
    auto Kind = InstInfo.Kind;
    InstInfo.Inst = nullptr;
    NonLoadUses.erase(Inst);

    unsigned FirstElement = InstInfo.FirstElement;
    unsigned NumElements = InstInfo.NumElements;

    SmallVector<SILInstruction*, 4> InsertedInsts;
    SILBuilder B(Inst, &InsertedInsts);

    LowerAssignInstruction(B, AI, InitKind);

    // If lowering of the assign introduced any new loads or stores, keep track
    // of them.
    for (auto I : InsertedInsts) {
      I->setDebugScope(AI->getDebugScope());
      if (isa<StoreInst>(I)) {
        NonLoadUses[I] = Uses.size();
        Uses.push_back(DIMemoryUse(I, Kind, FirstElement, NumElements));
      } else if (isa<LoadInst>(I)) {
        Uses.push_back(DIMemoryUse(I, Load, FirstElement, NumElements));
      }
    }
    return;
  }

  // Ignore non-stores for SelfInits.
  assert(isa<StoreInst>(Inst) && "Unknown store instruction!");
}

/// processNonTrivialRelease - We handle two kinds of release instructions here:
/// destroy_addr for alloc_stack's and strong_release/dealloc_box for
/// alloc_box's.  By the  time that DI gets here, we've validated that all uses
/// of the memory location are valid.  Unfortunately, the uses being valid
/// doesn't mean that the memory is actually initialized on all paths leading to
/// a release.  As such, we have to push the releases up the CFG to where the
/// value is initialized.
///
/// This returns true if the release was deleted.
///
void LifetimeChecker::processNonTrivialRelease(unsigned ReleaseID) {
  SILInstruction *Release = Releases[ReleaseID];
  
  // If the instruction is a deallocation of uninitialized memory, no action is
  // required (or desired).
  if (isa<DeallocStackInst>(Release) || isa<DeallocBoxInst>(Release) ||
      isa<DeallocRefInst>(Release))
    return;
  
  // We only handle strong_release and destroy_addr here.  The former is a
  // release of a class in an initializer, the later is used for local variable
  // destruction.
  assert(isa<StrongReleaseInst>(Release) || isa<DestroyAddrInst>(Release));

  // If the memory object is completely initialized, then nothing needs to be
  // done at this release point.
  AvailabilitySet Availability =
    getLivenessAtInst(Release, 0, TheMemory.NumElements);
  if (Availability.isAllYes()) return;


  // Right now we don't fully support cleaning up a partially initialized object
  // after a failure.  Handle this by only allowing an early 'return nil' in an
  // initializer after all properties are initialized.
  if (TheMemory.isClassInitSelf()) {
    // If this is a convenience initializer, report that self.init() must be
    // called.
    if (TheMemory.isDelegatingInit()) {
      diagnose(Module, Release->getLoc(),
               diag::self_init_must_be_called_before_failure);
    } else {
      // Otherwise all members must be initialized (including the base class, if
      // present).
      diagnose(Module, Release->getLoc(),
               diag::object_not_fully_initialized_before_failure);
      
      // Note each of the members that isn't initialized.
      DIMemoryUse Use(Release, DIUseKind::Load, 0, TheMemory.NumElements);
      noteUninitializedMembers(Use);
      
      // If we're tracking the state of super.init (i.e., in a derived class)
      // then report on failure to call super.init as well.
      if (TheMemory.isAnyDerivedClassSelf() &&
          Availability.get(Availability.size()-1) != DIKind::Yes)
        diagnose(Module, Release->getLoc(),
                 diag::must_call_super_init_failable_init);
    }
  }

  // If it is all 'no' then we can handle is specially without conditional code.
  if (Availability.isAllNo()) {
    // If this is an early release in a class, we need to emit a dealloc_ref to
    // free the memory.  If this is a derived class, we may have to do a load of
    // the 'self' box to get the class reference.
    if (TheMemory.isClassInitSelf()) {
      SILBuilderWithScope<4> B(Release);
      SILValue Pointer = Release->getOperand(0);

      // If we see an alloc_box as the pointer, then we're deallocating a 'box'
      // for self.  Make sure we're using its address result, not its refcount
      // result, and make sure that the box gets deallocated (not released)
      // since the pointer it contains will be manually cleaned up.
      if (isa<AllocBoxInst>(Pointer))
        Pointer = SILValue(Pointer.getDef(), 1);

      if (Pointer.getType().isAddress())
        Pointer = B.createLoad(Release->getLoc(), Pointer);
      auto Dealloc = B.createDeallocRef(Release->getLoc(), Pointer);
      
      // dealloc_box the self box is necessary.
      if (isa<AllocBoxInst>(Release->getOperand(0))) {
        auto DB = B.createDeallocBox(Release->getLoc(), Pointer.getType(),
                                     Release->getOperand(0));
        Releases.push_back(DB);
      }

      Releases[ReleaseID] = Dealloc;
      Release->eraseFromParent();
      return;
    }
    
    // Otherwise, in the normal case, the destroy_addr can just be zapped.
    assert(isa<DestroyAddrInst>(Release));
    SILValue Addr = Release->getOperand(0);
    Release->eraseFromParent();
    if (auto *AddrI = dyn_cast<SILInstruction>(Addr))
      recursivelyDeleteTriviallyDeadInstructions(AddrI);
    Releases[ReleaseID] = nullptr;
    return;
  }
  
  // If any elements are partially live, we need to emit conditional logic.
  if (Availability.hasAny(DIKind::Partial))
    HasConditionalInitAssignOrDestroys = true;

  // Otherwise, it is conditionally live, safe it for later processing.
  ConditionalDestroys.push_back({ ReleaseID, Availability });
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

/// handleConditionalInitAssign - This memory object has some stores
/// into (some element of) it that is either an init or an assign based on the
/// control flow path through the function, or have a destroy event that happens
/// when the memory object may or may not be initialized.  Handle this by
/// inserting a bitvector that tracks the liveness of each tuple element
/// independently.
SILValue LifetimeChecker::handleConditionalInitAssign() {
  SILLocation Loc = TheMemory.getLoc();
  Loc.markAutoGenerated();

  unsigned NumMemoryElements = TheMemory.getNumMemoryElements();

  // Create the control variable as the first instruction in the function (so
  // that it is easy to destroy the stack location.
  SILBuilderWithScope<16> B(TheMemory.getFunctionEntryPoint(),
                            TheMemory.getFunction().getDebugScope());
  SILType IVType =
    SILType::getBuiltinIntegerType(NumMemoryElements, Module.getASTContext());
  auto Alloc = B.createAllocStack(Loc, IVType);
  
  // Find all the return blocks in the function, inserting a dealloc_stack
  // before the return.
  for (auto &BB : TheMemory.getFunction()) {
    if (auto *RI = dyn_cast<ReturnInst>(BB.getTerminator())) {
      B.setInsertionPoint(RI);
      B.createDeallocStack(Loc, SILValue(Alloc, 0));
    }
  }
  
  // Before the memory allocation, store zero in the control variable.
  B.setInsertionPoint(TheMemory.MemoryInst->getNextNode());
  SILValue AllocAddr = SILValue(Alloc, 1);
  auto Zero = B.createIntegerLiteral(Loc, IVType, 0);
  B.createStore(Loc, Zero, AllocAddr);
  
  Identifier OrFn;

  // At each initialization, mark the initialized elements live.  At each
  // conditional assign, resolve the ambiguity by inserting a CFG diamond.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &Use = Uses[i];
    
    // Ignore deleted uses.
    if (Use.Inst == nullptr) continue;
    
    // Only full initializations make something live.  inout uses, escapes, and
    // assignments only happen when some kind of init made the element live.
    switch (Use.Kind) {
    default:
      // We can ignore most use kinds here.
      continue;
    case DIUseKind::InitOrAssign:
      // The dynamically unknown case is the interesting one, handle it below.
      break;

    case DIUseKind::Initialization:
      // If this is an initialization of only trivial elements, then we don't
      // need to update the bitvector.
      if (Use.onlyTouchesTrivialElements(TheMemory))
        continue;
      
      // Get the integer constant.
      B.setInsertionPoint(Use.Inst);
      APInt Bitmask = Use.getElementBitmask(NumMemoryElements);
      SILValue MaskVal = B.createIntegerLiteral(Loc, IVType, Bitmask);

      // If the mask is all ones, do a simple store, otherwise do a
      // load/or/store sequence to mask in the bits.
      if (!Bitmask.isAllOnesValue()) {
        SILValue Tmp = B.createLoad(Loc, AllocAddr);
        if (!OrFn.get())
          OrFn = getBinaryFunction("or", Tmp.getType(), B.getASTContext());
        
        SILValue Args[] = { Tmp, MaskVal };
        MaskVal = B.createBuiltin(Loc, OrFn, Tmp.getType(), {}, Args);
      }
      B.createStore(Loc, MaskVal, AllocAddr);
      continue;
    }
    
    // If this ambiguous store is only of trivial types, then we don't need to
    // do anything special.  We don't even need keep the init bit for the
    // element precise.
    if (Use.onlyTouchesTrivialElements(TheMemory))
      continue;
    
    B.setInsertionPoint(Use.Inst);

    // If this is the interesting case, we need to generate a CFG diamond for
    // each element touched, destroying any live elements so that the resulting
    // store is always an initialize.  This disambiguates the dynamic
    // uncertainty with a runtime check.
    SILValue Bitmask = B.createLoad(Loc, AllocAddr);
    
    // If we have multiple tuple elements, we'll have to do some shifting and
    // truncating of the mask value.  These values cache the function_ref so we
    // don't emit multiple of them.
    Identifier ShiftRightFn, TruncateFn;
    
    // If the memory object has multiple tuple elements, we need to destroy any
    // live subelements, since they can each be in a different state of
    // initialization.
    for (unsigned Elt = Use.FirstElement, e = Elt+Use.NumElements;
         Elt != e; ++Elt) {
      B.setInsertionPoint(Use.Inst);
      SILValue CondVal = Bitmask;
      if (NumMemoryElements != 1) {
        // Shift the mask down to this element.
        if (Elt != 0) {
          if (!ShiftRightFn.get())
            ShiftRightFn = getBinaryFunction("lshr", Bitmask.getType(),
                                             B.getASTContext());
          SILValue Amt = B.createIntegerLiteral(Loc, Bitmask.getType(), Elt);
          SILValue Args[] = { CondVal, Amt };
          CondVal = B.createBuiltin(Loc, ShiftRightFn, Bitmask.getType(),
          {}, Args);
        }
        
        if (!TruncateFn.get())
          TruncateFn = getTruncateToI1Function(Bitmask.getType(),
                                               B.getASTContext());
        CondVal = B.createBuiltin(Loc, TruncateFn,
                                  SILType::getBuiltinIntegerType(1,
                                                             B.getASTContext()),
                                  {}, CondVal);
      }
      
      SILBasicBlock *TrueBB, *ContBB;
      InsertCFGDiamond(CondVal, Loc, B, TrueBB, nullptr, ContBB);

      // Emit a destroy_addr in the taken block.
      B.setInsertionPoint(TrueBB->begin());
      SILValue EltPtr = TheMemory.emitElementAddress(Elt, Loc, B);
      if (auto *DA = B.emitDestroyAddr(Loc, EltPtr))
        Releases.push_back(DA);
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

  return AllocAddr;
}

/// Process any destroy_addr and strong_release instructions that are invoked on
/// a partially initialized value.  This generates code to destroy the elements
/// that are known to be alive, ignore the ones that are known to be dead, and
/// to emit branching logic when an element may or may not be initialized.
void LifetimeChecker::
handleConditionalDestroys(SILValue ControlVariableAddr) {
  SILBuilderWithScope<16> B(TheMemory.MemoryInst);
  Identifier ShiftRightFn, TruncateFn;

  unsigned NumMemoryElements = TheMemory.getNumMemoryElements();

  // After handling any conditional initializations, check to see if we have any
  // cases where the value is only partially initialized by the time its
  // lifetime ends.  In this case, we have to make sure not to destroy an
  // element that wasn't initialized yet.
  for (auto &CDElt : ConditionalDestroys) {
    auto *Release = Releases[CDElt.first];
    auto Loc = Release->getLoc();
    auto &Availability = CDElt.second;
    
    // The instruction in a partially live region is a destroy_addr or
    // strong_release.
    SILValue Addr = Release->getOperand(0);
  
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
    SILValue LoadedMask;
    for (unsigned Elt = 0; Elt != NumMemoryElements; ++Elt) {
      switch (Availability.get(Elt)) {
      case DIKind::No:
        // If an element is known to be uninitialized, then we know we can
        // completely ignore it.
        continue;
      case DIKind::Partial:
        // In the partially live case, we have to check our control variable to
        // destroy it.  Handle this below.
        break;

      case DIKind::Yes:
        // If an element is known to be initialized, then we can strictly
        // destroy its value at releases position.
        B.setInsertionPoint(Release);
        SILValue EltPtr = TheMemory.emitElementAddress(Elt, Loc, B);
        if (auto *DA = B.emitDestroyAddr(Release->getLoc(), EltPtr))
          Releases.push_back(DA);
        continue;
      }
      
      // Note - in some partial liveness cases, we can push the destroy_addr up
      // the CFG, instead of immediately generating dynamic control flow checks.
      // This could be handled in processNonTrivialRelease some day.
      
      // Insert a load of the liveness bitmask and split the CFG into a diamond
      // right before the destroy_addr, if we haven't already loaded it.
      B.setInsertionPoint(Release);
      if (!LoadedMask)
        LoadedMask = B.createLoad(Loc, ControlVariableAddr);
      SILValue CondVal = LoadedMask;
      
      // If this memory object has multiple tuple elements, we need to make sure
      // to test the right one.
      if (NumMemoryElements != 1) {
        // Shift the mask down to this element.
        if (Elt != 0) {
          if (!ShiftRightFn.get())
            ShiftRightFn = getBinaryFunction("lshr", CondVal.getType(),
                                             B.getASTContext());
          SILValue Amt = B.createIntegerLiteral(Loc, CondVal.getType(), Elt);
          SILValue Args[] = { CondVal, Amt };
          
          CondVal = B.createBuiltin(Loc, ShiftRightFn,
                                    CondVal.getType(), {},
                                    Args);
        }
        
        if (!TruncateFn.get())
          TruncateFn = getTruncateToI1Function(CondVal.getType(),
                                               B.getASTContext());
        CondVal = B.createBuiltin(Loc, TruncateFn,
                                  SILType::getBuiltinIntegerType(1,
                                                             B.getASTContext()),
                                  {}, CondVal);
      }
      
      SILBasicBlock *CondDestroyBlock, *ContBlock;
      InsertCFGDiamond(CondVal, Loc, B, CondDestroyBlock, nullptr, ContBlock);
      
      // Set up the conditional destroy block.
      B.setInsertionPoint(CondDestroyBlock->begin());
      SILValue EltPtr = TheMemory.emitElementAddress(Elt, Loc, B);
      if (auto *DA = B.emitDestroyAddr(Loc, EltPtr))
        Releases.push_back(DA);
    }
    
    // If this is an early release in a class, we need to emit a dealloc_ref to
    // free the memory.  If this is a derived class, we may have to do a load of
    // the 'self' box to get the class reference.
    if (TheMemory.isClassInitSelf()) {
      B.setInsertionPoint(Release);
      SILValue Pointer = Release->getOperand(0);
      
      // If we see an alloc_box as the pointer, then we're deallocating a 'box'
      // for self.  Make sure we're using its address result, not its refcount
      // result, and make sure that the box gets deallocated (not released)
      // since the pointer it contains will be manually cleaned up.
      if (isa<AllocBoxInst>(Pointer))
        Pointer = SILValue(Pointer.getDef(), 1);

      if (Pointer.getType().isAddress())
        Pointer = B.createLoad(Release->getLoc(), Pointer);
      B.createDeallocRef(Release->getLoc(), Pointer);
      
      // dealloc_box the self box is necessary.
      if (isa<AllocBoxInst>(Release->getOperand(0))) {
        auto DB = B.createDeallocBox(Release->getLoc(), Pointer.getType(),
                                     Release->getOperand(0));
        Releases.push_back(DB);
      }
    }
  
    // Finally, now that the original instruction is handled, remove the
    // original destroy.
    Release->eraseFromParent();
    if (auto *AddrI = dyn_cast<SILInstruction>(Addr))
      recursivelyDeleteTriviallyDeadInstructions(AddrI);
  }
}

void LifetimeChecker::
putIntoWorkList(SILBasicBlock *BB, WorkListType &WorkList) {
  LiveOutBlockState &State = getBlockInfo(BB);
  if (!State.isInWorkList && State.OutAvailability.containsUnknownElements()) {
    DEBUG(llvm::dbgs() << "    add block " << BB->getDebugID()
          << " to worklist\n");
    WorkList.push_back(BB);
    State.isInWorkList = true;
  }
}

void LifetimeChecker::
getPredsLiveOut(SILBasicBlock *BB, AvailabilitySet &Result) {
  DEBUG(llvm::dbgs() << "  Get liveness for block " << BB->getDebugID() << "\n");
  
  // Collect blocks for which we have to calculate the out-availability.
  // These are the pathes from blocks with known out-availability to the BB.
  WorkListType WorkList;
  for (auto Pred : BB->getPreds()) {
    putIntoWorkList(Pred, WorkList);
  }
  size_t idx = 0;
  while (idx < WorkList.size()) {
    SILBasicBlock *WorkBB = WorkList[idx++];
    for (auto Pred : WorkBB->getPreds()) {
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
      for (auto Pred : WorkBB->getPreds()) {
        changed |= BBState.mergeFromPred(getBlockInfo(Pred));
      }
      DEBUG(llvm::dbgs() << "      Block " << WorkBB->getDebugID() << " out: "
            << BBState.OutAvailability << "\n");

      // Clear the worklist-flag for the next call to getPredsLiveOut().
      // This could be moved out of the outer loop, but doing it here avoids
      // another loop with getBlockInfo() calls.
      BBState.isInWorkList = false;
    }
  } while (changed);

  // Finally merge to the result (= state at BB's entry).
  for (auto Pred : BB->getPreds()) {
    Result.mergeIn(getBlockInfo(Pred).OutAvailability);
  }
  DEBUG(llvm::dbgs() << "    Result: " << Result << "\n");

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
      for (SILBasicBlock::iterator BBI = Inst, E = InstBB->begin();
           BBI != E;) {
        SILInstruction *TheInst = --BBI;
        
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

    getPredsLiveOut(InstBB, Result);

    // If the result element wasn't computed, we must be analyzing code within
    // an unreachable cycle that is not dominated by "TheMemory".  Just force
    // the unset element to yes so that clients don't have to handle this.
    if (!Result.getConditional(0))
      Result.set(0, DIKind::Yes);

    return Result;
  }

  // Check locally to see if any elements are satified within the block, and
  // keep track of which ones are still needed in the NeededElements set.
  llvm::SmallBitVector NeededElements(TheMemory.NumElements);
  NeededElements.set(FirstElt, FirstElt+NumElts);
  
  // If there is a store in the current block, scan the block to see if the
  // store is before or after the load.  If it is before, it may produce some of
  // the elements we are looking for.
  if (getBlockInfo(InstBB).HasNonLoadUse) {
    for (SILBasicBlock::iterator BBI = Inst, E = InstBB->begin(); BBI != E;) {
      SILInstruction *TheInst = --BBI;

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
  getPredsLiveOut(InstBB, Result);
  
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

/// The specified instruction is a use of some number of elements.  Determine
/// whether all of the elements touched by the instruction are definitely
/// initialized at this point or not.
bool LifetimeChecker::isInitializedAtUse(const DIMemoryUse &Use,
                                         bool *SuperInitDone) {
  // Determine the liveness states of the elements that we care about.
  AvailabilitySet Liveness =
    getLivenessAtInst(Use.Inst, Use.FirstElement, Use.NumElements);

  // If the client wants to know about super.init, check to see if we failed
  // it or some other element.
  if (SuperInitDone) {
    *SuperInitDone = true;
    if (Use.FirstElement+Use.NumElements == TheMemory.NumElements &&
        TheMemory.isAnyDerivedClassSelf() &&
        Liveness.get(Liveness.size()-1) != DIKind::Yes)
      *SuperInitDone = false;
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
  SmallVector<SILInstruction*, 4> Releases;

  // Walk the use list of the pointer, collecting them into the Uses array.
  collectDIElementUsesFrom(MemInfo, Uses, Releases, false);

  LifetimeChecker(MemInfo, Uses, Releases).doIt();
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
      SILInstruction *Inst = I;
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
      SILInstruction *Inst = I++;
      
      // Unprocessed assigns just lower into assignments, not initializations.
      if (auto *AI = dyn_cast<AssignInst>(Inst)) {
        SILBuilderWithScope<4> B(AI);
        LowerAssignInstruction(B, AI, IsNotInitialization);
        // Assign lowering may split the block. If it did,
        // reset our iteration range to the block after the insertion.
        if (B.getInsertionBB() != &BB)
          I = E;
        Changed = true;
        continue;
      }

      // mark_uninitialized just becomes a noop, resolving to its operand.
      if (auto *MUI = dyn_cast<MarkUninitializedInst>(Inst)) {
        SILValue(MUI, 0).replaceAllUsesWith(MUI->getOperand());
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
    if (checkDefiniteInitialization(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::All);

    DEBUG(getFunction()->verify());

    // Lower raw-sil only instructions used by this pass, like "assign".
    if (lowerRawSILOperations(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::All);
  }

  StringRef getName() override { return "Definite Initialization"; }
};
} // end anonymous namespace

SILTransform *swift::createDefiniteInitialization() {
  return new DefiniteInitialization();
}
