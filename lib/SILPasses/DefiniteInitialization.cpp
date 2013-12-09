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
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
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
  DEBUG(llvm::errs() << "  *** Lowering [isInit=" << (bool)isInitialization
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

    B.emitDestroyValueOperation(Inst->getLoc(), IncomingVal);
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
        return V1 ? Optional<DIKind>(Nothing) : DIKind::No;
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
      for (unsigned i = 0, e = size(); i != e; ++i) {
        Optional<DIKind> RO = RHS.getConditional(i);
        
        // If RHS is unset, ignore it.
        if (!RO.hasValue())
          continue;

        DIKind R = RO.getValue();
        
        // If This is unset, take R
        Optional<DIKind> TO = getConditional(i);
        if (!TO.hasValue()) {
          set(i, R);
          continue;
        }
        DIKind T = TO.getValue();
        
        // If "this" is already partial, we won't learn anything.
        if (T == DIKind::Partial)
          continue;
        
        // If "T" is yes, or no, then switch to partial if we find a different
        // answer.
        if (T != R)
          set(i, DIKind::Partial);
      }
    }
  };
}


namespace {
  /// LiveOutBlockState - Keep track of information about blocks that have
  /// already been analyzed.  Since this is a global analysis, we need this to
  /// cache information about different paths through the CFG.
  struct LiveOutBlockState {
    /// Keep track of whether there is a Store, InOutUse, or Escape locally in
    /// this block.
    bool HasNonLoadUse : 1;

    /// Keep track of whether the element is live out of this block or not. This
    /// is only fully set when LOState==IsKnown.  In other states, this may only
    /// contain local availability information.
    ///
    AvailabilitySet Availability;

    enum LiveOutStateTy {
      IsUnknown,
      IsComputingLiveOut,
      IsKnown
    } LOState : 2;

    LiveOutBlockState(unsigned NumElements)
      : HasNonLoadUse(false),
        Availability(NumElements), LOState(IsUnknown) {
    }

    AvailabilitySet &getAvailabilitySet() {
      return Availability;
    }

    DIKind getAvailability(unsigned Elt) {
      return Availability.get(Elt);
    }

    Optional<DIKind> getAvailabilityConditional(unsigned Elt) {
      return Availability.getConditional(Elt);
    }

    void setBlockAvailability(const AvailabilitySet &AV) {
      assert(LOState != IsKnown &&"Changing live out state of computed block?");
      assert(!AV.containsUnknownElements() && "Set block to unknown value?");
      Availability = AV;
      LOState = LiveOutBlockState::IsKnown;
    }

    void setBlockAvailability1(DIKind K) {
      assert(LOState != IsKnown &&"Changing live out state of computed block?");
      assert(Availability.size() == 1 && "Not 1 element case");
      Availability.set(0, K);
      LOState = LiveOutBlockState::IsKnown;
    }

    void markAvailable(const DIMemoryUse &Use) {
      // If the memory object has nothing in it (e.g., is an empty tuple)
      // ignore.
      if (Availability.empty()) return;
      
      // Peel the first iteration of the 'set' loop since there is almost always
      // a single tuple element touched by a DIMemoryUse.
      Availability.set(Use.FirstElement, DIKind::Yes);
                         
      for (unsigned i = 1; i != Use.NumElements; ++i)
        Availability.set(Use.FirstElement+i, DIKind::Yes);
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
    
    /// The number of elements in this memory object.
    unsigned NumElements;
    
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
    std::vector<SILLocation> EmittedErrorLocs;
  public:
    LifetimeChecker(const DIMemoryObjectInfo &TheMemory,
                    SmallVectorImpl<DIMemoryUse> &Uses,
                    SmallVectorImpl<SILInstruction*> &Releases);

    void doIt();

  private:

    LiveOutBlockState &getBlockInfo(SILBasicBlock *BB) {
      return PerBlockInfo.insert({BB,
                                 LiveOutBlockState(NumElements)}).first->second;
    }
    
    AvailabilitySet getLivenessAtInst(SILInstruction *Inst, unsigned FirstElt,
                                      unsigned NumElts);

    DIKind getLivenessAtUse(const DIMemoryUse &Use);

    void handleStoreUse(unsigned UseID);
    void handleSuperclassUse(unsigned UseID);
    void updateInstructionForInitState(DIMemoryUse &InstInfo);


    void processNonTrivialRelease(unsigned ReleaseID);

    SILValue handleConditionalInitAssign();
    void handleConditionalDestroys(SILValue ControlVariableAddr);

    Optional<DIKind> getLiveOut1(SILBasicBlock *BB);
    void getPredsLiveOut1(SILBasicBlock *BB, Optional<DIKind> &Result);
    AvailabilitySet getLiveOutN(SILBasicBlock *BB);
    void getPredsLiveOutN(SILBasicBlock *BB, AvailabilitySet &Result);

    bool shouldEmitError(SILInstruction *Inst);
    void diagnoseInitError(const DIMemoryUse &Use, Diag<StringRef> DiagMessage);
  };
} // end anonymous namespace


LifetimeChecker::LifetimeChecker(const DIMemoryObjectInfo &TheMemory,
                                 SmallVectorImpl<DIMemoryUse> &Uses,
                                 SmallVectorImpl<SILInstruction*> &Releases)
  : Module(TheMemory.MemoryInst->getModule()), TheMemory(TheMemory), Uses(Uses),
    Releases(Releases) {

  NumElements = TheMemory.getElementCount();

  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (unsigned ui = 0, e = Uses.size(); ui != e; ++ui) {
    auto &Use = Uses[ui];
    assert(Use.Inst && "No instruction identified?");

    // Keep track of all the uses that aren't loads.
    if (Use.Kind == DIUseKind::Load)
      continue;

    NonLoadUses[Use.Inst] = ui;

    auto &BBInfo = getBlockInfo(Use.Inst->getParent());
    BBInfo.HasNonLoadUse = true;

    // Each of the non-load instructions will each be checked to make sure that
    // they are live-in or a full element store.  This means that the block they
    // are in should be treated as a live out for cross-block analysis purposes.
    BBInfo.markAvailable(Use);
    
    // If all of the tuple elements are available in the block, then it is known
    // to be live-out.  This is the norm for non-tuple memory objects.
    if (BBInfo.getAvailabilitySet().isAllYes())
      BBInfo.LOState = LiveOutBlockState::IsKnown;
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
  MemBBInfo.getAvailabilitySet().changeUnsetElementsTo(DIKind::No);
    
  MemBBInfo.LOState = LiveOutBlockState::IsKnown;
}

/// shouldEmitError - Check to see if we've already emitted an error at the
/// specified instruction.  If so, return false.  If not, remember the
/// instruction and return true.
bool LifetimeChecker::shouldEmitError(SILInstruction *Inst) {
  // Check to see if we've already emitted an error at this location.  If so,
  // swallow the error.
  for (auto L : EmittedErrorLocs)
    if (L == Inst->getLoc())
      return false;
  EmittedErrorLocs.push_back(Inst->getLoc());
  return true;
}

void LifetimeChecker::diagnoseInitError(const DIMemoryUse &Use,
                                        Diag<StringRef> DiagMessage) {
  auto *Inst = Use.Inst;

  if (!shouldEmitError(Inst))
    return;

  // If the definition is a declaration, try to reconstruct a name and
  // optionally an access path to the uninitialized element.
  std::string Name;
  if (ValueDecl *VD =
        dyn_cast_or_null<ValueDecl>(TheMemory.getLoc().getAsASTNode<Decl>()))
    Name = VD->getName().str();
  else
    Name = "<unknown>";

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

  // TODO: Given that we know the range of elements being accessed, we don't
  // need to go all the way deep into a recursive tuple here.  We could print
  // an error about "v" instead of "v.0" when "v" has tuple type and the whole
  // thing is accessed inappropriately.
  TheMemory.getPathStringToElement(FirstUndefElement, Name);

  diagnose(Module, Inst->getLoc(), DiagMessage, Name);

  // As a debugging hack, print the instruction itself if there is no location
  // information.  This should never happen.
  if (Inst->getLoc().isNull())
    llvm::errs() << "  the instruction: " << *Inst << "\n";

  // Provide context as note diagnostics.

  // TODO: The QoI could be improved in many different ways here.  For example,
  // We could give some path information where the use was uninitialized, like
  // the static analyzer.
  if (!TheMemory.IsSelfOfInitializer)
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
      // initializations of variables, because it only produces them when it knows
      // they are correct, and this is a super common case for "var x = y" cases.
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
    case DIUseKind::Load:
      // If the value is not definitively initialized, emit an error.
      // TODO: In the "No" case, we can emit a fixit adding a default
      // initialization of the type.
      // TODO: In the "partial" case, we can produce a more specific diagnostic
      // indicating where the control flow merged.
      if (getLivenessAtUse(Use) != DIKind::Yes) {
        // Otherwise, this is a use of an uninitialized value.  Emit a
        // diagnostic.
        diagnoseInitError(Use, diag::variable_used_before_initialized);
      }
      break;
      
    case DIUseKind::InOutUse:
      if (getLivenessAtUse(Use) != DIKind::Yes) {
        // This is a use of an uninitialized value.  Emit a diagnostic.
        diagnoseInitError(Use, diag::variable_inout_before_initialized);
      }
      break;
      
    case DIUseKind::Escape:
      if (getLivenessAtUse(Use) != DIKind::Yes) {
        Diag<StringRef> DiagMessage;

        // This is a use of an uninitialized value.  Emit a diagnostic.
        if (isa<MarkFunctionEscapeInst>(Inst))
          DiagMessage = diag::global_variable_function_use_uninit;
        else
          DiagMessage = diag::variable_escape_before_initialized;

        diagnoseInitError(Use, DiagMessage);
      }
      break;
    case DIUseKind::Superclass:
      handleSuperclassUse(i);
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
  DIKind DI = getLivenessAtUse(InstInfo);

  // If this is a partial store into a struct and the whole struct hasn't been
  // initialized, diagnose this as an error.
  if (InstInfo.Kind == DIUseKind::PartialStore && DI != DIKind::Yes) {
    diagnoseInitError(InstInfo, diag::struct_not_fully_initialized);
    return;
  }

  // If this is an initialization or a normal assignment, upgrade the store to
  // an initialization or assign in the uses list so that clients know about it.
  switch (DI) {
  case DIKind::No:
    InstInfo.Kind = DIUseKind::Initialization;
    break;
  case DIKind::Yes:
    InstInfo.Kind = DIUseKind::Assign;
    break;
  case DIKind::Partial:
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


/// handleSuperclassUse - When processing a 'self' argument on a class, this is
/// called on any conversions to base classes.  These are effectively escape
/// points, but we have additional semantics we want to apply and want to make
/// the diagnostics a lot better.
void LifetimeChecker::handleSuperclassUse(unsigned UseID) {
  DIMemoryUse &InstInfo = Uses[UseID];
  if (getLivenessAtUse(InstInfo) == DIKind::Yes) return;

  if (InstInfo.isSuperInitUse())
    return diagnoseInitError(InstInfo, diag::ivar_not_initialized_at_superinit);

  auto BaseType = InstInfo.Inst->getType(0).getSwiftRValueType().getString();
  diagnose(Module, InstInfo.Inst->getLoc(),
           diag::base_object_use_before_initialized, BaseType);
}


/// updateInstructionForInitState - When an instruction being analyzed moves
/// from being InitOrAssign to some concrete state, update it for that state.
/// This includes rewriting them from assign instructions into their composite
/// operations.
void LifetimeChecker::updateInstructionForInitState(DIMemoryUse &InstInfo) {
  SILInstruction *Inst = InstInfo.Inst;
  
  IsInitialization_t InitKind;
  if (InstInfo.Kind == DIUseKind::Initialization)
    InitKind = IsInitialization;
  else {
    assert(InstInfo.Kind == DIUseKind::Assign);
    InitKind = IsNotInitialization;
  }

  // If this is a copy_addr or store_weak, we just set the initialization bit
  // depending on what we find.
  if (auto *CA = dyn_cast<CopyAddrInst>(Inst)) {
    CA->setIsInitializationOfDest(InitKind);
    return;
  }
  
  if (auto *SW = dyn_cast<StoreWeakInst>(Inst)) {
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

    SmallVector<SILInstruction*, 8> InsertedInsts;
    SILBuilder B(Inst, &InsertedInsts);

    LowerAssignInstruction(B, AI, InitKind);

    // If lowering of the assign introduced any new loads or stores, keep track
    // of them.
    for (auto I : InsertedInsts) {
      if (isa<StoreInst>(I)) {
        NonLoadUses[I] = Uses.size();
        Uses.push_back(DIMemoryUse(I, Kind, FirstElement, NumElements));
      } else if (isa<LoadInst>(I)) {
        Uses.push_back(DIMemoryUse(I, Load, FirstElement, NumElements));
      }
    }
    return;
  }
  
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
  if (isa<DeallocStackInst>(Release) || isa<DeallocBoxInst>(Release))
    return;

  // If the memory object is completely initialized, then nothing needs to be
  // done at this release point.
  AvailabilitySet Availability = getLivenessAtInst(Release, 0, NumElements);
  if (Availability.isAllYes()) return;
  
  // If it is all no, then we can just remove it.
  if (Availability.isAllNo()) {
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

static SILValue getBinaryFunction(StringRef Name, SILType IntSILTy,
                                  SILLocation Loc, SILBuilder &B) {
  CanType IntTy = IntSILTy.getSwiftRValueType();
  unsigned NumBits =
    cast<BuiltinIntegerType>(IntTy)->getWidth().getFixedWidth();
  // Name is something like: add_Int64
  std::string NameStr = Name;
  NameStr += "_Int" + llvm::utostr(NumBits);
  
  // Woo, boilerplate to produce a function type.
  auto extInfo = SILFunctionType::ExtInfo(AbstractCC::Freestanding,
                                          /*thin*/ true,
                                          /*noreturn*/ false,
                                          /*autoclosure*/ false,
                                          /*block*/ false);
  
  SILParameterInfo Params[] = {
    SILParameterInfo(IntTy, ParameterConvention::Direct_Unowned),
    SILParameterInfo(IntTy, ParameterConvention::Direct_Unowned)
  };
  SILResultInfo Result(IntTy, ResultConvention::Unowned);
  
  auto FnType = SILFunctionType::get(nullptr, extInfo,
                                            ParameterConvention::Direct_Owned,
                                            Params, Result,
                                            B.getASTContext());
  auto Ty = SILType::getPrimitiveObjectType(FnType);
  return B.createBuiltinFunctionRef(Loc, NameStr, Ty);
}
static SILValue getTruncateToI1Function(SILType IntSILTy, SILLocation Loc,
                                        SILBuilder &B) {
  CanType IntTy = IntSILTy.getSwiftRValueType();
  unsigned NumBits =
    cast<BuiltinIntegerType>(IntTy)->getWidth().getFixedWidth();

  // Name is something like: trunc_Int64_Int8
  std::string NameStr = "trunc_Int" + llvm::utostr(NumBits) + "_Int1";

  // Woo, boilerplate to produce a function type.
  auto extInfo = SILFunctionType::ExtInfo(AbstractCC::Freestanding,
                                          /*thin*/ true,
                                          /*noreturn*/ false,
                                          /*autoclosure*/ false,
                                          /*block*/ false);
  
  SILParameterInfo Param(IntTy, ParameterConvention::Direct_Unowned);
  Type Int1Ty = BuiltinIntegerType::get(1, B.getASTContext());
  SILResultInfo Result(Int1Ty->getCanonicalType(),
                       ResultConvention::Unowned);
  
  auto FnType = SILFunctionType::get(nullptr, extInfo,
                                     ParameterConvention::Direct_Owned,
                                     Param, Result,
                                     B.getASTContext());
  auto Ty = SILType::getPrimitiveObjectType(FnType);

  
  return B.createBuiltinFunctionRef(Loc, NameStr, Ty);
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
  
  // Create the control variable as the first instruction in the function (so
  // that it is easy to destroy the stack location.
  SILBuilder B(TheMemory.getFunctionEntryPoint());
  SILType IVType =
    SILType::getBuiltinIntegerType(NumElements, Module.getASTContext());
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
  
  SILValue OrFn;

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
      APInt Bitmask = Use.getElementBitmask(NumElements);
      SILValue MaskVal = B.createIntegerLiteral(Loc, IVType, Bitmask);

      // If the mask is all ones, do a simple store, otherwise do a
      // load/or/store sequence to mask in the bits.
      if (!Bitmask.isAllOnesValue()) {
        SILValue Tmp = B.createLoad(Loc, AllocAddr);
        if (!OrFn) {
          SILBuilder FnB(TheMemory.getFunctionEntryPoint());
          OrFn = getBinaryFunction("or", Tmp.getType(), Loc, FnB);
        }
        
        SILValue Args[] = { Tmp, MaskVal };
        MaskVal = B.createApply(Loc, OrFn, Args);
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
    SILValue ShiftRightFn, TruncateFn;
    
    // If the memory object has multiple tuple elements, we need to destroy any
    // live subelements, since they can each be in a different state of
    // initialization.
    for (unsigned Elt = Use.FirstElement, e = Elt+Use.NumElements;
         Elt != e; ++Elt) {
      B.setInsertionPoint(Use.Inst);
      SILValue CondVal = Bitmask;
      if (NumElements != 1) {
        // Shift the mask down to this element.
        if (Elt != 0) {
          if (!ShiftRightFn)
            ShiftRightFn = getBinaryFunction("lshr", Bitmask.getType(), Loc, B);
          SILValue Amt = B.createIntegerLiteral(Loc, Bitmask.getType(), Elt);
          SILValue Args[] = { CondVal, Amt };
          CondVal = B.createApply(Loc, ShiftRightFn, Args);
        }
        
        if (!TruncateFn)
          TruncateFn = getTruncateToI1Function(Bitmask.getType(), Loc, B);
        CondVal = B.createApply(Loc, TruncateFn, CondVal);
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

void LifetimeChecker::
handleConditionalDestroys(SILValue ControlVariableAddr) {
  SILBuilder B(TheMemory.MemoryInst);
  SILValue ShiftRightFn, TruncateFn;
  
  // After handling any conditional initializations, check to see if we have any
  // cases where the value is only partially initialized by the time its
  // lifetime ends.  In this case, we have to make sure not to destroy an
  // element that wasn't initialized yet.
  for (auto &CDElt : ConditionalDestroys) {
    auto *DAI = cast<DestroyAddrInst>(Releases[CDElt.first]);
    auto &Availability = CDElt.second;
    
    // The only instruction that can be in a partially live region is a
    // destroy_addr.  A strong_release must only occur in code that was
    // mandatory inlined, and the argument would have required it to be live at
    // that site.
    SILValue Addr = DAI->getOperand();
  
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
    for (unsigned Elt = 0, e = NumElements; Elt != e; ++Elt) {
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
        // destroy its value at DAI's position.
        B.setInsertionPoint(DAI);
        SILValue EltPtr = TheMemory.emitElementAddress(Elt, DAI->getLoc(), B);
        if (auto *DA = B.emitDestroyAddr(DAI->getLoc(), EltPtr))
          Releases.push_back(DA);
        continue;
      }
      
      // Note - in some partial liveness cases, we can push the destroy_addr up
      // the CFG, instead of immediately generating dynamic control flow checks.
      // This could be handled in processNonTrivialRelease some day.
      
      // Insert a load of the liveness bitmask and split the CFG into a diamond
      // right before the destroy_addr, if we haven't already loaded it.
      B.setInsertionPoint(DAI);
      if (!LoadedMask)
        LoadedMask = B.createLoad(DAI->getLoc(), ControlVariableAddr);
      SILValue CondVal = LoadedMask;
      
      // If this memory object has multiple tuple elements, we need to make sure
      // to test the right one.
      if (NumElements != 1) {
        // Shift the mask down to this element.
        if (Elt != 0) {
          if (!ShiftRightFn) {
            SILBuilder FB(TheMemory.getFunctionEntryPoint());
            ShiftRightFn = getBinaryFunction("lshr", CondVal.getType(),
                                             DAI->getLoc(), FB);
          }
          SILValue Amt = B.createIntegerLiteral(DAI->getLoc(),
                                                CondVal.getType(), Elt);
          SILValue Args[] = { CondVal, Amt };
          CondVal = B.createApply(DAI->getLoc(), ShiftRightFn, Args);
        }
        
        if (!TruncateFn) {
          SILBuilder FB(TheMemory.getFunctionEntryPoint());
          TruncateFn = getTruncateToI1Function(CondVal.getType(),
                                               DAI->getLoc(), FB);
        }
        CondVal = B.createApply(DAI->getLoc(), TruncateFn, CondVal);
      }
      
      SILBasicBlock *CondDestroyBlock, *ContBlock;
      InsertCFGDiamond(CondVal, DAI->getLoc(),
                       B, CondDestroyBlock, nullptr, ContBlock);
      
      // Set up the conditional destroy block.
      B.setInsertionPoint(CondDestroyBlock->begin());
      SILValue EltPtr = TheMemory.emitElementAddress(Elt, DAI->getLoc(), B);
      if (auto *DA = B.emitDestroyAddr(DAI->getLoc(), EltPtr))
        Releases.push_back(DA);
    }
    
    // Finally, now that the destroy_addr is handled, remove the original
    // destroy.
    DAI->eraseFromParent();
    if (auto *AddrI = dyn_cast<SILInstruction>(Addr))
      recursivelyDeleteTriviallyDeadInstructions(AddrI);
  }
}

Optional<DIKind> LifetimeChecker::getLiveOut1(SILBasicBlock *BB) {
  LiveOutBlockState &BBState = getBlockInfo(BB);
  switch (BBState.LOState) {
  case LiveOutBlockState::IsKnown:
    return BBState.getAvailability(0);
  case LiveOutBlockState::IsComputingLiveOut:
    // In cyclic cases we contribute no information, allow other nodes feeding
    // in to define the successors liveness.
    return Nothing;
  case LiveOutBlockState::IsUnknown:
    // Otherwise, process this block.
    break;
  }

  // Set the block's state to reflect that we're currently processing it.  This
  // is required to handle cycles properly.
  BBState.LOState = LiveOutBlockState::IsComputingLiveOut;

  // Compute the liveness of our predecessors value.
  Optional<DIKind> Result = BBState.getAvailabilityConditional(0);
  getPredsLiveOut1(BB, Result);

  // Otherwise, we're golden.  Return success.
  getBlockInfo(BB).setBlockAvailability1(Result.getValue());
  return Result.getValue();
}

void LifetimeChecker::getPredsLiveOut1(SILBasicBlock *BB,
                                       Optional<DIKind> &Result) {
  bool LiveInAny = false, LiveInAll = true;

  // If we have a starting point, incorporate it into our state.
  if (Result.hasValue()) {
    if (Result.getValue() != DIKind::No)
      LiveInAny = true;
    if (Result.getValue() != DIKind::Yes)
      LiveInAll = false;
  }

  // Recursively processes all of our predecessor blocks.  If any of them is
  // not live out, then we aren't either.
  for (auto P : BB->getPreds()) {
    auto LOPred = getLiveOut1(P);
    if (!LOPred.hasValue()) continue;
    
    if (LOPred.getValue() != DIKind::No)
      LiveInAny = true;

    if (LOPred.getValue() != DIKind::Yes)
      LiveInAll = false;
  }

  if (LiveInAll)
    Result = DIKind::Yes;
  else if (LiveInAny)
    Result = DIKind::Partial;
  else
    Result = DIKind::No;
}

AvailabilitySet LifetimeChecker::getLiveOutN(SILBasicBlock *BB) {
  LiveOutBlockState &BBState = getBlockInfo(BB);
  switch (BBState.LOState) {
  case LiveOutBlockState::IsKnown:
    return BBState.getAvailabilitySet();
  case LiveOutBlockState::IsComputingLiveOut:
    // Speculate that it will be live out in cyclic cases.
    return AvailabilitySet(NumElements);
  case LiveOutBlockState::IsUnknown:
    // Otherwise, process this block.
    break;
  }
  
  // Set the block's state to reflect that we're currently processing it.  This
  // is required to handle cycles properly.
  BBState.LOState = LiveOutBlockState::IsComputingLiveOut;

  auto Result = AvailabilitySet(NumElements);
  getPredsLiveOutN(BB, Result);

  // Anything that our initial pass knew as a definition is still a definition
  // live out of this block.  Something known to be not-defined in a predecessor
  // does not drop it to "partial".
  auto &LocalAV = BBState.getAvailabilitySet();
  for (unsigned i = 0, e = NumElements; i != e; ++i) {
    auto EV = LocalAV.getConditional(i);
    if (EV.hasValue() && EV.getValue() == DIKind::Yes)
      Result.set(i, DIKind::Yes);
  }

  // Finally, cache and return our result.
  getBlockInfo(BB).setBlockAvailability(Result);
  return Result;
}

void LifetimeChecker::
getPredsLiveOutN(SILBasicBlock *BB, AvailabilitySet &Result) {
  // Recursively processes all of our predecessor blocks.  If any of them is
  // not live out, then we aren't either.
  for (auto P : BB->getPreds()) {
    // The liveness of this block is the intersection of all of the predecessor
    // block's liveness.
    Result.mergeIn(getLiveOutN(P));
  }
  
  // If any elements are still unknown, smash them to "yes".  This can't
  // happen in live code, and we want to avoid having analyzed blocks with
  // "unset" values.
  Result.changeUnsetElementsTo(DIKind::Yes);
}

/// getLivenessAtInst - Compute the liveness state for any number of tuple
/// elements at the specified instruction.  The elements are returned as an
/// AvailabilitySet.  Elements outside of the range specified may not be
/// computed correctly.
AvailabilitySet LifetimeChecker::
getLivenessAtInst(SILInstruction *Inst, unsigned FirstElt, unsigned NumElts) {
  AvailabilitySet Result(NumElements);

  // Empty tuple queries return a completely "unknown" vector, since they don't
  // care about any of the elements.
  if (NumElts == 0)
    return Result;
  
  SILBasicBlock *InstBB = Inst->getParent();
  
  // The vastly most common case is memory allocations that are not tuples,
  // so special case this with a more efficient algorithm.
  if (NumElements == 1) {
    
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

    Optional<DIKind> ResultVal = Nothing;
    getPredsLiveOut1(InstBB, ResultVal);
    Result.set(0, ResultVal.getValue());
    return Result;
  }

  // Check locally to see if any elements are satified within the block, and
  // keep track of which ones are still needed in the NeededElements set.
  llvm::SmallBitVector NeededElements(NumElements);
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
  getPredsLiveOutN(InstBB, Result);
  
  // If any of the elements was locally satisfied, make sure to mark them.
  for (unsigned i = FirstElt, e = i+NumElts; i != e; ++i) {
    if (!NeededElements[i])
      Result.set(i, DIKind::Yes);
  }
  return Result;
}

/// The specified instruction is a use of the element.  Determine whether all of
/// the tuple elements touched by the instruction are definitely initialized at
/// this point or not.  If the value is initialized on some paths, but not
/// others, this returns a partial result.
DIKind LifetimeChecker::getLivenessAtUse(const DIMemoryUse &Use) {
  // Determine the liveness states of the elements that we care about.
  AvailabilitySet Liveness =
    getLivenessAtInst(Use.Inst, Use.FirstElement, Use.NumElements);
  
  // Now that we know about each element, determine a yes/no/partial result
  // based on the elements we care about.
  bool LiveInAll = true, LiveInAny = false;
  
  for (unsigned i = Use.FirstElement, e = i+Use.NumElements;
       i != e; ++i) {
    DIKind ElementKind = Liveness.get(i);
    if (ElementKind != DIKind::No)
      LiveInAny = true;
    if (ElementKind != DIKind::Yes)
      LiveInAll = false;
  }
  
  if (LiveInAll)
    return DIKind::Yes;
  if (LiveInAny)
    return DIKind::Partial;
  return DIKind::No;
}






//===----------------------------------------------------------------------===//
//                           Top Level Driver
//===----------------------------------------------------------------------===//

static void processMemoryObject(SILInstruction *I) {
  // If the allocation's address has a mark_uninitialized use, then we'll
  // analyze it when we look at the mark_uninitialized instruction itself.
  if (!isa<MarkUninitializedInst>(I))
    for (auto UI : SILValue(I, 1).getUses())
      if (isa<MarkUninitializedInst>(UI->getUser()))
        return;
  
  DEBUG(llvm::errs() << "*** Definite Init looking at: " << *I << "\n");
  DIMemoryObjectInfo MemInfo(I);

  // Set up the datastructure used to collect the uses of the allocation.
  SmallVector<DIMemoryUse, 16> Uses;
  SmallVector<SILInstruction*, 4> Releases;

  // Walk the use list of the pointer, collecting them into the Uses array.
  collectDIElementUsesFrom(MemInfo, Uses, Releases, false);

  LifetimeChecker(MemInfo, Uses, Releases).doIt();
}

/// checkDefiniteInitialization - Check that all memory objects that require
/// initialization before use are properly set and transform the code as
/// required for flow-sensitive properties.
static void checkDefiniteInitialization(SILFunction &Fn) {
  for (auto &BB : Fn) {
    for (auto I = BB.begin(), E = BB.end(); I != E; ++I) {
      SILInstruction *Inst = I;
      if (isa<AllocBoxInst>(Inst) || isa<AllocStackInst>(Inst) ||
          isa<MarkUninitializedInst>(Inst))
        processMemoryObject(Inst);
    }
  }
}

/// lowerRawSILOperations - There are a variety of raw-sil instructions like
/// 'assign' that are only used by this pass.  Now that definite initialization
/// checking is done, remove them.
static void lowerRawSILOperations(SILFunction &Fn) {
  for (auto &BB : Fn) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I++;
      
      // Unprocessed assigns just lower into assignments, not initializations.
      if (auto *AI = dyn_cast<AssignInst>(Inst)) {
        SILBuilder B(AI);
        LowerAssignInstruction(B, AI, IsNotInitialization);
        // Assign lowering may split the block. If it did,
        // reset our iteration range to the block after the insertion.
        if (B.getInsertionBB() != &BB)
          I = E;
        continue;
      }

      // mark_uninitialized just becomes a noop, resolving to its operand.
      if (auto *MUI = dyn_cast<MarkUninitializedInst>(Inst)) {
        SILValue(MUI, 0).replaceAllUsesWith(MUI->getOperand());
        MUI->eraseFromParent();
        continue;
      }
      
      // mark_function_escape just gets zapped.
      if (isa<MarkFunctionEscapeInst>(Inst)) {
        Inst->eraseFromParent();
        continue;
      }
    }
  }
}



/// performSILDefiniteInitialization - Perform definitive initialization
/// analysis and promote alloc_box uses into SSA registers for later SSA-based
/// dataflow passes.
void swift::performSILDefiniteInitialization(SILModule *M) {
  for (auto &Fn : *M) {
    // Walk through and promote all of the alloc_box's that we can.
    checkDefiniteInitialization(Fn);
    Fn.verify();

    // Lower raw-sil only instructions used by this pass, like "assign".
    lowerRawSILOperations(Fn);
    Fn.verify();
  }
}
