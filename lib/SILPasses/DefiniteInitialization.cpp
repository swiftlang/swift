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
#include "swift/Subsystems.h"
#include "DIMemoryUseCollector.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

STATISTIC(NumLoadPromoted, "Number of loads promoted");
STATISTIC(NumDestroyAddrPromoted, "Number of destroy_addrs promoted");
STATISTIC(NumAssignRewritten, "Number of assigns rewritten");
STATISTIC(NumAllocRemoved, "Number of allocations completely removed");

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
// Scalarization Logic
//===----------------------------------------------------------------------===//

/// Given a pointer to a tuple type, compute the addresses of each element and
/// add them to the ElementAddrs vector.
static void getScalarizedElementAddresses(SILValue Pointer, SILBuilder &B,
                                          SILLocation Loc,
                                      SmallVectorImpl<SILValue> &ElementAddrs) {
  CanType AggType = Pointer.getType().getSwiftRValueType();
  TupleType *TT = AggType->castTo<TupleType>();
  for (auto &Field : TT->getFields()) {
    (void)Field;
    ElementAddrs.push_back(B.createTupleElementAddr(Loc, Pointer,
                                                    ElementAddrs.size()));
  }
}

/// Given an RValue of aggregate type, compute the values of the elements by
/// emitting a series of tuple_element instructions.
static void getScalarizedElements(SILValue V,
                                  SmallVectorImpl<SILValue> &ElementVals,
                                  SILLocation Loc, SILBuilder &B) {
  CanType AggType = V.getType().getSwiftRValueType();

  if (TupleType *TT = AggType->getAs<TupleType>()) {
    for (auto &Field : TT->getFields()) {
      (void)Field;
      ElementVals.push_back(B.emitTupleExtract(Loc, V, ElementVals.size()));
    }
    return;
  }

  assert(AggType->is<StructType>() ||
         AggType->is<BoundGenericStructType>());
  StructDecl *SD = cast<StructDecl>(AggType->getAnyNominal());
  for (auto *VD : SD->getStoredProperties()) {
    ElementVals.push_back(B.emitStructExtract(Loc, V, VD));
  }
}

/// Remove dead tuple_element_addr and struct_element_addr chains - only.
static void RemoveDeadAddressingInstructions(SILValue Pointer) {
  if (!Pointer.use_empty()) return;

  SILInstruction *I = dyn_cast<SILInstruction>(Pointer);
  if (I == 0 ||
      !(isa<TupleElementAddrInst>(Pointer) ||
        isa<StructElementAddrInst>(Pointer)))
    return;
  Pointer = I->getOperand(0);
  I->eraseFromParent();
  RemoveDeadAddressingInstructions(Pointer);
}


/// Scalarize a load down to its subelements.  If NewLoads is specified, this
/// can return the newly generated sub-element loads.
static SILValue scalarizeLoad(LoadInst *LI,
                              SmallVectorImpl<SILValue> &ElementAddrs,
                        SmallVectorImpl<SILInstruction*> *NewLoads = nullptr) {
  SILBuilder B(LI);
  SmallVector<SILValue, 4> ElementTmps;

  for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i) {
    auto *SubLI = B.createLoad(LI->getLoc(), ElementAddrs[i]);
    ElementTmps.push_back(SubLI);
    if (NewLoads) NewLoads->push_back(SubLI);
  }

  if (LI->getType().is<TupleType>())
    return B.createTuple(LI->getLoc(), LI->getType(), ElementTmps);
  return B.createStruct(LI->getLoc(), LI->getType(), ElementTmps);
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

    LiveOutBlockState(unsigned NumTupleElements)
      : HasNonLoadUse(false),
        Availability(NumTupleElements), LOState(IsUnknown) {
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
      Availability.set(Use.FirstTupleElement, DIKind::Yes);
                         
      for (unsigned i = 1; i != Use.NumTupleElements; ++i)
        Availability.set(Use.FirstTupleElement+i, DIKind::Yes);
    }
  };
} // end anonymous namespace

namespace {
  /// LifetimeChecker - This is the main heavy lifting for definite
  /// initialization checking of a memory object.
  class LifetimeChecker {
    SILModule &Module;

    /// TheMemory - This is either an alloc_box instruction or a
    /// mark_uninitialized instruction.  This represents the start of the
    /// lifetime of the value being analyzed.
    SILInstruction *TheMemory;

    /// This is the SILType of the memory object.
    SILType MemoryType;

    /// The number of tuple elements in this memory object.
    unsigned NumTupleElements;
    
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
    LifetimeChecker(SILInstruction *TheMemory,
                    SmallVectorImpl<DIMemoryUse> &Uses,
                    SmallVectorImpl<SILInstruction*> &Releases);

    void doIt();

  private:

    LiveOutBlockState &getBlockInfo(SILBasicBlock *BB) {
      return PerBlockInfo.insert({BB,
                        LiveOutBlockState(NumTupleElements)}).first->second;
    }
    
    AvailabilitySet getLivenessAtInst(SILInstruction *Inst, unsigned FirstElt,
                                      unsigned NumElts);

    DIKind getLivenessAtUse(const DIMemoryUse &Use);

    void handleStoreUse(unsigned UseID);
    void updateInstructionForInitState(DIMemoryUse &InstInfo);


    void processNonTrivialRelease(unsigned ReleaseID);

    SILValue handleConditionalInitAssign();
    void handleConditionalDestroys(SILValue ControlVariableAddr);

    Optional<DIKind> getLiveOut1(SILBasicBlock *BB);
    void getPredsLiveOut1(SILBasicBlock *BB, Optional<DIKind> &Result);
    AvailabilitySet getLiveOutN(SILBasicBlock *BB);
    void getPredsLiveOutN(SILBasicBlock *BB, AvailabilitySet &Result);

    void diagnoseInitError(const DIMemoryUse &Use, Diag<StringRef> DiagMessage);
  };
} // end anonymous namespace


LifetimeChecker::LifetimeChecker(SILInstruction *TheMemory,
                                 SmallVectorImpl<DIMemoryUse> &Uses,
                                 SmallVectorImpl<SILInstruction*> &Releases)
  : Module(TheMemory->getModule()), TheMemory(TheMemory), Uses(Uses),
    Releases(Releases) {

  // Compute the type of the memory object.
  if (auto *ABI = dyn_cast<AllocBoxInst>(TheMemory))
    MemoryType = ABI->getElementType();
  else if (auto *ASI = dyn_cast<AllocStackInst>(TheMemory))
    MemoryType = ASI->getElementType();
  else {
    assert(isa<MarkUninitializedInst>(TheMemory) && "Unknown memory object");
    MemoryType = TheMemory->getType(0).getObjectType();
  }

  NumTupleElements = TF::getElementCount(MemoryType.getSwiftRValueType());

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
  NonLoadUses[TheMemory] = ~0U;
  auto &MemBBInfo = getBlockInfo(TheMemory->getParent());
  MemBBInfo.HasNonLoadUse = true;

  // There is no scanning required (or desired) for the block that defines the
  // memory object itself.  Its live-out properties are whatever are trivially
  // locally inferred by the loop above.  Mark any unset elements as not
  // available.
  MemBBInfo.getAvailabilitySet().changeUnsetElementsTo(DIKind::No);
    
  MemBBInfo.LOState = LiveOutBlockState::IsKnown;
}

void LifetimeChecker::diagnoseInitError(const DIMemoryUse &Use,
                                        Diag<StringRef> DiagMessage) {
  auto *Inst = Use.Inst;

  // Check to see if we've already emitted an error at this location.  If so,
  // swallow the error.
  for (auto L : EmittedErrorLocs)
    if (L == Inst->getLoc())
      return;
  EmittedErrorLocs.push_back(Inst->getLoc());

  // If the definition is a declaration, try to reconstruct a name and
  // optionally an access path to the uninitialized element.
  std::string Name;
  if (ValueDecl *VD =
        dyn_cast_or_null<ValueDecl>(TheMemory->getLoc().getAsASTNode<Decl>()))
    Name = VD->getName().str();
  else
    Name = "<unknown>";

  // If the overall memory allocation is a tuple with multiple elements,
  // then dive in to explain *which* element is being used uninitialized.
  CanType AllocTy = MemoryType.getSwiftRValueType();

  // TODO: Given that we know the range of elements being accessed, we don't
  // need to go all the way deep into a recursive tuple here.  We could print
  // an error about "v" instead of "v.0" when "v" has tuple type and the whole
  // thing is accessed inappropriately.
  TF::getPathStringToElement(AllocTy, Use.FirstTupleElement, Name);

  diagnose(Module, Inst->getLoc(), DiagMessage, Name);

  // Provide context as note diagnostics.

  // TODO: The QoI could be improved in many different ways here.  For example,
  // We could give some path information where the use was uninitialized, like
  // the static analyzer.
  diagnose(Module, TheMemory->getLoc(), diag::variable_defined_here);
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
        // This is a use of an uninitialized value.  Emit a diagnostic.
        if (isa<MarkFunctionEscapeInst>(Inst))
          diagnoseInitError(Use, diag::global_variable_function_use_uninit);
        else
          diagnoseInitError(Use, diag::variable_escape_before_initialized);
      }
      break;
    }
  }

  // If we emitted an error, there is no reason to proceed with load promotion.
  if (!EmittedErrorLocs.empty()) return;

  // If the memory object has nontrivial type, then any destroy/release of the
  // memory object will destruct the memory.  If the memory (or some element
  // thereof) is not initialized on some path, the bad things happen.  Process
  // releases to adjust for this.
  if (!MemoryType.isTrivial(Module)) {
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
    if (!InstInfo.onlyTouchesTrivialElements(MemoryType))
      HasConditionalInitAssignOrDestroys = true;
    return;
  }
  
  // Otherwise, we have a definite init or assign.  Make sure the instruction
  // itself is tagged properly.
  updateInstructionForInitState(InstInfo);
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

    unsigned FirstTupleElement = InstInfo.FirstTupleElement;
    unsigned NumTupleElements = InstInfo.NumTupleElements;

    SmallVector<SILInstruction*, 8> InsertedInsts;
    SILBuilder B(Inst, &InsertedInsts);

    LowerAssignInstruction(B, AI, InitKind);

    // If lowering of the assign introduced any new loads or stores, keep track
    // of them.
    for (auto I : InsertedInsts) {
      if (isa<StoreInst>(I)) {
        NonLoadUses[I] = Uses.size();
        Uses.push_back(DIMemoryUse(I, Kind,
                                   FirstTupleElement, NumTupleElements));
      } else if (isa<LoadInst>(I)) {
        Uses.push_back(DIMemoryUse(I, Load,
                                   FirstTupleElement, NumTupleElements));
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
  AvailabilitySet Availability = getLivenessAtInst(Release, 0,NumTupleElements);
  if (Availability.isAllYes()) return;
  
  // If it is all no, then we can just remove it.
  if (Availability.isAllNo()) {
    SILValue Addr = Release->getOperand(0);
    Release->eraseFromParent();
    RemoveDeadAddressingInstructions(Addr);
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
  SILLocation Loc = TheMemory->getLoc();
  Loc.markAutoGenerated();
  
  // Create the control variable as the first instruction in the function (so
  // that it is easy to destroy the stack location.
  SILBuilder B(TheMemory->getFunction()->begin()->begin());
  SILType IVType =
    SILType::getBuiltinIntegerType(NumTupleElements, Module.getASTContext());
  auto Alloc = B.createAllocStack(Loc, IVType);
  
  // Find all the return blocks in the function, inserting a dealloc_stack
  // before the return.
  for (auto &BB : *TheMemory->getFunction()) {
    if (auto *RI = dyn_cast<ReturnInst>(BB.getTerminator())) {
      B.setInsertionPoint(RI);
      B.createDeallocStack(Loc, SILValue(Alloc, 0));
    }
  }
  
  // Before the memory allocation, store zero in the control variable.
  B.setInsertionPoint(TheMemory->getNextNode());
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
      if (Use.onlyTouchesTrivialElements(MemoryType))
        continue;
      
      // Get the integer constant.
      B.setInsertionPoint(Use.Inst);
      APInt Bitmask = Use.getElementBitmask(NumTupleElements);
      SILValue MaskVal = B.createIntegerLiteral(Loc, IVType,Bitmask);

      // If the mask is all ones, do a simple store, otherwise do a
      // load/or/store sequence to mask in the bits.
      if (!Bitmask.isAllOnesValue()) {
        SILValue Tmp = B.createLoad(Loc, AllocAddr);
        if (!OrFn) {
          SILBuilder FnB(TheMemory->getFunction()->begin()->begin());
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
    if (Use.onlyTouchesTrivialElements(MemoryType))
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
    for (unsigned Elt = Use.FirstTupleElement, e = Elt+Use.NumTupleElements;
         Elt != e; ++Elt) {
      B.setInsertionPoint(Use.Inst);
      SILValue CondVal = Bitmask;
      if (NumTupleElements != 1) {
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
      SILValue EltPtr = TF::emitElementAddress(SILValue(TheMemory, 1),
                                               Elt, Loc, B);
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
  SILBuilder B(TheMemory);
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
    for (unsigned Elt = 0, e = NumTupleElements; Elt != e; ++Elt) {
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
        SILValue EltPtr = TF::emitElementAddress(Addr, Elt, DAI->getLoc(), B);
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
      if (NumTupleElements != 1) {
        // Shift the mask down to this element.
        if (Elt != 0) {
          if (!ShiftRightFn) {
            SILBuilder FB(TheMemory->getFunction()->begin()->begin());
            ShiftRightFn = getBinaryFunction("lshr", CondVal.getType(),
                                             DAI->getLoc(), FB);
          }
          SILValue Amt = B.createIntegerLiteral(DAI->getLoc(),
                                                CondVal.getType(), Elt);
          SILValue Args[] = { CondVal, Amt };
          CondVal = B.createApply(DAI->getLoc(), ShiftRightFn, Args);
        }
        
        if (!TruncateFn) {
          SILBuilder FB(TheMemory->getFunction()->begin()->begin());
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
      SILValue EltPtr = TF::emitElementAddress(Addr, Elt, DAI->getLoc(), B);
      if (auto *DA = B.emitDestroyAddr(DAI->getLoc(), EltPtr))
        Releases.push_back(DA);
    }
    
    // Finally, now that the destroy_addr is handled, remove the original
    // destroy.
    DAI->eraseFromParent();
    RemoveDeadAddressingInstructions(Addr);
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
    return AvailabilitySet(NumTupleElements);
  case LiveOutBlockState::IsUnknown:
    // Otherwise, process this block.
    break;
  }
  
  // Set the block's state to reflect that we're currently processing it.  This
  // is required to handle cycles properly.
  BBState.LOState = LiveOutBlockState::IsComputingLiveOut;

  auto Result = BBState.getAvailabilitySet();
  getPredsLiveOutN(BB, Result);

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
  AvailabilitySet Result(NumTupleElements);

  // Empty tuple queries return a completely "unknown" vector, since they don't
  // care about any of the elements.
  if (NumElts == 0)
    return Result;
  
  SILBasicBlock *InstBB = Inst->getParent();
  
  // The vastly most common case is memory allocations that are not tuples,
  // so special case this with a more efficient algorithm.
  if (NumTupleElements == 1) {
    
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
        Result.set(0, TheInst == TheMemory ? DIKind::No : DIKind::Yes);
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
  llvm::SmallBitVector NeededElements(NumTupleElements);
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
      if (TheInst == TheMemory) {
        // The result is perfectly decided locally.
        for (unsigned i = FirstElt, e = i+NumElts; i != e; ++i)
          Result.set(i, NeededElements[i] ? DIKind::No : DIKind::Yes);
        return Result;
      }
      
      // Check to see which tuple elements this instruction defines.  Clear them
      // from the set we're scanning from.
      auto &TheInstUse = Uses[It->second];
      NeededElements.reset(TheInstUse.FirstTupleElement,
                      TheInstUse.FirstTupleElement+TheInstUse.NumTupleElements);
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
    getLivenessAtInst(Use.Inst, Use.FirstTupleElement, Use.NumTupleElements);
  
  // Now that we know about each element, determine a yes/no/partial result
  // based on the elements we care about.
  bool LiveInAll = true, LiveInAny = false;
  
  for (unsigned i = Use.FirstTupleElement, e = i+Use.NumTupleElements;
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
//                          ElementUseCollector
//===----------------------------------------------------------------------===//

namespace {
  class ElementUseCollector {
    SmallVectorImpl<DIMemoryUse> &Uses;
    SmallVectorImpl<SILInstruction*> &Releases;
    
    /// This is true if definite initialization has finished processing assign
    /// and other ambiguous instructions into init vs assign classes.
    bool isDefiniteInitFinished;

    /// When walking the use list, if we index into a struct element, keep track
    /// of this, so that any indexes into tuple subelements don't affect the
    /// element we attribute an access to.
    bool InStructSubElement = false;

    /// When walking the use list, if we index into an enum slice, keep track
    /// of this.
    bool InEnumSubElement = false;
  public:
    ElementUseCollector(SmallVectorImpl<DIMemoryUse> &Uses,
                        SmallVectorImpl<SILInstruction*> &Releases,
                        bool isDefiniteInitFinished)
      : Uses(Uses), Releases(Releases),
        isDefiniteInitFinished(isDefiniteInitFinished) {
    }

    void collectFromMarkUninitialized(MarkUninitializedInst *MUI) {
      collectUses(SILValue(MUI, 0), 0);
    }

    /// This is the main entry point for the use walker.  It collects uses from
    /// the address and the refcount result of the allocation.
    void collectFromAllocation(SILInstruction *I) {
      collectUses(SILValue(I, 1), 0);

      // Collect information about the retain count result as well.
      for (auto UI : SILValue(I, 0).getUses()) {
        auto *User = UI->getUser();

        // If this is a release or dealloc_stack, then remember it as such.
        if (isa<StrongReleaseInst>(User) || isa<DeallocStackInst>(User) ||
            isa<DeallocBoxInst>(User)) {
          Releases.push_back(User);
        }
      }
    }

  private:
    void collectUses(SILValue Pointer, unsigned BaseTupleElt);

    void addElementUses(unsigned BaseTupleElt, SILType UseTy,
                        SILInstruction *User, DIUseKind Kind);
    void collectTupleElementUses(TupleElementAddrInst *TEAI,
                                 unsigned BaseTupleElt);
  };
  
  
} // end anonymous namespace

/// addElementUses - An operation (e.g. load, store, inout use, etc) on a value
/// acts on all of the aggregate elements in that value.  For example, a load
/// of $*(Int,Int) is a use of both Int elements of the tuple.  This is a helper
/// to keep the Uses data structure up to date for aggregate uses.
void ElementUseCollector::addElementUses(unsigned BaseTupleElt, SILType UseTy,
                                         SILInstruction *User, DIUseKind Kind) {
  // If we're in a subelement of a struct or enum, just mark the struct, not
  // things that come after it in a parent tuple.
  unsigned NumTupleElements = 1;
  if (!InStructSubElement && !InEnumSubElement)
    NumTupleElements = TF::getElementCount(UseTy.getSwiftRValueType());
  
  Uses.push_back(DIMemoryUse(User, Kind, BaseTupleElt, NumTupleElements));
}

/// Given a tuple_element_addr or struct_element_addr, compute the new
/// BaseTupleElt implicit in the selected member, and recursively add uses of
/// the instruction.
void ElementUseCollector::
collectTupleElementUses(TupleElementAddrInst *TEAI, unsigned BaseTupleElt) {

  // If we're walking into a tuple within a struct or enum, don't adjust the
  // BaseElt.  The uses hanging off the tuple_element_addr are going to be
  // counted as uses of the struct or enum itself.
  if (InStructSubElement || InEnumSubElement)
    return collectUses(SILValue(TEAI, 0), BaseTupleElt);

  // tuple_element_addr P, 42 indexes into the current tuple element.
  // Recursively process its uses with the adjusted element number.
  unsigned FieldNo = TEAI->getFieldNo();
  auto *TT = TEAI->getTupleType();
  unsigned NewBaseElt = BaseTupleElt;
  for (unsigned i = 0; i != FieldNo; ++i) {
    CanType EltTy = TT->getElementType(i)->getCanonicalType();
    NewBaseElt += TF::getElementCount(EltTy);
  }
  
  collectUses(SILValue(TEAI, 0), NewBaseElt);
}


void ElementUseCollector::collectUses(SILValue Pointer, unsigned BaseTupleElt) {
  assert(Pointer.getType().isAddress() &&
         "Walked through the pointer to the value?");
  SILType PointeeType = Pointer.getType().getObjectType();

  /// This keeps track of instructions in the use list that touch multiple tuple
  /// elements and should be scalarized.  This is done as a second phase to
  /// avoid invalidating the use iterator.
  ///
  SmallVector<SILInstruction*, 4> UsesToScalarize;
  
  for (auto UI : Pointer.getUses()) {
    auto *User = UI->getUser();

    // struct_element_addr P, #field indexes into the current element.
    if (auto *SEAI = dyn_cast<StructElementAddrInst>(User)) {
      // Set the "InStructSubElement" flag and recursively process the uses.
      llvm::SaveAndRestore<bool> X(InStructSubElement, true);
      collectUses(SILValue(SEAI, 0), BaseTupleElt);
      continue;
    }

    // Instructions that compute a subelement are handled by a helper.
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(User)) {
      collectTupleElementUses(TEAI, BaseTupleElt);
      continue;
    }
    
    // Loads are a use of the value.
    if (isa<LoadInst>(User)) {
      if (PointeeType.is<TupleType>())
        UsesToScalarize.push_back(User);
      else
        Uses.push_back(DIMemoryUse(User, DIUseKind::Load, BaseTupleElt, 1));
      continue;
    }

    if (isa<LoadWeakInst>(User)) {
      Uses.push_back(DIMemoryUse(User, DIUseKind::Load, BaseTupleElt, 1));
      continue;
    }

    // Stores *to* the allocation are writes.
    if ((isa<StoreInst>(User) || isa<AssignInst>(User)) &&
        UI->getOperandNumber() == 1) {
      if (PointeeType.is<TupleType>()) {
        UsesToScalarize.push_back(User);
        continue;
      }
      
      // Coming out of SILGen, we assume that raw stores are initializations,
      // unless they have trivial type (which we classify as InitOrAssign).
      DIUseKind Kind;
      if (InStructSubElement)
        Kind = DIUseKind::PartialStore;
      else if (isa<AssignInst>(User))
        Kind = DIUseKind::InitOrAssign;
      else if (PointeeType.isTrivial(User->getModule()))
        Kind = DIUseKind::InitOrAssign;
      else
        Kind = DIUseKind::Initialization;
      
      Uses.push_back(DIMemoryUse(User, Kind, BaseTupleElt, 1));
      continue;
    }
    
    if (auto SWI = dyn_cast<StoreWeakInst>(User))
      if (UI->getOperandNumber() == 1) {
        DIUseKind Kind;
        if (InStructSubElement)
          Kind = DIUseKind::PartialStore;
        else if (SWI->isInitializationOfDest())
          Kind = DIUseKind::Initialization;
        else if (isDefiniteInitFinished)
          Kind = DIUseKind::Assign;
        else
          Kind = DIUseKind::InitOrAssign;
        Uses.push_back(DIMemoryUse(User, Kind, BaseTupleElt, 1));
        continue;
      }
    
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      // If this is a copy of a tuple, we should scalarize it so that we don't
      // have an access that crosses elements.
      if (PointeeType.is<TupleType>()) {
        UsesToScalarize.push_back(CAI);
        continue;
      }
      
      // If this is the source of the copy_addr, then this is a load.  If it is
      // the destination, then this is an unknown assignment.  Note that we'll
      // revisit this instruction and add it to Uses twice if it is both a load
      // and store to the same aggregate.
      DIUseKind Kind;
      if (UI->getOperandNumber() == 0)
        Kind = DIUseKind::Load;
      else if (InStructSubElement)
        Kind = DIUseKind::PartialStore;
      else if (CAI->isInitializationOfDest())
        Kind = DIUseKind::Initialization;
      else if (isDefiniteInitFinished)
        Kind = DIUseKind::Assign;
      else
        Kind = DIUseKind::InitOrAssign;
      
      Uses.push_back(DIMemoryUse(CAI, Kind, BaseTupleElt, 1));
      continue;
    }
    
    // Initializations are definitions.  This is currently used in constructors
    // and should go away someday.
    if (isa<InitializeVarInst>(User)) {
      auto Kind = InStructSubElement ?
        DIUseKind::PartialStore : DIUseKind::Initialization;
      addElementUses(BaseTupleElt, PointeeType, User, Kind);
      continue;
    }

    // The apply instruction does not capture the pointer when it is passed
    // through [inout] arguments or for indirect returns.  InOut arguments are
    // treated as uses and may-store's, but an indirect return is treated as a
    // full store.
    //
    // Note that partial_apply instructions always close over their argument.
    //
    if (auto *Apply = dyn_cast<ApplyInst>(User)) {
      auto FTI = Apply->getSubstCalleeType();
      unsigned ArgumentNumber = UI->getOperandNumber()-1;

      auto Param = FTI->getParameters()[ArgumentNumber];
      assert(Param.isIndirect());

      switch (Param.getConvention()) {
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Guaranteed:
        llvm_unreachable("address value passed to indirect parameter");

      // If this is an in-parameter, it is like a load.
      case ParameterConvention::Indirect_In:
        addElementUses(BaseTupleElt, PointeeType, User, DIUseKind::IndirectIn);
        continue;

      // If this is an out-parameter, it is like a store.
      case ParameterConvention::Indirect_Out:
        assert(!InStructSubElement && "We're initializing sub-members?");
        addElementUses(BaseTupleElt, PointeeType, User,
                       DIUseKind::Initialization);
        continue;

      // If this is an @inout parameter, it is like both a load and store.
      case ParameterConvention::Indirect_Inout:
        addElementUses(BaseTupleElt, PointeeType, User, DIUseKind::InOutUse);
        continue;
      }
      llvm_unreachable("bad parameter convention");
    }

    // enum_data_addr is treated like a tuple_element_addr or other instruction
    // that is looking into the memory object (i.e., the memory object needs to
    // be explicitly initialized by a copy_addr or some other use of the
    // projected address).
    if (isa<EnumDataAddrInst>(User)) {
      assert(!InStructSubElement && !InEnumSubElement &&
             "enum_data_addr shouldn't apply to subelements");
      // Keep track of the fact that we're inside of an enum.  This informs our
      // recursion that tuple stores are not scalarized outside, and that stores
      // should not be treated as partial stores.
      llvm::SaveAndRestore<bool> X(InEnumSubElement, true);
      collectUses(SILValue(User, 0), BaseTupleElt);
      continue;
    }

    // init_existential is modeled as an initialization store, where the uses
    // are treated as subelement accesses.
    if (isa<InitExistentialInst>(User)) {
      assert(!InStructSubElement && !InEnumSubElement &&
             "init_existential should not apply to subelements");
      Uses.push_back(DIMemoryUse(User, DIUseKind::Initialization,
                                 BaseTupleElt, 1));

      // Set the "InEnumSubElement" flag (so we don't consider tuple indexes to
      // index across elements) and recursively process the uses.
      llvm::SaveAndRestore<bool> X(InEnumSubElement, true);
      collectUses(SILValue(User, 0), BaseTupleElt);
      continue;
    }
    
    // inject_enum_addr is treated as a store unconditionally.
    if (isa<InjectEnumAddrInst>(User)) {
      assert(!InStructSubElement &&
             "inject_enum_addr the subelement of a struct unless in a ctor");
      Uses.push_back(DIMemoryUse(User, DIUseKind::Initialization,
                                 BaseTupleElt, 1));
      continue;
    }

    // upcast_existential is modeled as a load or initialization depending on
    // which operand we're looking at.
    if (isa<UpcastExistentialInst>(User)) {
      auto Kind = UI->getOperandNumber() == 1 ?
        DIUseKind::Initialization : DIUseKind::Load;
      Uses.push_back(DIMemoryUse(User, Kind, BaseTupleElt, 1));
      continue;
    }
    
    // project_existential is a use of the protocol value, so it is modeled as a
    // load.
    if (isa<ProjectExistentialInst>(User) || isa<ProtocolMethodInst>(User)) {
      Uses.push_back(DIMemoryUse(User, DIUseKind::Load, BaseTupleElt, 1));
      // TODO: Is it safe to ignore all uses of the project_existential?
      continue;
    }

    // We model destroy_addr as a release of the entire value.
    if (isa<DestroyAddrInst>(User)) {
      Releases.push_back(User);
      continue;
    }

    // Otherwise, the use is something complicated, it escapes.
    addElementUses(BaseTupleElt, PointeeType, User, DIUseKind::Escape);
  }

  // Now that we've walked all of the immediate uses, scalarize any operations
  // working on tuples if we need to for canonicalization or analysis reasons.
  if (!UsesToScalarize.empty()) {
    SILInstruction *PointerInst = cast<SILInstruction>(Pointer);
    SmallVector<SILValue, 4> ElementAddrs;
    SILBuilder AddrBuilder(++SILBasicBlock::iterator(PointerInst));
    getScalarizedElementAddresses(Pointer, AddrBuilder, PointerInst->getLoc(),
                                  ElementAddrs);

    
    SmallVector<SILValue, 4> ElementTmps;
    for (auto *User : UsesToScalarize) {
      ElementTmps.clear();

      DEBUG(llvm::errs() << "  *** Scalarizing: " << *User << "\n");

      // Scalarize LoadInst
      if (auto *LI = dyn_cast<LoadInst>(User)) {
        SILValue Result = scalarizeLoad(LI, ElementAddrs);
        SILValue(LI, 0).replaceAllUsesWith(Result);
        LI->eraseFromParent();
        continue;
      }

      SILBuilder B(User);

      // Scalarize AssignInst
      if (auto *AI = dyn_cast<AssignInst>(User)) {
        getScalarizedElements(AI->getOperand(0), ElementTmps, AI->getLoc(), B);

        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
          B.createAssign(AI->getLoc(), ElementTmps[i], ElementAddrs[i]);
        AI->eraseFromParent();
        continue;
      }
      
      // Scalarize StoreInst
      if (auto *SI = dyn_cast<StoreInst>(User)) {
        getScalarizedElements(SI->getOperand(0), ElementTmps, SI->getLoc(), B);
        
        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
          B.createStore(SI->getLoc(), ElementTmps[i], ElementAddrs[i]);
        SI->eraseFromParent();
        continue;
      }
      
      // Scalarize CopyAddrInst.
      auto *CAI = cast<CopyAddrInst>(User);

      // Determine if this is a copy *from* or *to* "Pointer".
      if (CAI->getSrc() == Pointer) {
        // Copy from pointer.
        getScalarizedElementAddresses(CAI->getDest(), B, CAI->getLoc(),
                                      ElementTmps);
        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
        B.createCopyAddr(CAI->getLoc(), ElementAddrs[i], ElementTmps[i],
                         CAI->isTakeOfSrc(), CAI->isInitializationOfDest());
        
      } else {
        getScalarizedElementAddresses(CAI->getSrc(), B, CAI->getLoc(),
                                      ElementTmps);
        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
        B.createCopyAddr(CAI->getLoc(), ElementTmps[i], ElementAddrs[i],
                         CAI->isTakeOfSrc(), CAI->isInitializationOfDest());
      }
      CAI->eraseFromParent();
    }
    
    // Now that we've scalarized some stuff, recurse down into the newly created
    // element address computations to recursively process it.  This can cause
    // further scalarization.
    for (auto EltPtr : ElementAddrs)
      collectTupleElementUses(cast<TupleElementAddrInst>(EltPtr), BaseTupleElt);
  }
}


//===----------------------------------------------------------------------===//
// Subelement Analysis Implementation
//===----------------------------------------------------------------------===//

static unsigned getNumSubElements(SILType T, SILModule &M) {
  if (auto TT = T.getAs<TupleType>()) {
    unsigned NumElements = 0;
    for (auto index : indices(TT.getElementTypes()))
      NumElements += getNumSubElements(T.getTupleElementType(index), M);
    return NumElements;
  }
  
  if (auto *SD = T.getStructOrBoundGenericStruct()) {
    unsigned NumElements = 0;
    for (auto *D : SD->getStoredProperties())
      NumElements += getNumSubElements(T.getFieldType(D, M), M);
    return NumElements;
  }
  
  // If this isn't a tuple or struct, it is a single element.
  return 1;
}

/// getAccessPathRoot - Given an address, dive through any tuple/struct element
/// addresses to get the underlying value.
static SILValue getAccessPathRoot(SILValue Pointer) {
  while (1) {
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(Pointer))
      Pointer = TEAI->getOperand();
    else if (auto SEAI = dyn_cast<StructElementAddrInst>(Pointer))
      Pointer = SEAI->getOperand();
    else
      return Pointer;
  }
}

/// Compute the element number indicated by the specified pointer (which is
/// derived from the root by a series of tuple/struct element addresses) and return
/// the first subelement addressed by the address.  For example, given:
///
///   root = alloc { a: { c: i64, d: i64 }, b: (i64, i64) }
///   tmp1 = struct_element_addr root, 1
///   tmp2 = tuple_element_addr tmp1, 0
///
/// This will return an access path of [struct: 'b', tuple: 0] and a base
/// element of 2.
///
static unsigned ComputeAccessPath(SILValue Pointer, SILInstruction *RootInst) {
  unsigned SubEltNumber = 0;
  SILModule &M = RootInst->getModule();
  
  while (1) {
    // If we got to the root, we're done.
    if (RootInst == Pointer.getDef())
      return SubEltNumber;
    
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(Pointer)) {
      SILType TT = TEAI->getOperand().getType();
      
      // Keep track of what subelement is being referenced.
      for (unsigned i = 0, e = TEAI->getFieldNo(); i != e; ++i) {
        SubEltNumber += getNumSubElements(TT.getTupleElementType(i), M);
      }
      Pointer = TEAI->getOperand();
    } else {
      auto *SEAI = cast<StructElementAddrInst>(Pointer);
      SILType ST = SEAI->getOperand().getType();
      
      // Keep track of what subelement is being referenced.
      StructDecl *SD = SEAI->getStructDecl();
      for (auto *D : SD->getStoredProperties()) {
        if (D == SEAI->getField()) break;
        SubEltNumber += getNumSubElements(ST.getFieldType(D, M), M);
      }
      
      Pointer = SEAI->getOperand();
    }
  }
}



/// Given an aggregate value and an access path, extract the value indicated by
/// the path.
static SILValue ExtractSubElement(SILValue Val, unsigned SubElementNumber,
                                  SILBuilder &B, SILLocation Loc) {
  SILType ValTy = Val.getType();
  
  // Extract tuple elements.
  if (auto TT = ValTy.getAs<TupleType>()) {
    for (unsigned EltNo : indices(TT.getElementTypes())) {
      // Keep track of what subelement is being referenced.
      SILType EltTy = ValTy.getTupleElementType(EltNo);
      unsigned NumSubElt = getNumSubElements(EltTy, B.getModule());
      if (SubElementNumber < NumSubElt) {
        Val = B.emitTupleExtract(Loc, Val, EltNo, EltTy);
        return ExtractSubElement(Val, SubElementNumber, B, Loc);
      }
      
      SubElementNumber -= NumSubElt;
    }
    
    llvm_unreachable("Didn't find field");
  }
  
  // Extract struct elements.
  if (auto *SD = ValTy.getStructOrBoundGenericStruct()) {
    for (auto *D : SD->getStoredProperties()) {
      auto fieldType = ValTy.getFieldType(D, B.getModule());
      unsigned NumSubElt = getNumSubElements(fieldType, B.getModule());
      
      if (SubElementNumber < NumSubElt) {
        Val = B.emitStructExtract(Loc, Val, D);
        return ExtractSubElement(Val, SubElementNumber, B, Loc);
      }
      
      SubElementNumber -= NumSubElt;
      
    }
    llvm_unreachable("Didn't find field");
  }
  
  // Otherwise, we're down to a scalar.
  assert(SubElementNumber == 0 && "Miscalculation indexing subelements");
  return Val;
}

//===----------------------------------------------------------------------===//
//                          Allocation Optimization
//===----------------------------------------------------------------------===//

namespace {
  /// AllocOptimize - This performs load promotion and deletes synthesized
  /// allocations if all loads can be removed.
  class AllocOptimize {
    SILModule &Module;
    
    /// TheMemory - This is either an alloc_box or alloc_stack instruction.
    SILInstruction *TheMemory;
    
    /// This is the SILType of the memory object.
    SILType MemoryType;
    
    /// The number of primitive subelements across all elements of this memory
    /// value.
    unsigned NumMemorySubElements;
    
    SmallVectorImpl<DIMemoryUse> &Uses;
    SmallVectorImpl<SILInstruction*> &Releases;
    
    llvm::SmallPtrSet<SILBasicBlock*, 32> HasLocalDefinition;
    
    /// This is a map of uses that are not loads (i.e., they are Stores,
    /// InOutUses, and Escapes), to their entry in Uses.
    llvm::SmallDenseMap<SILInstruction*, unsigned, 16> NonLoadUses;
    
    /// Does this value escape anywhere in the function.
    bool HasAnyEscape = false;
    
  public:
    AllocOptimize(SILInstruction *TheMemory,
                  SmallVectorImpl<DIMemoryUse> &Uses,
                  SmallVectorImpl<SILInstruction*> &Releases);
    
    void doIt();
    
  private:
    
    bool promoteLoad(SILInstruction *Inst);
    bool promoteDestroyAddr(DestroyAddrInst *DAI);
    
    // Load promotion.
    bool hasEscapedAt(SILInstruction *I);
    void updateAvailableValues(SILInstruction *Inst,
                               llvm::SmallBitVector &RequiredElts,
                         SmallVectorImpl<std::pair<SILValue, unsigned>> &Result,
                               llvm::SmallBitVector &ConflictingValues);
    void computeAvailableValues(SILInstruction *StartingFrom,
                                llvm::SmallBitVector &RequiredElts,
                        SmallVectorImpl<std::pair<SILValue, unsigned>> &Result);
    void computeAvailableValuesFrom(SILBasicBlock::iterator StartingFrom,
                                    SILBasicBlock *BB,
                                    llvm::SmallBitVector &RequiredElts,
                        SmallVectorImpl<std::pair<SILValue, unsigned>> &Result,
  llvm::SmallDenseMap<SILBasicBlock*, llvm::SmallBitVector, 32> &VisitedBlocks,
                                    llvm::SmallBitVector &ConflictingValues);
    
    void explodeCopyAddr(CopyAddrInst *CAI);
    
    void tryToRemoveDeadAllocation();
  };
} // end anonymous namespace


AllocOptimize::AllocOptimize(SILInstruction *TheMemory,
                             SmallVectorImpl<DIMemoryUse> &Uses,
                             SmallVectorImpl<SILInstruction*> &Releases)
: Module(TheMemory->getModule()), TheMemory(TheMemory), Uses(Uses),
  Releases(Releases) {
  
  // Compute the type of the memory object.
  if (auto *ABI = dyn_cast<AllocBoxInst>(TheMemory))
    MemoryType = ABI->getElementType();
  else {
    assert(isa<AllocStackInst>(TheMemory));
    MemoryType = cast<AllocStackInst>(TheMemory)->getElementType();
  }
  
  NumMemorySubElements = getNumSubElements(MemoryType, Module);
  
  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (unsigned ui = 0, e = Uses.size(); ui != e; ++ui) {
    auto &Use = Uses[ui];
    assert(Use.Inst && "No instruction identified?");
    
    // Keep track of all the uses that aren't loads.
    if (Use.Kind == DIUseKind::Load)
      continue;
    
    NonLoadUses[Use.Inst] = ui;
    
    HasLocalDefinition.insert(Use.Inst->getParent());
    
    if (Use.Kind == DIUseKind::Escape) {
      // Determine which blocks the value can escape from.  We aren't allowed to
      // promote loads in blocks reachable from an escape point.
      HasAnyEscape = true;
    }
  }
  
  // If isn't really a use, but we account for the alloc_box/mark_uninitialized
  // as a use so we see it in our dataflow walks.
  NonLoadUses[TheMemory] = ~0U;
  HasLocalDefinition.insert(TheMemory->getParent());
}


/// hasEscapedAt - Return true if the box has escaped at the specified
/// instruction.  We are not allowed to do load promotion in an escape region.
bool AllocOptimize::hasEscapedAt(SILInstruction *I) {
  // FIXME: This is not an aggressive implementation.  :)
  
  // TODO: At some point, we should special case closures that just *read* from
  // the escaped value (by looking at the body of the closure).  They should not
  // prevent load promotion, and will allow promoting values like X in regions
  // dominated by "... && X != 0".
  return HasAnyEscape;
}


/// The specified instruction is a non-load access of the element being
/// promoted.  See if it provides a value or refines the demanded element mask
/// used for load promotion.
void AllocOptimize::
updateAvailableValues(SILInstruction *Inst, llvm::SmallBitVector &RequiredElts,
                      SmallVectorImpl<std::pair<SILValue, unsigned>> &Result,
                      llvm::SmallBitVector &ConflictingValues) {
  // Handle store and assign.
  if (isa<StoreInst>(Inst) || isa<AssignInst>(Inst)) {
    unsigned StartSubElt = ComputeAccessPath(Inst->getOperand(1), TheMemory);
    SILType ValTy = Inst->getOperand(0).getType();
    
    for (unsigned i = 0, e = getNumSubElements(ValTy, Module); i != e; ++i) {
      // If this element is not required, don't fill it in.
      if (!RequiredElts[StartSubElt+i]) continue;
      
      // If there is no result computed for this subelement, record it.  If
      // there already is a result, check it for conflict.  If there is no
      // conflict, then we're ok.
      auto &Entry = Result[StartSubElt+i];
      if (Entry.first == SILValue())
        Entry = { Inst->getOperand(0), i };
      else if (Entry.first != Inst->getOperand(0) || Entry.second != i)
        ConflictingValues[StartSubElt+i] = true;
      
      // This element is now provided.
      RequiredElts[StartSubElt+i] = false;
    }
    
    return;
  }
  
  // If we get here with a copy_addr, it must be storing into the element. Check
  // to see if any loaded subelements are being used, and if so, explode the
  // copy_addr to its individual pieces.
  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    unsigned StartSubElt = ComputeAccessPath(Inst->getOperand(1), TheMemory);
    SILType ValTy = Inst->getOperand(1).getType();
    
    bool AnyRequired = false;
    for (unsigned i = 0, e = getNumSubElements(ValTy, Module); i != e; ++i) {
      // If this element is not required, don't fill it in.
      AnyRequired = RequiredElts[StartSubElt+i];
      if (AnyRequired) break;
    }
    
    // If this is a copy addr that doesn't intersect the loaded subelements,
    // just continue with an unmodified load mask.
    if (!AnyRequired)
      return;
    
    // If the copyaddr is of an non-loadable type, we can't promote it.  Just
    // consider it to be a clobber.
    if (CAI->getOperand(0).getType().isLoadable(Module)) {
      // Otherwise, some part of the copy_addr's value is demanded by a load, so
      // we need to explode it to its component pieces.  This only expands one
      // level of the copyaddr.
      explodeCopyAddr(CAI);
      
      // The copy_addr doesn't provide any values, but we've arranged for our
      // iterators to visit the newly generated instructions, which do.
      return;
    }
  }
  
  
  
  // TODO: inout apply's should only clobber pieces passed in.
  
  // Otherwise, this is some unknown instruction, conservatively assume that all
  // values are clobbered.
  RequiredElts.clear();
  ConflictingValues = llvm::SmallBitVector(Result.size(), true);
  return;
}


/// Try to find available values of a set of subelements of the current value,
/// starting right before the specified instruction.
///
/// The bitvector indicates which subelements we're interested in, and result
/// captures the available value (plus an indicator of which subelement of that
/// value is needed).
///
void AllocOptimize::
computeAvailableValues(SILInstruction *StartingFrom,
                       llvm::SmallBitVector &RequiredElts,
                       SmallVectorImpl<std::pair<SILValue, unsigned>> &Result) {
  llvm::SmallDenseMap<SILBasicBlock*, llvm::SmallBitVector, 32> VisitedBlocks;
  llvm::SmallBitVector ConflictingValues(Result.size());
  
  computeAvailableValuesFrom(StartingFrom, StartingFrom->getParent(),
                             RequiredElts, Result, VisitedBlocks,
                             ConflictingValues);
  
  // If we have any conflicting values, explicitly mask them out of the result,
  // so we don't pick one arbitrary available value.
  if (!ConflictingValues.none())
    for (unsigned i = 0, e = Result.size(); i != e; ++i)
      if (ConflictingValues[i])
        Result[i] = { SILValue(), 0U };
  
  return;
}

void AllocOptimize::
computeAvailableValuesFrom(SILBasicBlock::iterator StartingFrom,
                           SILBasicBlock *BB,
                           llvm::SmallBitVector &RequiredElts,
                         SmallVectorImpl<std::pair<SILValue, unsigned>> &Result,
   llvm::SmallDenseMap<SILBasicBlock*, llvm::SmallBitVector, 32> &VisitedBlocks,
                           llvm::SmallBitVector &ConflictingValues) {
  assert(!RequiredElts.none() && "Scanning with a goal of finding nothing?");
  
  // If there is a potential modification in the current block, scan the block
  // to see if the store or escape is before or after the load.  If it is
  // before, check to see if it produces the value we are looking for.
  if (HasLocalDefinition.count(BB)) {
    for (SILBasicBlock::iterator BBI = StartingFrom; BBI != BB->begin();) {
      SILInstruction *TheInst = std::prev(BBI);
      
      // If this instruction is unrelated to the element, ignore it.
      if (!NonLoadUses.count(TheInst)) {
        --BBI;
        continue;
      }
      
      // Given an interesting instruction, incorporate it into the set of
      // results, and filter down the list of demanded subelements that we still
      // need.
      updateAvailableValues(TheInst, RequiredElts, Result, ConflictingValues);
      
      // If this satisfied all of the demanded values, we're done.
      if (RequiredElts.none())
        return;
      
      // Otherwise, keep scanning the block.  If the instruction we were looking
      // at just got exploded, don't skip the next instruction.
      if (&*std::prev(BBI) == TheInst)
        --BBI;
    }
  }
  
  
  // Otherwise, we need to scan up the CFG looking for available values.
  for (auto PI = BB->pred_begin(), E = BB->pred_end(); PI != E; ++PI) {
    SILBasicBlock *PredBB = *PI;
    
    // If the predecessor block has already been visited (potentially due to a
    // cycle in the CFG), don't revisit it.  We can do this safely because we
    // are optimistically assuming that all incoming elements in a cycle will be
    // the same.  If we ever detect a conflicting element, we record it and do
    // not look at the result.
    auto Entry = VisitedBlocks.insert({PredBB, RequiredElts});
    if (!Entry.second) {
      // If we are revisiting a block and asking for different required elements
      // then anything that isn't agreeing is in conflict.
      const auto &PrevRequired = Entry.first->second;
      if (PrevRequired != RequiredElts) {
        ConflictingValues |= (PrevRequired ^ RequiredElts);
        
        RequiredElts &= ~ConflictingValues;
        if (RequiredElts.none())
          return;
      }
      continue;
    }
    
    // Make sure to pass in the same set of required elements for each pred.
    llvm::SmallBitVector Elts = RequiredElts;
    computeAvailableValuesFrom(PredBB->end(), PredBB, Elts, Result,
                               VisitedBlocks, ConflictingValues);
    
    // If we have any conflicting values, don't bother searching for them.
    RequiredElts &= ~ConflictingValues;
    if (RequiredElts.none())
      return;
  }
}


static bool anyMissing(unsigned StartSubElt, unsigned NumSubElts,
                       ArrayRef<std::pair<SILValue, unsigned>> &Values) {
  while (NumSubElts) {
    if (!Values[StartSubElt].first.isValid()) return true;
    ++StartSubElt;
    --NumSubElts;
  }
  return false;
}


/// AggregateAvailableValues - Given a bunch of primitive subelement values,
/// build out the right aggregate type (LoadTy) by emitting tuple and struct
/// instructions as necessary.
static SILValue
AggregateAvailableValues(SILInstruction *Inst, SILType LoadTy,
                         SILValue Address,
                         ArrayRef<std::pair<SILValue, unsigned>> AvailableValues,
                         unsigned FirstElt) {
  assert(LoadTy.isObject());
  SILModule &M = Inst->getModule();
  
  // Check to see if the requested value is fully available, as an aggregate.
  // This is a super-common case for single-element structs, but is also a
  // general answer for arbitrary structs and tuples as well.
  if (FirstElt < AvailableValues.size()) {  // #Elements may be zero.
    SILValue FirstVal = AvailableValues[FirstElt].first;
    if (FirstVal.isValid() && AvailableValues[FirstElt].second == 0 &&
        FirstVal.getType() == LoadTy) {
      // If the first element of this value is available, check any extra ones
      // before declaring success.
      bool AllMatch = true;
      for (unsigned i = 0, e = getNumSubElements(LoadTy, M); i != e; ++i)
        if (AvailableValues[FirstElt+i].first != FirstVal ||
            AvailableValues[FirstElt+i].second != i) {
          AllMatch = false;
          break;
        }
      
      if (AllMatch)
        return FirstVal;
    }
  }
  
  
  SILBuilder B(Inst);
  
  if (TupleType *TT = LoadTy.getAs<TupleType>()) {
    SmallVector<SILValue, 4> ResultElts;
    
    for (unsigned EltNo : indices(TT->getFields())) {
      SILType EltTy = LoadTy.getTupleElementType(EltNo);
      unsigned NumSubElt = getNumSubElements(EltTy, M);
      
      // If we are missing any of the available values in this struct element,
      // compute an address to load from.
      SILValue EltAddr;
      if (anyMissing(FirstElt, NumSubElt, AvailableValues))
        EltAddr = B.createTupleElementAddr(Inst->getLoc(), Address, EltNo,
                                           EltTy.getAddressType());
      
      ResultElts.push_back(AggregateAvailableValues(Inst, EltTy, EltAddr,
                                                    AvailableValues, FirstElt));
      FirstElt += NumSubElt;
    }
    
    return B.createTuple(Inst->getLoc(), LoadTy, ResultElts);
  }
  
  // Extract struct elements.
  if (auto *SD = LoadTy.getStructOrBoundGenericStruct()) {
    SmallVector<SILValue, 4> ResultElts;
    
    for (auto *FD : SD->getStoredProperties()) {
      SILType EltTy = LoadTy.getFieldType(FD, M);
      unsigned NumSubElt = getNumSubElements(EltTy, M);
      
      // If we are missing any of the available values in this struct element,
      // compute an address to load from.
      SILValue EltAddr;
      if (anyMissing(FirstElt, NumSubElt, AvailableValues))
        EltAddr = B.createStructElementAddr(Inst->getLoc(), Address, FD,
                                            EltTy.getAddressType());
      
      ResultElts.push_back(AggregateAvailableValues(Inst, EltTy, EltAddr,
                                                    AvailableValues, FirstElt));
      FirstElt += NumSubElt;
    }
    return B.createStruct(Inst->getLoc(), LoadTy, ResultElts);
  }
  
  // Otherwise, we have a simple primitive.  If the value is available, use it,
  // otherwise emit a load of the value.
  auto Val = AvailableValues[FirstElt];
  if (!Val.first.isValid())
    return B.createLoad(Inst->getLoc(), Address);
  
  SILValue EltVal = ExtractSubElement(Val.first, Val.second, B, Inst->getLoc());
  // It must be the same type as LoadTy if available.
  assert(EltVal.getType() == LoadTy &&
         "Subelement types mismatch");
  return EltVal;
}


/// At this point, we know that this element satisfies the definitive init
/// requirements, so we can try to promote loads to enable SSA-based dataflow
/// analysis.  We know that accesses to this element only access this element,
/// cross element accesses have been scalarized.
///
/// This returns true if the load has been removed from the program.
///
bool AllocOptimize::promoteLoad(SILInstruction *Inst) {
  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.
  
  // We only handle load and copy_addr right now.
  if (auto CAI = dyn_cast<CopyAddrInst>(Inst)) {
    // If this is a CopyAddr, verify that the element type is loadable.  If not,
    // we can't explode to a load.
    if (!CAI->getSrc().getType().isLoadable(Module))
      return false;
  } else if (!isa<LoadInst>(Inst))
    return false;
  
  // If the box has escaped at this instruction, we can't safely promote the
  // load.
  if (hasEscapedAt(Inst))
    return false;
  
  SILType LoadTy = Inst->getOperand(0).getType().getObjectType();
  
  // If this is a load/copy_addr from a struct field that we want to promote,
  // compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned FirstElt = ComputeAccessPath(Inst->getOperand(0), TheMemory);
  unsigned NumLoadSubElements = getNumSubElements(LoadTy, Module);
  
  // Set up the bitvector of elements being demanded by the load.
  llvm::SmallBitVector RequiredElts(NumMemorySubElements);
  RequiredElts.set(FirstElt, FirstElt+NumLoadSubElements);
  
  SmallVector<std::pair<SILValue, unsigned>, 8> AvailableValues;
  AvailableValues.resize(NumMemorySubElements);
  
  // Find out if we have any available values.  If no bits are demanded, we
  // trivially succeed. This can happen when there is a load of an empty struct.
  if (NumLoadSubElements != 0) {
    computeAvailableValues(Inst, RequiredElts, AvailableValues);
    
    // If there are no values available at this load point, then we fail to
    // promote this load and there is nothing to do.
    bool AnyAvailable = false;
    for (unsigned i = FirstElt, e = i+NumLoadSubElements; i != e; ++i)
      if (AvailableValues[i].first.isValid()) {
        AnyAvailable = true;
        break;
      }
    
    if (!AnyAvailable)
      return false;
  }
  
  // Ok, we have some available values.  If we have a copy_addr, explode it now,
  // exposing the load operation within it.  Subsequent optimization passes will
  // see the load and propagate the available values into it.
  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    explodeCopyAddr(CAI);
    
    // This is removing the copy_addr, but explodeCopyAddr takes care of
    // removing the instruction from Uses for us, so we return false.
    return false;
  }
  
  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller) loads for any subelements that were
  // not available.
  auto NewVal = AggregateAvailableValues(Inst, LoadTy, Inst->getOperand(0),
                                         AvailableValues, FirstElt);
  
  ++NumLoadPromoted;
  
  // Simply replace the load.
  assert(isa<LoadInst>(Inst));
  DEBUG(llvm::errs() << "  *** Promoting load: " << *Inst << "\n");
  DEBUG(llvm::errs() << "      To value: " << *NewVal.getDef() << "\n");
  
  SILValue(Inst, 0).replaceAllUsesWith(NewVal);
  SILValue Addr = Inst->getOperand(0);
  Inst->eraseFromParent();
  RemoveDeadAddressingInstructions(Addr);
  return true;
}

/// promoteDestroyAddr - DestroyAddr is a composed operation merging
/// load+strong_release.  If the implicit load's value is available, explode it.
///
/// Note that we handle the general case of a destroy_addr of a piece of the
/// memory object, not just destroy_addrs of the entire thing.
///
bool AllocOptimize::promoteDestroyAddr(DestroyAddrInst *DAI) {
  SILValue Address = DAI->getOperand();
  
  // We cannot promote destroys of address-only types, because we can't expose
  // the load.
  SILType LoadTy = Address.getType().getObjectType();
  if (LoadTy.isAddressOnly(Module))
    return false;
  
  // If the box has escaped at this instruction, we can't safely promote the
  // load.
  if (hasEscapedAt(DAI))
    return false;
  
  // Compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned FirstElt = ComputeAccessPath(Address, TheMemory);
  unsigned NumLoadSubElements = getNumSubElements(LoadTy, Module);
  
  // Set up the bitvector of elements being demanded by the load.
  llvm::SmallBitVector RequiredElts(NumMemorySubElements);
  RequiredElts.set(FirstElt, FirstElt+NumLoadSubElements);
  
  SmallVector<std::pair<SILValue, unsigned>, 8> AvailableValues;
  AvailableValues.resize(NumMemorySubElements);
  
  // Find out if we have any available values.  If no bits are demanded, we
  // trivially succeed. This can happen when there is a load of an empty struct.
  if (NumLoadSubElements != 0) {
    computeAvailableValues(DAI, RequiredElts, AvailableValues);
    
    // If some value is not available at this load point, then we fail.
    for (unsigned i = FirstElt, e = FirstElt+NumLoadSubElements; i != e; ++i)
      if (!AvailableValues[i].first.isValid())
        return false;
  }
  
  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller) loads for any subelements that were
  // not available.
  auto NewVal =
  AggregateAvailableValues(DAI, LoadTy, Address, AvailableValues, FirstElt);
  
  ++NumDestroyAddrPromoted;
  
  DEBUG(llvm::errs() << "  *** Promoting destroy_addr: " << *DAI << "\n");
  DEBUG(llvm::errs() << "      To value: " << *NewVal.getDef() << "\n");
  
  SILBuilder(DAI).emitDestroyValueOperation(DAI->getLoc(), NewVal);
  DAI->eraseFromParent();
  return true;
}



/// Explode a copy_addr instruction of a loadable type into lower level
/// operations like loads, stores, retains, releases, copy_value, etc.
void AllocOptimize::explodeCopyAddr(CopyAddrInst *CAI) {
  DEBUG(llvm::errs() << "  -- Exploding copy_addr: " << *CAI << "\n");
  
  SILType ValTy = CAI->getDest().getType().getObjectType();
  auto &TL = Module.getTypeLowering(ValTy);
  
  // Keep track of the new instructions emitted.
  SmallVector<SILInstruction*, 4> NewInsts;
  SILBuilder B(CAI, &NewInsts);
  
  // Use type lowering to lower the copyaddr into a load sequence + store
  // sequence appropriate for the type.
  SILValue StoredValue = TL.emitLoadOfCopy(B, CAI->getLoc(), CAI->getSrc(),
                                           CAI->isTakeOfSrc());
  
  TL.emitStoreOfCopy(B, CAI->getLoc(), StoredValue, CAI->getDest(),
                     CAI->isInitializationOfDest());
  
  
  // Next, remove the copy_addr itself.
  CAI->eraseFromParent();
  
  // Update our internal state for this being gone.
  NonLoadUses.erase(CAI);
  
  // Remove the copy_addr from Uses.  A single copy_addr can appear multiple
  // times if the source and dest are to elements within a single aggregate, but
  // we only want to pick up the CopyAddrKind from the store.
  DIMemoryUse LoadUse, StoreUse;
  for (auto &Use : Uses) {
    if (Use.Inst != CAI) continue;
    
    if (Use.Kind == DIUseKind::Load) {
      assert(LoadUse.isInvalid());
      LoadUse = Use;
    } else {
      assert(StoreUse.isInvalid());
      StoreUse = Use;
    }
    
    Use.Inst = nullptr;
    
    // Keep scanning in case the copy_addr appears multiple times.
  }
  
  assert((LoadUse.isValid() || StoreUse.isValid()) &&
         "we should have a load or a store, possibly both");
  assert(StoreUse.isInvalid() || StoreUse.Kind == Assign ||
         StoreUse.Kind == PartialStore || StoreUse.Kind == Initialization);
  
  // Now that we've emitted a bunch of instructions, including a load and store
  // but also including other stuff, update the internal state of
  // LifetimeChecker to reflect them.
  
  // Update the instructions that touch the memory.  NewInst can grow as this
  // iterates, so we can't use a foreach loop.
  for (auto *NewInst : NewInsts) {
    switch (NewInst->getKind()) {
      default:
        NewInst->dump();
        assert(0 && "Unknown instruction generated by copy_addr lowering");
        
      case ValueKind::StoreInst:
        // If it is a store to the memory object (as oppose to a store to
        // something else), track it as an access.
        if (StoreUse.isValid()) {
          StoreUse.Inst = NewInst;
          NonLoadUses[NewInst] = Uses.size();
          Uses.push_back(StoreUse);
        }
        continue;
        
      case ValueKind::LoadInst:
        // If it is a load from the memory object (as oppose to a load from
        // something else), track it as an access.  We need to explictly check to
        // see if the load accesses "TheMemory" because it could either be a load
        // for the copy_addr source, or it could be a load corresponding to the
        // "assign" operation on the destination of the copyaddr.
        if (LoadUse.isValid() &&
            getAccessPathRoot(NewInst->getOperand(0)).getDef() == TheMemory) {
          LoadUse.Inst = NewInst;
          Uses.push_back(LoadUse);
        }
        continue;
        
      case ValueKind::CopyValueInst:
      case ValueKind::StrongRetainInst:
      case ValueKind::StrongReleaseInst:
      case ValueKind::UnownedRetainInst:
      case ValueKind::UnownedReleaseInst:
      case ValueKind::DestroyValueInst:   // Destroy overwritten value
        // These are ignored.
        continue;
    }
  }
}




static void eraseUsesOfInstruction(SILInstruction *Inst) {
  for (auto UI : Inst->getUses()) {
    auto *User = UI->getUser();

    // If the instruction itself has any uses, recursively zap them so that
    // nothing uses this instruction.
    eraseUsesOfInstruction(User);

    // Walk through the operand list and delete any random instructions that
    // will become trivially dead when this instruction is removed.

    for (auto &Op : User->getAllOperands()) {
      if (auto *OpI = dyn_cast<SILInstruction>(Op.get().getDef())) {
        // Don't recursively delete the pointer we're getting in.
        if (OpI != Inst) {
          Op.drop();
          recursivelyDeleteTriviallyDeadInstructions(OpI);
        }
      }
    }

    User->eraseFromParent();
  }
}

/// tryToRemoveDeadAllocation - If the allocation is an autogenerated allocation
/// that is only stored to (after load promotion) then remove it completely.
void AllocOptimize::tryToRemoveDeadAllocation() {
  assert((isa<AllocBoxInst>(TheMemory) || isa<AllocStackInst>(TheMemory)) &&
         "Unhandled allocation case");

  // We don't want to remove allocations that are required for useful debug
  // information at -O0.  As such, we only remove allocations if:
  //
  // 1. They are in a transparent function.
  // 2. They are in a normal function, but didn't come from a VarDecl, or came
  //    from one that was autogenerated or inlined from a transparent function.
  SILLocation Loc = TheMemory->getLoc();
  if (!TheMemory->getFunction()->isTransparent() &&
      Loc.getAsASTNode<VarDecl>() && !Loc.isAutoGenerated() &&
      !Loc.is<MandatoryInlinedLocation>())
    return;

  // Check the uses list to see if there are any non-store uses left over after
  // load promotion and other things DI does.
  for (auto &U : Uses) {
    // Ignore removed instructions.
    if (U.Inst == nullptr) continue;

    switch (U.Kind) {
    case DIUseKind::Assign:
    case DIUseKind::PartialStore:
    case DIUseKind::InitOrAssign:
      break;    // These don't prevent removal.
    case DIUseKind::Initialization:
      if (!isa<ApplyInst>(U.Inst))
        break;
      // FALL THROUGH.
    case DIUseKind::Load:
    case DIUseKind::IndirectIn:
    case DIUseKind::InOutUse:
    case DIUseKind::Escape:
      DEBUG(llvm::errs() << "*** Failed to remove autogenerated alloc: "
            "kept alive by: " << *U.Inst);
      return;   // These do prevent removal.
    }
  }

  // If the memory object has non-trivial type, then removing the deallocation
  // will drop any releases.  Check that there is nothing preventing removal.
  if (!MemoryType.isTrivial(Module)) {
    for (auto *R : Releases) {
      if (R == nullptr || isa<DeallocStackInst>(R) || isa<DeallocBoxInst>(R))
        continue;

      DEBUG(llvm::errs() << "*** Failed to remove autogenerated alloc: "
            "kept alive by release: " << *R);
      return;
    }
  }

  DEBUG(llvm::errs() << "*** Removing autogenerated alloc_stack: "<<*TheMemory);

  // If it is safe to remove, do it.  Recursively remove all instructions
  // hanging off the allocation instruction, then return success.  Let the
  // caller remove the allocation itself to avoid iterator invalidation.
  eraseUsesOfInstruction(TheMemory);
}

/// doIt - returns true on error.
void AllocOptimize::doIt() {
  // If we've successfully checked all of the definitive initialization
  // requirements, try to promote loads.  This can explode copy_addrs, so the
  // use list may change size.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &Use = Uses[i];
    // Ignore entries for instructions that got expanded along the way.
    if (Use.Inst && Use.Kind == DIUseKind::Load)
      if (promoteLoad(Use.Inst))
        Uses[i].Inst = nullptr;  // remove entry if load got deleted.
  }
  
  // destroy_addr(p) is strong_release(load(p)), try to promote it too.
  for (unsigned i = 0; i != Releases.size(); ++i) {
    if (auto *DAI = dyn_cast_or_null<DestroyAddrInst>(Releases[i]))
      if (promoteDestroyAddr(DAI)) {
        // remove entry if destroy_addr got deleted.
        Releases[i] = nullptr;
      }
  }
  
  // If this is an allocation, try to remove it completely.
  if (!isa<MarkUninitializedInst>(TheMemory))
    tryToRemoveDeadAllocation();
 }


static void optimizeMemoryAllocations(SILFunction &Fn) {
  for (auto &BB : Fn) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I;
      if (isa<AllocBoxInst>(Inst) || isa<AllocStackInst>(Inst)) {
        DEBUG(llvm::errs() << "*** DI Optimize looking at: " << *Inst << "\n");
        
        // Set up the datastructure used to collect the uses of the allocation.
        SmallVector<DIMemoryUse, 16> Uses;
        SmallVector<SILInstruction*, 4> Releases;
        
        // Walk the use list of the pointer, collecting them.
        ElementUseCollector(Uses, Releases, true).collectFromAllocation(Inst);
        
        AllocOptimize(Inst, Uses, Releases).doIt();
        
        // Carefully move iterator to avoid invalidation problems.
        ++I;
        if (Inst->use_empty()) {
          Inst->eraseFromParent();
          ++NumAllocRemoved;
        }
        continue;
      }
      ++I;
    }
  }

}


//===----------------------------------------------------------------------===//
//                           Top Level Driver
//===----------------------------------------------------------------------===//

static void processAllocation(SILInstruction *I) {
  assert(isa<AllocBoxInst>(I) || isa<AllocStackInst>(I));
  DEBUG(llvm::errs() << "*** Definite Init looking at: " << *I << "\n");

  // Set up the datastructure used to collect the uses of the allocation.
  SmallVector<DIMemoryUse, 16> Uses;
  SmallVector<SILInstruction*, 4> Releases;

  // Walk the use list of the pointer, collecting them into the Uses array.
  ElementUseCollector(Uses, Releases, false).collectFromAllocation(I);

  LifetimeChecker(I, Uses, Releases).doIt();
}

static void processMarkUninitialized(MarkUninitializedInst *MUI) {
  DEBUG(llvm::errs() << "*** Definite Init looking at: " << *MUI << "\n");
  
  // Set up the datastructure used to collect the uses of the
  // mark_uninitialized.
  SmallVector<DIMemoryUse, 16> Uses;
  SmallVector<SILInstruction*, 4> Releases;

  // Walk the use list of the pointer, collecting them into the Uses array.
  ElementUseCollector(Uses, Releases, false).collectFromMarkUninitialized(MUI);

  assert(Releases.empty() && "Shouldn't have releases of MUIs");
  LifetimeChecker(MUI, Uses, Releases).doIt();
}


/// checkDefiniteInitialization - Check that all memory objects that require
/// initialization before use are properly set and transform the code as
/// required for flow-sensitive properties.
static void checkDefiniteInitialization(SILFunction &Fn) {
  for (auto &BB : Fn) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I;
      if (isa<AllocBoxInst>(Inst) || isa<AllocStackInst>(Inst)) {
        processAllocation(Inst);
        
        // Carefully move iterator to avoid invalidation problems.
        ++I;
        if (Inst->use_empty()) {
          Inst->eraseFromParent();
          ++NumAllocRemoved;
        }
        continue;
      }

      if (auto *MUI = dyn_cast<MarkUninitializedInst>(Inst))
        processMarkUninitialized(MUI);

      ++I;
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
    
    optimizeMemoryAllocations(Fn);
    Fn.verify();
  }
}
