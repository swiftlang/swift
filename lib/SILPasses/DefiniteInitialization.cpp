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
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
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
                                   bool isInitialization) {
  DEBUG(llvm::errs() << "  *** Lowering [isInit=" << isInitialization << "]: "
            << *Inst << "\n");

  ++NumAssignRewritten;

  auto &M = Inst->getModule();
  SILValue Src = Inst->getSrc();

  // If this is an initialization, or the storage type is trivial, we
  // can just replace the assignment with a store.

  // Otherwise, if it has trivial type, we can always just replace the
  // assignment with a store.  If it has non-trivial type and is an
  // initialization, we can also replace it with a store.
  if (isInitialization || Inst->getDest().getType().isTrivial(M)) {
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



//===----------------------------------------------------------------------===//
// Tuple Element Flattening/Counting Logic
//===----------------------------------------------------------------------===//

/// getTupleElementCount - Return the number of elements in the flattened
/// SILType.  For tuples, this is the (recursive) count of the fields it
/// contains.
static unsigned getTupleElementCount(CanType T) {
  CanTupleType TT = dyn_cast<TupleType>(T);

  // If this isn't a tuple, it is a single element.
  if (!TT) return 1;

  unsigned NumElements = 0;
  for (auto EltTy : TT.getElementTypes())
    NumElements += getTupleElementCount(EltTy);
  return NumElements;
}

#if 0
/// Given a symbolic element number, return the type of the element.
static CanType getTupleElementType(CanType T, unsigned EltNo) {
  TupleType *TT = T->getAs<TupleType>();

  // If this isn't a tuple, it is a leaf element.
  if (!TT) {
    assert(EltNo == 0);
    return T;
  }

  for (auto &Elt : TT->getFields()) {
    auto FieldType = Elt.getType()->getCanonicalType();
    unsigned NumFields = getTupleElementCount(FieldType);
    if (EltNo < NumFields)
      return getTupleElementType(FieldType, EltNo);
    EltNo -= NumFields;
  }

  assert(0 && "invalid element number");
  abort();
}
#endif

/// Push the symbolic path name to the specified element number onto the
/// specified std::string.
static void getPathStringToTupleElement(CanType T, unsigned Element,
                                        std::string &Result) {
  CanTupleType TT = dyn_cast<TupleType>(T);
  if (!TT) return;

  unsigned FieldNo = 0;
  for (auto &Field : TT->getFields()) {
    CanType FieldTy(Field.getType());
    unsigned ElementsForField = getTupleElementCount(FieldTy);
    
    if (Element < ElementsForField) {
      Result += '.';
      if (Field.hasName())
        Result += Field.getName().str();
      else
        Result += llvm::utostr(FieldNo);
      return getPathStringToTupleElement(FieldTy, Element, Result);
    }
    
    Element -= ElementsForField;
    
    ++FieldNo;
  }
  assert(0 && "Element number is out of range for this type!");
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
// Access Path Analysis Logic
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

/// Compute the access path indicated by the specified pointer (which is derived
/// from the root by a series of tuple/struct element addresses) and return
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
// Per-Element Promotion Logic
//===----------------------------------------------------------------------===//

namespace {
  enum UseKind {
    // The instruction is a Load.
    Load,

    // The instruction is a Store.
    Store,

    // The instruction is a store to a member of a larger struct value.
    PartialStore,

    /// An indirecet 'inout' parameter of an Apply instruction.
    InOutUse,

    /// An indirect 'in' parameter of an Apply instruction.
    IndirectIn,

    /// An out parameter of an Apply instruction.  This is like a
    /// store except it cannot be removed.
    IndirectResult,

    /// This instruction is a general escape of the value, e.g. a call to a
    /// closure that captures it.
    Escape
  };
} // end anonymous namespace


namespace {
  /// This struct represents a single classified access to the memory object
  /// being analyzed, along with classification information about the access.
  struct MemoryUse {
    /// This is the instruction accessing the memory.
    SILInstruction *Inst;

    /// This is what kind of access it is, load, store, escape, etc.
    UseKind Kind;

    /// For memory objects of (potentially recursive) tuple type, this keeps
    /// track of which tuple elements are affected.
    unsigned short FirstTupleElement, NumTupleElements;

    MemoryUse(SILInstruction *Inst, UseKind Kind, unsigned FTE, unsigned NTE)
      : Inst(Inst), Kind(Kind),FirstTupleElement(FTE), NumTupleElements(NTE) {
        assert(FTE == FirstTupleElement && NumTupleElements == NTE &&
               "more than 65K tuple elements not supported yet");
    }

    MemoryUse() : Inst(nullptr) {}

    bool isInvalid() const { return Inst == nullptr; }
    bool isValid() const { return Inst != nullptr; }
  };
} // end anonymous namespace


enum class EscapeKind {
  Unknown,
  Yes,
  No
};

enum class DIKind {
  No,
  Yes,
  Partial
};


namespace {
  /// LiveOutBlockState - Keep track of information about blocks that have
  /// already been analyzed.  Since this is a global analysis, we need this to
  /// cache information about different paths through the CFG.
  struct LiveOutBlockState {
    /// For this block, keep track of whether there is a path from the entry
    /// of the function to the end of the block that crosses an escape site.
    EscapeKind EscapeInfo : 2;

    /// Keep track of whether there is a Store, InOutUse, or Escape locally in
    /// this block.
    bool HasNonLoadUse : 1;

    /// Keep track of whether the element is live out of this block or not. This
    /// is only fully set when LOState==IsKnown.  In other states, this may only
    /// contain local availability information.
    ///
    llvm::SmallBitVector TupleElementAvailability;

    enum LiveOutStateTy {
      IsUnknown,
      IsComputingLiveOut,
      IsKnown
    } LOState : 2;

    LiveOutBlockState(unsigned NumTupleElements)
      : EscapeInfo(EscapeKind::Unknown), HasNonLoadUse(false),
        LOState(IsUnknown) {
      TupleElementAvailability.resize(NumTupleElements*2, true);
    }

    DIKind getAvailability(unsigned Elt) {
      return getAvailability(Elt, TupleElementAvailability);
    }

    static Optional<DIKind> getAvailabilityField(unsigned Elt,
                                                 llvm::SmallBitVector &BV) {
      // T,T -> Nothing/Unknown
      // F,F -> No
      // F,T -> Yes
      // T,F -> Partial
      bool V1 = BV[Elt*2], V2 = BV[Elt*2+1];
      if (V1 == V2)
        return V1 ? Optional<DIKind>(Nothing) : DIKind::No;
      return V2 ? DIKind::Yes : DIKind::Partial;
    }
    Optional<DIKind> getAvailabilityField(unsigned Elt) {
      return getAvailabilityField(Elt, TupleElementAvailability);
    }

    static DIKind getAvailability(unsigned Elt, llvm::SmallBitVector &BV) {
      // This will assert if nothing is set, the client shouldn't call this when
      // that is possible.
      return getAvailabilityField(Elt, BV).getValue();
    }

    static void setAvailability(unsigned Elt, DIKind K,
                                llvm::SmallBitVector &BV) {
      switch (K) {
      case DIKind::No:      BV[Elt*2] = false; BV[Elt*2+1] = false; break;
      case DIKind::Yes:     BV[Elt*2] = false, BV[Elt*2+1] = true; break;
      case DIKind::Partial: BV[Elt*2] = true, BV[Elt*2+1] = false; break;
      }
    }
    void setAvailability(unsigned Elt, DIKind K) {
      setAvailability(Elt, K, TupleElementAvailability);
    }

    void setBlockAvailability(const llvm::SmallBitVector &BV) {
      assert(LOState != IsKnown &&"Changing live out state of computed block?");
      TupleElementAvailability = BV;
      LOState = LiveOutBlockState::IsKnown;

#ifndef NDEBUG
      // Check that we didn't get any unknown values.
      for (unsigned i = 0, e = BV.size()/2; i != e; ++i)
        assert(getAvailabilityField(i).hasValue() &&
               "Set block to unknown value");
#endif
    }

    void setBlockAvailability1(DIKind K) {
      assert(LOState != IsKnown &&"Changing live out state of computed block?");
      assert(TupleElementAvailability.size() == 2 && "Not 1 element case");
      setAvailability(0, K, TupleElementAvailability);
      LOState = LiveOutBlockState::IsKnown;
    }

    void markAvailable(const MemoryUse &Use) {
      // If the memory object has nothing in it (e.g., is an empty tuple)
      // ignore.
      if (TupleElementAvailability.empty()) return;
      
      // Peel the first iteration of the 'set' loop since there is almost always
      // a single tuple element touched by a MemoryUse.
      setAvailability(Use.FirstTupleElement, DIKind::Yes);
                         
      for (unsigned i = 1; i != Use.NumTupleElements; ++i)
        setAvailability(Use.FirstTupleElement+i, DIKind::Yes);
    }
  };
} // end anonymous namespace

namespace {
  /// ElementPromotion - This is the main heavy lifting for processing the uses
  /// of an element of an allocation.
  class ElementPromotion {
    /// TheMemory - This is either an alloc_box instruction or a
    /// mark_uninitialized instruction.  This represents the start of the
    /// lifetime of the value being analyzed.
    SILInstruction *TheMemory;

    /// This is the SILType of the memory object.
    SILType MemoryType;

    /// The number of tuple elements in this memory object.
    unsigned NumTupleElements;
    
    /// The number of primitive subelements across all elements of this memory
    /// value.
    unsigned NumMemorySubElements;

    SmallVectorImpl<MemoryUse> &Uses;
    SmallVectorImpl<SILInstruction*> &Releases;

    llvm::SmallDenseMap<SILBasicBlock*, LiveOutBlockState, 32> PerBlockInfo;

    /// This is a map of uses that are not loads (i.e., they are Stores,
    /// InOutUses, and Escapes), to their entry in Uses.
    llvm::SmallDenseMap<SILInstruction*, unsigned, 16> NonLoadUses;

    /// Does this value escape anywhere in the function.
    bool HasAnyEscape = false;

    // Keep track of whether we've emitted an error.  We only emit one error per
    // location as a policy decision.
    std::vector<SILLocation> EmittedErrorLocs;
  public:
    ElementPromotion(SILInstruction *TheMemory,
                     SmallVectorImpl<MemoryUse> &Uses,
                     SmallVectorImpl<SILInstruction*> &Releases);

    bool doIt();

  private:

    LiveOutBlockState &getBlockInfo(SILBasicBlock *BB) {
      return PerBlockInfo.insert({BB,
                        LiveOutBlockState(NumTupleElements)}).first->second;
    }
    
    DIKind checkDefinitelyInit(const MemoryUse &Use);

    void handleStoreUse(MemoryUse &InstInfo, DIKind Kind);

    bool processNonTrivialRelease(SILInstruction *Inst);

    bool promoteLoad(SILInstruction *Inst);
    bool promoteDestroyAddr(DestroyAddrInst *DAI);

    Optional<DIKind> getLiveOut1(SILBasicBlock *BB);
    void getPredsLiveOut1(SILBasicBlock *BB, Optional<DIKind> &Result);
    llvm::SmallBitVector getLiveOutN(SILBasicBlock *BB);
    void getPredsLiveOutN(SILBasicBlock *BB, llvm::SmallBitVector &Result);

    void diagnoseInitError(const MemoryUse &Use, Diag<StringRef> DiagMessage);

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


ElementPromotion::ElementPromotion(SILInstruction *TheMemory,
                                   SmallVectorImpl<MemoryUse> &Uses,
                                   SmallVectorImpl<SILInstruction*> &Releases)
  : TheMemory(TheMemory), Uses(Uses), Releases(Releases) {

  // Compute the type of the memory object.
  if (auto *ABI = dyn_cast<AllocBoxInst>(TheMemory))
    MemoryType = ABI->getElementType();
  else if (auto *ASI = dyn_cast<AllocStackInst>(TheMemory))
    MemoryType = ASI->getElementType();
  else {
    assert(isa<MarkUninitializedInst>(TheMemory) && "Unknown memory object");
    MemoryType = TheMemory->getType(0).getObjectType();
  }

  NumTupleElements = getTupleElementCount(MemoryType.getSwiftRValueType());
  NumMemorySubElements = getNumSubElements(MemoryType, TheMemory->getModule());

  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (unsigned ui = 0, e = Uses.size(); ui != e; ++ui) {
    auto &Use = Uses[ui];
    assert(Use.Inst && "No instruction identified?");

    // Keep track of all the uses that aren't loads.
    if (Use.Kind == UseKind::Load)
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
    bool AllSet = true;
    for (unsigned i = 0, e = NumTupleElements; i != e; ++i) {
      auto Val = BBInfo.getAvailabilityField(i);
      if (!Val.hasValue() || Val.getValue() != DIKind::Yes)
        AllSet = false;
    }
    if (AllSet)
      BBInfo.LOState = LiveOutBlockState::IsKnown;

    if (Use.Kind == UseKind::Escape) {
      // Determine which blocks the value can escape from.  We aren't allowed to
      // promote loads in blocks reachable from an escape point.
      HasAnyEscape = true;
      BBInfo.EscapeInfo = EscapeKind::Yes;
    }
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
  for (unsigned i = 0, e = NumTupleElements; i != e; ++i)
    if (!MemBBInfo.getAvailabilityField(i).hasValue())
      MemBBInfo.setAvailability(i, DIKind::No);
    
  MemBBInfo.LOState = LiveOutBlockState::IsKnown;
}

void ElementPromotion::diagnoseInitError(const MemoryUse &Use,
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
  getPathStringToTupleElement(AllocTy, Use.FirstTupleElement, Name);

  diagnose(Inst->getModule(), Inst->getLoc(), DiagMessage, Name);

  // Provide context as note diagnostics.

  // TODO: The QoI could be improved in many different ways here.  For example,
  // We could give some path information where the use was uninitialized, like
  // the static analyzer.
  diagnose(Inst->getModule(), TheMemory->getLoc(), diag::variable_defined_here);
}

static bool isStoreObviouslyAnInitialization(SILInstruction *Inst) {
  if (isa<AssignInst>(Inst))
    return false;
  
  else if (auto CA = dyn_cast<CopyAddrInst>(Inst)) {
    if (CA->isInitializationOfDest()) return true;
  } else if (auto SW = dyn_cast<StoreWeakInst>(Inst)) {
    if (SW->isInitializationOfDest()) return true;
  } else if (isa<InitExistentialInst>(Inst) ||
             isa<UpcastExistentialInst>(Inst) ||
             isa<EnumDataAddrInst>(Inst) ||
             isa<InjectEnumAddrInst>(Inst)) {
    // These instructions *on a box* are only formed by direct initialization
    // like "var x : Proto = foo".
    return true;
  } else {
    return true;
  }

  return false;
}

/// doIt - returns true on error.
bool ElementPromotion::doIt() {
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

    // We assume that SILGen knows what it is doing when it produces
    // initializations of variables, because it only produces them when it knows
    // they are correct, and this is a super common case for "var x = 4" cases.
    if (Use.Kind == UseKind::IndirectResult ||
        (Use.Kind == UseKind::Store &&
         isStoreObviouslyAnInitialization(Inst)))
      continue;
    
    // Check to see if the value is known-initialized here or not.
    DIKind DI = checkDefinitelyInit(Use);
    
    switch (Use.Kind) {
    case UseKind::IndirectResult:
    case UseKind::Store:
    case UseKind::PartialStore:
      handleStoreUse(Use, DI);
      break;

    case UseKind::IndirectIn:
    case UseKind::Load:
      // If the value is not definitively initialized, emit an error.
      // TODO: In the "No" case, we can emit a fixit adding a default
      // initialization of the type.
      // TODO: In the "partial" case, we can produce a more specific diagnostic
      // indicating where the control flow merged.
      if (DI != DIKind::Yes) {
        // Otherwise, this is a use of an uninitialized value.  Emit a
        // diagnostic.
        diagnoseInitError(Use, diag::variable_used_before_initialized);
      }
      break;
      
    case UseKind::InOutUse:
      if (DI != DIKind::Yes) {
        // This is a use of an uninitialized value.  Emit a diagnostic.
        diagnoseInitError(Use, diag::variable_inout_before_initialized);
      }
      break;
      
    case UseKind::Escape:
      if (DI != DIKind::Yes) {
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
  if (!EmittedErrorLocs.empty()) return true;

  // If the memory object has nontrivial type, then any destroy/release of the
  // memory object will destruct the memory.  If the memory (or some element
  // thereof) is not initialized on some path, the bad things happen.  Process
  // releases to adjust for this.
  if (!MemoryType.isTrivial(TheMemory->getModule())) {
    for (unsigned i = 0; i != Releases.size(); ++i) {
      if (processNonTrivialRelease(Releases[i])) {
        Releases[i] = Releases.back();
        Releases.pop_back();
        --i;
        continue;
      }

      // FIXME: processNonTrivialRelease shouldn't generate diagnostics.
      if (!EmittedErrorLocs.empty()) return true;
    }
  }


  // If we've successfully checked all of the definitive initialization
  // requirements, try to promote loads.  This can explode copy_addrs, so the
  // use list may change size.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &Use = Uses[i];
    // Ignore entries for instructions that got expanded along the way.
    if (Use.Inst && Use.Kind == UseKind::Load)
      if (promoteLoad(Use.Inst))
        Uses[i].Inst = nullptr;  // remove entry if load got deleted.
  }

  // destroy_addr(p) is strong_release(load(p)), try to promote it too.
  for (unsigned i = 0; i != Releases.size(); ++i) {
    if (auto *DAI = dyn_cast<DestroyAddrInst>(Releases[i]))
      if (promoteDestroyAddr(DAI)) {
        // remove entry if destroy_addr got deleted.
        Releases[i] = Releases.back();
        Releases.pop_back();
        --i;
      }
  }

  // If this is an allocation, try to remove it completely.
  if (!isa<MarkUninitializedInst>(TheMemory))
    tryToRemoveDeadAllocation();

  return false;
}

void ElementPromotion::
handleStoreUse(MemoryUse &InstInfo, DIKind DI) {
  SILInstruction *Inst = InstInfo.Inst;

  // If this is a partial store into a struct and the whole struct hasn't been
  // initialized, diagnose this as an error.
  if (InstInfo.Kind == UseKind::PartialStore && DI != DIKind::Yes) {
    diagnoseInitError(InstInfo, diag::struct_not_fully_initialized);
    return;
  }

  // If it is initialized on some paths, but not others, then we have an
  // inconsistent initialization error.
  //
  // FIXME: This needs to be supported through the introduction of a boolean
  // control path, or (for reference types as an important special case) a store
  // of zero at the definition point.
  if (DI == DIKind::Partial) {
    diagnoseInitError(InstInfo, diag::variable_initialized_on_some_paths);
    return;
  }

  // If this is a copy_addr or store_weak, we just set the initialization bit
  // depending on what we find.
  if (auto *CA = dyn_cast<CopyAddrInst>(Inst)) {
    CA->setIsInitializationOfDest(IsInitialization_t(DI == DIKind::No));
    return;
  }
  if (auto *SW = dyn_cast<StoreWeakInst>(Inst)) {
    SW->setIsInitializationOfDest(IsInitialization_t(DI == DIKind::No));
    return;
  }

  // If this is an assign, rewrite it based on whether it is an initialization
  // or not.
  if (auto *AI = dyn_cast<AssignInst>(Inst)) {
    // Remove this instruction from our data structures, since we will be
    // removing it.
    InstInfo.Inst = nullptr;
    NonLoadUses.erase(Inst);

    unsigned FirstTupleElement = InstInfo.FirstTupleElement;
    unsigned NumTupleElements = InstInfo.NumTupleElements;

    SmallVector<SILInstruction*, 8> InsertedInsts;
    SILBuilder B(Inst, &InsertedInsts);

    LowerAssignInstruction(B, AI, DI == DIKind::No);

    // If lowering of the assign introduced any new stores, keep track of them.
    for (auto I : InsertedInsts) {
      if (isa<StoreInst>(I)) {
        NonLoadUses[I] = Uses.size();
        Uses.push_back(MemoryUse(I, Store,
                                 FirstTupleElement, NumTupleElements));
      } else if (isa<LoadInst>(I)) {
        Uses.push_back(MemoryUse(I, Load, FirstTupleElement, NumTupleElements));
      }
    }
  }
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
bool ElementPromotion::processNonTrivialRelease(SILInstruction *Inst) {
  // If the instruction is a deallocation of uninitialized memory, no action is
  // required (or desired).
  if (isa<DeallocStackInst>(Inst) || isa<DeallocBoxInst>(Inst))
    return false;

  // If the memory object is completely initialized, then nothing needs to be
  // done at this release point.
  DIKind DI =
    checkDefinitelyInit(MemoryUse(Inst, UseKind::Escape, 0, NumTupleElements));
  if (DI == DIKind::Yes) return false;

  // If the memory is definitely not initialized at this point, we can just drop
  // destory_addrs.
  if (DI == DIKind::No && isa<DestroyAddrInst>(Inst)) {
    SILValue Addr = Inst->getOperand(0);
    Inst->eraseFromParent();
    RemoveDeadAddressingInstructions(Addr);
    return true;
  }


  // If the element type of the memory object is a class value
  

  // Okay, the release is conditionally live.  We have to force it up the CFG to
  // a place where we have unconditional liveness, and if the memory object is a
  // tuple, we have to do so for each element individually.

  /// TODO: We could make this more powerful to directly support these
  /// cases, at least when the value doesn't escape.
  ///
  /// When this gets fixed, the code in the ~ElementUseCollector() method
  /// can be removed.
  ///

  // This is a release of an uninitialized value.  Emit a diagnostic.
  diagnoseInitError(MemoryUse(Inst, UseKind::Load, 0, 0),
                    diag::variable_destroyed_before_initialized);
  return false;
}



Optional<DIKind> ElementPromotion::getLiveOut1(SILBasicBlock *BB) {
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
  Optional<DIKind> Result = BBState.getAvailabilityField(0);
  getPredsLiveOut1(BB, Result);

  // Otherwise, we're golden.  Return success.
  getBlockInfo(BB).setBlockAvailability1(Result.getValue());
  return Result.getValue();
}

void ElementPromotion::getPredsLiveOut1(SILBasicBlock *BB,
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

llvm::SmallBitVector ElementPromotion::getLiveOutN(SILBasicBlock *BB) {
  LiveOutBlockState &BBState = getBlockInfo(BB);
  switch (BBState.LOState) {
  case LiveOutBlockState::IsKnown:
    return BBState.TupleElementAvailability;
  case LiveOutBlockState::IsComputingLiveOut:
    // Speculate that it will be live out in cyclic cases.
    return llvm::SmallBitVector(NumTupleElements*2, true);
  case LiveOutBlockState::IsUnknown:
    // Otherwise, process this block.
    break;
  }
  
  // Set the block's state to reflect that we're currently processing it.  This
  // is required to handle cycles properly.
  BBState.LOState = LiveOutBlockState::IsComputingLiveOut;

  llvm::SmallBitVector Result = BBState.TupleElementAvailability;
  getPredsLiveOutN(BB, Result);

  // Finally, cache and return our result.
  getBlockInfo(BB).setBlockAvailability(Result);
  return Result;
}

void ElementPromotion::getPredsLiveOutN(SILBasicBlock *BB,
                                        llvm::SmallBitVector &Result) {
  // The liveness of this block is the intersection of all of the predecessor
  // block's liveness.
  llvm::SmallBitVector Tmp;

  // Recursively processes all of our predecessor blocks.  If any of them is
  // not live out, then we aren't either.
  for (auto P : BB->getPreds()) {
    Tmp = getLiveOutN(P);

    for (unsigned i = 0, e = NumTupleElements; i != e; ++i) {
      Optional<DIKind> TK = LiveOutBlockState::getAvailabilityField(i, Tmp);

      // If T is unset, ignore it.
      if (!TK.hasValue())
        continue;

      // If R is unset, take T
      Optional<DIKind> RK = LiveOutBlockState::getAvailabilityField(i, Result);
      if (!RK.hasValue()) {
        Result[i*2] = Tmp[i*2], Result[i*2+1] = Tmp[i*2+1];
        continue;
      }

      // If "R" is already partial, we don't know anything.
      if (RK.getValue() == DIKind::Partial)
        continue;

      // If "R" is yes, or no, then switch to partial if we find a different
      // answer.
      if (RK.getValue() != TK.getValue())
        LiveOutBlockState::setAvailability(i, DIKind::Partial, Result);
    }
  }
  
  // If any elements are still unknown, smash them to "yes".  This can't
  // happen in live code, and we want to avoid having analyzed blocks with
  // "unset" values.
  for (unsigned i = 0, e = NumTupleElements; i != e; ++i)
    if (!LiveOutBlockState::getAvailabilityField(i, Result).hasValue())
      LiveOutBlockState::setAvailability(i, DIKind::Yes, Result);
}


/// The specified instruction is a use of the element.  Determine whether the
/// element is definitely initialized at this point or not.  If the value is
/// initialized on some paths, but not others, this returns a partial result.
DIKind ElementPromotion::checkDefinitelyInit(const MemoryUse &Use) {
  SILBasicBlock *InstBB = Use.Inst->getParent();
  
  // The vastly most common case is memory allocations that are not tuples,
  // so special case this with a more efficient algorithm.
  if (NumTupleElements == 1) {
    
    // If there is a store in the current block, scan the block to see if the
    // store is before or after the load.  If it is before, it produces the value
    // we are looking for.
    if (getBlockInfo(InstBB).HasNonLoadUse) {
      for (SILBasicBlock::iterator BBI = Use.Inst, E = InstBB->begin();
           BBI != E;) {
        SILInstruction *TheInst = --BBI;
        
        // If this instruction is unrelated to the memory, ignore it.
        if (!NonLoadUses.count(TheInst))
          continue;
        
        // If we found the allocation itself, then we are loading something that
        // is not defined at all yet.  Otherwise, we've found a definition, or
        // something else that will require that the memory is initialized at
        // this point.
        return TheInst == TheMemory ? DIKind::No : DIKind::Yes;
      }
    }

    Optional<DIKind> Result = Nothing;
    getPredsLiveOut1(InstBB, Result);
    return Result.getValue();
  }

  // Empty tuples are always (trivially) initialized.
  if (Use.NumTupleElements == 0) return DIKind::Yes;
  
  // Check all the tuple elements covered by this memory use.
  llvm::SmallBitVector UsesElements(NumTupleElements);
  UsesElements.set(Use.FirstTupleElement,
                   Use.FirstTupleElement+Use.NumTupleElements);
  
  // If there is a store in the current block, scan the block to see if the
  // store is before or after the load.  If it is before, it may produce some of
  // the elements we are looking for.
  if (getBlockInfo(InstBB).HasNonLoadUse) {
    for (SILBasicBlock::iterator BBI = Use.Inst, E = InstBB->begin();
         BBI != E;) {
      SILInstruction *TheInst = --BBI;

      // If this instruction is unrelated to the memory, ignore it.
      auto It = NonLoadUses.find(TheInst);
      if (It == NonLoadUses.end())
        continue;
      
      // If we found the allocation itself, then we are loading something that
      // is not defined at all yet.
      if (TheInst == TheMemory)
        return DIKind::No;

      // Check to see which tuple elements this instruction defines.  Clear them
      // from the set we're scanning from.
      auto &TheInstUse = Uses[It->second];
      UsesElements.reset(TheInstUse.FirstTupleElement,
                      TheInstUse.FirstTupleElement+TheInstUse.NumTupleElements);
      // If that satisfied all of the elements we're looking for, then we're
      // done.  Otherwise, keep going.
      if (UsesElements.none())
        return DIKind::Yes;
    }
  }

  // Compute the liveness of each element according to our predecessors.
  llvm::SmallBitVector Result(NumTupleElements*2, true);
  getPredsLiveOutN(InstBB, Result);

  // Now that we know about each element, determine a yes/no/partial result
  // based on the elements we care about.
  bool LiveInAll = true, LiveInAny = false;

  for (unsigned i = Use.FirstTupleElement, e = i+Use.NumTupleElements;
       i != e; ++i) {
    // If this element was satisfied locally, ignore its predecessor liveness.
    if (!UsesElements[i]) continue;

    DIKind ElementKind = LiveOutBlockState::getAvailability(i, Result);
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
//                              Load Promotion
//===----------------------------------------------------------------------===//

/// hasEscapedAt - Return true if the box has escaped at the specified
/// instruction.  We are not allowed to do load promotion in an escape region.
bool ElementPromotion::hasEscapedAt(SILInstruction *I) {
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
void ElementPromotion::
updateAvailableValues(SILInstruction *Inst, llvm::SmallBitVector &RequiredElts,
                      SmallVectorImpl<std::pair<SILValue, unsigned>> &Result,
                      llvm::SmallBitVector &ConflictingValues) {
  SILModule &M = Inst->getModule();

  // Handle store and assign.
  if (isa<StoreInst>(Inst) || isa<AssignInst>(Inst)) {
    unsigned StartSubElt = ComputeAccessPath(Inst->getOperand(1), TheMemory);
    SILType ValTy = Inst->getOperand(0).getType();

    for (unsigned i = 0, e = getNumSubElements(ValTy, M); i != e; ++i) {
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
    for (unsigned i = 0, e = getNumSubElements(ValTy, M); i != e; ++i) {
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
    if (CAI->getOperand(0).getType().isLoadable(CAI->getModule())) {
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
void ElementPromotion::
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

void ElementPromotion::
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
  if (getBlockInfo(BB).HasNonLoadUse) {
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
bool ElementPromotion::promoteLoad(SILInstruction *Inst) {
  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.

  // We only handle load and copy_addr right now.
  if (auto CAI = dyn_cast<CopyAddrInst>(Inst)) {
    // If this is a CopyAddr, verify that the element type is loadable.  If not,
    // we can't explode to a load.
    if (!CAI->getSrc().getType().isLoadable(Inst->getModule()))
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
  unsigned NumLoadSubElements =
    getNumSubElements(LoadTy, TheMemory->getModule());
  
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
    for (unsigned i = 0, e = AvailableValues.size(); i != e; ++i)
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
bool ElementPromotion::promoteDestroyAddr(DestroyAddrInst *DAI) {
  SILValue Address = DAI->getOperand();

  // We cannot promote destroys of address-only types, because we can't expose
  // the load.
  SILType LoadTy = Address.getType().getObjectType();
  if (LoadTy.isAddressOnly(DAI->getModule()))
    return false;

  // If the box has escaped at this instruction, we can't safely promote the
  // load.
  if (hasEscapedAt(DAI))
    return false;

  // Compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned FirstElt = ComputeAccessPath(Address, TheMemory);
  unsigned NumLoadSubElements = getNumSubElements(LoadTy, DAI->getModule());

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
    for (unsigned i = 0, e = AvailableValues.size(); i != e; ++i)
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
void ElementPromotion::explodeCopyAddr(CopyAddrInst *CAI) {
  SILType ValTy = CAI->getDest().getType().getObjectType();
  auto &TL = CAI->getModule().getTypeLowering(ValTy);

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
  MemoryUse LoadUse, StoreUse;
  for (auto &Use : Uses) {
    if (Use.Inst != CAI) continue;
    
    if (Use.Kind == UseKind::Load) {
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
  assert(StoreUse.isInvalid() || StoreUse.Kind == Store ||
         StoreUse.Kind == PartialStore);

  // Now that we've emitted a bunch of instructions, including a load and store
  // but also including other stuff, update the internal state of
  // ElementPromotion to reflect them.

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




//===----------------------------------------------------------------------===//
//                          ElementUseCollector
//===----------------------------------------------------------------------===//

namespace {
  class ElementUseCollector {
    SmallVectorImpl<MemoryUse> &Uses;
    SmallVectorImpl<SILInstruction*> &Releases;

    /// When walking the use list, if we index into a struct element, keep track
    /// of this, so that any indexes into tuple subelements don't affect the
    /// element we attribute an access to.
    bool InStructSubElement = false;

    /// When walking the use list, if we index into an enum slice, keep track
    /// of this.
    bool InEnumSubElement = false;
  public:
    ElementUseCollector(SmallVectorImpl<MemoryUse> &Uses,
                        SmallVectorImpl<SILInstruction*> &Releases)
      : Uses(Uses), Releases(Releases) {
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
                        SILInstruction *User, UseKind Kind);
    void collectTupleElementUses(TupleElementAddrInst *TEAI,
                                 unsigned BaseTupleElt);
  };
  
  
} // end anonymous namespace

/// addElementUses - An operation (e.g. load, store, inout use, etc) on a value
/// acts on all of the aggregate elements in that value.  For example, a load
/// of $*(Int,Int) is a use of both Int elements of the tuple.  This is a helper
/// to keep the Uses data structure up to date for aggregate uses.
void ElementUseCollector::addElementUses(unsigned BaseTupleElt, SILType UseTy,
                                         SILInstruction *User, UseKind Kind) {
  // If we're in a subelement of a struct or enum, just mark the struct, not
  // things that come after it in a parent tuple.
  unsigned NumTupleElements = 1;
  if (!InStructSubElement && !InEnumSubElement)
    NumTupleElements = getTupleElementCount(UseTy.getSwiftRValueType());
  
  Uses.push_back(MemoryUse(User, Kind, BaseTupleElt, NumTupleElements));
}

/// Given a tuple_element_addr or struct_element_addr, compute the new
/// BaseTupleElt implicit in the selected member, and recursively add uses of the
/// instruction.
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
    NewBaseElt += getTupleElementCount(EltTy);
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
        Uses.push_back(MemoryUse(User, UseKind::Load, BaseTupleElt, 1));
      continue;
    }

    if (isa<LoadWeakInst>(User)) {
      Uses.push_back(MemoryUse(User, UseKind::Load, BaseTupleElt, 1));
      continue;
    }

    // Stores *to* the allocation are writes.
    if ((isa<StoreInst>(User) || isa<AssignInst>(User) ||
         isa<StoreWeakInst>(User)) &&
        UI->getOperandNumber() == 1) {
      if (PointeeType.is<TupleType>()) {
        assert(!isa<StoreWeakInst>(User) &&
               "Can't weak store a struct or tuple");
        UsesToScalarize.push_back(User);
        continue;
      }
      
      auto Kind = InStructSubElement ? UseKind::PartialStore : UseKind::Store;
      Uses.push_back(MemoryUse(User, Kind, BaseTupleElt, 1));
      continue;
    }

    if (isa<CopyAddrInst>(User)) {
      // If this is a copy of a tuple, we should scalarize it so that we don't
      // have an access that crosses elements.
      if (PointeeType.is<TupleType>()) {
        UsesToScalarize.push_back(User);
        continue;
      }
      
      // If this is the source of the copy_addr, then this is a load.  If it is
      // the destination, then this is a store.  Note that we'll revisit this
      // instruction and add it to Uses twice if it is both a load and store to
      // the same aggregate.
      auto Kind = InStructSubElement ? UseKind::PartialStore : UseKind::Store;
      if (UI->getOperandNumber() == 0) Kind = UseKind::Load;
      Uses.push_back(MemoryUse(User, Kind, BaseTupleElt, 1));
      continue;
    }
    
    // Initializations are definitions.  This is currently used in constructors
    // and should go away someday.
    if (isa<InitializeVarInst>(User)) {
      auto Kind = InStructSubElement ? UseKind::PartialStore : UseKind::Store;
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
        addElementUses(BaseTupleElt, PointeeType, User, UseKind::IndirectIn);
        continue;

      // If this is an out-parameter, it is like a store.
      case ParameterConvention::Indirect_Out:
        assert(!InStructSubElement && "We're initializing sub-members?");
        addElementUses(BaseTupleElt, PointeeType, User, UseKind::IndirectResult);
        continue;

      // If this is an @inout parameter, it is like both a load and store.
      case ParameterConvention::Indirect_Inout:
        addElementUses(BaseTupleElt, PointeeType, User, UseKind::InOutUse);
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
      Uses.push_back(MemoryUse(User, UseKind::Store, BaseTupleElt, 1));

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
      Uses.push_back(MemoryUse(User, UseKind::Store, BaseTupleElt, 1));
      continue;
    }

    // upcast_existential is modeled as a load or store depending on which
    // operand we're looking at.
    if (isa<UpcastExistentialInst>(User)) {
      auto Kind = UI->getOperandNumber() == 1 ? UseKind::Store : UseKind::Load;
      Uses.push_back(MemoryUse(User, Kind, BaseTupleElt, 1));
      continue;
    }

    // project_existential is a use of the protocol value, so it is modeled as a
    // load.
    if (isa<ProjectExistentialInst>(User) || isa<ProtocolMethodInst>(User)) {
      Uses.push_back(MemoryUse(User, UseKind::Load, BaseTupleElt, 1));
      // TODO: Is it safe to ignore all uses of the project_existential?
      continue;
    }

    // We model destroy_addr as a release of the entire value.
    if (isa<DestroyAddrInst>(User)) {
      Releases.push_back(User);
      continue;
    }

    // Otherwise, the use is something complicated, it escapes.
    addElementUses(BaseTupleElt, PointeeType, User, UseKind::Escape);
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
//                         Dead Allocation Elimination
//===----------------------------------------------------------------------===//

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
void ElementPromotion::tryToRemoveDeadAllocation() {
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
    case UseKind::Store:
    case UseKind::PartialStore:
      break;    // These don't prevent removal.

    case UseKind::Load:
    case UseKind::IndirectResult:
    case UseKind::IndirectIn:
    case UseKind::InOutUse:
    case UseKind::Escape:
      DEBUG(llvm::errs() << "*** Failed to remove autogenerated alloc: "
            "kept alive by: " << *U.Inst);
      return;   // These do prevent removal.
    }
  }

  // If the memory object has non-trivial type, then removing the deallocation
  // will drop any releases.  Check that there is nothing preventing removal.
  if (!MemoryType.isTrivial(TheMemory->getModule())) {
    for (auto *R : Releases) {
      if (isa<DeallocStackInst>(R) || isa<DeallocBoxInst>(R))
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

//===----------------------------------------------------------------------===//
//                           Top Level Driver
//===----------------------------------------------------------------------===//

static void processAllocation(SILInstruction *I) {
  assert(isa<AllocBoxInst>(I) || isa<AllocStackInst>(I));
  DEBUG(llvm::errs() << "*** Definite Init looking at: " << *I << "\n");

  // Set up the datastructure used to collect the uses of the allocation.
  SmallVector<MemoryUse, 16> Uses;
  SmallVector<SILInstruction*, 4> Releases;

  // Walk the use list of the pointer, collecting them into the Uses array.
  ElementUseCollector(Uses, Releases).collectFromAllocation(I);

  // Promote each tuple element individually, since they have individual
  // lifetimes and DI properties.
  ElementPromotion(I, Uses, Releases).doIt();
}

static void processMarkUninitialized(MarkUninitializedInst *MUI) {
  DEBUG(llvm::errs() << "*** Definite Init looking at: " << *MUI << "\n");
  
  // Set up the datastructure used to collect the uses of the
  // mark_uninitialized.
  SmallVector<MemoryUse, 16> Uses;
  SmallVector<SILInstruction*, 4> Releases;

  // Walk the use list of the pointer, collecting them into the Uses array.
  ElementUseCollector(Uses, Releases).collectFromMarkUninitialized(MUI);

  assert(Releases.empty() && "Shouldn't have releases of MUIs");
  ElementPromotion(MUI, Uses, Releases).doIt();
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
        LowerAssignInstruction(B, AI, false);
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
