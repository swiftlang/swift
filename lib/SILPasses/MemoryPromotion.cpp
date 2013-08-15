//===--- MemoryPromotion.cpp - Promote memory to SSA registers ------------===//
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

#define DEBUG_TYPE "memory-promotion"
#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

STATISTIC(NumLoadPromoted, "Number of loads promoted");
STATISTIC(NumAssignRewritten, "Number of assigns rewritten");

template<typename ...ArgTypes>
static void diagnose(SILModule *M, SILLocation loc, ArgTypes... args) {
  M->getASTContext().Diags.diagnose(loc.getSourceLoc(), Diagnostic(args...));
}

//===----------------------------------------------------------------------===//
// Tuple Element Flattening/Counting Logic
//===----------------------------------------------------------------------===//

/// getElementCount - Return the number of elements in the flattened SILType.
/// For tuples, this is the (recursive) count of the fields it contains.
static unsigned getElementCount(CanType T) {
  TupleType *TT = T->getAs<TupleType>();

  // If this isn't a tuple, it is a single element.
  if (!TT) return 1;

  unsigned NumElements = 0;
  for (auto &Elt : TT->getFields())
    NumElements += getElementCount(Elt.getType()->getCanonicalType());
  return NumElements;
}

/// Push the symbolic path name to the specified element number onto the
/// specified std::string.
static void getPathStringToElement(CanType T, unsigned Element,
                                   std::string &Result) {
  TupleType *TT = T->getAs<TupleType>();
  if (!TT) return;

  unsigned FieldNo = 0;
  for (auto &Field : TT->getFields()) {
    unsigned ElementsForField =
      getElementCount(Field.getType()->getCanonicalType());
    
    if (Element < ElementsForField) {
      Result += '.';
      if (Field.hasName())
        Result += Field.getName().str();
      else
        Result += llvm::utostr(FieldNo);
      return getPathStringToElement(Field.getType()->getCanonicalType(),
                                    Element, Result);
    }
    
    Element -= ElementsForField;
    
    ++FieldNo;
  }
  assert(0 && "Element number is out of range for this type!");
}

//===----------------------------------------------------------------------===//
// Scalarization Logic
//===----------------------------------------------------------------------===//

static bool isStructOrTupleToScalarize(SILType T) {
  return T.is<TupleType>() || T.is<StructType>() ||
         T.is<BoundGenericStructType>();
}


/// Given a pointer to an aggregate type, compute the addresses of each
/// element and add them to the ElementAddrs vector.
static void getScalarizedElementAddresses(SILValue Pointer,
                              SmallVectorImpl<SILInstruction*> &ElementAddrs) {
  CanType AggType = Pointer.getType().getSwiftRValueType();

  SILInstruction *PointerInst = cast<SILInstruction>(Pointer.getDef());
  SILBuilder B(++SILBasicBlock::iterator(PointerInst));

  if (TupleType *TT = AggType->getAs<TupleType>()) {
    for (auto &Field : TT->getFields()) {
      auto ResultTy = Field.getType()->getCanonicalType();
      ElementAddrs.push_back(B.createTupleElementAddr(PointerInst->getLoc(),
                                                      Pointer,
                                                      ElementAddrs.size(),
                                   SILType::getPrimitiveAddressType(ResultTy)));
    }
    return;
  }

  assert(AggType->is<StructType>() || AggType->is<BoundGenericStructType>());
  StructDecl *SD;
  if (auto *ST = AggType->getAs<StructType>())
    SD = ST->getDecl();
  else
    SD = AggType->castTo<BoundGenericStructType>()->getDecl();

  for (auto *VD : SD->getPhysicalFields()) {
    auto ResultTy = VD->getType()->getCanonicalType();
    ElementAddrs.push_back(B.createStructElementAddr(PointerInst->getLoc(),
                                                     Pointer, VD,
                                   SILType::getPrimitiveAddressType(ResultTy)));
  }
}

/// Given an RValue of aggregate type, compute the values of the elements by
/// emitting a series of tuple_element instructions.
static void getScalarizedElements(SILValue V,
                                  SmallVectorImpl<SILValue> &ElementVals,
                                  SILLocation Loc, SILBuilder &B) {
  CanType AggType = V.getType().getSwiftRValueType();

  if (TupleType *TT = AggType->getAs<TupleType>()) {
    // If this is exploding a tuple_inst, just return the element values.  This
    // can happen when recursively scalarizing stuff.
    if (auto *TI = dyn_cast<TupleInst>(V)) {
      for (unsigned i = 0, e = TI->getNumOperands(); i != e; ++i)
        ElementVals.push_back(TI->getOperand(i));
      return;
    }

    for (auto &Field : TT->getFields()) {
      auto ResultTy = Field.getType()->getCanonicalType();
      ElementVals.push_back(B.createTupleExtract(Loc, V, ElementVals.size(),
                                    SILType::getPrimitiveObjectType(ResultTy)));
    }
    return;
  }

  assert(AggType->is<StructType>() ||
         AggType->is<BoundGenericStructType>());

  // If this is exploding a struct_inst, just return the element values.  This
  // can happen when recursively scalarizing stuff.
  if (auto *SI = dyn_cast<StructInst>(V)) {
    for (unsigned i = 0, e = SI->getNumOperands(); i != e; ++i)
      ElementVals.push_back(SI->getOperand(i));
    return;
  }

  StructDecl *SD;
  if (auto *ST = AggType->getAs<StructType>())
    SD = ST->getDecl();
  else
    SD = AggType->castTo<BoundGenericStructType>()->getDecl();

  for (auto *VD : SD->getPhysicalFields()) {
    auto ResultTy = VD->getType()->getCanonicalType();
    ElementVals.push_back(B.createStructExtract(Loc, V, VD,
                                    SILType::getPrimitiveObjectType(ResultTy)));
  }
}

/// Given a value of [bound generic]struct type, and a load that is known to be
/// from a pointer with an aggregate of the specified struct type, figure out
/// which field path is being accessed, and extract the element value from
/// StructVal that corresponds to it.
static SILValue ExtractMatchingElement(SILValue Pointer, SILValue StructVal,
                                       SILBuilder &B, SILLocation Loc) {
  // If we find the underlying element we care about, we're done.
  if (Pointer.getType().getObjectType() == StructVal.getType())
    return StructVal;

  // Otherwise, we must be in an element of the struct, and the pointer must be
  // a tuple_element_addr or struct_element_addr.
  if (auto *TEAI = dyn_cast<TupleElementAddrInst>(Pointer)) {
    SILValue Res = ExtractMatchingElement(TEAI->getOperand(), StructVal,B,Loc);

    return B.createTupleExtract(Loc, Res, TEAI->getFieldNo(),
                                TEAI->getType().getObjectType());
  }

  auto *SEAI = cast<StructElementAddrInst>(Pointer);
  SILValue Res = ExtractMatchingElement(SEAI->getOperand(), StructVal, B, Loc);
  return B.createStructExtract(Loc, Res, SEAI->getField(),
                               SEAI->getType().getObjectType());
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



//===----------------------------------------------------------------------===//
// Per-Element Promotion Logic
//===----------------------------------------------------------------------===//

namespace {
  enum UseKind {
    // The instruction is a LoadInst.
    Load,

    // The instruction is a StoreInst.
    Store,

    /// The instruction is an Apply, this is a byref or indirect return.
    ByrefUse,

    /// This instruction is a general escape of the value, e.g. a call to a
    /// closure that captures it.
    Escape
  };

  /// ElementUses - This class keeps track of all of the uses of a single
  /// element (i.e. tuple element or struct field) of a memory object.
  typedef std::vector<std::pair<SILInstruction*, UseKind>> ElementUses;

  enum class EscapeKind {
    Unknown,
    Yes,
    No
  };

  /// LiveOutBlockState - Keep track of information about blocks that have
  /// already been analyzed.  Since this is a global analysis, we need this to
  /// cache information about different paths through the CFG.
  struct LiveOutBlockState {
    /// For this block, keep track of whether there is a path from the entry
    /// of the function to the end of the block that crosses an escape site.
    EscapeKind EscapeInfo = EscapeKind::Unknown;

    /// Keep track of whether there is a Store, ByrefUse, or Escape locally in
    /// this block.
    bool HasNonLoadUse = false;

  };
} // end anonymous namespace

namespace {
  /// ElementPromotion - This is the main heavy lifting for processing the uses
  /// of an element of an allocation.
  class ElementPromotion {
    AllocBoxInst *TheAllocBox;
    unsigned ElementNumber;
    ElementUses &Uses;
    llvm::SmallDenseMap<SILBasicBlock*, LiveOutBlockState, 32> PerBlockInfo;

    /// This is the set of uses that are not loads (i.e., they are Stores,
    /// ByrefUses, and Escapes).
    llvm::SmallPtrSet<SILInstruction*, 16> NonLoadUses;

    bool HasAnyEscape = false;

    // Keep track of whether we've emitted an error.  We only emit one error per
    // element as a policy decision.
    bool HadError = false;
  public:
    ElementPromotion(AllocBoxInst *TheAllocBox, unsigned ElementNumber,
                     ElementUses &Uses);

    void doIt();
    
  private:
    void handleLoadUse(SILInstruction *Inst);
    void handleStoreUse(SILInstruction *Inst);
    void handleByrefUse(SILInstruction *Inst);
    void handleEscape(SILInstruction *Inst);

    enum DIKind {
      DI_Yes,
      DI_No,
      DI_Partial
    };
    DIKind checkDefinitelyInit(SILInstruction *Inst, SILValue *AV = nullptr);
    
    
    void diagnoseInitError(SILInstruction *Use,
                           Diag<StringRef> DiagMessage);
  };
} // end anonymous namespace

ElementPromotion::ElementPromotion(AllocBoxInst *TheAllocBox,
                                   unsigned ElementNumber, ElementUses &Uses)
  : TheAllocBox(TheAllocBox), ElementNumber(ElementNumber), Uses(Uses) {

  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (auto Use : Uses) {
    // Keep track of all the uses that aren't loads.
    if (Use.second != UseKind::Load)
      NonLoadUses.insert(Use.first);

    if (Use.second == UseKind::Escape) {
      // Determine which blocks the value can escape from.  We aren't allowed to
      // promote loads in blocks reachable from an escape point.
      HasAnyEscape = true;
      auto &BBInfo = PerBlockInfo[Use.first->getParent()];
      BBInfo.EscapeInfo = EscapeKind::Yes;
      BBInfo.HasNonLoadUse = true;
    } else if (Use.second == UseKind::Store ||
               Use.second == UseKind::ByrefUse) {
      // Keep track of which blocks have local stores.  This makes scanning for
      // assignments cheaper later.
      PerBlockInfo[Use.first->getParent()].HasNonLoadUse = true;
    }
  }
}

void ElementPromotion::diagnoseInitError(SILInstruction *Use,
                                         Diag<StringRef> DiagMessage) {
  HadError = true;

  // If the definition is a declaration, try to reconstruct a name and
  // optionally an access path to the uninitialized element.
  std::string Name;
  if (ValueDecl *VD =
        dyn_cast_or_null<ValueDecl>(TheAllocBox->getLoc().getAs<Decl>()))
    Name = VD->getName().str();
  else
    Name = "<unknown>";

  // If the overall memory allocation is a tuple with multiple elements,
  // then dive in to explain *which* element is being used uninitialized.
  CanType AllocTy = TheAllocBox->getElementType().getSwiftRValueType();
  getPathStringToElement(AllocTy, ElementNumber, Name);
  
  diagnose(Use->getModule(), Use->getLoc(), DiagMessage, Name);

  // Provide context as note diagnostics.

  // TODO: The QoI could be improved in many different ways here.  For example,
  // We could give some path information where the use was uninitialized, like
  // the static analyzer.
  diagnose(Use->getModule(), TheAllocBox->getLoc(),
           diag::variable_defined_here);
}


void ElementPromotion::doIt() {
  // With any escapes tallied up, we can work through all the uses, checking
  // for definitive initialization, promoting loads, rewriting assigns, and
  // performing other tasks.
  for (auto Use : Uses) {
    switch (Use.second) {
    case UseKind::Load:     handleLoadUse(Use.first); break;
    case UseKind::Store:    handleStoreUse(Use.first); break;
    case UseKind::ByrefUse: handleByrefUse(Use.first); break;
    case UseKind::Escape:   handleEscape(Use.first); break;
    // FIXME: Boxes may not be completely constructed by the time they are
    // destroyed.  We need to handle destroying partially constructed boxes.
    }

    if (HadError) break;
  }
}

/// Given a load (i.e., a LoadInst or CopyAddr), determine whether the loaded
/// value is definitely assigned or not.  If not, produce a diagnostic.  If so,
/// attempt to promote the value into SSA form.
void ElementPromotion::handleLoadUse(SILInstruction *Inst) {
  SILValue Result;

  // If this is a Load (not a CopyAddr or LoadWeak), we try to compute the
  // loaded value as an SSA register.  Otherwise, we don't ask for an available
  // value to avoid constructing SSA for the value.

  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.
  auto DI = checkDefinitelyInit(Inst, isa<LoadInst>(Inst) ? &Result : nullptr);

  // If the value is not definitively initialized, emit an error.

  // TODO: In the "No" case, we can emit a fixit adding a default initialization
  // of the type.
  // TODO: In the "partial" case, we can produce a more specific diagnostic
  // indicating where the control flow merged.
  if (DI != DI_Yes) {
    // Otherwise, this is a use of an uninitialized value.  Emit a diagnostic.
    diagnoseInitError(Inst, diag::variable_used_before_initialized);
    return;
  }

  // If the value is definitely initialized, check to see if this is a load
  // that we have a value available for.
  if (!Result) return;

  // FIXME: We cannot do load promotion in regions where the value has
  // escaped!

  // If so, we can replace the load now.  Note that all loads have been
  // scalarized at this point to access a single element, and that we know this
  // is a LoadInst, not a CopyAddr or LoadWeak.  Stores of tuples have been
  // scalarized to storing their elements, but stores of structs have not.
  assert(!isStructOrTupleToScalarize(Inst->getType(0)));

  // If this is a store of a struct, the load will be of an incorrect type,
  // try to determine (from the load pointer) what struct element is being
  // referenced, and extract that from the struct value.
  if (Result.getType().is<StructType>() ||
      Result.getType().is<BoundGenericStructType>()) {
    SILBuilder B(Inst);
    Result = ExtractMatchingElement(Inst->getOperand(0), Result, B,
                                    Inst->getLoc());
  }

  // FIXME: This check shouldn't be needed any longer.
  if (Inst->getType(0) == Result.getType()) {
    SILValue(Inst, 0).replaceAllUsesWith(Result);
    SILValue Addr = Inst->getOperand(0);
    Inst->eraseFromParent();
    RemoveDeadAddressingInstructions(Addr);
    ++NumLoadPromoted;
  }
}



void ElementPromotion::handleStoreUse(SILInstruction *Inst) {
  // Generally, we don't need to do anything for stores, since this analysis is
  // use-driven.  However, we *do* need to decide if "assignments" are stores,
  // initializations, or ambiguous and then rewrite them.  As such, we look at
  // AssignInst and "assignment" CopyAddr's.  We ignore initialize copy_addrs,
  // relying on SILGen to only produce them when known correct.
  if (isa<AssignInst>(Inst))
    ;
  else if (auto CA = dyn_cast<CopyAddrInst>(Inst)) {
    if (CA->isInitializationOfDest()) return;
  } else if (auto SW = dyn_cast<StoreWeakInst>(Inst)) {
    if (SW->isInitializationOfDest()) return;
  } else {
    return;
  }

  SILType StoredType = Inst->getOperand(0).getType();

  bool HasTrivialType = false;
  if (isa<AssignInst>(Inst))
    HasTrivialType = Inst->getModule()->
      Types.getTypeLowering(StoredType).isTrivial();

  // Check to see if the value is known-initialized here or not.  If the assign
  // has non-trivial type, then we're interested in using any live-in value that
  // is available.
  SILValue IncomingVal;
  auto DI = checkDefinitelyInit(Inst, HasTrivialType ? nullptr : &IncomingVal);

  // If it is initialized on some paths, but not others, then we have an
  // inconsistent initialization error.
  if (DI == DI_Partial) {
    diagnoseInitError(Inst, diag::variable_initialized_on_some_paths);
    return;
  }

  // If this is a copy_addr or store_weak, we just set the initialization bit
  // depending on what we find.
  if (auto *CA = dyn_cast<CopyAddrInst>(Inst)) {
    CA->setIsInitializationOfDest(IsInitialization_t(DI == DI_No));
    return;
  }
  if (auto *SW = dyn_cast<StoreWeakInst>(Inst)) {
    SW->setIsInitializationOfDest(IsInitialization_t(DI == DI_No));
    return;
  }

  assert(isa<AssignInst>(Inst));

  ++NumAssignRewritten;
  SILBuilder B(Inst);

  // "unowned" assignments are expanded to unowned operations.
  bool isOwned = true;
  if (auto *RST = StoredType.getSwiftRValueType()->getAs<ReferenceStorageType>())
    isOwned = RST->getOwnership() != Ownership::Unowned;


  // Otherwise, if it has trivial type, we can always just replace the
  // assignment with a store.  If it has non-trivial type and is an
  // initialization, we can also replace it with a store.
  if (HasTrivialType || DI == DI_No) {
    auto NewStore =
      B.createStore(Inst->getLoc(), Inst->getOperand(0), Inst->getOperand(1));

    // Non-trivial values must be retained, since the box owns them.
    if (HasTrivialType)
      ;
    else if (isOwned)
      B.createRetainInst(Inst->getLoc(), Inst->getOperand(0));
    else
      B.createUnownedRetain(Inst->getLoc(), Inst->getOperand(0));

    NonLoadUses.insert(NewStore);
    NonLoadUses.erase(Inst);
    Inst->eraseFromParent();
    return;
  }

  // Otherwise, we need to replace the assignment with the full
  // load/store/retain/release dance.  If we have a live-in value available, we
  // can use that instead of doing a reload.
  if (!IncomingVal)
    IncomingVal = B.createLoad(Inst->getLoc(), Inst->getOperand(1));

  if (isOwned)
    B.createRetainInst(Inst->getLoc(), Inst->getOperand(0));
  else
    B.createUnownedRetain(Inst->getLoc(), Inst->getOperand(0));

  auto NewStore =
    B.createStore(Inst->getLoc(), Inst->getOperand(0), Inst->getOperand(1));

  if (isOwned)
    B.createReleaseInst(Inst->getLoc(), IncomingVal);
  else
    B.createUnownedRelease(Inst->getLoc(), IncomingVal);

  NonLoadUses.insert(NewStore);
  NonLoadUses.erase(Inst);
  Inst->eraseFromParent();
}

/// Given a byref use (an Apply), determine whether the loaded
/// value is definitely assigned or not.  If not, produce a diagnostic.
void ElementPromotion::handleByrefUse(SILInstruction *Inst) {
  auto DI = checkDefinitelyInit(Inst);
  if (DI == DI_Yes)
    return;

  // Otherwise, this is a use of an uninitialized value.  Emit a diagnostic.
  diagnoseInitError(Inst, diag::variable_byref_before_initialized);
}

void ElementPromotion::handleEscape(SILInstruction *Inst) {
  auto DI = checkDefinitelyInit(Inst);
  if (DI == DI_Yes)
    return;

  // Otherwise, this is a use of an uninitialized value.  Emit a diagnostic.
  diagnoseInitError(Inst, diag::variable_escape_before_initialized);
}




/// getStoredValueFrom - Given a may store to the stack slot we're promoting,
/// return the value being stored.
static SILValue getStoredValueFrom(SILInstruction *I) {
  if (auto *SI = dyn_cast<StoreInst>(I))
    return SI->getOperand(0);
  if (auto *AI = dyn_cast<AssignInst>(I))
    return AI->getOperand(0);
  return SILValue();
}

/// The specified instruction is a use of the element.  Determine whether the
/// element is definitely initialized at this point or not.  If the value is
/// initialized on some paths, but not others, this returns a partial result.
///
/// In addition to computing whether a value is definitely initialized or not,
/// if AV is non-null, this function can return the currently live value in some
/// cases.
ElementPromotion::DIKind
ElementPromotion::checkDefinitelyInit(SILInstruction *Inst, SILValue *AV) {
  // If there is a store in the current block, scan the block to see if the
  // store is before or after the load.  If it is before, it produces the value
  // we are looking for.
  if (PerBlockInfo[Inst->getParent()].HasNonLoadUse) {
    for (SILBasicBlock::iterator BBI = Inst, E = Inst->getParent()->begin();
         BBI != E;) {
      SILInstruction *TheInst = --BBI;
      if (NonLoadUses.count(TheInst)) {
        if (AV) *AV = getStoredValueFrom(TheInst);

        return DI_Yes;
      }

      // If we found the allocation itself, then we are loading something that
      // is not defined at all yet.
      if (TheInst == TheAllocBox)
        return DI_No;
    }
  }

  // FIXME: Need to do cross-block analysis.

  return DI_Partial;
}


//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

namespace {
  class ElementUseCollector {
    SmallVectorImpl<ElementUses> &Uses;

    /// When walking the use list, if we index into a struct element, keep track
    /// of this, so that any indexes into tuple subelements don't affect the
    /// element we attribute an access to.
    bool InStructSubElement = false;
  public:
    ElementUseCollector(SmallVectorImpl<ElementUses> &Uses)
      : Uses(Uses) {
    }

    /// This is the main entry point for the use walker.
    void collectUses(SILValue Pointer, unsigned BaseElt);
    
  private:
    void addElementUses(unsigned BaseElt, SILType UseTy,
                        SILInstruction *User, UseKind Kind);
    void collectElementUses(SILInstruction *ElementPtr, unsigned BaseElt);
  };
  
  
} // end anonymous namespace

/// addElementUses - An operation (e.g. load, store, byref use, etc) on a value
/// acts on all of the aggregate elements in that value.  For example, a load
/// of $*(Int,Int) is a use of both Int elements of the tuple.  This is a helper
/// to keep the Uses data structure up to date for aggregate uses.
void ElementUseCollector::addElementUses(unsigned BaseElt, SILType UseTy,
                                         SILInstruction *User, UseKind Kind) {
  for (unsigned i = 0, e = getElementCount(UseTy.getSwiftRValueType());
       i != e; ++i)
    Uses[BaseElt+i].push_back({ User, Kind });
}

/// Given a tuple_element_addr or struct_element_addr, compute the new BaseElt
/// implicit in the selected member, and recursively add uses of the instruction.
void ElementUseCollector::
collectElementUses(SILInstruction *ElementPtr, unsigned BaseElt) {
  // struct_element_addr P, #field indexes into the current element.
  if (auto *SEAI = dyn_cast<StructElementAddrInst>(ElementPtr)) {
    // Set the "InStructSubElement" flag and recursively process the uses.
    llvm::SaveAndRestore<bool> X(InStructSubElement, true);
    collectUses(SILValue(SEAI, 0), BaseElt);
    return;
  }

  auto *TEAI = cast<TupleElementAddrInst>(ElementPtr);

  // If we're walking into a tuple within a struct, don't adjust the BaseElt.
  // the uses hanging off the tuple_element_addr are going to be counted as uses
  // of the struct itself.
  if (InStructSubElement)
    return collectUses(SILValue(TEAI, 0), BaseElt);

  auto RValueType = TEAI->getOperand().getType().getSwiftRValueType();
  
  // tuple_element_addr P, 42 indexes into the current element.  Recursively
  // process its uses with the adjusted element number.
  unsigned FieldNo = TEAI->getFieldNo();
  auto *TT = RValueType->castTo<TupleType>();
  unsigned NewBaseElt = BaseElt;
  for (unsigned i = 0; i != FieldNo; ++i) {
    CanType EltTy = TT->getElementType(i)->getCanonicalType();
    NewBaseElt += getElementCount(EltTy);
  }
  
  collectUses(SILValue(TEAI, 0), NewBaseElt);
}


void ElementUseCollector::collectUses(SILValue Pointer, unsigned BaseElt) {
  assert(Pointer.getType().isAddress() &&
         "Walked through the pointer to the value?");
  SILType PointeeType = Pointer.getType().getObjectType();

  /// This keeps track of instructions in the use list that touch multiple
  /// elements and should be scalarized.  This is done as a second phase to
  /// avoid invalidating the use iterator.
  ///
  SmallVector<SILInstruction*, 4> UsesToScalarize;
  
  for (auto UI : Pointer.getUses()) {
    auto *User = cast<SILInstruction>(UI->getUser());

    // These show up as uses but aren't significant for this analysis.
    if (isa<DeallocStackInst>(User) ||
        isa<RetainInst>(User) ||
        isa<ReleaseInst>(User) ||
        isa<DeallocRefInst>(User))
      continue;
    
    // Instructions that compute a subelement are handled by a helper.
    if (isa<TupleElementAddrInst>(User) || isa<StructElementAddrInst>(User)) {
      collectElementUses(User, BaseElt);
      continue;
    }
    
    // Loads are a use of the value.
    if (isa<LoadInst>(User)) {
      if (isStructOrTupleToScalarize(PointeeType))
        UsesToScalarize.push_back(User);
      else
        Uses[BaseElt].push_back({User, UseKind::Load});
      continue;
    }

    if (isa<LoadWeakInst>(User)) {
      Uses[BaseElt].push_back({User, UseKind::Load});
      continue;
    }

    // Stores *to* the allocation are writes.
    if ((isa<StoreInst>(User) || isa<AssignInst>(User) ||
         isa<StoreWeakInst>(User)) &&
        UI->getOperandNumber() == 1) {
      // We only scalarize stores of aggregate stores of tuples to their
      // elements, we do not scalarize stores of structs to their elements.
      if (PointeeType.is<TupleType>()) {
        assert(!isa<StoreWeakInst>(User) &&
               "Can't weak store a struct or tuple");
        UsesToScalarize.push_back(User);
      } else
        Uses[BaseElt].push_back({ User, UseKind::Store });
      continue;
    }

    if (isa<CopyAddrInst>(User)) {
      // If this is the source of the copy_addr, then this is a load.  If it is
      // the destination, then this is a store.
      auto Kind = UI->getOperandNumber() == 0 ? UseKind::Load : UseKind::Store;
      addElementUses(BaseElt, PointeeType, User, Kind);
      continue;
    }
    
    // Initializations are definitions of the whole thing.  This is currently
    // used in constructors and should go away someday.
    if (isa<InitializeVarInst>(User)) {
      addElementUses(BaseElt, PointeeType, User, UseKind::Store);
      continue;
    }

    // The apply instruction does not capture the pointer when it is passed
    // through [byref] arguments or for indirect returns.  Byref arguments are
    // treated as uses and may-store's, but an indirect return is treated as a
    // full store.
    //
    // Note that partial_apply instructions always close over their argument.
    //
    if (auto *Apply = dyn_cast<ApplyInst>(User)) {
      SILType FnTy = Apply->getOperand(0).getType();
      SILFunctionTypeInfo *FTI = FnTy.getFunctionTypeInfo(*Apply->getModule());
      unsigned ArgumentNumber = UI->getOperandNumber()-1;

      // If this is an indirect return slot, it is a store.
      if (ArgumentNumber == 0 && FTI->hasIndirectReturn()) {
        addElementUses(BaseElt, PointeeType, User, UseKind::Store);
        continue;
      }

      // Otherwise, check for [byref].
      Type ArgTy = FTI->getSwiftArgumentType(ArgumentNumber);
      if (ArgTy->is<LValueType>()) {
        addElementUses(BaseElt, PointeeType, User, UseKind::ByrefUse);
        continue;
      }

      // Otherwise, it is an escape.
    }

     // Otherwise, the use is something complicated, it escapes.
    addElementUses(BaseElt, PointeeType, User, UseKind::Escape);
  }

  // Now that we've walked all of the immediate uses, scalarize any elements
  // that we need to for canonicalization or analysis reasons.
  if (!UsesToScalarize.empty()) {
    SmallVector<SILInstruction*, 4> ElementAddrs;
    getScalarizedElementAddresses(Pointer, ElementAddrs);
    
    SmallVector<SILValue, 4> ElementTmps;
    for (auto *User : UsesToScalarize) {
      SILBuilder B(User);
      ElementTmps.clear();

      // Scalarize LoadInst
      if (auto *LI = dyn_cast<LoadInst>(User)) {
        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
          ElementTmps.push_back(B.createLoad(LI->getLoc(), ElementAddrs[i]));
        
        SILInstruction *Result;
        if (LI->getType().is<TupleType>())
          Result = B.createTuple(LI->getLoc(), LI->getType(), ElementTmps);
        else
          Result = B.createStruct(LI->getLoc(), LI->getType(), ElementTmps);

        SILValue(LI,0).replaceAllUsesWith(Result);
        LI->eraseFromParent();
        continue;
      }
      
      // Scalarize AssignInst
      if (auto *AI = dyn_cast<AssignInst>(User)) {
        getScalarizedElements(AI->getOperand(0), ElementTmps, AI->getLoc(), B);

        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
          B.createAssign(AI->getLoc(), ElementTmps[i], ElementAddrs[i]);
        AI->eraseFromParent();
        continue;
      }
      
      // Scalarize StoreInst
      auto *SI = cast<StoreInst>(User);
      getScalarizedElements(SI->getOperand(0), ElementTmps, SI->getLoc(), B);
      
      for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
        B.createStore(SI->getLoc(), ElementTmps[i], ElementAddrs[i]);
      SI->eraseFromParent();
    }
    
    // Now that we've scalarized some stuff, recurse down into the newly created
    // element address computations to recursively process it.  This can cause
    // further scalarization.
    for (auto EltPtr : ElementAddrs)
      collectElementUses(EltPtr, BaseElt);
  }
}


static void optimizeAllocBox(AllocBoxInst *ABI) {
  // Set up the datastructure used to collect the uses of the alloc_box.  The
  // uses are bucketed up into the elements of the allocation that are being
  // used.  This matters for element-wise tuples and fragile structs.
  SmallVector<ElementUses, 1> Uses;
  Uses.resize(getElementCount(ABI->getElementType().getSwiftRValueType()));

  // Walk the use list of the pointer, collecting them into the Uses array.
  ElementUseCollector(Uses).collectUses(SILValue(ABI, 1), 0);

  // Process each scalar value in the uses array individually.
  unsigned EltNo = 0;
  for (auto &Elt : Uses)
    ElementPromotion(ABI, EltNo++, Elt).doIt();
}


/// performSILMemoryPromotion - Promote alloc_box uses into SSA registers and
/// perform definitive initialization analysis.
void swift::performSILMemoryPromotion(SILModule *M) {
  for (auto &Fn : *M) {
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        auto *ABI = dyn_cast<AllocBoxInst>(I);
        if (ABI == nullptr) {
          ++I;
          continue;
        }

        optimizeAllocBox(ABI);

        // Carefully move iterator to avoid invalidation problems.
        ++I;
        if (ABI->use_empty())
          ABI->eraseFromParent();
      }
    }
  }
}


