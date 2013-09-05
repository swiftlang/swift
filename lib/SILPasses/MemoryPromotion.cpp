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
#include "swift/Basic/Fixnum.h"
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

/// Emit the sequence that an assign instruction lowers to once we know
/// if it is an initialization or an assignment.  If it is an assignment,
/// a live-in value can be provided to optimize out the reload.
static void LowerAssignInstruction(SILBuilder &B, AssignInst *Inst,
                                   bool isInitialization,
                                   SILValue IncomingVal) {
  DEBUG(llvm::errs() << "  *** Lowering [isInit=" << isInitialization << "]: "
            << *Inst << "\n");

  ++NumAssignRewritten;

  auto *M = Inst->getModule();
  SILValue Src = Inst->getSrc();

  auto &destTL = M->getTypeLowering(Inst->getDest().getType());

  // If this is an initialization, or the storage type is trivial, we
  // can just replace the assignment with a store.

  // Otherwise, if it has trivial type, we can always just replace the
  // assignment with a store.  If it has non-trivial type and is an
  // initialization, we can also replace it with a store.
  if (isInitialization || destTL.isTrivial()) {
    B.createStore(Inst->getLoc(), Src, Inst->getDest());
    Inst->eraseFromParent();
    return;
  }

  // Otherwise, we need to replace the assignment with the full
  // load/store/release dance.  Note that the new value is already
  // considered to be retained (by the semantics of the storage type),
  // and we're transfering that ownership count into the destination.

  // This is basically destTL.emitStoreOfCopy, except that if we have
  // a known incoming value, we can avoid the load.
  if (!IncomingVal)
    IncomingVal = B.createLoad(Inst->getLoc(), Inst->getDest());
  B.createStore(Inst->getLoc(), Src, Inst->getDest());
  destTL.emitRelease(B, Inst->getLoc(), IncomingVal);

  Inst->eraseFromParent();
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
  StructDecl *SD = cast<StructDecl>(AggType->getAnyNominal());

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

  StructDecl *SD = cast<StructDecl>(AggType->getAnyNominal());
  for (auto *VD : SD->getPhysicalFields()) {
    auto ResultTy = VD->getType()->getCanonicalType();
    ElementVals.push_back(B.createStructExtract(Loc, V, VD,
                                    SILType::getPrimitiveObjectType(ResultTy)));
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

//===----------------------------------------------------------------------===//
// Access Path Analysis Logic
//===----------------------------------------------------------------------===//

// An access path is an array of tuple or struct members.  Note that the path
// is actually stored backwards for efficiency, the back() is the element
// closest to the underlying alloc_box.
typedef Fixnum<31, unsigned> TupleIndexTy;
typedef PointerUnion<VarDecl*, TupleIndexTy> StructOrTupleElement;
typedef SmallVector<StructOrTupleElement, 8> AccessPathTy;

/// Given a pointer that is known to be derived from an alloc_box, chase up to
/// the alloc box, computing the access path.
static void ComputeAccessPath(SILValue Pointer, AccessPathTy &AccessPath) {
  // If we got to the root, we're done.
  while (!isa<AllocBoxInst>(Pointer)) {
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(Pointer)) {
      AccessPath.push_back(Fixnum<31, unsigned>(TEAI->getFieldNo()));
      Pointer = TEAI->getOperand();
    } else {
      auto *SEAI = cast<StructElementAddrInst>(Pointer);
      AccessPath.push_back(SEAI->getField());
      Pointer = SEAI->getOperand();
    }
  }
}

/// Given an aggregate value and an access path, extract the value indicated by
/// the path.
static SILValue ExtractElement(SILValue Val,
                               ArrayRef<StructOrTupleElement> AccessPath,
                               SILBuilder &B, SILLocation Loc) {
  for (auto I = AccessPath.rbegin(), E = AccessPath.rend(); I != E; ++I) {
    StructOrTupleElement Elt = *I;

    if (Elt.is<VarDecl*>())
      Val = B.createStructExtract(Loc, Val, Elt.get<VarDecl*>());
    else
      Val = B.createTupleExtract(Loc, Val, Elt.get<TupleIndexTy>());
  }

  return Val;
}




/// If the specified instruction is a store of some value, check to see if it is
/// storing to something that intersects the access path of a load.  If the two
/// accesses are non-intersecting, return true.  Otherwise, attempt to compute
/// the accessed subelement value and return it in LoadResultVal.
static bool checkLoadAccessPathAndComputeValue(SILInstruction *Inst,
                                               SILValue &LoadResultVal,
                                               AccessPathTy &LoadAccessPath) {
  // We can only store forward from store and assign's.
  if (!isa<StoreInst>(Inst) && !isa<AssignInst>(Inst)) return false;

  // Get the access path for the store/assign.
  AccessPathTy StoreAccessPath;
  ComputeAccessPath(Inst->getOperand(1), StoreAccessPath);

  // Since loads are always completely scalarized, we know that the load access
  // path will either be non-intersecting or that the load is deeper-or-equal in
  // length than the store.
  if (LoadAccessPath.size() < StoreAccessPath.size()) return true;

  // In the case when the load is deeper (not equal) to the stored value, we'll
  // have to do a number of extracts.  Compute how many.
  unsigned LoadUnwrapLevel = LoadAccessPath.size()-StoreAccessPath.size();

  // Ignoring those extracts, the remaining access path needs to be exactly
  // identical.  If not, we have a non-intersecting access.
  if (ArrayRef<StructOrTupleElement>(LoadAccessPath).slice(LoadUnwrapLevel) !=
        ArrayRef<StructOrTupleElement>(StoreAccessPath))
    return true;

  SILValue StoredVal = Inst->getOperand(0);

  SILBuilder B(Inst);
  if (LoadUnwrapLevel == 0) {
    // Exact match (which is common).
    LoadResultVal = StoredVal;
  } else {
    LoadResultVal = ExtractElement(StoredVal,
       ArrayRef<StructOrTupleElement>(LoadAccessPath).slice(0, LoadUnwrapLevel),
                                   B, Inst->getLoc());
  }

  return false;
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

    /// The instruction is an Apply, this is a byref or indirect return.
    ByrefUse,

    /// This instruction is a general escape of the value, e.g. a call to a
    /// closure that captures it.
    Escape,

    /// This instruction is a release, which may be a last use.
    /// TODO: remove this when we support partially constructed values.
    Release
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

    /// Keep track of whether the element is live out of this block or not.
    ///
    enum LiveOutAvailability {
      IsNotLiveOut,
      IsLiveOut,
      IsComputingLiveOut,
      IsUnknown
    } Availability = IsUnknown;
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

    /// Does this value escape anywhere in the function.
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
    void handleStoreUse(SILInstruction *Inst, bool isPartialStore);
    void handleByrefUse(SILInstruction *Inst);
    void handleEscape(SILInstruction *Inst);
    void handleRelease(SILInstruction *Inst);

    enum DIKind {
      DI_Yes,
      DI_No,
      DI_Partial
    };
    DIKind checkDefinitelyInit(SILInstruction *Inst, SILValue *AV = nullptr,
                               AccessPathTy *AccessPath = nullptr);
    bool isLiveOut(SILBasicBlock *BB);

    bool hasEscapedAt(SILInstruction *I);

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
    if (Use.second == UseKind::Load)
      continue;

    NonLoadUses.insert(Use.first);

    auto &BBInfo = PerBlockInfo[Use.first->getParent()];
    BBInfo.HasNonLoadUse = true;

    // Each of the non-load instructions will each be checked to make sure that
    // they are live-in or a full element store.  This means that the block they
    // are in should be treated as a live out for cross-block analysis purposes.
    BBInfo.Availability = LiveOutBlockState::IsLiveOut;

    if (Use.second == UseKind::Escape) {
      // Determine which blocks the value can escape from.  We aren't allowed to
      // promote loads in blocks reachable from an escape point.
      HasAnyEscape = true;
      BBInfo.EscapeInfo = EscapeKind::Yes;
    }
  }

  // If isn't really a use, but we account for the alloc_box as a use so we see
  // it in our dataflow walks.
  NonLoadUses.insert(TheAllocBox);
  PerBlockInfo[TheAllocBox->getParent()].HasNonLoadUse = true;

  // If there was not another store in the alloc_box block, then it is known to
  // be not live out.
  auto &BBInfo = PerBlockInfo[TheAllocBox->getParent()];
  if (BBInfo.Availability == LiveOutBlockState::IsUnknown)
    BBInfo.Availability = LiveOutBlockState::IsNotLiveOut;
}

void ElementPromotion::diagnoseInitError(SILInstruction *Use,
                                         Diag<StringRef> DiagMessage) {
  HadError = true;

  // If the definition is a declaration, try to reconstruct a name and
  // optionally an access path to the uninitialized element.
  std::string Name;
  if (ValueDecl *VD =
        dyn_cast_or_null<ValueDecl>(TheAllocBox->getLoc().getAsASTNode<Decl>()))
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
    case UseKind::Load:         handleLoadUse(Use.first); break;
    case UseKind::Store:        handleStoreUse(Use.first, false); break;
    case UseKind::PartialStore: handleStoreUse(Use.first, true); break;
    case UseKind::ByrefUse:     handleByrefUse(Use.first); break;
    case UseKind::Escape:       handleEscape(Use.first); break;
    case UseKind::Release:      handleRelease(Use.first); break;
    }

    if (HadError) break;
  }
}

/// Given a load (i.e., a LoadInst, CopyAddr, LoadWeak, or ProjectExistential),
/// determine whether the loaded value is definitely assigned or not.  If not,
/// produce a diagnostic.  If so, attempt to promote the value into SSA form.
void ElementPromotion::handleLoadUse(SILInstruction *Inst) {
  SILValue Result;

  // If this is a Load (not a CopyAddr or LoadWeak), we try to compute the
  // loaded value as an SSA register.  Otherwise, we don't ask for an available
  // value to avoid constructing SSA for the value.
  bool WantValue = isa<LoadInst>(Inst);

  // If the box has escaped at this instruction, we do not want to promote the
  // load, so don't try to compute the result value.
  if (WantValue && hasEscapedAt(Inst))
    WantValue = false;

  // If this is a load from a struct field that we want to promote, compute the
  // access path down to the field so we can determine precise def/use behavior.
  AccessPathTy AccessPath;
  if (WantValue)
    ComputeAccessPath(Inst->getOperand(0), AccessPath);

  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.
  auto DI = checkDefinitelyInit(Inst, WantValue ? &Result : nullptr,
                                      WantValue ? &AccessPath : nullptr);

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

  assert(!isStructOrTupleToScalarize(Inst->getType(0)));

  DEBUG(llvm::errs() << "  *** Promoting load: " << *Inst << "\n");
  DEBUG(llvm::errs() << "      To value: " << Result.getDef() << "\n");



  SILValue(Inst, 0).replaceAllUsesWith(Result);
  SILValue Addr = Inst->getOperand(0);
  Inst->eraseFromParent();
  RemoveDeadAddressingInstructions(Addr);
  ++NumLoadPromoted;
}



void ElementPromotion::handleStoreUse(SILInstruction *Inst,
                                      bool isPartialStore) {

  // We assume that SILGen knows what it is doing when it produces
  // initializations of variables, because it only produces them when it knows
  // they are correct, and this is a super common case for "var x = 4" cases.
  if (!isPartialStore) {
    if (isa<AssignInst>(Inst))
      ;
    else if (auto CA = dyn_cast<CopyAddrInst>(Inst)) {
      if (CA->isInitializationOfDest()) return;
    } else if (auto SW = dyn_cast<StoreWeakInst>(Inst)) {
      if (SW->isInitializationOfDest()) return;
    } else if (isa<InitExistentialInst>(Inst) ||
               isa<UpcastExistentialInst>(Inst)) {
      // These instructions *on a box* are only formed by direct initialization
      // like "var x : Proto = foo".
      return;
    } else {
      return;
    }
  }

  SILType StoredType = Inst->getOperand(1).getType().getObjectType();

  // If we are lowering/expanding an "assign", we may turn it into a read/write
  // operation to release the old value.  If so, we want to determine a live-in
  // value and classify the type a bit.
  bool HasTrivialType = false;
  bool WantsValue = false;
  AccessPathTy AccessPath;
  if (isa<AssignInst>(Inst)) {
    HasTrivialType = Inst->getModule()->
      Types.getTypeLowering(StoredType).isTrivial();

    // Only compute the live-in type if we have a complete store of a
    // non-trivial type.
    WantsValue = !HasTrivialType && !isPartialStore;

    if (WantsValue)
      ComputeAccessPath(Inst->getOperand(1), AccessPath);
  }

  // Check to see if the value is known-initialized here or not.  If the assign
  // has non-trivial type, then we're interested in using any live-in value that
  // is available.
  SILValue IncomingVal;
  auto DI = checkDefinitelyInit(Inst,
                                WantsValue ? &IncomingVal : nullptr,
                                WantsValue ? &AccessPath : nullptr);

  // If this is a partial store into a struct and the whole struct hasn't been
  // initialized, diagnose this as an error.
  if (isPartialStore && DI != DI_Yes) {
    diagnoseInitError(Inst, diag::struct_not_fully_initialized);
    return;
  }

  // If it is initialized on some paths, but not others, then we have an
  // inconsistent initialization error.
  //
  // FIXME: This needs to be supported through the introduction of a boolean
  // control path, or (for reference types as an important special case) a store
  // of zero at the definition point.
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

  // If this is an assign, rewrite it based on whether it is an initialization
  // or not.
  if (auto *AI = dyn_cast<AssignInst>(Inst)) {
    NonLoadUses.erase(Inst);

    SmallVector<SILInstruction*, 8> InsertedInsts;
    SILBuilder B(Inst, &InsertedInsts);

    LowerAssignInstruction(B, AI, HasTrivialType || DI == DI_No, IncomingVal);

    // If lowering of the assign introduced any new stores, keep track of them.
    for (auto I : InsertedInsts)
      if (isa<StoreInst>(I))
        NonLoadUses.insert(I);
  }
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

/// At the time when a box is destroyed, it might be completely uninitialized,
/// and if it is a tuple, it may only be partially initialized.  To avoid
/// ambiguity, we require that all elements of the value are completely
/// initialized at the point of a release.
///
/// TODO: We could make this more powerful to directly support these cases, at
/// lease when the value doesn't escape.
///
void ElementPromotion::handleRelease(SILInstruction *Inst) {
  auto DI = checkDefinitelyInit(Inst);
  if (DI == DI_Yes)
    return;

  // Otherwise, this is a release of an uninitialized value.  Emit a diagnostic.
  diagnoseInitError(Inst, diag::variable_destroyed_before_initialized);
}

/// hasEscapedAt - Return true if the box has escaped at the specified
/// instruction.  We are not allowed to do load promotion in an escape region.
bool ElementPromotion::hasEscapedAt(SILInstruction *I) {
  // FIXME: This is not an aggressive implementation.  :)
  return HasAnyEscape;
}


bool ElementPromotion::isLiveOut(SILBasicBlock *BB) {
  LiveOutBlockState &BBState = PerBlockInfo[BB];
  switch (BBState.Availability) {
  case LiveOutBlockState::IsNotLiveOut: return false;
  case LiveOutBlockState::IsLiveOut:    return true;
  case LiveOutBlockState::IsComputingLiveOut:
    // Speculate that it will be live out in cyclic cases.
    return true;
  case LiveOutBlockState::IsUnknown:
    // Otherwise, process this block.
    break;
  }

  // Set the block's state to reflect that we're currently processing it.  This
  // is required to handle cycles properly.
  BBState.Availability = LiveOutBlockState::IsComputingLiveOut;

  // Recursively processes all of our predecessor blocks.  If any of them is
  // not live out, then we aren't either.
  for (auto PI = BB->pred_begin(), E = BB->pred_end(); PI != E; ++PI) {
    if (!isLiveOut(*PI)) {
      // If any predecessor fails, then we're not live out either.
      PerBlockInfo[BB].Availability = LiveOutBlockState::IsNotLiveOut;
      return false;
    }
  }

  // Otherwise, we're golden.  Return success.
  PerBlockInfo[BB].Availability = LiveOutBlockState::IsLiveOut;
  return true;
}


/// The specified instruction is a use of the element.  Determine whether the
/// element is definitely initialized at this point or not.  If the value is
/// initialized on some paths, but not others, this returns a partial result.
///
/// In addition to computing whether a value is definitely initialized or not,
/// if AV is non-null, this function can return the currently live value in some
/// cases.
ElementPromotion::DIKind
ElementPromotion::checkDefinitelyInit(SILInstruction *Inst, SILValue *AV,
                                      AccessPathTy *AccessPath) {
  SILBasicBlock *InstBB = Inst->getParent();
  // If there is a store in the current block, scan the block to see if the
  // store is before or after the load.  If it is before, it produces the value
  // we are looking for.
  if (PerBlockInfo[InstBB].HasNonLoadUse) {
    for (SILBasicBlock::iterator BBI = Inst, E = Inst->getParent()->begin();
         BBI != E;) {
      SILInstruction *TheInst = --BBI;

      // If this instruction is unrelated to the alloc_box element, ignore it.
      if (!NonLoadUses.count(TheInst))
        continue;

      // If we found the allocation itself, then we are loading something that
      // is not defined at all yet.
      if (TheInst == TheAllocBox)
        return DI_No;

      // If we're trying to compute a value (due to a load), check to see if the
      // loaded pointer's access path and this potential store are to the same
      // sub-element member.  If not, this is a store to some other struct
      // member.  If it is to the right member, try to compute the available
      // value that can replace the load.
      if (AV) {
        if (checkLoadAccessPathAndComputeValue(TheInst, *AV, *AccessPath))
          continue;
      }

      return DI_Yes;
    }
  }

  // Okay, the value isn't locally available in this block.  Check to see if it
  // is live in all predecessors and, if interested, collect the list of
  // definitions we'll build SSA form from.
  for (auto PI = InstBB->pred_begin(), E = InstBB->pred_end(); PI != E; ++PI) {
    if (!isLiveOut(*PI))
      return DI_No;
  }

  return DI_Yes;
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
      } else {
        auto Kind = InStructSubElement ? UseKind::PartialStore : UseKind::Store;
        Uses[BaseElt].push_back({ User, Kind });
      }
      continue;
    }

    if (isa<CopyAddrInst>(User)) {
      // If this is the source of the copy_addr, then this is a load.  If it is
      // the destination, then this is a store.
      auto Kind = InStructSubElement ? UseKind::PartialStore : UseKind::Store;
      if (UI->getOperandNumber() == 0) Kind = UseKind::Load;
      addElementUses(BaseElt, PointeeType, User, Kind);
      continue;
    }
    
    // Initializations are definitions of the whole thing.  This is currently
    // used in constructors and should go away someday.
    if (isa<InitializeVarInst>(User)) {
      auto Kind = InStructSubElement ? UseKind::PartialStore : UseKind::Store;
      addElementUses(BaseElt, PointeeType, User, Kind);
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
        assert(!InStructSubElement && "We're initializing sub-members?");
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

    // init_existential is modeled as a initialization store, where the uses are
    // treated as subelement accesses.
    if (auto *IE = dyn_cast<InitExistentialInst>(User)) {
      assert(!InStructSubElement &&
             "init_existential the subelement of a struct unless in a ctor");
      Uses[BaseElt].push_back({ User, UseKind::Store });

      // Set the "InStructSubElement" flag (so we don't consider stores to be
      // full definitions) and recursively process the uses.
      llvm::SaveAndRestore<bool> X(InStructSubElement, true);
      collectUses(SILValue(IE, 0), BaseElt);
      continue;
    }

    // upcast_existential is modeled as a load or store depending on which
    // operand we're looking at.
    if (isa<UpcastExistentialInst>(User)) {
      if (UI->getOperandNumber() == 1)
        Uses[BaseElt].push_back({ User, UseKind::Store });
      else
        Uses[BaseElt].push_back({ User, UseKind::Load });
      continue;
    }

    // project_existential is a use of the protocol value, so it is modeled as a
    // load.
    if (isa<ProjectExistentialInst>(User) || isa<ProtocolMethodInst>(User)) {
      Uses[BaseElt].push_back({User, UseKind::Load});
      // TODO: Is it safe to ignore all uses of the project_existential?
      continue;
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

      DEBUG(llvm::errs() << "  *** Scalarizing: " << *User << "\n");

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
  DEBUG(llvm::errs() << "*** MemPromotion looking at: " << *ABI << "\n");

  // Set up the datastructure used to collect the uses of the alloc_box.  The
  // uses are bucketed up into the elements of the allocation that are being
  // used.  This matters for element-wise tuples and fragile structs.
  SmallVector<ElementUses, 1> Uses;
  Uses.resize(getElementCount(ABI->getElementType().getSwiftRValueType()));

  // Walk the use list of the pointer, collecting them into the Uses array.
  ElementUseCollector(Uses).collectUses(SILValue(ABI, 1), 0);

  // Collect information about the retain count result as well.
  for (auto UI : SILValue(ABI, 0).getUses()) {
    auto *User = cast<SILInstruction>(UI->getUser());

    // If this is a release, then remember it as such.
    if (isa<StrongReleaseInst>(User)) {
      for (auto &UseArray : Uses)
        UseArray.push_back({ User, UseKind::Release });
    }
  }

  // Process each scalar value in the uses array individually.
  unsigned EltNo = 0;
  for (auto &Elt : Uses)
    ElementPromotion(ABI, EltNo++, Elt).doIt();
}


/// processAssign - It is an invariant of this pass that all assign instructions
/// are processed.  Assignments into boxes are handled in a flow sensitive way
/// when the box is promoted.  Free standing assignments (e.g. into global
/// variables or anything else) are always known to be assignments (not
/// initializations) and are lowered as such.
static void processAssign(AssignInst *AI) {
  // Check to see if this is actually referring to a box.  If so, ignore it.
  SILValue Pointer = AI->getDest();
  while (isa<TupleElementAddrInst>(Pointer) ||
         isa<StructElementAddrInst>(Pointer))
    Pointer = cast<SILInstruction>(Pointer)->getOperand(0);

  if (isa<AllocBoxInst>(Pointer))
    return;

  // If this isn't an assignment into a box, lower it as an assignment (not an
  // initialization).
  SILBuilder B(AI);
  LowerAssignInstruction(B, AI, false, SILValue());
}

/// performSILMemoryPromotion - Promote alloc_box uses into SSA registers and
/// perform definitive initialization analysis.
void swift::performSILMemoryPromotion(SILModule *M) {
  for (auto &Fn : *M) {
    // Walk through an promote all of the alloc_box's that we can.
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        if (auto *AI = dyn_cast<AssignInst>(I)) {
          ++I;
          processAssign(AI);
          continue;
        }

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


