//===--- DIMemoryUseCollector.cpp - Memory use analysis for DI ------------===//
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
#include "DIMemoryUseCollector.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

//===----------------------------------------------------------------------===//
//                  DIMemoryObjectInfo Implementation
//===----------------------------------------------------------------------===//

static unsigned getElementCountRec(CanType T,
                                   bool IsSelfOfNonDelegatingInitializer) {
  // If this is a tuple, it is always recursively flattened.
  if (CanTupleType TT = dyn_cast<TupleType>(T)) {
    assert(!IsSelfOfNonDelegatingInitializer && "self never has tuple type");
    unsigned NumElements = 0;
    for (auto EltTy : TT.getElementTypes())
      NumElements += getElementCountRec(EltTy, false);
    return NumElements;
  }

  // If this is the top level of a 'self' value, we flatten structs and classes.
  // Stored properties with tuple types are tracked with independent lifetimes
  // for each of the tuple members.
  if (IsSelfOfNonDelegatingInitializer) {
    auto *NTD = cast<NominalTypeDecl>(T->getAnyNominal());
    unsigned NumElements = 0;
    for (auto *VD : NTD->getStoredProperties())
      NumElements += getElementCountRec(VD->getType()->getCanonicalType(),
                                        false);
    return NumElements;
  }

  // Otherwise, it is a single element.
  return 1;
}


DIMemoryObjectInfo::DIMemoryObjectInfo(SILInstruction *MI) {
  MemoryInst = MI;
  IsSelfOfNonDelegatingInitializer = false;

  // Compute the type of the memory object.
  if (auto *ABI = dyn_cast<AllocBoxInst>(MemoryInst))
    MemorySILType = ABI->getElementType();
  else if (auto *ASI = dyn_cast<AllocStackInst>(MemoryInst))
    MemorySILType = ASI->getElementType();
  else {
    auto *MUI = cast<MarkUninitializedInst>(MemoryInst);
    MemorySILType = MUI->getType().getObjectType();

    // If this is a 'self' decl in an init method for a non-enum value, then we
    // want to process the stored members of the struct/class elementwise.
    if (MUI->getKind() != MarkUninitializedInst::GlobalVar) {
      if (!isEnumSelf() && !isDelegatingInit())
        IsSelfOfNonDelegatingInitializer = true;
    }
  }
  
  // Compute the number of elements to track in this memory object.
  // If this is a 'self' in a delegating initializer, we only track one bit:
  // whether self.init is called or not.
  if (isDelegatingInit()) {
    NumElements = 1;
    return;
  }
  
  // Otherwise, we break down the initializer.
  NumElements = getElementCountRec(getType(), IsSelfOfNonDelegatingInitializer);

  // If this is a derived class init method, track an extra element to determine
  // whether super.init has been called at each program point.
  if (isDerivedClassSelf())
    ++NumElements;
}

SILInstruction *DIMemoryObjectInfo::getFunctionEntryPoint() const {
  return getFunction().begin()->begin();
}

/// Given a symbolic element number, return the type of the element.
static CanType getElementTypeRec(CanType T, unsigned EltNo,
                                 bool IsSelfOfNonDelegatingInitializer) {
  // If this is a tuple type, walk into it.
  if (TupleType *TT = T->getAs<TupleType>()) {
    assert(!IsSelfOfNonDelegatingInitializer && "self never has tuple type");
    for (auto &Elt : TT->getFields()) {
      auto FieldType = Elt.getType()->getCanonicalType();
      unsigned NumFieldElements = getElementCountRec(FieldType, false);
      if (EltNo < NumFieldElements)
        return getElementTypeRec(FieldType, EltNo, false);
      EltNo -= NumFieldElements;
    }
    assert(0 && "invalid element number");
    abort();
  }

  // If this is the top level of a 'self' value, we flatten structs and classes.
  // Stored properties with tuple types are tracked with independent lifetimes
  // for each of the tuple members.
  if (IsSelfOfNonDelegatingInitializer) {
    auto *NTD = cast<NominalTypeDecl>(T->getAnyNominal());
    for (auto *VD : NTD->getStoredProperties()) {
      auto FieldType = VD->getType()->getCanonicalType();
      unsigned NumFieldElements = getElementCountRec(FieldType, false);
      if (EltNo < NumFieldElements)
        return getElementTypeRec(FieldType, EltNo, false);
      EltNo -= NumFieldElements;
    }
    assert(0 && "invalid element number");
    abort();
  }

  // Otherwise, it is a leaf element.
  assert(EltNo == 0);
  return T;
}

/// getElementTypeRec - Return the swift type of the specified element.
CanType DIMemoryObjectInfo::getElementType(unsigned EltNo) const {
  return getElementTypeRec(getType(), EltNo, IsSelfOfNonDelegatingInitializer);
}

/// computeTupleElementAddress - Given a tuple element number (in the flattened
/// sense) return a pointer to a leaf element of the specified number.
SILValue DIMemoryObjectInfo::
emitElementAddress(unsigned EltNo, SILLocation Loc, SILBuilder &B) const {
  SILValue Ptr = getAddress();
  CanType PointeeType = getType();
  bool IsSelf = IsSelfOfNonDelegatingInitializer;

  while (1) {
    // If we have a tuple, flatten it.
    if (CanTupleType TT = dyn_cast<TupleType>(PointeeType)) {
      assert(!IsSelf && "self never has tuple type");

      // Figure out which field we're walking into.
      unsigned FieldNo = 0;
      for (auto EltTy : TT.getElementTypes()) {
        unsigned NumSubElt = getElementCountRec(EltTy, false);
        if (EltNo < NumSubElt) {
          Ptr = B.createTupleElementAddr(Loc, Ptr, FieldNo);
          PointeeType = EltTy;
          break;
        }

        EltNo -= NumSubElt;
        ++FieldNo;
      }
      continue;
    }

    // If this is the top level of a 'self' value, we flatten structs and classes.
    // Stored properties with tuple types are tracked with independent lifetimes
    // for each of the tuple members.
    if (IsSelf) {
      auto *NTD = cast<NominalTypeDecl>(PointeeType->getAnyNominal());
      for (auto *VD : NTD->getStoredProperties()) {
        auto FieldType = VD->getType()->getCanonicalType();
        unsigned NumFieldElements = getElementCountRec(FieldType, false);
        if (EltNo < NumFieldElements) {
          if (isa<StructDecl>(NTD))
            Ptr = B.createStructElementAddr(Loc, Ptr, VD);
          else {
            assert(isa<ClassDecl>(NTD));
            Ptr = B.createRefElementAddr(Loc, Ptr, VD);
          }

          PointeeType = FieldType;
          IsSelf = false;
          break;
        }
        EltNo -= NumFieldElements;
      }
      continue;
    }

    // Have we gotten to our leaf element?
    assert(EltNo == 0 && "Element count problem");
    return Ptr;
  }
}


/// Push the symbolic path name to the specified element number onto the
/// specified std::string.
static void getPathStringToElementRec(CanType T, unsigned EltNo,
                                      bool IsSelfOfNonDelegatingInitializer,
                                      std::string &Result) {
  if (CanTupleType TT = dyn_cast<TupleType>(T)) {
    assert(!IsSelfOfNonDelegatingInitializer && "self never has tuple type");
    unsigned FieldNo = 0;
    for (auto &Field : TT->getFields()) {
      CanType FieldTy = Field.getType()->getCanonicalType();
      unsigned NumFieldElements = getElementCountRec(FieldTy, false);
      
      if (EltNo < NumFieldElements) {
        Result += '.';
        if (Field.hasName())
          Result += Field.getName().str();
        else
          Result += llvm::utostr(FieldNo);
        return getPathStringToElementRec(FieldTy, EltNo, false, Result);
      }
      
      EltNo -= NumFieldElements;
      
      ++FieldNo;
    }
    assert(0 && "Element number is out of range for this type!");
  }

  // If this is indexing into a field of 'self', look it up.
  if (IsSelfOfNonDelegatingInitializer) {
    auto *NTD = cast<NominalTypeDecl>(T->getAnyNominal());
    for (auto *VD : NTD->getStoredProperties()) {
      auto FieldType = VD->getType()->getCanonicalType();
      unsigned NumFieldElements = getElementCountRec(FieldType, false);
      if (EltNo < NumFieldElements) {
        Result += '.';
        Result += VD->getName().str();
        return getPathStringToElementRec(FieldType, EltNo, false, Result);
      }
      EltNo -= NumFieldElements;
    }
  }

  // Otherwise, there are no subelements.
  assert(EltNo == 0 && "Element count problem");
}

void DIMemoryObjectInfo::getPathStringToElement(unsigned Element,
                                                std::string &Result) const {
  getPathStringToElementRec(getType(), Element,
                            IsSelfOfNonDelegatingInitializer, Result);
}


//===----------------------------------------------------------------------===//
//                        DIMemoryUse Implementation
//===----------------------------------------------------------------------===//


/// onlyTouchesTrivialElements - Return true if all of the accessed elements
/// have trivial type.
bool DIMemoryUse::
onlyTouchesTrivialElements(const DIMemoryObjectInfo &MI) const {
  auto &Module = Inst->getModule();
  
  for (unsigned i = FirstElement, e = i+NumElements; i != e; ++i){
    auto EltTy = MI.getElementType(i);
    if (!SILType::getPrimitiveObjectType(EltTy).isTrivial(Module))
      return false;
  }
  return true;
}


//===----------------------------------------------------------------------===//
//                          Scalarization Logic
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
  TupleType *TT = V.getType().getSwiftRValueType()->castTo<TupleType>();
  for (auto &Field : TT->getFields()) {
    (void)Field;
    ElementVals.push_back(B.emitTupleExtract(Loc, V, ElementVals.size()));
  }
}


/// Scalarize a load down to its subelements.  If NewLoads is specified, this
/// can return the newly generated sub-element loads.
static SILValue scalarizeLoad(LoadInst *LI,
                              SmallVectorImpl<SILValue> &ElementAddrs) {
  SILBuilder B(LI);
  SmallVector<SILValue, 4> ElementTmps;
  
  for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i) {
    auto *SubLI = B.createLoad(LI->getLoc(), ElementAddrs[i]);
    ElementTmps.push_back(SubLI);
  }
  
  if (LI->getType().is<TupleType>())
    return B.createTuple(LI->getLoc(), LI->getType(), ElementTmps);
  return B.createStruct(LI->getLoc(), LI->getType(), ElementTmps);
}


//===----------------------------------------------------------------------===//
//                     ElementUseCollector Implementation
//===----------------------------------------------------------------------===//

namespace {
  class ElementUseCollector {
    const DIMemoryObjectInfo &TheMemory;
    SmallVectorImpl<DIMemoryUse> &Uses;
    SmallVectorImpl<SILInstruction*> &Releases;
    
    /// This is true if definite initialization has finished processing assign
    /// and other ambiguous instructions into init vs assign classes.
    bool isDefiniteInitFinished;

    /// IsSelfOfNonDelegatingInitializer - This is true if we're looking at the
    /// top level of a 'self' variable in a non-delegating init method.
    bool IsSelfOfNonDelegatingInitializer;

    /// When walking the use list, if we index into a struct element, keep track
    /// of this, so that any indexes into tuple subelements don't affect the
    /// element we attribute an access to.
    bool InStructSubElement = false;

    /// When walking the use list, if we index into an enum slice, keep track
    /// of this.
    bool InEnumSubElement = false;
  public:
    ElementUseCollector(const DIMemoryObjectInfo &TheMemory,
                        SmallVectorImpl<DIMemoryUse> &Uses,
                        SmallVectorImpl<SILInstruction*> &Releases,
                        bool isDefiniteInitFinished)
      : TheMemory(TheMemory), Uses(Uses), Releases(Releases),
        isDefiniteInitFinished(isDefiniteInitFinished) {
    }

    /// This is the main entry point for the use walker.  It collects uses from
    /// the address and the refcount result of the allocation.
    void collectFrom() {
      IsSelfOfNonDelegatingInitializer =
        TheMemory.IsSelfOfNonDelegatingInitializer;

      // If this is a delegating initializer, collect uses specially.
      if (TheMemory.isDelegatingInit()) {
        if (TheMemory.getType()->hasReferenceSemantics())
          collectDelegatingClassInitSelfUses();
        else
          collectDelegatingValueTypeInitSelfUses();
      } else if (IsSelfOfNonDelegatingInitializer &&
                TheMemory.getType()->getClassOrBoundGenericClass() != nullptr) {
        // If this is a class pointer, we need to look through
        // ref_element_addrs.
        collectClassSelfUses();
      } else
        collectUses(TheMemory.getAddress(), 0);

      if (!isa<MarkUninitializedInst>(TheMemory.MemoryInst)) {
        // Collect information about the retain count result as well.
        for (auto UI : SILValue(TheMemory.MemoryInst, 0).getUses()) {
          auto *User = UI->getUser();

          // If this is a release or dealloc_stack, then remember it as such.
          if (isa<StrongReleaseInst>(User) || isa<DeallocStackInst>(User) ||
              isa<DeallocBoxInst>(User)) {
            Releases.push_back(User);
          }
        }
      }
    }

  private:
    void collectUses(SILValue Pointer, unsigned BaseEltNo);
    void collectClassSelfUses();
    void collectDelegatingClassInitSelfUses();
    void collectDelegatingValueTypeInitSelfUses();
    void collectClassSelfUses(SILValue ClassPointer, SILType MemorySILType,
                              llvm::SmallDenseMap<VarDecl*, unsigned> &EN);

    void addElementUses(unsigned BaseEltNo, SILType UseTy,
                        SILInstruction *User, DIUseKind Kind);
    void collectTupleElementUses(TupleElementAddrInst *TEAI,
                                 unsigned BaseEltNo);
    void collectStructElementUses(StructElementAddrInst *SEAI,
                                  unsigned BaseEltNo);
  };
} // end anonymous namespace

/// addElementUses - An operation (e.g. load, store, inout use, etc) on a value
/// acts on all of the aggregate elements in that value.  For example, a load
/// of $*(Int,Int) is a use of both Int elements of the tuple.  This is a helper
/// to keep the Uses data structure up to date for aggregate uses.
void ElementUseCollector::addElementUses(unsigned BaseEltNo, SILType UseTy,
                                         SILInstruction *User, DIUseKind Kind) {
  // If we're in a subelement of a struct or enum, just mark the struct, not
  // things that come after it in a parent tuple.
  unsigned NumElements = 1;
  if (TheMemory.NumElements != 1 && !InStructSubElement && !InEnumSubElement)
    NumElements = getElementCountRec(UseTy.getSwiftRValueType(),
                                     IsSelfOfNonDelegatingInitializer);
  
  Uses.push_back(DIMemoryUse(User, Kind, BaseEltNo, NumElements));
}

/// Given a tuple_element_addr or struct_element_addr, compute the new
/// BaseEltNo implicit in the selected member, and recursively add uses of
/// the instruction.
void ElementUseCollector::
collectTupleElementUses(TupleElementAddrInst *TEAI, unsigned BaseEltNo) {

  // If we're walking into a tuple within a struct or enum, don't adjust the
  // BaseElt.  The uses hanging off the tuple_element_addr are going to be
  // counted as uses of the struct or enum itself.
  if (InStructSubElement || InEnumSubElement)
    return collectUses(SILValue(TEAI, 0), BaseEltNo);

  assert(!IsSelfOfNonDelegatingInitializer && "self doesn't have tuple type");

  // tuple_element_addr P, 42 indexes into the current tuple element.
  // Recursively process its uses with the adjusted element number.
  unsigned FieldNo = TEAI->getFieldNo();
  auto *TT = TEAI->getTupleType();
  for (unsigned i = 0; i != FieldNo; ++i) {
    CanType EltTy = TT->getElementType(i)->getCanonicalType();
    BaseEltNo += getElementCountRec(EltTy, false);
  }
  
  collectUses(SILValue(TEAI, 0), BaseEltNo);
}

void ElementUseCollector::collectStructElementUses(StructElementAddrInst *SEAI,
                                                   unsigned BaseEltNo) {
  // Generally, we set the "InStructSubElement" flag and recursively process
  // the uses so that we know that we're looking at something within the
  // current element.
  if (!IsSelfOfNonDelegatingInitializer) {
    llvm::SaveAndRestore<bool> X(InStructSubElement, true);
    collectUses(SILValue(SEAI, 0), BaseEltNo);
    return;
  }

  // If this is the top level of 'self' in an init method, we treat each
  // element of the struct as an element to be analyzed independently.
  llvm::SaveAndRestore<bool> X(IsSelfOfNonDelegatingInitializer, false);

  for (auto *VD : SEAI->getStructDecl()->getStoredProperties()) {
    if (SEAI->getField() == VD)
      break;

    auto FieldType = VD->getType()->getCanonicalType();
    BaseEltNo += getElementCountRec(FieldType, false);
  }

  collectUses(SILValue(SEAI, 0), BaseEltNo);
}

void ElementUseCollector::collectUses(SILValue Pointer, unsigned BaseEltNo) {
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
      collectStructElementUses(SEAI, BaseEltNo);
      continue;
    }

    // Instructions that compute a subelement are handled by a helper.
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(User)) {
      collectTupleElementUses(TEAI, BaseEltNo);
      continue;
    }
    
    // Loads are a use of the value.
    if (isa<LoadInst>(User)) {
      if (PointeeType.is<TupleType>())
        UsesToScalarize.push_back(User);
      else
        addElementUses(BaseEltNo, PointeeType, User, DIUseKind::Load);
      continue;
    }

    if (isa<LoadWeakInst>(User)) {
      Uses.push_back(DIMemoryUse(User, DIUseKind::Load, BaseEltNo, 1));
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
      
      addElementUses(BaseEltNo, PointeeType, User, Kind);
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
        Uses.push_back(DIMemoryUse(User, Kind, BaseEltNo, 1));
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
      
      addElementUses(BaseEltNo, PointeeType, User, Kind);
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
        addElementUses(BaseEltNo, PointeeType, User, DIUseKind::IndirectIn);
        continue;

      // If this is an out-parameter, it is like a store.
      case ParameterConvention::Indirect_Out:
        assert(!InStructSubElement && "We're initializing sub-members?");
        addElementUses(BaseEltNo, PointeeType, User,
                       DIUseKind::Initialization);
        continue;

      // If this is an @inout parameter, it is like both a load and store.
      case ParameterConvention::Indirect_Inout:
        addElementUses(BaseEltNo, PointeeType, User, DIUseKind::InOutUse);
        continue;
      }
      llvm_unreachable("bad parameter convention");
    }

    // init_enum_data_addr is treated like a tuple_element_addr or other instruction
    // that is looking into the memory object (i.e., the memory object needs to
    // be explicitly initialized by a copy_addr or some other use of the
    // projected address).
    if (isa<InitEnumDataAddrInst>(User)) {
      assert(!InStructSubElement &&
             "init_enum_data_addr shouldn't apply to struct subelements");
      // Keep track of the fact that we're inside of an enum.  This informs our
      // recursion that tuple stores are not scalarized outside, and that stores
      // should not be treated as partial stores.
      llvm::SaveAndRestore<bool> X(InEnumSubElement, true);
      collectUses(SILValue(User, 0), BaseEltNo);
      continue;
    }

    // init_existential is modeled as an initialization store, where the uses
    // are treated as subelement accesses.
    if (isa<InitExistentialInst>(User)) {
      assert(!InStructSubElement &&
             "init_existential should not apply to struct subelements");
      Uses.push_back(DIMemoryUse(User, DIUseKind::Initialization,
                                 BaseEltNo, 1));

      // Set the "InEnumSubElement" flag (so we don't consider tuple indexes to
      // index across elements) and recursively process the uses.
      llvm::SaveAndRestore<bool> X(InEnumSubElement, true);
      collectUses(SILValue(User, 0), BaseEltNo);
      continue;
    }
    
    // inject_enum_addr is treated as a store unconditionally.
    if (isa<InjectEnumAddrInst>(User)) {
      assert(!InStructSubElement &&
             "inject_enum_addr the subelement of a struct unless in a ctor");
      Uses.push_back(DIMemoryUse(User, DIUseKind::Initialization,
                                 BaseEltNo, 1));
      continue;
    }

    // upcast_existential is modeled as a load or initialization depending on
    // which operand we're looking at.
    if (isa<UpcastExistentialInst>(User)) {
      auto Kind = UI->getOperandNumber() == 1 ?
        DIUseKind::Initialization : DIUseKind::Load;
      Uses.push_back(DIMemoryUse(User, Kind, BaseEltNo, 1));
      continue;
    }
    
    // project_existential is a use of the protocol value, so it is modeled as a
    // load.
    if (isa<ProjectExistentialInst>(User) || isa<ProtocolMethodInst>(User)) {
      Uses.push_back(DIMemoryUse(User, DIUseKind::Load, BaseEltNo, 1));
      // TODO: Is it safe to ignore all uses of the project_existential?
      continue;
    }

    // We model destroy_addr as a release of the entire value.
    if (isa<DestroyAddrInst>(User)) {
      Releases.push_back(User);
      continue;
    }

    // Otherwise, the use is something complicated, it escapes.
    addElementUses(BaseEltNo, PointeeType, User, DIUseKind::Escape);
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
      collectTupleElementUses(cast<TupleElementAddrInst>(EltPtr), BaseEltNo);
  }
}


/// collectClassSelfUses - Collect all the uses of a 'self' pointer in a class
/// constructor.  The memory object has class type.
void ElementUseCollector::collectClassSelfUses() {
  assert(IsSelfOfNonDelegatingInitializer &&
         TheMemory.getType()->getClassOrBoundGenericClass() != nullptr);

  // For efficiency of lookup below, compute a mapping of the local ivars in the
  // class to their element number.
  llvm::SmallDenseMap<VarDecl*, unsigned> EltNumbering;

  {
    auto *NTD = cast<NominalTypeDecl>(TheMemory.getType()->getAnyNominal());
    unsigned NumElements = 0;
    for (auto *VD : NTD->getStoredProperties()) {
      EltNumbering[VD] = NumElements;
      NumElements += getElementCountRec(VD->getType()->getCanonicalType(),
                                        false);
    }
  }

  // If we are looking at the init method for a root class, just walk the
  // MUI use-def chain directly to find our uses.
  auto *MUI = cast<MarkUninitializedInst>(TheMemory.MemoryInst);
  if (MUI->getKind() == MarkUninitializedInst::RootSelf) {
    collectClassSelfUses(TheMemory.getAddress(), TheMemory.MemorySILType,
                         EltNumbering);
    return;
  }

  // Otherwise, we have a 'self' init in a derived class, and things are more
  // complicated.  Because super.init is allows to change self, SILGen is
  // producing a box for 'self'.  This means that we need to see through this
  // box to find the uses of 'self'.  We handle this by pattern matching: we
  // allow only a single use of MUI, which must be a store to an
  // alloc_box/alloc_stack.
  assert(MUI->hasOneUse());
  auto *TheStore = cast<StoreInst>((*MUI->use_begin())->getUser());
  SILValue SelfBox = TheStore->getOperand(1);
  assert(isa<AllocBoxInst>(SelfBox) || isa<AllocStackInst>(SelfBox));

  // Okay, given that we have a proper setup, we walk the use chains of the self
  // box to find any accesses to it.  The possible uses are one of:
  //   1) The initialization store (TheStore).
  //   2) Loads of the box, which have uses of self hanging off of them.
  //   3) An assign to the box, which happens at super.init.
  //   4) Potential escapes after super.init, if self is closed over.
  // Handle each of these in turn.
  //
  for (auto UI : SelfBox.getUses()) {
    SILInstruction *User = UI->getUser();

    // Ignore the initialization store.
    if (User == TheStore) continue;

    // Loads of the box produce self, so collect uses from them.
    if (auto *LI = dyn_cast<LoadInst>(User)) {
      collectClassSelfUses(LI, TheMemory.MemorySILType, EltNumbering);
      continue;
    }

    // destroyaddr on the box is load+release, which is ignored.
    if (isa<DestroyAddrInst>(User)) continue;

    // We can safely handle anything else as an escape.  They should all happen
    // after super.init is invoked.  As such, all elements must be initialized
    // and super.init must be called.
    Uses.push_back(DIMemoryUse(User, DIUseKind::Load,
                               0, TheMemory.NumElements));
  }
}

/// isSuperInitUse - Return true if this "upcast" is part of a call to
/// super.init.
static bool isSuperInitUse(UpcastInst *Inst) {

  // "Inst" is an Upcast instruction.  Check to see if it is used by an apply
  // that came from a call to super.init.
  for (auto UI : Inst->getUses()) {
    auto *AI = dyn_cast<ApplyInst>(UI->getUser());
    if (!AI) continue;

    auto *LocExpr = AI->getLoc().getAsASTNode<ApplyExpr>();
    if (!LocExpr) {
      // If we're reading a .sil file, treat a call to "superinit" as a
      // super.init call as a hack to allow us to write testcases.
      if (AI->getLoc().is<SILFileLocation>())
        if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
          if (FRI->getReferencedFunction()->getName() == "superinit")
            return true;
      continue;
    }

    // This is a super.init call if structured like this:
    // (call_expr type='SomeClass'
    //   (dot_syntax_call_expr type='() -> SomeClass' super
    //     (other_constructor_ref_expr implicit decl=SomeClass.init)
    //     (super_ref_expr type='SomeClass'))
    //   (...some argument...)
    LocExpr = dyn_cast<ApplyExpr>(LocExpr->getFn());
    if (!LocExpr || !isa<OtherConstructorDeclRefExpr>(LocExpr->getFn()))
      continue;

    if (isa<SuperRefExpr>(LocExpr->getArg()))
      return true;
  }

  return false;
}

/// isSelfInitUse - Return true if this apply_inst is a call to self.init.
static bool isSelfInitUse(ApplyInst *AI) {
  auto *LocExpr = AI->getLoc().getAsASTNode<ApplyExpr>();
  if (!LocExpr) {
    // If we're reading a .sil file, treat a call to "selfinit" as a
    // self.init call as a hack to allow us to write testcases.
    if (AI->getLoc().is<SILFileLocation>())
      if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
        if (FRI->getReferencedFunction()->getName() == "selfinit")
          return true;
    return false;
  }

  // This is a super.init call if structured like this:
  // (call_expr type='SomeClass'
  //   (dot_syntax_call_expr type='() -> SomeClass' self
  //     (other_constructor_ref_expr implicit decl=SomeClass.init)
  //     (decl_ref_expr type='SomeClass', "self"))
  //   (...some argument...)
  LocExpr = dyn_cast<ApplyExpr>(LocExpr->getFn());
  if (!LocExpr || !isa<OtherConstructorDeclRefExpr>(LocExpr->getFn()))
    return false;

  return true;
}


void ElementUseCollector::
collectClassSelfUses(SILValue ClassPointer, SILType MemorySILType,
                     llvm::SmallDenseMap<VarDecl*, unsigned> &EltNumbering) {
  for (auto UI : ClassPointer.getUses()) {
    auto *User = UI->getUser();

    // ref_element_addr P, #field lookups up a field.
    if (auto *REAI = dyn_cast<RefElementAddrInst>(User)) {
      assert(EltNumbering.count(REAI->getField()) &&
             "ref_element_addr not a local field?");;
      // Recursively collect uses of the fields.  Note that fields of the class
      // could be tuples, so they may be tracked as independent elements.
      llvm::SaveAndRestore<bool> X(IsSelfOfNonDelegatingInitializer, false);
      collectUses(REAI, EltNumbering[REAI->getField()]);
      continue;
    }
    
    // We ignore retains and releases of self.
    if (isa<StrongRetainInst>(User) || isa<StrongReleaseInst>(User))
      continue;

    // If this is an upcast instruction, it is a conversion of self to the base.
    // This is either part of a super.init sequence, or a general superclass
    // access.
    DIUseKind Kind = DIUseKind::Load;
    if (auto *UCI = dyn_cast<UpcastInst>(User))
      if (isSuperInitUse(UCI))
        Kind = DIUseKind::SuperInit;

    // If this is an ApplyInst, check to see if this is part of a self.init
    // call in a delegating initializer.
    if (isa<ApplyInst>(User) && isSelfInitUse(cast<ApplyInst>(User)))
      Kind = DIUseKind::SelfInit;

    Uses.push_back(DIMemoryUse(User, Kind, 0, TheMemory.NumElements));
  }
}

/// collectDelegatingClassInitSelfUses - Collect uses of the self argument in a
/// delegating-constructor-for-a-class case.
void ElementUseCollector::collectDelegatingClassInitSelfUses() {
  // When we're analyzing a delegating constructor, we aren't field sensitive at
  // all.  Just treat all members of self as uses of the single
  // non-field-sensitive value.
  assert(TheMemory.NumElements == 1 && "delegating inits only have 1 bit");
  auto *MUI = cast<MarkUninitializedInst>(TheMemory.MemoryInst);
 
  // Because self.init is allows to change self, SILGen is producing a box for
  // 'self'.  This means that we need to see through this box to find the uses
  // of 'self'.  We handle this by pattern matching: we allow only a single use
  // of MUI, which must be a store to an alloc_box/alloc_stack.
  assert(MUI->hasOneUse());
  auto *TheStore = cast<StoreInst>((*MUI->use_begin())->getUser());
  SILValue SelfBox = TheStore->getOperand(1);
  assert(isa<AllocBoxInst>(SelfBox) || isa<AllocStackInst>(SelfBox));
  
  // Okay, given that we have a proper setup, we walk the use chains of the self
  // box to find any accesses to it.  The possible uses are one of:
  //   1) The initialization store (TheStore).
  //   2) Loads of the box, which have uses of self hanging off of them.
  //   3) An assign to the box, which happens at super.init.
  //   4) Potential escapes after super.init, if self is closed over.
  // Handle each of these in turn.
  //
  for (auto UI : SelfBox.getUses()) {
    SILInstruction *User = UI->getUser();
    
    // Ignore the initialization store.
    if (User == TheStore) continue;
    
    // Loads of the box produce self, so collect uses from them.
    if (auto *LI = dyn_cast<LoadInst>(User)) {
      
      // If we have a load, then this is a use of the box.  Look at the uses of
      // the load to find out more information.
      for (auto UI : LI->getUses()) {
        auto *User = UI->getUser();
        
        // We ignore retains and releases of self.
        if (isa<StrongRetainInst>(User) || isa<StrongReleaseInst>(User))
          continue;

        // peer_method is a method lookup for delegation to a foreign
        // constructor, which is ignored.
        if (isa<PeerMethodInst>(User)) continue;

        // We only track two kinds of uses for delegating initializers:
        // calls to self.init, and "other", which we choose to model as escapes.
        // This intentionally ignores all stores, which (if they got emitted as
        // copyaddr or assigns) will eventually get rewritten as assignments
        // (not initializations), which is the right thing to do.
        DIUseKind Kind = DIUseKind::Escape;
        
        // If this is an ApplyInst, check to see if this is part of a self.init
        // call in a delegating initializer.
        if (isa<ApplyInst>(User) && isSelfInitUse(cast<ApplyInst>(User)))
          Kind = DIUseKind::SelfInit;
        
        Uses.push_back(DIMemoryUse(User, Kind, 0, 1));
      }
      continue;
    }
    
    // destroyaddr on the box is load+release, which is ignored.
    if (isa<DestroyAddrInst>(User)) continue;

    // We can safely handle anything else as an escape.  They should all happen
    // after self.init is invoked.
    Uses.push_back(DIMemoryUse(User, DIUseKind::Escape, 0, 1));
  }
}


void ElementUseCollector::collectDelegatingValueTypeInitSelfUses() {
  // When we're analyzing a delegating constructor, we aren't field sensitive at
  // all.  Just treat all members of self as uses of the single
  // non-field-sensitive value.
  assert(TheMemory.NumElements == 1 && "delegating inits only have 1 bit");

  auto *MUI = cast<MarkUninitializedInst>(TheMemory.MemoryInst);
  
  for (auto UI : MUI->getUses()) {
    auto *User = UI->getUser();
    
    // We only track two kinds of uses for delegating initializers:
    // calls to self.init, and "other", which we choose to model as escapes.
    // This intentionally ignores all stores, which (if they got emitted as
    // copyaddr or assigns) will eventually get rewritten as assignments
    // (not initializations), which is the right thing to do.
    DIUseKind Kind = DIUseKind::Escape;

    // Stores *to* the allocation are writes.  If the value being stored is a
    // call to self.init()... then we have a self.init call.
    if (auto *AI = dyn_cast<AssignInst>(User))
      if (auto *Call = dyn_cast<ApplyInst>(AI->getOperand(0)))
        if (isSelfInitUse(Call))
          Kind = DIUseKind::SelfInit;
    
    // We can safely handle anything else as an escape.  They should all happen
    // after self.init is invoked.
    Uses.push_back(DIMemoryUse(User, Kind, 0, 1));
  }
}

/// collectDIElementUsesFrom - Analyze all uses of the specified allocation
/// instruction (alloc_box, alloc_stack or mark_uninitialized), classifying them
/// and storing the information found into the Uses and Releases lists.
void swift::collectDIElementUsesFrom(const DIMemoryObjectInfo &MemoryInfo,
                                     SmallVectorImpl<DIMemoryUse> &Uses,
                                     SmallVectorImpl<SILInstruction*> &Releases,
                                     bool isDIFinished) {
  ElementUseCollector(MemoryInfo, Uses, Releases, isDIFinished).collectFrom();
}
