//===--- DIMemoryUseCollector.cpp - Memory use analysis for DI ------------===//
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
#include "DIMemoryUseCollector.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

//===----------------------------------------------------------------------===//
//                  DIMemoryObjectInfo Implementation
//===----------------------------------------------------------------------===//

static unsigned getElementCountRec(SILModule &Module,
                                   SILType T,
                                   bool IsSelfOfNonDelegatingInitializer) {
  // If this is a tuple, it is always recursively flattened.
  if (CanTupleType TT = T.getAs<TupleType>()) {
    assert(!IsSelfOfNonDelegatingInitializer && "self never has tuple type");
    unsigned NumElements = 0;
    for (unsigned i = 0, e = TT->getNumElements(); i < e; i++)
      NumElements += getElementCountRec(Module, T.getTupleElementType(i), false);
    return NumElements;
  }

  // If this is the top level of a 'self' value, we flatten structs and classes.
  // Stored properties with tuple types are tracked with independent lifetimes
  // for each of the tuple members.
  if (IsSelfOfNonDelegatingInitializer) {
    // Protocols never have a stored properties.
    if (auto *NTD = T.getNominalOrBoundGenericNominal()) {

      unsigned NumElements = 0;
      for (auto *VD : NTD->getStoredProperties())
        NumElements += getElementCountRec(Module, T.getFieldType(VD, Module),
                                          false);
      return NumElements;
    }
  }

  // Otherwise, it is a single element.
  return 1;
}


DIMemoryObjectInfo::DIMemoryObjectInfo(SILInstruction *MI) {
  auto &Module = MI->getModule();

  MemoryInst = MI;
  // Compute the type of the memory object.
  if (auto *ABI = dyn_cast<AllocBoxInst>(MemoryInst)) {
    assert(ABI->getBoxType()->getLayout()->getFields().size() == 1
           && "analyzing multi-field boxes not implemented");
    MemorySILType =
      ABI->getBoxType()->getFieldType(Module, 0);
  } else if (auto *ASI = dyn_cast<AllocStackInst>(MemoryInst)) {
    MemorySILType = ASI->getElementType();
  } else {
    auto *MUI = cast<MarkUninitializedInst>(MemoryInst);
    MemorySILType = MUI->getType().getObjectType();

    // If this is a let variable we're initializing, remember this so we don't
    // allow reassignment.
    if (MUI->isVar())
      if (auto *decl = MUI->getLoc().getAsASTNode<VarDecl>())
        IsLet = decl->isLet();
  }
  
  // Compute the number of elements to track in this memory object.
  // If this is a 'self' in a delegating initializer, we only track one bit:
  // whether self.init is called or not.
  if (isDelegatingInit()) {
    NumElements = 1;
    return;
  }

  // If this is a derived class init method for which stored properties are
  // separately initialized, track an element for the super.init call.
  if (isDerivedClassSelfOnly()) {
    NumElements = 1;
    return;
  }

  // Otherwise, we break down the initializer.
  NumElements = getElementCountRec(Module, MemorySILType, isNonDelegatingInit());

  // If this is a derived class init method, track an extra element to determine
  // whether super.init has been called at each program point.
  if (isDerivedClassSelf())
    ++NumElements;
}

SILInstruction *DIMemoryObjectInfo::getFunctionEntryPoint() const {
  return &*getFunction().begin()->begin();
}

/// Given a symbolic element number, return the type of the element.
static SILType getElementTypeRec(SILModule &Module, SILType T, unsigned EltNo,
                                 bool IsSelfOfNonDelegatingInitializer) {
  // If this is a tuple type, walk into it.
  if (CanTupleType TT = T.getAs<TupleType>()) {
    assert(!IsSelfOfNonDelegatingInitializer && "self never has tuple type");
    for (unsigned i = 0, e = TT->getNumElements(); i < e; i++) {
      auto FieldType = T.getTupleElementType(i);
      unsigned NumFieldElements = getElementCountRec(Module, FieldType, false);
      if (EltNo < NumFieldElements)
        return getElementTypeRec(Module, FieldType, EltNo, false);
      EltNo -= NumFieldElements;
    }
    llvm::report_fatal_error("invalid element number");
  }

  // If this is the top level of a 'self' value, we flatten structs and classes.
  // Stored properties with tuple types are tracked with independent lifetimes
  // for each of the tuple members.
  if (IsSelfOfNonDelegatingInitializer) {
    if (auto *NTD = T.getNominalOrBoundGenericNominal()) {
      for (auto *VD : NTD->getStoredProperties()) {
        auto FieldType = T.getFieldType(VD, Module);
        unsigned NumFieldElements = getElementCountRec(Module, FieldType, false);
        if (EltNo < NumFieldElements)
          return getElementTypeRec(Module, FieldType, EltNo, false);
        EltNo -= NumFieldElements;
      }
      llvm::report_fatal_error("invalid element number");
    }
  }

  // Otherwise, it is a leaf element.
  assert(EltNo == 0);
  return T;
}

/// getElementTypeRec - Return the swift type of the specified element.
SILType DIMemoryObjectInfo::getElementType(unsigned EltNo) const {
  auto &Module = MemoryInst->getModule();
  return getElementTypeRec(Module, MemorySILType, EltNo, isNonDelegatingInit());
}

/// computeTupleElementAddress - Given a tuple element number (in the flattened
/// sense) return a pointer to a leaf element of the specified number.
SILValue DIMemoryObjectInfo::
emitElementAddress(unsigned EltNo, SILLocation Loc, SILBuilder &B) const {
  SILValue Ptr = getAddress();
  bool IsSelf = isNonDelegatingInit();
  auto &Module = MemoryInst->getModule();

  auto PointeeType = MemorySILType;

  while (1) {
    // If we have a tuple, flatten it.
    if (CanTupleType TT = PointeeType.getAs<TupleType>()) {
      assert(!IsSelf && "self never has tuple type");

      // Figure out which field we're walking into.
      unsigned FieldNo = 0;
      for (unsigned i = 0, e = TT->getNumElements(); i < e; i++) {
        auto EltTy = PointeeType.getTupleElementType(i);
        unsigned NumSubElt = getElementCountRec(Module, EltTy, false);
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

    // If this is the top level of a 'self' value, we flatten structs and
    // classes.  Stored properties with tuple types are tracked with independent
    // lifetimes for each of the tuple members.
    if (IsSelf) {
      if (auto *NTD = PointeeType.getNominalOrBoundGenericNominal()) {
        if (isa<ClassDecl>(NTD) && Ptr->getType().isAddress())
          Ptr = B.createLoad(Loc, Ptr, LoadOwnershipQualifier::Unqualified);
        for (auto *VD : NTD->getStoredProperties()) {
          auto FieldType = PointeeType.getFieldType(VD, Module);
          unsigned NumFieldElements = getElementCountRec(Module, FieldType, false);
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
    }

    // Have we gotten to our leaf element?
    assert(EltNo == 0 && "Element count problem");
    return Ptr;
  }
}


/// Push the symbolic path name to the specified element number onto the
/// specified std::string.
static void getPathStringToElementRec(SILModule &Module, SILType T,
                                      unsigned EltNo, std::string &Result) {
  if (CanTupleType TT = T.getAs<TupleType>()) {
    unsigned FieldNo = 0;
    for (unsigned i = 0, e = TT->getNumElements(); i < e; i++) {
      auto Field = TT->getElement(i);
      SILType FieldTy = T.getTupleElementType(i);
      unsigned NumFieldElements = getElementCountRec(Module, FieldTy, false);
      
      if (EltNo < NumFieldElements) {
        Result += '.';
        if (Field.hasName())
          Result += Field.getName().str();
        else
          Result += llvm::utostr(FieldNo);
        return getPathStringToElementRec(Module, FieldTy, EltNo, Result);
      }
      
      EltNo -= NumFieldElements;
      
      ++FieldNo;
    }
    llvm_unreachable("Element number is out of range for this type!");
  }

  // Otherwise, there are no subelements.
  assert(EltNo == 0 && "Element count problem");
}

ValueDecl *DIMemoryObjectInfo::
getPathStringToElement(unsigned Element, std::string &Result) const {
  auto &Module = MemoryInst->getModule();

  if (isAnyInitSelf())
    Result = "self";
  else if (ValueDecl *VD =
        dyn_cast_or_null<ValueDecl>(getLoc().getAsASTNode<Decl>()))
    Result = VD->getName().str();
  else
    Result = "<unknown>";


  // If this is indexing into a field of 'self', look it up.
  if (isNonDelegatingInit() && !isDerivedClassSelfOnly()) {
    if (auto *NTD = MemorySILType.getNominalOrBoundGenericNominal()) {
      for (auto *VD : NTD->getStoredProperties()) {
        auto FieldType = MemorySILType.getFieldType(VD, Module);
        unsigned NumFieldElements = getElementCountRec(Module, FieldType, false);
        if (Element < NumFieldElements) {
          Result += '.';
          Result += VD->getName().str();
          getPathStringToElementRec(Module, FieldType, Element, Result);
          return VD;
        }
        Element -= NumFieldElements;
      }
    }
  }

  // Get the path through a tuple, if relevant.
  getPathStringToElementRec(Module, MemorySILType, Element, Result);

  // If we are analyzing a variable, we can generally get the decl associated
  // with it.
  if (auto *MUI = dyn_cast<MarkUninitializedInst>(MemoryInst))
    if (MUI->isVar())
      return MUI->getLoc().getAsASTNode<VarDecl>();

  // Otherwise, we can't.
  return nullptr;
}

/// If the specified value is a 'let' property in an initializer, return true.
bool DIMemoryObjectInfo::isElementLetProperty(unsigned Element) const {
  // If we aren't representing 'self' in a non-delegating initializer, then we
  // can't have 'let' properties.
  if (!isNonDelegatingInit()) return IsLet;

  auto &Module = MemoryInst->getModule();

  if (auto *NTD = MemorySILType.getNominalOrBoundGenericNominal()) {
    for (auto *VD : NTD->getStoredProperties()) {
      auto FieldType = MemorySILType.getFieldType(VD, Module);
      unsigned NumFieldElements = getElementCountRec(Module, FieldType, false);
      if (Element < NumFieldElements)
        return VD->isLet();
      Element -= NumFieldElements;
    }
  }

  // Otherwise, we miscounted elements?
  assert(Element == 0 && "Element count problem");
  return false;
}


//===----------------------------------------------------------------------===//
//                        DIMemoryUse Implementation
//===----------------------------------------------------------------------===//


/// onlyTouchesTrivialElements - Return true if all of the accessed elements
/// have trivial type.
bool DIMemoryUse::
onlyTouchesTrivialElements(const DIMemoryObjectInfo &MI) const {
  auto &Module = Inst->getModule();
  
  for (unsigned i = FirstElement, e = i+NumElements; i != e; ++i) {
    // Skip 'super.init' bit
    if (i == MI.getNumMemoryElements())
      return false;

    auto EltTy = MI.getElementType(i);
    if (!EltTy.isTrivial(Module))
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
  CanType AggType = Pointer->getType().getSwiftRValueType();
  TupleType *TT = AggType->castTo<TupleType>();
  for (auto &Field : TT->getElements()) {
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
  TupleType *TT = V->getType().getSwiftRValueType()->castTo<TupleType>();
  for (auto &Field : TT->getElements()) {
    (void)Field;
    ElementVals.push_back(B.emitTupleExtract(Loc, V, ElementVals.size()));
  }
}


/// Scalarize a load down to its subelements.  If NewLoads is specified, this
/// can return the newly generated sub-element loads.
static SILValue scalarizeLoad(LoadInst *LI,
                              SmallVectorImpl<SILValue> &ElementAddrs) {
  SILBuilderWithScope B(LI);
  SmallVector<SILValue, 4> ElementTmps;
  
  for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i) {
    auto *SubLI = B.createLoad(LI->getLoc(), ElementAddrs[i],
                               LoadOwnershipQualifier::Unqualified);
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
    SILModule &Module;
    const DIMemoryObjectInfo &TheMemory;
    SmallVectorImpl<DIMemoryUse> &Uses;
    SmallVectorImpl<TermInst*> &FailableInits;
    SmallVectorImpl<SILInstruction*> &Releases;
    
    /// This is true if definite initialization has finished processing assign
    /// and other ambiguous instructions into init vs assign classes.
    bool isDefiniteInitFinished;

    /// IsSelfOfNonDelegatingInitializer - This is true if we're looking at the
    /// top level of a 'self' variable in a non-delegating init method.
    bool IsSelfOfNonDelegatingInitializer;

    /// How should address_to_pointer be handled?
    ///
    /// In DefiniteInitialization it is considered as an inout parameter to get
    /// diagnostics about passing a let variable to an inout mutable-pointer
    /// argument.
    /// In PredictableMemOpt it is considered as an escape point to be
    /// conservative.
    bool TreatAddressToPointerAsInout;

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
                        SmallVectorImpl<TermInst*> &FailableInits,
                        SmallVectorImpl<SILInstruction*> &Releases,
                        bool isDefiniteInitFinished,
                        bool TreatAddressToPointerAsInout)
      : Module(TheMemory.MemoryInst->getModule()),
        TheMemory(TheMemory), Uses(Uses),
        FailableInits(FailableInits), Releases(Releases),
        isDefiniteInitFinished(isDefiniteInitFinished),
        TreatAddressToPointerAsInout(TreatAddressToPointerAsInout) {
    }

    /// This is the main entry point for the use walker.  It collects uses from
    /// the address and the refcount result of the allocation.
    void collectFrom() {
      IsSelfOfNonDelegatingInitializer =
        TheMemory.isNonDelegatingInit();

      // If this is a delegating initializer, collect uses specially.
      if (TheMemory.isDelegatingInit()) {
        if (TheMemory.getType()->hasReferenceSemantics())
          collectDelegatingClassInitSelfUses();
        else
          collectDelegatingValueTypeInitSelfUses();
      } else if (IsSelfOfNonDelegatingInitializer &&
                TheMemory.getType()->getClassOrBoundGenericClass() != nullptr) {
        if (TheMemory.isDerivedClassSelfOnly())
          collectDelegatingClassInitSelfUses();
        else
          // If this is a class pointer, we need to look through
          // ref_element_addrs.
          collectClassSelfUses();
      } else {
        if (auto *ABI = TheMemory.getContainer())
          collectContainerUses(ABI);
        else
          collectUses(TheMemory.getAddress(), 0);
      }

      if (!isa<MarkUninitializedInst>(TheMemory.MemoryInst)) {
        // Collect information about the retain count result as well.
        for (auto UI : TheMemory.MemoryInst->getUses()) {
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
    void collectContainerUses(AllocBoxInst *ABI);
    void recordFailureBB(TermInst *TI, SILBasicBlock *BB);
    void recordFailableInitCall(SILInstruction *I);
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
    NumElements = getElementCountRec(Module, UseTy,
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
    return collectUses(TEAI, BaseEltNo);

  assert(!IsSelfOfNonDelegatingInitializer && "self doesn't have tuple type");

  // tuple_element_addr P, 42 indexes into the current tuple element.
  // Recursively process its uses with the adjusted element number.
  unsigned FieldNo = TEAI->getFieldNo();
  auto T = TEAI->getOperand()->getType();
  if (T.is<TupleType>()) {
    for (unsigned i = 0; i != FieldNo; ++i) {
      SILType EltTy = T.getTupleElementType(i);
      BaseEltNo += getElementCountRec(Module, EltTy, false);
    }
  }
  
  collectUses(TEAI, BaseEltNo);
}

void ElementUseCollector::collectStructElementUses(StructElementAddrInst *SEAI,
                                                   unsigned BaseEltNo) {
  // Generally, we set the "InStructSubElement" flag and recursively process
  // the uses so that we know that we're looking at something within the
  // current element.
  if (!IsSelfOfNonDelegatingInitializer) {
    llvm::SaveAndRestore<bool> X(InStructSubElement, true);
    collectUses(SEAI, BaseEltNo);
    return;
  }

  // If this is the top level of 'self' in an init method, we treat each
  // element of the struct as an element to be analyzed independently.
  llvm::SaveAndRestore<bool> X(IsSelfOfNonDelegatingInitializer, false);

  for (auto *VD : SEAI->getStructDecl()->getStoredProperties()) {
    if (SEAI->getField() == VD)
      break;

    auto FieldType = SEAI->getOperand()->getType().getFieldType(VD, Module);
    BaseEltNo += getElementCountRec(Module, FieldType, false);
  }

  collectUses(SEAI, BaseEltNo);
}

void ElementUseCollector::collectContainerUses(AllocBoxInst *ABI) {
  for (Operand *UI : ABI->getUses()) {
    auto *User = UI->getUser();

    // Deallocations and retain/release don't affect the value directly.
    if (isa<DeallocBoxInst>(User))
      continue;
    if (isa<StrongRetainInst>(User))
      continue;
    if (isa<StrongReleaseInst>(User))
      continue;

    if (auto project = dyn_cast<ProjectBoxInst>(User)) {
      collectUses(User, project->getFieldIndex());
      continue;
    }

    // Other uses of the container are considered escapes of the values.
    for (unsigned field : indices(ABI->getBoxType()->getLayout()->getFields()))
      addElementUses(field,
                     ABI->getBoxType()->getFieldType(ABI->getModule(), field),
                     User, DIUseKind::Escape);
  }
}

void ElementUseCollector::collectUses(SILValue Pointer, unsigned BaseEltNo) {
  assert(Pointer->getType().isAddress() &&
         "Walked through the pointer to the value?");
  SILType PointeeType = Pointer->getType().getObjectType();

  /// This keeps track of instructions in the use list that touch multiple tuple
  /// elements and should be scalarized.  This is done as a second phase to
  /// avoid invalidating the use iterator.
  ///
  SmallVector<SILInstruction*, 4> UsesToScalarize;
  
  for (auto UI : Pointer->getUses()) {
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
    
    if (auto SUI = dyn_cast<StoreUnownedInst>(User))
      if (UI->getOperandNumber() == 1) {
        DIUseKind Kind;
        if (InStructSubElement)
          Kind = DIUseKind::PartialStore;
        else if (SUI->isInitializationOfDest())
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
    // through 'inout' arguments or for indirect returns.  InOut arguments are
    // treated as uses and may-store's, but an indirect return is treated as a
    // full store.
    //
    // Note that partial_apply instructions always close over their argument.
    //
    if (auto *Apply = dyn_cast<ApplyInst>(User)) {
      auto substConv = Apply->getSubstCalleeConv();
      unsigned ArgumentNumber = UI->getOperandNumber()-1;

      // If this is an out-parameter, it is like a store.
      unsigned NumIndirectResults = substConv.getNumIndirectSILResults();
      if (ArgumentNumber < NumIndirectResults) {
        assert(!InStructSubElement && "We're initializing sub-members?");
        addElementUses(BaseEltNo, PointeeType, User,
                       DIUseKind::Initialization);
        continue;

      // Otherwise, adjust the argument index.      
      } else {
        ArgumentNumber -= NumIndirectResults;
      }

      auto ParamConvention =
          substConv.getParameters()[ArgumentNumber].getConvention();

      switch (ParamConvention) {
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Guaranteed:
        llvm_unreachable("address value passed to indirect parameter");

      // If this is an in-parameter, it is like a load.
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
        addElementUses(BaseEltNo, PointeeType, User, DIUseKind::IndirectIn);
        continue;

      // If this is an @inout parameter, it is like both a load and store.
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable: {
        // If we're in the initializer for a struct, and this is a call to a
        // mutating method, we model that as an escape of self.  If an
        // individual sub-member is passed as inout, then we model that as an
        // inout use.
        auto Kind = DIUseKind::InOutUse;
        if ((TheMemory.isStructInitSelf() || TheMemory.isProtocolInitSelf()) &&
            Pointer == TheMemory.getAddress())
          Kind = DIUseKind::Escape;
 
        addElementUses(BaseEltNo, PointeeType, User, Kind);
        continue;
      }
      }
      llvm_unreachable("bad parameter convention");
    }
    
    if (isa<AddressToPointerInst>(User) && TreatAddressToPointerAsInout) {
      // address_to_pointer is a mutable escape, which we model as an inout use.
      addElementUses(BaseEltNo, PointeeType, User, DIUseKind::InOutUse);
      continue;
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
      collectUses(User, BaseEltNo);
      continue;
    }

    // init_existential_addr is modeled as an initialization store.
    if (isa<InitExistentialAddrInst>(User)) {
      assert(!InStructSubElement &&
             "init_existential_addr should not apply to struct subelements");
      Uses.push_back(DIMemoryUse(User, DIUseKind::Initialization,
                                 BaseEltNo, 1));
      continue;
    }
    
    // inject_enum_addr is modeled as an initialization store.
    if (isa<InjectEnumAddrInst>(User)) {
      assert(!InStructSubElement &&
             "inject_enum_addr the subelement of a struct unless in a ctor");
      Uses.push_back(DIMemoryUse(User, DIUseKind::Initialization,
                                 BaseEltNo, 1));
      continue;
    }

    // open_existential_addr is a use of the protocol value,
    // so it is modeled as a load.
    if (isa<OpenExistentialAddrInst>(User)) {
      Uses.push_back(DIMemoryUse(User, DIUseKind::Load, BaseEltNo, 1));
      // TODO: Is it safe to ignore all uses of the open_existential_addr?
      continue;
    }

    // We model destroy_addr as a release of the entire value.
    if (isa<DestroyAddrInst>(User)) {
      Releases.push_back(User);
      continue;
    }

    if (isa<DeallocStackInst>(User)) {
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
    SILBuilderWithScope AddrBuilder(++SILBasicBlock::iterator(PointerInst),
                                    PointerInst);
    getScalarizedElementAddresses(Pointer, AddrBuilder, PointerInst->getLoc(),
                                  ElementAddrs);
    
    SmallVector<SILValue, 4> ElementTmps;
    for (auto *User : UsesToScalarize) {
      ElementTmps.clear();

      DEBUG(llvm::errs() << "  *** Scalarizing: " << *User << "\n");

      // Scalarize LoadInst
      if (auto *LI = dyn_cast<LoadInst>(User)) {
        SILValue Result = scalarizeLoad(LI, ElementAddrs);
        LI->replaceAllUsesWith(Result);
        LI->eraseFromParent();
        continue;
      }

      // Scalarize AssignInst
      if (auto *AI = dyn_cast<AssignInst>(User)) {
        SILBuilderWithScope B(User, AI);
        getScalarizedElements(AI->getOperand(0), ElementTmps, AI->getLoc(), B);

        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
          B.createAssign(AI->getLoc(), ElementTmps[i], ElementAddrs[i]);
        AI->eraseFromParent();
        continue;
      }
      
      // Scalarize StoreInst
      if (auto *SI = dyn_cast<StoreInst>(User)) {
        SILBuilderWithScope B(User, SI);
        getScalarizedElements(SI->getOperand(0), ElementTmps, SI->getLoc(), B);
        
        for (unsigned i = 0, e = ElementAddrs.size(); i != e; ++i)
          B.createStore(SI->getLoc(), ElementTmps[i], ElementAddrs[i],
                        StoreOwnershipQualifier::Unqualified);
        SI->eraseFromParent();
        continue;
      }
      
      // Scalarize CopyAddrInst.
      auto *CAI = cast<CopyAddrInst>(User);
      SILBuilderWithScope B(User, CAI);

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

/// recordFailureBB - we have to detect if the self box contents were consumed.
/// Do this by checking for a store into the self box in the success branch.
/// Once we rip this out of SILGen, DI will be able to figure this out in a
/// more logical manner.
void ElementUseCollector::recordFailureBB(TermInst *TI,
                                          SILBasicBlock *BB) {
  for (auto &II : *BB)
    if (auto *SI = dyn_cast<StoreInst>(&II)) {
      if (SI->getDest() == TheMemory.MemoryInst) {
        FailableInits.push_back(TI);
        return;
      }
    }
}

/// recordFailableInitCall - If I is a call of a throwing or failable
/// initializer, add the actual conditional (try_apply or cond_br) to
/// the set for dataflow analysis.
void ElementUseCollector::recordFailableInitCall(SILInstruction *I) {
  // If we have a store to self inside the normal BB, we have a 'real'
  // try_apply. Otherwise, this is a 'try? self.init()' or similar,
  // and there is a store after.
  if (auto *TAI = dyn_cast<TryApplyInst>(I)) {
    recordFailureBB(TAI, TAI->getNormalBB());
    return;
  }

  if (auto *AI = dyn_cast<ApplyInst>(I)) {
    // See if this is an optional initializer.
    for (auto UI : AI->getUses()) {
      SILInstruction *User = UI->getUser();

      if (!isa<SelectEnumInst>(User) && !isa<SelectEnumAddrInst>(User))
        continue;
      
      if (!User->hasOneUse())
        continue;
      
      User = User->use_begin()->getUser();
      if (auto *CBI = dyn_cast<CondBranchInst>(User)) {
        recordFailureBB(CBI, CBI->getTrueBB());
        return;
      }
    }
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
    SILType T = TheMemory.MemorySILType;
    auto *NTD = T.getNominalOrBoundGenericNominal();
    unsigned NumElements = 0;
    for (auto *VD : NTD->getStoredProperties()) {
      EltNumbering[VD] = NumElements;
      NumElements += getElementCountRec(Module, T.getFieldType(VD, Module),
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

  // Okay, given that we have a proper setup, we walk the use chains of the self
  // box to find any accesses to it.  The possible uses are one of:
  //   1) The initialization store (TheStore).
  //   2) Loads of the box, which have uses of self hanging off of them.
  //   3) An assign to the box, which happens at super.init.
  //   4) Potential escapes after super.init, if self is closed over.
  // Handle each of these in turn.
  //
  for (auto UI : MUI->getUses()) {
    SILInstruction *User = UI->getUser();

    // Stores to self are initializations store or the rebind of self as
    // part of the super.init call.  Ignore both of these.
    if (isa<StoreInst>(User) && UI->getOperandNumber() == 1)
      continue;

    // Loads of the box produce self, so collect uses from them.
    if (auto *LI = dyn_cast<LoadInst>(User)) {
      collectClassSelfUses(LI, TheMemory.MemorySILType, EltNumbering);
      continue;
    }

    // destroyaddr on the box is load+release, which is treated as a release.
    if (isa<DestroyAddrInst>(User) || isa<StrongReleaseInst>(User)) {
      Releases.push_back(User);
      continue;
    }
    
    // Ignore the deallocation of the stack box.  Its contents will be
    // uninitialized by the point it executes.
    if (isa<DeallocStackInst>(User))
      continue;

    // We can safely handle anything else as an escape.  They should all happen
    // after super.init is invoked.  As such, all elements must be initialized
    // and super.init must be called.
    Uses.push_back(DIMemoryUse(User, DIUseKind::Load,
                               0, TheMemory.NumElements));
  }
}

/// isSuperInitUse - If this "upcast" is part of a call to super.init, return
/// the Apply instruction for the call, otherwise return null.
static SILInstruction *isSuperInitUse(UpcastInst *Inst) {

  // "Inst" is an Upcast instruction.  Check to see if it is used by an apply
  // that came from a call to super.init.
  for (auto UI : Inst->getUses()) {
    auto *inst = UI->getUser();
    // If this used by another upcast instruction, recursively handle it, we may
    // have a multiple upcast chain.
    if (auto *UCIU = dyn_cast<UpcastInst>(inst))
      if (auto *subAI = isSuperInitUse(UCIU))
        return subAI;

    // The call to super.init has to either be an apply or a try_apply.
    if (!isa<FullApplySite>(inst))
      continue;

    auto *LocExpr = inst->getLoc().getAsASTNode<ApplyExpr>();
    if (!LocExpr) {
      // If we're reading a .sil file, treat a call to "superinit" as a
      // super.init call as a hack to allow us to write testcases.
      auto *AI = dyn_cast<ApplyInst>(inst);
      if (AI && inst->getLoc().isSILFile())
        if (auto *Fn = AI->getReferencedFunction())
          if (Fn->getName() == "superinit")
            return inst;
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

    if (LocExpr->getArg()->isSuperExpr())
      return inst;

    // Instead of super_ref_expr, we can also get this for inherited delegating
    // initializers:

    // (derived_to_base_expr implicit type='C'
    //   (declref_expr type='D' decl='self'))
    if (auto *DTB = dyn_cast<DerivedToBaseExpr>(LocExpr->getArg()))
      if (auto *DRE = dyn_cast<DeclRefExpr>(DTB->getSubExpr()))
        if (DRE->getDecl()->isImplicit() &&
            DRE->getDecl()->getName().str() == "self")
          return inst;
  }

  return nullptr;
}

/// isSelfInitUse - Return true if this apply_inst is a call to self.init.
static bool isSelfInitUse(SILInstruction *I) {
  // If we're reading a .sil file, treat a call to "selfinit" as a
  // self.init call as a hack to allow us to write testcases.
  if (I->getLoc().isSILFile()) {
    if (auto *AI = dyn_cast<ApplyInst>(I))
      if (auto *Fn = AI->getReferencedFunction())
        if (Fn->getName().startswith("selfinit"))
          return true;
    
    // If this is a copy_addr to a delegating self MUI, then we treat it as a
    // self init for the purposes of testcases.
    if (auto *CAI = dyn_cast<CopyAddrInst>(I))
      if (auto *MUI = dyn_cast<MarkUninitializedInst>(CAI->getDest()))
        if (MUI->isDelegatingSelf())
          return true;
    return false;
  }

  // Otherwise, a self.init call must have location info, and must be an expr
  // to be considered.
  auto *LocExpr = I->getLoc().getAsASTNode<Expr>();
  if (!LocExpr) return false;

  // If this is a force_value_expr, it might be a self.init()! call, look
  // through it.
  if (auto *FVE = dyn_cast<ForceValueExpr>(LocExpr))
    LocExpr = FVE->getSubExpr();
  
  // If we have the rebind_self_in_constructor_expr, then the call is the
  // sub-expression.
  if (auto *RB = dyn_cast<RebindSelfInConstructorExpr>(LocExpr)) {
    LocExpr = RB->getSubExpr();
    // Look through TryExpr or ForceValueExpr, but not both.
    if (auto *TE = dyn_cast<AnyTryExpr>(LocExpr))
      LocExpr = TE->getSubExpr();
    else if (auto *FVE = dyn_cast<ForceValueExpr>(LocExpr))
      LocExpr = FVE->getSubExpr();
    
  }

  // Look through covariant return, if any.
  if (auto CRE = dyn_cast<CovariantReturnConversionExpr>(LocExpr))
    LocExpr = CRE->getSubExpr();
  
  // This is a self.init call if structured like this:
  //
  // (call_expr type='SomeClass'
  //   (dot_syntax_call_expr type='() -> SomeClass' self
  //     (other_constructor_ref_expr implicit decl=SomeClass.init)
  //     (decl_ref_expr type='SomeClass', "self"))
  //   (...some argument...)
  //
  // Or like this:
  //
  // (call_expr type='SomeClass'
  //   (dot_syntax_call_expr type='() -> SomeClass' self
  //     (decr_ref_expr implicit decl=SomeClass.init)
  //     (decl_ref_expr type='SomeClass', "self"))
  //   (...some argument...)
  //
  if (auto *AE = dyn_cast<ApplyExpr>(LocExpr)) {
    if ((AE = dyn_cast<ApplyExpr>(AE->getFn()))) {
      if (isa<OtherConstructorDeclRefExpr>(AE->getFn()))
        return true;
      if (auto *DRE = dyn_cast<DeclRefExpr>(AE->getFn()))
        if (auto *CD = dyn_cast<ConstructorDecl>(DRE->getDecl()))
          if (CD->isFactoryInit())
            return true;
    }
  }
  return false;
}


/// Return true if this SILBBArgument is the result of a call to self.init.
static bool isSelfInitUse(SILArgument *Arg) {
  // We only handle a very simple pattern here where there is a single
  // predecessor to the block, and the predecessor instruction is a try_apply
  // of a throwing delegated init.
  auto *BB = Arg->getParent();
  auto *Pred = BB->getSinglePredecessorBlock();

  // The two interesting cases are where self.init throws, in which case
  // the argument came from a try_apply, or if self.init is failable,
  // in which case we have a switch_enum.
  if (!Pred ||
      (!isa<TryApplyInst>(Pred->getTerminator()) &&
       !isa<SwitchEnumInst>(Pred->getTerminator())))
    return false;

  return isSelfInitUse(Pred->getTerminator());
}

/// Returns true if \p Method is a callee of a full apply site that takes in \p
/// Pointer as an argument. In such a case, we want to ignore the class method
/// use and allow for the use by the apply inst to take precedence.
static bool shouldIgnoreClassMethodUseError(
    ClassMethodInst *Method, SILValue Pointer) {

  // In order to work around use-list ordering issues, if this method is called
  // by an apply site that has I as an argument, we want to process the apply
  // site for errors to emit, not the class method. If we do not obey these
  // conditions, then continue to treat the class method as an escape.
  auto CheckFullApplySite = [&](Operand *Op) -> bool {
    FullApplySite FAS(Op->getUser());
    if (!FAS || (FAS.getCallee() != Method))
      return false;
    return llvm::any_of(
        FAS.getArgumentsWithoutIndirectResults(),
        [&](SILValue Arg) -> bool { return Arg == Pointer; });
  };

  return llvm::any_of(Method->getUses(), CheckFullApplySite);
}

void ElementUseCollector::
collectClassSelfUses(SILValue ClassPointer, SILType MemorySILType,
                     llvm::SmallDenseMap<VarDecl*, unsigned> &EltNumbering) {
  for (auto UI : ClassPointer->getUses()) {
    auto *User = UI->getUser();

    // super_method always looks at the metatype for the class, not at any of
    // its stored properties, so it doesn't have any DI requirements.
    if (isa<SuperMethodInst>(User))
      continue;


    // ref_element_addr P, #field lookups up a field.
    if (auto *REAI = dyn_cast<RefElementAddrInst>(User)) {
      assert(EltNumbering.count(REAI->getField()) &&
             "ref_element_addr not a local field?");
      // Recursively collect uses of the fields.  Note that fields of the class
      // could be tuples, so they may be tracked as independent elements.
      llvm::SaveAndRestore<bool> X(IsSelfOfNonDelegatingInitializer, false);
      collectUses(REAI, EltNumbering[REAI->getField()]);
      continue;
    }

    // retains of self in class constructors can be ignored since we do not care
    // about the retain that we are producing, but rather the consumer of the
    // retain. This /should/ be true today and will be verified as true in
    // Semantic SIL.
    if (isa<StrongRetainInst>(User)) {
      continue;
    }

    // releases of self are tracked as a release. In the case of a failing
    // initializer, the release on the exit path needs to cleanup the partially
    // initialized elements.
    if (isa<StrongReleaseInst>(User)) {
      Releases.push_back(User);
      continue;
    }

    if (auto *Method = dyn_cast<ClassMethodInst>(User)) {
      if (shouldIgnoreClassMethodUseError(Method, ClassPointer)){
        continue;
      }
    }

    // If this is an upcast instruction, it is a conversion of self to the base.
    // This is either part of a super.init sequence, or a general superclass
    // access.
    if (auto *UCI = dyn_cast<UpcastInst>(User))
      if (auto *AI = isSuperInitUse(UCI)) {
        // We remember the applyinst as the super.init site, not the upcast.
        Uses.push_back(DIMemoryUse(AI, DIUseKind::SuperInit,
                                   0, TheMemory.NumElements));
        recordFailableInitCall(AI);
        continue;
      }

    // If this is an ApplyInst, check to see if this is part of a self.init
    // call in a delegating initializer.
    DIUseKind Kind = DIUseKind::Load;
    if (isa<FullApplySite>(User) && isSelfInitUse(User)) {
      Kind = DIUseKind::SelfInit;
      recordFailableInitCall(User);
    }

    // If this is a ValueMetatypeInst, this is a simple reference
    // to "type(of:)", which is always fine, even if self is
    // uninitialized.
    if (isa<ValueMetatypeInst>(User))
      continue;

    // If this is a partial application of self, then this is an escape point
    // for it.
    if (isa<PartialApplyInst>(User))
      Kind = DIUseKind::Escape;

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

  // We walk the use chains of the self MUI to find any accesses to it.  The
  // possible uses are:
  //   1) The initialization store.
  //   2) Loads of the box, which have uses of self hanging off of them.
  //   3) An assign to the box, which happens at super.init.
  //   4) Potential escapes after super.init, if self is closed over.
  // Handle each of these in turn.
  //
  for (auto UI : MUI->getUses()) {
    SILInstruction *User = UI->getUser();

    // Stores to self are initializations store or the rebind of self as
    // part of the super.init call.  Ignore both of these.
    if (isa<StoreInst>(User) && UI->getOperandNumber() == 1)
      continue;

    // For class initializers, the assign into the self box may be
    // captured as SelfInit or SuperInit elsewhere.
    if (TheMemory.isClassInitSelf() &&
        isa<AssignInst>(User) && UI->getOperandNumber() == 1) {
      // If the source of the assignment is an application of a C
      // function, there is no metatype argument, so treat the
      // assignment to the self box as the initialization.
      if (auto apply = dyn_cast<ApplyInst>(cast<AssignInst>(User)->getSrc())) {
        if (auto fn = apply->getCalleeFunction()) {
          if (fn->getRepresentation()
                == SILFunctionTypeRepresentation::CFunctionPointer) {
            Uses.push_back(DIMemoryUse(User, DIUseKind::SelfInit, 0, 1));
            continue;
          }
        }
      }

    }

    // Stores *to* the allocation are writes.  If the value being stored is a
    // call to self.init()... then we have a self.init call.
    if (auto *AI = dyn_cast<AssignInst>(User)) {
      if (auto *AssignSource = dyn_cast<SILInstruction>(AI->getOperand(0)))
        if (isSelfInitUse(AssignSource)) {
          Uses.push_back(DIMemoryUse(User, DIUseKind::SelfInit, 0, 1));
          continue;
        }
      if (auto *AssignSource = dyn_cast<SILArgument>(AI->getOperand(0)))
        if (AssignSource->getParent() == AI->getParent())
          if (isSelfInitUse(AssignSource)) {
            Uses.push_back(DIMemoryUse(User, DIUseKind::SelfInit, 0, 1));
            continue;
          }
    }
    
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      if (isSelfInitUse(CAI)) {
        Uses.push_back(DIMemoryUse(User, DIUseKind::SelfInit, 0, 1));
        continue;
      }
    }

    // Loads of the box produce self, so collect uses from them.
    if (auto *LI = dyn_cast<LoadInst>(User)) {
      
      // If we have a load, then this is a use of the box.  Look at the uses of
      // the load to find out more information.
      for (auto UI : LI->getUses()) {
        auto *User = UI->getUser();

        // super_method always looks at the metatype for the class, not at any of
        // its stored properties, so it doesn't have any DI requirements.
        if (isa<SuperMethodInst>(User))
          continue;

        // We ignore retains of self.
        if (isa<StrongRetainInst>(User))
          continue;

        // A release of a load from the self box in a class delegating
        // initializer might be releasing an uninitialized self, which requires
        // special processing.
        if (isa<StrongReleaseInst>(User)) {
          Releases.push_back(User);
          continue;
        }

        if (auto *Method = dyn_cast<ClassMethodInst>(User)) {
          // class_method that refers to an initializing constructor is a method
          // lookup for delegation, which is ignored.
          if (Method->getMember().kind == SILDeclRef::Kind::Initializer)
            continue;

          /// Returns true if \p Method used by an apply in a way that we know
          /// will cause us to emit a better error.
          if (shouldIgnoreClassMethodUseError(Method, LI))
            continue;
        }

        // If this is an upcast instruction, it is a conversion of self to the
        // base.  This is either part of a super.init sequence, or a general
        // superclass access.  We special case super.init calls since they are
        // part of the object lifecycle.
        if (auto *UCI = dyn_cast<UpcastInst>(User)) {
          if (auto *subAI = isSuperInitUse(UCI)) {
            Uses.push_back(DIMemoryUse(subAI, DIUseKind::SuperInit, 0, 1));
            recordFailableInitCall(subAI);
            continue;
          }
        }

        // We only track two kinds of uses for delegating initializers:
        // calls to self.init, and "other", which we choose to model as escapes.
        // This intentionally ignores all stores, which (if they got emitted as
        // copyaddr or assigns) will eventually get rewritten as assignments
        // (not initializations), which is the right thing to do.
        DIUseKind Kind = DIUseKind::Escape;

        // If this is an ApplyInst, check to see if this is part of a self.init
        // call in a delegating initializer.
        if (isa<FullApplySite>(User) && isSelfInitUse(User)) {
          Kind = DIUseKind::SelfInit;
          recordFailableInitCall(User);
        }

        // If this load's value is being stored back into the delegating
        // mark_uninitialized buffer and it is a self init use, skip the
        // use. This is to handle situations where due to usage of a metatype to
        // allocate, we do not actually consume self.
        if (auto *SI = dyn_cast<StoreInst>(User)) {
          if (SI->getDest() == MUI && isSelfInitUse(User)) {
            continue;
          }
        }

        // A simple reference to "type(of:)" is always fine,
        // even if self is uninitialized.
        if (isa<ValueMetatypeInst>(User)) {
          continue;
        }

        Uses.push_back(DIMemoryUse(User, Kind, 0, 1));
      }
      continue;
    }
   
    // destroyaddr on the box is load+release, which is treated as a release.
    if (isa<DestroyAddrInst>(User)) {
      Releases.push_back(User);
      continue;
    }

    // We can safely handle anything else as an escape.  They should all happen
    // after self.init is invoked.
    Uses.push_back(DIMemoryUse(User, DIUseKind::Escape, 0, 1));
  }

  // The MUI must be used on a project_box or alloc_stack instruction.  Chase
  // down the box value to see if there are any releases.
  if (isa<AllocStackInst>(MUI->getOperand()))
    return;

  auto *PBI = cast<ProjectBoxInst>(MUI->getOperand());
  auto *ABI = cast<AllocBoxInst>(PBI->getOperand());

  for (auto UI : ABI->getUses()) {
    SILInstruction *User = UI->getUser();
    if (isa<StrongReleaseInst>(User))
      Releases.push_back(User);
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
    
    // destroy_addr is a release of the entire value.  This can be an early
    // release for a conditional initializer.
    if (isa<DestroyAddrInst>(User)) {
      Releases.push_back(User);
      continue;
    }
    
    // We only track two kinds of uses for delegating initializers:
    // calls to self.init, and "other", which we choose to model as escapes.
    // This intentionally ignores all stores, which (if they got emitted as
    // copyaddr or assigns) will eventually get rewritten as assignments
    // (not initializations), which is the right thing to do.
    DIUseKind Kind = DIUseKind::Escape;

    // Stores *to* the allocation are writes.  If the value being stored is a
    // call to self.init()... then we have a self.init call.
    if (auto *AI = dyn_cast<AssignInst>(User)) {
      if (auto *AssignSource = dyn_cast<SILInstruction>(AI->getOperand(0)))
        if (isSelfInitUse(AssignSource))
          Kind = DIUseKind::SelfInit;
      if (auto *AssignSource = dyn_cast<SILArgument>(AI->getOperand(0)))
        if (AssignSource->getParent() == AI->getParent())
          if (isSelfInitUse(AssignSource))
            Kind = DIUseKind::SelfInit;
    }
    
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      if (isSelfInitUse(CAI))
        Kind = DIUseKind::SelfInit;
    }
    
    
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
                                     SmallVectorImpl<TermInst*> &FailableInits,
                                     SmallVectorImpl<SILInstruction*> &Releases,
                                     bool isDIFinished,
                                     bool TreatAddressToPointerAsInout) {
  ElementUseCollector(MemoryInfo, Uses, FailableInits, Releases, isDIFinished,
                      TreatAddressToPointerAsInout)
      .collectFrom();
}
