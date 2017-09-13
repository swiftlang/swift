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
#include "swift/AST/Expr.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/StringExtras.h"

#ifdef SWIFT_SILOPTIMIZER_PASSMANAGER_DIMEMORYUSECOLLECTOROWNERSHIP_H
#error "Included ownership header?!"
#endif

using namespace swift;

//===----------------------------------------------------------------------===//
//                  DIMemoryObjectInfo Implementation
//===----------------------------------------------------------------------===//

static unsigned getElementCountRec(SILModule &Module, SILType T) {
  // If this is a tuple, it is always recursively flattened.
  if (CanTupleType TT = T.getAs<TupleType>()) {
    unsigned NumElements = 0;
    for (unsigned i = 0, e = TT->getNumElements(); i < e; i++)
      NumElements += getElementCountRec(Module, T.getTupleElementType(i));
    return NumElements;
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
    llvm_unreachable(
        "Predictable Mem Opts should only be analyzing alloc_box/alloc_stack");
  }

  // Break down the initializer.
  NumElements = getElementCountRec(Module, MemorySILType);
}

SILInstruction *DIMemoryObjectInfo::getFunctionEntryPoint() const {
  return &*getFunction().begin()->begin();
}

/// Given a symbolic element number, return the type of the element.
static SILType getElementTypeRec(SILModule &Module, SILType T, unsigned EltNo) {
  // If this is a tuple type, walk into it.
  if (CanTupleType TT = T.getAs<TupleType>()) {
    for (unsigned i = 0, e = TT->getNumElements(); i < e; i++) {
      auto FieldType = T.getTupleElementType(i);
      unsigned NumFieldElements = getElementCountRec(Module, FieldType);
      if (EltNo < NumFieldElements)
        return getElementTypeRec(Module, FieldType, EltNo);
      EltNo -= NumFieldElements;
    }
    llvm::report_fatal_error("invalid element number");
  }

  // Otherwise, it is a leaf element.
  assert(EltNo == 0);
  return T;
}

/// getElementTypeRec - Return the swift type of the specified element.
SILType DIMemoryObjectInfo::getElementType(unsigned EltNo) const {
  auto &Module = MemoryInst->getModule();
  return getElementTypeRec(Module, MemorySILType, EltNo);
}

/// computeTupleElementAddress - Given a tuple element number (in the flattened
/// sense) return a pointer to a leaf element of the specified number.
SILValue DIMemoryObjectInfo::
emitElementAddress(unsigned EltNo, SILLocation Loc, SILBuilder &B) const {
  SILValue Ptr = getAddress();
  auto &Module = MemoryInst->getModule();

  auto PointeeType = MemorySILType;

  while (1) {
    // If we have a tuple, flatten it.
    if (CanTupleType TT = PointeeType.getAs<TupleType>()) {

      // Figure out which field we're walking into.
      unsigned FieldNo = 0;
      for (unsigned i = 0, e = TT->getNumElements(); i < e; i++) {
        auto EltTy = PointeeType.getTupleElementType(i);
        unsigned NumSubElt = getElementCountRec(Module, EltTy);
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
      unsigned NumFieldElements = getElementCountRec(Module, FieldTy);

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

  if (auto *VD = dyn_cast_or_null<ValueDecl>(getLoc().getAsASTNode<Decl>()))
    Result = VD->getBaseName().userFacingName();
  else
    Result = "<unknown>";

  // Get the path through a tuple, if relevant.
  getPathStringToElementRec(Module, MemorySILType, Element, Result);

  // Otherwise, we can't.
  return nullptr;
}

/// If the specified value is a 'let' property in an initializer, return true.
bool DIMemoryObjectInfo::isElementLetProperty(unsigned Element) const {
  // If we aren't representing 'self' in a non-delegating initializer, then we
  // can't have 'let' properties.
  return IsLet;
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
  TupleType *TT = Pointer->getType().castTo<TupleType>();
  for (auto Index : indices(TT->getElements())) {
    ElementAddrs.push_back(B.createTupleElementAddr(Loc, Pointer, Index));
  }
}

/// Given an RValue of aggregate type, compute the values of the elements by
/// emitting a series of tuple_element instructions.
static void getScalarizedElements(SILValue V,
                                  SmallVectorImpl<SILValue> &ElementVals,
                                  SILLocation Loc, SILBuilder &B) {
  TupleType *TT = V->getType().castTo<TupleType>();
  for (auto Index : indices(TT->getElements())) {
    ElementVals.push_back(B.emitTupleExtract(Loc, V, Index));
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
      if (auto *ABI = TheMemory.getContainer())
        collectContainerUses(ABI);
      else
        collectUses(TheMemory.getAddress(), 0);

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

  private:
    void collectUses(SILValue Pointer, unsigned BaseEltNo);
    void collectContainerUses(AllocBoxInst *ABI);
    void recordFailureBB(TermInst *TI, SILBasicBlock *BB);
    void recordFailableInitCall(SILInstruction *I);
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
    NumElements = getElementCountRec(Module, UseTy);

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

  // tuple_element_addr P, 42 indexes into the current tuple element.
  // Recursively process its uses with the adjusted element number.
  unsigned FieldNo = TEAI->getFieldNo();
  auto T = TEAI->getOperand()->getType();
  if (T.is<TupleType>()) {
    for (unsigned i = 0; i != FieldNo; ++i) {
      SILType EltTy = T.getTupleElementType(i);
      BaseEltNo += getElementCountRec(Module, EltTy);
    }
  }
  
  collectUses(TEAI, BaseEltNo);
}

void ElementUseCollector::collectStructElementUses(StructElementAddrInst *SEAI,
                                                   unsigned BaseEltNo) {
  // Generally, we set the "InStructSubElement" flag and recursively process
  // the uses so that we know that we're looking at something within the
  // current element.
  llvm::SaveAndRestore<bool> X(InStructSubElement, true);
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

// Returns true when the instruction represents added instrumentation for
/// run-time sanitizers.
static bool isSanitizerInstrumentation(SILInstruction *Instruction,
                                       ASTContext &Ctx) {
  auto *BI = dyn_cast<BuiltinInst>(Instruction);
  if (!BI)
    return false;

  Identifier Name = BI->getName();
  if (Name == Ctx.getIdentifier("tsanInoutAccess"))
    return true;

  return false;
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

    // Look through begin_access.
    if (isa<BeginAccessInst>(User)) {
      collectUses(User, BaseEltNo);
      continue;
    }

    // Ignore end_access.
    if (isa<EndAccessInst>(User)) {
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
      case ParameterConvention::Indirect_In_Constant:
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
        addElementUses(BaseEltNo, PointeeType, User, DIUseKind::InOutUse);
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

    // Sanitizer instrumentation is not user visible, so it should not
    // count as a use and must not affect compile-time diagnostics.
    if (isSanitizerInstrumentation(User, Module.getASTContext()))
      continue;

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

static void
collectBorrowedSuperUses(UpcastInst *Inst,
                         llvm::SmallVectorImpl<UpcastInst *> &UpcastUsers) {
  for (auto *Use : Inst->getUses()) {
    if (auto *URCI = dyn_cast<UncheckedRefCastInst>(Use->getUser())) {
      for (auto *InnerUse : URCI->getUses()) {
        if (auto *InnerUpcastUser = dyn_cast<UpcastInst>(InnerUse->getUser())) {
          UpcastUsers.push_back(InnerUpcastUser);
        }
      }
    }
  }
}

/// isSuperInitUse - If this "upcast" is part of a call to super.init, return
/// the Apply instruction for the call, otherwise return null.
static SILInstruction *isSuperInitUse(UpcastInst *Inst) {

  // "Inst" is an Upcast instruction.  Check to see if it is used by an apply
  // that came from a call to super.init.
  for (auto *UI : Inst->getUses()) {
    auto *User = UI->getUser();
    // If this used by another upcast instruction, recursively handle it, we may
    // have a multiple upcast chain.
    if (auto *UCIU = dyn_cast<UpcastInst>(User))
      if (auto *subAI = isSuperInitUse(UCIU))
        return subAI;

    // The call to super.init has to either be an apply or a try_apply.
    if (!isa<FullApplySite>(User))
      continue;

    auto *LocExpr = User->getLoc().getAsASTNode<ApplyExpr>();
    if (!LocExpr) {
      // If we're reading a .sil file, treat a call to "superinit" as a
      // super.init call as a hack to allow us to write testcases.
      auto *AI = dyn_cast<ApplyInst>(User);
      if (AI && User->getLoc().isSILFile())
        if (auto *Fn = AI->getReferencedFunction())
          if (Fn->getName() == "superinit")
            return User;
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
      return User;

    // Instead of super_ref_expr, we can also get this for inherited delegating
    // initializers:

    // (derived_to_base_expr implicit type='C'
    //   (declref_expr type='D' decl='self'))
    if (auto *DTB = dyn_cast<DerivedToBaseExpr>(LocExpr->getArg()))
      if (auto *DRE = dyn_cast<DeclRefExpr>(DTB->getSubExpr()))
        if (DRE->getDecl()->isImplicit() &&
            DRE->getDecl()->getBaseName() == "self")
          return User;
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
    if (auto *UCI = dyn_cast<UpcastInst>(User)) {
      if (auto *AI = isSuperInitUse(UCI)) {
        // We remember the applyinst as the super.init site, not the upcast.
        Uses.push_back(DIMemoryUse(AI, DIUseKind::SuperInit,
                                   0, TheMemory.NumElements));
        recordFailableInitCall(AI);

        // Now that we know that we have a super.init site, check if our upcast
        // has any borrow users. These used to be represented by a separate
        // load, but now with sil ownership, they are represented as borrows
        // from the same upcast as the super init user upcast.
        llvm::SmallVector<UpcastInst *, 4> ExtraUpcasts;
        collectBorrowedSuperUses(UCI, ExtraUpcasts);
        for (auto *Upcast : ExtraUpcasts) {
          Uses.push_back(
              DIMemoryUse(Upcast, DIUseKind::Load, 0, TheMemory.NumElements));
        }
        continue;
      }
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
