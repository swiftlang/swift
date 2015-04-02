//===--- PredictableMemOpt.cpp - Perform predictable memory optzns --------===//
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

#define DEBUG_TYPE "predictable-memopt"
#include "swift/Basic/Fallthrough.h"
#include "swift/SILPasses/Passes.h"
#include "DIMemoryUseCollector.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/Statistic.h"
using namespace swift;

STATISTIC(NumLoadPromoted, "Number of loads promoted");
STATISTIC(NumDestroyAddrPromoted, "Number of destroy_addrs promoted");
STATISTIC(NumAllocRemoved, "Number of allocations completely removed");

//===----------------------------------------------------------------------===//
// Subelement Analysis Implementation
//===----------------------------------------------------------------------===//

// We can only analyze components of structs whose storage is fully accessible
// from Swift.
static StructDecl *
getFullyReferenceableStruct(SILType Ty) {
  auto SD = Ty.getStructOrBoundGenericStruct();
  if (!SD || SD->hasUnreferenceableStorage())
    return nullptr;
  return SD;
}

static unsigned getNumSubElements(SILType T, SILModule &M) {
  if (!M.getTypeLowering(T).isValid())
    return 0;

  if (auto TT = T.getAs<TupleType>()) {
    unsigned NumElements = 0;
    for (auto index : indices(TT.getElementTypes()))
      NumElements += getNumSubElements(T.getTupleElementType(index), M);
    return NumElements;
  }
  
  if (auto *SD = getFullyReferenceableStruct(T)) {
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

/// Compute the subelement number indicated by the specified pointer (which is
/// derived from the root by a series of tuple/struct element addresses) by
/// treating the type as a linearized namespace with sequential elements.  For
/// example, given:
///
///   root = alloc { a: { c: i64, d: i64 }, b: (i64, i64) }
///   tmp1 = struct_element_addr root, 1
///   tmp2 = tuple_element_addr tmp1, 0
///
/// This will return a subelement number of 2.
///
/// If this pointer is to within a existential projection, it returns ~0U.
///
static unsigned computeSubelement(SILValue Pointer, SILInstruction *RootInst) {
  unsigned SubEltNumber = 0;
  SILModule &M = RootInst->getModule();
  
  while (1) {
    // If we got to the root, we're done.
    if (RootInst == Pointer.getDef())
      return SubEltNumber;
    
    auto *Inst = cast<SILInstruction>(Pointer);
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(Inst)) {
      SILType TT = TEAI->getOperand().getType();
      
      // Keep track of what subelement is being referenced.
      for (unsigned i = 0, e = TEAI->getFieldNo(); i != e; ++i) {
        SubEltNumber += getNumSubElements(TT.getTupleElementType(i), M);
      }
      Pointer = TEAI->getOperand();
    } else if (auto *SEAI = dyn_cast<StructElementAddrInst>(Inst)) {
      SILType ST = SEAI->getOperand().getType();
      
      // Keep track of what subelement is being referenced.
      StructDecl *SD = SEAI->getStructDecl();
      for (auto *D : SD->getStoredProperties()) {
        if (D == SEAI->getField()) break;
        SubEltNumber += getNumSubElements(ST.getFieldType(D, M), M);
      }
      
      Pointer = SEAI->getOperand();
    } else {
      assert((isa<InitExistentialAddrInst>(Inst) || isa<InjectEnumAddrInst>(Inst))&&
             "Unknown access path instruction");
      // Cannot promote loads and stores from within an existential projection.
      return ~0U;
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
  if (auto *SD = getFullyReferenceableStruct(ValTy)) {
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
    
    bool doIt();
    
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
    
    bool tryToRemoveDeadAllocation();
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
    unsigned StartSubElt = computeSubelement(Inst->getOperand(1), TheMemory);
    assert(StartSubElt != ~0U && "Store within enum projection not handled");
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
    unsigned StartSubElt = computeSubelement(Inst->getOperand(1), TheMemory);
    assert(StartSubElt != ~0U && "Store within enum projection not handled");
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
  
  
  SILBuilderWithScope<16> B(Inst);
  
  if (TupleType *TT = LoadTy.getAs<TupleType>()) {
    SmallVector<SILValue, 4> ResultElts;
    
    for (unsigned EltNo : indices(TT->getElements())) {
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
  
  // Extract struct elements from fully referenceable structs.
  if (auto *SD = getFullyReferenceableStruct(LoadTy)) {
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
  unsigned FirstElt = computeSubelement(Inst->getOperand(0), TheMemory);
  
  // If this is a load from within an enum projection, we can't promote it since
  // we don't track subelements in a type that could be changing.
  if (FirstElt == ~0U)
    return false;
  
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
  DEBUG(llvm::dbgs() << "  *** Promoting load: " << *Inst << "\n");
  DEBUG(llvm::dbgs() << "      To value: " << *NewVal.getDef() << "\n");
  
  SILValue(Inst, 0).replaceAllUsesWith(NewVal);
  SILValue Addr = Inst->getOperand(0);
  Inst->eraseFromParent();
  if (auto *AddrI = dyn_cast<SILInstruction>(Addr))
    recursivelyDeleteTriviallyDeadInstructions(AddrI);
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
  unsigned FirstElt = computeSubelement(Address, TheMemory);
  assert(FirstElt != ~0U && "destroy within enum projection is not valid");
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
  
  DEBUG(llvm::dbgs() << "  *** Promoting destroy_addr: " << *DAI << "\n");
  DEBUG(llvm::dbgs() << "      To value: " << *NewVal.getDef() << "\n");
  
  SILBuilderWithScope<1>(DAI).emitReleaseValueOperation(DAI->getLoc(), NewVal);
  DAI->eraseFromParent();
  return true;
}



/// Explode a copy_addr instruction of a loadable type into lower level
/// operations like loads, stores, retains, releases, retain_value, etc.
void AllocOptimize::explodeCopyAddr(CopyAddrInst *CAI) {
  DEBUG(llvm::dbgs() << "  -- Exploding copy_addr: " << *CAI << "\n");
  
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
    NewInst->setDebugScope(CAI->getDebugScope());
    switch (NewInst->getKind()) {
    default:
      NewInst->dump();
      llvm_unreachable("Unknown instruction generated by copy_addr lowering");
      
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
      
    case ValueKind::RetainValueInst:
    case ValueKind::StrongRetainInst:
    case ValueKind::StrongReleaseInst:
    case ValueKind::UnownedRetainInst:
    case ValueKind::UnownedReleaseInst:
    case ValueKind::ReleaseValueInst:   // Destroy overwritten value
      // These are ignored.
      continue;
    }
  }
}

/// tryToRemoveDeadAllocation - If the allocation is an autogenerated allocation
/// that is only stored to (after load promotion) then remove it completely.
bool AllocOptimize::tryToRemoveDeadAllocation() {
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
    return false;

  // Check the uses list to see if there are any non-store uses left over after
  // load promotion and other things DI does.
  for (auto &U : Uses) {
    // Ignore removed instructions.
    if (U.Inst == nullptr) continue;

    switch (U.Kind) {
    case DIUseKind::SelfInit:
    case DIUseKind::SuperInit:
      llvm_unreachable("Can't happen on allocations");
    case DIUseKind::Assign:
    case DIUseKind::PartialStore:
    case DIUseKind::InitOrAssign:
      break;    // These don't prevent removal.
    case DIUseKind::Initialization:
      if (!isa<ApplyInst>(U.Inst) &&
          // A copy_addr that is not a take affects the retain count
          // of the source.
          (!isa<CopyAddrInst>(U.Inst) ||
           cast<CopyAddrInst>(U.Inst)->isTakeOfSrc()))
        break;
      // FALL THROUGH.
     SWIFT_FALLTHROUGH;
    case DIUseKind::Load:
    case DIUseKind::IndirectIn:
    case DIUseKind::InOutUse:
    case DIUseKind::Escape:
      DEBUG(llvm::dbgs() << "*** Failed to remove autogenerated alloc: "
            "kept alive by: " << *U.Inst);
      return false;   // These do prevent removal.
    }
  }

  // If the memory object has non-trivial type, then removing the deallocation
  // will drop any releases.  Check that there is nothing preventing removal.
  if (!MemoryType.isTrivial(Module)) {
    for (auto *R : Releases) {
      if (R == nullptr || isa<DeallocStackInst>(R) || isa<DeallocBoxInst>(R))
        continue;

      DEBUG(llvm::dbgs() << "*** Failed to remove autogenerated alloc: "
            "kept alive by release: " << *R);
      return false;
    }
  }

  DEBUG(llvm::dbgs() << "*** Removing autogenerated alloc_stack: "<<*TheMemory);

  // If it is safe to remove, do it.  Recursively remove all instructions
  // hanging off the allocation instruction, then return success.  Let the
  // caller remove the allocation itself to avoid iterator invalidation.
  eraseUsesOfInstruction(TheMemory);

  return true;
}

/// doIt - returns true on error.
bool AllocOptimize::doIt() {
  bool Changed = false;

  // Don't  try to optimize incomplete aggregates.
  if (MemoryType.aggregateHasUnreferenceableStorage())
    return false;

  // If we've successfully checked all of the definitive initialization
  // requirements, try to promote loads.  This can explode copy_addrs, so the
  // use list may change size.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &Use = Uses[i];
    // Ignore entries for instructions that got expanded along the way.
    if (Use.Inst && Use.Kind == DIUseKind::Load)
      if (promoteLoad(Use.Inst)) {
        Uses[i].Inst = nullptr;  // remove entry if load got deleted.
        Changed = true;
      }
  }
  
  // destroy_addr(p) is strong_release(load(p)), try to promote it too.
  for (unsigned i = 0; i != Releases.size(); ++i) {
    if (auto *DAI = dyn_cast_or_null<DestroyAddrInst>(Releases[i]))
      if (promoteDestroyAddr(DAI)) {
        // remove entry if destroy_addr got deleted.
        Releases[i] = nullptr;
        Changed = true;
      }
  }
  
  // If this is an allocation, try to remove it completely.
  if (!isa<MarkUninitializedInst>(TheMemory))
    Changed |= tryToRemoveDeadAllocation();

  return Changed;
 }


static bool optimizeMemoryAllocations(SILFunction &Fn) {
  bool Changed = false;
  for (auto &BB : Fn) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I;
      if (!isa<AllocBoxInst>(Inst) && !isa<AllocStackInst>(Inst)) {
        ++I;
        continue;
      }

      DEBUG(llvm::dbgs() << "*** DI Optimize looking at: " << *Inst << "\n");
      DIMemoryObjectInfo MemInfo(Inst);

      // Set up the datastructure used to collect the uses of the allocation.
      SmallVector<DIMemoryUse, 16> Uses;
      SmallVector<SILInstruction*, 4> Releases;
      
      // Walk the use list of the pointer, collecting them.
      collectDIElementUsesFrom(MemInfo, Uses, Releases, true);
      
      Changed |= AllocOptimize(Inst, Uses, Releases).doIt();
      
      // Carefully move iterator to avoid invalidation problems.
      ++I;
      if (Inst->use_empty()) {
        Inst->eraseFromParent();
        ++NumAllocRemoved;
        Changed = true;
      }
    }
  }
  return Changed;
}

namespace {
class PredictableMemoryOptimizations : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    if (optimizeMemoryAllocations(*getFunction()))
      invalidateAnalysis(SILAnalysis::PreserveKind::Nothing);
  }

  StringRef getName() override { return "Predictable Memory Opts"; }
};
} // end anonymous namespace


SILTransform *swift::createPredictableMemoryOptimizations() {
  return new PredictableMemoryOptimizations();
}
