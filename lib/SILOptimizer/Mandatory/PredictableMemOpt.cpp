//===--- PredictableMemOpt.cpp - Perform predictable memory optzns --------===//
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

#define DEBUG_TYPE "predictable-memopt"

#include "PMOMemoryUseCollector.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/BranchPropagatedUser.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumLoadPromoted, "Number of loads promoted");
STATISTIC(NumDestroyAddrPromoted, "Number of destroy_addrs promoted");
STATISTIC(NumAllocRemoved, "Number of allocations completely removed");

//===----------------------------------------------------------------------===//
//                            Subelement Analysis
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
static SILValue getAccessPathRoot(SILValue pointer) {
  while (true) {
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(pointer)) {
      pointer = TEAI->getOperand();
      continue;
    }

    if (auto *SEAI = dyn_cast<StructElementAddrInst>(pointer)) {
      pointer = SEAI->getOperand();
      continue;
    }

    if (auto *BAI = dyn_cast<BeginAccessInst>(pointer)) {
      pointer = BAI->getSource();
      continue;
    }

    return pointer;
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
/// If this pointer is to within an existential projection, it returns ~0U.
static unsigned computeSubelement(SILValue Pointer,
                                  SingleValueInstruction *RootInst) {
  unsigned SubElementNumber = 0;
  SILModule &M = RootInst->getModule();
  
  while (1) {
    // If we got to the root, we're done.
    if (RootInst == Pointer)
      return SubElementNumber;

    if (auto *PBI = dyn_cast<ProjectBoxInst>(Pointer)) {
      Pointer = PBI->getOperand();
      continue;
    }

    if (auto *BAI = dyn_cast<BeginAccessInst>(Pointer)) {
      Pointer = BAI->getSource();
      continue;
    }

    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(Pointer)) {
      SILType TT = TEAI->getOperand()->getType();
      
      // Keep track of what subelement is being referenced.
      for (unsigned i = 0, e = TEAI->getFieldNo(); i != e; ++i) {
        SubElementNumber += getNumSubElements(TT.getTupleElementType(i), M);
      }
      Pointer = TEAI->getOperand();
      continue;
    }

    if (auto *SEAI = dyn_cast<StructElementAddrInst>(Pointer)) {
      SILType ST = SEAI->getOperand()->getType();
      
      // Keep track of what subelement is being referenced.
      StructDecl *SD = SEAI->getStructDecl();
      for (auto *D : SD->getStoredProperties()) {
        if (D == SEAI->getField()) break;
        SubElementNumber += getNumSubElements(ST.getFieldType(D, M), M);
      }
      
      Pointer = SEAI->getOperand();
      continue;
    }

    
    assert(isa<InitExistentialAddrInst>(Pointer) &&
           "Unknown access path instruction");
    // Cannot promote loads and stores from within an existential projection.
    return ~0U;
  }
}

//===----------------------------------------------------------------------===//
//                              Available Value
//===----------------------------------------------------------------------===//

namespace {

class AvailableValueAggregator;

struct AvailableValue {
  friend class AvailableValueAggregator;

  SILValue Value;
  unsigned SubElementNumber;

  /// If this gets too expensive in terms of copying, we can use an arena and a
  /// FrozenPtrSet like we do in ARC.
  SmallSetVector<StoreInst *, 1> InsertionPoints;

  /// Just for updating.
  SmallVectorImpl<PMOMemoryUse> *Uses;

public:
  AvailableValue() = default;

  /// Main initializer for available values.
  ///
  /// *NOTE* We assume that all available values start with a singular insertion
  /// point and insertion points are added by merging.
  AvailableValue(SILValue Value, unsigned SubElementNumber,
                 StoreInst *InsertPoint)
      : Value(Value), SubElementNumber(SubElementNumber), InsertionPoints() {
    InsertionPoints.insert(InsertPoint);
  }

  /// Deleted copy constructor. This is a move only type.
  AvailableValue(const AvailableValue &) = delete;

  /// Deleted copy operator. This is a move only type.
  AvailableValue &operator=(const AvailableValue &) = delete;

  /// Move constructor.
  AvailableValue(AvailableValue &&Other)
      : Value(nullptr), SubElementNumber(~0), InsertionPoints() {
    std::swap(Value, Other.Value);
    std::swap(SubElementNumber, Other.SubElementNumber);
    std::swap(InsertionPoints, Other.InsertionPoints);
  }

  /// Move operator.
  AvailableValue &operator=(AvailableValue &&Other) {
    std::swap(Value, Other.Value);
    std::swap(SubElementNumber, Other.SubElementNumber);
    std::swap(InsertionPoints, Other.InsertionPoints);
    return *this;
  }

  operator bool() const { return bool(Value); }

  bool operator==(const AvailableValue &Other) const {
    return Value == Other.Value && SubElementNumber == Other.SubElementNumber;
  }

  bool operator!=(const AvailableValue &Other) const {
    return !(*this == Other);
  }

  SILValue getValue() const { return Value; }
  SILType getType() const { return Value->getType(); }
  unsigned getSubElementNumber() const { return SubElementNumber; }
  ArrayRef<StoreInst *> getInsertionPoints() const {
    return InsertionPoints.getArrayRef();
  }

  void mergeInsertionPoints(const AvailableValue &Other) & {
    assert(Value == Other.Value && SubElementNumber == Other.SubElementNumber);
    InsertionPoints.set_union(Other.InsertionPoints);
  }

  void addInsertionPoint(StoreInst *I) & { InsertionPoints.insert(I); }

  AvailableValue emitStructExtract(SILBuilder &B, SILLocation Loc, VarDecl *D,
                                   unsigned SubElementNumber) const {
    SILValue NewValue = B.emitStructExtract(Loc, Value, D);
    return {NewValue, SubElementNumber, InsertionPoints};
  }

  AvailableValue emitTupleExtract(SILBuilder &B, SILLocation Loc,
                                  unsigned EltNo,
                                  unsigned SubElementNumber) const {
    SILValue NewValue = B.emitTupleExtract(Loc, Value, EltNo);
    return {NewValue, SubElementNumber, InsertionPoints};
  }

  AvailableValue emitBeginBorrow(SILBuilder &b, SILLocation loc) const {
    // If we do not have ownership or already are guaranteed, just return a copy
    // of our state.
    if (!b.hasOwnership() || Value.getOwnershipKind().isCompatibleWith(
                                 ValueOwnershipKind::Guaranteed)) {
      return {Value, SubElementNumber, InsertionPoints};
    }

    // Otherwise, return newValue.
    return {b.createBeginBorrow(loc, Value), SubElementNumber, InsertionPoints};
  }

  void dump() const LLVM_ATTRIBUTE_USED;
  void print(llvm::raw_ostream &os) const;

private:
  /// Private constructor.
  AvailableValue(SILValue Value, unsigned SubElementNumber,
                 const decltype(InsertionPoints) &InsertPoints)
      : Value(Value), SubElementNumber(SubElementNumber),
        InsertionPoints(InsertPoints) {}
};

} // end anonymous namespace

void AvailableValue::dump() const { print(llvm::dbgs()); }

void AvailableValue::print(llvm::raw_ostream &os) const {
  os << "Available Value Dump. Value: ";
  if (getValue()) {
    os << getValue();
  } else {
    os << "NoValue;\n";
  }
  os << "SubElementNumber: " << getSubElementNumber() << "\n";
  os << "Insertion Points:\n";
  for (auto *I : getInsertionPoints()) {
    os << *I;
  }
}

namespace llvm {

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, const AvailableValue &V) {
  V.print(os);
  return os;
}

} // end llvm namespace

//===----------------------------------------------------------------------===//
//                           Subelement Extraction
//===----------------------------------------------------------------------===//

/// Given an aggregate value and an access path, non-destructively extract the
/// value indicated by the path.
static SILValue nonDestructivelyExtractSubElement(const AvailableValue &Val,
                                                  SILBuilder &B,
                                                  SILLocation Loc) {
  SILType ValTy = Val.getType();
  unsigned SubElementNumber = Val.SubElementNumber;

  // Extract tuple elements.
  if (auto TT = ValTy.getAs<TupleType>()) {
    for (unsigned EltNo : indices(TT.getElementTypes())) {
      // Keep track of what subelement is being referenced.
      SILType EltTy = ValTy.getTupleElementType(EltNo);
      unsigned NumSubElt = getNumSubElements(EltTy, B.getModule());
      if (SubElementNumber < NumSubElt) {
        auto BorrowedVal = Val.emitBeginBorrow(B, Loc);
        auto NewVal =
            BorrowedVal.emitTupleExtract(B, Loc, EltNo, SubElementNumber);
        SILValue result = nonDestructivelyExtractSubElement(NewVal, B, Loc);
        // If our original value wasn't guaranteed and we did actually perform a
        // borrow as a result, insert the end_borrow.
        if (BorrowedVal.getValue() != Val.getValue())
          B.createEndBorrow(Loc, BorrowedVal.getValue());
        return result;
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
        auto BorrowedVal = Val.emitBeginBorrow(B, Loc);
        auto NewVal =
            BorrowedVal.emitStructExtract(B, Loc, D, SubElementNumber);
        SILValue result = nonDestructivelyExtractSubElement(NewVal, B, Loc);
        // If our original value wasn't guaranteed and we did actually perform a
        // borrow as a result, insert the end_borrow.
        if (BorrowedVal.getValue() != Val.getValue())
          B.createEndBorrow(Loc, BorrowedVal.getValue());
        return result;
      }
      
      SubElementNumber -= NumSubElt;
      
    }
    llvm_unreachable("Didn't find field");
  }

  // Otherwise, we're down to a scalar. If we have ownership enabled,
  // we return a copy. Otherwise, there we can ignore ownership
  // issues. This is ok since in [ossa] we are going to eliminate a
  // load [copy] or a load [trivial], while in non-[ossa] SIL we will
  // be replacing unqualified loads.
  assert(SubElementNumber == 0 && "Miscalculation indexing subelements");
  if (!B.hasOwnership())
    return Val.getValue();
  return B.emitCopyValueOperation(Loc, Val.getValue());
}

//===----------------------------------------------------------------------===//
//                        Available Value Aggregation
//===----------------------------------------------------------------------===//

static bool anyMissing(unsigned StartSubElt, unsigned NumSubElts,
                       ArrayRef<AvailableValue> &Values) {
  while (NumSubElts) {
    if (!Values[StartSubElt])
      return true;
    ++StartSubElt;
    --NumSubElts;
  }
  return false;
}

namespace {

/// A class that aggregates available values, loading them if they are not
/// available.
class AvailableValueAggregator {
  SILModule &M;
  SILBuilderWithScope B;
  SILLocation Loc;
  MutableArrayRef<AvailableValue> AvailableValueList;
  SmallVectorImpl<PMOMemoryUse> &Uses;
  DeadEndBlocks &deadEndBlocks;
  bool isTake;

  /// Keep track of all instructions that we have added. Once we are done
  /// promoting a value, we need to make sure that if we need to balance any
  /// copies (to avoid leaks), we do so. This is not used if we are performing a
  /// take.
  SmallVector<SILInstruction *, 16> insertedInsts;

public:
  AvailableValueAggregator(SILInstruction *Inst,
                           MutableArrayRef<AvailableValue> AvailableValueList,
                           SmallVectorImpl<PMOMemoryUse> &Uses,
                           DeadEndBlocks &deadEndBlocks, bool isTake)
      : M(Inst->getModule()), B(Inst), Loc(Inst->getLoc()),
        AvailableValueList(AvailableValueList), Uses(Uses),
        deadEndBlocks(deadEndBlocks), isTake(isTake) {}

  // This is intended to be passed by reference only once constructed.
  AvailableValueAggregator(const AvailableValueAggregator &) = delete;
  AvailableValueAggregator(AvailableValueAggregator &&) = delete;
  AvailableValueAggregator &
  operator=(const AvailableValueAggregator &) = delete;
  AvailableValueAggregator &operator=(AvailableValueAggregator &&) = delete;

  SILValue aggregateValues(SILType LoadTy, SILValue Address, unsigned FirstElt,
                           bool isTopLevel = true);
  bool canTake(SILType loadTy, unsigned firstElt) const;

  /// If as a result of us copying values, we may have unconsumed destroys, find
  /// the appropriate location and place the values there. Only used when
  /// ownership is enabled.
  LoadInst *addMissingDestroysForCopiedValues(LoadInst *li, SILValue newVal);

  void print(llvm::raw_ostream &os) const;
  void dump() const LLVM_ATTRIBUTE_USED;
private:
  SILValue aggregateFullyAvailableValue(SILType loadTy, unsigned firstElt);
  SILValue aggregateTupleSubElts(TupleType *tt, SILType loadTy,
                                 SILValue address, unsigned firstElt);
  SILValue aggregateStructSubElts(StructDecl *sd, SILType loadTy,
                                  SILValue address, unsigned firstElt);
  SILValue handlePrimitiveValue(SILType loadTy, SILValue address,
                                unsigned firstElt);
  bool isFullyAvailable(SILType loadTy, unsigned firstElt) const;
};

} // end anonymous namespace

void AvailableValueAggregator::dump() const { print(llvm::dbgs()); }

void AvailableValueAggregator::print(llvm::raw_ostream &os) const {
  os << "Available Value List, N = " << AvailableValueList.size()
     << ". Elts:\n";
  for (auto &V : AvailableValueList) {
    os << V;
  }
}

bool AvailableValueAggregator::isFullyAvailable(SILType loadTy,
                                                unsigned firstElt) const {
  if (firstElt >= AvailableValueList.size()) { // #Elements may be zero.
    return false;
  }

  auto &firstVal = AvailableValueList[firstElt];

  // Make sure that the first element is available and is the correct type.
  if (!firstVal || firstVal.getType() != loadTy)
    return false;

  return llvm::all_of(range(getNumSubElements(loadTy, M)),
                      [&](unsigned index) -> bool {
                        auto &val = AvailableValueList[firstElt + index];
                        return val.getValue() == firstVal.getValue() &&
                               val.getSubElementNumber() == index;
                      });
}

// We can only take if we never have to split a larger value to promote this
// address.
bool AvailableValueAggregator::canTake(SILType loadTy,
                                       unsigned firstElt) const {
  // If we do not have ownership, we can always take since we do not need to
  // keep any ownership invariants up to date. In the future, we should be able
  // to chop up larger values before they are being stored.
  if (!B.hasOwnership())
    return true;

  // If we are trivially fully available, just return true.
  if (isFullyAvailable(loadTy, firstElt))
    return true;

  // Otherwise see if we are an aggregate with fully available leaf types.
  if (TupleType *tt = loadTy.getAs<TupleType>()) {
    return llvm::all_of(indices(tt->getElements()), [&](unsigned eltNo) {
      SILType eltTy = loadTy.getTupleElementType(eltNo);
      unsigned numSubElt = getNumSubElements(eltTy, M);
      bool success = canTake(eltTy, firstElt);
      firstElt += numSubElt;
      return success;
    });
  }

  if (auto *sd = getFullyReferenceableStruct(loadTy)) {
    return llvm::all_of(sd->getStoredProperties(), [&](VarDecl *decl) -> bool {
      SILType eltTy = loadTy.getFieldType(decl, M);
      unsigned numSubElt = getNumSubElements(eltTy, M);
      bool success = canTake(eltTy, firstElt);
      firstElt += numSubElt;
      return success;
    });
  }

  // Otherwise, fail. The value is not fully available at its leafs. We can not
  // perform a take.
  return false;
}

/// Given a bunch of primitive subelement values, build out the right aggregate
/// type (LoadTy) by emitting tuple and struct instructions as necessary.
SILValue AvailableValueAggregator::aggregateValues(SILType LoadTy,
                                                   SILValue Address,
                                                   unsigned FirstElt,
                                                   bool isTopLevel) {
  // If we are performing a take, make sure that we have available values for
  // /all/ of our values. Otherwise, bail.
  if (isTopLevel && isTake && !canTake(LoadTy, FirstElt)) {
    return SILValue();
  }

  // Check to see if the requested value is fully available, as an aggregate.
  // This is a super-common case for single-element structs, but is also a
  // general answer for arbitrary structs and tuples as well.
  if (SILValue Result = aggregateFullyAvailableValue(LoadTy, FirstElt))
    return Result;

  // If we have a tuple type, then aggregate the tuple's elements into a full
  // tuple value.
  if (TupleType *TT = LoadTy.getAs<TupleType>())
    return aggregateTupleSubElts(TT, LoadTy, Address, FirstElt);

  // If we have a struct type, then aggregate the struct's elements into a full
  // struct value.
  if (auto *SD = getFullyReferenceableStruct(LoadTy))
    return aggregateStructSubElts(SD, LoadTy, Address, FirstElt);

  // Otherwise, we have a non-aggregate primitive. Load or extract the value.
  return handlePrimitiveValue(LoadTy, Address, FirstElt);
}

// See if we have this value is fully available. In such a case, return it as an
// aggregate. This is a super-common case for single-element structs, but is
// also a general answer for arbitrary structs and tuples as well.
SILValue
AvailableValueAggregator::aggregateFullyAvailableValue(SILType loadTy,
                                                       unsigned firstElt) {
  // Check if our underlying type is fully available. If it isn't, bail.
  if (!isFullyAvailable(loadTy, firstElt))
    return SILValue();

  // Ok, grab out first value. (note: any actually will do).
  auto &firstVal = AvailableValueList[firstElt];

  // Ok, we know that all of our available values are all parts of the same
  // value. Without ownership, we can just return the underlying first value.
  if (!B.hasOwnership())
    return firstVal.getValue();

  // Otherwise, we need to put in a copy. This is b/c we only propagate along +1
  // values and we are eliminating a load [copy].
  ArrayRef<StoreInst *> insertPts = firstVal.getInsertionPoints();
  if (insertPts.size() == 1) {
    // Use the scope and location of the store at the insertion point.
    SILBuilderWithScope builder(insertPts[0], &insertedInsts);
    SILLocation loc = insertPts[0]->getLoc();
    // If we have a take, just return the value.
    if (isTake)
      return firstVal.getValue();
    // Otherwise, return a copy of the value.
    return builder.emitCopyValueOperation(loc, firstVal.getValue());
  }

  // If we have multiple insertion points, put copies at each point and use the
  // SSA updater to get a value. The reason why this is safe is that we can only
  // have multiple insertion points if we are storing exactly the same value
  // implying that we can just copy firstVal at each insertion point.
  SILSSAUpdater updater(B.getModule());
  updater.Initialize(loadTy);

  Optional<SILValue> singularValue;
  for (auto *insertPt : insertPts) {
    // Use the scope and location of the store at the insertion point.
    SILBuilderWithScope builder(insertPt, &insertedInsts);
    SILLocation loc = insertPt->getLoc();
    SILValue eltVal = firstVal.getValue();

    // If we are not taking, copy the element value.
    if (!isTake) {
      eltVal = builder.emitCopyValueOperation(loc, eltVal);
    }

    if (!singularValue.hasValue()) {
      singularValue = eltVal;
    } else if (*singularValue != eltVal) {
      singularValue = SILValue();
    }

    // And then put the value into the SSA updater.
    updater.AddAvailableValue(insertPt->getParent(), eltVal);
  }

  // If we only are tracking a singular value, we do not need to construct
  // SSA. Just return that value.
  if (auto val = singularValue.getValueOr(SILValue()))
    return val;

  // Finally, grab the value from the SSA updater.
  SILValue result = updater.GetValueInMiddleOfBlock(B.getInsertionBB());
  assert(result.getOwnershipKind().isCompatibleWith(ValueOwnershipKind::Owned));
  return result;
}

SILValue AvailableValueAggregator::aggregateTupleSubElts(TupleType *TT,
                                                         SILType LoadTy,
                                                         SILValue Address,
                                                         unsigned FirstElt) {
  SmallVector<SILValue, 4> ResultElts;

  for (unsigned EltNo : indices(TT->getElements())) {
    SILType EltTy = LoadTy.getTupleElementType(EltNo);
    unsigned NumSubElt = getNumSubElements(EltTy, M);

    // If we are missing any of the available values in this struct element,
    // compute an address to load from.
    SILValue EltAddr;
    if (anyMissing(FirstElt, NumSubElt, AvailableValueList)) {
      assert(!isTake && "When taking, values should never be missing?!");
      EltAddr =
          B.createTupleElementAddr(Loc, Address, EltNo, EltTy.getAddressType());
    }

    ResultElts.push_back(
        aggregateValues(EltTy, EltAddr, FirstElt, /*isTopLevel*/ false));
    FirstElt += NumSubElt;
  }

  return B.createTuple(Loc, LoadTy, ResultElts);
}

SILValue AvailableValueAggregator::aggregateStructSubElts(StructDecl *sd,
                                                          SILType loadTy,
                                                          SILValue address,
                                                          unsigned firstElt) {
  SmallVector<SILValue, 4> resultElts;

  for (auto *decl : sd->getStoredProperties()) {
    SILType eltTy = loadTy.getFieldType(decl, M);
    unsigned numSubElt = getNumSubElements(eltTy, M);

    // If we are missing any of the available values in this struct element,
    // compute an address to load from.
    SILValue eltAddr;
    if (anyMissing(firstElt, numSubElt, AvailableValueList)) {
      assert(!isTake && "When taking, values should never be missing?!");
      eltAddr =
          B.createStructElementAddr(Loc, address, decl, eltTy.getAddressType());
    }

    resultElts.push_back(
        aggregateValues(eltTy, eltAddr, firstElt, /*isTopLevel*/ false));
    firstElt += numSubElt;
  }

  return B.createStruct(Loc, loadTy, resultElts);
}

// We have looked through all of the aggregate values and finally found a
// "primitive value". If the value is available, use it (extracting if we need
// to), otherwise emit a load of the value with the appropriate qualifier.
SILValue AvailableValueAggregator::handlePrimitiveValue(SILType loadTy,
                                                        SILValue address,
                                                        unsigned firstElt) {
  auto &val = AvailableValueList[firstElt];

  // If the value is not available, load the value and update our use list.
  if (!val) {
    assert(!isTake && "Should only take fully available values?!");
    LoadInst *load = ([&]() {
      if (B.hasOwnership()) {
        return B.createTrivialLoadOr(Loc, address,
                                     LoadOwnershipQualifier::Copy);
      }
      return B.createLoad(Loc, address, LoadOwnershipQualifier::Unqualified);
    }());
    Uses.emplace_back(load, PMOUseKind::Load);
    return load;
  }

  // If we have 1 insertion point, just extract the value and return.
  //
  // This saves us from having to spend compile time in the SSA updater in this
  // case.
  ArrayRef<StoreInst *> insertPts = val.getInsertionPoints();
  if (insertPts.size() == 1) {
    // Use the scope and location of the store at the insertion point.
    SILBuilderWithScope builder(insertPts[0], &insertedInsts);
    SILLocation loc = insertPts[0]->getLoc();
    SILValue eltVal = nonDestructivelyExtractSubElement(val, builder, loc);
    assert(
        !builder.hasOwnership() ||
        eltVal.getOwnershipKind().isCompatibleWith(ValueOwnershipKind::Owned));
    assert(eltVal->getType() == loadTy && "Subelement types mismatch");
    return eltVal;
  }

  // If we have an available value, then we want to extract the subelement from
  // the borrowed aggregate before each insertion point.
  SILSSAUpdater updater(B.getModule());
  updater.Initialize(loadTy);

  Optional<SILValue> singularValue;
  for (auto *i : insertPts) {
    // Use the scope and location of the store at the insertion point.
    SILBuilderWithScope builder(i, &insertedInsts);
    SILLocation loc = i->getLoc();
    SILValue eltVal = nonDestructivelyExtractSubElement(val, builder, loc);
    assert(
        !builder.hasOwnership() ||
        eltVal.getOwnershipKind().isCompatibleWith(ValueOwnershipKind::Owned));

    if (!singularValue.hasValue()) {
      singularValue = eltVal;
    } else if (*singularValue != eltVal) {
      singularValue = SILValue();
    }

    updater.AddAvailableValue(i->getParent(), eltVal);
  }

  // If we only are tracking a singular value, we do not need to construct
  // SSA. Just return that value.
  if (auto val = singularValue.getValueOr(SILValue()))
    return val;

  // Finally, grab the value from the SSA updater.
  SILValue eltVal = updater.GetValueInMiddleOfBlock(B.getInsertionBB());
  assert(!B.hasOwnership() ||
         eltVal.getOwnershipKind().isCompatibleWith(ValueOwnershipKind::Owned));
  assert(eltVal->getType() == loadTy && "Subelement types mismatch");
  return eltVal;
}

LoadInst *
AvailableValueAggregator::addMissingDestroysForCopiedValues(LoadInst *li,
                                                            SILValue newVal) {
  // If ownership is not enabled... bail. We do not need to do this since we do
  // not need to insert an extra copy unless we have ownership since without
  // ownership stores do not consume.
  if (!B.hasOwnership())
    return li;

  SmallPtrSet<SILBasicBlock *, 8> visitedBlocks;
  SmallVector<SILBasicBlock *, 8> leakingBlocks;
  bool foundLoop = false;
  auto loc = RegularLocation::getAutoGeneratedLocation();
  while (!insertedInsts.empty()) {
    auto *cvi = dyn_cast<CopyValueInst>(insertedInsts.pop_back_val());
    if (!cvi)
      continue;

    // Clear our state.
    visitedBlocks.clear();
    leakingBlocks.clear();
    // The linear lifetime checker doesn't care if the passed in load is
    // actually a user of our copy_value. What we care about is that the load is
    // guaranteed to be in the block where we have reformed the tuple in a
    // consuming manner. This means if we add it as the consuming use of the
    // copy, we can find the leaking places if any exist.
    //
    // Then perform the linear lifetime check. If we succeed, continue. We have
    // no further work to do.
    auto errorKind = ownership::ErrorBehaviorKind::ReturnFalse;
    auto error = valueHasLinearLifetime(
        cvi, {li}, {}, visitedBlocks, deadEndBlocks, errorKind, &leakingBlocks);
    if (!error.getFoundError())
      continue;

    // Ok, we found some leaking blocks. Since we are using the linear lifetime
    // checker with memory, we do not have any guarantees that the store is out
    // side of a loop and a load is in a loop. In such a case, we want to
    // replace the load with a copy_value.
    foundLoop |= error.getFoundOverConsume();

    // Ok, we found some leaking blocks. Insert destroys at the
    // beginning of these blocks for our copy_value.
    for (auto *bb : leakingBlocks) {
      SILBuilderWithScope b(bb->begin());
      b.emitDestroyValueOperation(loc, cvi);
    }
  }

  // If we didn't find a loop, we are done, just return li to get RAUWed.
  if (!foundLoop)
    return li;

  // If we found a loop, then we know that our leaking blocks are the exiting
  // blocks of the loop. Thus we need to change the load inst to a copy_value
  // instead of deleting it.
  newVal = SILBuilderWithScope(li).emitCopyValueOperation(loc, newVal);
  li->replaceAllUsesWith(newVal);
  SILValue addr = li->getOperand();
  li->eraseFromParent();
  if (auto *addrI = addr->getDefiningInstruction())
    recursivelyDeleteTriviallyDeadInstructions(addrI);
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                          Available Value Dataflow
//===----------------------------------------------------------------------===//

namespace {

/// Given a piece of memory, the memory's uses, and destroys perform a single
/// round of optimistic dataflow switching to intersection when a back edge is
/// encountered.
class AvailableValueDataflowContext {
  /// The base memory we are performing dataflow upon.
  AllocationInst *TheMemory;

  /// The number of sub elements of our memory.
  unsigned NumMemorySubElements;

  /// The set of uses that we are tracking. This is only here so we can update
  /// when exploding copy_addr. It would be great if we did not have to store
  /// this.
  SmallVectorImpl<PMOMemoryUse> &Uses;

  /// The set of blocks with local definitions.
  ///
  /// We use this to determine if we should visit a block or look at a block's
  /// predecessors during dataflow.
  llvm::SmallPtrSet<SILBasicBlock *, 32> HasLocalDefinition;

  /// This is a map of uses that are not loads (i.e., they are Stores,
  /// InOutUses, and Escapes), to their entry in Uses.
  llvm::SmallDenseMap<SILInstruction *, unsigned, 16> NonLoadUses;

  /// Does this value escape anywhere in the function. We use this very
  /// conservatively.
  bool HasAnyEscape = false;

public:
  AvailableValueDataflowContext(AllocationInst *TheMemory,
                                unsigned NumMemorySubElements,
                                SmallVectorImpl<PMOMemoryUse> &Uses);

  /// Try to compute available values for "TheMemory" at the instruction \p
  /// StartingFrom. We only compute the values for set bits in \p
  /// RequiredElts. We return the vailable values in \p Result. If any available
  /// values were found, return true. Otherwise, return false.
  bool computeAvailableValues(SILInstruction *StartingFrom,
                              unsigned FirstEltOffset,
                              unsigned NumLoadSubElements,
                              SmallBitVector &RequiredElts,
                              SmallVectorImpl<AvailableValue> &Result);

  /// Return true if the box has escaped at the specified instruction.  We are
  /// not
  /// allowed to do load promotion in an escape region.
  bool hasEscapedAt(SILInstruction *I);

  /// Explode a copy_addr, updating the Uses at the same time.
  void explodeCopyAddr(CopyAddrInst *CAI);

private:
  SILModule &getModule() const { return TheMemory->getModule(); }

  void updateAvailableValues(SILInstruction *Inst,
                             SmallBitVector &RequiredElts,
                             SmallVectorImpl<AvailableValue> &Result,
                             SmallBitVector &ConflictingValues);
  void computeAvailableValuesFrom(
      SILBasicBlock::iterator StartingFrom, SILBasicBlock *BB,
      SmallBitVector &RequiredElts,
      SmallVectorImpl<AvailableValue> &Result,
      llvm::SmallDenseMap<SILBasicBlock *, SmallBitVector, 32>
          &VisitedBlocks,
      SmallBitVector &ConflictingValues);
};

} // end anonymous namespace

AvailableValueDataflowContext::AvailableValueDataflowContext(
    AllocationInst *InputTheMemory, unsigned NumMemorySubElements,
    SmallVectorImpl<PMOMemoryUse> &InputUses)
    : TheMemory(InputTheMemory), NumMemorySubElements(NumMemorySubElements),
      Uses(InputUses) {
  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (unsigned ui : indices(Uses)) {
    auto &Use = Uses[ui];
    assert(Use.Inst && "No instruction identified?");

    // Keep track of all the uses that aren't loads.
    if (Use.Kind == PMOUseKind::Load)
      continue;

    NonLoadUses[Use.Inst] = ui;
    HasLocalDefinition.insert(Use.Inst->getParent());

    if (Use.Kind == PMOUseKind::Escape) {
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

void AvailableValueDataflowContext::updateAvailableValues(
    SILInstruction *Inst, SmallBitVector &RequiredElts,
    SmallVectorImpl<AvailableValue> &Result,
    SmallBitVector &ConflictingValues) {
  // Handle store.
  if (auto *SI = dyn_cast<StoreInst>(Inst)) {
    unsigned StartSubElt = computeSubelement(SI->getDest(), TheMemory);
    assert(StartSubElt != ~0U && "Store within enum projection not handled");
    SILType ValTy = SI->getSrc()->getType();

    for (unsigned i : range(getNumSubElements(ValTy, getModule()))) {
      // If this element is not required, don't fill it in.
      if (!RequiredElts[StartSubElt+i]) continue;

      // This element is now provided.
      RequiredElts[StartSubElt + i] = false;

      // If there is no result computed for this subelement, record it.  If
      // there already is a result, check it for conflict.  If there is no
      // conflict, then we're ok.
      auto &Entry = Result[StartSubElt+i];
      if (!Entry) {
        Entry = {SI->getSrc(), i, SI};
        continue;
      }

      // TODO: This is /really/, /really/, conservative. This basically means
      // that if we do not have an identical store, we will not promote.
      if (Entry.getValue() != SI->getSrc() ||
          Entry.getSubElementNumber() != i) {
        ConflictingValues[StartSubElt + i] = true;
        continue;
      }

      Entry.addInsertionPoint(SI);
    }

    return;
  }
  
  // If we get here with a copy_addr, it must be storing into the element. Check
  // to see if any loaded subelements are being used, and if so, explode the
  // copy_addr to its individual pieces.
  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    unsigned StartSubElt = computeSubelement(CAI->getDest(), TheMemory);
    assert(StartSubElt != ~0U && "Store within enum projection not handled");
    SILType ValTy = CAI->getDest()->getType();

    bool AnyRequired = false;
    for (unsigned i : range(getNumSubElements(ValTy, getModule()))) {
      // If this element is not required, don't fill it in.
      AnyRequired = RequiredElts[StartSubElt+i];
      if (AnyRequired) break;
    }
    
    // If this is a copy addr that doesn't intersect the loaded subelements,
    // just continue with an unmodified load mask.
    if (!AnyRequired)
      return;
    
    // If the copyaddr is of a non-loadable type, we can't promote it.  Just
    // consider it to be a clobber.
    if (CAI->getSrc()->getType().isLoadable(getModule())) {
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
  ConflictingValues = SmallBitVector(Result.size(), true);
  return;
}

bool AvailableValueDataflowContext::computeAvailableValues(
    SILInstruction *StartingFrom, unsigned FirstEltOffset,
    unsigned NumLoadSubElements, SmallBitVector &RequiredElts,
    SmallVectorImpl<AvailableValue> &Result) {
  llvm::SmallDenseMap<SILBasicBlock*, SmallBitVector, 32> VisitedBlocks;
  SmallBitVector ConflictingValues(Result.size());

  computeAvailableValuesFrom(StartingFrom->getIterator(),
                             StartingFrom->getParent(), RequiredElts, Result,
                             VisitedBlocks, ConflictingValues);
  // If there are no values available at this load point, then we fail to
  // promote this load and there is nothing to do.
  SmallBitVector AvailableValueIsPresent(NumMemorySubElements);

  for (unsigned i :
       range(FirstEltOffset, FirstEltOffset + NumLoadSubElements)) {
    AvailableValueIsPresent[i] = Result[i].getValue();
  }

  // If we do not have any values available, bail.
  if (AvailableValueIsPresent.none())
    return false;

  // Otherwise, if we have any conflicting values, explicitly mask them out of
  // the result, so we don't pick one arbitrary available value.
  if (ConflictingValues.none()) {
    return true;
  }

  // At this point, we know that we have /some/ conflicting values and some
  // available values.
  if (AvailableValueIsPresent.reset(ConflictingValues).none())
    return false;

  // Otherwise, mask out the available values and return true. We have at least
  // 1 available value.
  int NextIter = ConflictingValues.find_first();
  while (NextIter != -1) {
    assert(NextIter >= 0 && "Int can not be represented?!");
    unsigned Iter = NextIter;
    Result[Iter] = {};
    NextIter = ConflictingValues.find_next(Iter);
  }

  return true;
}

void AvailableValueDataflowContext::computeAvailableValuesFrom(
    SILBasicBlock::iterator StartingFrom, SILBasicBlock *BB,
    SmallBitVector &RequiredElts, SmallVectorImpl<AvailableValue> &Result,
    llvm::SmallDenseMap<SILBasicBlock *, SmallBitVector, 32>
        &VisitedBlocks,
    SmallBitVector &ConflictingValues) {
  assert(!RequiredElts.none() && "Scanning with a goal of finding nothing?");
  
  // If there is a potential modification in the current block, scan the block
  // to see if the store or escape is before or after the load.  If it is
  // before, check to see if it produces the value we are looking for.
  if (HasLocalDefinition.count(BB)) {
    for (SILBasicBlock::iterator BBI = StartingFrom; BBI != BB->begin();) {
      SILInstruction *TheInst = &*std::prev(BBI);

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
    SmallBitVector Elts = RequiredElts;
    computeAvailableValuesFrom(PredBB->end(), PredBB, Elts, Result,
                               VisitedBlocks, ConflictingValues);
    
    // If we have any conflicting values, don't bother searching for them.
    RequiredElts &= ~ConflictingValues;
    if (RequiredElts.none())
      return;
  }
}

/// Explode a copy_addr instruction of a loadable type into lower level
/// operations like loads, stores, retains, releases, retain_value, etc.
void AvailableValueDataflowContext::explodeCopyAddr(CopyAddrInst *CAI) {
  LLVM_DEBUG(llvm::dbgs() << "  -- Exploding copy_addr: " << *CAI << "\n");

  SILType ValTy = CAI->getDest()->getType().getObjectType();
  auto &TL = getModule().getTypeLowering(ValTy);

  // Keep track of the new instructions emitted.
  SmallVector<SILInstruction *, 4> NewInsts;
  SILBuilder B(CAI, &NewInsts);
  B.setCurrentDebugScope(CAI->getDebugScope());

  // Use type lowering to lower the copyaddr into a load sequence + store
  // sequence appropriate for the type.
  SILValue StoredValue =
      TL.emitLoadOfCopy(B, CAI->getLoc(), CAI->getSrc(), CAI->isTakeOfSrc());

  TL.emitStoreOfCopy(B, CAI->getLoc(), StoredValue, CAI->getDest(),
                     CAI->isInitializationOfDest());

  // Update our internal state for this being gone.
  NonLoadUses.erase(CAI);

  // Remove the copy_addr from Uses.  A single copy_addr can appear multiple
  // times if the source and dest are to elements within a single aggregate, but
  // we only want to pick up the CopyAddrKind from the store.
  PMOMemoryUse LoadUse, StoreUse;
  for (auto &Use : Uses) {
    if (Use.Inst != CAI)
      continue;

    if (Use.Kind == PMOUseKind::Load) {
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
         StoreUse.Kind == Initialization || StoreUse.Kind == InitOrAssign);

  // Now that we've emitted a bunch of instructions, including a load and store
  // but also including other stuff, update the internal state of
  // LifetimeChecker to reflect them.

  // Update the instructions that touch the memory.  NewInst can grow as this
  // iterates, so we can't use a foreach loop.
  for (auto *NewInst : NewInsts) {
    switch (NewInst->getKind()) {
    default:
      NewInst->dump();
      llvm_unreachable("Unknown instruction generated by copy_addr lowering");

    case SILInstructionKind::StoreInst:
      // If it is a store to the memory object (as oppose to a store to
      // something else), track it as an access.
      if (StoreUse.isValid()) {
        StoreUse.Inst = NewInst;
        // If our store use by the copy_addr is an assign, then we know that
        // before we store the new value, we loaded the old value implying that
        // our store is technically initializing memory when it occurs. So
        // change the kind to Initialization.
        if (StoreUse.Kind == Assign)
          StoreUse.Kind = Initialization;
        NonLoadUses[NewInst] = Uses.size();
        Uses.push_back(StoreUse);
      }
      continue;

    case SILInstructionKind::LoadInst:
      // If it is a load from the memory object (as oppose to a load from
      // something else), track it as an access.  We need to explicitly check to
      // see if the load accesses "TheMemory" because it could either be a load
      // for the copy_addr source, or it could be a load corresponding to the
      // "assign" operation on the destination of the copyaddr.
      if (LoadUse.isValid() &&
          getAccessPathRoot(NewInst->getOperand(0)) == TheMemory) {
        LoadUse.Inst = NewInst;
        Uses.push_back(LoadUse);
      }
      continue;

    case SILInstructionKind::RetainValueInst:
    case SILInstructionKind::StrongRetainInst:
    case SILInstructionKind::StrongReleaseInst:
    case SILInstructionKind::ReleaseValueInst: // Destroy overwritten value
      // These are ignored.
      continue;
    }
  }

  // Next, remove the copy_addr itself.
  CAI->eraseFromParent();
}

bool AvailableValueDataflowContext::hasEscapedAt(SILInstruction *I) {
  // Return true if the box has escaped at the specified instruction.  We are
  // not allowed to do load promotion in an escape region.

  // FIXME: This is not an aggressive implementation.  :)

  // TODO: At some point, we should special case closures that just *read* from
  // the escaped value (by looking at the body of the closure).  They should not
  // prevent load promotion, and will allow promoting values like X in regions
  // dominated by "... && X != 0".
  return HasAnyEscape;
}

//===----------------------------------------------------------------------===//
//                          Allocation Optimization
//===----------------------------------------------------------------------===//

static SILType getMemoryType(AllocationInst *memory) {
  // Compute the type of the memory object.
  if (auto *abi = dyn_cast<AllocBoxInst>(memory)) {
    assert(abi->getBoxType()->getLayout()->getFields().size() == 1 &&
           "optimizing multi-field boxes not implemented");
    return abi->getBoxType()->getFieldType(abi->getModule(), 0);
  }

  assert(isa<AllocStackInst>(memory));
  return cast<AllocStackInst>(memory)->getElementType();
}

namespace {

/// This performs load promotion and deletes synthesized allocations if all
/// loads can be removed.
class AllocOptimize {

  SILModule &Module;

  /// This is either an alloc_box or alloc_stack instruction.
  AllocationInst *TheMemory;

  /// This is the SILType of the memory object.
  SILType MemoryType;

  /// The number of primitive subelements across all elements of this memory
  /// value.
  unsigned NumMemorySubElements;

  SmallVectorImpl<PMOMemoryUse> &Uses;
  SmallVectorImpl<SILInstruction *> &Releases;

  DeadEndBlocks &deadEndBlocks;

  /// A structure that we use to compute our available values.
  AvailableValueDataflowContext DataflowContext;

public:
  AllocOptimize(AllocationInst *memory, SmallVectorImpl<PMOMemoryUse> &uses,
                SmallVectorImpl<SILInstruction *> &releases,
                DeadEndBlocks &deadEndBlocks)
      : Module(memory->getModule()), TheMemory(memory),
        MemoryType(getMemoryType(memory)),
        NumMemorySubElements(getNumSubElements(MemoryType, Module)), Uses(uses),
        Releases(releases), deadEndBlocks(deadEndBlocks),
        DataflowContext(TheMemory, NumMemorySubElements, uses) {}

  bool optimizeMemoryAccesses();
  bool tryToRemoveDeadAllocation();

private:
  bool promoteLoad(SILInstruction *Inst);
  void promoteDestroyAddr(DestroyAddrInst *dai,
                          MutableArrayRef<AvailableValue> values);
  bool canPromoteDestroyAddr(DestroyAddrInst *dai,
                             SmallVectorImpl<AvailableValue> &availableValues);
};

} // end anonymous namespace


/// If we are able to optimize \p Inst, return the source address that
/// instruction is loading from. If we can not optimize \p Inst, then just
/// return an empty SILValue.
static SILValue tryFindSrcAddrForLoad(SILInstruction *Inst) {
  // We only handle load [copy], load [trivial], load and copy_addr right
  // now. Notably we do not support load [take] when promoting loads.
  if (auto *LI = dyn_cast<LoadInst>(Inst))
    if (LI->getOwnershipQualifier() != LoadOwnershipQualifier::Take)
      return LI->getOperand();

  // If this is a CopyAddr, verify that the element type is loadable.  If not,
  // we can't explode to a load.
  auto *CAI = dyn_cast<CopyAddrInst>(Inst);
  if (!CAI || !CAI->getSrc()->getType().isLoadable(CAI->getModule()))
    return SILValue();
  return CAI->getSrc();
}

/// At this point, we know that this element satisfies the definitive init
/// requirements, so we can try to promote loads to enable SSA-based dataflow
/// analysis.  We know that accesses to this element only access this element,
/// cross element accesses have been scalarized.
///
/// This returns true if the load has been removed from the program.
bool AllocOptimize::promoteLoad(SILInstruction *Inst) {
  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.

  // First attempt to find a source addr for our "load" instruction. If we fail
  // to find a valid value, just return.
  SILValue SrcAddr = tryFindSrcAddrForLoad(Inst);
  if (!SrcAddr)
    return false;

  // If the box has escaped at this instruction, we can't safely promote the
  // load.
  if (DataflowContext.hasEscapedAt(Inst))
    return false;

  SILType LoadTy = SrcAddr->getType().getObjectType();

  // If this is a load/copy_addr from a struct field that we want to promote,
  // compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned FirstElt = computeSubelement(SrcAddr, TheMemory);

  // If this is a load from within an enum projection, we can't promote it since
  // we don't track subelements in a type that could be changing.
  if (FirstElt == ~0U)
    return false;
  
  unsigned NumLoadSubElements = getNumSubElements(LoadTy, Module);
  
  // Set up the bitvector of elements being demanded by the load.
  SmallBitVector RequiredElts(NumMemorySubElements);
  RequiredElts.set(FirstElt, FirstElt+NumLoadSubElements);

  SmallVector<AvailableValue, 8> AvailableValues;
  AvailableValues.resize(NumMemorySubElements);
  
  // Find out if we have any available values.  If no bits are demanded, we
  // trivially succeed. This can happen when there is a load of an empty struct.
  if (NumLoadSubElements != 0 &&
      !DataflowContext.computeAvailableValues(
          Inst, FirstElt, NumLoadSubElements, RequiredElts, AvailableValues))
    return false;

  // Ok, we have some available values.  If we have a copy_addr, explode it now,
  // exposing the load operation within it.  Subsequent optimization passes will
  // see the load and propagate the available values into it.
  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    DataflowContext.explodeCopyAddr(CAI);

    // This is removing the copy_addr, but explodeCopyAddr takes care of
    // removing the instruction from Uses for us, so we return false.
    return false;
  }
  
  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller loads for any subelements that were
  // not available.
  auto *Load = cast<LoadInst>(Inst);
  AvailableValueAggregator Agg(Load, AvailableValues, Uses, deadEndBlocks,
                               false /*isTake*/);
  SILValue newVal = Agg.aggregateValues(LoadTy, Load->getOperand(), FirstElt);

  LLVM_DEBUG(llvm::dbgs() << "  *** Promoting load: " << *Load << "\n");
  LLVM_DEBUG(llvm::dbgs() << "      To value: " << *newVal << "\n");

  // If we inserted any copies, we created the copies at our stores. We know
  // that in our load block, we will reform the aggregate as appropriate at the
  // load implying that the value /must/ be fully consumed. Thus any leaking
  // blocks that we may have can be found by performing a linear lifetime check
  // over all copies that we found using the load as the "consuming uses" (just
  // for the purposes of identifying the consuming block).
  auto *oldLoad = Agg.addMissingDestroysForCopiedValues(Load, newVal);

  ++NumLoadPromoted;

  // If we are returned the load, eliminate it. Otherwise, it was already
  // handled for us... so return true.
  if (!oldLoad)
    return true;

  oldLoad->replaceAllUsesWith(newVal);
  SILValue addr = oldLoad->getOperand();
  oldLoad->eraseFromParent();
  if (auto *addrI = addr->getDefiningInstruction())
    recursivelyDeleteTriviallyDeadInstructions(addrI);
  return true;
}

/// Return true if we can promote the given destroy.
bool AllocOptimize::canPromoteDestroyAddr(
    DestroyAddrInst *dai, SmallVectorImpl<AvailableValue> &availableValues) {
  SILValue address = dai->getOperand();

  // We cannot promote destroys of address-only types, because we can't expose
  // the load.
  SILType loadTy = address->getType().getObjectType();
  if (loadTy.isAddressOnly(Module))
    return false;
  
  // If the box has escaped at this instruction, we can't safely promote the
  // load.
  if (DataflowContext.hasEscapedAt(dai))
    return false;
  
  // Compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned firstElt = computeSubelement(address, TheMemory);
  assert(firstElt != ~0U && "destroy within enum projection is not valid");
  unsigned numLoadSubElements = getNumSubElements(loadTy, Module);

  // Find out if we have any available values.  If no bits are demanded, we
  // trivially succeed. This can happen when there is a load of an empty struct.
  if (numLoadSubElements == 0)
    return true;

  // Set up the bitvector of elements being demanded by the load.
  SmallBitVector requiredElts(NumMemorySubElements);
  requiredElts.set(firstElt, firstElt + numLoadSubElements);

  // Compute our available values. If we do not have any available values,
  // return false. We have nothing further to do.
  SmallVector<AvailableValue, 8> tmpList;
  tmpList.resize(NumMemorySubElements);
  if (!DataflowContext.computeAvailableValues(dai, firstElt, numLoadSubElements,
                                              requiredElts, tmpList))
    return false;

  // Now check that we can perform a take upon our available values. This
  // implies today that our value is fully available. If the value is not fully
  // available, we would need to split stores to promote this destroy_addr. We
  // do not support that yet.
  AvailableValueAggregator agg(dai, tmpList, Uses, deadEndBlocks,
                               true /*isTake*/);
  if (!agg.canTake(loadTy, firstElt))
    return false;

  // Ok, we can promote this destroy_addr... move the temporary lists contents
  // into the final AvailableValues list.
  std::move(tmpList.begin(), tmpList.end(),
            std::back_inserter(availableValues));

  return true;
}

// DestroyAddr is a composed operation merging load [take] + destroy_value.  If
// the implicit load's value is available, explode it.
//
// NOTE: We only do this if we have a fully available value.
//
// Note that we handle the general case of a destroy_addr of a piece of the
// memory object, not just destroy_addrs of the entire thing.
void AllocOptimize::promoteDestroyAddr(
    DestroyAddrInst *dai, MutableArrayRef<AvailableValue> availableValues) {
  SILValue address = dai->getOperand();
  SILType loadTy = address->getType().getObjectType();

  // Compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned firstElt = computeSubelement(address, TheMemory);

  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller) loads for any subelements that were
  // not available.
  AvailableValueAggregator agg(dai, availableValues, Uses, deadEndBlocks,
                               true /*isTake*/);
  SILValue newVal = agg.aggregateValues(loadTy, address, firstElt);

  ++NumDestroyAddrPromoted;

  LLVM_DEBUG(llvm::dbgs() << "  *** Promoting destroy_addr: " << *dai << "\n");
  LLVM_DEBUG(llvm::dbgs() << "      To value: " << *newVal << "\n");

  SILBuilderWithScope(dai).emitDestroyValueOperation(dai->getLoc(), newVal);
  dai->eraseFromParent();
}

namespace {

struct DestroyAddrPromotionState {
  ArrayRef<SILInstruction *> destroys;
  SmallVector<unsigned, 8> destroyAddrIndices;
  SmallVector<AvailableValue, 32> availableValueList;
  SmallVector<unsigned, 8> availableValueStartOffsets;

  DestroyAddrPromotionState(ArrayRef<SILInstruction *> destroys)
      : destroys(destroys) {}

  unsigned size() const {
    return destroyAddrIndices.size();
  }

  void initializeForDestroyAddr(unsigned destroyAddrIndex) {
    availableValueStartOffsets.push_back(availableValueList.size());
    destroyAddrIndices.push_back(destroyAddrIndex);
  }

  std::pair<DestroyAddrInst *, MutableArrayRef<AvailableValue>>
  getData(unsigned index) {
    unsigned destroyAddrIndex = destroyAddrIndices[index];
    unsigned startOffset = availableValueStartOffsets[index];
    unsigned count;

    if ((availableValueStartOffsets.size() - 1) != index) {
      count = availableValueStartOffsets[index + 1] - startOffset;
    } else {
      count = availableValueList.size() - startOffset;
    }

    MutableArrayRef<AvailableValue> values(&availableValueList[startOffset],
                                           count);
    auto *dai = cast<DestroyAddrInst>(destroys[destroyAddrIndex]);
    return {dai, values};
  }
};

} // end anonymous namespace

/// If the allocation is an autogenerated allocation that is only stored to
/// (after load promotion) then remove it completely.
bool AllocOptimize::tryToRemoveDeadAllocation() {
  assert((isa<AllocBoxInst>(TheMemory) || isa<AllocStackInst>(TheMemory)) &&
         "Unhandled allocation case");

  // We don't want to remove allocations that are required for useful debug
  // information at -O0.  As such, we only remove allocations if:
  //
  // 1. They are in a transparent function.
  // 2. They are in a normal function, but didn't come from a VarDecl, or came
  //    from one that was autogenerated or inlined from a transparent function.
  SILLocation loc = TheMemory->getLoc();
  if (!TheMemory->getFunction()->isTransparent() &&
      loc.getAsASTNode<VarDecl>() && !loc.isAutoGenerated() &&
      !loc.is<MandatoryInlinedLocation>())
    return false;

  // Check the uses list to see if there are any non-store uses left over after
  // load promotion and other things PMO does.
  for (auto &u : Uses) {
    // Ignore removed instructions.
    if (u.Inst == nullptr)
      continue;

    switch (u.Kind) {
    case PMOUseKind::Assign:
      // Until we can promote the value being destroyed by the assign, we can
      // not remove deallocations with such assigns.
      return false;
    case PMOUseKind::InitOrAssign:
      break;    // These don't prevent removal.
    case PMOUseKind::Initialization:
      if (!isa<ApplyInst>(u.Inst) &&
          // A copy_addr that is not a take affects the retain count
          // of the source.
          (!isa<CopyAddrInst>(u.Inst) ||
           cast<CopyAddrInst>(u.Inst)->isTakeOfSrc()))
        break;
      // FALL THROUGH.
     LLVM_FALLTHROUGH;
    case PMOUseKind::Load:
    case PMOUseKind::IndirectIn:
    case PMOUseKind::InOutUse:
    case PMOUseKind::Escape:
      LLVM_DEBUG(llvm::dbgs() << "*** Failed to remove autogenerated alloc: "
                                 "kept alive by: "
                              << *u.Inst);
      return false;   // These do prevent removal.
    }
  }

  // If our memory is trivially typed, we can just remove it without needing to
  // consider if the stored value needs to be destroyed. So at this point,
  // delete the memory!
  if (MemoryType.isTrivial(Module)) {
    LLVM_DEBUG(llvm::dbgs() << "*** Removing autogenerated trivial allocation: "
                            << *TheMemory);

    // If it is safe to remove, do it.  Recursively remove all instructions
    // hanging off the allocation instruction, then return success.  Let the
    // caller remove the allocation itself to avoid iterator invalidation.
    eraseUsesOfInstruction(TheMemory);

    return true;
  }

  // Otherwise removing the deallocation will drop any releases.  Check that
  // there is nothing preventing removal.
  DestroyAddrPromotionState state(Releases);

  for (auto p : llvm::enumerate(Releases)) {
    auto *r = p.value();
    if (r == nullptr)
      continue;

    // We stash all of the destroy_addr that we see.
    if (auto *dai = dyn_cast<DestroyAddrInst>(r)) {
      state.initializeForDestroyAddr(p.index() /*destroyAddrIndex*/);
      // Make sure we can actually promote this destroy addr. If we can not,
      // then we must bail. In order to not gather available values twice, we
      // gather the available values here that we will use to promote the
      // values.
      if (!canPromoteDestroyAddr(dai, state.availableValueList))
        return false;
      continue;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "*** Failed to remove autogenerated non-trivial alloc: "
                  "kept alive by release: "
               << *r);
    return false;
  }

  // If we reached this point, we can promote all of our destroy_addr.
  for (unsigned i : range(state.size())) {
    DestroyAddrInst *dai;
    MutableArrayRef<AvailableValue> values;
    std::tie(dai, values) = state.getData(i);
    promoteDestroyAddr(dai, values);
    // We do not need to unset releases, since we are going to exit here.
  }

  LLVM_DEBUG(llvm::dbgs() << "*** Removing autogenerated non-trivial alloc: "
                          << *TheMemory);

  // If it is safe to remove, do it.  Recursively remove all instructions
  // hanging off the allocation instruction, then return success.  Let the
  // caller remove the allocation itself to avoid iterator invalidation.
  eraseUsesOfInstruction(TheMemory);

  return true;
}

bool AllocOptimize::optimizeMemoryAccesses() {
  bool changed = false;

  // If we've successfully checked all of the definitive initialization
  // requirements, try to promote loads.  This can explode copy_addrs, so the
  // use list may change size.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &use = Uses[i];
    // Ignore entries for instructions that got expanded along the way.
    if (use.Inst && use.Kind == PMOUseKind::Load) {
      if (promoteLoad(use.Inst)) {
        Uses[i].Inst = nullptr;  // remove entry if load got deleted.
        changed = true;
      }
    }
  }

  return changed;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

static AllocationInst *getOptimizableAllocation(SILInstruction *i) {
  if (!isa<AllocBoxInst>(i) && !isa<AllocStackInst>(i)) {
    return nullptr;
  }

  auto *alloc = cast<AllocationInst>(i);

  // If our aggregate has unreferencable storage, we can't optimize. Return
  // nullptr.
  if (getMemoryType(alloc).aggregateHasUnreferenceableStorage())
    return nullptr;

  // Otherwise we are good to go. Lets try to optimize this memory!
  return alloc;
}

static bool optimizeMemoryAccesses(SILFunction &fn) {
  bool changed = false;
  DeadEndBlocks deadEndBlocks(&fn);

  for (auto &bb : fn) {
    auto i = bb.begin(), e = bb.end();
    while (i != e) {
      // First see if i is an allocation that we can optimize. If not, skip it.
      AllocationInst *alloc = getOptimizableAllocation(&*i);
      if (!alloc) {
        ++i;
        continue;
      }

      LLVM_DEBUG(llvm::dbgs() << "*** PMO Optimize Memory Accesses looking at: "
                              << *alloc << "\n");
      PMOMemoryObjectInfo memInfo(alloc);

      // Set up the datastructure used to collect the uses of the allocation.
      SmallVector<PMOMemoryUse, 16> uses;
      SmallVector<SILInstruction *, 4> destroys;

      // Walk the use list of the pointer, collecting them. If we are not able
      // to optimize, skip this value. *NOTE* We may still scalarize values
      // inside the value.
      if (!collectPMOElementUsesFrom(memInfo, uses, destroys)) {
        ++i;
        continue;
      }

      AllocOptimize allocOptimize(alloc, uses, destroys, deadEndBlocks);
      changed |= allocOptimize.optimizeMemoryAccesses();

      // Move onto the next instruction. We know this is safe since we do not
      // eliminate allocations here.
      ++i;
    }
  }

  return changed;
}

static bool eliminateDeadAllocations(SILFunction &fn) {
  bool changed = false;
  DeadEndBlocks deadEndBlocks(&fn);

  for (auto &bb : fn) {
    auto i = bb.begin(), e = bb.end();
    while (i != e) {
      // First see if i is an allocation that we can optimize. If not, skip it.
      AllocationInst *alloc = getOptimizableAllocation(&*i);
      if (!alloc) {
        ++i;
        continue;
      }

      LLVM_DEBUG(llvm::dbgs()
                 << "*** PMO Dead Allocation Elimination looking at: " << *alloc
                 << "\n");
      PMOMemoryObjectInfo memInfo(alloc);

      // Set up the datastructure used to collect the uses of the allocation.
      SmallVector<PMOMemoryUse, 16> uses;
      SmallVector<SILInstruction *, 4> destroys;

      // Walk the use list of the pointer, collecting them. If we are not able
      // to optimize, skip this value. *NOTE* We may still scalarize values
      // inside the value.
      if (!collectPMOElementUsesFrom(memInfo, uses, destroys)) {
        ++i;
        continue;
      }

      AllocOptimize allocOptimize(alloc, uses, destroys, deadEndBlocks);
      changed |= allocOptimize.tryToRemoveDeadAllocation();

      // Move onto the next instruction. We know this is safe since we do not
      // eliminate allocations here.
      ++i;
      if (alloc->use_empty()) {
        alloc->eraseFromParent();
        ++NumAllocRemoved;
        changed = true;
      }
    }
  }

  return changed;
}

namespace {

class PredictableMemoryAccessOptimizations : public SILFunctionTransform {
  /// The entry point to the transformation.
  ///
  /// FIXME: This pass should not need to rerun on deserialized
  /// functions. Nothing should have changed in the upstream pipeline after
  /// deserialization. However, rerunning does improve some benchmarks. This
  /// either indicates that this pass missing some opportunities the first time,
  /// or has a pass order dependency on other early passes.
  void run() override {
    // TODO: Can we invalidate here just instructions?
    if (optimizeMemoryAccesses(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
  }
};

class PredictableDeadAllocationElimination : public SILFunctionTransform {
  void run() override {
    if (eliminateDeadAllocations(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
  }
};

} // end anonymous namespace

SILTransform *swift::createPredictableMemoryAccessOptimizations() {
  return new PredictableMemoryAccessOptimizations();
}

SILTransform *swift::createPredictableDeadAllocationElimination() {
  return new PredictableDeadAllocationElimination();
}
