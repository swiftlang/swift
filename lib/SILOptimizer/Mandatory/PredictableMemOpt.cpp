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
#include "swift/Basic/Assertions.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/BlotSetVector.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/OSSALifetimeCompletion.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumLoadPromoted, "Number of loads promoted");
STATISTIC(NumLoadTakePromoted, "Number of load takes promoted");
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

static unsigned getNumSubElements(SILType T, SILModule &M,
                                  TypeExpansionContext context) {

  if (auto TT = T.getAs<TupleType>()) {
    unsigned NumElements = 0;
    for (auto index : indices(TT.getElementTypes()))
      NumElements +=
          getNumSubElements(T.getTupleElementType(index), M, context);
    return NumElements;
  }
  
  if (auto *SD = getFullyReferenceableStruct(T)) {
    unsigned NumElements = 0;
    for (auto *D : SD->getStoredProperties())
      NumElements +=
          getNumSubElements(T.getFieldType(D, M, context), M, context);
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
      for (unsigned i = 0, e = TEAI->getFieldIndex(); i != e; ++i) {
        SubElementNumber +=
            getNumSubElements(TT.getTupleElementType(i), M,
                              TypeExpansionContext(*RootInst->getFunction()));
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
        auto context = TypeExpansionContext(*RootInst->getFunction());
        SubElementNumber +=
            getNumSubElements(ST.getFieldType(D, M, context), M, context);
      }
      
      Pointer = SEAI->getOperand();
      continue;
    }

    // This fails when we visit unchecked_take_enum_data_addr. We should just
    // add support for enums.
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
  llvm::SmallSetVector<StoreInst *, 1> InsertionPoints;

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

  void addInsertionPoint(StoreInst *si) & { InsertionPoints.insert(si); }

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
    if (!b.hasOwnership() ||
        Value->getOwnershipKind().isCompatibleWith(OwnershipKind::Guaranteed)) {
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

static bool isFullyAvailable(SILType loadTy, unsigned firstElt,
                             ArrayRef<AvailableValue> AvailableValues) {
  if (firstElt >= AvailableValues.size()) { // #Elements may be zero.
    return false;
  }

  auto &firstVal = AvailableValues[firstElt];

  // Make sure that the first element is available and is the correct type.
  if (!firstVal || firstVal.getType() != loadTy)
    return false;

  auto *function = firstVal.getValue()->getFunction();
  return llvm::all_of(
    range(getNumSubElements(loadTy, function->getModule(),
                            TypeExpansionContext(*function))),
    [&](unsigned index) -> bool {
      auto &val = AvailableValues[firstElt + index];
      return val.getValue() == firstVal.getValue() &&
        val.getSubElementNumber() == index;
    });
}

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
      unsigned NumSubElt = getNumSubElements(
          EltTy, B.getModule(), TypeExpansionContext(B.getFunction()));
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
      auto fieldType = ValTy.getFieldType(
          D, B.getModule(), TypeExpansionContext(B.getFunction()));
      unsigned NumSubElt = getNumSubElements(
          fieldType, B.getModule(), TypeExpansionContext(B.getFunction()));

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

enum class AvailableValueExpectedOwnership {
  Take,
  Borrow,
  Copy,
};

/// A class that aggregates available values, loading them if they are not
/// available.
class AvailableValueAggregator {
  SILModule &M;
  SILBuilderWithScope B;
  SILLocation Loc;
  ArrayRef<AvailableValue> AvailableValueList;
  SmallVectorImpl<PMOMemoryUse> &Uses;
  DeadEndBlocks &deadEndBlocks;
  AvailableValueExpectedOwnership expectedOwnership;

  /// Keep track of all instructions that we have added. Once we are done
  /// promoting a value, we need to make sure that if we need to balance any
  /// copies (to avoid leaks), we do so. This is not used if we are performing a
  /// take.
  SmallVector<SILInstruction *, 16> insertedInsts;

  /// The list of phi nodes inserted by the SSA updater.
  SmallVector<SILPhiArgument *, 16> insertedPhiNodes;

  /// A set of copy_values whose lifetime we balanced while inserting phi
  /// nodes. This means that these copy_value must be skipped in
  /// addMissingDestroysForCopiedValues.
  SmallPtrSet<CopyValueInst *, 16> copyValueProcessedWithPhiNodes;

public:
  AvailableValueAggregator(SILInstruction *Inst,
                           ArrayRef<AvailableValue> AvailableValueList,
                           SmallVectorImpl<PMOMemoryUse> &Uses,
                           DeadEndBlocks &deadEndBlocks,
                           AvailableValueExpectedOwnership expectedOwnership)
      : M(Inst->getModule()), B(Inst), Loc(Inst->getLoc()),
        AvailableValueList(AvailableValueList), Uses(Uses),
        deadEndBlocks(deadEndBlocks), expectedOwnership(expectedOwnership) {}

  // This is intended to be passed by reference only once constructed.
  AvailableValueAggregator(const AvailableValueAggregator &) = delete;
  AvailableValueAggregator(AvailableValueAggregator &&) = delete;
  AvailableValueAggregator &
  operator=(const AvailableValueAggregator &) = delete;
  AvailableValueAggregator &operator=(AvailableValueAggregator &&) = delete;

  SILValue aggregateValues(SILType LoadTy, SILValue Address, unsigned FirstElt,
                           bool isTopLevel = true);
  bool canTake(SILType loadTy, unsigned firstElt) const;

  void print(llvm::raw_ostream &os) const;
  void dump() const LLVM_ATTRIBUTE_USED;

  bool isTake() const {
    return expectedOwnership == AvailableValueExpectedOwnership::Take;
  }

  bool isBorrow() const {
    return expectedOwnership == AvailableValueExpectedOwnership::Borrow;
  }

  bool isCopy() const {
    return expectedOwnership == AvailableValueExpectedOwnership::Copy;
  }

  /// Given a load_borrow that we have aggregated a new value for, fixup the
  /// reference counts of the intermediate copies and phis to ensure that all
  /// forwarding operations in the CFG are strongly control equivalent (i.e. run
  /// the same number of times).
  void fixupOwnership(SILInstruction *load, SILValue newVal) {
    assert(isa<LoadBorrowInst>(load) || isa<LoadInst>(load));

    addHandOffCopyDestroysForPhis(load, newVal);
    addMissingDestroysForCopiedValues(load, newVal);
  }

private:
  SILValue aggregateFullyAvailableValue(SILType loadTy, unsigned firstElt);
  SILValue aggregateTupleSubElts(TupleType *tt, SILType loadTy,
                                 SILValue address, unsigned firstElt);
  SILValue aggregateStructSubElts(StructDecl *sd, SILType loadTy,
                                  SILValue address, unsigned firstElt);
  SILValue handlePrimitiveValue(SILType loadTy, SILValue address,
                                unsigned firstElt);


  /// If as a result of us copying values, we may have unconsumed destroys, find
  /// the appropriate location and place the values there. Only used when
  /// ownership is enabled.
  void addMissingDestroysForCopiedValues(SILInstruction *load, SILValue newVal);

  /// As a result of us using the SSA updater, insert hand off copy/destroys at
  /// each phi and make sure that intermediate phis do not leak by inserting
  /// destroys along paths that go through the intermediate phi that do not also
  /// go through the
  void addHandOffCopyDestroysForPhis(SILInstruction *load, SILValue newVal);
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
  if (isFullyAvailable(loadTy, firstElt, AvailableValueList))
    return true;

  // Otherwise see if we are an aggregate with fully available leaf types.
  if (TupleType *tt = loadTy.getAs<TupleType>()) {
    return llvm::all_of(indices(tt->getElements()), [&](unsigned eltNo) {
      SILType eltTy = loadTy.getTupleElementType(eltNo);
      unsigned numSubElt =
          getNumSubElements(eltTy, M, TypeExpansionContext(B.getFunction()));
      bool success = canTake(eltTy, firstElt);
      firstElt += numSubElt;
      return success;
    });
  }

  if (auto *sd = getFullyReferenceableStruct(loadTy)) {
    return llvm::all_of(sd->getStoredProperties(), [&](VarDecl *decl) -> bool {
      auto context = TypeExpansionContext(B.getFunction());
      SILType eltTy = loadTy.getFieldType(decl, M, context);
      unsigned numSubElt = getNumSubElements(eltTy, M, context);
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
  if (isTopLevel && isTake() && !canTake(LoadTy, FirstElt)) {
    return SILValue();
  }

  // Check to see if the requested value is fully available, as an aggregate.
  // This is a super-common case for single-element structs, but is also a
  // general answer for arbitrary structs and tuples as well.
  if (SILValue Result = aggregateFullyAvailableValue(LoadTy, FirstElt)) {
    return Result;
  }

  // If we have a tuple type, then aggregate the tuple's elements into a full
  // tuple value.
  if (TupleType *tupleType = LoadTy.getAs<TupleType>()) {
    SILValue result =
        aggregateTupleSubElts(tupleType, LoadTy, Address, FirstElt);
    if (isTopLevel && result->getOwnershipKind() == OwnershipKind::Guaranteed) {
      SILValue borrowedResult = result;
      SILBuilderWithScope builder(&*B.getInsertionPoint(), &insertedInsts);
      result = builder.emitCopyValueOperation(Loc, borrowedResult);
      SmallVector<BorrowedValue, 4> introducers;
      bool foundIntroducers =
          getAllBorrowIntroducingValues(borrowedResult, introducers);
      (void)foundIntroducers;
      assert(foundIntroducers);
      for (auto value : introducers) {
        builder.emitEndBorrowOperation(Loc, value.value);
      }
    }
    return result;
  }

  // If we have a struct type, then aggregate the struct's elements into a full
  // struct value.
  if (auto *structDecl = getFullyReferenceableStruct(LoadTy)) {
    SILValue result =
        aggregateStructSubElts(structDecl, LoadTy, Address, FirstElt);
    if (isTopLevel && result->getOwnershipKind() == OwnershipKind::Guaranteed) {
      SILValue borrowedResult = result;
      SILBuilderWithScope builder(&*B.getInsertionPoint(), &insertedInsts);
      result = builder.emitCopyValueOperation(Loc, borrowedResult);
      SmallVector<BorrowedValue, 4> introducers;
      bool foundIntroducers =
          getAllBorrowIntroducingValues(borrowedResult, introducers);
      (void)foundIntroducers;
      assert(foundIntroducers);
      for (auto value : introducers) {
        builder.emitEndBorrowOperation(Loc, value.value);
      }
    }
    return result;
  }

  // Otherwise, we have a non-aggregate primitive. Load or extract the value.
  //
  // NOTE: We should never call this when taking since when taking we know that
  // our underlying value is always fully available.
  assert(!isTake());
  return handlePrimitiveValue(LoadTy, Address, FirstElt);
}

// See if we have this value is fully available. In such a case, return it as an
// aggregate. This is a super-common case for single-element structs, but is
// also a general answer for arbitrary structs and tuples as well.
SILValue
AvailableValueAggregator::aggregateFullyAvailableValue(SILType loadTy,
                                                       unsigned firstElt) {
  // Check if our underlying type is fully available. If it isn't, bail.
  if (!isFullyAvailable(loadTy, firstElt, AvailableValueList))
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
    if (isTake())
      return firstVal.getValue();
    // Otherwise, return a copy of the value.
    return builder.emitCopyValueOperation(loc, firstVal.getValue());
  }

  // If we have multiple insertion points, put copies at each point and use the
  // SSA updater to get a value. The reason why this is safe is that we can only
  // have multiple insertion points if we are storing exactly the same value
  // implying that we can just copy firstVal at each insertion point.
  SILSSAUpdater updater(&insertedPhiNodes);
  updater.initialize(&B.getFunction(), loadTy,
                     B.hasOwnership() ? OwnershipKind::Owned
                                      : OwnershipKind::None);

  std::optional<SILValue> singularValue;
  for (auto *insertPt : insertPts) {
    // Use the scope and location of the store at the insertion point.
    SILBuilderWithScope builder(insertPt, &insertedInsts);
    SILLocation loc = insertPt->getLoc();
    SILValue eltVal = firstVal.getValue();

    // If we are not taking, copy the element value.
    if (!isTake()) {
      eltVal = builder.emitCopyValueOperation(loc, eltVal);
    }

    if (!singularValue.has_value()) {
      singularValue = eltVal;
    } else if (*singularValue != eltVal) {
      singularValue = SILValue();
    }

    // And then put the value into the SSA updater.
    updater.addAvailableValue(insertPt->getParent(), eltVal);
  }

  // If we only are tracking a singular value, we do not need to construct
  // SSA. Just return that value.
  if (auto val = singularValue.value_or(SILValue())) {
    // This assert documents that we are expecting that if we are in ossa, have
    // a non-trivial value, and are not taking, we should never go down this
    // code path. If we did, we would need to insert a copy here. The reason why
    // we know we will never go down this code path is since we have been
    // inserting copy_values implying that our potential singular value would be
    // of the copy_values which are guaranteed to all be different.
    assert((!B.hasOwnership() || isTake() ||
            val->getType().isTrivial(*B.getInsertionBB()->getParent())) &&
           "Should never reach this code path if we are in ossa and have a "
           "non-trivial value");
    return val;
  }

  // Finally, grab the value from the SSA updater.
  SILValue result = updater.getValueInMiddleOfBlock(B.getInsertionBB());
  assert(result->getOwnershipKind().isCompatibleWith(OwnershipKind::Owned));
  if (isTake() || !B.hasOwnership()) {
    return result;
  }

  // Be careful with this value and insert a copy in our load block to prevent
  // any weird control equivalence issues.
  SILBuilderWithScope builder(&*B.getInsertionPoint(), &insertedInsts);
  return builder.emitCopyValueOperation(Loc, result);
}

SILValue AvailableValueAggregator::aggregateTupleSubElts(TupleType *TT,
                                                         SILType LoadTy,
                                                         SILValue Address,
                                                         unsigned FirstElt) {
  SmallVector<SILValue, 4> ResultElts;

  for (unsigned EltNo : indices(TT->getElements())) {
    SILType EltTy = LoadTy.getTupleElementType(EltNo);
    unsigned NumSubElt =
        getNumSubElements(EltTy, M, TypeExpansionContext(B.getFunction()));

    // If we are missing any of the available values in this struct element,
    // compute an address to load from.
    SILValue EltAddr;
    if (anyMissing(FirstElt, NumSubElt, AvailableValueList)) {
      assert(!isTake() && "When taking, values should never be missing?!");
      EltAddr =
          B.createTupleElementAddr(Loc, Address, EltNo, EltTy.getAddressType());
    }

    ResultElts.push_back(
        aggregateValues(EltTy, EltAddr, FirstElt, /*isTopLevel*/ false));
    FirstElt += NumSubElt;
  }

  // If we are going to use this to promote a borrowed value, insert borrow
  // operations. Eventually I am going to do this for everything, but this
  // should make it easier to bring up.
  if (!isTake()) {
    for (unsigned i : indices(ResultElts)) {
      ResultElts[i] = B.emitBeginBorrowOperation(Loc, ResultElts[i]);
    }
  }

  return B.createTuple(Loc, LoadTy, ResultElts);
}

SILValue AvailableValueAggregator::aggregateStructSubElts(StructDecl *sd,
                                                          SILType loadTy,
                                                          SILValue address,
                                                          unsigned firstElt) {
  SmallVector<SILValue, 4> resultElts;

  for (auto *decl : sd->getStoredProperties()) {
    auto context = TypeExpansionContext(B.getFunction());
    SILType eltTy = loadTy.getFieldType(decl, M, context);
    unsigned numSubElt = getNumSubElements(eltTy, M, context);

    // If we are missing any of the available values in this struct element,
    // compute an address to load from.
    SILValue eltAddr;
    if (anyMissing(firstElt, numSubElt, AvailableValueList)) {
      assert(!isTake() && "When taking, values should never be missing?!");
      eltAddr =
          B.createStructElementAddr(Loc, address, decl, eltTy.getAddressType());
    }

    resultElts.push_back(
        aggregateValues(eltTy, eltAddr, firstElt, /*isTopLevel*/ false));
    firstElt += numSubElt;
  }

  if (!isTake()) {
    for (unsigned i : indices(resultElts)) {
      resultElts[i] = B.emitBeginBorrowOperation(Loc, resultElts[i]);
    }
  }

  return B.createStruct(Loc, loadTy, resultElts);
}

// We have looked through all of the aggregate values and finally found a value
// that is not available without transforming, i.e. a "primitive value". If the
// value is available, use it (extracting if we need to), otherwise emit a load
// of the value with the appropriate qualifier.
SILValue AvailableValueAggregator::handlePrimitiveValue(SILType loadTy,
                                                        SILValue address,
                                                        unsigned firstElt) {
  assert(!isTake() && "Should only take fully available values?!");

  // If the value is not available, load the value and update our use list.
  auto &val = AvailableValueList[firstElt];
  if (!val) {
    LoadInst *load = ([&]() {
      if (B.hasOwnership()) {
        SILBuilderWithScope builder(&*B.getInsertionPoint(), &insertedInsts);
        return builder.createTrivialLoadOr(Loc, address,
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
    assert(!builder.hasOwnership() ||
           eltVal->getOwnershipKind().isCompatibleWith(OwnershipKind::Owned));
    assert(eltVal->getType() == loadTy && "Subelement types mismatch");

    if (!builder.hasOwnership()) {
      return eltVal;
    }

    SILBuilderWithScope builder2(&*B.getInsertionPoint(), &insertedInsts);
    return builder2.emitCopyValueOperation(Loc, eltVal);
  }

  // If we have an available value, then we want to extract the subelement from
  // the borrowed aggregate before each insertion point. Note that since we have
  // inserted copies at each of these insertion points, we know that we will
  // never have the same value along all paths unless we have a trivial value
  // meaning the SSA updater given a non-trivial value must /always/ be used.
  SILSSAUpdater updater(&insertedPhiNodes);
  updater.initialize(&B.getFunction(), loadTy,
                     B.hasOwnership() ? OwnershipKind::Owned
                                      : OwnershipKind::None);

  std::optional<SILValue> singularValue;
  for (auto *i : insertPts) {
    // Use the scope and location of the store at the insertion point.
    SILBuilderWithScope builder(i, &insertedInsts);
    SILLocation loc = i->getLoc();
    SILValue eltVal = nonDestructivelyExtractSubElement(val, builder, loc);
    assert(!builder.hasOwnership() ||
           eltVal->getOwnershipKind().isCompatibleWith(OwnershipKind::Owned));

    if (!singularValue.has_value()) {
      singularValue = eltVal;
    } else if (*singularValue != eltVal) {
      singularValue = SILValue();
    }

    updater.addAvailableValue(i->getParent(), eltVal);
  }

  SILBasicBlock *insertBlock = B.getInsertionBB();

  // If we are not in ossa and have a singular value or if we are in ossa and
  // have a trivial singular value, just return that value.
  //
  // This can never happen for non-trivial values in ossa since we never should
  // visit this code path if we have a take implying that non-trivial values
  // /will/ have a copy and thus are guaranteed (since each copy yields a
  // different value) to not be singular values.
  if (auto val = singularValue.value_or(SILValue())) {
    assert((!B.hasOwnership() ||
            val->getType().isTrivial(*insertBlock->getParent())) &&
           "Should have inserted copies for each insertion point, so shouldn't "
           "have a singular value if non-trivial?!");
    return val;
  }

  // Finally, grab the value from the SSA updater.
  SILValue eltVal = updater.getValueInMiddleOfBlock(insertBlock);
  assert(!B.hasOwnership() ||
         eltVal->getOwnershipKind().isCompatibleWith(OwnershipKind::Owned));
  assert(eltVal->getType() == loadTy && "Subelement types mismatch");
  if (!B.hasOwnership())
    return eltVal;
  SILBuilderWithScope builder(&*B.getInsertionPoint(), &insertedInsts);
  return builder.emitCopyValueOperation(Loc, eltVal);
}

static SILInstruction *
getNonPhiBlockIncomingValueDef(SILValue incomingValue,
                               SingleValueInstruction *phiCopy) {
  assert(isa<CopyValueInst>(phiCopy));
  auto *phiBlock = phiCopy->getParent();
  if (phiBlock == incomingValue->getParentBlock()) {
    return nullptr;
  }

  if (auto *cvi = dyn_cast<CopyValueInst>(incomingValue)) {
    return cvi;
  }

  assert(isa<SILPhiArgument>(incomingValue));

  // Otherwise, our copy_value may not be post-dominated by our phi. To
  // work around that, we need to insert destroys along the other
  // paths. So set base to the first instruction in our argument's block,
  // so we can insert destroys for our base.
  return &*incomingValue->getParentBlock()->begin();
}

static bool
terminatorHasAnyKnownPhis(TermInst *ti,
                          ArrayRef<SILPhiArgument *> insertedPhiNodesSorted) {
  for (auto succArgList : ti->getSuccessorBlockArgumentLists()) {
    if (llvm::any_of(succArgList, [&](SILArgument *arg) {
          return binary_search(insertedPhiNodesSorted,
                               cast<SILPhiArgument>(arg));
        })) {
      return true;
    }
  }

  return false;
}

namespace {

class PhiNodeCopyCleanupInserter {
  llvm::SmallMapVector<SILValue, unsigned, 8> incomingValues;

  /// Map from index -> (incomingValueIndex, copy).
  ///
  /// We are going to stable_sort this array using the indices of
  /// incomingValueIndex. This will ensure that we always visit in
  /// insertion order our incoming values (since the indices we are
  /// sorting by are the count of incoming values we have seen so far
  /// when we see the incoming value) and maintain the internal
  /// insertion sort within our range as well. This ensures that we
  /// visit our incoming values in visitation order and that within
  /// their own values, also visit them in visitation order with
  /// respect to each other.
  SmallFrozenMultiMap<unsigned, SingleValueInstruction *, 16> copiesToCleanup;

  /// The lifetime frontier that we use to compute lifetime endpoints
  /// when emitting cleanups.
  ValueLifetimeAnalysis::Frontier lifetimeFrontier;

public:
  PhiNodeCopyCleanupInserter() = default;

  void trackNewCleanup(SILValue incomingValue, CopyValueInst *copy) {
    auto entry = std::make_pair(incomingValue, incomingValues.size());
    auto iter = incomingValues.insert(entry);
    // If we did not succeed, then iter.first.second is the index of
    // incoming value. Otherwise, it will be nextIndex.
    copiesToCleanup.insert(iter.first->second, copy);
  }

  void emit(DeadEndBlocks &deadEndBlocks) &&;
};

} // end anonymous namespace

void PhiNodeCopyCleanupInserter::emit(DeadEndBlocks &deadEndBlocks) && {
  // READ THIS: We are being very careful here to avoid allowing for
  // non-determinism to enter here.
  //
  // 1. First we create a list of indices of our phi node data. Then we use a
  //    stable sort those indices into the order in which our phi node cleanups
  //    would be in if we compared just using incomingValues. We use a stable
  //    sort here to ensure that within the same "cohort" of values, our order
  //    is insertion order.
  //
  // 2. We go through the list of phiNodeCleanupStates in insertion order. We
  //    also maintain a set of already visited base values. When we visit the
  //    first phiNodeCleanupState for a specific phi, we process the phi
  //    then. This ensures that we always process the phis in insertion order as
  //    well.
  copiesToCleanup.setFrozen();

  for (auto keyValue : copiesToCleanup.getRange()) {
    unsigned incomingValueIndex = keyValue.first;
    auto copies = keyValue.second;

    SILValue incomingValue =
        std::next(incomingValues.begin(), incomingValueIndex)->first;
    SingleValueInstruction *phiCopy = copies.front();
    auto *insertPt = getNonPhiBlockIncomingValueDef(incomingValue, phiCopy);
    auto loc = RegularLocation::getAutoGeneratedLocation();

    // Before we do anything, see if we have a single cleanup state. In such a
    // case, we could have that we have a phi node as an incoming value and a
    // copy_value in that same block. In such a case, we want to just insert the
    // copy and continue. This means that
    // cleanupState.getNonPhiBlockIncomingValueDef() should always return a
    // non-null value in the code below.
    if (copies.size() == 1 && isa<SILArgument>(incomingValue) && !insertPt) {
      SILBasicBlock *phiBlock = phiCopy->getParent();
      SILBuilderWithScope builder(phiBlock->getTerminator());
      builder.createDestroyValue(loc, incomingValue);
      continue;
    }

    // Otherwise, we know that we have for this incomingValue, multiple
    // potential insert pts that we need to handle at the same time with our
    // lifetime query. Lifetime extend our base over these copy_value uses.
    assert(lifetimeFrontier.empty());
    auto *def = getNonPhiBlockIncomingValueDef(incomingValue, phiCopy);
    assert(def && "Should never have a nullptr here since we handled all of "
                  "the single block cases earlier");
    ValueLifetimeAnalysis analysis(def, copies);
    bool foundCriticalEdges = !analysis.computeFrontier(
        lifetimeFrontier, ValueLifetimeAnalysis::DontModifyCFG, &deadEndBlocks);
    (void)foundCriticalEdges;
    assert(!foundCriticalEdges);

    while (!lifetimeFrontier.empty()) {
      auto *insertPoint = lifetimeFrontier.pop_back_val();
      SILBuilderWithScope builder(insertPoint);
      builder.createDestroyValue(loc, incomingValue);
    }
  }
}

void AvailableValueAggregator::addHandOffCopyDestroysForPhis(
    SILInstruction *load, SILValue newVal) {
  assert(isa<LoadBorrowInst>(load) || isa<LoadInst>(load));

  SmallVector<SILBasicBlock *, 8> leakingBlocks;
  SmallVector<std::pair<SILBasicBlock *, SILValue>, 8> incomingValues;
  auto loc = RegularLocation::getAutoGeneratedLocation();

#ifndef NDEBUG
  LLVM_DEBUG(llvm::dbgs() << "Inserted Phis!\n");
  for (auto *phi : insertedPhiNodes) {
    LLVM_DEBUG(llvm::dbgs() << "Phi: " << *phi);
  }
#endif

  // Before we begin, identify the offset for all phis that are intermediate
  // phis inserted by the SSA updater. We are taking advantage of the fact that
  // the SSA updater just constructs the web without knowledge of ownership. So
  // if a phi node is only used by another phi node that we inserted, then we
  // have an intermediate phi node.
  //
  // TODO: There should be a better way of doing this than doing a copy + sort.
  SmallVector<SILPhiArgument *, 32> insertedPhiNodesSorted;
  llvm::copy(insertedPhiNodes, std::back_inserter(insertedPhiNodesSorted));
  llvm::sort(insertedPhiNodesSorted);

  SmallBitVector intermediatePhiOffsets(insertedPhiNodes.size());
  for (unsigned i : indices(insertedPhiNodes)) {
    if (TermInst *termInst =
            insertedPhiNodes[i]->getSingleUserOfType<TermInst>()) {
      // Only set the value if we find termInst has a successor with a phi node
      // in our insertedPhiNodes.
      if (terminatorHasAnyKnownPhis(termInst, insertedPhiNodesSorted)) {
        intermediatePhiOffsets.set(i);
      }
    }
  }

  // First go through all of our phi nodes doing the following:
  //
  // 1. If any of the phi node have a copy_value as an operand, we know that the
  //    copy_value does not dominate our final definition since otherwise the
  //    SSA updater would not have inserted a phi node here. In such a case
  //    since we may not have that the copy_value is post-dominated by the phi,
  //    we need to insert a copy_value at the phi to allow for post-domination
  //    and then use the ValueLifetimeChecker to determine the rest of the
  //    frontier for the base value.
  //
  // 2. If our phi node is used by another phi node, we run into a similar
  //    problem where we could have that our original phi node does not dominate
  //    our final definition (since the SSA updater would not have inserted the
  //    phi) and may not be strongly control dependent on our phi. To work
  //    around this problem, we insert at the phi a copy_value to allow for the
  //    phi to post_dominate its copy and then extend the lifetime of the phied
  //    value over that copy.
  //
  // As an extra complication to this, when we insert compensating releases for
  // any copy_values from (1), we need to insert the destroy_value on "base
  // values" (either a copy_value or the first instruction of a phi argument's
  // block) /after/ we have found all of the base_values to ensure that if the
  // same base value is used by multiple phis, we do not insert too many destroy
  // value.
  //
  // NOTE: At first glance one may think that such a problem could not occur
  // with phi nodes as well. Sadly if we allow for double backedge loops, it is
  // possible (there may be more cases).
  PhiNodeCopyCleanupInserter cleanupInserter;

  for (unsigned i : indices(insertedPhiNodes)) {
    auto *phi = insertedPhiNodes[i];

    // If our phi is not owned, continue. No fixes are needed.
    if (phi->getOwnershipKind() != OwnershipKind::Owned)
      continue;

    LLVM_DEBUG(llvm::dbgs() << "Visiting inserted phi: " << *phi);
    // Otherwise, we have a copy_value that may not be strongly control
    // equivalent with our phi node. In such a case, we need to use
    // ValueLifetimeAnalysis to lifetime extend the copy such that we can
    // produce a new copy_value at the phi. We insert destroys along the
    // frontier.
    leakingBlocks.clear();
    incomingValues.clear();

    phi->getIncomingPhiValues(incomingValues);
    unsigned phiIndex = phi->getIndex();
    for (auto pair : incomingValues) {
      SILValue value = pair.second;

      // If we had a non-trivial type with non-owned ownership, we will not see
      // a copy_value, so skip them here.
      if (value->getOwnershipKind() != OwnershipKind::Owned)
        continue;

      // Otherwise, value should be from a copy_value or a phi node.
      assert(isa<CopyValueInst>(value) || isa<SILPhiArgument>(value));

      // If we have a copy_value, remove it from the inserted insts set so we
      // skip it when we start processing insertedInstrs.
      if (auto *cvi = dyn_cast<CopyValueInst>(value)) {
        copyValueProcessedWithPhiNodes.insert(cvi);

        // Then check if our termInst is in the same block as our copy_value. In
        // such a case, we can just use the copy_value as our phi's value
        // without needing to worry about any issues around control equivalence.
        if (pair.first == cvi->getParent())
          continue;
      } else {
        assert(isa<SILPhiArgument>(value));
      }

      // Otherwise, insert a copy_value instruction right before the phi. We use
      // that for our actual phi.
      auto *termInst = pair.first->getTerminator();
      SILBuilderWithScope builder(termInst);
      CopyValueInst *phiCopy = builder.createCopyValue(loc, value);
      termInst->setOperand(phiIndex, phiCopy);

      // Now that we know our base, phi, phiCopy for this specific incoming
      // value, append it to the phiNodeCleanupState so we can insert
      // destroy_values late after we visit all insertedPhiNodes.
      cleanupInserter.trackNewCleanup(value, phiCopy);
    }

    // Then see if our phi is an intermediate phi. If it is an intermediate phi,
    // we know that this is not the phi node that is post-dominated by the
    // load_borrow and that we will lifetime extend it via the child
    // phi. Instead, we need to just ensure that our phi arg does not leak onto
    // its set of post-dominating paths, subtracting from that set the path
    // through our terminator use.
    if (intermediatePhiOffsets[i]) {
      continue;
    }

    // If we reach this point, then we know that we are a phi node that actually
    // dominates our user so we need to lifetime extend it over the
    // load_borrow. Thus insert copy_value along the incoming edges and then
    // lifetime extend the phi node over the load_borrow.
    //
    // The linear lifetime checker doesn't care if the passed in load is
    // actually a user of our copy_value. What we care about is that the load is
    // guaranteed to be in the block where we have reformed the tuple in a
    // consuming manner. This means if we add it as the consuming use of the
    // copy, we can find the leaking places if any exist.
    //
    // Then perform the linear lifetime check. If we succeed, continue. We have
    // no further work to do.
    auto *loadOperand = &load->getAllOperands()[0];
    LinearLifetimeChecker checker(&deadEndBlocks);
    bool consumedInLoop = checker.completeConsumingUseSet(
        phi, loadOperand, [&](SILBasicBlock::iterator iter) {
          SILBuilderWithScope builder(iter);
          builder.emitDestroyValueOperation(loc, phi);
        });

    // Ok, we found some leaking blocks and potentially that our load is
    // "consumed" inside a different loop in the loop nest from cvi. If we are
    // consumed in the loop, then our visit should have inserted all of the
    // necessary destroys for us by inserting the destroys on the loop
    // boundaries. So, continue.
    //
    // NOTE: This includes cases where due to an infinite loop, we did not
    // insert /any/ destroys since the loop has no boundary in a certain sense.
    if (consumedInLoop) {
      continue;
    }

    // Otherwise, we need to insert one last destroy after the load for our phi.
    auto next = std::next(load->getIterator());
    SILBuilderWithScope builder(next);
    builder.emitDestroyValueOperation(
        RegularLocation::getAutoGeneratedLocation(), phi);
  }

  // Alright! In summary, we just lifetime extended all of our phis,
  // lifetime extended them to the load block, and inserted phi copies
  // at all of our intermediate phi nodes. Now we need to cleanup and
  // insert all of the compensating destroy_value that we need.
  std::move(cleanupInserter).emit(deadEndBlocks);

  // Clear the phi node array now that we are done.
  insertedPhiNodes.clear();
}

void AvailableValueAggregator::addMissingDestroysForCopiedValues(
    SILInstruction *load, SILValue newVal) {
  assert(B.hasOwnership() &&
         "We assume this is only called if we have ownership");

  SmallVector<SILBasicBlock *, 8> leakingBlocks;
  auto loc = RegularLocation::getAutoGeneratedLocation();

  for (auto *inst : insertedInsts) {
    // Otherwise, see if this is a load [copy]. It if it a load [copy], then we
    // know that the load [copy] must be in the load block meaning we can just
    // put a destroy_value /after/ the load_borrow to ensure that the value
    // lives long enough for us to copy_value it or a derived value for the
    // begin_borrow.
    if (auto *li = dyn_cast<LoadInst>(inst)) {
      if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
        assert(li->getParent() == load->getParent());
        auto next = std::next(load->getIterator());
        SILBuilderWithScope builder(next);
        builder.emitDestroyValueOperation(
            RegularLocation::getAutoGeneratedLocation(), li);
        continue;
      }
    }

    // Our copy_value may have been unset above if it was used by a phi
    // (implying it does not dominate our final user).
    auto *cvi = dyn_cast<CopyValueInst>(inst);
    if (!cvi)
      continue;

    // If we already handled this copy_value above when handling phi nodes, just
    // continue.
    if (copyValueProcessedWithPhiNodes.count(cvi))
      continue;

    // Clear our state.
    leakingBlocks.clear();

    // The linear lifetime checker doesn't care if the passed in load is
    // actually a user of our copy_value. What we care about is that the load is
    // guaranteed to be in the block where we have reformed the tuple in a
    // consuming manner. This means if we add it as the consuming use of the
    // copy, we can find the leaking places if any exist.
    //
    // Then perform the linear lifetime check. If we succeed, continue. We have
    // no further work to do.
    auto *loadOperand = &load->getAllOperands()[0];
    LinearLifetimeChecker checker(&deadEndBlocks);
    bool consumedInLoop = checker.completeConsumingUseSet(
        cvi, loadOperand, [&](SILBasicBlock::iterator iter) {
          SILBuilderWithScope builder(iter);
          builder.emitDestroyValueOperation(loc, cvi);
        });

    // Ok, we found some leaking blocks and potentially that our load is
    // "consumed" inside a different loop in the loop nest from cvi. If we are
    // consumed in the loop, then our visit should have inserted all of the
    // necessary destroys for us by inserting the destroys on the loop
    // boundaries. So, continue.
    //
    // NOTE: This includes cases where due to an infinite loop, we did not
    // insert /any/ destroys since the loop has no boundary in a certain sense.
    if (consumedInLoop) {
      continue;
    }

    // Otherwise, we need to insert one last destroy after the load for our phi.
    auto next = std::next(load->getIterator());
    SILBuilderWithScope builder(next);
    builder.emitDestroyValueOperation(
        RegularLocation::getAutoGeneratedLocation(), cvi);
  }
}

//===----------------------------------------------------------------------===//
//                          Available Value Dataflow
//===----------------------------------------------------------------------===//

namespace {

/// Given a piece of memory, the memory's uses, and destroys perform a single
/// round of semi-optimistic backwards dataflow for each use. The result is the
/// set of available values that reach the specific use of the field in the
/// allocated object.
///
/// The general form of the algorithm is that in our constructor, we analyze our
/// uses and determine available values. Then users call computeAvailableValues
/// which looks backwards up the control flow graph for available values that we
/// can use.
///
/// NOTE: The reason why we say that the algorithm is semi-optimistic is that we
/// assume that all incoming elements into a loopheader will be the same. If we
/// find a conflict, we record it and fail.
class AvailableValueDataflowContext {
  /// The base memory we are performing dataflow upon.
  AllocationInst *TheMemory;

  /// The number of sub elements of our memory.
  unsigned NumMemorySubElements;

  /// The set of uses that we are tracking. This is only here so we can update
  /// when exploding copy_addr. It would be great if we did not have to store
  /// this.
  SmallVectorImpl<PMOMemoryUse> &Uses;

  InstructionDeleter &deleter;

  /// The set of blocks with local definitions.
  ///
  /// We use this to determine if we should visit a block or look at a block's
  /// predecessors during dataflow for an available value.
  BasicBlockFlag HasLocalDefinition;

  /// The set of blocks that have definitions which specifically "kill" the
  /// given value. If a block is in this set, there must be an instruction in
  /// LoadTakeUse whose parent is the block. This is just used to speed up
  /// computation.
  ///
  /// NOTE: These are not considered escapes.
  BasicBlockFlag HasLocalKill;

  /// This is a set of load takes that we are tracking. HasLocalKill is the set
  /// of parent blocks of these instructions.
  llvm::SmallPtrSet<SILInstruction *, 8> LoadTakeUses;

  /// This is a map of uses that are not loads (i.e., they are Stores,
  /// InOutUses, and Escapes), to their entry in Uses.
  llvm::SmallDenseMap<SILInstruction *, unsigned, 16> NonLoadUses;

  /// Does this value escape anywhere in the function. We use this very
  /// conservatively.
  bool HasAnyEscape = false;

public:
  AvailableValueDataflowContext(AllocationInst *TheMemory,
                                unsigned NumMemorySubElements,
                                SmallVectorImpl<PMOMemoryUse> &Uses,
                                InstructionDeleter &deleter);

  // Find an available for for subelements of 'SrcAddr'.
  // Return the SILType of the object in 'SrcAddr' and index of the first sub
  // element in that object.
  // If not all subelements are availab, return nullopt.
  std::optional<std::pair<SILType, unsigned>>
  computeAvailableValues(SILValue SrcAddr, SILInstruction *Inst,
                         SmallVectorImpl<AvailableValue> &AvailableValues);

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
    SmallVectorImpl<PMOMemoryUse> &InputUses, InstructionDeleter &deleter)
    : TheMemory(InputTheMemory), NumMemorySubElements(NumMemorySubElements),
      Uses(InputUses), deleter(deleter),
      HasLocalDefinition(InputTheMemory->getFunction()),
      HasLocalKill(InputTheMemory->getFunction()) {
  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (unsigned ui : indices(Uses)) {
    auto &Use = Uses[ui];
    assert(Use.Inst && "No instruction identified?");

    // If we have a load...
    if (Use.Kind == PMOUseKind::Load) {
      // Skip load borrow use and open_existential_addr.
      if (isa<LoadBorrowInst>(Use.Inst) || isa<OpenExistentialAddrInst>(Use.Inst))
        continue;

      // That is not a load take, continue. Otherwise, stash the load [take].
      if (auto *LI = dyn_cast<LoadInst>(Use.Inst)) {
        if (LI->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
          LoadTakeUses.insert(LI);
          HasLocalKill.set(LI->getParent());
        }
        continue;
      }

      // If we have a copy_addr as our load, it means we are processing a source
      // of the value. If the copy_addr is taking from the source, we need to
      // treat it like a load take use.
      if (auto *CAI = dyn_cast<CopyAddrInst>(Use.Inst)) {
        if (CAI->isTakeOfSrc() == IsTake) {
          LoadTakeUses.insert(CAI);
          HasLocalKill.set(CAI->getParent());
        }
        continue;
      }

      llvm_unreachable("Unhandled SILInstructionKind for PMOUseKind::Load?!");
    }

    // Keep track of all the uses that aren't loads.
    NonLoadUses[Use.Inst] = ui;
    HasLocalDefinition.set(Use.Inst->getParent());

    if (Use.Kind == PMOUseKind::Escape) {
      // Determine which blocks the value can escape from.  We aren't allowed to
      // promote loads in blocks reachable from an escape point.
      HasAnyEscape = true;
    }
  }
  
  // If isn't really a use, but we account for the alloc_box/mark_uninitialized
  // as a use so we see it in our dataflow walks.
  NonLoadUses[TheMemory] = ~0U;
  HasLocalDefinition.set(TheMemory->getParent());
}


std::optional<std::pair<SILType, unsigned>>
AvailableValueDataflowContext::computeAvailableValues(
    SILValue SrcAddr, SILInstruction *Inst,
    SmallVectorImpl<AvailableValue> &AvailableValues) {
  // If the box has escaped at this instruction, we can't safely promote the
  // load.
  if (hasEscapedAt(Inst))
    return std::nullopt;

  SILType LoadTy = SrcAddr->getType().getObjectType();

  // If this is a load/copy_addr from a struct field that we want to promote,
  // compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned FirstElt = computeSubelement(SrcAddr, TheMemory);

  // If this is a load from within an enum projection, we can't promote it since
  // we don't track subelements in a type that could be changing.
  if (FirstElt == ~0U)
    return std::nullopt;

  unsigned NumLoadSubElements = getNumSubElements(
    LoadTy, getModule(), TypeExpansionContext(*TheMemory->getFunction()));

  // Set up the bitvector of elements being demanded by the load.
  SmallBitVector RequiredElts(NumMemorySubElements);
  RequiredElts.set(FirstElt, FirstElt + NumLoadSubElements);

  AvailableValues.resize(NumMemorySubElements);

  // Find out if we have any available values.  If no bits are demanded, we
  // trivially succeed. This can happen when there is a load of an empty struct.
  if (NumLoadSubElements != 0
      && !computeAvailableValues(
        Inst, FirstElt, NumLoadSubElements, RequiredElts, AvailableValues))
    return std::nullopt;

  return std::make_pair(LoadTy, FirstElt);
}

// This function takes in the current (potentially uninitialized) available
// values for theMemory and for the subset of AvailableValues corresponding to
// \p address either:
//
// 1. If uninitialized, optionally initialize the available value with a new
//    SILValue. It is optional since in certain cases, (for instance when
//    invalidating one just wants to skip empty available values).
//
// 2. Given an initialized value, either add the given instruction as an
//    insertion point or state that we have a conflict.
static inline void updateAvailableValuesHelper(
    SingleValueInstruction *theMemory, SILInstruction *inst, SILValue address,
    SmallBitVector &requiredElts, SmallVectorImpl<AvailableValue> &result,
    SmallBitVector &conflictingValues,
    function_ref<std::optional<AvailableValue>(unsigned)> defaultFunc,
    function_ref<bool(AvailableValue &, unsigned)> isSafeFunc) {
  auto &mod = theMemory->getModule();
  unsigned startSubElt = computeSubelement(address, theMemory);

  // TODO: Is this needed now?
  assert(startSubElt != ~0U && "Store within enum projection not handled");
  for (unsigned i : range(getNumSubElements(
           address->getType().getObjectType(), mod,
           TypeExpansionContext(*theMemory->getFunction())))) {
    // If this element is not required, don't fill it in.
    if (!requiredElts[startSubElt + i])
      continue;

    // At this point we know that we will either mark the value as conflicting
    // or give it a value.
    requiredElts[startSubElt + i] = false;

    // First see if we have an entry at all.
    auto &entry = result[startSubElt + i];

    // If we don't...
    if (!entry) {
      // and we are told to initialize it, do so.
      if (auto defaultValue = defaultFunc(i)) {
        entry = std::move(defaultValue.value());
      } else {
        // Otherwise, mark this as a conflicting value. There is some available
        // value here, we just do not know what it is at this point. This
        // ensures that if we visit a kill where we do not have an entry yet, we
        // properly invalidate our state.
        conflictingValues[startSubElt + i] = true;
      }
      continue;
    }

    // Check if our caller thinks that the value currently in entry is
    // compatible with \p inst. If not, mark the values as conflicting and
    // continue.
    if (!isSafeFunc(entry, i)) {
      conflictingValues[startSubElt + i] = true;
      continue;
    }

    // Otherwise, we found another insertion point for our available
    // value. Today this will always be a Store.
    entry.addInsertionPoint(cast<StoreInst>(inst));
  }
}

void AvailableValueDataflowContext::updateAvailableValues(
    SILInstruction *Inst, SmallBitVector &RequiredElts,
    SmallVectorImpl<AvailableValue> &Result,
    SmallBitVector &ConflictingValues) {

  // If we are visiting a load [take], it invalidates the underlying available
  // values.
  //
  // NOTE: Since we are always looking back from the instruction to promote,
  // when we attempt to promote the load [take] itself, we will never hit this
  // code since.
  if (auto *LI = dyn_cast<LoadInst>(Inst)) {
    // First see if this is a load inst that we are tracking.
    if (LoadTakeUses.count(LI)) {
      updateAvailableValuesHelper(
          TheMemory, LI, LI->getOperand(), RequiredElts, Result,
          ConflictingValues,
          /*default*/
          [](unsigned) -> std::optional<AvailableValue> {
            // We never initialize values. We only
            // want to invalidate.
            return std::nullopt;
          },
          /*isSafe*/
          [](AvailableValue &, unsigned) -> bool {
            // Always assume values conflict.
            return false;
          });
      return;
    }
  }

  // Handle store.
  if (auto *SI = dyn_cast<StoreInst>(Inst)) {
    updateAvailableValuesHelper(
        TheMemory, SI, SI->getDest(), RequiredElts, Result, ConflictingValues,
        /*default*/
        [&](unsigned ResultOffset) -> std::optional<AvailableValue> {
          std::optional<AvailableValue> Result;
          Result.emplace(SI->getSrc(), ResultOffset, SI);
          return Result;
        },
        /*isSafe*/
        [&](AvailableValue &Entry, unsigned ResultOffset) -> bool {
          // TODO: This is /really/, /really/, conservative. This basically
          // means that if we do not have an identical store, we will not
          // promote.
          return Entry.getValue() == SI->getSrc() &&
                 Entry.getSubElementNumber() == ResultOffset;
        });
    return;
  }

  // If we got here from an apply, we must either be initializing the element
  // via an @out parameter or we are trying to model an invalidating load of the
  // value (e.x.: indirect_in, indirect_inout).

  // If we get here with a copy_addr, we must either be storing into the element
  // or tracking some sort of take of the src. First check if we are taking (in
  // which case, we just track invalidation of src) and continue. Otherwise we
  // must be storing into the copy_addr so see which loaded subelements are
  // being used, and if so, explode the copy_addr to its individual pieces.
  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    // If we have a load take use, we must be tracking a store of CAI.
    if (LoadTakeUses.count(CAI)) {
      updateAvailableValuesHelper(
          TheMemory, CAI, CAI->getSrc(), RequiredElts, Result,
          ConflictingValues,
          /*default*/
          [](unsigned) -> std::optional<AvailableValue> {
            // We never give values default initialized
            // values. We only want to invalidate.
            return std::nullopt;
          },
          /*isSafe*/
          [](AvailableValue &, unsigned) -> bool {
            // Always assume values conflict.
            return false;
          });
      return;
    }

    unsigned StartSubElt = computeSubelement(CAI->getDest(), TheMemory);
    assert(StartSubElt != ~0U && "Store within enum projection not handled");
    SILType ValTy = CAI->getDest()->getType();

    bool AnyRequired = false;
    for (unsigned i : range(getNumSubElements(
             ValTy, getModule(), TypeExpansionContext(*CAI->getFunction())))) {
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
    if (CAI->getSrc()->getType().isLoadable(*CAI->getFunction())) {
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
  // to see if the store, escape, or load [take] is before or after the load. If
  // it is before, check to see if it produces the value we are looking for.
  bool shouldCheckBlock =
      HasLocalDefinition.get(BB) || HasLocalKill.get(BB);
  if (shouldCheckBlock) {
    for (SILBasicBlock::iterator BBI = StartingFrom; BBI != BB->begin();) {
      SILInstruction *TheInst = &*std::prev(BBI);
      // If this instruction is unrelated to the element, ignore it.
      if (!NonLoadUses.count(TheInst) && !LoadTakeUses.count(TheInst)) {
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

  SILFunction *F = CAI->getFunction();
  auto &TL = F->getTypeLowering(ValTy);

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
  LoadTakeUses.erase(CAI);
  // NOTE: We do not need to update HasLocalKill since the copy_addr
  // and the loads/stores will have the same parent block.

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
        if (auto *LI = dyn_cast<LoadInst>(NewInst)) {
          if (LI->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
            LoadTakeUses.insert(LI);
            HasLocalKill.set(LI->getParent());
          }
        }
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
  deleter.forceDelete(CAI);
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
//                               Optimize loads
//===----------------------------------------------------------------------===//

static SILType getMemoryType(AllocationInst *memory) {
  // Compute the type of the memory object.
  if (auto *abi = dyn_cast<AllocBoxInst>(memory)) {
    assert(abi->getBoxType()->getLayout()->getFields().size() == 1 &&
           "optimizing multi-field boxes not implemented");
    return getSILBoxFieldType(TypeExpansionContext(*abi->getFunction()),
                              abi->getBoxType(), abi->getModule().Types, 0);
  }

  assert(isa<AllocStackInst>(memory));
  return cast<AllocStackInst>(memory)->getElementType();
}

namespace {

/// This performs load promotion and deletes synthesized allocations if all
/// loads can be removed.
class OptimizeAllocLoads {

  SILModule &Module;

  /// This is either an alloc_box or alloc_stack instruction.
  AllocationInst *TheMemory;

  /// This is the SILType of the memory object.
  SILType MemoryType;

  /// The number of primitive subelements across all elements of this memory
  /// value.
  unsigned NumMemorySubElements;

  SmallVectorImpl<PMOMemoryUse> &Uses;

  DeadEndBlocks &deadEndBlocks;

  InstructionDeleter &deleter;

  /// A structure that we use to compute our available values.
  AvailableValueDataflowContext DataflowContext;

public:
  OptimizeAllocLoads(AllocationInst *memory,
                     SmallVectorImpl<PMOMemoryUse> &uses,
                     DeadEndBlocks &deadEndBlocks,
                     InstructionDeleter &deleter)
      : Module(memory->getModule()), TheMemory(memory),
        MemoryType(getMemoryType(memory)),
        NumMemorySubElements(getNumSubElements(
            MemoryType, Module, TypeExpansionContext(*memory->getFunction()))),
        Uses(uses), deadEndBlocks(deadEndBlocks), deleter(deleter),
        DataflowContext(TheMemory, NumMemorySubElements, uses, deleter) {}

  bool optimize();

private:
  bool promoteLoadCopy(LoadInst *li);
  bool promoteLoadBorrow(LoadBorrowInst *lbi);
  bool promoteCopyAddr(CopyAddrInst *cai);
};

} // end anonymous namespace

/// If we are able to optimize \p Inst, return the source address that
/// instruction is loading from. If we can not optimize \p Inst, then just
/// return an empty SILValue.
static SILValue tryFindSrcAddrForLoad(SILInstruction *i) {
  // We can always promote a load_borrow.
  if (auto *lbi = dyn_cast<LoadBorrowInst>(i))
    return lbi->getOperand();

  // We only handle load [copy], load [trivial], load and copy_addr right
  // now. Notably we do not support load [take] when promoting loads.
  if (auto *li = dyn_cast<LoadInst>(i))
    if (li->getOwnershipQualifier() != LoadOwnershipQualifier::Take)
      return li->getOperand();

  // If this is a CopyAddr, verify that the element type is loadable.  If not,
  // we can't explode to a load.
  auto *cai = dyn_cast<CopyAddrInst>(i);
  if (!cai || !cai->getSrc()->getType().isLoadable(*cai->getFunction()))
    return SILValue();
  return cai->getSrc();
}

/// At this point, we know that this element satisfies the definitive init
/// requirements, so we can try to promote loads to enable SSA-based dataflow
/// analysis.  We know that accesses to this element only access this element,
/// cross element accesses have been scalarized.
///
/// This returns true if the load has been removed from the program.
bool OptimizeAllocLoads::promoteLoadCopy(LoadInst *li) {
  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.

  // First attempt to find a source addr for our "load" instruction. If we fail
  // to find a valid value, just return.
  SILValue srcAddr = tryFindSrcAddrForLoad(li);
  if (!srcAddr)
    return false;

  SmallVector<AvailableValue, 8> availableValues;
  auto result = DataflowContext.computeAvailableValues(srcAddr, li, availableValues);
  if (!result.has_value())
    return false;

  SILType loadTy = result->first;
  unsigned firstElt = result->second;

  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller loads for any subelements that were
  // not available. We are "propagating" a +1 available value from the store
  // points.
  AvailableValueAggregator agg(li, availableValues, Uses, deadEndBlocks,
                               AvailableValueExpectedOwnership::Copy);
  SILValue newVal = agg.aggregateValues(loadTy, li->getOperand(), firstElt);

  LLVM_DEBUG(llvm::dbgs() << "  *** Promoting load: " << *li);
  LLVM_DEBUG(llvm::dbgs() << "      To value: " << *newVal);
  ++NumLoadPromoted;

  // If we did not have ownership, we did not insert extra copies at our stores,
  // so we can just RAUW and return.
  if (!li->getFunction()->hasOwnership()) {
    li->replaceAllUsesWith(newVal);
    SILValue addr = li->getOperand();
    deleter.forceDelete(li);
    if (auto *addrI = addr->getDefiningInstruction())
      deleter.deleteIfDead(addrI);
    return true;
  }

  // If we inserted any copies, we created the copies at our stores. We know
  // that in our load block, we will reform the aggregate as appropriate at the
  // load implying that the value /must/ be fully consumed. If we promoted a +0
  // value, we created dominating destroys along those paths. Thus any leaking
  // blocks that we may have can be found by performing a linear lifetime check
  // over all copies that we found using the load as the "consuming uses" (just
  // for the purposes of identifying the consuming block).
  agg.fixupOwnership(li, newVal);

  // Now that we have fixed up all of our missing destroys, insert the copy
  // value for our actual load and RAUW.
  newVal = SILBuilderWithScope(li).emitCopyValueOperation(li->getLoc(), newVal);

  li->replaceAllUsesWith(newVal);
  SILValue addr = li->getOperand();
  deleter.forceDelete(li);
  if (auto *addrI = addr->getDefiningInstruction())
    deleter.deleteIfDead(addrI);
  return true;
}

bool OptimizeAllocLoads::promoteCopyAddr(CopyAddrInst *cai) {
  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.

  // First attempt to find a source addr for our "load" instruction. If we fail
  // to find a valid value, just return.
  SILValue srcAddr = tryFindSrcAddrForLoad(cai);
  if (!srcAddr)
    return false;

  SmallVector<AvailableValue, 8> availableValues;
  auto result = DataflowContext.computeAvailableValues(srcAddr, cai,
                                                       availableValues);
  if (!result.has_value())
    return false;

  // Ok, we have some available values.  If we have a copy_addr, explode it now,
  // exposing the load operation within it.  Subsequent optimization passes will
  // see the load and propagate the available values into it.
  DataflowContext.explodeCopyAddr(cai);

  // This is removing the copy_addr, but explodeCopyAddr takes care of
  // removing the instruction from Uses for us, so we return false.
  return false;
}

/// At this point, we know that this element satisfies the definitive init
/// requirements, so we can try to promote loads to enable SSA-based dataflow
/// analysis.  We know that accesses to this element only access this element,
/// cross element accesses have been scalarized.
///
/// This returns true if the load has been removed from the program.
bool OptimizeAllocLoads::promoteLoadBorrow(LoadBorrowInst *lbi) {
  // Note that we intentionally don't support forwarding of weak pointers,
  // because the underlying value may drop be deallocated at any time.  We would
  // have to prove that something in this function is holding the weak value
  // live across the promoted region and that isn't desired for a stable
  // diagnostics pass this like one.

  // First attempt to find a source addr for our "load" instruction. If we fail
  // to find a valid value, just return.
  SILValue srcAddr = tryFindSrcAddrForLoad(lbi);
  if (!srcAddr)
    return false;

  SmallVector<AvailableValue, 8> availableValues;
  auto result = DataflowContext.computeAvailableValues(srcAddr, lbi,
                                                       availableValues);
  if (!result.has_value())
    return false;

  // Bail if the load_borrow has reborrows. In this case it's not so easy to
  // find the insertion points for the destroys.
  if (!lbi->getUsersOfType<BranchInst>().empty()) {
    return false;
  }

  ++NumLoadPromoted;

  SILType loadTy = result->first;
  unsigned firstElt = result->second;

  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller loads for any subelements that were
  // not available. We are "propagating" a +1 available value from the store
  // points.
  AvailableValueAggregator agg(lbi, availableValues, Uses, deadEndBlocks,
                               AvailableValueExpectedOwnership::Borrow);
  SILValue newVal = agg.aggregateValues(loadTy, lbi->getOperand(), firstElt);

  LLVM_DEBUG(llvm::dbgs() << "  *** Promoting load: " << *lbi);
  LLVM_DEBUG(llvm::dbgs() << "      To value: " << *newVal);

  // If we inserted any copies, we created the copies at our
  // stores. We know that in our load block, we will reform the
  // aggregate as appropriate, will borrow the value there and give us
  // a whole pristine new value. Now in this routine, we go through
  // all of the copies and phis that we inserted and ensure that:
  //
  // 1. Phis are always strongly control equivalent to the copies that
  //    produced their incoming values.
  //
  // 2. All intermediate copies are properly lifetime extended to the
  //    load block and all leaking blocks are filled in as appropriate
  //    with destroy_values.
  agg.fixupOwnership(lbi, newVal);

  // Now that we have fixed up the lifetimes of all of our incoming copies so
  // that they are alive over the load point, copy, borrow newVal and insert
  // destroy_value after the end_borrow and then RAUW.
  SILBuilderWithScope builder(lbi);
  SILValue copiedVal = builder.emitCopyValueOperation(lbi->getLoc(), newVal);
  newVal = builder.createBeginBorrow(lbi->getLoc(), copiedVal);

  for (auto *ebi : lbi->getUsersOfType<EndBorrowInst>()) {
    auto next = std::next(ebi->getIterator());
    SILBuilderWithScope(next).emitDestroyValueOperation(ebi->getLoc(),
                                                        copiedVal);
  }

  lbi->replaceAllUsesWith(newVal);
  SILValue addr = lbi->getOperand();
  deleter.forceDelete(lbi);
  if (auto *addrI = addr->getDefiningInstruction())
    deleter.deleteIfDead(addrI);
  return true;
}

bool OptimizeAllocLoads::optimize() {
  bool changed = false;

  // If we've successfully checked all of the definitive initialization
  // requirements, try to promote loads.  This can explode copy_addrs, so the
  // use list may change size.
  for (unsigned i = 0; i != Uses.size(); ++i) {
    auto &use = Uses[i];
    // Ignore entries for instructions that got expanded along the way.
    if (use.Inst && use.Kind == PMOUseKind::Load) {
      if (auto *cai = dyn_cast<CopyAddrInst>(use.Inst)) {
        if (promoteCopyAddr(cai)) {
          Uses[i].Inst = nullptr; // remove entry if load got deleted.
          changed = true;
        }
        continue;
      }

      if (auto *lbi = dyn_cast<LoadBorrowInst>(use.Inst)) {
        if (promoteLoadBorrow(lbi)) {
          Uses[i].Inst = nullptr; // remove entry if load got deleted.
          changed = true;
        }
        continue;
      }

      if (auto *li = dyn_cast<LoadInst>(use.Inst)) {
        if (promoteLoadCopy(li)) {
          Uses[i].Inst = nullptr; // remove entry if load got deleted.
          changed = true;
        }
        continue;
      }
    }
  }

  return changed;
}

//===----------------------------------------------------------------------===//
//                         Optimize dead allocation:
//                         Fully promote each access
//===----------------------------------------------------------------------===//

namespace {

class PromotableInstructions {
  // All promotable instructions share a vector of available values.
  SmallVectorImpl<AvailableValue> &allAvailableValues;

  SmallVector<SILInstruction *> promotableInsts;
  SmallVector<std::pair<unsigned, unsigned>, 8> availableValueOffsets;

public:
  PromotableInstructions(SmallVectorImpl<AvailableValue> &allAvailableValues)
      : allAvailableValues(allAvailableValues) {}

  unsigned size() const { return promotableInsts.size(); }

  void push(SILInstruction *instruction) {
    promotableInsts.push_back(instruction);
  }

  // Available values must be initialized in the same order that the
  // instructions are pushed. Return the instruction's index.
  unsigned
  initializeAvailableValues(SILInstruction *instruction,
                            SmallVectorImpl<AvailableValue> &&availableValues) {

    unsigned nextInstIdx = availableValueOffsets.size();
    assert(instruction == promotableInsts[nextInstIdx]);

    unsigned startOffset = allAvailableValues.size();
    unsigned endOffset = startOffset + availableValues.size();
    availableValueOffsets.push_back({startOffset, endOffset});
    std::move(availableValues.begin(), availableValues.end(),
              std::back_inserter(allAvailableValues));
    return nextInstIdx;
  }

  ArrayRef<SILInstruction *> instructions() const { return promotableInsts; }

  ArrayRef<AvailableValue> availableValues(unsigned index) {
    return mutableAvailableValues(index);
  }

  MutableArrayRef<AvailableValue> mutableAvailableValues(unsigned index) {
    unsigned startOffset, endOffset;
    std::tie(startOffset, endOffset) = availableValueOffsets[index];
    return {allAvailableValues.begin() + startOffset,
            allAvailableValues.begin() + endOffset};
  }

#ifndef NDEBUG
  void verify() {
    for (unsigned i : range(promotableInsts.size())) {
      promotableInsts[i]->verifyOperandOwnership();
      assert(!availableValues(i).empty()
             && "Value without any available values?!");
    }
  }
#endif
};

} // end anonymous namespace

namespace {

struct Promotions {
  SmallVector<AvailableValue, 32> allAvailableValues;
  PromotableInstructions loadTakes;
  PromotableInstructions destroys;

  Promotions()
      : loadTakes(allAvailableValues), destroys(allAvailableValues) {}

#ifndef NDEBUG
  void verify() {
    loadTakes.verify();
    destroys.verify();
  }
#endif
};

} // end anonymous namespace

namespace {

/// This performs load promotion and deletes synthesized allocations if all
/// loads can be removed.
class OptimizeDeadAlloc {

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

  InstructionDeleter &deleter;

  DominanceInfo *domInfo;

  /// A structure that we use to compute our available values.
  AvailableValueDataflowContext DataflowContext;

  Promotions promotions;

  SmallBlotSetVector<SILValue, 32> valuesNeedingLifetimeCompletion;

public:
  SILFunction *getFunction() const { return TheMemory->getFunction(); }

  bool isTrivial() const { return MemoryType.isTrivial(getFunction()); }

  OptimizeDeadAlloc(AllocationInst *memory,
                    SmallVectorImpl<PMOMemoryUse> &uses,
                    SmallVectorImpl<SILInstruction *> &releases,
                    DeadEndBlocks &deadEndBlocks, InstructionDeleter &deleter,
                    DominanceInfo *domInfo)
      : Module(memory->getModule()), TheMemory(memory),
        MemoryType(getMemoryType(memory)),
        NumMemorySubElements(getNumSubElements(
            MemoryType, Module, TypeExpansionContext(*memory->getFunction()))),
        Uses(uses), Releases(releases), deadEndBlocks(deadEndBlocks),
        deleter(deleter), domInfo(domInfo),
        DataflowContext(TheMemory, NumMemorySubElements, uses, deleter) {}

  /// If the allocation is an autogenerated allocation that is only stored to
  /// (after load promotion) then remove it completely.
  bool tryToRemoveDeadAllocation();

private:
  SILInstruction *collectUsesForPromotion();

  /// Return true if a load [take] or destroy_addr can be promoted. If so, this
  /// initializes the available values in promotions.
  bool canPromoteTake(SILInstruction *i,
                      PromotableInstructions &promotableInsts);

  /// Promote a load take cleaning up everything except for RAUWing the
  /// instruction with the aggregated result. The routine returns the new
  /// aggregated result to the caller and expects the caller to eventually RAUW
  /// \p inst with the return value. The reason why we do this is to allow for
  /// the caller to work around invalidation issues by not deleting the load
  /// [take] until after all load [take] have been cleaned up.
  ///
  /// \returns the value that the caller will RAUW with \p inst.
  SILValue promoteLoadTake(LoadInst *inst,
                           ArrayRef<AvailableValue> availableValues);
  void promoteDestroyAddr(DestroyAddrInst *dai,
                          ArrayRef<AvailableValue> availableValues);

  void removeDeadAllocation();
};

} // end anonymous namespace

// We don't want to remove allocations that are required for useful debug
// information at -O0.  As such, we only remove allocations if:
//
// 1. They are in a transparent function.
// 2. They are in a normal function, but didn't come from a VarDecl, or came
//    from one that was autogenerated or inlined from a transparent function.
static bool isRemovableAutogeneratedAllocation(AllocationInst *TheMemory) {
  SILLocation loc = TheMemory->getLoc();
  return TheMemory->getFunction()->isTransparent() ||
         !loc.getAsASTNode<VarDecl>() || loc.isAutoGenerated() ||
         loc.is<MandatoryInlinedLocation>();
}

bool OptimizeDeadAlloc::tryToRemoveDeadAllocation() {
  assert(TheMemory->getFunction()->hasOwnership() &&
         "Can only eliminate dead allocations with ownership enabled");
  assert((isa<AllocBoxInst>(TheMemory) || isa<AllocStackInst>(TheMemory)) &&
         "Unhandled allocation case");

  if (!isRemovableAutogeneratedAllocation(TheMemory))
    return false;

  // Check the uses list to see if there are any non-store uses left over after
  // load promotion and other things PMO does.
  if (auto *badUser = collectUsesForPromotion()) {
    LLVM_DEBUG(llvm::dbgs() << "*** Failed to remove autogenerated alloc: "
                               "kept alive by: "
                            << *badUser);
    return false;
  }

  if (isTrivial()) {
    removeDeadAllocation();
    return true;
  }
  for (auto *load : promotions.loadTakes.instructions()) {
    if (!canPromoteTake(load, promotions.loadTakes))
      return false;
  }
  for (auto *destroy : promotions.destroys.instructions()) {
    if (!canPromoteTake(destroy, promotions.destroys))
      return false;
  }
  // Gather up all found available values before promoting anything so we can
  // fix up lifetimes later if we need to.
  for (auto pmoMemUse : Uses) {
    if (pmoMemUse.Inst && pmoMemUse.Kind == PMOUseKind::Initialization) {
      // Today if we promote, this is always a store, since we would have
      // blown up the copy_addr otherwise. Given that, always make sure we
      // clean up the src as appropriate after we optimize.
      auto *si = dyn_cast<StoreInst>(pmoMemUse.Inst);
      if (!si)
        return false;
      auto src = si->getSrc();

      // Bail if src has any uses that are forwarding unowned uses. This
      // allows us to know that we never have to deal with forwarding unowned
      // instructions like br. These are corner cases that complicate the
      // logic below.
      for (auto *use : src->getUses()) {
        if (use->getOperandOwnership() == OperandOwnership::ForwardingUnowned)
          return false;
      }
      valuesNeedingLifetimeCompletion.insert(src);
    }
  }
  removeDeadAllocation();
  return true;
}

// Collect all uses that require promotion before this allocation can be
// eliminated. Returns nullptr on success. Upon failure, return the first
// instruction corresponding to a use that cannot be promoted.
//
// Populates 'loadTakeList'.
SILInstruction *OptimizeDeadAlloc::collectUsesForPromotion() {
  for (auto &u : Uses) {
    // Ignore removed instructions.
    if (u.Inst == nullptr)
      continue;

    switch (u.Kind) {
    case PMOUseKind::Assign:
      // Until we can promote the value being destroyed by the assign, we can
      // not remove deallocations with such assigns.
      return u.Inst;
    case PMOUseKind::InitOrAssign:
      continue; // These don't prevent removal.
    case PMOUseKind::Load:
      // For now only handle takes from alloc_stack.
      //
      // TODO: It should be implementable, but it has not been needed yet.
      if (auto *li = dyn_cast<LoadInst>(u.Inst)) {
        if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Take) {
          promotions.loadTakes.push(li);
          continue;
        }
      }
      return u.Inst;
    case PMOUseKind::Initialization:
      if (!isa<ApplyInst>(u.Inst) &&
          // A copy_addr that is not a take affects the retain count
          // of the source.
          (!isa<CopyAddrInst>(u.Inst)
           || cast<CopyAddrInst>(u.Inst)->isTakeOfSrc())) {
        continue;
      }
      LLVM_FALLTHROUGH;
    case PMOUseKind::IndirectIn:
    case PMOUseKind::InOutUse:
    case PMOUseKind::Escape:
      return u.Inst; // These do prevent removal.
    }
  }
  // Ignore destroys of trivial values. They are destroy_value instructions
  // that only destroy the dead box itself.
  if (!isTrivial()) {
    // Non-trivial allocations require ownership cleanup. We only promote
    // alloc_stack in that case--all releases must be destroy_addr.
    for (auto *release : Releases) {
      // We stash all of the destroy_addr that we see.
      if (auto *dai = dyn_cast_or_null<DestroyAddrInst>(release)) {
        promotions.destroys.push(dai);
        continue;
      }
      return release;
    }
  }
  return nullptr;
}

/// Return true if we can promote the given destroy.
bool OptimizeDeadAlloc::canPromoteTake(
    SILInstruction *inst, PromotableInstructions &promotableInsts) {

  SILValue address = inst->getOperand(0);

  // We cannot promote destroys of address-only types, because we can't expose
  // the load.
  SILType loadTy = address->getType().getObjectType();
  if (loadTy.isAddressOnly(*inst->getFunction()))
    return false;

  // If the box has escaped at this instruction, we can't safely promote the
  // load.
  if (DataflowContext.hasEscapedAt(inst))
    return false;

  // Compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned firstElt = computeSubelement(address, TheMemory);
  assert(firstElt != ~0U && "destroy within enum projection is not valid");
  auto expansionContext = TypeExpansionContext(*inst->getFunction());
  unsigned numLoadSubElements =
      getNumSubElements(loadTy, Module, expansionContext);

  // Find out if we have any available values.  If no bits are demanded, we
  // trivially succeed. This can happen when there is a load of an empty struct.
  if (numLoadSubElements == 0)
    return true;

  // Set up the bitvector of elements being demanded by the load.
  SmallBitVector requiredElts(NumMemorySubElements);
  requiredElts.set(firstElt, firstElt + numLoadSubElements);

  // Compute our available values. If we do not have any available values,
  // return false. We have nothing further to do.
  SmallVector<AvailableValue, 8> availableValues;
  availableValues.resize(NumMemorySubElements);
  if (!DataflowContext.computeAvailableValues(
          inst, firstElt, numLoadSubElements, requiredElts, availableValues))
    return false;

  // Now check that we can perform a take upon our available values. This
  // implies today that our value is fully available. If the value is not fully
  // available, we would need to split stores to promote this destroy_addr. We
  // do not support that yet.
  AvailableValueAggregator agg(inst, availableValues, Uses, deadEndBlocks,
                               AvailableValueExpectedOwnership::Take);
  if (!agg.canTake(loadTy, firstElt))
    return false;

  // As a final check, make sure that we have an available value for each value,
  // if not bail.
  for (const auto &av : availableValues)
    if (!av.Value)
      return false;

  // Ok, we can promote this destroy_addr... move the temporary lists contents
  // into the final AvailableValues list.
  promotableInsts.initializeAvailableValues(inst, std::move(availableValues));

  return true;
}

void OptimizeDeadAlloc::removeDeadAllocation() {
  // If our memory is trivially typed, we can just remove it without needing to
  // consider if the stored value needs to be destroyed. So at this point,
  // delete the memory!
  if (isTrivial()) {
    LLVM_DEBUG(llvm::dbgs() << "*** Removing autogenerated trivial allocation: "
                            << *TheMemory);

    // If it is safe to remove, do it.  Recursively remove all instructions
    // hanging off the allocation instruction, then return success.  Let the
    // caller remove the allocation itself to avoid iterator invalidation.
    deleter.forceDeleteWithUsers(TheMemory);
    return;
  }

  // Since our load [take] may be available values for our
  // destroy_addr/load [take], we promote the destroy_addr first and then handle
  // load [take] with extra rigour later to handle that possibility.
  for (auto idxVal : llvm::enumerate(promotions.destroys.instructions())) {
    auto *dai = cast<DestroyAddrInst>(idxVal.value());
    auto vals = promotions.destroys.availableValues(idxVal.index());
    promoteDestroyAddr(dai, vals);
    // We do not need to unset releases, since we are going to exit here.
  }

  llvm::SmallMapVector<LoadInst *, SILValue, 32> loadsToDelete;
  for (auto idxVal : llvm::enumerate(promotions.loadTakes.instructions())) {
    for (auto &availableVal :
         promotions.loadTakes.mutableAvailableValues(idxVal.index())) {
      auto *availableLoad = dyn_cast<LoadInst>(availableVal.Value);
      if (!availableLoad)
        continue;

      auto iter = loadsToDelete.find(availableLoad);
      if (iter == loadsToDelete.end())
        continue;

      SILValue newValue = iter->second;
      assert(newValue && "We should neer store a nil SILValue into this map");
      availableVal.Value = newValue;
    }

    auto *loadTake = cast<LoadInst>(idxVal.value());
    auto vals = promotions.loadTakes.availableValues(idxVal.index());
    SILValue result = promoteLoadTake(loadTake, vals);
    assert(result);

    // We need to erase liCast here before we erase it since a load [take] that
    // we are promoting could be an available value for another load
    // [take]. Consider the following SIL:
    //
    // %mem = alloc_stack
    // store %arg to [init] %mem
    // %0 = load [take] %mem
    // store %0 to [init] %mem
    // %1 = load [take] %mem
    // destroy_value %1
    // dealloc_stack %mem
    //
    // In such a case, we are going to delete %0 here, but %0 is an available
    // value for %1, so we will
    auto insertIter = loadsToDelete.insert({loadTake, result});
    valuesNeedingLifetimeCompletion.erase(loadTake);
    (void)insertIter;
    assert(insertIter.second && "loadTakeState doesn't have unique loads?!");
  }

  // Now that we have promoted all of our load [take], perform the actual
  // RAUW/removal.
  for (auto p : loadsToDelete) {
    LoadInst *li = p.first;
    SILValue newValue = p.second;
    li->replaceAllUsesWith(newValue);
    deleter.forceDelete(li);
  }

  LLVM_DEBUG(llvm::dbgs() << "*** Removing autogenerated non-trivial alloc: "
                          << *TheMemory);

  // If it is safe to remove, do it.  Recursively remove all instructions
  // hanging off the allocation instruction, then return success.
  deleter.forceDeleteWithUsers(TheMemory);

  // Now look at all of our available values and complete any of their
  // post-dominating consuming use sets. This can happen if we have an enum that
  // is known dynamically none along a path. This is dynamically correct, but
  // can not be represented in OSSA so we insert these destroys along said path.
  OSSALifetimeCompletion completion(TheMemory->getFunction(), domInfo,
                                    deadEndBlocks);

  while (!valuesNeedingLifetimeCompletion.empty()) {
    auto optV = valuesNeedingLifetimeCompletion.pop_back_val();
    if (!optV)
      continue;
    SILValue v = *optV;
    // Lexical enums can have incomplete lifetimes in non payload paths that
    // don't end in unreachable. Force their lifetime to end immediately after
    // the last use instead.
    auto boundary = v->getType().isOrHasEnum()
                        ? OSSALifetimeCompletion::Boundary::Liveness
                        : OSSALifetimeCompletion::Boundary::Availability;
    LLVM_DEBUG(llvm::dbgs() << "Completing lifetime of: ");
    LLVM_DEBUG(v->dump());
    completion.completeOSSALifetime(v, boundary);
  }
}

SILValue
OptimizeDeadAlloc::promoteLoadTake(LoadInst *li,
                                   ArrayRef<AvailableValue> availableValues) {
  assert(li->getOwnershipQualifier() == LoadOwnershipQualifier::Take &&
         "load [copy], load [trivial], load should be handled by "
         "promoteLoadCopy");
  SILValue address = li->getOperand();
  SILType loadTy = address->getType().getObjectType();

  // Compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned firstElt = computeSubelement(address, TheMemory);

  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller) loads for any subelements that were
  // not available.
  AvailableValueAggregator agg(li, availableValues, Uses, deadEndBlocks,
                               AvailableValueExpectedOwnership::Take);
  SILValue newVal = agg.aggregateValues(loadTy, address, firstElt);
  assert(newVal);

  ++NumLoadTakePromoted;

  LLVM_DEBUG(llvm::dbgs() << "  *** Promoting load_take: " << *li);
  LLVM_DEBUG(llvm::dbgs() << "      To value: " << *newVal);

  // Our parent RAUWs with newVal/erases li.
  return newVal;
}

// DestroyAddr is a composed operation merging load [take] + destroy_value.  If
// the implicit load's value is available, explode it.
//
// NOTE: We only do this if we have a fully available value.
//
// Note that we handle the general case of a destroy_addr of a piece of the
// memory object, not just destroy_addrs of the entire thing.
void OptimizeDeadAlloc::promoteDestroyAddr(
    DestroyAddrInst *dai, ArrayRef<AvailableValue> availableValues) {
  SILValue address = dai->getOperand();
  SILType loadTy = address->getType().getObjectType();

  // Compute the access path down to the field so we can determine precise
  // def/use behavior.
  unsigned firstElt = computeSubelement(address, TheMemory);

  // Aggregate together all of the subelements into something that has the same
  // type as the load did, and emit smaller) loads for any subelements that were
  // not available.
  AvailableValueAggregator agg(dai, availableValues, Uses, deadEndBlocks,
                               AvailableValueExpectedOwnership::Take);
  SILValue newVal = agg.aggregateValues(loadTy, address, firstElt);

  ++NumDestroyAddrPromoted;

  LLVM_DEBUG(llvm::dbgs() << "  *** Promoting destroy_addr: " << *dai);
  LLVM_DEBUG(llvm::dbgs() << "      To value: " << *newVal);

  SILBuilderWithScope(dai).emitDestroyValueOperation(dai->getLoc(), newVal);
  deleter.forceDelete(dai);
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

  // Do not perform this on move only values since we introduce copies to
  // promote things.
  if (getMemoryType(alloc).isMoveOnly())
    return nullptr;

  // Otherwise we are good to go. Lets try to optimize this memory!
  return alloc;
}

bool swift::optimizeMemoryAccesses(SILFunction *fn) {
  bool changed = false;
  DeadEndBlocks deadEndBlocks(fn);

  InstructionDeleter deleter;
  for (auto &bb : *fn) {
    for (SILInstruction &inst : bb.deletableInstructions()) {
      // First see if i is an allocation that we can optimize. If not, skip it.
      AllocationInst *alloc = getOptimizableAllocation(&inst);
      if (!alloc) {
        continue;
      }

      LLVM_DEBUG(llvm::dbgs()
                 << "*** PMO Optimize Memory Accesses looking at: " << *alloc);
      PMOMemoryObjectInfo memInfo(alloc);

      // Set up the datastructure used to collect the uses of the allocation.
      SmallVector<PMOMemoryUse, 16> uses;

      // Walk the use list of the pointer, collecting them. If we are not able
      // to optimize, skip this value. *NOTE* We may still scalarize values
      // inside the value.
      if (!collectPMOElementUsesFrom(memInfo, uses)) {
        // Avoid advancing this iterator until after collectPMOElementUsesFrom()
        // runs. It creates and deletes instructions other than alloc.
        continue;
      }
      OptimizeAllocLoads optimizeAllocLoads(alloc, uses, deadEndBlocks,
                                            deleter);
      changed |= optimizeAllocLoads.optimize();

      // Move onto the next instruction. We know this is safe since we do not
      // eliminate allocations here.
    }
  }

  return changed;
}

bool swift::eliminateDeadAllocations(SILFunction *fn, DominanceInfo *domInfo) {
  if (!fn->hasOwnership())
    return false;

  bool changed = false;
  DeadEndBlocks deadEndBlocks(fn);

  for (auto &bb : *fn) {
    InstructionDeleter deleter;
    for (SILInstruction &inst : bb.deletableInstructions()) {
      // First see if i is an allocation that we can optimize. If not, skip it.
      AllocationInst *alloc = getOptimizableAllocation(&inst);
      if (!alloc) {
        continue;
      }

      LLVM_DEBUG(llvm::dbgs()
                 << "*** PMO Dead Allocation Elimination looking at: "
                 << *alloc);
      PMOMemoryObjectInfo memInfo(alloc);

      // Set up the datastructure used to collect the uses of the allocation.
      SmallVector<PMOMemoryUse, 16> uses;
      SmallVector<SILInstruction *, 4> destroys;

      // Walk the use list of the pointer, collecting them. If we are not able
      // to optimize, skip this value. *NOTE* We may still scalarize values
      // inside the value.
      if (!collectPMOElementUsesAndDestroysFrom(memInfo, uses, destroys)) {
        continue;
      }
      OptimizeDeadAlloc optimizeDeadAlloc(alloc, uses, destroys, deadEndBlocks,
                                          deleter, domInfo);
      if (optimizeDeadAlloc.tryToRemoveDeadAllocation()) {
        deleter.cleanupDeadInstructions();
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
    auto *func = getFunction();
    LLVM_DEBUG(llvm::dbgs() << "Looking at: " << func->getName() << "\n");
    // TODO: Can we invalidate here just instructions?
    if (optimizeMemoryAccesses(func))
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
  }
};

class PredictableDeadAllocationElimination : public SILFunctionTransform {
  void run() override {
    auto *func = getFunction();
    LLVM_DEBUG(llvm::dbgs() << "Looking at: " << func->getName() << "\n");
    auto *da = getAnalysis<DominanceAnalysis>();
    // If we are already canonical or do not have ownership, just bail.
    if (func->wasDeserializedCanonical() || !func->hasOwnership())
      return;
    if (eliminateDeadAllocations(func, da->get(func)))
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
