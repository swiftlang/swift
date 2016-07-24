//===--- LLVMMergeFunctions.cpp - Merge similar functions for swift -------===//
//
// This source file is part of the Swift.org open source project
// Licensed under Apache License v2.0 with Runtime Library Exception
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// See http://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//
//
// This pass looks for similar functions that are mergeable and folds them.
// The implementation is similar to LLVM's MergeFunctions pass. Instead of
// merging identical functions, it merges functions which only differ by a few
// constants in certain instructions.
// Currently this is very Swift specific in the sense that it's intended to
// merge specialized functions which only differ by loading different metadata
// pointers.
// TODO: It could make sense to generalize this pass and move it to LLVM.
//
// This pass should run after LLVM's MergeFunctions pass, because it works best
// if there are no _identical_ functions in the module.
// Note: it would also work for identical functions but could produce more
// code overhead than the LLVM pass.
//
// There is a big TODO: currently there is a large code overlap in this file
// and the LLVM pass, mainly the IR comparison functions. This should be
// factored out into a separate utility and used by both passes.
//
//===----------------------------------------------------------------------===//

#include "swift/LLVMPasses/Passes.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

using namespace llvm;
using namespace swift;

#define DEBUG_TYPE "swift-mergefunc"

STATISTIC(NumSwiftFunctionsMerged, "Number of functions merged");
STATISTIC(NumSwiftThunksWritten, "Number of thunks generated");

static cl::opt<unsigned> NumFunctionsForSanityCheck(
    "swiftmergefunc-sanity",
    cl::desc("How many functions in module could be used for "
             "SwiftMergeFunctions pass sanity check. "
             "'0' disables this check. Works only with '-debug' key."),
    cl::init(0), cl::Hidden);

static cl::opt<unsigned> FunctionMergeThreshold(
    "swiftmergefunc-threshold",
    cl::desc("Functions larger than the threshold are considered for merging."
             "'0' disables function merging at all."),
    cl::init(30), cl::Hidden);

namespace {

// TODO: the following code (GlobalNumberState, FunctionComparator) is copied
// from LLVM's MergeFunctions pass. This code should be shared and not copied.

/// GlobalNumberState assigns an integer to each global value in the program,
/// which is used by the comparison routine to order references to globals. This
/// state must be preserved throughout the pass, because Functions and other
/// globals need to maintain their relative order. Globals are assigned a number
/// when they are first visited. This order is deterministic, and so the
/// assigned numbers are as well. When two functions are merged, neither number
/// is updated. If the symbols are weak, this would be incorrect. If they are
/// strong, then one will be replaced at all references to the other, and so
/// direct callsites will now see one or the other symbol, and no update is
/// necessary. Note that if we were guaranteed unique names, we could just
/// compare those, but this would not work for stripped bitcodes or for those
/// few symbols without a name.
class GlobalNumberState {
  struct Config : ValueMapConfig<GlobalValue*> {
    enum { FollowRAUW = false };
  };
  // Each GlobalValue is mapped to an identifier. The Config ensures when RAUW
  // occurs, the mapping does not change. Tracking changes is unnecessary, and
  // also problematic for weak symbols (which may be overwritten).
  typedef ValueMap<GlobalValue *, uint64_t, Config> ValueNumberMap;
  ValueNumberMap GlobalNumbers;
  // The next unused serial number to assign to a global.
  uint64_t NextNumber;
  public:
    GlobalNumberState() : GlobalNumbers(), NextNumber(0) {}
    uint64_t getNumber(GlobalValue* Global) {
      ValueNumberMap::iterator MapIter;
      bool Inserted;
      std::tie(MapIter, Inserted) = GlobalNumbers.insert({Global, NextNumber});
      if (Inserted)
        NextNumber++;
      return MapIter->second;
    }
    void clear() {
      GlobalNumbers.clear();
    }
};

/// FunctionComparator - Compares two functions to determine whether or not
/// they will generate machine code with the same behaviour. DataLayout is
/// used if available. The comparator always fails conservatively (erring on the
/// side of claiming that two functions are different).
class FunctionComparator {
public:
  FunctionComparator(const Function *F1, const Function *F2,
                     GlobalNumberState* GN)
      : FnL(F1), FnR(F2), GlobalNumbers(GN) {}

  /// Test whether the two functions have equivalent behaviour.
  int compare();
  /// Hash a function. Equivalent functions will have the same hash, and unequal
  /// functions will have different hashes with high probability.
  typedef uint64_t FunctionHash;
  static FunctionHash functionHash(Function &);

private:
  /// Test whether two basic blocks have equivalent behaviour.
  int cmpBasicBlocks(const BasicBlock *BBL, const BasicBlock *BBR);

  /// Constants comparison.
  /// Its analog to lexicographical comparison between hypothetical numbers
  /// of next format:
  /// <bitcastability-trait><raw-bit-contents>
  ///
  /// 1. Bitcastability.
  /// Check whether L's type could be losslessly bitcasted to R's type.
  /// On this stage method, in case when lossless bitcast is not possible
  /// method returns -1 or 1, thus also defining which type is greater in
  /// context of bitcastability.
  /// Stage 0: If types are equal in terms of cmpTypes, then we can go straight
  ///          to the contents comparison.
  ///          If types differ, remember types comparison result and check
  ///          whether we still can bitcast types.
  /// Stage 1: Types that satisfies isFirstClassType conditions are always
  ///          greater then others.
  /// Stage 2: Vector is greater then non-vector.
  ///          If both types are vectors, then vector with greater bitwidth is
  ///          greater.
  ///          If both types are vectors with the same bitwidth, then types
  ///          are bitcastable, and we can skip other stages, and go to contents
  ///          comparison.
  /// Stage 3: Pointer types are greater than non-pointers. If both types are
  ///          pointers of the same address space - go to contents comparison.
  ///          Different address spaces: pointer with greater address space is
  ///          greater.
  /// Stage 4: Types are neither vectors, nor pointers. And they differ.
  ///          We don't know how to bitcast them. So, we better don't do it,
  ///          and return types comparison result (so it determines the
  ///          relationship among constants we don't know how to bitcast).
  ///
  /// Just for clearance, let's see how the set of constants could look
  /// on single dimension axis:
  ///
  /// [NFCT], [FCT, "others"], [FCT, pointers], [FCT, vectors]
  /// Where: NFCT - Not a FirstClassType
  ///        FCT - FirstClassTyp:
  ///
  /// 2. Compare raw contents.
  /// It ignores types on this stage and only compares bits from L and R.
  /// Returns 0, if L and R has equivalent contents.
  /// -1 or 1 if values are different.
  /// Pretty trivial:
  /// 2.1. If contents are numbers, compare numbers.
  ///    Ints with greater bitwidth are greater. Ints with same bitwidths
  ///    compared by their contents.
  /// 2.2. "And so on". Just to avoid discrepancies with comments
  /// perhaps it would be better to read the implementation itself.
  /// 3. And again about overall picture. Let's look back at how the ordered set
  /// of constants will look like:
  /// [NFCT], [FCT, "others"], [FCT, pointers], [FCT, vectors]
  ///
  /// Now look, what could be inside [FCT, "others"], for example:
  /// [FCT, "others"] =
  /// [
  ///   [double 0.1], [double 1.23],
  ///   [i32 1], [i32 2],
  ///   { double 1.0 },       ; StructTyID, NumElements = 1
  ///   { i32 1 },            ; StructTyID, NumElements = 1
  ///   { double 1, i32 1 },  ; StructTyID, NumElements = 2
  ///   { i32 1, double 1 }   ; StructTyID, NumElements = 2
  /// ]
  ///
  /// Let's explain the order. Float numbers will be less than integers, just
  /// because of cmpType terms: FloatTyID < IntegerTyID.
  /// Floats (with same fltSemantics) are sorted according to their value.
  /// Then you can see integers, and they are, like floats, which
  /// could be easy sorted among each others.
  /// The structures. Structures are grouped at the tail, again because of their
  /// TypeID: StructTyID > IntegerTyID > FloatTyID.
  /// Structures with greater number of elements are greater. Structures with
  /// greater elements going first are greater.
  /// The same logic with vectors, arrays and other possible complex types.
  ///
  /// Bitcastable constants.
  /// Let's assume, that some constant, belongs to some group of
  /// "so-called-equal" values with different types, and at the same time
  /// belongs to another group of constants with equal types
  /// and "really" equal values.
  ///
  /// Now, prove that this is impossible:
  ///
  /// If constant A with type TyA is bitcastable to B with type TyB, then:
  /// 1. All constants with equal types to TyA, are bitcastable to B. Since
  ///    those should be vectors (if TyA is vector), pointers
  ///    (if TyA is pointer), or else (if TyA equal to TyB), those types should
  ///    be equal to TyB.
  /// 2. All constants with non-equal, but bitcastable types to TyA, are
  ///    bitcastable to B.
  ///    Once again, just because we allow it to vectors and pointers only.
  ///    This statement could be expanded as below:
  /// 2.1. All vectors with equal bitwidth to vector A, has equal bitwidth to
  ///      vector B, and thus bitcastable to B as well.
  /// 2.2. All pointers of the same address space, no matter what they point to,
  ///      bitcastable. So if C is pointer, it could be bitcasted to A and to B.
  /// So any constant equal or bitcastable to A is equal or bitcastable to B.
  /// QED.
  ///
  /// In another words, for pointers and vectors, we ignore top-level type and
  /// look at their particular properties (bit-width for vectors, and
  /// address space for pointers).
  /// If these properties are equal - compare their contents.
  int cmpConstants(const Constant *L, const Constant *R) const;

  /// Compares two global values by number. Uses the GlobalNumbersState to
  /// identify the same globals across function calls.
  int cmpGlobalValues(GlobalValue *L, GlobalValue *R) const;

  /// Assign or look up previously assigned numbers for the two values, and
  /// return whether the numbers are equal. Numbers are assigned in the order
  /// visited.
  /// Comparison order:
  /// Stage 0: Value that is function itself is always greater then others.
  ///          If left and right values are references to their functions, then
  ///          they are equal.
  /// Stage 1: Constants are greater than non-constants.
  ///          If both left and right are constants, then the result of
  ///          cmpConstants is used as cmpValues result.
  /// Stage 2: InlineAsm instances are greater than others. If both left and
  ///          right are InlineAsm instances, InlineAsm* pointers casted to
  ///          integers and compared as numbers.
  /// Stage 3: For all other cases we compare order we meet these values in
  ///          their functions. If right value was met first during scanning,
  ///          then left value is greater.
  ///          In another words, we compare serial numbers, for more details
  ///          see comments for sn_mapL and sn_mapR.
  int cmpValues(const Value *L, const Value *R) const;

  /// Compare two Instructions for equivalence, similar to
  /// Instruction::isSameOperationAs but with modifications to the type
  /// comparison.
  /// Stages are listed in "most significant stage first" order:
  /// On each stage below, we do comparison between some left and right
  /// operation parts. If parts are non-equal, we assign parts comparison
  /// result to the operation comparison result and exit from method.
  /// Otherwise we proceed to the next stage.
  /// Stages:
  /// 1. Operations opcodes. Compared as numbers.
  /// 2. Number of operands.
  /// 3. Operation types. Compared with cmpType method.
  /// 4. Compare operation subclass optional data as stream of bytes:
  /// just convert it to integers and call cmpNumbers.
  /// 5. Compare in operation operand types with cmpType in
  /// most significant operand first order.
  /// 6. Last stage. Check operations for some specific attributes.
  /// For example, for Load it would be:
  /// 6.1.Load: volatile (as boolean flag)
  /// 6.2.Load: alignment (as integer numbers)
  /// 6.3.Load: synch-scope (as integer numbers)
  /// 6.4.Load: range metadata (as integer numbers)
  /// On this stage its better to see the code, since its not more than 10-15
  /// strings for particular instruction, and could change sometimes.
  int cmpOperations(const Instruction *L, const Instruction *R) const;

  int cmpOperands(const Instruction *L, const Instruction *R, unsigned opIdx);

  /// Compare two GEPs for equivalent pointer arithmetic.
  /// Parts to be compared for each comparison stage,
  /// most significant stage first:
  /// 1. Address space. As numbers.
  /// 2. Constant offset, (using GEPOperator::accumulateConstantOffset method).
  /// 3. Pointer operand type (using cmpType method).
  /// 4. Number of operands.
  /// 5. Compare operands, using cmpValues method.
  int cmpGEPs(const GEPOperator *GEPL, const GEPOperator *GEPR);
  int cmpGEPs(const GetElementPtrInst *GEPL, const GetElementPtrInst *GEPR) {
    return cmpGEPs(cast<GEPOperator>(GEPL), cast<GEPOperator>(GEPR));
  }

  /// cmpType - compares two types,
  /// defines total ordering among the types set.
  ///
  /// Return values:
  /// 0 if types are equal,
  /// -1 if Left is less than Right,
  /// +1 if Left is greater than Right.
  ///
  /// Description:
  /// Comparison is broken onto stages. Like in lexicographical comparison
  /// stage coming first has higher priority.
  /// On each explanation stage keep in mind total ordering properties.
  ///
  /// 0. Before comparison we coerce pointer types of 0 address space to
  /// integer.
  /// We also don't bother with same type at left and right, so
  /// just return 0 in this case.
  ///
  /// 1. If types are of different kind (different type IDs).
  ///    Return result of type IDs comparison, treating them as numbers.
  /// 2. If types are integers, check that they have the same width. If they
  /// are vectors, check that they have the same count and subtype.
  /// 3. Types have the same ID, so check whether they are one of:
  /// * Void
  /// * Float
  /// * Double
  /// * X86_FP80
  /// * FP128
  /// * PPC_FP128
  /// * Label
  /// * Metadata
  /// We can treat these types as equal whenever their IDs are same.
  /// 4. If Left and Right are pointers, return result of address space
  /// comparison (numbers comparison). We can treat pointer types of same
  /// address space as equal.
  /// 5. If types are complex.
  /// Then both Left and Right are to be expanded and their element types will
  /// be checked with the same way. If we get Res != 0 on some stage, return it.
  /// Otherwise return 0.
  /// 6. For all other cases put llvm_unreachable.
  int cmpTypes(Type *TyL, Type *TyR) const;

  int cmpNumbers(uint64_t L, uint64_t R) const;
  int cmpAPInts(const APInt &L, const APInt &R) const;
  int cmpAPFloats(const APFloat &L, const APFloat &R) const;
  int cmpInlineAsm(const InlineAsm *L, const InlineAsm *R) const;
  int cmpMem(StringRef L, StringRef R) const;
  int cmpAttrs(const AttributeSet L, const AttributeSet R) const;
  int cmpRangeMetadata(const MDNode* L, const MDNode* R) const;
  int cmpOperandBundlesSchema(const Instruction *L, const Instruction *R) const;

  // The two functions undergoing comparison.
  const Function *FnL, *FnR;

  /// Assign serial numbers to values from left function, and values from
  /// right function.
  /// Explanation:
  /// Being comparing functions we need to compare values we meet at left and
  /// right sides.
  /// Its easy to sort things out for external values. It just should be
  /// the same value at left and right.
  /// But for local values (those were introduced inside function body)
  /// we have to ensure they were introduced at exactly the same place,
  /// and plays the same role.
  /// Let's assign serial number to each value when we meet it first time.
  /// Values that were met at same place will be with same serial numbers.
  /// In this case it would be good to explain few points about values assigned
  /// to BBs and other ways of implementation (see below).
  ///
  /// 1. Safety of BB reordering.
  /// It's safe to change the order of BasicBlocks in function.
  /// Relationship with other functions and serial numbering will not be
  /// changed in this case.
  /// As follows from FunctionComparator::compare(), we do CFG walk: we start
  /// from the entry, and then take each terminator. So it doesn't matter how in
  /// fact BBs are ordered in function. And since cmpValues are called during
  /// this walk, the numbering depends only on how BBs located inside the CFG.
  /// So the answer is - yes. We will get the same numbering.
  ///
  /// 2. Impossibility to use dominance properties of values.
  /// If we compare two instruction operands: first is usage of local
  /// variable AL from function FL, and second is usage of local variable AR
  /// from FR, we could compare their origins and check whether they are
  /// defined at the same place.
  /// But, we are still not able to compare operands of PHI nodes, since those
  /// could be operands from further BBs we didn't scan yet.
  /// So it's impossible to use dominance properties in general.
  mutable DenseMap<const Value *, int> sn_mapL, sn_mapR;

  // The global state we will use
  GlobalNumberState* GlobalNumbers;
};

} // end anonymous namespace

int FunctionComparator::cmpNumbers(uint64_t L, uint64_t R) const {
  if (L < R) return -1;
  if (L > R) return 1;
  return 0;
}

int FunctionComparator::cmpAPInts(const APInt &L, const APInt &R) const {
  if (int Res = cmpNumbers(L.getBitWidth(), R.getBitWidth()))
    return Res;
  if (L.ugt(R)) return 1;
  if (R.ugt(L)) return -1;
  return 0;
}

int FunctionComparator::cmpAPFloats(const APFloat &L, const APFloat &R) const {
  // Floats are ordered first by semantics (i.e. float, double, half, etc.),
  // then by value interpreted as a bitstring (aka APInt).
  const fltSemantics &SL = L.getSemantics(), &SR = R.getSemantics();
  if (int Res = cmpNumbers(APFloat::semanticsPrecision(SL),
                           APFloat::semanticsPrecision(SR)))
    return Res;
  if (int Res = cmpNumbers(APFloat::semanticsMaxExponent(SL),
                           APFloat::semanticsMaxExponent(SR)))
    return Res;
  if (int Res = cmpNumbers(APFloat::semanticsMinExponent(SL),
                           APFloat::semanticsMinExponent(SR)))
    return Res;
  if (int Res = cmpNumbers(APFloat::semanticsSizeInBits(SL),
                           APFloat::semanticsSizeInBits(SR)))
    return Res;
  return cmpAPInts(L.bitcastToAPInt(), R.bitcastToAPInt());
}

int FunctionComparator::cmpMem(StringRef L, StringRef R) const {
  // Prevent heavy comparison, compare sizes first.
  if (int Res = cmpNumbers(L.size(), R.size()))
    return Res;

  // Compare strings lexicographically only when it is necessary: only when
  // strings are equal in size.
  return L.compare(R);
}

int FunctionComparator::cmpAttrs(const AttributeSet L,
                                 const AttributeSet R) const {
  if (int Res = cmpNumbers(L.getNumSlots(), R.getNumSlots()))
    return Res;

  for (unsigned i = 0, e = L.getNumSlots(); i != e; ++i) {
    AttributeSet::iterator LI = L.begin(i), LE = L.end(i), RI = R.begin(i),
                           RE = R.end(i);
    for (; LI != LE && RI != RE; ++LI, ++RI) {
      Attribute LA = *LI;
      Attribute RA = *RI;
      if (LA < RA)
        return -1;
      if (RA < LA)
        return 1;
    }
    if (LI != LE)
      return 1;
    if (RI != RE)
      return -1;
  }
  return 0;
}

int FunctionComparator::cmpRangeMetadata(const MDNode* L,
                                         const MDNode* R) const {
  if (L == R)
    return 0;
  if (!L)
    return -1;
  if (!R)
    return 1;
  // Range metadata is a sequence of numbers. Make sure they are the same
  // sequence. 
  // TODO: Note that as this is metadata, it is possible to drop and/or merge
  // this data when considering functions to merge. Thus this comparison would
  // return 0 (i.e. equivalent), but merging would become more complicated
  // because the ranges would need to be combined. It is not likely that
  // functions differ ONLY in this metadata if they are actually the same
  // function semantically.
  if (int Res = cmpNumbers(L->getNumOperands(), R->getNumOperands()))
    return Res;
  for (size_t I = 0; I < L->getNumOperands(); ++I) {
    ConstantInt* LLow = mdconst::extract<ConstantInt>(L->getOperand(I));
    ConstantInt* RLow = mdconst::extract<ConstantInt>(R->getOperand(I));
    if (int Res = cmpAPInts(LLow->getValue(), RLow->getValue()))
      return Res;
  }
  return 0;
}

int FunctionComparator::cmpOperandBundlesSchema(const Instruction *L,
                                                const Instruction *R) const {
  ImmutableCallSite LCS(L);
  ImmutableCallSite RCS(R);

  assert(LCS && RCS && "Must be calls or invokes!");
  assert(LCS.isCall() == RCS.isCall() && "Can't compare otherwise!");

  if (int Res =
          cmpNumbers(LCS.getNumOperandBundles(), RCS.getNumOperandBundles()))
    return Res;

  for (unsigned i = 0, e = LCS.getNumOperandBundles(); i != e; ++i) {
    auto OBL = LCS.getOperandBundleAt(i);
    auto OBR = RCS.getOperandBundleAt(i);

    if (int Res = OBL.getTagName().compare(OBR.getTagName()))
      return Res;

    if (int Res = cmpNumbers(OBL.Inputs.size(), OBR.Inputs.size()))
      return Res;
  }

  return 0;
}

/// Constants comparison:
/// 1. Check whether type of L constant could be losslessly bitcasted to R
/// type.
/// 2. Compare constant contents.
/// For more details see declaration comments.
int FunctionComparator::cmpConstants(const Constant *L,
                                     const Constant *R) const {

  Type *TyL = L->getType();
  Type *TyR = R->getType();

  // Check whether types are bitcastable. This part is just re-factored
  // Type::canLosslesslyBitCastTo method, but instead of returning true/false,
  // we also pack into result which type is "less" for us.
  int TypesRes = cmpTypes(TyL, TyR);
  if (TypesRes != 0) {
    // Types are different, but check whether we can bitcast them.
    if (!TyL->isFirstClassType()) {
      if (TyR->isFirstClassType())
        return -1;
      // Neither TyL nor TyR are values of first class type. Return the result
      // of comparing the types
      return TypesRes;
    }
    if (!TyR->isFirstClassType()) {
      if (TyL->isFirstClassType())
        return 1;
      return TypesRes;
    }

    // Vector -> Vector conversions are always lossless if the two vector types
    // have the same size, otherwise not.
    unsigned TyLWidth = 0;
    unsigned TyRWidth = 0;

    if (auto *VecTyL = dyn_cast<VectorType>(TyL))
      TyLWidth = VecTyL->getBitWidth();
    if (auto *VecTyR = dyn_cast<VectorType>(TyR))
      TyRWidth = VecTyR->getBitWidth();

    if (TyLWidth != TyRWidth)
      return cmpNumbers(TyLWidth, TyRWidth);

    // Zero bit-width means neither TyL nor TyR are vectors.
    if (!TyLWidth) {
      PointerType *PTyL = dyn_cast<PointerType>(TyL);
      PointerType *PTyR = dyn_cast<PointerType>(TyR);
      if (PTyL && PTyR) {
        unsigned AddrSpaceL = PTyL->getAddressSpace();
        unsigned AddrSpaceR = PTyR->getAddressSpace();
        if (int Res = cmpNumbers(AddrSpaceL, AddrSpaceR))
          return Res;
      }
      if (PTyL)
        return 1;
      if (PTyR)
        return -1;

      // TyL and TyR aren't vectors, nor pointers. We don't know how to
      // bitcast them.
      return TypesRes;
    }
  }

  // OK, types are bitcastable, now check constant contents.

  if (L->isNullValue() && R->isNullValue())
    return TypesRes;
  if (L->isNullValue() && !R->isNullValue())
    return 1;
  if (!L->isNullValue() && R->isNullValue())
    return -1;

  auto GlobalValueL = const_cast<GlobalValue*>(dyn_cast<GlobalValue>(L));
  auto GlobalValueR = const_cast<GlobalValue*>(dyn_cast<GlobalValue>(R));
  if (GlobalValueL && GlobalValueR) {
    return cmpGlobalValues(GlobalValueL, GlobalValueR);
  }

  if (int Res = cmpNumbers(L->getValueID(), R->getValueID()))
    return Res;

  if (const auto *SeqL = dyn_cast<ConstantDataSequential>(L)) {
    const auto *SeqR = cast<ConstantDataSequential>(R);
    // This handles ConstantDataArray and ConstantDataVector. Note that we
    // compare the two raw data arrays, which might differ depending on the host
    // endianness. This isn't a problem though, because the endianness of a
    // module will affect the order of the constants, but this order is the same
    // for a given input module and host platform.
    return cmpMem(SeqL->getRawDataValues(), SeqR->getRawDataValues());
  }

  switch (L->getValueID()) {
  case Value::UndefValueVal:
  case Value::ConstantTokenNoneVal:
    return TypesRes;
  case Value::ConstantIntVal: {
    const APInt &LInt = cast<ConstantInt>(L)->getValue();
    const APInt &RInt = cast<ConstantInt>(R)->getValue();
    return cmpAPInts(LInt, RInt);
  }
  case Value::ConstantFPVal: {
    const APFloat &LAPF = cast<ConstantFP>(L)->getValueAPF();
    const APFloat &RAPF = cast<ConstantFP>(R)->getValueAPF();
    return cmpAPFloats(LAPF, RAPF);
  }
  case Value::ConstantArrayVal: {
    const ConstantArray *LA = cast<ConstantArray>(L);
    const ConstantArray *RA = cast<ConstantArray>(R);
    uint64_t NumElementsL = cast<ArrayType>(TyL)->getNumElements();
    uint64_t NumElementsR = cast<ArrayType>(TyR)->getNumElements();
    if (int Res = cmpNumbers(NumElementsL, NumElementsR))
      return Res;
    for (uint64_t i = 0; i < NumElementsL; ++i) {
      if (int Res = cmpConstants(cast<Constant>(LA->getOperand(i)),
                                 cast<Constant>(RA->getOperand(i))))
        return Res;
    }
    return 0;
  }
  case Value::ConstantStructVal: {
    const ConstantStruct *LS = cast<ConstantStruct>(L);
    const ConstantStruct *RS = cast<ConstantStruct>(R);
    unsigned NumElementsL = cast<StructType>(TyL)->getNumElements();
    unsigned NumElementsR = cast<StructType>(TyR)->getNumElements();
    if (int Res = cmpNumbers(NumElementsL, NumElementsR))
      return Res;
    for (unsigned i = 0; i != NumElementsL; ++i) {
      if (int Res = cmpConstants(cast<Constant>(LS->getOperand(i)),
                                 cast<Constant>(RS->getOperand(i))))
        return Res;
    }
    return 0;
  }
  case Value::ConstantVectorVal: {
    const ConstantVector *LV = cast<ConstantVector>(L);
    const ConstantVector *RV = cast<ConstantVector>(R);
    unsigned NumElementsL = cast<VectorType>(TyL)->getNumElements();
    unsigned NumElementsR = cast<VectorType>(TyR)->getNumElements();
    if (int Res = cmpNumbers(NumElementsL, NumElementsR))
      return Res;
    for (uint64_t i = 0; i < NumElementsL; ++i) {
      if (int Res = cmpConstants(cast<Constant>(LV->getOperand(i)),
                                 cast<Constant>(RV->getOperand(i))))
        return Res;
    }
    return 0;
  }
  case Value::ConstantExprVal: {
    const ConstantExpr *LE = cast<ConstantExpr>(L);
    const ConstantExpr *RE = cast<ConstantExpr>(R);
    unsigned NumOperandsL = LE->getNumOperands();
    unsigned NumOperandsR = RE->getNumOperands();
    if (int Res = cmpNumbers(NumOperandsL, NumOperandsR))
      return Res;
    for (unsigned i = 0; i < NumOperandsL; ++i) {
      if (int Res = cmpConstants(cast<Constant>(LE->getOperand(i)),
                                 cast<Constant>(RE->getOperand(i))))
        return Res;
    }
    return 0;
  }
  case Value::BlockAddressVal: {
    const BlockAddress *LBA = cast<BlockAddress>(L);
    const BlockAddress *RBA = cast<BlockAddress>(R);
    if (int Res = cmpValues(LBA->getFunction(), RBA->getFunction()))
      return Res;
    if (LBA->getFunction() == RBA->getFunction()) {
      // They are BBs in the same function. Order by which comes first in the
      // BB order of the function. This order is deterministic.
      Function* F = LBA->getFunction();
      BasicBlock *LBB = LBA->getBasicBlock();
      BasicBlock *RBB = RBA->getBasicBlock();
      if (LBB == RBB)
        return 0;
      for (BasicBlock &BB : F->getBasicBlockList()) {
        if (&BB == LBB) {
          assert(&BB != RBB);
          return -1;
        }
        if (&BB == RBB)
          return 1;
      }
      llvm_unreachable("Basic Block Address does not point to a basic block in "
                       "its function.");
      return -1;
    } else {
      // cmpValues said the functions are the same. So because they aren't
      // literally the same pointer, they must respectively be the left and
      // right functions.
      assert(LBA->getFunction() == FnL && RBA->getFunction() == FnR);
      // cmpValues will tell us if these are equivalent BasicBlocks, in the
      // context of their respective functions.
      return cmpValues(LBA->getBasicBlock(), RBA->getBasicBlock());
    }
  }
  default: // Unknown constant, abort.
    DEBUG(dbgs() << "Looking at valueID " << L->getValueID() << "\n");
    llvm_unreachable("Constant ValueID not recognized.");
    return -1;
  }
}

int FunctionComparator::cmpGlobalValues(GlobalValue *L, GlobalValue *R) const {
  return cmpNumbers(GlobalNumbers->getNumber(L), GlobalNumbers->getNumber(R));
}

/// cmpType - compares two types,
/// defines total ordering among the types set.
/// See method declaration comments for more details.
int FunctionComparator::cmpTypes(Type *TyL, Type *TyR) const {
  PointerType *PTyL = dyn_cast<PointerType>(TyL);
  PointerType *PTyR = dyn_cast<PointerType>(TyR);

  const DataLayout &DL = FnL->getParent()->getDataLayout();
  if (PTyL && PTyL->getAddressSpace() == 0)
    TyL = DL.getIntPtrType(TyL);
  if (PTyR && PTyR->getAddressSpace() == 0)
    TyR = DL.getIntPtrType(TyR);

  if (TyL == TyR)
    return 0;

  if (int Res = cmpNumbers(TyL->getTypeID(), TyR->getTypeID()))
    return Res;

  switch (TyL->getTypeID()) {
  default:
    llvm_unreachable("Unknown type!");
    // Fall through in Release mode.
  case Type::IntegerTyID:
    return cmpNumbers(cast<IntegerType>(TyL)->getBitWidth(),
                      cast<IntegerType>(TyR)->getBitWidth());
  case Type::VectorTyID: {
    VectorType *VTyL = cast<VectorType>(TyL), *VTyR = cast<VectorType>(TyR);
    if (int Res = cmpNumbers(VTyL->getNumElements(), VTyR->getNumElements()))
      return Res;
    return cmpTypes(VTyL->getElementType(), VTyR->getElementType());
  }
  // TyL == TyR would have returned true earlier, because types are uniqued.
  case Type::VoidTyID:
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
  case Type::LabelTyID:
  case Type::MetadataTyID:
  case Type::TokenTyID:
    return 0;

  case Type::PointerTyID: {
    assert(PTyL && PTyR && "Both types must be pointers here.");
    return cmpNumbers(PTyL->getAddressSpace(), PTyR->getAddressSpace());
  }

  case Type::StructTyID: {
    StructType *STyL = cast<StructType>(TyL);
    StructType *STyR = cast<StructType>(TyR);
    if (STyL->getNumElements() != STyR->getNumElements())
      return cmpNumbers(STyL->getNumElements(), STyR->getNumElements());

    if (STyL->isPacked() != STyR->isPacked())
      return cmpNumbers(STyL->isPacked(), STyR->isPacked());

    for (unsigned i = 0, e = STyL->getNumElements(); i != e; ++i) {
      if (int Res = cmpTypes(STyL->getElementType(i), STyR->getElementType(i)))
        return Res;
    }
    return 0;
  }

  case Type::FunctionTyID: {
    FunctionType *FTyL = cast<FunctionType>(TyL);
    FunctionType *FTyR = cast<FunctionType>(TyR);
    if (FTyL->getNumParams() != FTyR->getNumParams())
      return cmpNumbers(FTyL->getNumParams(), FTyR->getNumParams());

    if (FTyL->isVarArg() != FTyR->isVarArg())
      return cmpNumbers(FTyL->isVarArg(), FTyR->isVarArg());

    if (int Res = cmpTypes(FTyL->getReturnType(), FTyR->getReturnType()))
      return Res;

    for (unsigned i = 0, e = FTyL->getNumParams(); i != e; ++i) {
      if (int Res = cmpTypes(FTyL->getParamType(i), FTyR->getParamType(i)))
        return Res;
    }
    return 0;
  }

  case Type::ArrayTyID: {
    ArrayType *ATyL = cast<ArrayType>(TyL);
    ArrayType *ATyR = cast<ArrayType>(TyR);
    if (ATyL->getNumElements() != ATyR->getNumElements())
      return cmpNumbers(ATyL->getNumElements(), ATyR->getNumElements());
    return cmpTypes(ATyL->getElementType(), ATyR->getElementType());
  }
  }
}

// Determine whether the two operations are the same except that pointer-to-A
// and pointer-to-B are equivalent. This should be kept in sync with
// Instruction::isSameOperationAs.
// Read method declaration comments for more details.
int FunctionComparator::cmpOperations(const Instruction *L,
                                      const Instruction *R) const {
  // Differences from Instruction::isSameOperationAs:
  //  * replace type comparison with calls to isEquivalentType.
  //  * we test for I->hasSameSubclassOptionalData (nuw/nsw/tail) at the top
  //  * because of the above, we don't test for the tail bit on calls later on
  if (int Res = cmpNumbers(L->getOpcode(), R->getOpcode()))
    return Res;

  if (int Res = cmpNumbers(L->getNumOperands(), R->getNumOperands()))
    return Res;

  if (int Res = cmpTypes(L->getType(), R->getType()))
    return Res;

  if (int Res = cmpNumbers(L->getRawSubclassOptionalData(),
                           R->getRawSubclassOptionalData()))
    return Res;

  if (const AllocaInst *AI = dyn_cast<AllocaInst>(L)) {
    if (int Res = cmpTypes(AI->getAllocatedType(),
                           cast<AllocaInst>(R)->getAllocatedType()))
      return Res;
    if (int Res =
            cmpNumbers(AI->getAlignment(), cast<AllocaInst>(R)->getAlignment()))
      return Res;
  }

  // We have two instructions of identical opcode and #operands.  Check to see
  // if all operands are the same type
  for (unsigned i = 0, e = L->getNumOperands(); i != e; ++i) {
    if (int Res =
            cmpTypes(L->getOperand(i)->getType(), R->getOperand(i)->getType()))
      return Res;
  }

  // Check special state that is a part of some instructions.
  if (const LoadInst *LI = dyn_cast<LoadInst>(L)) {
    if (int Res = cmpNumbers(LI->isVolatile(), cast<LoadInst>(R)->isVolatile()))
      return Res;
    if (int Res =
            cmpNumbers(LI->getAlignment(), cast<LoadInst>(R)->getAlignment()))
      return Res;
    if (int Res =
            cmpNumbers(LI->getOrdering(), cast<LoadInst>(R)->getOrdering()))
      return Res;
    if (int Res =
            cmpNumbers(LI->getSynchScope(), cast<LoadInst>(R)->getSynchScope()))
      return Res;
    return cmpRangeMetadata(LI->getMetadata(LLVMContext::MD_range),
        cast<LoadInst>(R)->getMetadata(LLVMContext::MD_range));
  }
  if (const StoreInst *SI = dyn_cast<StoreInst>(L)) {
    if (int Res =
            cmpNumbers(SI->isVolatile(), cast<StoreInst>(R)->isVolatile()))
      return Res;
    if (int Res =
            cmpNumbers(SI->getAlignment(), cast<StoreInst>(R)->getAlignment()))
      return Res;
    if (int Res =
            cmpNumbers(SI->getOrdering(), cast<StoreInst>(R)->getOrdering()))
      return Res;
    return cmpNumbers(SI->getSynchScope(), cast<StoreInst>(R)->getSynchScope());
  }
  if (const CmpInst *CI = dyn_cast<CmpInst>(L))
    return cmpNumbers(CI->getPredicate(), cast<CmpInst>(R)->getPredicate());
  if (const CallInst *CI = dyn_cast<CallInst>(L)) {
    if (int Res = cmpNumbers(CI->getCallingConv(),
                             cast<CallInst>(R)->getCallingConv()))
      return Res;
    if (int Res =
            cmpAttrs(CI->getAttributes(), cast<CallInst>(R)->getAttributes()))
      return Res;
    if (int Res = cmpOperandBundlesSchema(CI, R))
      return Res;
    return cmpRangeMetadata(
        CI->getMetadata(LLVMContext::MD_range),
        cast<CallInst>(R)->getMetadata(LLVMContext::MD_range));
  }
  if (const InvokeInst *II = dyn_cast<InvokeInst>(L)) {
    if (int Res = cmpNumbers(II->getCallingConv(),
                             cast<InvokeInst>(R)->getCallingConv()))
      return Res;
    if (int Res =
            cmpAttrs(II->getAttributes(), cast<InvokeInst>(R)->getAttributes()))
      return Res;
    if (int Res = cmpOperandBundlesSchema(II, R))
      return Res;
    return cmpRangeMetadata(
        II->getMetadata(LLVMContext::MD_range),
        cast<InvokeInst>(R)->getMetadata(LLVMContext::MD_range));
  }
  if (const InsertValueInst *IVI = dyn_cast<InsertValueInst>(L)) {
    ArrayRef<unsigned> LIndices = IVI->getIndices();
    ArrayRef<unsigned> RIndices = cast<InsertValueInst>(R)->getIndices();
    if (int Res = cmpNumbers(LIndices.size(), RIndices.size()))
      return Res;
    for (size_t i = 0, e = LIndices.size(); i != e; ++i) {
      if (int Res = cmpNumbers(LIndices[i], RIndices[i]))
        return Res;
    }
  }
  if (const ExtractValueInst *EVI = dyn_cast<ExtractValueInst>(L)) {
    ArrayRef<unsigned> LIndices = EVI->getIndices();
    ArrayRef<unsigned> RIndices = cast<ExtractValueInst>(R)->getIndices();
    if (int Res = cmpNumbers(LIndices.size(), RIndices.size()))
      return Res;
    for (size_t i = 0, e = LIndices.size(); i != e; ++i) {
      if (int Res = cmpNumbers(LIndices[i], RIndices[i]))
        return Res;
    }
  }
  if (const FenceInst *FI = dyn_cast<FenceInst>(L)) {
    if (int Res =
            cmpNumbers(FI->getOrdering(), cast<FenceInst>(R)->getOrdering()))
      return Res;
    return cmpNumbers(FI->getSynchScope(), cast<FenceInst>(R)->getSynchScope());
  }

  if (const AtomicCmpXchgInst *CXI = dyn_cast<AtomicCmpXchgInst>(L)) {
    if (int Res = cmpNumbers(CXI->isVolatile(),
                             cast<AtomicCmpXchgInst>(R)->isVolatile()))
      return Res;
    if (int Res = cmpNumbers(CXI->isWeak(),
                             cast<AtomicCmpXchgInst>(R)->isWeak()))
      return Res;
    if (int Res = cmpNumbers(CXI->getSuccessOrdering(),
                             cast<AtomicCmpXchgInst>(R)->getSuccessOrdering()))
      return Res;
    if (int Res = cmpNumbers(CXI->getFailureOrdering(),
                             cast<AtomicCmpXchgInst>(R)->getFailureOrdering()))
      return Res;
    return cmpNumbers(CXI->getSynchScope(),
                      cast<AtomicCmpXchgInst>(R)->getSynchScope());
  }
  if (const AtomicRMWInst *RMWI = dyn_cast<AtomicRMWInst>(L)) {
    if (int Res = cmpNumbers(RMWI->getOperation(),
                             cast<AtomicRMWInst>(R)->getOperation()))
      return Res;
    if (int Res = cmpNumbers(RMWI->isVolatile(),
                             cast<AtomicRMWInst>(R)->isVolatile()))
      return Res;
    if (int Res = cmpNumbers(RMWI->getOrdering(),
                             cast<AtomicRMWInst>(R)->getOrdering()))
      return Res;
    return cmpNumbers(RMWI->getSynchScope(),
                      cast<AtomicRMWInst>(R)->getSynchScope());
  }
  if (const PHINode *PNL = dyn_cast<PHINode>(L)) {
    const PHINode *PNR = cast<PHINode>(R);
    // Ensure that in addition to the incoming values being identical
    // (checked by the caller of this function), the incoming blocks
    // are also identical.
    for (unsigned i = 0, e = PNL->getNumIncomingValues(); i != e; ++i) {
      if (int Res =
              cmpValues(PNL->getIncomingBlock(i), PNR->getIncomingBlock(i)))
        return Res;
    }
  }
  return 0;
}

// Determine whether two GEP operations perform the same underlying arithmetic.
// Read method declaration comments for more details.
int FunctionComparator::cmpGEPs(const GEPOperator *GEPL,
                               const GEPOperator *GEPR) {

  unsigned int ASL = GEPL->getPointerAddressSpace();
  unsigned int ASR = GEPR->getPointerAddressSpace();

  if (int Res = cmpNumbers(ASL, ASR))
    return Res;

  // When we have target data, we can reduce the GEP down to the value in bytes
  // added to the address.
  const DataLayout &DL = FnL->getParent()->getDataLayout();
  unsigned BitWidth = DL.getPointerSizeInBits(ASL);
  APInt OffsetL(BitWidth, 0), OffsetR(BitWidth, 0);
  if (GEPL->accumulateConstantOffset(DL, OffsetL) &&
      GEPR->accumulateConstantOffset(DL, OffsetR))
    return cmpAPInts(OffsetL, OffsetR);
  if (int Res = cmpTypes(GEPL->getSourceElementType(),
                         GEPR->getSourceElementType()))
    return Res;

  if (int Res = cmpNumbers(GEPL->getNumOperands(), GEPR->getNumOperands()))
    return Res;

  for (unsigned i = 0, e = GEPL->getNumOperands(); i != e; ++i) {
    if (int Res = cmpValues(GEPL->getOperand(i), GEPR->getOperand(i)))
      return Res;
  }

  return 0;
}

int FunctionComparator::cmpInlineAsm(const InlineAsm *L,
                                     const InlineAsm *R) const {
  // InlineAsm's are uniqued. If they are the same pointer, obviously they are
  // the same, otherwise compare the fields.
  if (L == R)
    return 0;
  if (int Res = cmpTypes(L->getFunctionType(), R->getFunctionType()))
    return Res;
  if (int Res = cmpMem(L->getAsmString(), R->getAsmString()))
    return Res;
  if (int Res = cmpMem(L->getConstraintString(), R->getConstraintString()))
    return Res;
  if (int Res = cmpNumbers(L->hasSideEffects(), R->hasSideEffects()))
    return Res;
  if (int Res = cmpNumbers(L->isAlignStack(), R->isAlignStack()))
    return Res;
  if (int Res = cmpNumbers(L->getDialect(), R->getDialect()))
    return Res;
  llvm_unreachable("InlineAsm blocks were not uniqued.");
  return 0;
}

/// Compare two values used by the two functions under pair-wise comparison. If
/// this is the first time the values are seen, they're added to the mapping so
/// that we will detect mismatches on next use.
/// See comments in declaration for more details.
int FunctionComparator::cmpValues(const Value *L, const Value *R) const {
  // Catch self-reference case.
  if (L == FnL) {
    if (R == FnR)
      return 0;
    return -1;
  }
  if (R == FnR) {
    if (L == FnL)
      return 0;
    return 1;
  }

  const Constant *ConstL = dyn_cast<Constant>(L);
  const Constant *ConstR = dyn_cast<Constant>(R);
  if (ConstL && ConstR) {
    if (L == R)
      return 0;
    return cmpConstants(ConstL, ConstR);
  }

  if (ConstL)
    return 1;
  if (ConstR)
    return -1;

  const InlineAsm *InlineAsmL = dyn_cast<InlineAsm>(L);
  const InlineAsm *InlineAsmR = dyn_cast<InlineAsm>(R);

  if (InlineAsmL && InlineAsmR)
    return cmpInlineAsm(InlineAsmL, InlineAsmR);
  if (InlineAsmL)
    return 1;
  if (InlineAsmR)
    return -1;

  auto LeftSN = sn_mapL.insert(std::make_pair(L, sn_mapL.size())),
       RightSN = sn_mapR.insert(std::make_pair(R, sn_mapR.size()));

  return cmpNumbers(LeftSN.first->second, RightSN.first->second);
}

static bool isEligibleForConstantSharing(const Instruction *I) {
  switch (I->getOpcode()) {
    case Instruction::Load:
    case Instruction::Store:
    case Instruction::Call:
      return true;
    default:
      return false;
  }
}

int FunctionComparator::cmpOperands(const Instruction *L, const Instruction *R,
                                    unsigned opIdx) {
  Value *OpL = L->getOperand(opIdx);
  Value *OpR = R->getOperand(opIdx);

  int Res = cmpValues(OpL, OpR);
  if (Res == 0)
    return Res;

  if (!isa<Constant>(OpL) || !isa<Constant>(OpR))
    return Res;

  if (!isEligibleForConstantSharing(L))
    return Res;

  if (const CallInst *CL = dyn_cast<CallInst>(L)) {
    if (CL->isInlineAsm())
      return Res;
    if (Function *CalleeL = CL->getCalledFunction()) {
      if (CalleeL->isIntrinsic())
        return Res;
    }
    const CallInst *CR = cast<CallInst>(R);
    if (CR->isInlineAsm())
      return Res;
    if (Function *CalleeR = CR->getCalledFunction()) {
      if (CalleeR->isIntrinsic())
        return Res;
    }
  }

  if (cmpTypes(OpL->getType(), OpR->getType()))
    return Res;

  return 0;
}

// Test whether two basic blocks have equivalent behaviour.
int FunctionComparator::cmpBasicBlocks(const BasicBlock *BBL,
                                       const BasicBlock *BBR) {
  BasicBlock::const_iterator InstL = BBL->begin(), InstLE = BBL->end();
  BasicBlock::const_iterator InstR = BBR->begin(), InstRE = BBR->end();

  do {
    if (int Res = cmpValues(&*InstL, &*InstR))
      return Res;

    const GetElementPtrInst *GEPL = dyn_cast<GetElementPtrInst>(InstL);
    const GetElementPtrInst *GEPR = dyn_cast<GetElementPtrInst>(InstR);

    if (GEPL && !GEPR)
      return 1;
    if (GEPR && !GEPL)
      return -1;

    if (GEPL && GEPR) {
      if (int Res =
              cmpValues(GEPL->getPointerOperand(), GEPR->getPointerOperand()))
        return Res;
      if (int Res = cmpGEPs(GEPL, GEPR))
        return Res;
    } else {
      if (int Res = cmpOperations(&*InstL, &*InstR))
        return Res;
      assert(InstL->getNumOperands() == InstR->getNumOperands());

      for (unsigned i = 0, e = InstL->getNumOperands(); i != e; ++i) {
        if (int Res = cmpOperands(&*InstL, &*InstR, i))
          return Res;
        // cmpValues should ensure this is true.
        assert(cmpTypes(InstL->getOperand(i)->getType(),
                        InstR->getOperand(i)->getType()) == 0);
      }
    }

    ++InstL, ++InstR;
  } while (InstL != InstLE && InstR != InstRE);

  if (InstL != InstLE && InstR == InstRE)
    return 1;
  if (InstL == InstLE && InstR != InstRE)
    return -1;
  return 0;
}

// Test whether the two functions have equivalent behaviour.
int FunctionComparator::compare() {
  sn_mapL.clear();
  sn_mapR.clear();

  if (int Res = cmpAttrs(FnL->getAttributes(), FnR->getAttributes()))
    return Res;

  if (int Res = cmpNumbers(FnL->hasGC(), FnR->hasGC()))
    return Res;

  if (FnL->hasGC()) {
    if (int Res = cmpMem(FnL->getGC(), FnR->getGC()))
      return Res;
  }

  if (int Res = cmpNumbers(FnL->hasSection(), FnR->hasSection()))
    return Res;

  if (FnL->hasSection()) {
    if (int Res = cmpMem(FnL->getSection(), FnR->getSection()))
      return Res;
  }

  if (int Res = cmpNumbers(FnL->isVarArg(), FnR->isVarArg()))
    return Res;

  // TODO: if it's internal and only used in direct calls, we could handle this
  // case too.
  if (int Res = cmpNumbers(FnL->getCallingConv(), FnR->getCallingConv()))
    return Res;

  if (int Res = cmpTypes(FnL->getFunctionType(), FnR->getFunctionType()))
    return Res;

  assert(FnL->arg_size() == FnR->arg_size() &&
         "Identically typed functions have different numbers of args!");

  // Visit the arguments so that they get enumerated in the order they're
  // passed in.
  for (Function::const_arg_iterator ArgLI = FnL->arg_begin(),
                                    ArgRI = FnR->arg_begin(),
                                    ArgLE = FnL->arg_end();
       ArgLI != ArgLE; ++ArgLI, ++ArgRI) {
    if (cmpValues(&*ArgLI, &*ArgRI) != 0)
      llvm_unreachable("Arguments repeat!");
  }

  Function::const_iterator LIter = FnL->begin(), LEnd = FnL->end();
  Function::const_iterator RIter = FnR->begin(), REnd = FnR->end();

  do {
    const BasicBlock *BBL = &*LIter;
    const BasicBlock *BBR = &*RIter;

    if (int Res = cmpValues(BBL, BBR))
      return Res;

    if (int Res = cmpBasicBlocks(BBL, BBR))
      return Res;

     ++LIter, ++RIter;
  } while (LIter != LEnd && RIter != REnd);

  return 0;
}

namespace {
// Accumulate the hash of a sequence of 64-bit integers. This is similar to a
// hash of a sequence of 64bit ints, but the entire input does not need to be
// available at once. This interface is necessary for functionHash because it
// needs to accumulate the hash as the structure of the function is traversed
// without saving these values to an intermediate buffer. This form of hashing
// is not often needed, as usually the object to hash is just read from a
// buffer.
class HashAccumulator64 {
  uint64_t Hash;
public:
  // Initialize to random constant, so the state isn't zero.
  HashAccumulator64() { Hash = 0x6acaa36bef8325c5ULL; }
  void add(uint64_t V) {
     Hash = llvm::hashing::detail::hash_16_bytes(Hash, V);
  }
  // No finishing is required, because the entire hash value is used.
  uint64_t getHash() { return Hash; }
};
} // end anonymous namespace

// A function hash is calculated by considering only the number of arguments and
// whether a function is varargs, the order of basic blocks (given by the
// successors of each basic block in depth first order), and the order of
// opcodes of each instruction within each of these basic blocks. This mirrors
// the strategy compare() uses to compare functions by walking the BBs in depth
// first order and comparing each instruction in sequence. Because this hash
// does not look at the operands, it is insensitive to things such as the
// target of calls and the constants used in the function, which makes it useful
// when possibly merging functions which are the same modulo constants and call
// targets.
FunctionComparator::FunctionHash FunctionComparator::functionHash(Function &F) {
  HashAccumulator64 H;
  H.add(F.isVarArg());
  H.add(F.arg_size());
  
  SmallVector<const BasicBlock *, 8> BBs;
  SmallSet<const BasicBlock *, 16> VisitedBBs;

  // Walk the blocks in the same order as FunctionComparator::cmpBasicBlocks(),
  // accumulating the hash of the function "structure." (BB and opcode sequence)
  BBs.push_back(&F.getEntryBlock());
  VisitedBBs.insert(BBs[0]);
  while (!BBs.empty()) {
    const BasicBlock *BB = BBs.pop_back_val();
    // This random value acts as a block header, as otherwise the partition of
    // opcodes into BBs wouldn't affect the hash, only the order of the opcodes
    H.add(45798); 
    for (auto &Inst : *BB) {
      H.add(Inst.getOpcode());
    }
    const TerminatorInst *Term = BB->getTerminator();
    for (unsigned i = 0, e = Term->getNumSuccessors(); i != e; ++i) {
      if (!VisitedBBs.insert(Term->getSuccessor(i)).second)
        continue;
      BBs.push_back(Term->getSuccessor(i));
    }
  }
  return H.getHash();
}


namespace {

/// SwiftMergeFunctions finds functions which only differ by constants in
/// certain instructions, e.g. resulting from specialized functions of layout
/// compatible types.
/// Such functions are merged by replacing the differing constants by a
/// parameter. The original functions are replaced by thunks which call the
/// merged function with the specific argument constants.
///
class SwiftMergeFunctions : public ModulePass {
public:
  static char ID;
  SwiftMergeFunctions()
    : ModulePass(ID), FnTree(FunctionNodeCmp(&GlobalNumbers)) {
  }

  bool runOnModule(Module &M) override;

private:
  enum {
    /// The maximum number of parameters added to a merged functions. This
    /// roughly corresponds to the number of differing constants.
    maxAddedParams = 4
  };

  struct FunctionEntry;

  /// Describes the set of functions which are considered as "equivalent" (i.e.
  /// only differing by some constants).
  struct EquivalenceClass {
    /// The single-linked list of all functions which are a member of this
    /// equivalence class.
    FunctionEntry *First;

    /// A very cheap hash, used to early exit if functions do not match.
    FunctionComparator::FunctionHash Hash;
  public:
    // Note the hash is recalculated potentially multiple times, but it is cheap.
    EquivalenceClass(FunctionEntry *First)
      : First(First), Hash(FunctionComparator::functionHash(*First->F)) {
      assert(!First->Next);
    }
  };

  /// The function comparison operator is provided here so that FunctionNodes do
  /// not need to become larger with another pointer.
  class FunctionNodeCmp {
    GlobalNumberState* GlobalNumbers;
  public:
    FunctionNodeCmp(GlobalNumberState* GN) : GlobalNumbers(GN) {}
    bool operator()(const EquivalenceClass &LHS, const EquivalenceClass &RHS) const {
      // Order first by hashes, then full function comparison.
      if (LHS.Hash != RHS.Hash)
        return LHS.Hash < RHS.Hash;
      FunctionComparator FCmp(LHS.First->F, RHS.First->F, GlobalNumbers);
      return FCmp.compare() == -1;
    }
  };
  typedef std::set<EquivalenceClass, FunctionNodeCmp> FnTreeType;

  /// 
  struct FunctionEntry {
    FunctionEntry(Function *F, FnTreeType::iterator I) :
        F(F), Next(nullptr), numUnhandledCallees(0), TreeIter(I),
        isMerged(false) { }

    /// Back-link to the function.
    AssertingVH<Function> F;

    /// The next function in its equivalence class.
    FunctionEntry *Next;

    /// The number of not-yet merged callees. Used to process the merging in
    /// bottom-up call order.
    /// This is only valid in the first entry of an equivalence class. The
    /// counts of all functions in an equivalence class are accumulated in the
    /// first entry.
    int numUnhandledCallees;

    /// The iterator of the function's equivalence class in the FnTree.
    /// It's FnTree.end() if the function is not in an equivalence class.
    FnTreeType::iterator TreeIter;

    /// True if this function is already a thunk, calling the merged function.
    bool isMerged;
  };

  /// Describes an operator of a specific instruction.
  struct OpLocation {
    Instruction *I;
    unsigned OpIndex;
  };

  /// Information for a function. Used during merging.
  struct FunctionInfo {

    FunctionInfo(Function *F) : F(F), CurrentInst(nullptr), NumParamsNeeded(0) {
    }

    void init() {
      CurrentInst = &*F->begin()->begin();
      NumParamsNeeded = 0;
    }

    /// Advances the current instruction to the next instruction.
    void nextInst() {
      assert(CurrentInst);
      if (isa<TerminatorInst>(CurrentInst)) {
        auto BlockIter = std::next(CurrentInst->getParent()->getIterator());
        if (BlockIter == F->end()) {
          CurrentInst = nullptr;
          return;
        }
        CurrentInst = &*BlockIter->begin();
        return;
      }
      CurrentInst = &*std::next(CurrentInst->getIterator());
    }

    Function *F;

    /// The current instruction while iterating over all instructions.
    Instruction *CurrentInst;

    /// Roughly the number of parameters needed if this function would be
    /// merged with the first function of the equivalence class.
    int NumParamsNeeded;
  };

  typedef SmallVector<FunctionInfo, 8> FunctionInfos;

  /// Describes a parameter which we create to parameterize the merged function.
  struct ParamInfo {
    /// The value of the parameter for all the functions in the equivalence
    /// class.
    SmallVector<Constant *, 8> Values;

    /// All uses of the parameter in the merged function.
    SmallVector<OpLocation, 16> Uses;

    /// Checks if this parameter can be used to describe an operand in all
    /// functions of the equivalence class. Returns true if all values match
    /// the specific instruction operands in all functions.
    bool matches(const FunctionInfos &FInfos, unsigned OpIdx) const {
      unsigned NumFuncs = FInfos.size();
      assert(Values.size() == NumFuncs);
      for (unsigned Idx = 0; Idx < NumFuncs; ++Idx) {
        const FunctionInfo &FI = FInfos[Idx];
        Constant *C = cast<Constant>(FI.CurrentInst->getOperand(OpIdx));
        if (Values[Idx] != C)
          return false;
      }
      return true;
    }
  };

  typedef SmallVector<ParamInfo, maxAddedParams> ParamInfos;

  GlobalNumberState GlobalNumbers;

  /// A work queue of functions that may have been modified and should be
  /// analyzed again.
  std::vector<WeakVH> Deferred;

  /// The set of all distinct functions. Use the insert() and remove() methods
  /// to modify it. The map allows efficient lookup and deferring of Functions.
  FnTreeType FnTree;

  ValueMap<Function*, FunctionEntry *> FuncEntries;

  FunctionEntry *getEntry(Function *F) const {
    return FuncEntries.lookup(F);
  }
  
  bool isInEquivalenceClass(FunctionEntry *FE) const {
    if (FE->TreeIter != FnTree.end()) {
      return true;
    }
    assert(!FE->Next);
    assert(FE->numUnhandledCallees == 0);
    return false;
  }

  /// Checks the rules of order relation introduced among functions set.
  /// Returns true, if sanity check has been passed, and false if failed.
  bool doSanityCheck(std::vector<WeakVH> &Worklist);

  /// Updates the numUnhandledCallees of all user functions of the equivalence
  /// class containing \p FE by \p Delta.
  void updateUnhandledCalleeCount(FunctionEntry *FE, int Delta);

  bool tryMergeEquivalenceClass(FunctionEntry *FirstInClass);

  FunctionInfo removeFuncWithMostParams(FunctionInfos &FInfos);

  bool deriveParams(ParamInfos &Params, FunctionInfos &FInfos);

  bool constsDiffer(const FunctionInfos &FInfos, unsigned OpIdx);

  bool tryMapToParameter(FunctionInfos &FInfos, unsigned OpIdx,
                         ParamInfos &Params);

  void mergeWithParams(const FunctionInfos &FInfos, ParamInfos &Params);

  void removeEquivalenceClassFromTree(FunctionEntry *FE);

  void writeThunk(Function *ToFunc, Function *Thunk,
                  const ParamInfos &Params, unsigned FuncIdx);

  /// Replace all direct calls of Old with calls of New. Will bitcast New if
  /// necessary to make types match.
  bool replaceDirectCallers(Function *Old, Function *New,
                            const ParamInfos &Params, unsigned FuncIdx);
};

} // end anonymous namespace

char SwiftMergeFunctions::ID = 0;
INITIALIZE_PASS_BEGIN(SwiftMergeFunctions,
                      "swift-merge-functions", "Swift merge function pass",
                      false, false)
INITIALIZE_PASS_END(SwiftMergeFunctions,
                    "swift-merge-functions", "Swift merge function pass",
                    false, false)

llvm::ModulePass *swift::createSwiftMergeFunctionsPass() {
  initializeSwiftMergeFunctionsPass(*llvm::PassRegistry::getPassRegistry());
  return new SwiftMergeFunctions();
}

bool SwiftMergeFunctions::doSanityCheck(std::vector<WeakVH> &Worklist) {
  if (const unsigned Max = NumFunctionsForSanityCheck) {
    unsigned TripleNumber = 0;
    bool Valid = true;

    dbgs() << "MERGEFUNC-SANITY: Started for first " << Max << " functions.\n";

    unsigned i = 0;
    for (std::vector<WeakVH>::iterator I = Worklist.begin(), E = Worklist.end();
         I != E && i < Max; ++I, ++i) {
      unsigned j = i;
      for (std::vector<WeakVH>::iterator J = I; J != E && j < Max; ++J, ++j) {
        Function *F1 = cast<Function>(*I);
        Function *F2 = cast<Function>(*J);
        int Res1 = FunctionComparator(F1, F2, &GlobalNumbers).compare();
        int Res2 = FunctionComparator(F2, F1, &GlobalNumbers).compare();

        // If F1 <= F2, then F2 >= F1, otherwise report failure.
        if (Res1 != -Res2) {
          dbgs() << "MERGEFUNC-SANITY: Non-symmetric; triple: " << TripleNumber
                 << "\n";
          F1->dump();
          F2->dump();
          Valid = false;
        }

        if (Res1 == 0)
          continue;

        unsigned k = j;
        for (std::vector<WeakVH>::iterator K = J; K != E && k < Max;
             ++k, ++K, ++TripleNumber) {
          if (K == J)
            continue;

          Function *F3 = cast<Function>(*K);
          int Res3 = FunctionComparator(F1, F3, &GlobalNumbers).compare();
          int Res4 = FunctionComparator(F2, F3, &GlobalNumbers).compare();

          bool Transitive = true;

          if (Res1 != 0 && Res1 == Res4) {
            // F1 > F2, F2 > F3 => F1 > F3
            Transitive = Res3 == Res1;
          } else if (Res3 != 0 && Res3 == -Res4) {
            // F1 > F3, F3 > F2 => F1 > F2
            Transitive = Res3 == Res1;
          } else if (Res4 != 0 && -Res3 == Res4) {
            // F2 > F3, F3 > F1 => F2 > F1
            Transitive = Res4 == -Res1;
          }

          if (!Transitive) {
            dbgs() << "MERGEFUNC-SANITY: Non-transitive; triple: "
                   << TripleNumber << "\n";
            dbgs() << "Res1, Res3, Res4: " << Res1 << ", " << Res3 << ", "
                   << Res4 << "\n";
            F1->dump();
            F2->dump();
            F3->dump();
            Valid = false;
          }
        }
      }
    }

    dbgs() << "MERGEFUNC-SANITY: " << (Valid ? "Passed." : "Failed.") << "\n";
    return Valid;
  }
  return true;
}

/// Returns true if function \p F is eligible for merging.
static bool isEligibleFunction(Function *F) {
  if (F->isDeclaration())
    return false;
  
  if (F->hasAvailableExternallyLinkage())
    return false;

  if (F->getFunctionType()->isVarArg())
    return false;
  
  unsigned Benefit = 0;

  // We don't want to merge very small functions, because the overhead of
  // adding creating thunks and/or adding parameters to the call sites
  // outweighs the benefit.
  for (BasicBlock &BB : *F) {
    for (Instruction &I : BB) {
      if (CallSite CS = CallSite(&I)) {
        Function *Callee = CS.getCalledFunction();
        if (!Callee || !Callee->isIntrinsic()) {
          Benefit += 5;
          continue;
        }
      }
      Benefit += 1;
    }
  }
  if (Benefit < FunctionMergeThreshold)
    return false;
  
  return true;
}

bool SwiftMergeFunctions::runOnModule(Module &M) {
  
  if (FunctionMergeThreshold == 0)
    return false;

  bool Changed = false;

  // All functions in the module, ordered by hash. Functions with a unique
  // hash value are easily eliminated.
  std::vector<std::pair<FunctionComparator::FunctionHash, Function *>>
    HashedFuncs;

  for (Function &Func : M) {
    if (isEligibleFunction(&Func)) {
      HashedFuncs.push_back({FunctionComparator::functionHash(Func), &Func});
    }
  }

  std::stable_sort(
      HashedFuncs.begin(), HashedFuncs.end(),
      [](const std::pair<FunctionComparator::FunctionHash, Function *> &a,
         const std::pair<FunctionComparator::FunctionHash, Function *> &b) {
        return a.first < b.first;
      });

  std::vector<FunctionEntry> FuncEntryStorage;
  FuncEntryStorage.reserve(HashedFuncs.size());

  auto S = HashedFuncs.begin();
  for (auto I = HashedFuncs.begin(), IE = HashedFuncs.end(); I != IE; ++I) {

    Function *F = I->second;
    FuncEntryStorage.push_back(FunctionEntry(F, FnTree.end()));
    FunctionEntry &FE = FuncEntryStorage.back();
    FuncEntries[F] = &FE;

    // If the hash value matches the previous value or the next one, we must
    // consider merging it. Otherwise it is dropped and never considered again.
    if ((I != S && std::prev(I)->first == I->first) ||
        (std::next(I) != IE && std::next(I)->first == I->first) ) {
      Deferred.push_back(WeakVH(F));
    }
  }

  do {
    std::vector<WeakVH> Worklist;
    Deferred.swap(Worklist);

    DEBUG(dbgs() << "======\nbuild tree: worklist-size=" << Worklist.size() <<
                    '\n');
    DEBUG(doSanityCheck(Worklist));

    SmallVector<FunctionEntry *, 8> FuncsToMerge;

    // Insert all candidates into the Worklist.
    for (std::vector<WeakVH>::iterator I = Worklist.begin(),
           E = Worklist.end(); I != E; ++I) {
      if (!*I) continue;
      Function *F = cast<Function>(*I);
      FunctionEntry *FE = getEntry(F);
      assert(!isInEquivalenceClass(FE));

      std::pair<FnTreeType::iterator, bool> Result = FnTree.insert(FE);

      FE->TreeIter = Result.first;
      const EquivalenceClass &Eq = *Result.first;

      if (Result.second) {
        assert(Eq.First == FE);
        DEBUG(dbgs() << "  new in tree: " << F->getName() << '\n');
      } else {
        assert(Eq.First != FE);
        DEBUG(dbgs() << "  add to existing: " << F->getName() << '\n');
        // Add the function to the existing equivalence class.
        FE->Next = Eq.First->Next;
        Eq.First->Next = FE;
        // Schedule for merging if the function's equivalence class reaches the
        // size of 2.
        if (!FE->Next)
          FuncsToMerge.push_back(Eq.First);
      }
    }
    DEBUG(dbgs() << "merge functions: tree-size=" << FnTree.size() << '\n');

    // Figure out the leaf functions. We want to do the merging in bottom-up
    // call order. This ensures that we don't parameterize on callee function
    // names if we don't have to (because the callee may be merged).
    // Note that "leaf functions" refer to the sub-call-graph of functions which
    // are in the FnTree.
    for (FunctionEntry *ToMerge : FuncsToMerge) {
      assert(isInEquivalenceClass(ToMerge));
      updateUnhandledCalleeCount(ToMerge, 1);
    }

    // Check if there are any leaf functions at all.
    bool LeafFound = false;
    for (FunctionEntry *ToMerge : FuncsToMerge) {
      if (ToMerge->numUnhandledCallees == 0)
        LeafFound = true;
    }
    for (FunctionEntry *ToMerge : FuncsToMerge) {
      if (isInEquivalenceClass(ToMerge)) {
        // Only merge leaf functions (or all functions if all functions are in
        // a call cycle).
        if (ToMerge->numUnhandledCallees == 0 || !LeafFound) {
          updateUnhandledCalleeCount(ToMerge, -1);
          Changed |= tryMergeEquivalenceClass(ToMerge);
        } else {
          // Non-leaf functions (i.e. functions in a call cycle) may become
          // leaf functions in the next iteration.
          removeEquivalenceClassFromTree(ToMerge);
        }
      }
    }
  } while (!Deferred.empty());

  FnTree.clear();
  GlobalNumbers.clear();
  FuncEntries.clear();

  return Changed;
}

void SwiftMergeFunctions::updateUnhandledCalleeCount(FunctionEntry *FE,
                                                     int Delta) {
  // Iterate over all functions of FE's equivalence class.
  do {
    for (Use &U : FE->F->uses()) {
      if (Instruction *I = dyn_cast<Instruction>(U.getUser())) {
        FunctionEntry *CallerFE = getEntry(I->getFunction());
        if (CallerFE && CallerFE->TreeIter != FnTree.end()) {
          // Accumulate the count in the first entry of the equivalence class.
          FunctionEntry *Head = CallerFE->TreeIter->First;
          Head->numUnhandledCallees += Delta;
        }
      }
    }
    FE = FE->Next;
  } while (FE);
}

bool SwiftMergeFunctions::tryMergeEquivalenceClass(FunctionEntry *FirstInClass) {
  // Build the FInfos vector from all functions in the equivalence class.
  FunctionInfos FInfos;
  FunctionEntry *FE = FirstInClass;
  do {
    FInfos.push_back(FunctionInfo(FE->F));
    FE->isMerged = true;
    FE = FE->Next;
  } while (FE);
  assert(FInfos.size() >= 2);

  // Merged or not: in any case we remove the equivalence class from the FnTree.
  removeEquivalenceClassFromTree(FirstInClass);

  // Contains functions which differ too much from the first function (i.e.
  // would need too many parameters).
  FunctionInfos Removed;

  bool Changed = false;
  int Try = 0;

  // We need multiple tries if there are some functions in FInfos which differ
  // too much from the first function in FInfos. But we limit the number of
  // tries to a small number, because this is quadratic.
  while (FInfos.size() >= 2 && Try++ < 4) {
    ParamInfos Params;
    bool Merged = deriveParams(Params, FInfos);
    if (Merged) {
      mergeWithParams(FInfos, Params);
      Changed = true;
    } else {
      // We ran out of parameters. Remove the function from the set which
      // differs most from the first function.
      Removed.push_back(removeFuncWithMostParams(FInfos));
    }
    if (Merged || FInfos.size() < 2) {
      // Try again with the functions which were removed from the original set.
      FInfos.swap(Removed);
      Removed.clear();
    }
  }
  return Changed;
}

/// Remove the function from \p FInfos which needs the most parameters. Add the
/// removed function to
SwiftMergeFunctions::FunctionInfo SwiftMergeFunctions::
removeFuncWithMostParams(FunctionInfos &FInfos) {
  FunctionInfos::iterator MaxIter = FInfos.end();
  for (auto Iter = FInfos.begin(), End = FInfos.end(); Iter != End; ++Iter) {
    if (MaxIter == FInfos.end() ||
        Iter->NumParamsNeeded > MaxIter->NumParamsNeeded) {
      MaxIter = Iter;
    }
  }
  FunctionInfo Removed = *MaxIter;
  FInfos.erase(MaxIter);
  return Removed;
}

/// Finds the set of parameters which are required to merge the functions in
/// \p FInfos.
/// Returns true on success, i.e. the functions in \p FInfos can be merged with
/// the parameters returned in \p Params.
bool SwiftMergeFunctions::deriveParams(ParamInfos &Params,
                                       FunctionInfos &FInfos) {
  for (FunctionInfo &FI : FInfos)
    FI.init();

  FunctionInfo &FirstFI = FInfos.front();

  // Iterate over all instructions synchronously in all functions.
  do {
    if (isEligibleForConstantSharing(FirstFI.CurrentInst)) {
      for (unsigned OpIdx = 0, NumOps = FirstFI.CurrentInst->getNumOperands();
           OpIdx != NumOps; ++OpIdx) {

        if (constsDiffer(FInfos, OpIdx)) {
          // This instruction has operands which differ in at least some
          // functions. So we need to parameterize it.
          if (!tryMapToParameter(FInfos, OpIdx, Params)) {
            // We ran out of parameters.
            return false;
          }
        }
      }
    }
    // Go to the next instruction in all functions.
    for (FunctionInfo &FI : FInfos)
      FI.nextInst();
  } while (FirstFI.CurrentInst);

  return true;
}

/// Returns true if the \p OpIdx's constant operand in the current instruction
/// does differ in any of the functions in \p FInfos.
bool SwiftMergeFunctions::constsDiffer(const FunctionInfos &FInfos,
                                       unsigned OpIdx) {
  Constant *CommonConst = nullptr;

  for (const FunctionInfo &FI : FInfos) {
    Value *Op = FI.CurrentInst->getOperand(OpIdx);
    if (Constant *C = dyn_cast<Constant>(Op)) {
      if (!CommonConst) {
        CommonConst = C;
      } else if (C != CommonConst) {
        return true;
      }
    }
  }
  return false;
}

/// Create a new parameter for differing operands or try to reuse an existing
/// parameter.
/// Returns true if a parameter could be created or found without exceeding the
/// maximum number of parameters.
bool SwiftMergeFunctions::tryMapToParameter(FunctionInfos &FInfos,
                                            unsigned OpIdx, ParamInfos &Params) {
  ParamInfo *Matching = nullptr;
  // Try to find an existing parameter which exactly matches the differing
  // operands of the current instruction.
  for (ParamInfo &PI : Params) {
    if (PI.matches(FInfos, OpIdx)) {
      Matching = &PI;
      break;
    }
  }
  if (!Matching) {
    // We need a new parameter.
    // Check if we are within the limit.
    if (Params.size() >= maxAddedParams)
      return false;

    Params.resize(Params.size() + 1);
    Matching = &Params.back();
    // Store the constant values into the new parameter.
    Constant *FirstC = cast<Constant>(FInfos[0].CurrentInst->getOperand(OpIdx));
    for (FunctionInfo &FI : FInfos) {
      Constant *C = cast<Constant>(FI.CurrentInst->getOperand(OpIdx));
      Matching->Values.push_back(C);
      if (C != FirstC)
        FI.NumParamsNeeded += 1;
    }
  }
  /// Remember where the parameter is needed when we build our merged function.
  Matching->Uses.push_back({FInfos[0].CurrentInst, OpIdx});
  return true;
}

/// Merge all functions in \p FInfos by creating thunks which call the single
/// merged function with additional parameters.
void SwiftMergeFunctions::mergeWithParams(const FunctionInfos &FInfos,
                                          ParamInfos &Params) {
  // We reuse the body of the first function for the new merged function.
  Function *FirstF = FInfos.front().F;

  // Build the type for the merged function. This will be the type of the
  // original function (FirstF) but with the additional parameter which are
  // needed to parameterize the merged function.
  FunctionType *OrigTy = FirstF->getFunctionType();
  SmallVector<Type *, 8> ParamTypes(OrigTy->param_begin(), OrigTy->param_end());

  for (const ParamInfo &PI : Params) {
    ParamTypes.push_back(PI.Values[0]->getType());
  }

  FunctionType *funcType =
                  FunctionType::get(OrigTy->getReturnType(), ParamTypes, false);

  // Create the new function.
  // TODO: Use a better name than just adding a suffix. Ideally it would be
  // a name which can be demangled in a meaningful way.
  Function *NewFunction = Function::Create(funcType,
                                           FirstF->getLinkage(),
                                           FirstF->getName() + "_merged");
  NewFunction->copyAttributesFrom(FirstF);
  NewFunction->setLinkage(GlobalValue::InternalLinkage);

  // Insert the new function after the last function in the equivalence class.
  FirstF->getParent()->getFunctionList().insert(
                        std::next(FInfos[1].F->getIterator()), NewFunction);
  
  DEBUG(dbgs() << "  Merge into " << NewFunction->getName() << '\n');

  // Move the body of FirstF into the NewFunction.
  NewFunction->getBasicBlockList().splice(NewFunction->begin(),
                                          FirstF->getBasicBlockList());

  auto NewArgIter = NewFunction->arg_begin();
  for (Argument &OrigArg : FirstF->args()) {
    Argument &NewArg = *NewArgIter++;
    OrigArg.replaceAllUsesWith(&NewArg);
  }

  // Replace all differing operands with a parameter.
  for (const ParamInfo &PI : Params) {
    Argument *NewArg = &*NewArgIter++;
    for (const OpLocation &OL : PI.Uses) {
      OL.I->setOperand(OL.OpIndex, NewArg);
    }
    ParamTypes.push_back(PI.Values[0]->getType());
  }

  for (unsigned FIdx = 0, NumFuncs = FInfos.size(); FIdx < NumFuncs; ++FIdx) {
    Function *OrigFunc = FInfos[FIdx].F;
    if (replaceDirectCallers(OrigFunc, NewFunction, Params, FIdx)) {
      // We could replace all uses (and the function is not externally visible),
      // so we can delete the original function.
      auto Iter = FuncEntries.find(OrigFunc);
      assert(Iter != FuncEntries.end());
      assert(!isInEquivalenceClass(&*Iter->second));
      Iter->second->F = nullptr;
      FuncEntries.erase(Iter);
      OrigFunc->eraseFromParent();
    } else {
      // Otherwise we need a thunk which calls the merged function.
      writeThunk(NewFunction, OrigFunc, Params, FIdx);
    }
    ++NumSwiftFunctionsMerged;
  }
}

/// Remove all functions of \p FE's equivalence class from FnTree. Add them to
/// Deferred so that we'll look at them in the next round.
void SwiftMergeFunctions::removeEquivalenceClassFromTree(FunctionEntry *FE) {
  if (!isInEquivalenceClass(FE))
    return;

  FnTreeType::iterator Iter = FE->TreeIter;
  FunctionEntry *Unlink = Iter->First;
  Unlink->numUnhandledCallees = 0;
  while (Unlink) {
    DEBUG(dbgs() << "    remove from tree: " << Unlink->F->getName() << '\n');
    if (!Unlink->isMerged)
      Deferred.emplace_back(Unlink->F);
    Unlink->TreeIter = FnTree.end();
    assert(Unlink->numUnhandledCallees == 0);
    FunctionEntry *NextEntry = Unlink->Next;
    Unlink->Next = nullptr;
    Unlink = NextEntry;
  }
  FnTree.erase(Iter);
}

// Helper for writeThunk,
// Selects proper bitcast operation,
// but a bit simpler then CastInst::getCastOpcode.
static Value *createCast(IRBuilder<> &Builder, Value *V, Type *DestTy) {
  Type *SrcTy = V->getType();
  if (SrcTy->isStructTy()) {
    assert(DestTy->isStructTy());
    assert(SrcTy->getStructNumElements() == DestTy->getStructNumElements());
    Value *Result = UndefValue::get(DestTy);
    for (unsigned int I = 0, E = SrcTy->getStructNumElements(); I < E; ++I) {
      Value *Element = createCast(
          Builder, Builder.CreateExtractValue(V, makeArrayRef(I)),
          DestTy->getStructElementType(I));

      Result =
          Builder.CreateInsertValue(Result, Element, makeArrayRef(I));
    }
    return Result;
  }
  assert(!DestTy->isStructTy());
  if (SrcTy->isIntegerTy() && DestTy->isPointerTy())
    return Builder.CreateIntToPtr(V, DestTy);
  else if (SrcTy->isPointerTy() && DestTy->isIntegerTy())
    return Builder.CreatePtrToInt(V, DestTy);
  else
    return Builder.CreateBitCast(V, DestTy);
}

/// Replace \p Thunk with a simple tail call to \p ToFunc. Also add parameters
/// to the call to \p ToFunc, which are defined by the FuncIdx's value in
/// \p Params.
void SwiftMergeFunctions::writeThunk(Function *ToFunc, Function *Thunk,
                                     const ParamInfos &Params,
                                     unsigned FuncIdx) {
  // Delete the existing content of Thunk.
  Thunk->dropAllReferences();
  
  BasicBlock *BB = BasicBlock::Create(Thunk->getContext(), "", Thunk);
  IRBuilder<> Builder(BB);

  SmallVector<Value *, 16> Args;
  unsigned ParamIdx = 0;
  FunctionType *ToFuncTy = ToFunc->getFunctionType();
  
  // Add arguments which are passed through Thunk.
  for (Argument & AI : Thunk->args()) {
    Args.push_back(createCast(Builder, &AI, ToFuncTy->getParamType(ParamIdx)));
    ++ParamIdx;
  }
  // Add new arguments defined by Params.
  for (const ParamInfo &PI : Params) {
    assert(ParamIdx < ToFuncTy->getNumParams());
    Args.push_back(createCast(Builder, PI.Values[FuncIdx],
                   ToFuncTy->getParamType(ParamIdx)));
    ++ParamIdx;
  }

  CallInst *CI = Builder.CreateCall(ToFunc, Args);
  CI->setTailCall();
  CI->setCallingConv(ToFunc->getCallingConv());
  CI->setAttributes(ToFunc->getAttributes());
  if (Thunk->getReturnType()->isVoidTy()) {
    Builder.CreateRetVoid();
  } else {
    Builder.CreateRet(createCast(Builder, CI, Thunk->getReturnType()));
  }

  DEBUG(dbgs() << "    writeThunk: " << Thunk->getName() << '\n');
  ++NumSwiftThunksWritten;
}

/// Replace direct callers of Old with New. Also add parameters to the call to
/// \p New, which are defined by the FuncIdx's value in \p Params.
bool SwiftMergeFunctions::replaceDirectCallers(Function *Old, Function *New,
                                   const ParamInfos &Params, unsigned FuncIdx) {
  bool AllReplaced = true;

  SmallVector<CallInst *, 8> Callers;
  
  for (Use &U : Old->uses()) {
    Instruction *I = dyn_cast<Instruction>(U.getUser());
    if (!I) {
      AllReplaced = false;
      continue;
    }
    FunctionEntry *FE = getEntry(I->getFunction());
    if (FE)
      removeEquivalenceClassFromTree(FE);
    
    CallInst *CI = dyn_cast<CallInst>(I);
    if (!CI || CI->getCalledValue() != Old) {
      AllReplaced = false;
      continue;
    }
    Callers.push_back(CI);
  }
  if (!AllReplaced)
    return false;

  for (CallInst *CI : Callers) {
    auto &Context = New->getContext();
    auto NewFuncAttrs = New->getAttributes();
    auto CallSiteAttrs = CI->getAttributes();

    CallSiteAttrs = CallSiteAttrs.addAttributes(
        Context, AttributeSet::ReturnIndex, NewFuncAttrs.getRetAttributes());

    SmallVector<Type *, 8> OldParamTypes;
    SmallVector<Value *, 16> NewArgs;
    IRBuilder<> Builder(CI);

    FunctionType *NewFuncTy = New->getFunctionType();
    (void) NewFuncTy;
    unsigned ParamIdx = 0;
    
    // Add the existing parameters.
    for (Value *OldArg : CI->arg_operands()) {
      AttributeSet Attrs = NewFuncAttrs.getParamAttributes(ParamIdx);
      if (Attrs.getNumSlots())
        CallSiteAttrs = CallSiteAttrs.addAttributes(Context, ParamIdx, Attrs);

      NewArgs.push_back(OldArg);
      OldParamTypes.push_back(OldArg->getType());
      ++ParamIdx;
    }
    // Add the new parameters.
    for (const ParamInfo &PI : Params) {
      assert(ParamIdx < NewFuncTy->getNumParams());
      NewArgs.push_back(PI.Values[FuncIdx]);
      OldParamTypes.push_back(PI.Values[FuncIdx]->getType());
      ++ParamIdx;
    }

    auto *FType = FunctionType::get(Old->getFunctionType()->getReturnType(),
                                    OldParamTypes, false);
    auto *FPtrType = PointerType::get(FType,
                        cast<PointerType>(New->getType())->getAddressSpace());

    Value *Callee = ConstantExpr::getBitCast(New, FPtrType);
    CallInst *NewCI = Builder.CreateCall(Callee, NewArgs);
    NewCI->setCallingConv(CI->getCallingConv());
    NewCI->setAttributes(CallSiteAttrs);

    CI->replaceAllUsesWith(NewCI);
    CI->eraseFromParent();
  }
  return Old->hasLocalLinkage();
}

