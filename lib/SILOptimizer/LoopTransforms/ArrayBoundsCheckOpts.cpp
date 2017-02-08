//===--- ArrayBoundsCheckOpts.cpp - Bounds check elim ---------------------===//
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

#define DEBUG_TYPE "sil-abcopts"

#include "swift/Basic/STLExtras.h"
#include "swift/AST/Builtins.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/DestructorAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/IVAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/InstructionUtils.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Debug.h"

#include <algorithm>

#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace PatternMatch;

static llvm::cl::opt<bool> ShouldReportBoundsChecks("sil-abcopts-report",
                                              llvm::cl::init(false));

static llvm::cl::opt<bool> EnableABCOpts("enable-abcopts",
                                         llvm::cl::init(true));

static llvm::cl::opt<bool> EnableABCHoisting("enable-abc-hoisting",
                                             llvm::cl::init(true));


using ArraySet = llvm::SmallPtrSet<SILValue, 16>;
// A pair of the array pointer and the array check kind (kCheckIndex or
// kCheckSubscript).
using ArrayAccessDesc = llvm::PointerIntPair<ValueBase *, 1, bool>;
using IndexedArraySet = llvm::DenseSet<std::pair<ValueBase *, ArrayAccessDesc>>;
using InstructionSet = llvm::SmallPtrSet<SILInstruction *, 16>;

/// The effect an instruction can have on array bounds.
enum class ArrayBoundsEffect {
  kNone = 0,
  kMayChangeArg, // Can only change the array argument.
  kMayChangeAny  // Might change any array.
};

static SILValue getArrayStructPointer(ArrayCallKind K, SILValue Array) {
  assert(K != ArrayCallKind::kNone);

  if (K < ArrayCallKind::kMakeMutable) {
    auto LI = dyn_cast<LoadInst>(Array);
    if (!LI) {
      return Array;
    }
    return LI->getOperand();
  }
  return Array;
}

/// Check whether the store is to the address obtained from a getElementAddress
/// semantic call.
///  %40 = function_ref @getElementAddress
///  %42 = apply %40(%28, %37)
///  %43 = struct_extract %42
///  %44 = pointer_to_address strict %43
///  store %1 to %44 : $*Int
static bool isArrayEltStore(StoreInst *SI) {
  // Strip the MarkDependenceInst (new array implementation) where the above
  // pattern looks like the following.
  // %40 = function_ref @getElementAddress
  // %41 = apply %40(%21, %35)
  // %42 = struct_element_addr %0 : $*Array<Int>, #Array._buffer
  // %43 = struct_element_addr %42 : $*_ArrayBuffer<Int>, #_ArrayBuffer._storage
  // %44 = struct_element_addr %43 : $*_BridgeStorage
  // %45 = load %44 : $*Builtin.BridgeObject
  // %46 = unchecked_ref_cast %45 : $... to $_ContiguousArrayStorageBase
  // %47 = unchecked_ref_cast %46 : $... to $Builtin.NativeObject
  // %48 = struct_extract %41 : $..., #UnsafeMutablePointer._rawValue
  // %49 = pointer_to_address %48 : $Builtin.RawPointer to strict $*Int
  // %50 = mark_dependence %49 : $*Int on %47 : $Builtin.NativeObject
  // store %1 to %50 : $*Int
  SILValue Dest = SI->getDest();
  if (auto *MD = dyn_cast<MarkDependenceInst>(Dest))
    Dest = MD->getOperand(0);

  if (auto *PtrToAddr =
          dyn_cast<PointerToAddressInst>(stripAddressProjections(Dest)))
    if (auto *SEI = dyn_cast<StructExtractInst>(PtrToAddr->getOperand())) {
      ArraySemanticsCall Call(SEI->getOperand());
      if (Call && Call.getKind() == ArrayCallKind::kGetElementAddress)
        return true;
    }
  return false;
}

static bool isReleaseSafeArrayReference(SILValue Ref,
                                        ArraySet &ReleaseSafeArrayReferences,
                                        RCIdentityFunctionInfo *RCIA) {
  auto RefRoot = RCIA->getRCIdentityRoot(Ref);
  if (ReleaseSafeArrayReferences.count(RefRoot))
    return true;
  RefRoot = getArrayStructPointer(ArrayCallKind::kCheckIndex, RefRoot);
  return ReleaseSafeArrayReferences.count(RefRoot);
}

/// Determines the kind of array bounds effect the instruction can have.
static ArrayBoundsEffect
mayChangeArraySize(SILInstruction *I, ArrayCallKind &Kind, SILValue &Array,
                   ArraySet &ReleaseSafeArrayReferences,
                   RCIdentityFunctionInfo *RCIA) {
  Array = SILValue();
  Kind = ArrayCallKind::kNone;

  // TODO: What else.
  if (isa<StrongRetainInst>(I) || isa<RetainValueInst>(I) ||
      isa<CondFailInst>(I) || isa<DeallocStackInst>(I) ||
      isa<AllocationInst>(I))
    return ArrayBoundsEffect::kNone;

  // A retain on an arbitrary class can have sideeffects because of the deinit
  // function.
  if (auto SR = dyn_cast<StrongReleaseInst>(I))
    return isReleaseSafeArrayReference(SR->getOperand(),
                                       ReleaseSafeArrayReferences, RCIA)
               ? ArrayBoundsEffect::kNone
               : ArrayBoundsEffect::kMayChangeAny;

  if (auto RV = dyn_cast<ReleaseValueInst>(I))
    return isReleaseSafeArrayReference(RV->getOperand(),
                                       ReleaseSafeArrayReferences, RCIA)
               ? ArrayBoundsEffect::kNone
               : ArrayBoundsEffect::kMayChangeAny;

  // Check array bounds semantic.
  ArraySemanticsCall ArrayCall(I);
  Kind = ArrayCall.getKind();
  if (Kind != ArrayCallKind::kNone) {
    if (Kind < ArrayCallKind::kMutateUnknown) {
      // These methods are not mutating and pass the array owned. Therefore we
      // will potentially see a load of the array struct if there are mutating
      // functions in the loop on the same array.
      Array = getArrayStructPointer(Kind, ArrayCall.getSelf());
      return ArrayBoundsEffect::kNone;
    } else if (Kind >= ArrayCallKind::kArrayInit)
      return ArrayBoundsEffect::kMayChangeAny;

    Array = ArrayCall.getSelf();
    return ArrayBoundsEffect::kMayChangeArg;
  }

  if (!I->mayHaveSideEffects())
    return ArrayBoundsEffect::kNone;

  // A store to an alloc_stack can't possibly store to the array size which is
  // stored in a runtime allocated object sub field of an alloca.
  if (auto *SI = dyn_cast<StoreInst>(I)) {
    auto Ptr = SI->getDest();
    return isa<AllocStackInst>(Ptr) || isArrayEltStore(SI)
               ? ArrayBoundsEffect::kNone
               : ArrayBoundsEffect::kMayChangeAny;
  }

  return ArrayBoundsEffect::kMayChangeAny;
}

/// Two allocations of a mutable array struct cannot reference the same
/// storage after modification. So we can treat them as not aliasing for the
/// purpose of bound checking. The change would only be tracked through one of
/// the allocations.
static bool isIdentifiedUnderlyingArrayObject(SILValue V) {
  // Allocations are safe.
  if (isa<AllocationInst>(V))
    return true;

  // Function arguments are safe.
  if (isa<SILFunctionArgument>(V))
    return true;

  return false;
}

/// Array bounds check analysis finds array bounds checks that are safe to
/// eliminate if there exists an earlier bounds check that covers the same
/// index.
///
/// We analyze a region of code for instructions that mayModify the size of an
/// array whenever we encounter an instruction that mayModify a specific array
/// or all arrays we clear the safe arrays (either a specific array or all of
/// them).
///
/// We classify instructions wrt to their effect on arrays. We are conservative,
/// any instruction that may write the size of an array (ie. an unidentified
/// store) is classified as mayModify.
///
/// Arrays are identified by their 'underlying' pointer to the array structure
/// which must either be an alloc_stack or a function argument.
///
/// Because size modifying instructions would create a copy of the storage this
/// is sufficient for the purpose of eliminating potential aliasing.
///
class ABCAnalysis {

  // List of arrays in memory which are unsafe.
  ArraySet UnsafeArrays;

  // If true, all arrays in memory are considered to be unsafe. In this case the
  // list in UnsafeArrays is not relevant.
  bool allArraysInMemoryAreUnsafe;

  ArraySet &ReleaseSafeArrayReferences;
  RCIdentityFunctionInfo *RCIA;
  bool LoopMode;

public:
  ABCAnalysis(bool loopMode, ArraySet &ReleaseSafe,
              RCIdentityFunctionInfo *rcia)
      : allArraysInMemoryAreUnsafe(false),
        ReleaseSafeArrayReferences(ReleaseSafe), RCIA(rcia),
        LoopMode(loopMode) {}

  ABCAnalysis(const ABCAnalysis &) = delete;
  ABCAnalysis &operator=(const ABCAnalysis &) = delete;

  /// Find safe array bounds check in a loop. A bounds_check is safe if no size
  /// modifying instruction to the same array has been seen so far.
  ///
  /// The code relies on isIdentifiedUnderlyingArrayObject' to make sure that a
  /// 'safe arrays' is not aliased.
  /// If an instruction is encountered that might modify any array this method
  /// stops further analysis and returns false. Otherwise, true is returned and
  /// the safe arrays can be queried.
  void analyzeBlock(SILBasicBlock *BB) {
    for (auto &Inst : *BB)
      analyzeInstruction(&Inst);
  }

  /// Returns false if the instruction may change the size of any array. All
  /// redundant safe array accesses seen up to the instruction can be removed.
  void analyze(SILInstruction *I) {
    assert(!LoopMode &&
           "This function can only be used in on cfg without loops");
    (void)LoopMode;

    analyzeInstruction(I);
  }

  /// Returns true if the Array is unsafe.
  bool isUnsafe(SILValue Array) const {
    return allArraysInMemoryAreUnsafe || UnsafeArrays.count(Array) != 0;
  }

  /// Returns true if all arrays in memory are considered to be unsafe and
  /// clears this flag.
  bool clearArraysUnsafeFlag() {
    bool arraysUnsafe = allArraysInMemoryAreUnsafe;
    allArraysInMemoryAreUnsafe = false;
    return arraysUnsafe;
  }

private:
  /// Analyze one instruction wrt. the instructions we have seen so far.
  void analyzeInstruction(SILInstruction *Inst) {
    SILValue Array;
    ArrayCallKind K;
    auto BoundsEffect =
        mayChangeArraySize(Inst, K, Array, ReleaseSafeArrayReferences, RCIA);

    if (BoundsEffect == ArrayBoundsEffect::kMayChangeAny) {
      DEBUG(llvm::dbgs() << " no safe because kMayChangeAny " << *Inst);
      allArraysInMemoryAreUnsafe = true;
      // No need to store specific arrays in this case.
      UnsafeArrays.clear();
      return;
    }

    assert(Array ||
           K == ArrayCallKind::kNone &&
               "Need to have an array for array semantic functions");

    // We need to make sure that the array container is not aliased in ways
    // that we don't understand.
    if (Array && !isIdentifiedUnderlyingArrayObject(Array)) {
      DEBUG(llvm::dbgs()
            << " not safe because of not identified underlying object "
            << *Array << " in " << *Inst);
      allArraysInMemoryAreUnsafe = true;
      // No need to store specific arrays in this case.
      UnsafeArrays.clear();
      return;
    }

    if (BoundsEffect == ArrayBoundsEffect::kMayChangeArg) {
      UnsafeArrays.insert(Array);
      return;
    }
    assert(BoundsEffect == ArrayBoundsEffect::kNone);
  }
};

// Get the pair of array and index. Because we want to disambiguate between the
// two types of check bounds checks merge in the type into the lower bit of one
// of the addresse index.
static std::pair<ValueBase *, ArrayAccessDesc>
getArrayIndexPair(SILValue Array, SILValue ArrayIndex, ArrayCallKind K) {
  assert((K == ArrayCallKind::kCheckIndex ||
          K == ArrayCallKind::kCheckSubscript) &&
         "Must be a bounds check call");
  return std::make_pair(
      Array,
      ArrayAccessDesc(ArrayIndex, K == ArrayCallKind::kCheckIndex));
}

/// Remove redundant checks in a basic block. This pass will reset the state
/// after an instruction that may modify any array allowing removal of redundant
/// checks up to that point and after that point.
static bool removeRedundantChecksInBlock(SILBasicBlock &BB, ArraySet &Arrays,
                                         RCIdentityFunctionInfo *RCIA) {
  ABCAnalysis ABC(false, Arrays, RCIA);
  IndexedArraySet RedundantChecks;
  bool Changed = false;

  DEBUG(llvm::dbgs() << "Removing in BB\n");
  DEBUG(BB.dump());

  // Process all instructions in the current block.
  for (auto Iter = BB.begin(); Iter != BB.end();) {
    auto Inst = &*Iter;
    ++Iter;

    ABC.analyze(Inst);

    if (ABC.clearArraysUnsafeFlag()) {
      // Any array may be modified -> forget everything. This is just a
      // shortcut to the isUnsafe test for a specific array below.
      RedundantChecks.clear();
      continue;
    }

    // Is this a check_bounds.
    ArraySemanticsCall ArrayCall(Inst);
    auto Kind = ArrayCall.getKind();
    if (Kind != ArrayCallKind::kCheckSubscript &&
        Kind != ArrayCallKind::kCheckIndex) {
      DEBUG(llvm::dbgs() << " not a check_bounds call " << *Inst);
      continue;
    }
    auto Array = ArrayCall.getSelf();

    // Get the underlying array pointer.
    Array = getArrayStructPointer(Kind, Array);

    // Is this an unsafe array whose size could have been changed?
    if (ABC.isUnsafe(Array)) {
      DEBUG(llvm::dbgs() << " not a safe array argument " << *Array);
      continue;
    }

    // Get the array index.
    auto ArrayIndex = ArrayCall.getIndex();
    if (!ArrayIndex)
      continue;

    auto IndexedArray =
        getArrayIndexPair(Array, ArrayIndex, Kind);
    DEBUG(llvm::dbgs() << " IndexedArray: " << *Array << " and "
                       << *ArrayIndex);

    // Saw a check for the first time.
    if (!RedundantChecks.count(IndexedArray)) {
      DEBUG(llvm::dbgs() << " first time: " << *Inst
                         << "  with array argument: " << *Array);
      RedundantChecks.insert(IndexedArray);
      continue;
    }

    // Remove the bounds check.
    ArrayCall.removeCall();
    Changed = true;
  }
  return Changed;
}

/// Walk down the dominator tree inside the loop, removing redundant checks.
static bool removeRedundantChecks(DominanceInfoNode *CurBB,
                                  ABCAnalysis &ABC,
                                  IndexedArraySet &DominatingSafeChecks,
                                  SILLoop *Loop) {
  auto *BB = CurBB->getBlock();
  if (!Loop->contains(BB))
    return false;
  bool Changed = false;

  // When we come back from the dominator tree recursion we need to remove
  // checks that we have seen for the first time.
  SmallVector<std::pair<ValueBase *, ArrayAccessDesc>, 8> SafeChecksToPop;

  // Process all instructions in the current block.
  for (auto Iter = BB->begin(); Iter != BB->end();) {
    auto Inst = &*Iter;
    ++Iter;

    // Is this a check_bounds.
    ArraySemanticsCall ArrayCall(Inst);
    auto Kind = ArrayCall.getKind();
    if (Kind != ArrayCallKind::kCheckSubscript &&
        Kind != ArrayCallKind::kCheckIndex) {
      DEBUG(llvm::dbgs() << " not a check_bounds call " << *Inst);
      continue;
    }
    auto Array = ArrayCall.getSelf();

    // Get the underlying array pointer.
    Array = getArrayStructPointer(Kind, Array);

    // Is this an unsafe array whose size could have been changed?
    if (ABC.isUnsafe(Array)) {
      DEBUG(llvm::dbgs() << " not a safe array argument " << *Array);
      continue;
    }

    // Get the array index.
    auto ArrayIndex = ArrayCall.getIndex();
    if (!ArrayIndex)
      continue;
    auto IndexedArray =
        getArrayIndexPair(Array, ArrayIndex, Kind);

    // Saw a check for the first time.
    if (!DominatingSafeChecks.count(IndexedArray)) {
      DEBUG(llvm::dbgs() << " first time: " << *Inst
                         << "  with array arg: " << *Array);
      DominatingSafeChecks.insert(IndexedArray);
      SafeChecksToPop.push_back(IndexedArray);
      continue;
    }

    // Remove the bounds check.
    ArrayCall.removeCall();
    Changed = true;
  }

  // Traverse the children in the dominator tree inside the loop.
  for (auto Child: *CurBB)
    Changed |=
        removeRedundantChecks(Child, ABC, DominatingSafeChecks, Loop);

  // Remove checks we have seen for the first time.
  std::for_each(SafeChecksToPop.begin(), SafeChecksToPop.end(),
                [&](std::pair<ValueBase *, ArrayAccessDesc> &V) {
    DominatingSafeChecks.erase(V);
  });

  return Changed;
}

static CondFailInst *hasCondFailUse(SILInstruction *I) {
    for (auto *Op : I->getUses())
      if (auto C = dyn_cast<CondFailInst>(Op->getUser()))
        return C;
    return nullptr;
}

/// Checks whether the apply instruction is checked for overflow by looking for
/// a cond_fail on the second result.
static CondFailInst *isOverflowChecked(BuiltinInst *AI) {
  for (auto *Op : AI->getUses()) {
    if (!match(Op->getUser(), m_TupleExtractInst(m_ValueBase(), 1)))
      continue;

    TupleExtractInst *TEI = cast<TupleExtractInst>(Op->getUser());
    if (CondFailInst *C = hasCondFailUse(TEI))
      return C;
  }
  return nullptr;
}

/// Look for checks that guarantee that start is less than or equal to end.
static bool isSignedLessEqual(SILValue Start, SILValue End, SILBasicBlock &BB) {

  // If we have an inclusive range "low...up" the loop exit count will be
  // "up + 1" but the overflow check is on "up".
  SILValue PreInclusiveEnd;
  if (!match(
          End,
          m_TupleExtractInst(m_ApplyInst(BuiltinValueKind::SAddOver,
                                         m_SILValue(PreInclusiveEnd), m_One()),
                             0)))
    PreInclusiveEnd = SILValue();

  bool IsPreInclusiveEndLEQ = false;
  bool IsPreInclusiveEndGTEnd = false;
  for (auto &Inst : BB)
    if (auto CF = dyn_cast<CondFailInst>(&Inst)) {
      // Try to match a cond_fail on "XOR , (SLE Start, End), 1".
      if (match(CF->getOperand(),
                m_ApplyInst(BuiltinValueKind::Xor,
                            m_ApplyInst(BuiltinValueKind::ICMP_SLE,
                                        m_Specific(Start),
                                        m_Specific(End)),
                            m_One())))
        return true;
      // Inclusive ranges will have a check on the upper value (before adding
      // one).
      if (PreInclusiveEnd) {
        if (match(CF->getOperand(),
                  m_ApplyInst(BuiltinValueKind::Xor,
                              m_ApplyInst(BuiltinValueKind::ICMP_SLE,
                                          m_Specific(Start),
                                          m_Specific(PreInclusiveEnd)),
                              m_One())))
          IsPreInclusiveEndLEQ = true;
        if (match(CF->getOperand(),
                  m_ApplyInst(BuiltinValueKind::Xor,
                              m_ApplyInst(BuiltinValueKind::ICMP_SGT,
                                          m_Specific(End),
                                          m_Specific(PreInclusiveEnd)),
                              m_One())))
          IsPreInclusiveEndGTEnd = true;
        if (IsPreInclusiveEndLEQ && IsPreInclusiveEndGTEnd)
          return true;
      }
    }

  return false;
}

static bool isLessThan(SILValue Start, SILValue End) {
  auto S = dyn_cast<IntegerLiteralInst>(Start);
  if (!S)
    return false;
  auto E = dyn_cast<IntegerLiteralInst>(End);
  if (!E)
    return false;
  return S->getValue().slt(E->getValue());
}

static BuiltinValueKind swapCmpID(BuiltinValueKind ID) {
  switch (ID) {
    case BuiltinValueKind::ICMP_EQ: return BuiltinValueKind::ICMP_EQ;
    case BuiltinValueKind::ICMP_NE: return BuiltinValueKind::ICMP_NE;
    case BuiltinValueKind::ICMP_SLE: return BuiltinValueKind::ICMP_SGE;
    case BuiltinValueKind::ICMP_SLT: return BuiltinValueKind::ICMP_SGT;
    case BuiltinValueKind::ICMP_SGE: return BuiltinValueKind::ICMP_SLE;
    case BuiltinValueKind::ICMP_SGT: return BuiltinValueKind::ICMP_SLT;
    case BuiltinValueKind::ICMP_ULE: return BuiltinValueKind::ICMP_UGE;
    case BuiltinValueKind::ICMP_ULT: return BuiltinValueKind::ICMP_UGT;
    case BuiltinValueKind::ICMP_UGE: return BuiltinValueKind::ICMP_ULE;
    case BuiltinValueKind::ICMP_UGT: return BuiltinValueKind::ICMP_ULT;
    default:
      return ID;
  }
}

static BuiltinValueKind invertCmpID(BuiltinValueKind ID) {
  switch (ID) {
    case BuiltinValueKind::ICMP_EQ: return BuiltinValueKind::ICMP_NE;
    case BuiltinValueKind::ICMP_NE: return BuiltinValueKind::ICMP_EQ;
    case BuiltinValueKind::ICMP_SLE: return BuiltinValueKind::ICMP_SGT;
    case BuiltinValueKind::ICMP_SLT: return BuiltinValueKind::ICMP_SGE;
    case BuiltinValueKind::ICMP_SGE: return BuiltinValueKind::ICMP_SLT;
    case BuiltinValueKind::ICMP_SGT: return BuiltinValueKind::ICMP_SLE;
    case BuiltinValueKind::ICMP_ULE: return BuiltinValueKind::ICMP_UGT;
    case BuiltinValueKind::ICMP_ULT: return BuiltinValueKind::ICMP_UGE;
    case BuiltinValueKind::ICMP_UGE: return BuiltinValueKind::ICMP_ULT;
    case BuiltinValueKind::ICMP_UGT: return BuiltinValueKind::ICMP_ULE;
    default:
      return ID;
  }
}

/// Checks if Start to End is the range of 0 to the count of an array.
/// Returns the array is this is the case.
static SILValue getZeroToCountArray(SILValue Start, SILValue End) {
  auto *IL = dyn_cast<IntegerLiteralInst>(Start);
  if (!IL || IL->getValue() != 0)
    return SILValue();
    
  auto *SEI = dyn_cast<StructExtractInst>(End);
  if (!SEI)
    return SILValue();
    
  ArraySemanticsCall SemCall(SEI->getOperand());
  if (SemCall.getKind() != ArrayCallKind::kGetCount)
    return SILValue();
  
  return SemCall.getSelf();
}

/// Checks whether the cond_br in the preheader's predecessor ensures that the
/// loop is only executed if "Start < End".
static bool isLessThanCheck(SILValue Start, SILValue End,
                            CondBranchInst *CondBr, SILBasicBlock *Preheader) {
  BuiltinInst *BI = dyn_cast<BuiltinInst>(CondBr->getCondition());
  if (!BI)
    return false;

  BuiltinValueKind Id = BI->getBuiltinInfo().ID;
  if (BI->getNumOperands() != 2)
    return false;

  SILValue LeftArg = BI->getOperand(0);
  SILValue RightArg = BI->getOperand(1);
  
  if (RightArg == Start) {
    std::swap(LeftArg, RightArg);
    Id = swapCmpID(Id);
  }
  if (LeftArg != Start || RightArg != End)
    return false;
  
  if (CondBr->getTrueBB() != Preheader) {
    assert(CondBr->getFalseBB() == Preheader);
    Id = invertCmpID(Id);
  }
  
  switch (Id) {
    case BuiltinValueKind::ICMP_SLT:
    case BuiltinValueKind::ICMP_ULT:
      return true;
    case BuiltinValueKind::ICMP_NE:
      // Special case: if it is a 0-to-count loop, we know that the count cannot
      // be negative. In this case the 'Start < End' check can also be done with
      // 'count != 0'.
      if (getZeroToCountArray(Start, End))
        return true;
      return false;
    default:
      return false;
  }
}

/// Checks whether there are checks in the preheader's predecessor that ensure
/// that "Start < End".
static bool isRangeChecked(SILValue Start, SILValue End,
                           SILBasicBlock *Preheader, DominanceInfo *DT) {
  // Check two constants.
  if (isLessThan(Start, End))
    return true;

  // Look for a branch on EQ around the Preheader.
  auto *PreheaderPred = Preheader->getSinglePredecessorBlock();
  if (!PreheaderPred)
    return false;
  auto *CondBr = dyn_cast<CondBranchInst>(PreheaderPred->getTerminator());
  if (!CondBr)
    return false;

  if (isLessThanCheck(Start, End, CondBr, Preheader))
    return true;

  // Walk up the dominator tree looking for a range check ("SLE Start, End").
  DominanceInfoNode *CurDTNode = DT->getNode(PreheaderPred);
  while (CurDTNode) {
    if (isSignedLessEqual(Start, End, *CurDTNode->getBlock()))
      return true;
    CurDTNode = CurDTNode->getIDom();
  }
  return false;
}

static bool dominates(DominanceInfo *DT, SILValue V, SILBasicBlock *B) {
  if (auto ValueBB = V->getParentBlock())
    return DT->dominates(ValueBB, B);
  return false;
}

/// Subtract a constant from a builtin integer value.
static SILValue getSub(SILLocation Loc, SILValue Val, unsigned SubVal,
                       SILBuilder &B) {
  SmallVector<SILValue, 4> Args(1, Val);
  Args.push_back(B.createIntegerLiteral(Loc, Val->getType(), SubVal));
  Args.push_back(B.createIntegerLiteral(
      Loc, SILType::getBuiltinIntegerType(1, B.getASTContext()), -1));

  auto *AI = B.createBuiltinBinaryFunctionWithOverflow(
      Loc, "ssub_with_overflow", Args);
  return B.createTupleExtract(Loc, AI, 0);
}

/// A canonical induction variable incremented by one from Start to End-1.
struct InductionInfo {
  SILArgument *HeaderVal;
  BuiltinInst *Inc;
  SILValue Start;
  SILValue End;
  BuiltinValueKind Cmp;
  bool IsOverflowCheckInserted;

  InductionInfo()
      : Cmp(BuiltinValueKind::None), IsOverflowCheckInserted(false) {}

  InductionInfo(SILArgument *HV, BuiltinInst *I, SILValue S, SILValue E,
                BuiltinValueKind C, bool IsOverflowChecked = false)
      : HeaderVal(HV), Inc(I), Start(S), End(E), Cmp(C),
        IsOverflowCheckInserted(IsOverflowChecked) {}

  bool isValid() { return Start && End; }
  operator bool() { return isValid(); }

  SILInstruction *getInstruction() { return Inc; }

  SILValue getFirstValue() {
    return Start;
  }

  SILValue getLastValue(SILLocation &Loc, SILBuilder &B) {
    return getSub(Loc, End, 1, B);
  }

  /// If necessary insert an overflow for this induction variable.
  /// If we compare for equality we need to make sure that the range does wrap.
  /// We would have trapped either when overflowing or when accessing an array
  /// out of bounds in the original loop.
  /// Returns true if an overflow check was inserted.
  bool checkOverflow(SILBuilder &Builder) {
    if (IsOverflowCheckInserted || Cmp != BuiltinValueKind::ICMP_EQ)
      return false;

    auto Loc = Inc->getLoc();
    auto ResultTy = SILType::getBuiltinIntegerType(1, Builder.getASTContext());
    auto *CmpSGE = Builder.createBuiltinBinaryFunction(
        Loc, "cmp_sge", Start->getType(), ResultTy, {Start, End});
    Builder.createCondFail(Loc, CmpSGE);
    IsOverflowCheckInserted = true;

    // We can now remove the cond fail on the increment the above comparison
    // guarantees that the addition won't overflow.
    auto *CondFail = isOverflowChecked(cast<BuiltinInst>(Inc));
    if (CondFail)
      CondFail->eraseFromParent();
    return true;
  }
};

/// Analyze canonical induction variables in a loop to find their start and end
/// values.
/// At the moment we only handle very simple induction variables that increment
/// by one and use equality comparison.
class InductionAnalysis {
  using InductionInfoMap = llvm::DenseMap<SILArgument *, InductionInfo *>;

  DominanceInfo *DT;
  SILBasicBlock *Preheader;
  SILBasicBlock *Header;
  SILBasicBlock *ExitingBlk;
  SILBasicBlock *ExitBlk;
  IVInfo &IVs;
  InductionInfoMap Map;
  llvm::SpecificBumpPtrAllocator<InductionInfo> Allocator;

public:
  InductionAnalysis(DominanceInfo *D, IVInfo &IVs, SILBasicBlock *Preheader,
                    SILBasicBlock *Header, SILBasicBlock *ExitingBlk,
                    SILBasicBlock *ExitBlk)
      : DT(D), Preheader(Preheader), Header(Header), ExitingBlk(ExitingBlk),
        ExitBlk(ExitBlk), IVs(IVs) {}

  InductionAnalysis(const InductionAnalysis &) = delete;
  InductionAnalysis &operator=(const InductionAnalysis &) = delete;

  bool analyze() {
    bool FoundIndVar = false;
    for (auto *Arg : Header->getArguments()) {
      // Look for induction variables.
      IVInfo::IVDesc IV;
      if (!(IV = IVs.getInductionDesc(Arg))) {
        DEBUG(llvm::dbgs() << " not an induction variable: " << *Arg);
        continue;
      }

      InductionInfo *Info;
      if (!(Info = analyzeIndVar(Arg, IV.Inc, IV.IncVal))) {
        DEBUG(llvm::dbgs() << " could not analyze the induction on: " << *Arg);
        continue;
      }

      DEBUG(llvm::dbgs() << " found an induction variable: " << *Arg);
      FoundIndVar = true;
      Map[Arg] = Info;
    }
    return FoundIndVar;
  }

  InductionInfo *operator[](SILArgument *A) {
    InductionInfoMap::iterator It =  Map.find(A);
    if (It == Map.end())
      return nullptr;
    return It->second;
  }

private:

  /// Analyze one potential induction variable starting at Arg.
  InductionInfo *analyzeIndVar(SILArgument *HeaderVal, BuiltinInst *Inc,
                               IntegerLiteralInst *IncVal) {
    if (IncVal->getValue() != 1)
      return nullptr;

    // Find the start value.
    auto *PreheaderTerm = dyn_cast<BranchInst>(Preheader->getTerminator());
    if (!PreheaderTerm)
      return nullptr;
    auto Start = PreheaderTerm->getArg(HeaderVal->getIndex());

    // Find the exit condition.
    auto CondBr = dyn_cast<CondBranchInst>(ExitingBlk->getTerminator());
    if (!CondBr)
      return nullptr;

    if (ExitBlk == CondBr->getFalseBB())
      return nullptr;
    assert(ExitBlk == CondBr->getTrueBB() &&
           "The loop's exiting blocks terminator must exit");

    auto Cond = CondBr->getCondition();
    SILValue End;

    // Look for a compare of induction variable + 1.
    // TODO: obviously we need to handle many more patterns.
    if (!match(Cond, m_ApplyInst(BuiltinValueKind::ICMP_EQ,
                                 m_TupleExtractInst(m_Specific(Inc), 0),
                                 m_SILValue(End))) &&
        !match(Cond,
               m_ApplyInst(BuiltinValueKind::ICMP_EQ, m_SILValue(End),
                           m_TupleExtractInst(m_Specific(Inc), 0)))) {
      DEBUG(llvm::dbgs() << " found no exit condition\n");
      return nullptr;
    }

    // Make sure our end value is loop invariant.
    if (!dominates(DT, End, Preheader))
      return nullptr;

    DEBUG(llvm::dbgs() << " found an induction variable (ICMP_EQ): "
                       << *HeaderVal << "  start: " << *Start
                       << "  end: " << *End);

    // Check whether the addition is overflow checked by a cond_fail or whether
    // code in the preheader's predecessor ensures that we won't overflow.
    bool IsRangeChecked = false;
    if (!isOverflowChecked(Inc)) {
      IsRangeChecked = isRangeChecked(Start, End, Preheader, DT);
      if (!IsRangeChecked)
        return nullptr;
    }
    return new (Allocator.Allocate()) InductionInfo(
        HeaderVal, Inc, Start, End, BuiltinValueKind::ICMP_EQ, IsRangeChecked);
  }
};

/// A block in the loop is guaranteed to be executed if it dominates the single
/// exiting block.
static bool isGuaranteedToBeExecuted(DominanceInfo *DT, SILBasicBlock *Block,
                                     SILBasicBlock *SingleExitingBlk) {
  // If there are multiple exiting blocks then no block in the loop is
  // guaranteed to be executed in _all_ iterations until the upper bound of the
  // induction variable is reached.
  if (!SingleExitingBlk)
    return false;
  return DT->dominates(Block, SingleExitingBlk);
}

/// Describes the access function "a[f(i)]" that is based on a canonical
/// induction variable.
class AccessFunction {
  InductionInfo *Ind;

  AccessFunction(InductionInfo *I) { Ind = I; }
public:

  operator bool() { return Ind != nullptr; }

  static AccessFunction getLinearFunction(SILValue Idx,
                                          InductionAnalysis &IndVars) {
    // Match the actual induction variable buried in the integer struct.
    // %2 = struct $Int(%1 : $Builtin.Word)
    //    = apply %check_bounds(%array, %2) : $@convention(thin) (Int, ArrayInt) -> ()
    auto ArrayIndexStruct = dyn_cast<StructInst>(Idx);
    if (!ArrayIndexStruct)
      return nullptr;

    auto AsArg =
        dyn_cast<SILArgument>(ArrayIndexStruct->getElements()[0]);
    if (!AsArg)
      return nullptr;

    if (auto *Ind = IndVars[AsArg])
      return AccessFunction(Ind);

    return nullptr;
  }

  /// Returns true if the loop iterates from 0 until count of \p Array.
  bool isZeroToCount(SILValue Array) {
    return getZeroToCountArray(Ind->Start, Ind->End) == Array;
  }

  /// Hoists the necessary check for beginning and end of the induction
  /// encapsulated by this access function to the header.
  void hoistCheckToPreheader(ArraySemanticsCall CheckToHoist,
                             SILBasicBlock *Preheader,
                             DominanceInfo *DT) {
    ApplyInst *AI = CheckToHoist;
    SILLocation Loc = AI->getLoc();
    SILBuilderWithScope Builder(Preheader->getTerminator(), AI);

    // Get the first induction value.
    auto FirstVal = Ind->getFirstValue();
    // Clone the struct for the start index.
    auto Start = cast<SILInstruction>(CheckToHoist.getIndex())
                     ->clone(Preheader->getTerminator());
    // Set the new start index to the first value of the induction.
    Start->setOperand(0, FirstVal);

      // Clone and fixup the load, retain sequence to the header.
    auto NewCheck = CheckToHoist.copyTo(Preheader->getTerminator(), DT);
    NewCheck->setOperand(1, Start);

    // Get the last induction value.
    auto LastVal = Ind->getLastValue(Loc, Builder);
    // Clone the struct for the end index.
    auto End = cast<SILInstruction>(CheckToHoist.getIndex())
                   ->clone(Preheader->getTerminator());
    // Set the new end index to the last value of the induction.
    End->setOperand(0, LastVal);

    NewCheck = CheckToHoist.copyTo(Preheader->getTerminator(), DT);
    NewCheck->setOperand(1, End);
  }
};

static bool hasArrayType(SILValue Value, SILModule &M) {
  return Value->getType().getNominalOrBoundGenericNominal() ==
           M.getASTContext().getArrayDecl();
}

/// Hoist bounds check in the loop to the loop preheader.
static bool hoistChecksInLoop(DominanceInfo *DT, DominanceInfoNode *DTNode,
                              ABCAnalysis &ABC, InductionAnalysis &IndVars,
                              SILBasicBlock *Preheader, SILBasicBlock *Header,
                              SILBasicBlock *SingleExitingBlk) {

  bool Changed = false;
  auto *CurBB = DTNode->getBlock();
  bool blockAlwaysExecutes = isGuaranteedToBeExecuted(DT, CurBB,
                                                      SingleExitingBlk);

  for (auto Iter = CurBB->begin(); Iter != CurBB->end();) {
    auto Inst = &*Iter;
    ++Iter;

    ArraySemanticsCall ArrayCall(Inst);
    auto Kind = ArrayCall.getKind();
    if (Kind != ArrayCallKind::kCheckSubscript &&
        Kind != ArrayCallKind::kCheckIndex) {
      DEBUG(llvm::dbgs() << " not a check_bounds call " << *Inst);
      continue;
    }
    auto ArrayVal = ArrayCall.getSelf();

    // Get the underlying array pointer.
    SILValue Array = getArrayStructPointer(Kind, ArrayVal);

    // The array must strictly dominate the header.
    if (!dominates(DT, Array, Preheader)) {
      DEBUG(llvm::dbgs() << " does not dominated header" << *Array);
      continue;
    }

    // Is this a safe array whose size could not have changed?
    // This is either a SILValue which is defined outside the loop or it is an
    // array, which loaded from memory and the memory is not changed in the loop.
    if (!dominates(DT, ArrayVal, Preheader) && ABC.isUnsafe(Array)) {
      DEBUG(llvm::dbgs() << " not a safe array argument " << *Array);
      continue;
    }

    // Get the array index.
    auto ArrayIndex = ArrayCall.getIndex();
    if (!ArrayIndex)
      continue;

    // Make sure we know how-to hoist the array call.
    if (!ArrayCall.canHoist(Preheader->getTerminator(), DT))
      continue;

    // Invariant check.
    if (blockAlwaysExecutes && dominates(DT, ArrayIndex, Preheader)) {
      assert(ArrayCall.canHoist(Preheader->getTerminator(), DT) &&
             "Must be able to hoist the instruction.");
      Changed = true;
      ArrayCall.hoist(Preheader->getTerminator(), DT);
      DEBUG(llvm::dbgs() << " could hoist invariant bounds check: " << *Inst);
      continue;
    }

    // Get the access function "a[f(i)]". At the moment this handles only the
    // identity function.
    auto F = AccessFunction::getLinearFunction(ArrayIndex, IndVars);
    if (!F) {
      DEBUG(llvm::dbgs() << " not a linear function " << *Inst);
      continue;
    }

    // Check if the loop iterates from 0 to the count of this array.
    if (F.isZeroToCount(ArrayVal) &&
        // This works only for Arrays but not e.g. for ArraySlice.
        hasArrayType(ArrayVal, Header->getModule())) {
      // We can remove the check. This is even possible if the block does not
      // dominate the loop exit block.
      Changed = true;
      ArrayCall.removeCall();
      DEBUG(llvm::dbgs() << "  Bounds check removed\n");
      continue;
    }
    
    // For hoisting bounds checks the block must dominate the exit block.
    if (!blockAlwaysExecutes)
      continue;

    // Hoist the access function and the check to the preheader for start and
    // end of the induction.
    assert(ArrayCall.canHoist(Preheader->getTerminator(), DT) &&
           "Must be able to hoist the call");

    F.hoistCheckToPreheader(ArrayCall, Preheader, DT);

    // Remove the old check in the loop and the match the retain with a release.
    ArrayCall.removeCall();
  
    DEBUG(llvm::dbgs() << "  Bounds check hoisted\n");
    Changed = true;
  }

  DEBUG(Preheader->getParent()->dump());
  // Traverse the children in the dominator tree.
  for (auto Child: *DTNode)
    Changed |= hoistChecksInLoop(DT, Child, ABC, IndVars, Preheader,
                                 Header, SingleExitingBlk);

  return Changed;
}


/// A dominating cond_fail on the same value ensures that this value is false.
static bool isValueKnownFalseAt(SILValue Val, SILInstruction *At,
                                DominanceInfo *DT) {
  auto *Inst = dyn_cast<SILInstruction>(Val);
  if (!Inst ||
      std::next(SILBasicBlock::iterator(Inst)) == Inst->getParent()->end())
    return false;
  auto *CF = dyn_cast<CondFailInst>(std::next(SILBasicBlock::iterator(Inst)));
  return CF && DT->properlyDominates(CF, At);
}

/// Based on the induction variable information this comparison is known to be
/// true.
static bool isComparisonKnownTrue(BuiltinInst *Builtin, InductionInfo &IndVar) {
  if (!IndVar.IsOverflowCheckInserted ||
      IndVar.Cmp != BuiltinValueKind::ICMP_EQ)
    return false;
  return match(Builtin,
               m_ApplyInst(BuiltinValueKind::ICMP_SLE, m_Specific(IndVar.Start),
                           m_Specific(IndVar.HeaderVal))) ||
         match(Builtin, m_ApplyInst(BuiltinValueKind::ICMP_SLT,
                                    m_Specific(IndVar.HeaderVal),
                                    m_Specific(IndVar.End)));
}

/// Based on the induction variable information this comparison is known to be
/// false.
static bool isComparisonKnownFalse(BuiltinInst *Builtin,
                                   InductionInfo &IndVar) {
  if (!IndVar.IsOverflowCheckInserted ||
      IndVar.Cmp != BuiltinValueKind::ICMP_EQ)
    return false;

  // Pattern match a false condition patterns that we can detect and optimize:
  // Iteration count < 0 (start)
  // Iteration count + 1 <= 0 (start)
  // Iteration count + 1 < 0 (start)
  // Iteration count + 1 == 0 (start)
  auto MatchIndVarHeader = m_Specific(IndVar.HeaderVal);
  auto MatchIncrementIndVar = m_TupleExtractInst(
      m_ApplyInst(BuiltinValueKind::SAddOver, MatchIndVarHeader, m_One()), 0);
  auto MatchIndVarStart = m_Specific(IndVar.Start);

  if (match(Builtin,
            m_ApplyInst(BuiltinValueKind::ICMP_SLT,
                        m_CombineOr(MatchIndVarHeader, MatchIncrementIndVar),
                        MatchIndVarStart)) ||
      match(Builtin, m_ApplyInst(BuiltinValueKind::ICMP_EQ,
                                 MatchIncrementIndVar, MatchIndVarStart)) ||
      match(Builtin, m_ApplyInst(BuiltinValueKind::ICMP_SLE,
                                 MatchIncrementIndVar, MatchIndVarStart))) {
    return true;
  }

  return false;
}

/// Analyze the loop for arrays that are not modified and perform dominator tree
/// based redundant bounds check removal.
static bool hoistBoundsChecks(SILLoop *Loop, DominanceInfo *DT, SILLoopInfo *LI,
                              IVInfo &IVs, ArraySet &Arrays,
                              RCIdentityFunctionInfo *RCIA, bool ShouldVerify) {
  auto *Header = Loop->getHeader();
  if (!Header) return false;

  auto *Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    // TODO: create one if necessary.
    return false;
  }

  // Only handle innermost loops for now.
  if (!Loop->getSubLoops().empty())
    return false;

  DEBUG(llvm::dbgs() << "Attempting to remove redundant checks in " << *Loop);
  DEBUG(Header->getParent()->dump());

  // Collect safe arrays. Arrays are safe if there is no function call that
  // could mutate their size in the loop.
  ABCAnalysis ABC(true, Arrays, RCIA);
  for (auto *BB : Loop->getBlocks()) {
    ABC.analyzeBlock(BB);
  }

  // Remove redundant checks down the dominator tree inside the loop,
  // starting at the header.
  // We may not go to dominated blocks outside the loop, because we didn't
  // check for safety outside the loop (with ABCAnalysis).
  IndexedArraySet DominatingSafeChecks;
  bool Changed = removeRedundantChecks(DT->getNode(Header), ABC,
                                       DominatingSafeChecks, Loop);

  if (!EnableABCHoisting)
    return Changed;

  DEBUG(llvm::dbgs() << "Attempting to hoist checks in " << *Loop);

  // Find an exiting block.
  SILBasicBlock *SingleExitingBlk = Loop->getExitingBlock();
  SILBasicBlock *ExitingBlk = SingleExitingBlk;
  SILBasicBlock *ExitBlk = Loop->getExitBlock();
  SILBasicBlock *Latch = Loop->getLoopLatch();
  if (!ExitingBlk || !Latch || !ExitBlk) {
    DEBUG(llvm::dbgs()
          << "No single exiting block or latch found\n");
    if (!Latch)
      return Changed;

    // Look back a split edge.
    if (!Loop->isLoopExiting(Latch) && Latch->getSinglePredecessorBlock() &&
        Loop->isLoopExiting(Latch->getSinglePredecessorBlock()))
      Latch = Latch->getSinglePredecessorBlock();
    if (Loop->isLoopExiting(Latch) && Latch->getSuccessors().size() == 2) {
      ExitingBlk = Latch;
      ExitBlk = Loop->contains(Latch->getSuccessors()[0])
                    ? Latch->getSuccessors()[1]
                    : Latch->getSuccessors()[0];
      DEBUG(llvm::dbgs() << "Found a latch ...\n");
    } else return Changed;
  }

  DEBUG(Preheader->getParent()->dump());

  // Find canonical induction variables.
  InductionAnalysis IndVars(DT, IVs, Preheader, Header, ExitingBlk, ExitBlk);
  bool IVarsFound = IndVars.analyze();
  if (!IVarsFound) {
    DEBUG(llvm::dbgs() << "No induction variables found\n");
  }

  // Hoist the overflow check of induction variables out of the loop. This also
  // needs to happen for memory safety. Also remove superflous range checks.
  if (IVarsFound) {
    SILValue TrueVal;
    SILValue FalseVal;
    for (auto *Arg : Header->getArguments()) {
      if (auto *IV = IndVars[Arg]) {
        SILBuilderWithScope B(Preheader->getTerminator(), IV->getInstruction());

        // Only if the loop has a single exiting block (which contains the
        // induction variable check) we may hoist the overflow check.
        if (SingleExitingBlk)
          Changed |= IV->checkOverflow(B);

        if (!IV->IsOverflowCheckInserted)
          continue;
        for (auto *BB : Loop->getBlocks())
          for (auto &Inst : *BB) {
            auto *Builtin = dyn_cast<BuiltinInst>(&Inst);
            if (!Builtin)
              continue;
            if (isComparisonKnownTrue(Builtin, *IV)) {
              if (!TrueVal)
                TrueVal = SILValue(B.createIntegerLiteral(
                    Builtin->getLoc(), Builtin->getType(), -1));
              Builtin->replaceAllUsesWith(TrueVal);
              Changed = true;
              continue;
            }
            if (isComparisonKnownFalse(Builtin, *IV)) {
              if (!FalseVal) {
                FalseVal = SILValue(B.createIntegerLiteral(
                    Builtin->getLoc(), Builtin->getType(), 0));
              }
              Builtin->replaceAllUsesWith(FalseVal);
              Changed = true;
              continue;
            }
            // Check whether a dominating check of the condition let's us
            // replace
            // the condition by false.
            SILValue Left, Right;
            if (match(Builtin, m_Or(m_SILValue(Left), m_SILValue(Right)))) {
              if (isValueKnownFalseAt(Left, Builtin, DT)) {
                if (!FalseVal)
                  FalseVal = SILValue(B.createIntegerLiteral(
                      Builtin->getLoc(), Builtin->getType(), 0));
                Builtin->setOperand(0, FalseVal);
                Changed = true;
              }
              if (isValueKnownFalseAt(Right, Builtin, DT)) {
                if (!FalseVal)
                  FalseVal = SILValue(B.createIntegerLiteral(
                      Builtin->getLoc(), Builtin->getType(), 0));
                Builtin->setOperand(1, FalseVal);
                Changed = true;
              }
            }
          }
      }
    }
  }

  DEBUG(Preheader->getParent()->dump());

  // Hoist bounds checks.
  Changed |= hoistChecksInLoop(DT, DT->getNode(Header), ABC, IndVars,
                               Preheader, Header, SingleExitingBlk);
  if (Changed) {
    Preheader->getParent()->verify();
  }
  return Changed;
}

#ifndef NDEBUG
static void reportBoundsChecks(SILFunction *F) {
  unsigned NumBCs = 0;

  F->dump();
  for (auto &BB : *F) {
    for (auto &Inst : BB) {
      ArraySemanticsCall ArrayCall(&Inst);
      auto Kind = ArrayCall.getKind();
      if (Kind != ArrayCallKind::kCheckSubscript)
        continue;
      auto Array = ArrayCall.getSelf();
      ++NumBCs;
      llvm::dbgs() << " # CheckBounds: " << Inst
                   << "     with array arg: " << *Array
                   << "     and index: " << Inst.getOperand(1);
    }
  }
  llvm::dbgs() << " ### " << NumBCs << " bounds checks in " << F->getName()
               << "\n";
}
#else
static void reportBoundsChecks(SILFunction *F) {}
#endif

namespace {

/// Remove redundant checks in basic blocks and hoist redundant checks out of
/// loops.
class ABCOpt : public SILFunctionTransform {

public:
  ABCOpt() {}

  StringRef getName() override { return "SIL Array bounds check optimization"; }

  void run() override {
    if (!EnableABCOpts)
      return;

    auto *LA = PM->getAnalysis<SILLoopAnalysis>();
    assert(LA);
    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    assert(DA);
    auto *IVA = PM->getAnalysis<IVAnalysis>();
    assert(IVA);

    SILFunction *F = getFunction();
    assert(F);
    SILLoopInfo *LI = LA->get(F);
    assert(LI);
    DominanceInfo *DT = DA->get(F);
    assert(DT);
    IVInfo &IVs = *IVA->get(F);
    auto *RCIA = getAnalysis<RCIdentityAnalysis>()->get(F);
    auto *DestAnalysis = PM->getAnalysis<DestructorAnalysis>();

    if (ShouldReportBoundsChecks) { reportBoundsChecks(F); };
    // Collect all arrays in this function. A release is only 'safe' if we know
    // its deinitializer does not have sideeffects that could cause memory
    // safety issues. A deinit could deallocate array or put a different array
    // in its location.
    ArraySet ReleaseSafeArrays;
    for (auto &BB : *F)
      for (auto &Inst : BB) {
        ArraySemanticsCall Call(&Inst);
        if (Call && Call.hasSelf()) {
          DEBUG(llvm::dbgs() << "Gathering " << *(ApplyInst*)Call);
          auto rcRoot = RCIA->getRCIdentityRoot(Call.getSelf());
          // Check the type of the array. We need to have an array element type
          // that is not calling a deinit function.
          if (DestAnalysis->mayStoreToMemoryOnDestruction(rcRoot->getType()))
            continue;

          ReleaseSafeArrays.insert(rcRoot);
          ReleaseSafeArrays.insert(
              getArrayStructPointer(ArrayCallKind::kCheckIndex, rcRoot));
        }
      }

    // Remove redundant checks on a per basic block basis.
    bool Changed = false;
    for (auto &BB : *F)
      Changed |= removeRedundantChecksInBlock(BB, ReleaseSafeArrays, RCIA);

    if (ShouldReportBoundsChecks) { reportBoundsChecks(F); };

    bool ShouldVerify = getOptions().VerifyAll;

    if (LI->empty()) {
      DEBUG(llvm::dbgs() << "No loops in " << F->getName() << "\n");
    } else {

      // Remove redundant checks along the dominator tree in a loop and hoist
      // checks.
      for (auto *LoopIt : *LI) {
        // Process loops recursively bottom-up in the loop tree.
        SmallVector<SILLoop *, 8> Worklist;
        Worklist.push_back(LoopIt);
        for (unsigned i = 0; i < Worklist.size(); ++i) {
          auto *L = Worklist[i];
          for (auto *SubLoop : *L)
            Worklist.push_back(SubLoop);
        }

        while (!Worklist.empty()) {
          Changed |= hoistBoundsChecks(Worklist.pop_back_val(), DT, LI, IVs,
                                       ReleaseSafeArrays, RCIA, ShouldVerify);
        }
      }

      if (ShouldReportBoundsChecks) { reportBoundsChecks(F); };
    }

    if (Changed) {
      PM->invalidateAnalysis(F,
                          SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }
};
} // end anonymous namespace

SILTransform *swift::createABCOpt() {
  return new ABCOpt();
}
