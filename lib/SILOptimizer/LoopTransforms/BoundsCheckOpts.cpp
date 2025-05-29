//===--- BoundsCheckOpts.cpp - Bounds check elimination -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-bcopts"

#include "swift/AST/Builtins.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/DestructorAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/IVAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"

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

static llvm::cl::opt<bool> ShouldReportBoundsChecks("sil-bcopts-report",
                                                    llvm::cl::init(false));

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
    auto LI = dyn_cast<LoadInst>(lookThroughCopyValueInsts(Array));
    if (!LI) {
      return Array;
    }
    return LI->getOperand();
  }
  return Array;
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
      isa<CopyValueInst>(I) || isa<CondFailInst>(I) ||
      isa<DeallocStackInst>(I) || isa<AllocationInst>(I))
    return ArrayBoundsEffect::kNone;

  // A release on an arbitrary class can have sideeffects because of the deinit
  // function.
  if (isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I) ||
      isa<DestroyValueInst>(I))
    return isReleaseSafeArrayReference(I->getOperand(0),
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
    if (SI->getOwnershipQualifier() == StoreOwnershipQualifier::Assign) {
      // store [assign] can call a destructor with unintended effects
      return ArrayBoundsEffect::kMayChangeAny;
    }
    auto Ptr = SI->getDest();
    return isa<AllocStackInst>(Ptr) || isAddressOfArrayElement(SI->getDest())
               ? ArrayBoundsEffect::kNone
               : ArrayBoundsEffect::kMayChangeAny;
  }

  if (isa<LoadInst>(I))
    return ArrayBoundsEffect::kNone;

  if (isa<BeginBorrowInst>(I) || isa<EndBorrowInst>(I))
    return ArrayBoundsEffect::kNone;

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

  auto rootVal = lookThroughAddressAndValueProjections(V);
  if (rootVal != V) {
    return isIdentifiedUnderlyingArrayObject(rootVal);
  }
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
      LLVM_DEBUG(llvm::dbgs() << " not safe because kMayChangeAny " << *Inst);
      allArraysInMemoryAreUnsafe = true;
      // No need to store specific arrays in this case.
      UnsafeArrays.clear();
      return;
    }

    assert(Array || K == ArrayCallKind::kNone &&
                        "Need to have an array for array semantic functions");

    // We need to make sure that the array container is not aliased in ways
    // that we don't understand.
    if (Array && !isIdentifiedUnderlyingArrayObject(Array)) {
      LLVM_DEBUG(llvm::dbgs()
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
// of the addresses' index.
static std::pair<ValueBase *, ArrayAccessDesc>
getArrayIndexPair(SILValue Array, SILValue ArrayIndex, ArrayCallKind K) {
  assert((K == ArrayCallKind::kCheckIndex ||
          K == ArrayCallKind::kCheckSubscript) &&
         "Must be a bounds check call");
  return std::make_pair(
      Array, ArrayAccessDesc(ArrayIndex, K == ArrayCallKind::kCheckIndex));
}

static CondFailInst *hasCondFailUse(SILValue V) {
  for (auto *Op : V->getUses())
    if (auto C = dyn_cast<CondFailInst>(Op->getUser()))
      return C;
  return nullptr;
}

/// Checks whether the apply instruction is checked for overflow by looking for
/// a cond_fail on the second result.
static CondFailInst *isOverflowChecked(BuiltinInst *AI) {
  for (auto *Op : AI->getUses()) {
    if (!match(Op->getUser(), m_TupleExtractOperation(m_ValueBase(), 1)))
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
  if (!match(End, m_TupleExtractOperation(
                      m_ApplyInst(BuiltinValueKind::SAddOver,
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
                                        m_Specific(Start), m_Specific(End)),
                            m_One())))
        return true;
      // Try to match a cond_fail on "SLT End, Start".
      if (match(CF->getOperand(),
                m_ApplyInst(BuiltinValueKind::ICMP_SLT, m_Specific(End),
                            m_Specific(Start))))
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
                  m_ApplyInst(BuiltinValueKind::ICMP_SLT,
                              m_Specific(PreInclusiveEnd), m_Specific(Start))))
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
  case BuiltinValueKind::ICMP_EQ:
    return BuiltinValueKind::ICMP_EQ;
  case BuiltinValueKind::ICMP_NE:
    return BuiltinValueKind::ICMP_NE;
  case BuiltinValueKind::ICMP_SLE:
    return BuiltinValueKind::ICMP_SGE;
  case BuiltinValueKind::ICMP_SLT:
    return BuiltinValueKind::ICMP_SGT;
  case BuiltinValueKind::ICMP_SGE:
    return BuiltinValueKind::ICMP_SLE;
  case BuiltinValueKind::ICMP_SGT:
    return BuiltinValueKind::ICMP_SLT;
  case BuiltinValueKind::ICMP_ULE:
    return BuiltinValueKind::ICMP_UGE;
  case BuiltinValueKind::ICMP_ULT:
    return BuiltinValueKind::ICMP_UGT;
  case BuiltinValueKind::ICMP_UGE:
    return BuiltinValueKind::ICMP_ULE;
  case BuiltinValueKind::ICMP_UGT:
    return BuiltinValueKind::ICMP_ULT;
  default:
    return ID;
  }
}

static BuiltinValueKind invertCmpID(BuiltinValueKind ID) {
  switch (ID) {
  case BuiltinValueKind::ICMP_EQ:
    return BuiltinValueKind::ICMP_NE;
  case BuiltinValueKind::ICMP_NE:
    return BuiltinValueKind::ICMP_EQ;
  case BuiltinValueKind::ICMP_SLE:
    return BuiltinValueKind::ICMP_SGT;
  case BuiltinValueKind::ICMP_SLT:
    return BuiltinValueKind::ICMP_SGE;
  case BuiltinValueKind::ICMP_SGE:
    return BuiltinValueKind::ICMP_SLT;
  case BuiltinValueKind::ICMP_SGT:
    return BuiltinValueKind::ICMP_SLE;
  case BuiltinValueKind::ICMP_ULE:
    return BuiltinValueKind::ICMP_UGT;
  case BuiltinValueKind::ICMP_ULT:
    return BuiltinValueKind::ICMP_UGE;
  case BuiltinValueKind::ICMP_UGE:
    return BuiltinValueKind::ICMP_ULT;
  case BuiltinValueKind::ICMP_UGT:
    return BuiltinValueKind::ICMP_ULE;
  default:
    return ID;
  }
}

/// Checks if Start to End is the range of 0 to the count of an array or a fixed
/// storage type. Returns the self value if this is the case.
static SILValue getZeroToCountOfSelf(SILValue start, SILValue end) {
  auto *intLiteral = dyn_cast<IntegerLiteralInst>(start);
  if (!intLiteral || intLiteral->getValue() != 0) {
    return SILValue();
  }
  auto *sei = dyn_cast<StructExtractInst>(end);
  if (!sei) {
    return SILValue();
  }
  auto *applyInst = dyn_cast<ApplyInst>(sei->getOperand());
  if (!applyInst) {
    return SILValue();
  }
  auto *callee = applyInst->getReferencedFunctionOrNull();
  if (!callee) {
    return SILValue();
  }
  for (auto attr : callee->getSemanticsAttrs()) {
    if (attr == "array.get_count" || attr == "fixed_storage.get_count") {
      return applyInst->hasSelfArgument() ? applyInst->getSelfArgument()
                                          : SILValue();
    }
  }
  return SILValue();
}

/// Checks whether the cond_br in the preheader's predecessor ensures that the
/// loop is only executed if "Start < End".
static bool isLessThanCheck(SILValue Start, SILValue End,
                            CondBranchInst *CondBr, SILBasicBlock *Preheader) {
  auto *BI = dyn_cast<BuiltinInst>(CondBr->getCondition());
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
    return getZeroToCountOfSelf(Start, End);
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
  if (CondBr && isLessThanCheck(Start, End, CondBr, Preheader))
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

static SILValue getAdd(SILLocation Loc, SILValue Val, unsigned AddVal,
                       SILBuilder &B) {
  SmallVector<SILValue, 4> Args(1, Val);
  Args.push_back(B.createIntegerLiteral(Loc, Val->getType(), AddVal));
  Args.push_back(B.createIntegerLiteral(
      Loc, SILType::getBuiltinIntegerType(1, B.getASTContext()), -1));

  auto *AI = B.createBuiltinBinaryFunctionWithOverflow(
      Loc, "sadd_with_overflow", Args);
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

  SILValue getFirstValue(SILLocation loc, SILBuilder &B, unsigned AddVal) {
    return AddVal != 0 ? getAdd(loc, Start, AddVal, B) : Start;
  }

  SILValue getLastValue(SILLocation loc, SILBuilder &B, unsigned SubVal) {
    return SubVal != 0 ? getSub(loc, End, SubVal, B) : End;
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
    Builder.createCondFail(Loc, CmpSGE, "loop induction variable overflowed");
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

  bool analyze() {
    bool FoundIndVar = false;
    for (auto *Arg : Header->getArguments()) {
      // Look for induction variables.
      IVInfo::IVDesc IV;
      if (!(IV = IVs.getInductionDesc(Arg))) {
        LLVM_DEBUG(llvm::dbgs() << " not an induction variable: " << *Arg);
        continue;
      }

      InductionInfo *Info;
      if (!(Info = analyzeIndVar(Arg, IV.Inc, IV.IncVal))) {
        LLVM_DEBUG(llvm::dbgs()
                   << " could not analyze the induction on: " << *Arg);
        continue;
      }

      LLVM_DEBUG(llvm::dbgs() << " found an induction variable: " << *Arg);
      FoundIndVar = true;
      Map[Arg] = Info;
    }
    return FoundIndVar;
  }

  InductionInfo *operator[](SILArgument *A) {
    InductionInfoMap::iterator It = Map.find(A);
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
                                 m_TupleExtractOperation(m_Specific(Inc), 0),
                                 m_SILValue(End))) &&
        !match(Cond,
               m_ApplyInst(BuiltinValueKind::ICMP_EQ, m_SILValue(End),
                           m_TupleExtractOperation(m_Specific(Inc), 0)))) {
      LLVM_DEBUG(llvm::dbgs() << " found no exit condition\n");
      return nullptr;
    }

    // Make sure our end value is loop invariant.
    if (!dominates(DT, End, Preheader))
      return nullptr;

    LLVM_DEBUG(llvm::dbgs()
               << " found an induction variable (ICMP_EQ): " << *HeaderVal
               << "  start: " << *Start << "  end: " << *End);

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
  bool preIncrement;

  AccessFunction(InductionInfo *I, bool isPreIncrement = false)
      : Ind(I), preIncrement(isPreIncrement) {}

public:
  operator bool() { return Ind != nullptr; }

  static AccessFunction getLinearFunction(SILValue Idx,
                                          InductionAnalysis &IndVars) {
    // Match the actual induction variable buried in the integer struct.
    // bb(%ivar)
    // %2 = struct $Int(%ivar : $Builtin.Word)
    //    = apply %check_bounds(%array, %2) :
    // or
    // bb(%ivar1)
    // %ivar2 = builtin "sadd_with_overflow_Int64"(%ivar1,...)
    // %t = tuple_extract %ivar2
    // %s = struct $Int(%t : $Builtin.Word)
    //    = apply %check_bounds(%array, %s) :

    bool preIncrement = false;

    auto ArrayIndexStruct = dyn_cast<StructInst>(Idx);
    if (!ArrayIndexStruct)
      return nullptr;

    auto AsArg = dyn_cast<SILArgument>(ArrayIndexStruct->getElements()[0]);

    if (!AsArg) {
      auto *TupleExtract =
          dyn_cast<TupleExtractInst>(ArrayIndexStruct->getElements()[0]);

      if (!TupleExtract) {
        return nullptr;
      }

      auto *Builtin = dyn_cast<BuiltinInst>(TupleExtract->getOperand());
      if (!Builtin || Builtin->getBuiltinKind() != BuiltinValueKind::SAddOver) {
        return nullptr;
      }

      AsArg = dyn_cast<SILArgument>(Builtin->getArguments()[0]);
      if (!AsArg) {
        return nullptr;
      }

      auto *incrVal = dyn_cast<IntegerLiteralInst>(Builtin->getArguments()[1]);
      if (!incrVal || incrVal->getValue() != 1)
        return nullptr;

      preIncrement = true;
    }

    if (auto *Ind = IndVars[AsArg])
      return AccessFunction(Ind, preIncrement);

    return nullptr;
  }

  /// Returns true if the loop iterates from 0 until count of \p selfValue.
  bool isZeroToCount(SILValue selfValue) {
    return getZeroToCountOfSelf(Ind->Start, Ind->End) == selfValue;
  }

  SILValue getFirstValue(SILInstruction *insertPt) {
    SILBuilderWithScope builder(insertPt);
    auto firstValue =
        Ind->getFirstValue(insertPt->getLoc(), builder, preIncrement ? 1 : 0);
    auto intType = SILType::getPrimitiveObjectType(
        builder.getASTContext().getIntType()->getCanonicalType());
    return builder.createStruct(insertPt->getLoc(), intType, {firstValue});
  }

  SILValue getLastValue(SILInstruction *insertPt) {
    SILBuilderWithScope builder(insertPt);
    auto lastValue =
        Ind->getLastValue(insertPt->getLoc(), builder, preIncrement ? 0 : 1);
    auto intType = SILType::getPrimitiveObjectType(
        builder.getASTContext().getIntType()->getCanonicalType());
    return builder.createStruct(insertPt->getLoc(), intType, {lastValue});
  }

  /// Hoists the necessary check for beginning and end of the induction
  /// encapsulated by this access function to the header.
  void hoistCheckToPreheader(ArraySemanticsCall CheckToHoist,
                             SILBasicBlock *Preheader, DominanceInfo *DT) {
    ApplyInst *AI = CheckToHoist;
    SILLocation Loc = AI->getLoc();
    SILBuilderWithScope Builder(Preheader->getTerminator(), AI);

    // Get the first induction value.
    auto FirstVal = Ind->getFirstValue(Loc, Builder, preIncrement ? 1 : 0);
    // Clone the struct for the start index.
    auto Start = cast<SingleValueInstruction>(CheckToHoist.getIndex())
                     ->clone(Preheader->getTerminator());
    // Set the new start index to the first value of the induction.
    Start->setOperand(0, FirstVal);

    // Clone and fixup the load, retain sequence to the header.
    auto NewCheck = CheckToHoist.copyTo(Preheader->getTerminator(), DT);
    NewCheck->setOperand(1, Start);

    // Get the last induction value.
    auto LastVal = Ind->getLastValue(Loc, Builder, preIncrement ? 0 : 1);
    // Clone the struct for the end index.
    auto End = cast<SingleValueInstruction>(CheckToHoist.getIndex())
                   ->clone(Preheader->getTerminator());
    // Set the new end index to the last value of the induction.
    End->setOperand(0, LastVal);

    NewCheck = CheckToHoist.copyTo(Preheader->getTerminator(), DT);
    NewCheck->setOperand(1, End);
  }
};

/// A dominating cond_fail on the same value ensures that this value is false.
static bool isValueKnownFalseAt(SILValue Val, SILInstruction *At,
                                DominanceInfo *DT) {
  auto *Inst = Val->getDefiningInstruction();
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
  auto MatchIncrementIndVar = m_TupleExtractOperation(
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
#endif

namespace {

// Should be more than enough to cover "usual" functions.
static constexpr int maxRecursionDepth = 500;

/// Remove redundant checks in basic blocks and hoist redundant checks out of
/// loops.
class BoundsCheckOpts : public SILFunctionTransform {
private:
  SILLoopInfo *LI;
  DominanceInfo *DT;
  IVInfo *IVs;
  RCIdentityFunctionInfo *RCIA;
  DestructorAnalysis *DestAnalysis;
  // Arrays with element type that does not call a deinit function.
  ArraySet releaseSafeArrays;

  /// Collect all release safe arrays in this function. A release is only 'safe'
  /// if we know its deinitializer does not have sideeffects that could cause
  /// memory safety issues. A deinit could deallocate array or put a different
  /// array in its location.
  void collectReleaseSafeArrays();

  bool optimizeBoundsChecksInBlocks();

  bool optimizeBoundsChecksInLoops();

  std::pair<bool, std::optional<InductionAnalysis>>
  findAndOptimizeInductionVariables(SILLoop *loop);

  bool
  optimizeArrayBoundsCheckInLoop(SILLoop *loop,
                                 std::optional<InductionAnalysis> &indVars);

  bool optimizeFixedStorageBoundsCheckInLoop(
      SILLoop *loop, std::optional<InductionAnalysis> &indVars);

  /// Remove redundant checks in a basic block. This function will reset the
  /// state after an instruction that may modify any array allowing removal of
  /// redundant checks up to that point and after that point.
  bool removeRedundantArrayChecksInBlock(SILBasicBlock &BB);
  /// Hoist or remove redundant bound checks in \p Loop
  bool optimizeArrayBoundsCheckInLoop(SILLoop *Loop);
  /// Walk down the dominator tree inside the loop, removing redundant checks.
  bool removeRedundantArrayBoundsChecksInLoop(
      DominanceInfoNode *CurBB, ABCAnalysis &ABC,
      IndexedArraySet &DominatingSafeChecks, SILLoop *Loop, int recursionDepth);
  /// Analyze the loop for arrays that are not modified and perform dominator
  /// tree based redundant bounds check removal.
  bool hoistArrayBoundsChecksInLoop(SILLoop *loop,
                                    DominanceInfoNode *currentNode,
                                    ABCAnalysis &abcAnalysis,
                                    InductionAnalysis &indVars,
                                    int recursionDepth);

  bool hoistFixedStorageBoundsChecksInLoop(SILLoop *loop,
                                           DominanceInfoNode *currentNode,
                                           InductionAnalysis &indVars,
                                           int recursionDepth);

  bool removeRedundantFixedStorageBoundsChecksInLoop(
      SILLoop *loop, DominanceInfoNode *currentNode,
      llvm::DenseSet<std::pair<SILValue, SILValue>> &dominatingSafeChecks,
      int recursionDepth);

public:
  void run() override {
    bool changed = false;
    SILFunction *func = getFunction();
    LI = PM->getAnalysis<SILLoopAnalysis>()->get(func);
    DT = PM->getAnalysis<DominanceAnalysis>()->get(func);
    IVs = PM->getAnalysis<IVAnalysis>()->get(func);
    RCIA = PM->getAnalysis<RCIdentityAnalysis>()->get(func);
    DestAnalysis = PM->getAnalysis<DestructorAnalysis>();

#ifndef NDEBUG
    if (ShouldReportBoundsChecks) {
      reportBoundsChecks(func);
    }
#endif
    LLVM_DEBUG(llvm::dbgs()
               << "BoundsCheckOpts on function: " << func->getName() << "\n");

    collectReleaseSafeArrays();
    changed |= optimizeBoundsChecksInBlocks();
    changed |= optimizeBoundsChecksInLoops();

#ifndef NDEBUG
    if (ShouldReportBoundsChecks) {
      reportBoundsChecks(func);
    }
#endif

    if (changed) {
      PM->invalidateAnalysis(
          func, SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }
};

void BoundsCheckOpts::collectReleaseSafeArrays() {
  auto *func = getFunction();

  for (auto &block : *func) {
    for (auto &inst : block) {
      ArraySemanticsCall semanticsCall(&inst);
      if (!semanticsCall || !semanticsCall.hasSelf()) {
        continue;
      }
      LLVM_DEBUG(llvm::dbgs() << "Gathering " << *(ApplyInst *)semanticsCall);
      auto rcRoot = RCIA->getRCIdentityRoot(semanticsCall.getSelf());
      // Check the type of the array. We need to have an array element type
      // that is not calling a deinit function.
      if (DestAnalysis->mayStoreToMemoryOnDestruction(rcRoot->getType()))
        continue;
      LLVM_DEBUG(llvm::dbgs() << "ReleaseSafeArray: " << rcRoot << "\n");
      releaseSafeArrays.insert(rcRoot);
      releaseSafeArrays.insert(
          getArrayStructPointer(ArrayCallKind::kCheckIndex, rcRoot));
    }
  }
}

bool BoundsCheckOpts::optimizeBoundsChecksInBlocks() {
  bool changed = false;
  for (auto &block : *getFunction()) {
    changed |= removeRedundantArrayChecksInBlock(block);
  }
  return changed;
}

bool BoundsCheckOpts::optimizeBoundsChecksInLoops() {
  bool changed = false;
  // Process loops recursively bottom-up in the loop tree.
  for (auto *loop : *LI) {
    SmallVector<SILLoop *, 8> worklist;
    worklist.push_back(loop);
    for (unsigned i = 0; i < worklist.size(); ++i) {
      for (auto *subLoop : *worklist[i]) {
        worklist.push_back(subLoop);
      }
    }

    while (!worklist.empty()) {
      changed |= optimizeArrayBoundsCheckInLoop(worklist.pop_back_val());
    }
  }
  return changed;
}

bool BoundsCheckOpts::removeRedundantArrayChecksInBlock(SILBasicBlock &BB) {
  ABCAnalysis ABC(false, releaseSafeArrays, RCIA);
  IndexedArraySet RedundantChecks;
  bool Changed = false;

  LLVM_DEBUG(llvm::dbgs() << "Removing in BB" << BB.getDebugID() << "\n");

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
      continue;
    }
    auto Array = ArrayCall.getSelf();

    // Get the underlying array pointer.
    Array = getArrayStructPointer(Kind, Array);

    // Is this an unsafe array whose size could have been changed?
    if (ABC.isUnsafe(Array)) {
      LLVM_DEBUG(llvm::dbgs() << " not a safe array argument " << *Array);
      continue;
    }

    // Get the array index.
    auto ArrayIndex = ArrayCall.getIndex();
    if (!ArrayIndex)
      continue;

    auto IndexedArray = getArrayIndexPair(Array, ArrayIndex, Kind);
    LLVM_DEBUG(llvm::dbgs()
               << " IndexedArray: " << *Array << " and " << *ArrayIndex);

    // Saw a check for the first time.
    if (!RedundantChecks.count(IndexedArray)) {
      LLVM_DEBUG(llvm::dbgs() << " first time: " << *Inst
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

bool BoundsCheckOpts::removeRedundantArrayBoundsChecksInLoop(
    DominanceInfoNode *CurBB, ABCAnalysis &ABC,
    IndexedArraySet &DominatingSafeChecks, SILLoop *Loop, int recursionDepth) {
  auto *BB = CurBB->getBlock();
  if (!Loop->contains(BB))
    return false;

  // Avoid a stack overflow for very deep dominator trees.
  if (recursionDepth >= maxRecursionDepth)
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
      continue;
    }
    auto Array = ArrayCall.getSelf();

    // Get the underlying array pointer.
    Array = getArrayStructPointer(Kind, Array);

    // Is this an unsafe array whose size could have been changed?
    if (ABC.isUnsafe(Array)) {
      LLVM_DEBUG(llvm::dbgs() << " not a safe array argument " << *Array);
      continue;
    }

    // Get the array index.
    auto ArrayIndex = ArrayCall.getIndex();
    if (!ArrayIndex)
      continue;
    auto IndexedArray = getArrayIndexPair(Array, ArrayIndex, Kind);

    // Saw a check for the first time.
    if (!DominatingSafeChecks.count(IndexedArray)) {
      LLVM_DEBUG(llvm::dbgs()
                 << " first time: " << *Inst << "  with array arg: " << *Array);
      DominatingSafeChecks.insert(IndexedArray);
      SafeChecksToPop.push_back(IndexedArray);
      continue;
    }

    // Remove the bounds check.
    ArrayCall.removeCall();
    Changed = true;
  }

  // Traverse the children in the dominator tree inside the loop.
  for (auto Child : *CurBB)
    Changed |= removeRedundantArrayBoundsChecksInLoop(
        Child, ABC, DominatingSafeChecks, Loop, recursionDepth + 1);

  // Remove checks we have seen for the first time.
  std::for_each(SafeChecksToPop.begin(), SafeChecksToPop.end(),
                [&](std::pair<ValueBase *, ArrayAccessDesc> &V) {
                  DominatingSafeChecks.erase(V);
                });

  return Changed;
}

bool BoundsCheckOpts::optimizeArrayBoundsCheckInLoop(SILLoop *loop) {
  auto *header = loop->getHeader();
  if (!header) {
    return false;
  }

  auto *preheader = loop->getLoopPreheader();
  if (!preheader) {
    // TODO: create one if necessary.
    return false;
  }

  // Only handle innermost loops for now.
  if (!loop->getSubLoops().empty()) {
    return false;
  }

  LLVM_DEBUG(
      llvm::dbgs() << "Attempting to remove redundant array bounds checks in "
                   << *loop);

  // Collect safe arrays. Arrays are safe if there is no function call that
  // could mutate their size in the loop.
  ABCAnalysis ABC(true, releaseSafeArrays, RCIA);
  for (auto *BB : loop->getBlocks()) {
    ABC.analyzeBlock(BB);
  }

  bool changed = false;
  auto result = findAndOptimizeInductionVariables(loop);
  changed |= result.first;
  auto indVars = std::move(result.second);
  changed |= optimizeArrayBoundsCheckInLoop(loop, indVars);
  changed |= optimizeFixedStorageBoundsCheckInLoop(loop, indVars);

  if (changed) {
    preheader->getParent()->verify(
        getAnalysis<BasicCalleeAnalysis>()->getCalleeCache());
  }
  return changed;
}

std::pair<bool, std::optional<InductionAnalysis>>
BoundsCheckOpts::findAndOptimizeInductionVariables(SILLoop *loop) {
  SILBasicBlock *singleExitingBlock = loop->getExitingBlock();
  SILBasicBlock *exitingBlk = singleExitingBlock;
  SILBasicBlock *exitBlock = loop->getExitBlock();
  SILBasicBlock *latch = loop->getLoopLatch();
  if (!exitingBlk || !latch || !exitBlock) {
    if (!latch) {
      LLVM_DEBUG(llvm::dbgs() << "No latch found\n");
      return {false, std::nullopt};
    }
    if (!loop->isLoopExiting(latch) || latch->getSuccessors().size() != 2) {
      return {false, std::nullopt};
    }
    exitingBlk = latch;
    exitBlock = loop->contains(latch->getSuccessors()[0])
                    ? latch->getSuccessors()[1]
                    : latch->getSuccessors()[0];
    LLVM_DEBUG(llvm::dbgs() << "Found a latch ...\n");
  }

  // Find canonical induction variables.
  InductionAnalysis indVars(DT, *IVs, loop->getLoopPreheader(),
                            loop->getHeader(), exitingBlk, exitBlock);
  bool found = indVars.analyze();
  if (!found) {
    LLVM_DEBUG(llvm::dbgs() << "No induction variables found\n");
    return {false, std::nullopt};
  }

  // Hoist the overflow check of induction variables out of the loop. This also
  // needs to happen for memory safety. Also remove superfluous range checks.
  bool changed = false;
  SILValue trueVal, falseVal;
  for (auto *arg : loop->getHeader()->getArguments()) {
    if (auto *ivar = indVars[arg]) {
      SILBuilderWithScope builder(loop->getLoopPreheader()->getTerminator(),
                                  ivar->getInstruction());

      // Only if the loop has a single exiting block (which contains the
      // induction variable check) we may hoist the overflow check.
      if (singleExitingBlock) {
        changed |= ivar->checkOverflow(builder);
        if (!ivar->IsOverflowCheckInserted) {
          continue;
        }
      }
      for (auto *block : loop->getBlocks()) {
        for (auto &inst : *block) {
          auto *builtin = dyn_cast<BuiltinInst>(&inst);
          if (!builtin) {
            continue;
          }
          if (isComparisonKnownTrue(builtin, *ivar)) {
            if (!trueVal)
              trueVal = builder.createIntegerLiteral(builtin->getLoc(),
                                                     builtin->getType(), -1);
            builtin->replaceAllUsesWith(trueVal);
            changed = true;
            continue;
          }
          if (isComparisonKnownFalse(builtin, *ivar)) {
            if (!falseVal) {
              falseVal = builder.createIntegerLiteral(builtin->getLoc(),
                                                      builtin->getType(), 0);
            }
            builtin->replaceAllUsesWith(falseVal);
            changed = true;
            continue;
          }
          // Check whether a dominating check of the condition let's us
          // replace
          // the condition by false.
          SILValue left, right;
          if (match(builtin, m_Or(m_SILValue(left), m_SILValue(right)))) {
            if (isValueKnownFalseAt(left, builtin, DT)) {
              if (!falseVal) {
                falseVal = builder.createIntegerLiteral(builtin->getLoc(),
                                                        builtin->getType(), 0);
              }
              builtin->setOperand(0, falseVal);
              changed = true;
            }
            if (isValueKnownFalseAt(right, builtin, DT)) {
              if (!falseVal) {
                falseVal = builder.createIntegerLiteral(builtin->getLoc(),
                                                        builtin->getType(), 0);
              }
              builtin->setOperand(1, falseVal);
              changed = true;
            }
          }
        }
      }
    }
  }
  return {changed, std::make_optional(std::move(indVars))};
}

bool BoundsCheckOpts::optimizeArrayBoundsCheckInLoop(
    SILLoop *loop, std::optional<InductionAnalysis> &indVars) {

  // Collect safe arrays. Arrays are safe if there is no function call that
  // could mutate their size in the loop.
  ABCAnalysis abcAnalysis(true, releaseSafeArrays, RCIA);
  for (auto *block : loop->getBlocks()) {
    abcAnalysis.analyzeBlock(block);
  }

  // Remove redundant checks down the dominator tree inside the loop,
  // starting at the header.
  IndexedArraySet dominatingSafeArrayChecks;
  bool changed = removeRedundantArrayBoundsChecksInLoop(
      DT->getNode(loop->getHeader()), abcAnalysis, dominatingSafeArrayChecks,
      loop,
      /*recursionDepth*/ 0);

  if (!EnableABCHoisting) {
    return changed;
  }

  LLVM_DEBUG(llvm::dbgs() << "Attempting to hoist array bounds checks in "
                          << *loop);

  if (!indVars) {
    LLVM_DEBUG(llvm::dbgs() << "No induction variables found\n");
    return changed;
  }

  // Hoist bounds checks.
  changed |=
      hoistArrayBoundsChecksInLoop(loop, DT->getNode(loop->getHeader()),
                                   abcAnalysis, *indVars, /*recursionDepth*/ 0);
  return changed;
}

bool BoundsCheckOpts::optimizeFixedStorageBoundsCheckInLoop(
    SILLoop *loop, std::optional<InductionAnalysis> &indVars) {
  // Try removing redundant bounds checks in the loop.
  LLVM_DEBUG(llvm::dbgs() << "Attempting to eliminate redundant bounds checks "
                             "for Span and InlineArray in "
                          << *loop);
  llvm::DenseSet<std::pair<SILValue, SILValue>>
      dominatingSafeFixedStorageChecks;
  bool changed = removeRedundantFixedStorageBoundsChecksInLoop(
      loop, DT->getNode(loop->getHeader()), dominatingSafeFixedStorageChecks,
      /*recursionDepth*/ 0);

  // Try hoisting bounds checks from the loop.
  LLVM_DEBUG(llvm::dbgs()
             << "Attempting to hoist bounds checks for Span and InlineArray in "
             << *loop);
  if (!indVars) {
    LLVM_DEBUG(llvm::dbgs() << "No induction variables found\n");
    return changed;
  }
  changed |= hoistFixedStorageBoundsChecksInLoop(
      loop, DT->getNode(loop->getHeader()), *indVars, /*recursionDepth*/ 0);
  return changed;
}

bool BoundsCheckOpts::hoistArrayBoundsChecksInLoop(
    SILLoop *loop, DominanceInfoNode *currentNode, ABCAnalysis &abcAnalysis,
    InductionAnalysis &indVars, int recursionDepth) {
  auto preheader = loop->getLoopPreheader();
  auto singleExitingBlock = loop->getExitingBlock();
  // Avoid a stack overflow for very deep dominator trees.
  if (recursionDepth >= maxRecursionDepth)
    return false;

  bool changed = false;
  auto *curBlock = currentNode->getBlock();
  bool blockAlwaysExecutes =
      isGuaranteedToBeExecuted(DT, curBlock, singleExitingBlock);

  for (auto Iter = curBlock->begin(); Iter != curBlock->end();) {
    auto Inst = &*Iter;
    ++Iter;

    ArraySemanticsCall ArrayCall(Inst);
    auto Kind = ArrayCall.getKind();
    if (Kind != ArrayCallKind::kCheckSubscript &&
        Kind != ArrayCallKind::kCheckIndex) {
      continue;
    }
    auto ArrayVal = ArrayCall.getSelf();

    // Get the underlying array pointer.
    SILValue Array = getArrayStructPointer(Kind, ArrayVal);

    // The array must strictly dominate the header.
    if (!dominates(DT, Array, preheader)) {
      LLVM_DEBUG(llvm::dbgs() << " does not dominated header" << *Array);
      continue;
    }

    // Is this a safe array whose size could not have changed?
    // This is either a SILValue which is defined outside the loop or it is an
    // array, which loaded from memory and the memory is not changed in the
    // loop.
    if (!dominates(DT, ArrayVal, preheader) && abcAnalysis.isUnsafe(Array)) {
      LLVM_DEBUG(llvm::dbgs() << " not a safe array argument " << *Array);
      continue;
    }

    // Get the array index.
    auto ArrayIndex = ArrayCall.getIndex();
    if (!ArrayIndex)
      continue;

    // Make sure we know how-to hoist the array call.
    if (!ArrayCall.canHoist(preheader->getTerminator(), DT))
      continue;

    // Invariant check.
    if (blockAlwaysExecutes && dominates(DT, ArrayIndex, preheader)) {
      assert(ArrayCall.canHoist(preheader->getTerminator(), DT) &&
             "Must be able to hoist the instruction.");
      changed = true;
      ArrayCall.hoist(preheader->getTerminator(), DT);
      LLVM_DEBUG(llvm::dbgs()
                 << " could hoist invariant bounds check: " << *Inst);
      continue;
    }

    // Get the access function "a[f(i)]". At the moment this handles only the
    // identity function.
    auto F = AccessFunction::getLinearFunction(ArrayIndex, indVars);
    if (!F) {
      LLVM_DEBUG(llvm::dbgs() << " not a linear function " << *Inst);
      continue;
    }

    // Check if the loop iterates from 0 to the count of this array.
    if (F.isZeroToCount(ArrayVal) &&
        // This works only for Arrays but not e.g. for ArraySlice.
        ArrayVal->getType().getASTType()->isArray()) {
      // We can remove the check. This is even possible if the block does not
      // dominate the loop exit block.
      changed = true;
      ArrayCall.removeCall();
      LLVM_DEBUG(llvm::dbgs() << "  Bounds check removed\n");
      continue;
    }

    // For hoisting bounds checks the block must dominate the exit block.
    if (!blockAlwaysExecutes)
      continue;

    // Hoist the access function and the check to the preheader for start and
    // end of the induction.
    assert(ArrayCall.canHoist(preheader->getTerminator(), DT) &&
           "Must be able to hoist the call");

    F.hoistCheckToPreheader(ArrayCall, preheader, DT);

    // Remove the old check in the loop and the match the retain with a release.
    ArrayCall.removeCall();

    LLVM_DEBUG(llvm::dbgs() << "  Bounds check hoisted\n");
    changed = true;
  }

  // Traverse the children in the dominator tree.
  for (auto child : *currentNode) {
    changed |= hoistArrayBoundsChecksInLoop(loop, child, abcAnalysis, indVars,
                                            recursionDepth + 1);
  }

  return changed;
}

bool BoundsCheckOpts::hoistFixedStorageBoundsChecksInLoop(
    SILLoop *loop, DominanceInfoNode *currentNode, InductionAnalysis &indVars,
    int recursionDepth) {
  auto preheader = loop->getLoopPreheader();
  auto singleExitingBlock = loop->getExitingBlock();
  // Avoid a stack overflow for very deep dominator trees.
  if (recursionDepth >= maxRecursionDepth)
    return false;

  bool changed = false;
  auto *curBlock = currentNode->getBlock();
  bool blockAlwaysExecutes =
      isGuaranteedToBeExecuted(DT, curBlock, singleExitingBlock);

  for (auto instIt = curBlock->begin(); instIt != curBlock->end();) {
    auto inst = &*instIt;
    ++instIt;

    FixedStorageSemanticsCall fixedStorageSemantics(inst);
    if (!fixedStorageSemantics ||
        fixedStorageSemantics.getKind() !=
            FixedStorageSemanticsCallKind::CheckIndex) {
      continue;
    }

    if (!fixedStorageSemantics->hasSelfArgument()) {
      continue;
    }

    auto selfValue = fixedStorageSemantics->getSelfArgument();

    if (!DT->dominates(selfValue->getParentBlock(), preheader)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "  " << *selfValue << " does not dominate preheader\n");
      continue;
    }

    auto indexValue = fixedStorageSemantics->getArgument(0);

    // If the bounds check is loop invariant, hoist it.
    if (blockAlwaysExecutes && dominates(DT, indexValue, preheader)) {
      LLVM_DEBUG(llvm::dbgs() << "  Invariant bounds check removed\n");
      changed = true;
      fixedStorageSemantics->moveBefore(preheader->getTerminator());
      continue;
    }

    auto accessFunction =
        AccessFunction::getLinearFunction(indexValue, indVars);
    if (!accessFunction) {
      LLVM_DEBUG(llvm::dbgs() << " not a linear function " << *inst);
      continue;
    }

    // If the loop iterates 0 through count, remove the bounds check.
    if (accessFunction.isZeroToCount(selfValue)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "  Redundant Span/InlineArray bounds check removed\n");
      changed = true;
      fixedStorageSemantics->eraseFromParent();
      continue;
    }

    // If the bounds check does not execute always, we cannot hoist it.
    if (!blockAlwaysExecutes) {
      LLVM_DEBUG(llvm::dbgs() << "  Bounds check does not execute always\n");
      continue;
    }

    LLVM_DEBUG(llvm::dbgs() << "  Span/InlineArray bounds check hoisted\n");
    changed = true;
    auto firstValue = accessFunction.getFirstValue(preheader->getTerminator());
    auto newLowerBoundCheck =
        fixedStorageSemantics->clone(preheader->getTerminator());
    newLowerBoundCheck->setOperand(1, firstValue);

    auto lastValue = accessFunction.getLastValue(preheader->getTerminator());
    auto newUpperBoundCheck =
        fixedStorageSemantics->clone(preheader->getTerminator());
    newUpperBoundCheck->setOperand(1, lastValue);
    fixedStorageSemantics->eraseFromParent();
  }

  // Traverse the children in the dominator tree.
  for (auto child : *currentNode) {
    changed |= hoistFixedStorageBoundsChecksInLoop(loop, child, indVars,
                                                   recursionDepth + 1);
  }

  return changed;
}

bool BoundsCheckOpts::removeRedundantFixedStorageBoundsChecksInLoop(
    SILLoop *loop, DominanceInfoNode *currentNode,
    llvm::DenseSet<std::pair<SILValue, SILValue>> &dominatingSafeChecks,
    int recursionDepth) {
  auto *currentBlock = currentNode->getBlock();
  if (!loop->contains(currentBlock)) {
    return false;
  }

  if (recursionDepth >= maxRecursionDepth) {
    return false;
  }

  bool changed = false;

  // When we come back from the dominator tree recursion we need to remove
  // checks that we have seen for the first time.
  SmallVector<std::pair<SILValue, SILValue>, 8> safeChecksToPop;

  for (auto iter = currentBlock->begin(); iter != currentBlock->end();) {
    auto inst = &*iter;
    ++iter;

    FixedStorageSemanticsCall fixedStorageSemantics(inst);
    if (!fixedStorageSemantics ||
        fixedStorageSemantics.getKind() !=
            FixedStorageSemanticsCallKind::CheckIndex) {
      continue;
    }

    if (!fixedStorageSemantics->hasSelfArgument()) {
      continue;
    }

    auto selfValue = fixedStorageSemantics->getSelfArgument();

    if (!DT->dominates(selfValue->getParentBlock(), loop->getLoopPreheader())) {
      LLVM_DEBUG(llvm::dbgs()
                 << "  " << *selfValue << " does not dominate preheader\n");
      continue;
    }

    auto indexValue = fixedStorageSemantics->getArgument(0);
    auto selfAndIndex = std::make_pair(selfValue, indexValue);
    if (!dominatingSafeChecks.count(selfAndIndex)) {
      LLVM_DEBUG(llvm::dbgs()
                 << " first time: " << *inst << "  with self: " << *selfValue);
      dominatingSafeChecks.insert(selfAndIndex);
      safeChecksToPop.push_back(selfAndIndex);
      continue;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "  Eliminated redundant Span/InlineArray bounds check");
    changed = true;
    fixedStorageSemantics->eraseFromParent();
  }

  // Traverse the children in the dominator tree inside the loop.
  for (auto child : *currentNode) {
    changed |= removeRedundantFixedStorageBoundsChecksInLoop(
        loop, child, dominatingSafeChecks, recursionDepth + 1);
  }

  // Remove checks we have seen for the first time.
  std::for_each(safeChecksToPop.begin(), safeChecksToPop.end(),
                [&](std::pair<SILValue, SILValue> &value) {
                  dominatingSafeChecks.erase(value);
                });

  return changed;
}

} // end anonymous namespace

SILTransform *swift::createBoundsCheckOpts() { return new BoundsCheckOpts(); }
