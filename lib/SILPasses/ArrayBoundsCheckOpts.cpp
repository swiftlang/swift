//===----- ArrayBoundsCheckOpts.cpp - Bounds check elim ---*- C++ -*-------===//
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

#define DEBUG_TYPE "sil-abcopts"

#include "swift/Basic/STLExtras.h"
#include "swift/AST/Builtins.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/IVAnalysis.h"
#include "swift/SILAnalysis/SILLoopInfo.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/ADT/DepthFirstIterator.h"
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
using IndexedArraySet = llvm::DenseSet<std::pair<ValueBase *, ValueBase *>>;
using InstructionSet = llvm::SmallPtrSet<SILInstruction *, 16>;
using ValueBaseList = llvm::SmallVector<ValueBase *, 4>;

/// The kind of array operation identified by looking at the semantics attribute
/// of the called function.
enum class ArrayCallKind {
  kNone = 0,
  kCheckSubscript,
  kCheckIndex,
  kGetCount,
  kGetCapacity,
  kGetElement,
  kMakeMutable,
  kSetElement,
  kMutateUnknown
};

/// The effect an instruction can have on array bounds.
enum class ArrayBoundsEffect {
  kNone = 0,
  kMayChangeArg, // Can only change the array argument.
  kMayChangeAny  // Might change any array.
};

/// Classify the instruction with respect to its high level array semantics.
static ArrayCallKind isArrayKindCall(SILInstruction *I, SILValue &Array) {
  auto *AI = dyn_cast<ApplyInst>(I);
  // Not an apply.
  if (!AI)
    return ArrayCallKind::kNone;
  auto *FR = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FR)
    return ArrayCallKind::kNone;
  auto *F = FR->getReferencedFunction();

  // No semantics attribute.
  if (!F || !F->hasDefinedSemantics())
   return ArrayCallKind::kNone;

  auto Kind = llvm::StringSwitch<ArrayCallKind>(F->getSemanticsString())
      .Case("array.check_subscript", ArrayCallKind::kCheckSubscript)
      .Case("array.check_index", ArrayCallKind::kCheckIndex)
      .Case("array.get_count", ArrayCallKind::kGetCount)
      .Case("array.get_capacity", ArrayCallKind::kGetCapacity)
      .Case("array.get_element", ArrayCallKind::kGetElement)
      .Case("array.make_mutable", ArrayCallKind::kMakeMutable)
      .Case("array.set_element", ArrayCallKind::kSetElement)
      .Case("array.mutate_unknown", ArrayCallKind::kMutateUnknown)
      .Default(ArrayCallKind::kNone);

  if (Kind == ArrayCallKind::kNone)
    return Kind;

  // We must have a self argument.
  if (AI->getNumArguments() < 1)
    return ArrayCallKind::kNone;

  Array = AI->getSelfArgument();
  return Kind;
}

static bool mayHaveSideEffects(SILInstruction *I) {
  if (auto *AI = dyn_cast<ApplyInst>(I))
    if (auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
      return !isSideEffectFree(BFRI);
  return I->mayHaveSideEffects();
}

static SILValue getArrayStructPointer(ArrayCallKind K, SILValue Array) {
  assert(K != ArrayCallKind::kNone);

  if (K < ArrayCallKind::kMakeMutable) {
    auto LI = dyn_cast<LoadInst>(Array.getDef());
    if (!LI) {
      return Array;
    }
    return LI->getOperand();
  }
  return Array;
}

/// Determines the kind of array bounds effect the instruction can have.
static ArrayBoundsEffect mayChangeArraySize(SILInstruction *I,
                                            ArrayCallKind &Kind,
                                            SILValue &Array) {
  Array = SILValue();
  Kind = ArrayCallKind::kNone;

  // TODO: What else.
  if (isa<StrongRetainInst>(I) || isa<RetainValueInst>(I) ||
      isa<CondFailInst>(I) || isa<DeallocStackInst>(I) ||
      isa<AllocationInst>(I))
    return ArrayBoundsEffect::kNone;

  // Check array bounds semantic.
  Kind = isArrayKindCall(I, Array);
  if (Kind != ArrayCallKind::kNone) {
    if (Kind < ArrayCallKind::kMutateUnknown) {
      // These methods are not mutating and pass the array owned. Therefore we
      // will potentially see a load of the array struct if there are mutating
      // functions in the loop on the same array.
      Array = getArrayStructPointer(Kind, Array);
      return ArrayBoundsEffect::kNone;
    }
    return ArrayBoundsEffect::kMayChangeArg;
  }

  if (!mayHaveSideEffects(I))
    return ArrayBoundsEffect::kNone;

  // A store to an alloc_stack can't possibly store to the array size which is
  // stored in a runtime allocated object sub field of an alloca.
  if (auto *SI = dyn_cast<StoreInst>(I)) {
    auto Ptr = SI->getDest();
    return isa<AllocStackInst>(Ptr.getDef()) ? ArrayBoundsEffect::kNone
                                             : ArrayBoundsEffect::kMayChangeAny;
  }

  return ArrayBoundsEffect::kMayChangeAny;
}

/// Two allocations of a mutable array struct can not reference the same
/// storage after modification. So we can treat them as not aliasing for the
/// purpose of bound checking. The change would only be tracked through one of
/// the allocations.
static bool isIdentifiedUnderlyingArrayObject(SILValue V) {
  // Allocations are safe.
  if (isa<AllocationInst>(V.getDef()))
    return true;

  // Function arguments are safe.
  if (auto Arg = dyn_cast<SILArgument>(V.getDef())) {
    auto Fn = Arg->getParent()->getParent();
    if (Arg->getParent() == Fn->begin())
      return true;
  }

  return false;
}

/// Array bounds check analysis finds array bounds checks that are safe to
/// eliminate if there exists an earlier bounds check that covers the same
/// index.
///
/// We analyse a region of code for instructions that mayModify the size of an
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
  ArraySet SafeArrays;
  ArraySet UnsafeArrays;
  bool LoopMode;

public:
  ABCAnalysis(bool loopMode = true) : LoopMode(loopMode) {}

  ABCAnalysis(const ABCAnalysis &) = delete;
  ABCAnalysis &operator=(const ABCAnalysis &) = delete;

  /// Find safe array bounds check in a loop. An bounds_check is safe if no size
  /// modifying instruction to the same array has been seen so far.
  ///
  /// The code relies on isIdentifiedUnderlyingArrayObject' to make sure that a
  /// 'safe arrays' is not aliased.
  /// If an instruction is encountered that might modify any array this method
  /// stops further analysis and returns false. Otherwise, true is returned and
  /// the safe arrays can be queried.
  bool analyseBlock(SILBasicBlock *BB) {
    for (auto &Inst : *BB)
      if (!analyseInstruction(&Inst))
        return false;

    return true;
  }

  /// Returns false if the instruction may change the size of any array. All
  /// redundant safe array accesses seen up to the instruction can be removed.
  bool analyse(SILInstruction *I, bool ClearUnsafeOnMayChangeAnyArray) {
    assert(!LoopMode &&
           "This function can only be used in on cfg without loops");
    (void)LoopMode;

    bool MayChangeAny = !analyseInstruction(I);
    if (ClearUnsafeOnMayChangeAnyArray && MayChangeAny)
      UnsafeArrays.clear();

    return !MayChangeAny;
  }

  ArraySet &getSafeArrays() { return SafeArrays; }

private:
  /// Analyse one instruction wrt. the instructions we have seen so far.
  bool analyseInstruction(SILInstruction *Inst) {
    SILValue Array;
    ArrayCallKind K;
    auto BoundsEffect = mayChangeArraySize(Inst, K, Array);
    assert(Array || K == ArrayCallKind::kNone);

    if (BoundsEffect == ArrayBoundsEffect::kMayChangeAny) {
      DEBUG(llvm::dbgs() << " no safe because kMayChangeAny " << *Inst);
      SafeArrays.clear();
      return false;
    }

    // We need to make sure that the array container is not aliased in ways
    // that we don't understand.
    if (Array && !isIdentifiedUnderlyingArrayObject(Array)) {
      DEBUG(llvm::dbgs()
            << " not safe because of not identified underlying object "
            << *Array.getDef() << " in " << *Inst);
      SafeArrays.clear();
      return false;
    }

    if (BoundsEffect == ArrayBoundsEffect::kMayChangeArg) {
      UnsafeArrays.insert(Array);
      SafeArrays.erase(Array);
      return true;
    }

    assert(BoundsEffect == ArrayBoundsEffect::kNone);

    // If we see a check_bounds on an array that is not marked unsafe add the
    // array to the safe set.
    if (K == ArrayCallKind::kCheckSubscript && !UnsafeArrays.count(Array))
      SafeArrays.insert(Array);

    return true;
  }
};

/// Check whether this a retain on the storage of the array.
static SILInstruction *isArrayBufferStorageRetain(SILInstruction *R,
                                                  SILValue Array,
                                                  ValueBaseList *InstSeq) {
  // Is this a retain.
  if (!isa<RetainValueInst>(R))
    return nullptr;

  if (!R->getOperand(0))
    return nullptr;

  // Find the projection from the array:
  // %42 = SILValue(Array, ...) : $Array<Int>
  // %43 = struct_extract %42 : $Array<Int>, #Array._buffer
  // %44 = struct_extract %43 : $_ArrayBuffer<Int>, #_ArrayBuffer.storage
  auto ArrayBufferStorageProj =
      dyn_cast<StructExtractInst>(R->getOperand(0).getDef());
  if (!ArrayBufferStorageProj || ArrayBufferStorageProj->getFieldNo() != 0)
    return nullptr;

  // Valid operand?
  if (!ArrayBufferStorageProj->getOperand())
    return nullptr;

  auto ArrayBufferProj = dyn_cast<StructExtractInst>(
      ArrayBufferStorageProj->getOperand().getDef());
  if (!ArrayBufferProj || ArrayBufferProj->getFieldNo() != 0)
    return nullptr;

  auto Arr = ArrayBufferProj->getOperand();
  if (Arr != Array)
    return nullptr;

  // Store the sequence.
  if (InstSeq) {
    InstSeq->push_back(Arr.getDef());
    InstSeq->push_back(ArrayBufferProj);
    InstSeq->push_back(ArrayBufferStorageProj);
    InstSeq->push_back(R);
  }

  return R;
}

/// Find a matching preceeding retain on the same array.
static SILInstruction *
findMatchingRetain(SILInstruction *BoundsCheck,
                   ValueBaseList *InstSeq = nullptr) {
  auto ApplyBoundsCheck = dyn_cast<ApplyInst>(BoundsCheck);
  assert(ApplyBoundsCheck);
  auto Array = ApplyBoundsCheck->getSelfArgument();
  unsigned BoundSearch = 4;
  for (auto E = BoundsCheck->getParent()->rend(),
            Iter = SILBasicBlock::reverse_iterator(BoundsCheck);
       Iter != E; ++Iter) {
    if (auto R = isArrayBufferStorageRetain(&*Iter, Array, InstSeq))
      return R;
    if (!BoundSearch--)
      return nullptr;
  }
  return nullptr;
}

static SILValue getArrayIndex(SILInstruction *BoundsCheck) {
  auto AI = dyn_cast<ApplyInst>(BoundsCheck);
  if (!AI) {
    DEBUG(llvm::dbgs() << " not an apply " << *BoundsCheck);
    return SILValue();
  }
  if (AI->getNumArguments() != 2) {
    DEBUG(llvm::dbgs() << " not an two args " << *BoundsCheck);
    return SILValue();
  }
  return AI->getArgument(0);
}

static bool
eraseArrayBoundsCheckAndMatchingRetain(SILInstruction *BoundsCheck) {
  // TODO: This could be quadratic if we don't find the retain immediatly before
  // the bounds check instruction (which seems to be the common case). We should
  // preprocess the block first to build matching retain and apply instructions.
  // For now we bound this by looking only at the preceeding 5 instructions.
  // Retain is always the preceeding instruction in all examples I have seen so
  // far.
  if (auto Retain = findMatchingRetain(BoundsCheck)) {
    DEBUG(llvm::dbgs() << " removing " << *BoundsCheck << "  and matching "
                       << *Retain);
    BoundsCheck->eraseFromParent();
    Retain->eraseFromParent();
    return true;
  }

  DEBUG(llvm::dbgs() << " ABC not removing " << *BoundsCheck);
  DEBUG(llvm::dbgs() << "  could not find matching retain\n");
  return false;
}

// Get the pair of array and index. Because we want to disambiguate between the
// two types of check bounds checks merge in the type into the lower bit of one
// of the addresses.
static std::pair<ValueBase *, ValueBase *>
getArrayIndexPair(SILValue Array, SILValue ArrayIndex, ArrayCallKind K) {
  assert((K == ArrayCallKind::kCheckIndex ||
          K == ArrayCallKind::kCheckSubscript) &&
         "Must be a bounds check call");
  assert(((uintptr_t)Array.getDef() & 0x1) != 1 &&
         "Pointers need to be aligned for this to work");
  return std::make_pair(
      reinterpret_cast<ValueBase *>(((uintptr_t)Array.getDef()) |
                                    (K == ArrayCallKind::kCheckIndex)),
      ArrayIndex.getDef());
}

/// Remove redundant checks in a basic block. This pass will reset the state
/// after an instruction that may modify any array allowing removal of redundant
/// checks up to that point and after that point.
static bool removeRedundantChecksInBlock(SILBasicBlock &BB) {
  ABCAnalysis ABC(false);
  IndexedArraySet RedundantChecks;
  bool Changed = false;

  DEBUG(llvm::dbgs() << "Removing in BB\n");
  DEBUG(BB.dump());

  // Process all instructions in the current block.
  for (auto Iter = BB.begin(); Iter != BB.end();) {
    auto Inst = &*Iter;
    ++Iter;

    // The analysis returns false if it encounters an instruction that may
    // modify the size of all arrays.
    if (!ABC.analyse(Inst, true)) {
      RedundantChecks.clear();
      continue;
    }

    ArraySet &SafeArrays = ABC.getSafeArrays();

    SILValue Array;
    // Is this a check_bounds.
    auto Kind = isArrayKindCall(Inst, Array);
    if (Kind != ArrayCallKind::kCheckSubscript &&
        Kind != ArrayCallKind::kCheckIndex) {
      DEBUG(llvm::dbgs() << " not a check_bounds call " << *Inst);
      continue;
    }

    // Get the underlying array pointer.
    Array = getArrayStructPointer(Kind, Array);

    // Is this a safe array check whose size could not have changed.
    if (!SafeArrays.count(Array)) {
      DEBUG(llvm::dbgs() << " not a safe array argument " << *Array.getDef());
      continue;
    }

    // Get the array index.
    auto ArrayIndex = getArrayIndex(Inst);
    if (!ArrayIndex)
      continue;

    auto IndexedArray =
        getArrayIndexPair(Array.getDef(), ArrayIndex.getDef(), Kind);
    DEBUG(llvm::dbgs() << " IndexedArray: " << *Array.getDef() << " and "
                       << *ArrayIndex.getDef());

    // Saw a check for the first time.
    if (!RedundantChecks.count(IndexedArray)) {
      DEBUG(llvm::dbgs() << " first time: " << *Inst
                         << "  with array argument: " << *Array.getDef());
      RedundantChecks.insert(IndexedArray);
      continue;
    }

    // Remove the bounds check together with the matching retain if we can find
    // the retain. This will erase Inst and the preceeding retain on success.
    Changed |= eraseArrayBoundsCheckAndMatchingRetain(Inst);
  }
  return Changed;
}

/// Walk down the dominator tree removing redundant checks.
static bool removeRedundantChecks(DominanceInfoNode *CurBB,
                                  ArraySet &SafeArrays,
                                  IndexedArraySet &DominatingSafeChecks) {
  auto *BB = CurBB->getBlock();
  bool Changed = false;

  // When we come back from the dominator tree recursion we need to remove
  // checks that we have seen for the first time.
  SmallVector<std::pair<ValueBase *, ValueBase *>, 8> SafeChecksToPop;

  // Process all instructions in the current block.
  for (auto Iter = BB->begin(); Iter != BB->end();) {
    auto Inst = &*Iter;
    ++Iter;

    SILValue Array;
    // Is this a check_bounds.
    auto Kind = isArrayKindCall(Inst, Array);
    if (Kind != ArrayCallKind::kCheckSubscript &&
        Kind != ArrayCallKind::kCheckIndex) {
      DEBUG(llvm::dbgs() << " not a check_bounds call " << *Inst);
      continue;
    }

    // Get the underlying array pointer.
    Array = getArrayStructPointer(Kind, Array);

    // Is this a safe array check whose size could not have changed.
    if (!SafeArrays.count(Array)) {
      DEBUG(llvm::dbgs() << " not a safe array argument " << *Array.getDef());
      continue;
    }

    // Get the array index.
    auto ArrayIndex = getArrayIndex(Inst);
    if (!ArrayIndex)
      continue;
    auto IndexedArray =
        getArrayIndexPair(Array.getDef(), ArrayIndex.getDef(), Kind);

    // Saw a check for the first time.
    if (!DominatingSafeChecks.count(IndexedArray)) {
      DEBUG(llvm::dbgs() << " first time: " << *Inst
                         << "  with array arg: " << *Array.getDef());
      DominatingSafeChecks.insert(IndexedArray);
      SafeChecksToPop.push_back(IndexedArray);
      continue;
    }

    // Remove the bounds check together with the matching retain if we can find
    // the retain. This will erase Inst and the preceeding retain on success.
    Changed |= eraseArrayBoundsCheckAndMatchingRetain(Inst);
  }

  // Traverse the children in the dominator tree.
  for (auto Child: *CurBB)
    Changed |=
        removeRedundantChecks(Child, SafeArrays, DominatingSafeChecks);

  // Remove checks we have seen for the first time.
  std::for_each(SafeChecksToPop.begin(), SafeChecksToPop.end(),
                [&](std::pair<ValueBase *, ValueBase *> &V) {
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
static CondFailInst *isOverflowChecked(ApplyInst *AI) {
  for (auto *Op : AI->getUses()) {
    SILValue Extract;
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
          End.getDef(),
          m_TupleExtractInst(m_ApplyInst(BuiltinValueKind::SAddOver,
                                         m_SILValue(PreInclusiveEnd), m_One()),
                             0)))
    PreInclusiveEnd = SILValue();

  bool IsPreInclusiveEndLEQ = false;
  bool IsPreInclusiveEndGTEnd = false;
  for (auto &Inst : BB)
    if (auto CF = dyn_cast<CondFailInst>(&Inst)) {
      // Try to match a cond_fail on "XOR , (SLE Start, End), 1".
      if (match(CF->getOperand().getDef(),
                m_ApplyInst(BuiltinValueKind::Xor,
                            m_ApplyInst(BuiltinValueKind::ICMP_SLE,
                                        m_Specific(Start.getDef()),
                                        m_Specific(End.getDef())),
                            m_One())))
        return true;
      // Inclusive ranges will have a check on the upper value (before adding
      // one).
      if (PreInclusiveEnd) {
        if (match(CF->getOperand().getDef(),
                  m_ApplyInst(BuiltinValueKind::Xor,
                              m_ApplyInst(BuiltinValueKind::ICMP_SLE,
                                          m_Specific(Start.getDef()),
                                          m_Specific(PreInclusiveEnd.getDef())),
                              m_One())))
          IsPreInclusiveEndLEQ = true;
        if (match(CF->getOperand().getDef(),
                  m_ApplyInst(BuiltinValueKind::Xor,
                              m_ApplyInst(BuiltinValueKind::ICMP_SGT,
                                          m_Specific(End.getDef()),
                                          m_Specific(PreInclusiveEnd.getDef())),
                              m_One())))
          IsPreInclusiveEndGTEnd = true;
        if (IsPreInclusiveEndLEQ && IsPreInclusiveEndGTEnd)
          return true;
      }
    }

  return false;
}

static bool isLessThan(SILValue Start, SILValue End) {
  auto S = dyn_cast<IntegerLiteralInst>(Start.getDef());
  if (!S)
    return false;
  auto E = dyn_cast<IntegerLiteralInst>(End.getDef());
  if (!E)
    return false;
  return S->getValue().slt(E->getValue());
}

/// Checks whether there are checks in the preheader's predecessor that ensure
/// that "Start < End".
static bool isRangeChecked(SILValue Start, SILValue End,
                           SILBasicBlock *Preheader, DominanceInfo *DT) {
  // Check two constants.
  if (isLessThan(Start, End))
    return true;

  // Look for a branch on EQ around the Preheader.
  auto *PreheaderPred = Preheader->getSinglePredecessor();
  if (!PreheaderPred)
    return false;
  auto *CondBr = dyn_cast<CondBranchInst>(PreheaderPred->getTerminator());
  if (!CondBr)
    return false;

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
  if (auto *Arg = dyn_cast<SILArgument>(V.getDef()))
    return DT->properlyDominates(Arg->getParent(), B);
  if (auto *Inst = dyn_cast<SILInstruction>(V.getDef()))
    return DT->properlyDominates(Inst->getParent(), B);
  return false;
}

/// Get a builtin compare function reference.
static SILValue getCmpFunction(SILLocation Loc, StringRef Name,
                               SILType IntSILTy, SILBuilder &B) {
  CanType IntTy = IntSILTy.getSwiftRValueType();
  auto BuiltinIntTy = cast<BuiltinIntegerType>(IntTy);
  std::string NameStr = Name;
  if (BuiltinIntTy == BuiltinIntegerType::getWordType(B.getASTContext())) {
    NameStr += "_Word";
  } else {
    unsigned NumBits = BuiltinIntTy->getWidth().getFixedWidth();
    NameStr += "_Int" + llvm::utostr(NumBits);
  }

  auto ExtInfo = SILFunctionType::ExtInfo(AbstractCC::Freestanding,
                                          FunctionType::Representation::Thin,
                                          /*noreturn*/ false,
                                          /*autoclosure*/ false);
  SILParameterInfo Params[] = {
      SILParameterInfo(IntTy, ParameterConvention::Direct_Unowned),
      SILParameterInfo(IntTy, ParameterConvention::Direct_Unowned)};

  Type Int1Ty = BuiltinIntegerType::get(1, B.getASTContext());
  SILResultInfo Result(Int1Ty->getCanonicalType(), ResultConvention::Unowned);

  auto FnType = SILFunctionType::get(nullptr, ExtInfo,
                                     ParameterConvention::Direct_Unowned,
                                     Params, Result, B.getASTContext());
  auto Ty = SILType::getPrimitiveObjectType(FnType);
  return B.createBuiltinFunctionRef(Loc, NameStr, Ty);
}

/// Subtract a constant from a builtin integer value.
static SILValue getSub(SILLocation Loc, SILValue Val, unsigned SubVal,
                       SILBuilder &B) {
  CanType IntTy = Val.getType().getSwiftRValueType();
  auto BuiltinIntTy = cast<BuiltinIntegerType>(IntTy);
  std::string NameStr = "ssub_with_overflow";
  if (BuiltinIntTy == BuiltinIntegerType::getWordType(B.getASTContext())) {
    NameStr += "_Word";
  } else {
    unsigned NumBits = BuiltinIntTy->getWidth().getFixedWidth();
    NameStr += "_Int" + llvm::utostr(NumBits);
  }

  auto ExtInfo = SILFunctionType::ExtInfo(AbstractCC::Freestanding,
                                          FunctionType::Representation::Thin,
                                          /*noreturn*/ false,
                                          /*autoclosure*/ false);
  CanType Int1Ty =
      BuiltinIntegerType::get(1, B.getASTContext())->getCanonicalType();
  SILParameterInfo Params[] = {
      SILParameterInfo(IntTy, ParameterConvention::Direct_Unowned),
      SILParameterInfo(IntTy, ParameterConvention::Direct_Unowned),
      SILParameterInfo(Int1Ty, ParameterConvention::Direct_Unowned)};

  TupleTypeElt ResultElts[] = {IntTy, Int1Ty};
  Type ResultTy = TupleType::get(ResultElts, B.getASTContext());
  SILResultInfo Result(ResultTy->getCanonicalType(), ResultConvention::Unowned);

  auto FnType = SILFunctionType::get(nullptr, ExtInfo,
                                     ParameterConvention::Direct_Unowned,
                                     Params, Result, B.getASTContext());
  auto Ty = SILType::getPrimitiveObjectType(FnType);
  auto FR = B.createBuiltinFunctionRef(Loc, NameStr, Ty);

  SmallVector<SILValue, 4> Args(1, Val);
  Args.push_back(B.createIntegerLiteral(Loc, Val.getType(), SubVal));
  Args.push_back(B.createIntegerLiteral(
      Loc, SILType::getBuiltinIntegerType(1, B.getASTContext()), -1));

  auto AI = B.createApply(Loc, FR, Args);
  return B.createTupleExtract(Loc, AI, 0);
}

/// A cannonical induction variable incremented by one from Start to End-1.
struct InductionInfo {
  SILArgument *HeaderVal;
  ApplyInst *Inc;
  SILValue Start;
  SILValue End;
  BuiltinValueKind Cmp;
  bool IsOverflowCheckInserted;

  InductionInfo()
      : Cmp(BuiltinValueKind::None), IsOverflowCheckInserted(false) {}

  InductionInfo(SILArgument *HV, ApplyInst *I, SILValue S, SILValue E,
                BuiltinValueKind C, bool IsOverflowChecked = false)
      : HeaderVal(HV), Inc(I), Start(S), End(E), Cmp(C),
        IsOverflowCheckInserted(IsOverflowChecked) {}

  bool isValid() { return Start && End; }
  operator bool() { return isValid(); }

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
  void checkOverflow(SILBuilder &Builder) {
    if (IsOverflowCheckInserted || Cmp != BuiltinValueKind::ICMP_EQ)
      return;

    auto Loc = Inc->getLoc();
    auto FR = getCmpFunction(Loc, "cmp_sge", Start.getType(), Builder);

    SmallVector<SILValue, 4> Args(1, Start);
    Args.push_back(End);
    auto CmpSGE = Builder.createApply(Loc, FR, Args);
    Builder.createCondFail(Loc, CmpSGE);
    IsOverflowCheckInserted = true;

    // We can now remove the cond fail on the increment the above comparison
    // guarantuees that the addition won't overflow.
    auto *CondFail = isOverflowChecked(cast<ApplyInst>(Inc));
    if (CondFail)
      CondFail->eraseFromParent();
  }
};

/// Analyse canonical induction variables in a loop to find their start and end
/// values.
/// At the moment we only handle very simple induction variables that increment
/// by one and use equality comparison.
class InductionAnalysis {
  using InductionInfoMap = llvm::DenseMap<SILArgument *, InductionInfo *>;

public:
  InductionAnalysis(DominanceInfo *D, IVInfo &IVs, SILBasicBlock *Preheader,
                    SILBasicBlock *Header, SILBasicBlock *ExitingBlk)
      : DT(D), Preheader(Preheader), Header(Header), ExitingBlk(ExitingBlk),
        IVs(IVs) {}

  InductionAnalysis(const InductionAnalysis &) = delete;
  InductionAnalysis &operator=(const InductionAnalysis &) = delete;

  bool analyse() {
    bool FoundIndVar = false;
    for (auto *Arg : Header->getBBArgs()) {
      // Look for induction variables.
      if (!IVs.isInductionVariable(Arg)) {
        DEBUG(llvm::dbgs() << " not a induction variable: " << *Arg);
        continue;
      }

      InductionInfo *Info;
      if (!(Info = analyseIndVar(Arg))) {
        DEBUG(llvm::dbgs() << " could not analyse the induction on: " << *Arg);
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

  /// Analyse one potential induction variable starting at Arg.
  InductionInfo *analyseIndVar(SILArgument *Arg) {
    // Find the start value.
    auto *PreheaderTerm = dyn_cast<BranchInst>(Preheader->getTerminator());
    if (!PreheaderTerm)
      return nullptr;
    auto Start = PreheaderTerm->getArg(Header->getBBArgIndex(Arg));

    // Find the increment.
    ApplyInst *FoundInc = nullptr;
    for (auto *Op : Arg->getUses()) {
      auto *AI = dyn_cast<ApplyInst>(Op->getUser());
      if (!AI)
        continue;

      SILValue L, R;
      // Find an overflow checked +1 increment.
      if (match(AI, m_ApplyInst(BuiltinValueKind::SAddOver, m_SILValue(L),
                                m_SILValue(R))) &&
          IVs.isInductionVariable(AI)) {
        assert(!FoundInc && "Already found induction increment");
        // Get the integer constant.
        auto IncValue = L.getDef() == Arg
                            ? cast<IntegerLiteralInst>(R.getDef())
                            : cast<IntegerLiteralInst>(L.getDef());
        if (IncValue->getValue() != 1)
          continue;
        DEBUG(llvm::dbgs() << " found increment " << *AI);
        FoundInc = AI;
        break;
      }
    }

    if (!FoundInc) {
      DEBUG(llvm::dbgs() << " increment not found\n");
      return nullptr;
    }

    // Find the exit condition.
    auto CondBr = dyn_cast<CondBranchInst>(ExitingBlk->getTerminator());
    if (!CondBr)
      return nullptr;

    if (Header == CondBr->getTrueBB())
      return nullptr;
    assert(Header == CondBr->getFalseBB());

    auto Cond = CondBr->getCondition();
    SILValue End;

    // Look for a compare of induction variable + 1.
    // TODO: obviously we need to handle many more patterns.
    if (!match(Cond, m_ApplyInst(BuiltinValueKind::ICMP_EQ,
                                 m_TupleExtractInst(m_Specific(FoundInc), 0),
                                 m_SILValue(End))) &&
        !match(Cond,
               m_ApplyInst(BuiltinValueKind::ICMP_EQ, m_SILValue(End),
                           m_TupleExtractInst(m_Specific(FoundInc), 0)))) {
      DEBUG(llvm::dbgs() << " found no exit condition\n");
      return nullptr;
    }

    // Make sure our end value is loop invariant.
    if (!dominates(DT, End, Header))
      return nullptr;

    DEBUG(llvm::dbgs() << " found an induction variable (ICMP_EQ): " << *Arg
                       << "  start: " << *Start.getDef()
                       << "  end: " << *End.getDef());

    // Check whether the addition is overflow checked by a cond_fail or whether
    // code in the preheader's predecessor ensures that we won't overflow.
    bool CondFailOnOverflow = isOverflowChecked(FoundInc);
    bool IsRangeChecked =
        CondFailOnOverflow ? false : isRangeChecked(Start, End, Preheader, DT);

    if (!CondFailOnOverflow && !IsRangeChecked)
      return nullptr;

    return new (Allocator.Allocate()) InductionInfo(
        Arg, FoundInc, Start, End, BuiltinValueKind::ICMP_EQ, IsRangeChecked);
  }

  DominanceInfo *DT;
  SILBasicBlock *Preheader;
  SILBasicBlock *Header;
  SILBasicBlock *ExitingBlk;
  IVInfo &IVs;
  InductionInfoMap Map;
  llvm::SpecificBumpPtrAllocator<InductionInfo> Allocator;
};

/// Hoist the bounds check \p CheckToHoist to the preheader updating the index
/// to \p Index.
/// The set of instructions to retain the array starting at the load is to be
/// specified in \p RetainSequence.
static void hoistBoundsCheckCallWithIndex(SILInstruction *CheckToHoist,
                                          SILValue Index,
                                          ValueBaseList &RetainSequence,
                                          SILBasicBlock *Preheader) {
  assert(isa<LoadInst>(RetainSequence[0]) ||
         isa<SILArgument>(RetainSequence[0]) &&
             "Expect a LoadInst or an Array parameter");
  auto *InsertPt = Preheader->getTerminator();

  // Clone and fixup the load, retain sequenence to the header.
  auto *LI =
    dyn_cast<LoadInst>(RetainSequence[0]);
  // We might also have an array argument.
  ValueBase *PrevI =
      LI ? LI->clone(InsertPt) : RetainSequence[0];
  ValueBase *Array = PrevI;

  for (unsigned i = 1, e = RetainSequence.size(); i != e; ++i) {
    auto *NewI = cast<SILInstruction>(RetainSequence[i])->clone(InsertPt);
    NewI->setOperand(0, PrevI);
    PrevI = NewI;
  }
  // Clone the check and update index and array.
  auto NewCheck = cast<ApplyInst>(CheckToHoist->clone(InsertPt));
  // Set index.
  NewCheck->setOperand(1, Index);
  // Set array.
  NewCheck->setOperand(2, Array);
  // Clone the function reference.
  auto ClonedRef =
      cast<FunctionRefInst>(NewCheck->getOperand(0))->clone(NewCheck);
  NewCheck->setOperand(0, ClonedRef);
}

/// A block in the loop is guarantueed to be excuted if it dominates the exiting
/// block.
static bool isGuarantueedToBeExecuted(DominanceInfo *DT, SILBasicBlock *Block,
                                      SILBasicBlock *ExitingBlk) {
  return DT->dominates(Block, ExitingBlk);
}

/// Describes the access function "a[f(i)]" that is based on a cannonical
/// induction variable.
class AccessFunction {
  InductionInfo *Ind;

  AccessFunction(InductionInfo *I) { Ind = I; }
public:

  operator bool() { return Ind != nullptr; }

  static AccessFunction getLinearFunction(SILValue Idx,
                                          InductionAnalysis &IndVars) {
    auto ArrayIndexStruct = dyn_cast<StructInst>(Idx);
    if (!ArrayIndexStruct)
      return nullptr;

    auto AsArg =
        dyn_cast<SILArgument>(ArrayIndexStruct->getElements()[0].getDef());
    if (!AsArg)
      return nullptr;

    if (auto *Ind = IndVars[AsArg])
      return AccessFunction(Ind);

    return nullptr;
  }

  /// Hoists the necessary check for beginning and end of the induction
  /// encapsulated by this acess function to the header.
  void hoistCheckToPreheader(SILInstruction *CheckToHoist,
                             SILBasicBlock *Preheader,
                             ValueBaseList &RetainSequence) {
    SILBuilder Builder(Preheader->getTerminator());
    SILLocation Loc = CheckToHoist->getLoc();

    // Get the first induction value.
    auto FirstVal = Ind->getFirstValue();
    // Clone the struct for the start index.
    auto Start = cast<SILInstruction>(CheckToHoist->getOperand(1))
                     ->clone(Preheader->getTerminator());
    // Set the new start index to the first value of the induction.
    Start->setOperand(0, FirstVal);

    hoistBoundsCheckCallWithIndex(CheckToHoist, Start, RetainSequence,
                                  Preheader);

    // Get the last induction value.
    auto LastVal = Ind->getLastValue(Loc, Builder);
    // Clone the struct for the end index.
    auto End = cast<SILInstruction>(CheckToHoist->getOperand(1))
                   ->clone(Preheader->getTerminator());
    // Set the new end index to the last value of the induction.
    End->setOperand(0, LastVal);

    hoistBoundsCheckCallWithIndex(CheckToHoist, End, RetainSequence, Preheader);
  }
};

/// Eliminate a check by hoisting it to the loop's preheader.
static bool hoistCheck(SILBasicBlock *Preheader, SILInstruction *CheckToHoist,
                       AccessFunction &Access) {
  ValueBaseList RetainSequence;
  auto R = findMatchingRetain(CheckToHoist, &RetainSequence);

  // The start of the retain sequence needs to be a load or the array struct
  // passed as an argument.
  if (!R || (!isa<LoadInst>(RetainSequence[0]) &&
             !isa<SILArgument>(RetainSequence[0])))
    return false;

  // Hoist the access function and the check to the preheader for start and end
  // of the induction.
  Access.hoistCheckToPreheader(CheckToHoist, Preheader, RetainSequence);

  // Remove the old check in the loop and the matching retain.
  CheckToHoist->eraseFromParent();
  R->eraseFromParent();

  DEBUG(llvm::dbgs() << "  Bounds check hoisted\n");
  return true;
}

/// Hoists an loop invariant bounds check.
static bool hoistInvariantCheck(SILBasicBlock *Preheader,
                                SILInstruction *Inst) {
  ValueBaseList RetainSequence;
  auto R = findMatchingRetain(Inst, &RetainSequence);
  if (!R || (!isa<LoadInst>(RetainSequence[0]) &&
             !isa<SILArgument>(RetainSequence[1])))
    return false;

  auto FR = dyn_cast<FunctionRefInst>(Inst->getOperand(0));
  if (!FR)
    return false;
  auto NewFR = FR->clone(Preheader->getTerminator());

  for (auto *V : RetainSequence) {
    // Don't need to move the argument.
    if (isa<SILArgument>(V))
      continue;
    auto Inst = cast<SILInstruction>(V);
    Inst->moveBefore(Preheader->getTerminator());
  }

  Inst->moveBefore(Preheader->getTerminator());
  Inst->setOperand(0, NewFR);

  return true;
}

/// Hoist bounds check in the loop to the loop preheader.
static bool hoistChecksInLoop(DominanceInfo *DT, DominanceInfoNode *DTNode,
                              ArraySet &SafeArrays, InductionAnalysis &IndVars,
                              SILBasicBlock *Preheader, SILBasicBlock *Header,
                              SILBasicBlock *ExitingBlk) {

  bool Changed = false;
  auto *CurBB = DTNode->getBlock();
  if (!isGuarantueedToBeExecuted(DT, CurBB, ExitingBlk))
      return false;

  for (auto Iter = CurBB->begin(); Iter != CurBB->end();) {
    auto Inst = &*Iter;
    ++Iter;

    SILValue ArrayVal;
    // Is this a check_bounds.
    auto Kind = isArrayKindCall(Inst, ArrayVal);
    if (Kind != ArrayCallKind::kCheckSubscript &&
        Kind != ArrayCallKind::kCheckIndex) {
      DEBUG(llvm::dbgs() << " not a check_bounds call " << *Inst);
      continue;
    }

    // Get the underlying array pointer.
    SILValue Array = getArrayStructPointer(Kind, ArrayVal);

    // The array must dominate the header.
    if (!dominates(DT, Array.getDef(), Header)) {
      DEBUG(llvm::dbgs() << " does not dominated header" << *Array.getDef());
      continue;
    }

    // Is this a safe array check whose size could not have changed.
    if (!SafeArrays.count(Array)) {
      DEBUG(llvm::dbgs() << " not a safe array argument " << *Array.getDef());
      continue;
    }

    // Get the array index.
    auto ArrayIndex = getArrayIndex(Inst);
    if (!ArrayIndex)
      continue;

    // Invariant check.
    if (dominates(DT, ArrayIndex, Header)) {
      Changed |= hoistInvariantCheck(Preheader, Inst);
      DEBUG(llvm::dbgs() << " " << (Changed ? "could " : "couldn't ")
                         << "hoist invariant bounds check: " << *Inst);
      continue;
    }

    // Get the access function "a[f(i)]". At the moment this handles only the
    // identity function.
    auto F = AccessFunction::getLinearFunction(ArrayIndex, IndVars);
    if (!F)
      continue;

    DEBUG(llvm::dbgs() << " can hoist " << *Inst);
    Changed |= hoistCheck(Preheader, Inst, F);
  }

  DEBUG(Preheader->getParent()->dump());
  // Traverse the children in the dominator tree.
  for (auto Child: *DTNode)
    Changed |= hoistChecksInLoop(DT, Child, SafeArrays, IndVars, Preheader,
                                 Header, ExitingBlk);

  return Changed;
}

/// Analyse the loop for arrays that are not modified and perform dominator tree
/// based redundant bounds check removal.
static bool hoistBoundsChecks(SILLoop *Loop, DominanceInfo *DT, SILLoopInfo *LI,
                              IVInfo &IVs, bool ShouldVerify) {
  auto *Header = Loop->getHeader();
  if (!Header) return false;

  auto *Preheader = Loop->getLoopPreheader();
  if (!Preheader) {
    // TODO: create one if neccessary.
    return false;
  }

  // Only handle innermost loops for now.
  if (!Loop->getSubLoops().empty())
    return false;

  DEBUG(llvm::dbgs() << "Attempting to remove redundant checks in " << *Loop);
  DEBUG(Header->getParent()->dump());

  // Collect safe arrays. Arrays are safe if there is no function call that
  // could mutate their size in the loop.
  ABCAnalysis ABC;
  for (auto *BB : Loop->getBlocks())
    // If analyseBlock fails we have seen an instruction that might-modify any
    // array.
    if (!ABC.analyseBlock(BB))
      return false;

  ArraySet &SafeArrays = ABC.getSafeArrays();

  // Debug
  DEBUG(llvm::dbgs() << "Safe arrays:\n";
        for (auto Arr
             : SafeArrays) { llvm::dbgs() << " " << *Arr.getDef(); });

  // Remove redundant checks down the dominator tree starting at the header.
  IndexedArraySet DominatingSafeChecks;
  bool Changed = removeRedundantChecks(DT->getNode(Header), SafeArrays,
                                       DominatingSafeChecks);

  if (!EnableABCHoisting)
    return Changed;

  DEBUG(llvm::dbgs() << "Attempting to hoist checks in " << *Loop);

  // Find an exiting block.
  SILBasicBlock *ExitingBlk = Loop->getExitingBlock();
  SILBasicBlock *Latch = Loop->getLoopLatch();
  if (!ExitingBlk || ExitingBlk != Latch) {
    DEBUG(llvm::dbgs()
          << "No single exiting block or the latch is not exiting\n");
    return Changed;
  }

  DEBUG(Preheader->getParent()->dump());

  // Find cannonical induction variables.
  InductionAnalysis IndVars(DT, IVs, Preheader, Header, ExitingBlk);
  bool IVarsFound = IndVars.analyse();
  if (!IVarsFound){
    DEBUG(llvm::dbgs() << "No induction variables found\n");
  }

  // Hoist the overflow check of induction variables out of the loop. This also
  // needs to happen for memory safety.
  if (IVarsFound)
    for (auto *Arg: Header->getBBArgs())
      if (auto *IV = IndVars[Arg]) {
        SILBuilder B(Preheader->getTerminator());
        IV->checkOverflow(B);
      }

  DEBUG(Preheader->getParent()->dump());

  // Hoist bounds checks.
  if (!SafeArrays.empty())
    Changed |= hoistChecksInLoop(DT, DT->getNode(Header), SafeArrays, IndVars,
                                 Preheader, Header, ExitingBlk);
  return Changed;
}

#ifndef NDEBUG
static void reportBoundsChecks(SILFunction *F) {
  unsigned NumBCs = 0;

  F->dump();
  for (auto &BB : *F) {
    for (auto &Inst : BB) {
      SILValue Array;
      auto Kind = isArrayKindCall(&Inst, Array);
      if (Kind != ArrayCallKind::kCheckSubscript)
        continue;
      ++NumBCs;
      llvm::dbgs() << " # CheckBounds: " << Inst
                   << "     with array arg: " << *Array.getDef()
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

    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    assert(LA);
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    assert(DA);
    IVAnalysis *IVA = PM->getAnalysis<IVAnalysis>();
    assert(IVA);

    SILFunction *F = getFunction();
    assert(F);
    SILLoopInfo *LI = LA->getLoopInfo(F);
    assert(LI);
    DominanceInfo *DT = DA->getDomInfo(F);
    assert(DT);
    IVInfo &IVs = IVA->getIVInfo(F);

    if (ShouldReportBoundsChecks) { reportBoundsChecks(F); };

    // Remove redundant checks on a per basic block basis.
    bool Changed = false;
    for (auto &BB : *F)
      Changed |= removeRedundantChecksInBlock(BB);

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
                                       ShouldVerify);
        }
      }

      if (ShouldReportBoundsChecks) { reportBoundsChecks(F); };
    }

    if (Changed)
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::Instructions);
  }
};
}

SILTransform *swift::createABCOpt() {
  return new ABCOpt();
}
