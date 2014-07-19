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
#include "swift/SIL/Dominance.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
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
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"

#include <algorithm>

#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<bool> ShouldReportBoundsChecks("sil-abcopts-report",
                                              llvm::cl::init(false));

static llvm::cl::opt<bool> EnableABCOpts("enable-abcopts",
                                         llvm::cl::init(false));

using ArraySet = llvm::SmallPtrSet<SILValue, 16>;
using IndexedArraySet =
    llvm::DenseSet<std::pair<ValueBase *, ValueBase *>>;

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
                                                  SILValue Array) {
  // Is this a retain.
  if (!isa<RetainValueInst>(R))
    return nullptr;

  if (!R->getOperand(0))
    return nullptr;

  // Find the projection from the array:
  // %42 = SILValue(Array)
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

  return R;
}

/// Find a matching preceeding retain on the same array.
static SILInstruction *findMatchingRetain(SILInstruction *BoundsCheck) {
  auto ApplyBoundsCheck = dyn_cast<ApplyInst>(BoundsCheck);
  assert(ApplyBoundsCheck);
  auto Array = ApplyBoundsCheck->getSelfArgument();
  unsigned BoundSearch = 4;
  for (auto E = BoundsCheck->getParent()->rend(),
            Iter = SILBasicBlock::reverse_iterator(BoundsCheck);
       Iter != E; ++Iter) {
    if (auto R = isArrayBufferStorageRetain(&*Iter, Array))
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

/// Analyse the loop for arrays that are not modified and perform dominator tree
/// based redundant bounds check removal.
/// TODO: Actual hoisting.
static bool hoistBoundsChecks(SILLoop *Loop, DominanceInfo *DT, SILLoopInfo *LI,
                              AliasAnalysis *AA, bool ShouldVerify) {
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

  DEBUG(llvm::dbgs() << "Attempting to hoist in " << *Loop);
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
    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();
    assert(AA);

    SILFunction *F = getFunction();
    assert(F);
    SILLoopInfo *LI = LA->getLoopInfo(F);
    assert(LI);
    DominanceInfo *DT = DA->getDomInfo(F);
    assert(DT);

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
          Changed |= hoistBoundsChecks(Worklist.pop_back_val(), DT, LI, AA,
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
