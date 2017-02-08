//===--- MemoryBehavior.cpp -----------------------------------------------===//
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

#define DEBUG_TYPE "sil-membehavior"

#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// The MemoryBehavior Cache must not grow beyond this size.
// We limit the size of the MB cache to 2**14 because we want to limit the
// memory usage of this cache.
static const int MemoryBehaviorAnalysisMaxCacheSize = 16384;

//===----------------------------------------------------------------------===//
//                       Memory Behavior Implementation
//===----------------------------------------------------------------------===//

namespace {

using MemBehavior = SILInstruction::MemoryBehavior;

/// Visitor that determines the memory behavior of an instruction relative to a
/// specific SILValue (i.e. can the instruction cause the value to be read,
/// etc.).
class MemoryBehaviorVisitor
    : public SILInstructionVisitor<MemoryBehaviorVisitor, MemBehavior> {

  AliasAnalysis *AA;

  SideEffectAnalysis *SEA;

  EscapeAnalysis *EA;

  /// The value we are attempting to discover memory behavior relative to.
  SILValue V;

  /// The SILType of the value.
  Optional<SILType> TypedAccessTy;

  /// Should we treat instructions that increment ref counts as None instead of
  /// MayHaveSideEffects.
  RetainObserveKind InspectionMode;

public:
  MemoryBehaviorVisitor(AliasAnalysis *AA, SideEffectAnalysis *SEA,
                        EscapeAnalysis *EA, SILValue V,
                        RetainObserveKind IgnoreRefCountIncs)
      : AA(AA), SEA(SEA), EA(EA), V(V), InspectionMode(IgnoreRefCountIncs) {}

  SILType getValueTBAAType() {
    if (!TypedAccessTy)
      TypedAccessTy = computeTBAAType(V);
    return *TypedAccessTy;
  }

  MemBehavior visitValueBase(ValueBase *V) {
    llvm_unreachable("unimplemented");
  }

  MemBehavior visitSILInstruction(SILInstruction *Inst) {
    // If we do not have any more information, just use the general memory
    // behavior implementation.
    auto Behavior = Inst->getMemoryBehavior();
    bool ReadOnlyAccess = isLetPointer(V);

    // If this is a regular read-write access then return the computed memory
    // behavior.
    if (!ReadOnlyAccess)
      return Behavior;

    // If this is a read-only access to 'let variable' then we can strip away
    // the write access.
    switch (Behavior) {
    case MemBehavior::MayHaveSideEffects: return MemBehavior::MayRead;
    case MemBehavior::MayReadWrite:       return MemBehavior::MayRead;
    case MemBehavior::MayWrite:           return MemBehavior::None;
    default: return Behavior;
    }
  }

  MemBehavior visitLoadInst(LoadInst *LI);
  MemBehavior visitStoreInst(StoreInst *SI);
  MemBehavior visitApplyInst(ApplyInst *AI);
  MemBehavior visitTryApplyInst(TryApplyInst *AI);
  MemBehavior visitBuiltinInst(BuiltinInst *BI);
  MemBehavior visitStrongReleaseInst(StrongReleaseInst *BI);
  MemBehavior visitUnownedReleaseInst(UnownedReleaseInst *BI);
  MemBehavior visitReleaseValueInst(ReleaseValueInst *BI);
  MemBehavior visitSetDeallocatingInst(SetDeallocatingInst *BI);

  // Instructions which are none if our SILValue does not alias one of its
  // arguments. If we cannot prove such a thing, return the relevant memory
  // behavior.
#define OPERANDALIAS_MEMBEHAVIOR_INST(Name)                             \
  MemBehavior visit##Name(Name *I) {                                    \
    for (Operand &Op : I->getAllOperands()) {                           \
      if (!AA->isNoAlias(Op.get(), V)) {                                \
        DEBUG(llvm::dbgs() << "  " #Name                                \
              " does alias inst. Returning Normal behavior.\n");        \
        return I->getMemoryBehavior();                                  \
      }                                                                 \
    }                                                                   \
                                                                        \
    DEBUG(llvm::dbgs() << "  " #Name " does not alias inst. Returning " \
          "None.\n");                                                   \
    return MemBehavior::None;                                           \
  }

  OPERANDALIAS_MEMBEHAVIOR_INST(InjectEnumAddrInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(UncheckedTakeEnumDataAddrInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(InitExistentialAddrInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(DeinitExistentialAddrInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(DeallocStackInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(FixLifetimeInst)
#undef OPERANDALIAS_MEMBEHAVIOR_INST

  // Override simple behaviors where MayHaveSideEffects is too general and
  // encompasses other behavior that is not read/write/ref count decrement
  // behavior we care about.
#define SIMPLE_MEMBEHAVIOR_INST(Name, Behavior)                         \
  MemBehavior visit##Name(Name *I) { return MemBehavior::Behavior; }
  SIMPLE_MEMBEHAVIOR_INST(CondFailInst, None)
#undef SIMPLE_MEMBEHAVIOR_INST

  // If we are asked to treat ref count increments as being inert, return None
  // for these.
  //
  // FIXME: Once we separate the notion of ref counts from reading/writing
  // memory this will be unnecessary.
#define REFCOUNTINC_MEMBEHAVIOR_INST(Name)                                     \
  MemBehavior visit##Name(Name *I) {                                           \
    if (InspectionMode == RetainObserveKind::IgnoreRetains)          \
      return MemBehavior::None;                                                \
    return I->getMemoryBehavior();                                             \
  }
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainUnownedInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(UnownedRetainInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(RetainValueInst)
#undef REFCOUNTINC_MEMBEHAVIOR_INST
};

} // end anonymous namespace

MemBehavior MemoryBehaviorVisitor::visitLoadInst(LoadInst *LI) {
  if (AA->isNoAlias(LI->getOperand(), V, computeTBAAType(LI->getOperand()),
                   getValueTBAAType())) {
    DEBUG(llvm::dbgs() << "  Load Operand does not alias inst. Returning "
                          "None.\n");
    return MemBehavior::None;
  }

  DEBUG(llvm::dbgs() << "  Could not prove that load inst does not alias "
                        "pointer. Returning may read.");
  return MemBehavior::MayRead;
}

MemBehavior MemoryBehaviorVisitor::visitStoreInst(StoreInst *SI) {
  // No store besides the initialization of a "let"-variable
  // can have any effect on the value of this "let" variable.
  if (isLetPointer(V) && SI->getDest() != V)
    return MemBehavior::None;

  // If the store dest cannot alias the pointer in question, then the
  // specified value cannot be modified by the store.
  if (AA->isNoAlias(SI->getDest(), V, computeTBAAType(SI->getDest()),
                   getValueTBAAType())) {
    DEBUG(llvm::dbgs() << "  Store Dst does not alias inst. Returning "
                          "None.\n");
    return MemBehavior::None;
  }

  // Otherwise, a store just writes.
  DEBUG(llvm::dbgs() << "  Could not prove store does not alias inst. "
                        "Returning MayWrite.\n");
  return MemBehavior::MayWrite;
}

MemBehavior MemoryBehaviorVisitor::visitBuiltinInst(BuiltinInst *BI) {
  // If our callee is not a builtin, be conservative and return may have side
  // effects.
  if (!BI) {
    return MemBehavior::MayHaveSideEffects;
  }

  // If the builtin is read none, it does not read or write memory.
  if (!BI->mayReadOrWriteMemory()) {
    DEBUG(llvm::dbgs() << "  Found apply of read none builtin. Returning"
                          " None.\n");
    return MemBehavior::None;
  }

  // If the builtin is side effect free, then it can only read memory.
  if (!BI->mayHaveSideEffects()) {
    DEBUG(llvm::dbgs() << "  Found apply of side effect free builtin. "
                          "Returning MayRead.\n");
    return MemBehavior::MayRead;
  }

  // FIXME: If the value (or any other values from the instruction that the
  // value comes from) that we are tracking does not escape and we don't alias
  // any of the arguments of the apply inst, we should be ok.

  // Otherwise be conservative and return that we may have side effects.
  DEBUG(llvm::dbgs() << "  Found apply of side effect builtin. "
                        "Returning MayHaveSideEffects.\n");
  return MemBehavior::MayHaveSideEffects;
}

MemBehavior MemoryBehaviorVisitor::visitTryApplyInst(TryApplyInst *AI) {
  MemBehavior Behavior = MemBehavior::MayHaveSideEffects;
  // Ask escape analysis.
  if (!EA->canObjectOrContentEscapeTo(V, AI))
    Behavior = MemBehavior::None;

  // Otherwise be conservative and return that we may have side effects.
  DEBUG(llvm::dbgs() << "  Found tryapply, returning " << Behavior << '\n');
  return Behavior;
}

MemBehavior MemoryBehaviorVisitor::visitApplyInst(ApplyInst *AI) {

  SideEffectAnalysis::FunctionEffects ApplyEffects;
  SEA->getEffects(ApplyEffects, AI);

  MemBehavior Behavior = MemBehavior::None;

  // We can ignore mayTrap().
  if (ApplyEffects.mayReadRC() ||
      (InspectionMode == RetainObserveKind::ObserveRetains &&
       ApplyEffects.mayAllocObjects())) {
    Behavior = MemBehavior::MayHaveSideEffects;
  } else {
    auto &GlobalEffects = ApplyEffects.getGlobalEffects();
    Behavior = GlobalEffects.getMemBehavior(InspectionMode);

    // Check all parameter effects.
    for (unsigned Idx = 0, End = AI->getNumArguments();
         Idx < End && Behavior < MemBehavior::MayHaveSideEffects; ++Idx) {
      auto &ArgEffect = ApplyEffects.getParameterEffects()[Idx];
      auto ArgBehavior = ArgEffect.getMemBehavior(InspectionMode);
      auto NewBehavior = combineMemoryBehavior(Behavior, ArgBehavior);
      if (NewBehavior != Behavior) {
        SILValue Arg = AI->getArgument(Idx);
        // We only consider the argument effects if the argument aliases V.
        if (!Arg->getType().isAddress() ||
            !AA->isNoAlias(Arg, V, computeTBAAType(Arg), getValueTBAAType())) {
          Behavior = NewBehavior;
        }
      }
    }
  }
  if (Behavior > MemBehavior::None) {
    if (Behavior > MemBehavior::MayRead && isLetPointer(V))
      Behavior = MemBehavior::MayRead;

    // Ask escape analysis.
    if (!EA->canObjectOrContentEscapeTo(V, AI))
      Behavior = MemBehavior::None;
  }
  DEBUG(llvm::dbgs() << "  Found apply, returning " << Behavior << '\n');
  return Behavior;
}

MemBehavior
MemoryBehaviorVisitor::visitStrongReleaseInst(StrongReleaseInst *SI) {
  if (!EA->canEscapeTo(V, SI))
    return MemBehavior::None;
  return MemBehavior::MayHaveSideEffects;
}

MemBehavior
MemoryBehaviorVisitor::visitUnownedReleaseInst(UnownedReleaseInst *SI) {
  if (!EA->canEscapeTo(V, SI))
    return MemBehavior::None;
  return MemBehavior::MayHaveSideEffects;
}

MemBehavior MemoryBehaviorVisitor::visitReleaseValueInst(ReleaseValueInst *SI) {
  if (!EA->canEscapeTo(V, SI))
    return MemBehavior::None;
  return MemBehavior::MayHaveSideEffects;
}

MemBehavior MemoryBehaviorVisitor::visitSetDeallocatingInst(SetDeallocatingInst *SDI) {
  return MemBehavior::None;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

MemBehavior
AliasAnalysis::computeMemoryBehavior(SILInstruction *Inst, SILValue V,
                                     RetainObserveKind InspectionMode) {
  MemBehaviorKeyTy Key = toMemoryBehaviorKey(SILValue(Inst), V,
                                             InspectionMode);
  // Check if we've already computed this result.
  auto It = MemoryBehaviorCache.find(Key);
  if (It != MemoryBehaviorCache.end()) {
    return It->second;
  }

  // Flush the cache if the size of the cache is too large.
  if (MemoryBehaviorCache.size() > MemoryBehaviorAnalysisMaxCacheSize) {
    MemoryBehaviorCache.clear();
    MemoryBehaviorValueBaseToIndex.clear();

    // Key is no longer valid as we cleared the MemoryBehaviorValueBaseToIndex.
    Key = toMemoryBehaviorKey(SILValue(Inst), V, InspectionMode);
  }

  // Calculate the aliasing result and store it in the cache.
  auto Result = computeMemoryBehaviorInner(Inst, V, InspectionMode);
  MemoryBehaviorCache[Key] = Result;
  return Result;
}

MemBehavior
AliasAnalysis::computeMemoryBehaviorInner(SILInstruction *Inst, SILValue V,
                                          RetainObserveKind InspectionMode) {
  DEBUG(llvm::dbgs() << "GET MEMORY BEHAVIOR FOR:\n    " << *Inst << "    "
                     << *V);
  assert(SEA && "SideEffectsAnalysis must be initialized!");
  return MemoryBehaviorVisitor(this, SEA, EA, V, InspectionMode).visit(Inst);
}

MemBehaviorKeyTy AliasAnalysis::toMemoryBehaviorKey(SILValue V1, SILValue V2,
                                                    RetainObserveKind M) {
  size_t idx1 = MemoryBehaviorValueBaseToIndex.getIndex(V1);
  assert(idx1 != std::numeric_limits<size_t>::max() &&
         "~0 index reserved for empty/tombstone keys");
  size_t idx2 = MemoryBehaviorValueBaseToIndex.getIndex(V2);
  assert(idx2 != std::numeric_limits<size_t>::max() &&
         "~0 index reserved for empty/tombstone keys");
  return {idx1, idx2, M};
}
