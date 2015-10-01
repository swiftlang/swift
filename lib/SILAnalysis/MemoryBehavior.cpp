//===--- MemoryBehavior.cpp -----------------------------------------------===//
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

#define DEBUG_TYPE "sil-membehavior"

#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/SideEffectAnalysis.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                               TBAA Utilities
//===----------------------------------------------------------------------===//

/// Is this an instruction that can act as a type "oracle" allowing typed access
/// TBAA to know what the real types associated with the SILInstruction are.
static bool isTypedAccessOracle(SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::RefElementAddrInst:
  case ValueKind::StructElementAddrInst:
  case ValueKind::TupleElementAddrInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
  case ValueKind::LoadInst:
  case ValueKind::StoreInst:
  case ValueKind::AllocStackInst:
  case ValueKind::AllocBoxInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::DeallocBoxInst:
    return true;
  default:
    return false;
  }
}

/// Look at the origin/user ValueBase of V to see if any of them are
/// TypedAccessOracle which enable one to ascertain via undefined behavior the
/// "true" type of the instruction.
SILType swift::findTypedAccessType(SILValue V) {
  // First look at the origin of V and see if we have any instruction that is a
  // typed oracle.
  if (auto *I = dyn_cast<SILInstruction>(V))
    if (isTypedAccessOracle(I))
      return V.getType();

  // Then look at any uses of V that potentially could act as a typed access
  // oracle.
  for (auto Use : V.getUses())
    if (isTypedAccessOracle(Use->getUser()))
      return V.getType();

  // Otherwise return an empty SILType
  return SILType();
}

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

  /// The alias analysis for any queries we may need.
  AliasAnalysis &AA;

  /// The value we are attempting to discover memory behavior relative to.
  SILValue V;

  /// Should we treat instructions that increment ref counts as None instead of
  /// MayHaveSideEffects.
  bool IgnoreRefCountIncrements;

public:
  MemoryBehaviorVisitor(AliasAnalysis &AA, SILValue V, bool IgnoreRefCountIncs)
      : AA(AA), V(V), IgnoreRefCountIncrements(IgnoreRefCountIncs) {}

  MemBehavior visitValueBase(ValueBase *V) {
    llvm_unreachable("unimplemented");
  }

  MemBehavior visitSILInstruction(SILInstruction *Inst) {
    // If we do not have any more information, just use the general memory
    // behavior implementation.
    auto Behavior = Inst->getMemoryBehavior();
    if (!isLetPointer(V))
      return Behavior;

    switch (Behavior) {
    case MemBehavior::MayHaveSideEffects:
      return MemBehavior::MayRead;
    case MemBehavior::MayReadWrite:
      return MemBehavior::MayRead;
    case MemBehavior::MayWrite:
      return MemBehavior::None;
    default:
      return Behavior;
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

  // Instructions which are none if our SILValue does not alias one of its
  // arguments. If we can not prove such a thing, return the relevant memory
  // behavior.
#define OPERANDALIAS_MEMBEHAVIOR_INST(Name)                             \
  MemBehavior visit##Name(Name *I) {                                    \
    for (Operand &Op : I->getAllOperands()) {                           \
      if (!AA.isNoAlias(Op.get(), V)) {                                 \
        DEBUG(llvm::dbgs() << "  " #Name                                \
              " does alias inst. Returning  Normal behavior.\n");       \
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
    if (IgnoreRefCountIncrements)                                              \
      return MemBehavior::None;                                                \
    return I->getMemoryBehavior();                                             \
  }
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainAutoreleasedInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainUnownedInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(UnownedRetainInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(RetainValueInst)
#undef REFCOUNTINC_MEMBEHAVIOR_INST
};

} // end anonymous namespace

MemBehavior MemoryBehaviorVisitor::visitLoadInst(LoadInst *LI) {
  if (AA.isNoAlias(LI->getOperand(), V, LI->getOperand().getType(),
                   findTypedAccessType(V))) {
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
  // specified value can not be modified by the store.
  if (AA.isNoAlias(SI->getDest(), V, SI->getDest().getType(),
                   findTypedAccessType(V))) {
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
  // If it is an allocstack which does not escape, tryapply instruction can not
  // read/modify the memory.
  if (auto *ASI = dyn_cast<AllocStackInst>(getUnderlyingObject(V)))
    if (isNonEscapingLocalObject(ASI->getAddressResult()))
      Behavior = MemBehavior::None;

  // Otherwise be conservative and return that we may have side effects.
  DEBUG(llvm::dbgs() << "  Found tryapply, returning " << Behavior << '\n');
  return Behavior;
}

MemBehavior MemoryBehaviorVisitor::visitApplyInst(ApplyInst *AI) {

  SideEffectAnalysis::FunctionEffects ApplyEffects;
  AA.getSideEffectAnalysis()->getEffects(ApplyEffects, AI);

  MemBehavior Behavior = MemBehavior::None;

  // We can ignore mayTrap().
  if (ApplyEffects.mayReadRC() ||
      (!IgnoreRefCountIncrements && ApplyEffects.mayAllocObjects())) {
    Behavior = MemBehavior::MayHaveSideEffects;
  } else {
    auto &GlobalEffects = ApplyEffects.getGlobalEffects();
    Behavior = GlobalEffects.getMemBehavior(IgnoreRefCountIncrements);

    // Check all parameter effects.
    for (unsigned Idx = 0, End = AI->getNumArguments();
         Idx < End && Behavior < MemBehavior::MayHaveSideEffects; ++Idx) {
      auto &ArgEffect = ApplyEffects.getParameterEffects()[Idx];
      auto ArgBehavior = ArgEffect.getMemBehavior(IgnoreRefCountIncrements);
      if (ArgBehavior > Behavior) {
        SILValue Arg = AI->getArgument(Idx);
        // We only consider the argument effects if the argument aliases V.
        if (!Arg.getType().isAddress() ||
            !AA.isNoAlias(Arg, V, Arg.getType(), findTypedAccessType(V))) {
          Behavior = ArgBehavior;
        }
      }
    }
  }
  if (Behavior > MemBehavior::MayRead && isLetPointer(V))
    Behavior = MemBehavior::MayRead;

  // If it is an allocstack which does not escape, apply instruction can not
  // read/modify the memory.
  if (auto *ASI = dyn_cast<AllocStackInst>(getUnderlyingObject(V)))
    if (isNonEscapingLocalObject(ASI->getAddressResult())) {
      Behavior = MemBehavior::None;
    }

  DEBUG(llvm::dbgs() << "  Found apply, returning " << Behavior << '\n');
  return Behavior;
}

MemBehavior
MemoryBehaviorVisitor::visitStrongReleaseInst(StrongReleaseInst *SI) {
  // Need to make sure that the allocated memory does not escape.
  // AllocBox to stack does not check for whether the address of promoted
  // allocstack can escape.
  //
  // TODO: come up with a test case which shows isNonEscapingLocalObject is
  // necessary.
  if (AllocStackInst *ASI = dyn_cast<AllocStackInst>(getUnderlyingObject(V)))
    if (isNonEscapingLocalObject(ASI->getAddressResult()))
      return MemBehavior::None;
  return MemBehavior::MayHaveSideEffects;
}

MemBehavior
MemoryBehaviorVisitor::visitUnownedReleaseInst(UnownedReleaseInst *SI) {
  // Need to make sure that the allocated memory does not escape.
  if (AllocStackInst *ASI = dyn_cast<AllocStackInst>(getUnderlyingObject(V)))
    if (isNonEscapingLocalObject(ASI->getAddressResult()))
      return MemBehavior::None;
  return MemBehavior::MayHaveSideEffects;
}

MemBehavior MemoryBehaviorVisitor::visitReleaseValueInst(ReleaseValueInst *SI) {
  // Need to make sure that the allocated memory does not escape.
  if (AllocStackInst *ASI = dyn_cast<AllocStackInst>(getUnderlyingObject(V)))
    if (isNonEscapingLocalObject(ASI->getAddressResult()))
      return MemBehavior::None;
  return MemBehavior::MayHaveSideEffects;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

SILInstruction::MemoryBehavior
AliasAnalysis::getMemoryBehavior(SILInstruction *Inst, SILValue V,
                                 bool IgnoreRefCountIncrements) {
  DEBUG(llvm::dbgs() << "GET MEMORY BEHAVIOR FOR:\n    " << *Inst << "    "
                     << *V.getDef());
  return MemoryBehaviorVisitor(*this, V, IgnoreRefCountIncrements).visit(Inst);
}
