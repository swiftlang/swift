//===-------------- ARCAnalysis.cpp - SIL ARC Analysis --------------------===//
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

#define DEBUG_TYPE "sil-arc-analysis"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::arc;

//===----------------------------------------------------------------------===//
//                             Decrement Analysis
//===----------------------------------------------------------------------===//

static bool isKnownToNotDecrementRefCount(FunctionRefInst *FRI) {
   return llvm::StringSwitch<bool>(FRI->getReferencedFunction()->getName())
     .Case("swift_keepAlive", true)
     .Case("_swift_isUniquelyReferenced", true)
     .Default(false);
}

static bool canApplyDecrementRefCount(ApplyInst *AI, SILValue Ptr,
                                      AliasAnalysis *AA) {
  // Ignore any thick functions for now due to us not handling the ref-counted
  // nature of its context.
  if (auto FTy = AI->getCallee().getType().getAs<SILFunctionType>())
    if (FTy->getExtInfo().hasContext())
      return true;

  // If we have a builtin that is side effect free, we can commute the
  // ApplyInst and the retain.
  if (auto *BI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
    if (isSideEffectFree(BI))
      return false;

  // swift_keepAlive can not retain values. Remove this when we get rid of that.
  if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
    if (isKnownToNotDecrementRefCount(FRI))
      return false;

  // Ok, this apply *MAY* decrement ref counts. Now our strategy is to attempt
  // to use properties of the pointer, the function's arguments, and the
  // function itself to prove that the pointer can not have its ref count be
  // effected by function.

  // TODO: Put in function property check section here when we get access to
  // such information.

  // First make sure that the underlying object of ptr is a local object which
  // does not escape. This prevents the apply from indirectly via the global
  // affecting the reference count of the pointer.
  if (!isNonEscapingLocalObject(getUnderlyingObject(Ptr)))
    return true;

  // Now that we know that the function can not affect the pointer indirectly,
  // make sure that the apply can not affect the pointer directly via the
  // applies arguments by proving that the pointer can not alias any of the
  // functions arguments.
  for (auto Op : AI->getArgumentsWithoutIndirectResult()) {
    for (int i = 0, e = Ptr->getNumTypes(); i < e; i++) {
      if (!AA->isNoAlias(Op, SILValue(Ptr.getDef(), i)))
        return true;
    }
  }

  // Success! The apply inst can not affect the reference count of ptr!
  return false;
}

/// Is the may have side effects user by the definition of its value kind unable
/// to decrement ref counts.
static bool canDecrementRefCountsByValueKind(SILInstruction *User) {
  assert(User->getMemoryBehavior()
           ==  SILInstruction::MemoryBehavior::MayHaveSideEffects &&
         "Invalid argument. Function is only applicable to isntructions with "
         "side effects.");
  switch (User->getKind()) {
  case ValueKind::DeallocStackInst:
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainAutoreleasedInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::PartialApplyInst:
  case ValueKind::FixLifetimeInst:
  case ValueKind::CopyBlockInst:
  case ValueKind::RetainValueInst:
  case ValueKind::CondFailInst:
    return false;

  case ValueKind::CopyAddrInst: {
    auto *CA = cast<CopyAddrInst>(User);
    if (CA->isInitializationOfDest() == IsInitialization_t::IsInitialization)
      return false;
  }
  SWIFT_FALLTHROUGH;
  default:
    return true;
  }
}

bool swift::arc::canDecrementRefCount(SILInstruction *User,
                                      SILValue Ptr, AliasAnalysis *AA) {
  // If we have an instruction that does not have *pure* side effects, it can
  // not affect ref counts.
  //
  // This distinguishes in between a "write" side effect and ref count side
  // effects.
  if (User->getMemoryBehavior() !=
      SILInstruction::MemoryBehavior::MayHaveSideEffects)
    return false;

  // Ok, we know that this instruction's generic behavior is
  // "MayHaveSideEffects". That is a criterion (it has effects not represented
  // by use-def chains) that is broader than ours (does it effect a particular
  // pointers ref counts). Thus begin by attempting to prove that the type of
  // instruction that the user is by definition can not decrement ref counts.
  if (!canDecrementRefCountsByValueKind(User))
    return false;

  // Ok, this instruction may have ref counts. If it is an apply, attempt to
  // prove that the callee is unable to affect Ptr.
  if (auto *AI = dyn_cast<ApplyInst>(User))
    return canApplyDecrementRefCount(AI, Ptr, AA);

  // We can not conservatively prove that this instruction can not decrement the
  // ref count of Ptr. So assume that it does.
  return true;
}

bool swift::arc::canCheckRefCount(SILInstruction *User) {
  if (auto *AI = dyn_cast<ApplyInst>(User))
    if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
        return FRI->getReferencedFunction()->getName() ==
            "_swift_isUniquelyReferenced";
  return false;
}

//===----------------------------------------------------------------------===//
//                                Use Analysis
//===----------------------------------------------------------------------===//

/// Returns true if a builtin apply can not use reference counted values.
///
/// The main case that this handles here are builtins that via read none imply
/// that they can not read globals and at the same time do not take any
/// non-trivial types via the arguments. The reason why we care about taking
/// non-trivial types as arguments is that we want to be careful in the face of
/// intrinsics that may be equivalent to bitcast and inttoptr operations.
static bool canApplyOfBuiltinUseNonTrivialValues(ApplyInst *AI,
                                                 BuiltinFunctionRefInst *BFRI) {
  SILModule &Mod = AI->getModule();

  auto &II = BFRI->getIntrinsicInfo();
  if (II.ID != llvm::Intrinsic::not_intrinsic) {
    if (II.hasAttribute(llvm::Attribute::ReadNone)) {
      for (auto &Op : AI->getArgumentOperands()) {
        if (!Op.get().getType().isTrivial(Mod)) {
          return false;
        }
      }
    }

    return true;
  }

  auto &BI = BFRI->getBuiltinInfo();
  if (BI.isReadNone()) {
    for (auto &Op : AI->getArgumentOperands()) {
      if (!Op.get().getType().isTrivial(Mod)) {
        return false;
      }
    }
  }

  return true;
}

/// Returns true if Inst is a function that we know never uses ref count values.
static bool canInstUseRefCountValues(SILInstruction *Inst) {
  switch (Inst->getKind()) {
  // These instructions do not use other values.
  case ValueKind::FunctionRefInst:
  case ValueKind::BuiltinFunctionRefInst:
  case ValueKind::IntegerLiteralInst:
  case ValueKind::FloatLiteralInst:
  case ValueKind::StringLiteralInst:
  case ValueKind::AllocStackInst:
  case ValueKind::AllocRefInst:
  case ValueKind::AllocRefDynamicInst:
  case ValueKind::AllocBoxInst:
  case ValueKind::AllocArrayInst:
  case ValueKind::MetatypeInst:
  case ValueKind::WitnessMethodInst:
    return true;

  // DeallocStackInst do not use reference counted values, only local storage
  // handles.
  case ValueKind::DeallocStackInst:
    return true;

  // Debug values do not use referenced counted values in a manner we care
  // about.
  case ValueKind::DebugValueInst:
  case ValueKind::DebugValueAddrInst:
    return true;

  // Casts do not use pointers in a manner that we care about since we strip
  // them during our analysis. The reason for this is if the cast is not dead
  // then there must be some other use after the cast that we will protect if a
  // release is not in between the cast and the use.
  case ValueKind::UpcastInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UncheckedRefBitCastInst:
    return true;

  // If we have a trivial bit cast between trivial types, it is not something
  // that can use ref count ops in a way we care about. We do need to be careful
  // with uses with ref count inputs. In such a case, we assume conservatively
  // that the bit cast could use it.
  //
  // The reason why this is different from the ref bitcast is b/c the use of a
  // ref bit cast is still a ref typed value implying that our ARC dataflow will
  // properly handle its users. A conversion of a reference count value to a
  // trivial value though could be used as a trivial value in ways that ARC
  // dataflow will not understand implying we need to treat it as a use to be
  // safe.
  case ValueKind::UncheckedTrivialBitCastInst: {
    SILValue Op = cast<UncheckedTrivialBitCastInst>(Inst)->getOperand();
    return Op.getType().isTrivial(Inst->getModule());
  }

  // Typed GEPs do not use pointers. The user of the typed GEP may but we will
  // catch that via the dataflow.
  case ValueKind::StructExtractInst:
  case ValueKind::TupleExtractInst:
  case ValueKind::StructElementAddrInst:
  case ValueKind::TupleElementAddrInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
  case ValueKind::RefElementAddrInst:
  case ValueKind::UncheckedEnumDataInst:
  case ValueKind::IndexAddrInst:
  case ValueKind::IndexRawPointerInst:
      return true;

  // Aggregate formation by themselves do not create new uses since it is their
  // users that would create the appropriate uses.
  case ValueKind::EnumInst:
  case ValueKind::StructInst:
  case ValueKind::TupleInst:
    return true;

  // Only uses non reference counted values.
  case ValueKind::CondFailInst:
    return true;

  case ValueKind::ApplyInst: {
    auto *AI = cast<ApplyInst>(Inst);

    // Certain builtin function refs we know can never use non-trivial values.
    if (auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
      return canApplyOfBuiltinUseNonTrivialValues(AI, BFRI);

    return false;
  }

  default:
    return false;
  }
}



bool swift::arc::canUseValue(SILInstruction *User, SILValue Ptr,
                             AliasAnalysis *AA) {
  // If Inst is an instruction that we know can never use values with reference
  // semantics, return true.
  if (canInstUseRefCountValues(User))
    return false;

  // If the user is a load or a store and we can prove that it does not access
  // the object then return true.
  // Notice that we need to check all of the values of the object.
  if (isa<StoreInst>(User)) {
    for (int i = 0, e = Ptr->getNumTypes(); i < e; i++) {
      if (AA->mayWriteToMemory(User, SILValue(Ptr.getDef(), i)))
        return true;
    }
    return false;
  }

  if (isa<LoadInst>(User) ) {
    for (int i = 0, e = Ptr->getNumTypes(); i < e; i++) {
      if (AA->mayReadFromMemory(User, SILValue(Ptr.getDef(), i)))
        return true;
    }
    return false;
  }

  // TODO: If we add in alias analysis support here for apply inst, we will need
  // to check that the pointer does not escape.

  // Otherwise, assume that Inst can use Target.
  return true;
}


//===----------------------------------------------------------------------===//
// Utility Methods for determining use, decrement of values in a contiguous
// instruction range in one BB.
//===----------------------------------------------------------------------===//

/// Return true if \p Op has arc uses in the instruction range [Start, End). We
/// assume that Start and End are both in the same basic block.
bool swift::arc::
valueHasARCUsesInInstructionRange(SILValue Op,
                                  SILBasicBlock::iterator Start,
                                  SILBasicBlock::iterator End,
                                  AliasAnalysis *AA) {
  assert(Start->getParent() == End->getParent() &&
         "Start and End should be in the same basic block");

  // If Start == End, then we have an empty range, return false.
  if (Start == End)
    return false;

  // Otherwise, until Start != End.
  while (Start != End) {
    // Check if Start can use Op in an ARC relevant way. If so, return true.
    if (canUseValue(&*Start, Op, AA))
      return true;

    // Otherwise, increment our iterator.
    ++Start;
  }

  // If all such instructions can not use Op, return false.
  return false;
}

/// Return true if \p Op has instructions in the instruction range (Start, End]
/// which may decrement it. We assume that Start and End are both in the same
/// basic block.
bool swift::arc::
valueHasARCDecrementOrCheckInInstructionRange(SILValue Op,
                                              SILBasicBlock::iterator Start,
                                              SILBasicBlock::iterator End,
                                              AliasAnalysis *AA) {
  assert(Start->getParent() == End->getParent() &&
         "Start and End should be in the same basic block");

  // If Start == End, then we have an empty range, return false.
  if (Start == End)
    return false;

  // Otherwise, until Start != End.
  while (Start != End) {
    // Check if Start can decrement or check Op's ref count. If so, return true.
    // Ref count checks do not have side effects, but are barriers for retains.
    if (canDecrementRefCount(&*Start, Op, AA) || canCheckRefCount(&*Start))
      return true;
    // Otherwise, increment our iterator.
    ++Start;
  }

  // If all such instructions can not decrement Op, return false.
  return false;
}
