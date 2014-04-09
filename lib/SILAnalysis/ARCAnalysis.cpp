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
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                             Decrement Analysis
//===----------------------------------------------------------------------===//


bool swift::arc::canDecrementRefCount(SILInstruction *User,
                                      SILValue Ptr, AliasAnalysis *AA) {
  // Clear up some small cases where MayHaveSideEffects is too broad for our
  // purposes and the instruction does not decrement ref counts.
  switch (User->getKind()) {
  case ValueKind::DeallocStackInst:
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainAutoreleasedInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::PartialApplyInst:
  case ValueKind::CondFailInst:
    return false;

  case ValueKind::CopyAddrInst: {
    auto *CA = cast<CopyAddrInst>(User);
    if (CA->isInitializationOfDest() == IsInitialization_t::IsInitialization)
      return false;
  }
  SWIFT_FALLTHROUGH;
  default:
    break;
  }

  if (auto *AI = dyn_cast<ApplyInst>(User)) {
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

    // Ok, this apply *MAY* decrement ref counts. Attempt to prove that it
    // cannot decrement Target using alias analysis and knowledge about our
    // calling convention.
    for (auto Op : AI->getArgumentsWithoutIndirectResult()) {
      for (int i = 0, e = Ptr.getDef()->getNumTypes(); i < e; i++) {
        if (!AA->isNoAlias(Op, SILValue(Ptr.getDef(), i)))
          return true;
      }
    }

    return false;
  }

  // Just make sure that we do not have side effects.
  return User->getMemoryBehavior() ==
    SILInstruction::MemoryBehavior::MayHaveSideEffects;
}

//===----------------------------------------------------------------------===//
//                                Use Analysis
//===----------------------------------------------------------------------===//

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
    for (int i = 0, e = Ptr.getDef()->getNumTypes(); i < e; i++) {
      if (AA->mayWriteToMemory(User, SILValue(Ptr.getDef(), i)))
        return true;
    }
    return false;
  }

  if (isa<LoadInst>(User) ) {
    for (int i = 0, e = Ptr.getDef()->getNumTypes(); i < e; i++) {
      if (AA->mayReadFromMemory(User, SILValue(Ptr.getDef(), i)))
        return true;
    }
    return false;
  }


  // Otherwise, assume that Inst can use Target.
  return true;
}
