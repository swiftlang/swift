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
#include "ARCAnalysis.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/Support/Debug.h"

//===----------------------------------------------------------------------===//
//                             Decrement Analysis
//===----------------------------------------------------------------------===//

/// Could Inst decrement the ref count associated with target?
bool swift::arc::cannotDecrementRefCount(SILInstruction *Inst,
                                         SILValue Target) {
  // Clear up some small cases where MayHaveSideEffects is too broad for our
  // purposes and the instruction does not decrement ref counts.
  switch (Inst->getKind()) {
  case ValueKind::DeallocStackInst:
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainAutoreleasedInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::PartialApplyInst:
  case ValueKind::CondFailInst:
    return true;
  case ValueKind::CopyAddrInst: {
    auto *CA = cast<CopyAddrInst>(Inst);
    if (CA->isInitializationOfDest() == IsInitialization_t::IsInitialization)
      return true;
  }
  SWIFT_FALLTHROUGH;
  default:
    break;
  }

  if (auto *AI = dyn_cast<ApplyInst>(Inst)) {
    // Ignore any thick functions for now due to us not handling the ref-counted
    // nature of its context.
    if (auto FTy = AI->getCallee().getType().getAs<SILFunctionType>())
      if (!FTy->isThin())
        return false;

    // If we have a builtin that is side effect free, we can commute the
    // ApplyInst and the retain.
    if (auto *BI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
      if (isSideEffectFree(BI))
        return true;

    return false;
  }

  // Just make sure that we do not have side effects.
  return Inst->getMemoryBehavior() !=
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
  case ValueKind::AllocBoxInst:
  case ValueKind::AllocArrayInst:
  case ValueKind::MetatypeInst:
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

/// Can Inst use Target in a manner that requires Target to be alive at Inst?
bool swift::arc::cannotUseValue(SILInstruction *Inst, SILValue Target) {
  // If Inst is an instruction that we know can never use values with reference
  // semantics, return true.
  if (canInstUseRefCountValues(Inst))
    return true;

  // Otherwise, assume that Inst can use Target.
  return false;
}
