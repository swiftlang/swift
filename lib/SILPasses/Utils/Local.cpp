//===--- Local.cpp - Functions that perform local SIL transformations. ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/Intrinsics.h"

using namespace swift;

static bool isSideEffectFree(BuiltinFunctionRefInst *FR) {

  // First, check if we are dealing with a swift builtin.
  const BuiltinInfo &BInfo = FR->getBuiltinInfo();
  if (BInfo.ID != BuiltinValueKind::None) {
    return BInfo.isReadNone();
  }

  // Second, specialcase llvm intrinsic.
  const IntrinsicInfo & IInfo = FR->getIntrinsicInfo();
  if (IInfo.ID != llvm::Intrinsic::not_intrinsic) {
    return ( (IInfo.hasAttribute(llvm::Attribute::ReadNone) ||
              IInfo.hasAttribute(llvm::Attribute::ReadOnly)) &&
            IInfo.hasAttribute(llvm::Attribute::NoUnwind) );
  }

  llvm_unreachable("All cases are covered.");
}

/// \brief Perform a fast local check to see if the instruction is dead if all
/// its uses were dead.
///
/// This routine only examines the state of the instruction at hand.
bool swift::isInstructionTriviallyDeadWithoutUses(SILInstruction *I) {
  if (isa<TermInst>(I))
    return false;

  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(I)) {
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef())) {
      return isSideEffectFree(FR);
    }
  }

  if (!I->mayHaveSideEffects())
    return true;

  return false;
}

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This routine only examines the state of the instruction at hand.
bool swift::isInstructionTriviallyDead(SILInstruction *I) {
  if (!I->use_empty())
    return false;

  return isInstructionTriviallyDeadWithoutUses(I);
}

/// \brief If the given instruction is dead, delete it along with its dead
/// operands.
///
/// \param I The instruction to be deleted.
/// \param Force If Force is set, don't check if the top level instruction is
///        considered dead - delete it regardless.
/// \return Returns true if any instructions were deleted.
bool swift::recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I,
                                                       bool Force) {
  // If the instruction is not dead, there is nothing to do.
  if (!I || (!Force && !isInstructionTriviallyDead(I)))
    return false;

  // Delete this instruction and others that become dead after it's deleted.
  SmallVector<SILInstruction*, 16> DeadInsts;
  DeadInsts.push_back(I);
  llvm::SmallPtrSet<SILInstruction*, 8> ErasedInsts;
  do {
    I = DeadInsts.pop_back_val();
    if (ErasedInsts.count(I) > 0)
      continue;

    // Check if any of the operands will become dead as well.
    MutableArrayRef<Operand> Ops = I->getAllOperands();
    for (Operand &Op : Ops) {
      SILValue OpVal = Op.get();
      if (!OpVal) continue;

      // Remove the reference from the instruction being deleted to this
      // operand.
      Op.drop();

      // If the operand is an instruction that is only used by the instruction
      // being deleted, delete it.
      if (SILInstruction *OpValInst = dyn_cast<SILInstruction>(OpVal))
        if (isInstructionTriviallyDead(OpValInst))
          DeadInsts.push_back(OpValInst);
    }

    // This will remove this instruction and all its uses.
    I->eraseFromParent();
    ErasedInsts.insert(I);
  } while (!DeadInsts.empty());

  return true;
}
