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

using namespace swift;

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This rutine only examines the state of the instruction at hand.
static bool isInstructionTriviallyDead(SILInstruction *I) {
  if (!I->use_empty() || isa<TermInst>(I))
    return false;

  if (!I->mayHaveSideEffects())
    return true;

  return false;
}

/// \brief If the given instruction is dead, delete it along with its dead
/// operands.
///
/// \return Returns true if any instructions were deleted.
bool swift::recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I) {
  // If the instruction is not dead, there is nothing to do.
  if (!I || !isInstructionTriviallyDead(I))
    return false;

  // Delete this instruction and others that become dead after it's deleted.
  SmallVector<SILInstruction*, 16> DeadInsts;
  DeadInsts.push_back(I);
  do {
    I = DeadInsts.pop_back_val();

    // Check if any of the operands will become dead as well.
    MutableArrayRef<Operand> Ops = I->getAllOperands();
    for (auto OpI = Ops.begin(), OpE = Ops.end(); OpI != OpE; ++OpI) {
      Operand &Op = *OpI;
      SILValue OpVal = Op.get();

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
  } while (!DeadInsts.empty());
  
  return true;
}
