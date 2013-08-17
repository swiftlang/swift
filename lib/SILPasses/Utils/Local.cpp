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
#include "llvm/IR/Intrinsics.h"

using namespace swift;

static bool isSideEffectFree(BuiltinFunctionRefInst *FR) {
  // FIXME: This list might not be complete. Would be good to derive this
  // info from llvm.
  switch (FR->getIntrinsicID()) {
    default:
      return false;
    case llvm::Intrinsic::fabs:
    case llvm::Intrinsic::log:
    case llvm::Intrinsic::log2:
    case llvm::Intrinsic::log10:
    case llvm::Intrinsic::exp:
    case llvm::Intrinsic::exp2:
    case llvm::Intrinsic::floor:
    case llvm::Intrinsic::sqrt:
    case llvm::Intrinsic::pow:
    case llvm::Intrinsic::powi:
    case llvm::Intrinsic::bswap:
    case llvm::Intrinsic::ctpop:
    case llvm::Intrinsic::ctlz:
    case llvm::Intrinsic::cttz:
    case llvm::Intrinsic::sadd_with_overflow:
    case llvm::Intrinsic::uadd_with_overflow:
    case llvm::Intrinsic::ssub_with_overflow:
    case llvm::Intrinsic::usub_with_overflow:
    case llvm::Intrinsic::smul_with_overflow:
    case llvm::Intrinsic::umul_with_overflow:
    case llvm::Intrinsic::convert_from_fp16:
    case llvm::Intrinsic::convert_to_fp16:
    case llvm::Intrinsic::x86_sse_cvtss2si:
    case llvm::Intrinsic::x86_sse_cvtss2si64:
    case llvm::Intrinsic::x86_sse_cvttss2si:
    case llvm::Intrinsic::x86_sse_cvttss2si64:
    case llvm::Intrinsic::x86_sse2_cvtsd2si:
    case llvm::Intrinsic::x86_sse2_cvtsd2si64:
    case llvm::Intrinsic::x86_sse2_cvttsd2si:
    case llvm::Intrinsic::x86_sse2_cvttsd2si64:
      return true;
  }

  // FIXME: Special handling of LLVM IR instructions.
  llvm_unreachable("All cases are covered.");
}

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This rutine only examines the state of the instruction at hand.
static bool isInstructionTriviallyDead(SILInstruction *I) {
  if (!I->use_empty() || isa<TermInst>(I))
    return false;

  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(I)) {
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef())) {
      if (isSideEffectFree(FR))
        return true;
    }
  }

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
