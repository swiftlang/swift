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
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SIL/CallGraph.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/Intrinsics.h"
#include <deque>

using namespace swift;

bool
swift::isSideEffectFree(BuiltinFunctionRefInst *FR) {

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

bool swift::isReadNone(BuiltinFunctionRefInst *FR) {
  // First, check if we are dealing with a swift builtin.
  const BuiltinInfo &BInfo = FR->getBuiltinInfo();
  if (BInfo.ID != BuiltinValueKind::None)
    return BInfo.isReadNone();

  // Second, specialcase llvm intrinsic.
  const IntrinsicInfo & IInfo = FR->getIntrinsicInfo();
  if (IInfo.ID != llvm::Intrinsic::not_intrinsic)
    return IInfo.hasAttribute(llvm::Attribute::ReadNone) &&
      IInfo.hasAttribute(llvm::Attribute::NoUnwind);

  llvm_unreachable("All cases are covered.");
}

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This routine only examines the state of the instruction at hand.
bool
swift::isInstructionTriviallyDead(SILInstruction *I) {
  if (!I->use_empty() || isa<TermInst>(I))
    return false;

  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(I)) {
    if (auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee())) {
      return isSideEffectFree(BFRI);
    }

    if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
      // If we call an apply inst to a global initializer, but the value is not
      // used it is safe to remove it.
      if (FRI->getReferencedFunction()->isGlobalInit())
        return true;
  }

  // condfail instructions that obviously can't fail are dead.
  if (auto *CFI = dyn_cast<CondFailInst>(I))
    if (auto *ILI = dyn_cast<IntegerLiteralInst>(CFI->getOperand()))
      if (!ILI->getValue())
        return true;

  // mark_uninitialized is never dead.
  if (isa<MarkUninitializedInst>(I))
    return false;

  // These invalidate enums so "write" memory, but that is not an essential
  // operation so we can remove these if they are trivially dead.
  if (isa<UncheckedTakeEnumDataAddrInst>(I))
    return true;
  
  if (!I->mayHaveSideEffects())
    return true;

  return false;
}

namespace {
  using CallbackTy = std::function<void(SILInstruction *)>;
} // end anonymous namespace

bool swift::
recursivelyDeleteTriviallyDeadInstructions(ArrayRef<SILInstruction *> IA,
                                           bool Force, CallbackTy Callback) {
  // Delete these instruction and others that become dead after it's deleted.
  llvm::SmallPtrSet<SILInstruction *, 8> DeadInsts;
  for (auto I : IA) {
    // If the instruction is not dead or force is false, there is nothing to do.
    if (Force || isInstructionTriviallyDead(I))
      DeadInsts.insert(I);
  }
  llvm::SmallPtrSet<SILInstruction *, 8> NextInsts;
  while (!DeadInsts.empty()) {
    for (auto I : DeadInsts) {
      // Check if any of the operands will become dead as well.
      MutableArrayRef<Operand> Ops = I->getAllOperands();
      for (Operand &Op : Ops) {
        SILValue OpVal = Op.get();
        if (!OpVal)
          continue;

        // Remove the reference from the instruction being deleted to this
        // operand.
        Op.drop();

        // If the operand is an instruction that is only used by the instruction
        // being deleted, delete it.
        if (SILInstruction *OpValInst = dyn_cast<SILInstruction>(OpVal))
          if (!DeadInsts.count(OpValInst) &&
              isInstructionTriviallyDead(OpValInst))
            NextInsts.insert(OpValInst);
      }
    }

    for (auto I : DeadInsts) {
      // This will remove this instruction and all its uses.
      Callback(I);
      I->eraseFromParent();
    }

    NextInsts.swap(DeadInsts);
    NextInsts.clear();
  }

  return true;
}

/// \brief If the given instruction is dead, delete it along with its dead
/// operands.
///
/// \param I The instruction to be deleted.
/// \param Force If Force is set, don't check if the top level instruction is
///        considered dead - delete it regardless.
/// \return Returns true if any instructions were deleted.
bool swift::recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I,
                                                       bool Force,
                                                       CallbackTy Callback) {

  ArrayRef<SILInstruction *> AI = ArrayRef<SILInstruction *>(I);
  return recursivelyDeleteTriviallyDeadInstructions(AI, Force, Callback);
}

void swift::eraseUsesOfInstruction(SILInstruction *Inst) {
  for (auto UI : Inst->getUses()) {
    auto *User = UI->getUser();

    // If the instruction itself has any uses, recursively zap them so that
    // nothing uses this instruction.
    eraseUsesOfInstruction(User);

    // Walk through the operand list and delete any random instructions that
    // will become trivially dead when this instruction is removed.

    for (auto &Op : User->getAllOperands()) {
      if (auto *OpI = dyn_cast<SILInstruction>(Op.get())) {
        // Don't recursively delete the pointer we're getting in.
        if (OpI != Inst) {
          Op.drop();
          recursivelyDeleteTriviallyDeadInstructions(OpI);
        }
      }
    }

    User->eraseFromParent();
  }
}

void swift::bottomUpCallGraphOrder(SILModule *M,
                                   std::vector<SILFunction*> &order) {
  CallGraphSorter<SILFunction*> sorter;
  for (auto &Caller : *M)
    for (auto &BB : Caller)
      for (auto &I : BB)
        if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(&I)) {
          SILFunction *Callee = FRI->getReferencedFunction();
          sorter.addEdge(&Caller, Callee);
        }

  sorter.sort(order);
}

void swift::replaceWithSpecializedFunction(ApplyInst *AI, SILFunction *NewF) {
  SILLocation Loc = AI->getLoc();
  ArrayRef<Substitution> Subst;

  SmallVector<SILValue, 4> Arguments;
  for (auto &Op : AI->getArgumentOperands()) {
    Arguments.push_back(Op.get());
  }

  SILBuilder Builder(AI);
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  ApplyInst *NAI =
      Builder.createApply(Loc, FRI, Arguments, AI->isTransparent());
  AI->replaceAllUsesWith(NAI);
  recursivelyDeleteTriviallyDeadInstructions(AI, true);
}

bool swift::hasUnboundGenericTypes(CanType T) {
  return T->hasArchetype();
}
