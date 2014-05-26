//===--- DeadCodeElimination.cpp - Delete dead code  ----------------------===//
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

#define DEBUG_TYPE "sil-dce"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {

// Without any complex analysis, does this instruction seem like
// something that we need to keep?
// FIXME: Reconcile the similarities between this and
//        isInstructionTriviallyDead.
static bool seemsUseful(SILInstruction *I) {
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(I))
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
      return !isSideEffectFree(FR);

  if (I->mayHaveSideEffects())
    return true;

  if (isa<ReturnInst>(I) || isa<AutoreleaseReturnInst>(I))
    return true;

  // FIXME: Rewrite branchy instructions as unconditonal branches
  //        when no instructions are control-dependent on them.
  if (isa<TermInst>(I))
    return true;

  return false;
}

class DCE : public SILFunctionTransform {
  llvm::SmallPtrSet<ValueBase *, 16> Live;
  llvm::SmallVector<SILInstruction *, 64> Worklist;

  /// The entry point to the transformation.
  void run() {
    SILFunction *F = getFunction();

    assert(Worklist.empty() && "Expected to start with an empty worklist!");
    Live.clear();
    markLive(*F);
    if (removeDead(*F))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  void markLive(SILFunction &F);
  bool removeDead(SILFunction &F);

  void markValueLive(ValueBase *V);
  void markTerminatorArgsLive(SILBasicBlock *Pred, SILBasicBlock *Succ,
                              size_t ArgIndex);
  void propagateLiveBlockArgument(SILArgument *Arg);
  void propagateLiveness(SILInstruction *I);

  StringRef getName() override { return "Dead Code Elimination"; }
};

// Keep track of the fact that V is live and add it to our worklist
// so that we can process the values it depends on.
void DCE::markValueLive(ValueBase *V) {
  if (Live.count(V))
    return;

  DEBUG(llvm::dbgs() << "Marking as live:\n");
  DEBUG(V->dump());

  Live.insert(V);

  if (auto *Def = dyn_cast<SILInstruction>(V)) {
    Worklist.push_back(Def);
    return;
  }

  if (auto *Arg = dyn_cast<SILArgument>(V))
    propagateLiveBlockArgument(Arg);
}

// Determine which instructions from this function we need to keep.
void DCE::markLive(SILFunction &F) {
  // Find the initial set of instructions in this function that appear
  // to be live in the sense that they are not trivially something we
  // can delete by examining only that instruction.
  for (auto &BB : F)
    for (auto &I : BB)
      if (seemsUseful(&I))
        markValueLive(&I);

  // Now propagate liveness backwards from each instruction in our
  // worklist, adding new instructions to the worklist as we discover
  // more that we need to keep.
  while (!Worklist.empty()) {
    auto *I = Worklist.pop_back_val();
    propagateLiveness(I);
  }
}

// Mark as live the terminator argument at index ArgIndex in Pred that
// targets Succ.
void DCE::markTerminatorArgsLive(SILBasicBlock *Pred,
                                 SILBasicBlock *Succ,
                                 size_t ArgIndex) {
  auto *Term = Pred->getTerminator();
  switch (Term->getKind()) {
  default:
    llvm_unreachable("Unexpected terminator kind!");

  case ValueKind::UnreachableInst:
  case ValueKind::SwitchIntInst:
    llvm_unreachable("Unexpected argument for terminator kind!");
    break;

  case ValueKind::DynamicMethodBranchInst:
  case ValueKind::SwitchEnumInst:
  case ValueKind::CheckedCastBranchInst:
    assert(ArgIndex == 0 && "Expected a single argument!");

    // We do not need to do anything with these. If the resulting
    // argument is used at the destination these terminators will end
    // up live, and then our normal liveness propagation will mark the
    // single operand of these instructions as live.
    break;

  case ValueKind::BranchInst:
    markValueLive(cast<BranchInst>(Term)->getArg(ArgIndex).getDef());
    break;

  case ValueKind::CondBranchInst: {
    auto *CondBr = cast<CondBranchInst>(Term);

    if (CondBr->getTrueBB() == Succ) {
      auto TrueArgs = CondBr->getTrueArgs();
      markValueLive(TrueArgs[ArgIndex].getDef());
    }

    if (CondBr->getFalseBB() == Succ) {
      auto FalseArgs = CondBr->getFalseArgs();
      markValueLive(FalseArgs[ArgIndex].getDef());
    }

    break;
  }
  }
}

// Propagate liveness back from Arg to the terminator arguments that
// supply its value.
void DCE::propagateLiveBlockArgument(SILArgument *Arg) {
  if (Arg->isFunctionArg())
    return;

  auto *Block = Arg->getParent();
  auto ArgIndex = Block->getBBArgIndex(Arg);

  for (auto Pred : Block->getPreds())
    markTerminatorArgsLive(Pred, Block, ArgIndex);
}

// Given an instruction which is considered live, propagate that liveness
// back to the instructions that produce values it consumes.
void DCE::propagateLiveness(SILInstruction *I) {
  if (!isa<TermInst>(I)) {
    for (auto &O : I->getAllOperands())
      markValueLive(O.get().getDef());
    return;
  }

  switch (I->getKind()) {
  default:
    llvm_unreachable("Unexpected terminator instruction!");

  case ValueKind::UnreachableInst:
  case ValueKind::BranchInst:
    break;

  case ValueKind::ReturnInst:
  case ValueKind::AutoreleaseReturnInst:
  case ValueKind::CondBranchInst:
  case ValueKind::SwitchIntInst:
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchEnumAddrInst:
  case ValueKind::DynamicMethodBranchInst:
  case ValueKind::CheckedCastBranchInst:
    markValueLive(I->getOperand(0).getDef());
    break;

  }
}

// Remove the instructions that are not potentially useful.
bool DCE::removeDead(SILFunction &F) {
  bool Changed = false;

  for (auto &BB : F) {
    for (auto I = BB.begin(), E = BB.end(); I != E; ) {
      auto Inst = I++;
      if (!Live.count(Inst)) {
        for (unsigned i = 0, e = Inst->getNumTypes(); i != e; ++i) {
          auto *Undef = SILUndef::get(Inst->getType(i),
                                      Inst->getModule());
          SILValue(Inst, i).replaceAllUsesWith(Undef);
        }

        DEBUG(llvm::dbgs() << "Removing dead instruction:\n");
        DEBUG(Inst->dump());

        Inst->eraseFromParent();
        Changed = true;
      }
    }
  }

  return Changed;
}


} // end anonymous namespace

SILTransform *swift::createDCE() {
  return new DCE();
}
