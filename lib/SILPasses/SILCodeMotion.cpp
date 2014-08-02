//===- SILCodeMotion.cpp - Code Motion Optimizations ----------------------===//
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

#define DEBUG_TYPE "codemotion"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/Projection.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/RecyclingAllocator.h"

STATISTIC(NumSunk,   "Number of instructions sunk");

using namespace swift;
using namespace swift::arc;

static const int SinkSearchWindow = 6;

/// \brief Returns True if we can sink this instruction to another basic block.
static bool canSinkInstruction(SILInstruction *Inst) {
  return Inst->use_empty() && !isa<TermInst>(Inst);
}

/// \brief Returns true if this instruction is a skip barrier, which means that
/// we can't sink other instructions past it.
static bool isSinkBarrier(SILInstruction *Inst) {
  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(Inst))
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
      return !isSideEffectFree(FR);

  if (isa<TermInst>(Inst))
    return false;

  if (Inst->mayHaveSideEffects())
    return true;

  return false;
}

/// \brief Search for an instruction that is identical to \p Iden by scanning
/// \p BB starting at the end of the block, stopping on sink barriers.
SILInstruction *findIdenticalInBlock(SILBasicBlock *BB, SILInstruction *Iden) {
  int SkipBudget = SinkSearchWindow;

  SILBasicBlock::iterator InstToSink = BB->getTerminator();

  while (SkipBudget) {
    // If we found a sinkable instruction that is identical to our goal
    // then return it.
    if (canSinkInstruction(InstToSink) && Iden->isIdenticalTo(InstToSink)) {
      DEBUG(llvm::dbgs() << "Found an identical instruction.");
      return InstToSink;
    }

    // If this instruction is a skip-barrier end the scan.
    if (isSinkBarrier(InstToSink))
      return nullptr;

    // If this is the first instruction in the block then we are done.
    if (InstToSink == BB->begin())
      return nullptr;

    SkipBudget--;
    InstToSink = std::prev(InstToSink);
    DEBUG(llvm::dbgs() << "Continuing scan. Next inst: " << *InstToSink);
  }

  return nullptr;
}

/// The 2 instructions given are not identical, but are passed as arguments
/// to a common successor.  It may be cheaper to pass one of their operands
/// to the successor instead of the whole instruction.
/// Return ~0U if no such operand could be found, otherwise return the index
/// of a suitable operand.
static unsigned cheaperToPassOperandsAsArguments(SILInstruction *First,
                                                 SILInstruction *Second) {
  // TODO: Add more cases than Struct
  StructInst *FirstStruct = dyn_cast<StructInst>(First);
  StructInst *SecondStruct = dyn_cast<StructInst>(Second);

  if (!FirstStruct || !SecondStruct)
    return ~0U;

  assert(First->getNumOperands() == Second->getNumOperands() &&
         First->getNumTypes() == Second->getNumTypes() &&
         "Types should be identical");

  unsigned DifferentOperandIndex = ~0U;

  // Check operands.
  for (unsigned i = 0, e = First->getNumOperands(); i != e; ++i) {
    if (First->getOperand(i) != Second->getOperand(i)) {
      // Only track one different operand for now
      if (DifferentOperandIndex != ~0U)
        return ~0U;
      DifferentOperandIndex = i;
    }
  }

  if (DifferentOperandIndex == ~0U)
    return ~0U;

  // Found a different operand, now check to see if its type is something
  // cheap enough to sink.
  // TODO: Sink more than just integers.
  const auto &ArgTy = First->getOperand(DifferentOperandIndex).getType();
  if (!ArgTy.is<BuiltinIntegerType>())
    return ~0U;

  return DifferentOperandIndex;
}

// Try to sink values from the Nth argument \p ArgNum.
static bool sinkArgument(SILBasicBlock *BB, unsigned ArgNum) {
  assert(ArgNum < BB->getNumBBArg() && "Invalid argument");

  // Find the first predecessor, the first terminator and the Nth argument.
  SILBasicBlock *FirstPred = *BB->pred_begin();
  TermInst *FirstTerm = FirstPred->getTerminator();
  auto FirstPredArg = FirstTerm->getOperand(ArgNum);
  SILInstruction *FSI = dyn_cast<SILInstruction>(FirstPredArg);

  // The list of identical instructions.
  SmallVector<SILValue, 8> Clones;
  Clones.push_back(FirstPredArg);

  // We only move instructions with a single use.
  if (!FSI || !FSI->hasOneUse())
    return false;

  // Don't move instructions that are sensitive to their location.
  if (FSI->mayHaveSideEffects())
    return false;

  // If the instructions are different, but only in terms of a cheap operand
  // then we can still sink it, and create new arguments for this operand.
  unsigned DifferentOperandIndex = ~0U;

  // Check if the Nth argument in all predecessors is identical.
  for (auto P : BB->getPreds()) {
    if (P == FirstPred)
      continue;

    // Only handle branch or conditional branch instructions.
    TermInst *TI = P->getTerminator();
    if (!isa<BranchInst>(TI) && !isa<CondBranchInst>(TI))
      return false;

    // Find the Nth argument passed to BB.
    SILValue Arg = TI->getOperand(ArgNum);
    SILInstruction *SI = dyn_cast<SILInstruction>(Arg);
    if (!SI || !SI->hasOneUse())
      return false;
    if (SI->isIdenticalTo(FSI)) {
      Clones.push_back(SI);
      continue;
    }

    // If the instructions are close enough, then we should sink them anyway.
    // For example, we should sink 'struct S(%0)' if %0 is small, eg, an integer
    unsigned DifferentOp = cheaperToPassOperandsAsArguments(FSI, SI);
    // Couldn't find a suitable operand, so bail.
    if (DifferentOp == ~0U)
      return false;
    // Make sure we found the same operand as prior iterations.
    if (DifferentOperandIndex == ~0U)
      DifferentOperandIndex = DifferentOp;
    else if (DifferentOp != DifferentOperandIndex)
      return false;

    Clones.push_back(SI);
  }

  if (!FSI)
    return false;

  SILValue Undef = SILUndef::get(FirstPredArg.getType(), BB->getModule());

  if (DifferentOperandIndex != ~0U) {
    // Sink one of the instructions to BB
    FSI->moveBefore(BB->begin());

    // The instruction we are lowering has an argument which is different
    // for each predecessor.  We need to sink the instruction, then add
    // arguments for each predecessor.
    SILValue(BB->getBBArg(ArgNum)).replaceAllUsesWith(FSI);

    const auto &ArgType = FSI->getOperand(DifferentOperandIndex).getType();
    BB->replaceBBArg(ArgNum, ArgType);

    // Update all branch instructions in the predecessors to pass the new
    // argument to this BB.
    auto CloneIt = Clones.begin();
    for (auto P : BB->getPreds()) {
      // Only handle branch or conditional branch instructions.
      TermInst *TI = P->getTerminator();
      assert((isa<BranchInst>(TI) || isa<CondBranchInst>(TI)) &&
             "Branch instruction required");

      SILInstruction *CloneInst = dyn_cast<SILInstruction>(*CloneIt);
      TI->setOperand(ArgNum, CloneInst->getOperand(DifferentOperandIndex));
      // Now delete the clone as we only needed it operand.
      if (CloneInst != FSI)
        recursivelyDeleteTriviallyDeadInstructions(CloneInst);
      ++CloneIt;
    }
    assert(CloneIt == Clones.end() && "Clone/pred mismatch");

    // The sunk instruction should now read from the argument of the BB it
    // was moved to.
    FSI->setOperand(DifferentOperandIndex, BB->getBBArg(ArgNum));
    return true;
  }

  // Sink one of the copies of the instruction.
  FirstPredArg.replaceAllUsesWith(Undef);
  FSI->moveBefore(BB->begin());
  SILValue(BB->getBBArg(ArgNum)).replaceAllUsesWith(FirstPredArg);

  // The argument is no longer in use. Replace all incoming inputs with undef
  // and try to delete the instruction.
  for (auto S : Clones)
    if (S.getDef() != FSI) {
      S.replaceAllUsesWith(Undef);
      auto DeadArgInst = cast<SILInstruction>(S.getDef());
      recursivelyDeleteTriviallyDeadInstructions(DeadArgInst);
    }

  return true;
}

/// Try to sink identical arguments coming from multiple predecessors.
static bool sinkArgumentsFromPredecessors(SILBasicBlock *BB) {
  if (BB->pred_empty() || BB->getSinglePredecessor())
    return false;

  // This block must be the only successor of all the predecessors.
  for (auto P : BB->getPreds())
    if (P->getSingleSuccessor() != BB)
      return false;

  // Try to sink values from each of the arguments to the basic block.
  bool Changed = false;
  for (int i = 0, e = BB->getNumBBArg(); i < e; ++i)
    Changed |= sinkArgument(BB, i);

  return Changed;
}

static bool sinkCodeFromPredecessors(SILBasicBlock *BB) {
  bool Changed = false;
  if (BB->pred_empty())
    return Changed;

  // This block must be the only successor of all the predecessors.
  for (auto P : BB->getPreds())
    if (P->getSingleSuccessor() != BB)
      return Changed;

  SILBasicBlock *FirstPred = *BB->pred_begin();
  // The first Pred must have at least one non-terminator.
  if (FirstPred->getTerminator() == FirstPred->begin())
    return Changed;

  DEBUG(llvm::dbgs() << " Sinking values from predecessors.\n");

  unsigned SkipBudget = SinkSearchWindow;

  // Start scanning backwards from the terminator.
  SILBasicBlock::iterator InstToSink = FirstPred->getTerminator();

  while (SkipBudget) {
    DEBUG(llvm::dbgs() << "Processing: " << *InstToSink);

    // Save the duplicated instructions in case we need to remove them.
    SmallVector<SILInstruction *, 4> Dups;

    if (canSinkInstruction(InstToSink)) {
      // For all preds:
      for (auto P : BB->getPreds()) {
        if (P == FirstPred)
          continue;

        // Search the duplicated instruction in the predecessor.
        if (SILInstruction *DupInst = findIdenticalInBlock(P, InstToSink)) {
          Dups.push_back(DupInst);
        } else {
          DEBUG(llvm::dbgs() << "Instruction mismatch.\n");
          Dups.clear();
          break;
        }
      }

      // If we found duplicated instructions, sink one of the copies and delete
      // the rest.
      if (Dups.size()) {
        DEBUG(llvm::dbgs() << "Moving: " << *InstToSink);
        InstToSink->moveBefore(BB->begin());
        Changed = true;
        for (auto I : Dups) {
          I->replaceAllUsesWith(InstToSink);
          I->eraseFromParent();
          NumSunk++;
        }

        // Restart the scan.
        InstToSink = FirstPred->getTerminator();
        DEBUG(llvm::dbgs() << "Restarting scan. Next inst: " << *InstToSink);
        continue;
      }
    }

    // If this instruction was a barrier then we can't sink anything else.
    if (isSinkBarrier(InstToSink)) {
      DEBUG(llvm::dbgs() << "Aborting on barrier: " << *InstToSink);
      return Changed;
    }

    // This is the first instruction, we are done.
    if (InstToSink == FirstPred->begin()) {
      DEBUG(llvm::dbgs() << "Reached the first instruction.");
      return Changed;
    }

    SkipBudget--;
    InstToSink = std::prev(InstToSink);
    DEBUG(llvm::dbgs() << "Continuing scan. Next inst: " << *InstToSink);
  }

  return Changed;
}

static void createRefCountOpForPayload(SILBuilder &Builder, SILInstruction *I,
                                       EnumElementDecl *EnumDecl) {
  // If we do not have any argument, there is nothing to do... bail...
  if (!EnumDecl->hasArgumentType())
    return;

  // Otherwise, create a UEDI for our payload.
  SILModule &Mod = I->getModule();
  SILType ArgType =
      I->getOperand(0).getType().getEnumElementType(EnumDecl, Mod);
  auto *UEDI = Builder.createUncheckedEnumData(I->getLoc(), I->getOperand(0),
                                               EnumDecl, ArgType);
  if (isa<RetainValueInst>(I)) {
    Builder.createRetainValue(I->getLoc(), UEDI);
    return;
  }

  Builder.createReleaseValue(I->getLoc(), UEDI);
}

/// Sink retain_value, release_value before switch_enum to be retain_value,
/// release_value on the payload of the switch_enum in the destination BBs. We
/// only do this if the destination BBs have only the switch enum as its
/// predecessor.
static bool tryToSinkRefCountAcrossSwitch(SwitchEnumInst *S, SILInstruction *I,
                                  AliasAnalysis *AA) {
  // If this instruction is not a retain_value, there is nothing left for us to
  // do... bail...
  if (!isa<RetainValueInst>(I))
    return false;

  SILValue Ptr = I->getOperand(0);

  // If the retain value's argument is not the switch's argument, we can't do
  // anything with our simplistic analysis... bail...
  if (Ptr != S->getOperand())
    return false;

  // If S has a default case bail since the default case could represent
  // multiple cases.
  //
  // TODO: I am currently just disabling this behavior so we can get this out
  // for Seed 5. After Seed 5, we should be able to recognize if a switch_enum
  // handles all cases except for 1 and has a default case. We might be able to
  // stick code into SILBuilder that has this behavior.
  if (S->hasDefault())
    return false;

  // Next go over all instructions after I in the basic block. If none of them
  // can decrement our ptr value, we can move the retain over the ref count
  // inst. If any of them do potentially decrement the ref count of Ptr, we can
  // not move it.
  SILBasicBlock::iterator II = I;
  if (valueHasARCDecrementsInInstructionRange(Ptr, std::next(II),
                                              SILBasicBlock::iterator(S),
                                              AA))
    return false;

  SILBuilder Builder(S);

  // Ok, we have a ref count instruction, sink it!
  for (unsigned i = 0, e = S->getNumCases(); i != e; ++i) {
    auto Case = S->getCase(i);
    EnumElementDecl *Enum = Case.first;
    SILBasicBlock *Succ = Case.second;
    Builder.setInsertionPoint(&*Succ->begin());
    createRefCountOpForPayload(Builder, I, Enum);
  }

  I->eraseFromParent();
  NumSunk++;
  return true;
}

/// Sink retain_value, release_value before enum_is_tag to be retain_value,
/// release_value on the payload of the switch_enum in the destination BBs. We
/// only do this if the destination BBs have only the switch enum as its
/// predecessor.
static bool tryToSinkRefCountAcrossEnumIsTag(CondBranchInst *CondBr,
                                             SILInstruction *I,
                                             AliasAnalysis *AA) {
  // If this instruction is not a retain_value, there is nothing left for us to
  // do... bail...
  if (!isa<RetainValueInst>(I))
    return false;

  SILValue Ptr = I->getOperand(0);

  // Make sure the condition comes from an enum_is_tag


  EnumIsTagInst *EITI = dyn_cast<EnumIsTagInst>(CondBr->getCondition());
  if (!EITI)
    return false;

  // Its not clear whether its safe to do the
  // valueHasARCDecrementsInInstructionRange analysis when the enum_is_tag is
  // in another BB.  For now only do this when its the same BB as the cond_br.
  if (EITI->getParent() != CondBr->getParent())
    return false;

  // If the retain value's argument is not the switch's argument, we can't do
  // anything with our simplistic analysis... bail...
  if (Ptr != EITI->getOperand())
    return false;

  // Next go over all instructions after I in the basic block. If none of them
  // can decrement our ptr value, we can move the retain over the ref count
  // inst. If any of them do potentially decrement the ref count of Ptr, we can
  // not move it.
  SILBasicBlock::iterator II = I;
  if (valueHasARCDecrementsInInstructionRange(Ptr, std::next(II),
                                              SILBasicBlock::iterator(EITI),
                                              AA))
    return false;

  // Work out which enum element is the true branch, and which is false.
  // If the enum only has 2 values and its tag isn't the true branch, then we
  // know the true branch must be the other tag.
  EnumElementDecl *Elts[2] = { EITI->getElement(), nullptr };
  const auto &Operand = EITI->getOperand();
  if (EnumDecl *E = Operand.getType().getEnumOrBoundGenericEnum()) {
    // Look for a single other element on this enum.
    EnumElementDecl *OtherElt = nullptr;
    for (EnumElementDecl *Elt : E->getAllElements()) {
      // Skip the case where we find the enum_is_tag element
      if (Elt == EITI->getElement())
        continue;
      // If we find another element, then we must have more than 2, so bail.
      if (OtherElt)
        return false;
      OtherElt = Elt;
    }
    // Only a single enum element?  How would this even get here?  We should
    // handle it in SILCombine.
    if (!OtherElt)
      return false;
    Elts[1] = OtherElt;
  } else
    return false;

  SILBuilder Builder(EITI);

  // Ok, we have a ref count instruction, sink it!
  for (unsigned i = 0; i != 2; ++i) {
    EnumElementDecl *Enum = Elts[i];
    SILBasicBlock *Succ = i == 0 ? CondBr->getTrueBB() : CondBr->getFalseBB();
    Builder.setInsertionPoint(&*Succ->begin());
    createRefCountOpForPayload(Builder, I, Enum);
  }

  I->eraseFromParent();
  NumSunk++;
  return true;
}

static bool tryToSinkRefCountInst(SILInstruction *T, SILInstruction *I,
                                  AliasAnalysis *AA) {
  if (auto *S = dyn_cast<SwitchEnumInst>(T))
    return tryToSinkRefCountAcrossSwitch(S, I, AA);
  if (auto *CondBr = dyn_cast<CondBranchInst>(T))
    if (tryToSinkRefCountAcrossEnumIsTag(CondBr, I, AA))
      return true;

  // We currently handle checked_cast_br and cond_br.
  if (!isa<CheckedCastBranchInst>(T) && !isa<CondBranchInst>(T))
    return false;

  if (!isa<StrongRetainInst>(I) && !isa<RetainValueInst>(I))
    return false;

  SILValue Ptr = I->getOperand(0);
  SILBasicBlock::iterator II = I;
  ++II;
  for (; &*II != T; ++II) {
    if (canDecrementRefCount(&*II, Ptr, AA))
      return false;
  }

  SILBuilder Builder(T);

  // Ok, we have a ref count instruction, sink it!
  for (auto &Succ : T->getParent()->getSuccs()) {
    SILBasicBlock *SuccBB = Succ.getBB();
    Builder.setInsertionPoint(&*SuccBB->begin());
    if (isa<StrongRetainInst>(I))
      Builder.createStrongRetain(I->getLoc(), Ptr);
    else
      // I should be RetainValueInst.
      Builder.createRetainValue(I->getLoc(), Ptr);
  }

  I->eraseFromParent();
  NumSunk++;
  return true;
}

/// Sink retains to successors if possible. We only do this if the successors
/// have only one predecessor.
static bool sinkRetainToSuccessors(SILBasicBlock *BB, AliasAnalysis *AA) {
  SILInstruction *S = BB->getTerminator();

  // Make sure that each one of our successors only has one predecessor,
  // us. If that condition is not true, bail...
  for (auto &Succ : BB->getSuccs()) {
    SILBasicBlock *SuccBB = Succ.getBB();
    if (!SuccBB || !SuccBB->getSinglePredecessor())
      return false;
  }

  // Ok, we can perform this transformation... We bail immediately if we do not
  // have
  bool Changed = false;
  SILBuilder Builder(BB);

  SILBasicBlock::iterator SI = S, SE = BB->begin();
  if (SI == SE)
    return false;

  SI = std::prev(SI);

  while (SI != SE) {
    SILInstruction *Inst = &*SI;
    SI = std::prev(SI);
    if (tryToSinkRefCountInst(S, Inst, AA))
      Changed = true;
  }

  return Changed | tryToSinkRefCountInst(S, &*SI, AA);
}

namespace {
class SILCodeMotion : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() {
    SILFunction &F = *getFunction();
    AliasAnalysis *AA = getAnalysis<AliasAnalysis>();

    DEBUG(llvm::dbgs() << "***** CodeMotion on function: " << F.getName() <<
          " *****\n");

    // Sink duplicated code from predecessors.
    bool Changed = false;
    for (auto &BB : F) {
      Changed |= sinkCodeFromPredecessors(&BB);
      Changed |= sinkArgumentsFromPredecessors(&BB);
      Changed |= sinkRetainToSuccessors(&BB, AA);
    }

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Code Motion"; }
};
} // end anonymous namespace

SILTransform *swift::createCodeMotion() {
  return new SILCodeMotion();
}
