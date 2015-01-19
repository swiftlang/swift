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

#define DEBUG_TYPE "sil-codemotion"
#include "swift/SILPasses/Passes.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/Projection.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/RecyclingAllocator.h"

STATISTIC(NumSunk,   "Number of instructions sunk");
STATISTIC(NumRefCountOpsSimplified, "number of enum ref count ops simplified.");
STATISTIC(NumHoisted, "Number of instructions hoisted");

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static void createRefCountOpForPayload(SILBuilder &Builder, SILInstruction *I,
                                       EnumElementDecl *EnumDecl,
                                       SILValue DefOfEnum = SILValue()) {
  assert(EnumDecl->hasArgumentType() &&
         "We assume enumdecl has an argument type");

  SILModule &Mod = I->getModule();

  // The enum value is either passed as an extra argument if we are moving an
  // retain that does not refer to the enum typed value - otherwise it is the
  // argument to the refcount instruction.
  SILValue EnumVal = DefOfEnum ? DefOfEnum : I->getOperand(0);

  SILType ArgType = EnumVal.getType().getEnumElementType(EnumDecl, Mod);

  auto *UEDI =
    Builder.createUncheckedEnumData(I->getLoc(), EnumVal, EnumDecl, ArgType);

  SILType UEDITy = UEDI->getType();

  // If our payload is trivial, we do not need to insert any retain or release
  // operations.
  if (UEDITy.isTrivial(Mod))
    return;

  ++NumRefCountOpsSimplified;

  // If we have a retain value...
  if (isa<RetainValueInst>(I)) {
    // And our payload has reference semantics, insert a strong_retain onto the
    // payload.
    if (UEDITy.hasReferenceSemantics()) {
      Builder.createStrongRetain(I->getLoc(), UEDI);
      return;
    }

    // Otherwise, insert a retain_value on the payload.
    Builder.createRetainValue(I->getLoc(), UEDI);
    return;
  }

  // At this point we know that we must have a release_value and a non-trivial
  // payload.
  assert(isa<ReleaseValueInst>(I) && "If I is not a retain value here, it must "
         "be a release value since enums do not have reference semantics.");

  // If our payload has reference semantics, insert the strong release.
  if (UEDITy.hasReferenceSemantics()) {
    Builder.createStrongRelease(I->getLoc(), UEDI);
    return;
  }

  // Otherwise if our payload is non-trivial but lacking reference semantics,
  // insert the release_value.
  Builder.createReleaseValue(I->getLoc(), UEDI);
}

//===----------------------------------------------------------------------===//
//                            Generic Sinking Code
//===----------------------------------------------------------------------===//

static const int SinkSearchWindow = 6;

/// \brief Returns True if we can sink this instruction to another basic block.
static bool canSinkInstruction(SILInstruction *Inst) {
  return Inst->use_empty() && !isa<TermInst>(Inst);
}

/// \brief Returns true if this instruction is a skip barrier, which means that
/// we can't sink other instructions past it.
static bool isSinkBarrier(SILInstruction *Inst) {
  // We know that some calls do not have side effects.
  if (auto *BI = dyn_cast<BuiltinInst>(Inst))
    return !isSideEffectFree(BI);

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

/// Sink retain_value, release_value before switch_enum to be retain_value,
/// release_value on the payload of the switch_enum in the destination BBs. We
/// only do this if the destination BBs have only the switch enum as its
/// predecessor.
static bool tryToSinkRefCountAcrossSwitch(SwitchEnumInst *Switch,
                                          SILBasicBlock::iterator RV,
                                          AliasAnalysis *AA,
                                          RCIdentityAnalysis *RCIA) {
  // If this instruction is not a retain_value, there is nothing left for us to
  // do... bail...
  if (!isa<RetainValueInst>(RV))
    return false;

  SILValue Ptr = RV->getOperand(0);

  // Next go over all instructions after I in the basic block. If none of them
  // can decrement our ptr value, we can move the retain over the ref count
  // inst. If any of them do potentially decrement the ref count of Ptr, we can
  // not move it.
  SILBasicBlock::iterator SwitchIter = Switch;
  if (auto B = valueHasARCDecrementOrCheckInInstructionRange(Ptr, RV,
                                                             SwitchIter,
                                                             AA)) {
    RV->moveBefore(*B);
    return true;
  }

  // If the retain value's argument is not the switch's argument, we can't do
  // anything with our simplistic analysis... bail...
  if (RCIA->getRCIdentityRoot(Ptr) !=
      RCIA->getRCIdentityRoot(Switch->getOperand()))
    return false;

  // If S has a default case bail since the default case could represent
  // multiple cases.
  //
  // TODO: I am currently just disabling this behavior so we can get this out
  // for Seed 5. After Seed 5, we should be able to recognize if a switch_enum
  // handles all cases except for 1 and has a default case. We might be able to
  // stick code into SILBuilder that has this behavior.
  if (Switch->hasDefault())
    return false;

  // Ok, we have a ref count instruction, sink it!
  SILBuilderWithScope<> Builder(Switch, RV->getDebugScope());
  for (unsigned i = 0, e = Switch->getNumCases(); i != e; ++i) {
    auto Case = Switch->getCase(i);
    EnumElementDecl *Enum = Case.first;
    SILBasicBlock *Succ = Case.second;
    Builder.setInsertionPoint(&*Succ->begin());
    if (Enum->hasArgumentType())
      createRefCountOpForPayload(Builder, RV, Enum, Switch->getOperand());
  }

  RV->eraseFromParent();
  NumSunk++;
  return true;
}

/// Sink retain_value, release_value before select_enum to be retain_value,
/// release_value on the payload of the switch_enum in the destination BBs. We
/// only do this if the destination BBs have only the switch enum as its
/// predecessor.
static bool tryToSinkRefCountAcrossSelectEnum(CondBranchInst *CondBr,
                                              SILBasicBlock::iterator I,
                                              AliasAnalysis *AA,
                                              RCIdentityAnalysis *RCIA) {
  // If this instruction is not a retain_value, there is nothing left for us to
  // do... bail...
  if (!isa<RetainValueInst>(I))
    return false;

  // Make sure the condition comes from a select_enum
  auto *SEI = dyn_cast<SelectEnumInst>(CondBr->getCondition());
  if (!SEI)
    return false;

  // Try to find a single literal "true" case.
  // TODO: More general conditions in which we can relate the BB to a single
  // case, such as when there's a single literal "false" case.
  EnumElementDecl *TrueElement = SEI->getSingleTrueElement();
  if (!TrueElement)
    return false;
  
  // Next go over all instructions after I in the basic block. If none of them
  // can decrement our ptr value, we can move the retain over the ref count
  // inst. If any of them do potentially decrement the ref count of Ptr, we can
  // not move it.

  SILValue Ptr = I->getOperand(0);
  SILBasicBlock::iterator CondBrIter = CondBr;
  if (auto B = valueHasARCDecrementOrCheckInInstructionRange(Ptr, std::next(I),
                                                             CondBrIter, AA)) {
    I->moveBefore(*B);
    return false;
  }

  // If the retain value's argument is not the cond_br's argument, we can't do
  // anything with our simplistic analysis... bail...
  if (RCIA->getRCIdentityRoot(Ptr) !=
      RCIA->getRCIdentityRoot(SEI->getEnumOperand()))
    return false;
  
  // Work out which enum element is the true branch, and which is false.
  // If the enum only has 2 values and its tag isn't the true branch, then we
  // know the true branch must be the other tag.
  EnumElementDecl *Elts[2] = {TrueElement, nullptr};
  EnumDecl *E = SEI->getEnumOperand().getType().getEnumOrBoundGenericEnum();
  if (!E)
    return false;

  // Look for a single other element on this enum.
  EnumElementDecl *OtherElt = nullptr;
  for (EnumElementDecl *Elt : E->getAllElements()) {
    // Skip the case where we find the select_enum element
    if (Elt == TrueElement)
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

  SILBuilderWithScope<> Builder(SEI, I->getDebugScope());

  // Ok, we have a ref count instruction, sink it!
  for (unsigned i = 0; i != 2; ++i) {
    EnumElementDecl *Enum = Elts[i];
    SILBasicBlock *Succ = i == 0 ? CondBr->getTrueBB() : CondBr->getFalseBB();
    Builder.setInsertionPoint(&*Succ->begin());
    if (Enum->hasArgumentType())
      createRefCountOpForPayload(Builder, I, Enum, SEI->getEnumOperand());
  }

  I->eraseFromParent();
  NumSunk++;
  return true;
}

static bool tryToSinkRefCountInst(SILBasicBlock::iterator T,
                                  SILBasicBlock::iterator I,
                                  bool CanSinkToSuccessors,
                                  AliasAnalysis *AA,
                                  RCIdentityAnalysis *RCIA) {
  // The following methods should only be attempted if we can sink to our
  // successor.
  if (CanSinkToSuccessors) {
    // If we have a switch, try to sink ref counts across it and then return
    // that result. We do not keep processing since the code below can not
    // properly sink ref counts over switch_enums so we might as well exit
    // early.
    if (auto *S = dyn_cast<SwitchEnumInst>(T))
      return tryToSinkRefCountAcrossSwitch(S, I, AA, RCIA);

    // In contrast, even if we do not sink ref counts across a cond_br from a
    // select_enum, we may be able to sink anyways. So we do not return on a
    // failure case.
    if (auto *CondBr = dyn_cast<CondBranchInst>(T))
      if (tryToSinkRefCountAcrossSelectEnum(CondBr, I, AA, RCIA))
        return true;
  }

  if (!isa<StrongRetainInst>(I) && !isa<RetainValueInst>(I))
    return false;

  SILValue Ptr = I->getOperand(0);
  if (auto B = valueHasARCDecrementOrCheckInInstructionRange(Ptr, std::next(I),
                                                             T, AA)) {
    DEBUG(llvm::dbgs() << "    Moving " << *I);
    I->moveBefore(*B);
    return true;
  }

  // Ok, we have a ref count instruction that *could* be sunk. If we have a
  // terminator that we can not sink through or the cfg will not let us sink
  // into our predecessors, just move the increment before the terminator.
  if (!CanSinkToSuccessors ||
      (!isa<CheckedCastBranchInst>(T) && !isa<CondBranchInst>(T))) {
    DEBUG(llvm::dbgs() << "    Moving " << *I);
    I->moveBefore(T);
    return true;
  }

  // Ok, it is legal for us to sink this increment to our successors. Create a
  // copy of this instruction in each one of our successors unless they are
  // ignoreable trap blocks.
  DEBUG(llvm::dbgs() << "    Sinking " << *I);
  SILBuilderWithScope<> Builder(T, I->getDebugScope());
  for (auto &Succ : T->getParent()->getSuccs()) {
    SILBasicBlock *SuccBB = Succ.getBB();

    if (isARCInertTrapBB(SuccBB))
      continue;

    Builder.setInsertionPoint(&*SuccBB->begin());
    if (isa<StrongRetainInst>(I)) {
      Builder.createStrongRetain(I->getLoc(), Ptr);
    } else {
      assert(isa<RetainValueInst>(I) && "This can only be retain_value");
      Builder.createRetainValue(I->getLoc(), Ptr);
    }
  }

  // Then erase this instruction.
  I->eraseFromParent();
  NumSunk++;
  return true;
}

/// Try sink a retain as far as possible.  This is either to sucessor BBs,
/// or as far down the current BB as possible
static bool sinkRefCountIncrement(SILBasicBlock *BB, AliasAnalysis *AA,
                                  RCIdentityAnalysis *RCIA) {

  // Make sure that each one of our successors only has one predecessor,
  // us.
  // If that condition is not true, we can still sink to the end of this BB,
  // but not to successors.
  bool CanSinkToSuccessor = std::none_of(BB->succ_begin(), BB->succ_end(),
    [](const SILSuccessor &S) -> bool {
      SILBasicBlock *SuccBB = S.getBB();
      return !SuccBB || !SuccBB->getSinglePredecessor();
  });

  SILInstruction *S = BB->getTerminator();
  SILBasicBlock::iterator SI = S, SE = BB->begin();
  if (SI == SE)
    return false;

  bool Changed = false;

  // Walk from the terminator up the BB.  Try move retains either to the next
  // BB, or the end of this BB.  Note that ordering is maintained of retains
  // within this BB.
  SI = std::prev(SI);
  while (SI != SE) {
    SILInstruction *Inst = &*SI;
    SI = std::prev(SI);

    // Try to:
    //
    //   1. If there are no decrements between our ref count inst and
    //      terminator, sink the ref count inst into either our successors.
    //   2. If there are such decrements, move the retain right before that
    //      decrement.
    Changed |= tryToSinkRefCountInst(S, Inst, CanSinkToSuccessor, AA, RCIA);
  }

  // Handle the first instruction in the BB.
  Changed |= tryToSinkRefCountInst(S, &*SI, CanSinkToSuccessor, AA, RCIA);
  return Changed;
}

//===----------------------------------------------------------------------===//
//                             Enum Tag Dataflow
//===----------------------------------------------------------------------===//

namespace {

class BBToDataflowStateMap;

using EnumBBCaseList = llvm::SmallVector<std::pair<SILBasicBlock *,
                                                   EnumElementDecl *>, 2>;

/// Class that performs enum tag state dataflow on the given BB.
class BBEnumTagDataflowState
    : public SILInstructionVisitor<BBEnumTagDataflowState, bool> {
  NullablePtr<SILBasicBlock> BB;

  using ValueToCaseSmallBlotMapVectorTy =
    BlotMapVector<SILValue, EnumElementDecl *,
                  llvm::SmallDenseMap<SILValue, unsigned>,
                  llvm::SmallVector<std::pair<SILValue,
                                              EnumElementDecl *>, 4>>;
  ValueToCaseSmallBlotMapVectorTy ValueToCaseMap;

  using EnumToEnumBBCaseListMapTy =
    BlotMapVector<SILValue, EnumBBCaseList,
                  llvm::SmallDenseMap<SILValue, unsigned>,
                  llvm::SmallVector<std::pair<SILValue, EnumBBCaseList>, 4>>;

  EnumToEnumBBCaseListMapTy EnumToEnumBBCaseListMap;

public:
  BBEnumTagDataflowState() = default;
  BBEnumTagDataflowState(const BBEnumTagDataflowState &Other) = default;
  ~BBEnumTagDataflowState() = default;

  bool init(SILBasicBlock *NewBB) {
    assert(NewBB && "NewBB should not be null");
    BB = NewBB;
    return true;
  }

  SILBasicBlock *getBB() { return BB.get(); }

  using iterator = decltype(ValueToCaseMap)::iterator;
  iterator begin() { return ValueToCaseMap.getItems().begin(); }
  iterator end() { return ValueToCaseMap.getItems().begin(); }
  Range<iterator> currentTrackedState() {
    return ValueToCaseMap.getItems();
  }

  void clear() { ValueToCaseMap.clear(); }

  bool visitValueBase(ValueBase *V) { return false; }

  bool visitEnumInst(EnumInst *EI) {
    DEBUG(llvm::dbgs() << "    Storing enum into map: " << *EI);
    ValueToCaseMap[SILValue(EI)] = EI->getElement();
    return false;
  }

  bool visitUncheckedEnumDataInst(UncheckedEnumDataInst *UEDI) {
    DEBUG(
        llvm::dbgs() << "    Storing unchecked enum data into map: " << *UEDI);
    ValueToCaseMap[SILValue(UEDI->getOperand())] = UEDI->getElement();
    return false;
  }

  bool visitRetainValueInst(RetainValueInst *RVI);
  bool visitReleaseValueInst(ReleaseValueInst *RVI);
  bool process();
  bool hoistDecrementsIntoSwitchRegions(AliasAnalysis *AA);
  bool sinkIncrementsOutOfSwitchRegions(AliasAnalysis *AA,
                                        RCIdentityAnalysis *RCIA);
  void handlePredSwitchEnum(SwitchEnumInst *S);
  void handlePredCondSelectEnum(CondBranchInst *CondBr);

  /// Helper method which initializes this state map with the data from the
  /// first predecessor BB.
  ///
  /// We will be performing an intersection in a later step of the merging.
  bool initWithFirstPred(BBToDataflowStateMap &BBToStateMap,
                         SILBasicBlock *FirstPredBB);

  /// Top level merging function for predecessors.
  void mergePredecessorStates(BBToDataflowStateMap &BBToStateMap);

  /// 
  void mergeSinglePredTermInfoIntoState(BBToDataflowStateMap &BBToStateMap,
                                        SILBasicBlock *Pred);

};

/// Map all blocks to BBEnumTagDataflowState in RPO order.
class BBToDataflowStateMap {
  llvm::DenseMap<SILBasicBlock *, unsigned> BBToRPOMap;
  std::vector<BBEnumTagDataflowState> BBToStateVec;
public:
  BBToDataflowStateMap(PostOrderAnalysis *POTA, SILFunction *F) {
    auto ReversePostOrder = POTA->getReversePostOrder(F);
    int PostOrderSize = std::distance(ReversePostOrder.begin(),
                                      ReversePostOrder.end());
    BBToStateVec.resize(PostOrderSize);
    unsigned RPOIdx = 0;
    for (SILBasicBlock *BB : ReversePostOrder) {
      BBToStateVec[RPOIdx].init(BB);
      BBToRPOMap[BB] = RPOIdx;
      ++RPOIdx;
    }
  }
  unsigned size() const {
    return BBToStateVec.size();
  }
  BBEnumTagDataflowState &getRPOState(unsigned RPOIdx) {
    return BBToStateVec[RPOIdx];
  }
  /// \return BBEnumTagDataflowState or NULL for unreachable blocks.
  BBEnumTagDataflowState *getBBState(SILBasicBlock *BB) {
    auto Iter = BBToRPOMap.find(BB);
    if (Iter == BBToRPOMap.end())
        return nullptr;
    return &getRPOState(Iter->second);
  }
};

} // end anonymous namespace

void BBEnumTagDataflowState::handlePredSwitchEnum(SwitchEnumInst *S) {

  // Find the tag associated with our BB and set the state of the
  // enum we switch on to that value. This is important so we can determine
  // covering switches for enums that have cases without payload.

  // Next check if we are the target of a default switch_enum case. If we are,
  // no interesting information can be extracted, so bail...
  if (S->hasDefault() && S->getDefaultBB() == getBB())
    return;

  // Otherwise, attempt to find the tag associated with this BB in the switch
  // enum...
  for (unsigned i = 0, e = S->getNumCases(); i != e; ++i) {
    auto P = S->getCase(i);

    // If this case of the switch is not matched up with this BB, skip the
    // case...
    if (P.second != getBB())
      continue;

    // Ok, we found the case for our BB. If we don't have an enum tag (which can
    // happen if we have a default statement), return. There is nothing more we
    // can do.
    if (!P.first)
      return;

    // Ok, we have a matching BB and a matching enum tag. Set the state and
    // return.
    ValueToCaseMap[S->getOperand()] = P.first;
    return;
  }
  llvm_unreachable("A successor of a switch_enum terminated BB should be in "
                   "the switch_enum.");
}

void BBEnumTagDataflowState::handlePredCondSelectEnum(CondBranchInst *CondBr) {

  SelectEnumInst *EITI = dyn_cast<SelectEnumInst>(CondBr->getCondition());
  if (!EITI)
    return;
  
  auto TrueElement = EITI->getSingleTrueElement();
  if (!TrueElement)
    return;

  // Find the tag associated with our BB and set the state of the
  // enum we switch on to that value. This is important so we can determine
  // covering switches for enums that have cases without payload.

  // Check if we are the true case, ie, we know that we are the given tag.
  const auto &Operand = EITI->getEnumOperand();
  if (CondBr->getTrueBB() == getBB()) {
    ValueToCaseMap[Operand] = TrueElement;
    return;
  }

  // If the enum only has 2 values and its tag isn't the true branch, then we
  // know the true branch must be the other tag.
  if (EnumDecl *E = Operand.getType().getEnumOrBoundGenericEnum()) {
    // Look for a single other element on this enum.
    EnumElementDecl *OtherElt = nullptr;
    for (EnumElementDecl *Elt : E->getAllElements()) {
      // Skip the case where we find the select_enum element
      if (Elt == TrueElement)
        continue;
      // If we find another element, then we must have more than 2, so bail.
      if (OtherElt)
        return;
      OtherElt = Elt;
    }
    // Only a single enum element?  How would this even get here?  We should
    // handle it in SILCombine.
    if (!OtherElt)
      return;
    // FIXME: Can we ever not be the false BB here?
    if (CondBr->getTrueBB() != getBB()) {
      ValueToCaseMap[Operand] = OtherElt;
      return;
    }
  }
}

bool
BBEnumTagDataflowState::
initWithFirstPred(BBToDataflowStateMap &BBToStateMap,
                  SILBasicBlock *FirstPredBB) {
  // Try to look up the state for the first pred BB.
  BBEnumTagDataflowState *FirstPredState = BBToStateMap.getBBState(FirstPredBB);

  // If we fail, we found an unreachable block, bail.
  if (FirstPredState == nullptr) {
    DEBUG(llvm::dbgs() << "        Found an unreachable block!\n");
    return false;
  }

  // Ok, our state is in the map, copy in the predecessors value to case map.
  ValueToCaseMap = FirstPredState->ValueToCaseMap;

  // If we are predecessors only successor, we can potentially hoist releases
  // into it, so associate the first pred BB and the case for each value that we
  // are tracking with it.
  //
  // TODO: I am writing this too fast. Clean this up later.
  if (FirstPredBB->getSingleSuccessor())
    for (auto P : ValueToCaseMap.getItems())
      EnumToEnumBBCaseListMap[P.first].push_back({FirstPredBB, P.second});

  return true;
}

void
BBEnumTagDataflowState::
mergeSinglePredTermInfoIntoState(BBToDataflowStateMap &BBToStateMap,
                                 SILBasicBlock *Pred) {
  // Grab the terminator of our one predecessor and if it is a switch enum, mix
  // it into this state.
  TermInst *PredTerm = Pred->getTerminator();
  if (auto *S = dyn_cast<SwitchEnumInst>(PredTerm)) {
    handlePredSwitchEnum(S);
    return;
  }

  auto *CondBr = dyn_cast<CondBranchInst>(PredTerm);
  if (!CondBr)
    return;

  handlePredCondSelectEnum(CondBr);
}

void
BBEnumTagDataflowState::
mergePredecessorStates(BBToDataflowStateMap &BBToStateMap) {

  // If we have no precessors, there is nothing to do so return early...
  if (getBB()->pred_empty()) {
    DEBUG(llvm::dbgs() << "            No Preds.\n");
    return;
  }

  auto PI = getBB()->pred_begin(), PE = getBB()->pred_end();
  if (*PI == getBB()) {
    DEBUG(llvm::dbgs() << "            Found a self loop. Bailing!\n");
    return;
  }

  // Grab the first predecessor BB.
  SILBasicBlock *FirstPred = *PI;
  ++PI;

  // Attempt to initialize our state with our first predecessor's state by just
  // copying. We will be doing an intersection with all of the other BB.
  if (!initWithFirstPred(BBToStateMap, FirstPred))
    return;

  // If we only have one predecessor see if we can gain any information and or
  // knowledge from the terminator of our one predecessor. There is nothing more
  // that we can do, return.
  //
  // This enables us to get enum information from switch_enum and cond_br about
  // the value that an enum can take in our block. This is a common case that
  // comes up.
  if (PI == PE) {
    mergeSinglePredTermInfoIntoState(BBToStateMap, FirstPred);
    return;
  }

  DEBUG(llvm::dbgs() << "            Merging in rest of predecessors...\n");

  // Enum values that while merging we found conflicting values for. We blot
  // them after the loop in order to ensure that we can still find the ends of
  // switch regions.
  llvm::SmallVector<SILValue, 4> CurBBValuesToBlot;

  // If we do not find state for a specific value in any of our predecessor BBs,
  // we can not be the end of a switch region since we can not cover our
  // predecessor BBs with enum decls. Blot after the loop.
  llvm::SmallVector<SILValue, 4> PredBBValuesToBlot;

  // And for each remaining predecessor...
  do {
    // If we loop on ourselves, bail...
    if (*PI == getBB()) {
      DEBUG(llvm::dbgs() << "            Found a self loop. Bailing!\n");
      return;
    }

    // Grab the predecessors state...
    SILBasicBlock *PredBB = *PI;

    BBEnumTagDataflowState *PredBBState = BBToStateMap.getBBState(PredBB);
    if (PredBBState == nullptr) {
      DEBUG(llvm::dbgs() << "            Found an unreachable block!\n");
      return;
    }

    ++PI;

    // Then for each (SILValue, Enum Tag) that we are tracking...
    for (auto P : ValueToCaseMap.getItems()) {
      // If this SILValue was blotted, there is nothing left to do, we found
      // some sort of conflicting definition and are being conservative.
      if (!P.first)
        continue;

      // Then attempt to look up the enum state associated in our SILValue in
      // the predecessor we are processing.
      auto PredValue = PredBBState->ValueToCaseMap.find(P.first);

      // If we can not find the state associated with this SILValue in this
      // predecessor or the value in the corresponding predecessor was blotted,
      // we can not find a covering switch for this BB or forward any enum tag
      // information for this enum value.
      if (PredValue == PredBBState->ValueToCaseMap.end() || !PredValue->first) {
        // Otherwise, we are conservative and do not forward the EnumTag that we
        // are tracking. Blot it!
        DEBUG(llvm::dbgs() << "                Blotting: " << P.first);
        CurBBValuesToBlot.push_back(P.first);
        PredBBValuesToBlot.push_back(P.first);
        continue;
      }

      // Check if out predecessor has any other successors. If that is true we
      // clear all the state since we can not hoist safely.
      if (!PredBB->getSingleSuccessor()) {
        EnumToEnumBBCaseListMap.clear();
        DEBUG(llvm::dbgs() << "                Predecessor has other "
              "successors. Clearing BB cast list map.\n");
      } else {
        // Otherwise, add this case to our predecessor case list. We will unique
        // this after we have finished processing all predecessors.
        auto Case = std::make_pair(PredBB, PredValue->second);
        EnumToEnumBBCaseListMap[PredValue->first].push_back(Case);
      }

      // And the states match, the enum state propagates to this BB.
      if (PredValue->second == P.second)
        continue;

      // Otherwise, we are conservative and do not forward the EnumTag that we
      // are tracking. Blot it!
      DEBUG(llvm::dbgs() << "                Blotting: " << P.first);
      CurBBValuesToBlot.push_back(P.first);
    }
  } while (PI != PE);

  for (SILValue V : CurBBValuesToBlot) {
    ValueToCaseMap.blot(V);
  }
  for (SILValue V : PredBBValuesToBlot) {
    EnumToEnumBBCaseListMap.blot(V);
  }
}

bool BBEnumTagDataflowState::visitRetainValueInst(RetainValueInst *RVI) {
  auto FindResult = ValueToCaseMap.find(RVI->getOperand());
  if (FindResult == ValueToCaseMap.end())
    return false;

  // If we do not have any argument, kill the retain_value.
  if (!FindResult->second->hasArgumentType()) {
    RVI->eraseFromParent();
    return true;
  }

  DEBUG(llvm::dbgs() << "    Found RetainValue: " << *RVI);
  DEBUG(llvm::dbgs() << "        Paired to Enum Oracle: " << FindResult->first);

  SILBuilderWithScope<> Builder(RVI, RVI->getDebugScope());
  createRefCountOpForPayload(Builder, RVI, FindResult->second);
  RVI->eraseFromParent();
  return true;
}

bool BBEnumTagDataflowState::visitReleaseValueInst(ReleaseValueInst *RVI) {
  auto FindResult = ValueToCaseMap.find(RVI->getOperand());
  if (FindResult == ValueToCaseMap.end())
    return false;

  // If we do not have any argument, just delete the release value.
  if (!FindResult->second->hasArgumentType()) {
    RVI->eraseFromParent();
    return true;
  }

  DEBUG(llvm::dbgs() << "    Found ReleaseValue: " << *RVI);
  DEBUG(llvm::dbgs() << "        Paired to Enum Oracle: " << FindResult->first);

  SILBuilderWithScope<> Builder(RVI , RVI->getDebugScope());
  createRefCountOpForPayload(Builder, RVI, FindResult->second);
  RVI->eraseFromParent();
  return true;
}

bool BBEnumTagDataflowState::process() {
  bool Changed = false;

  auto SI = getBB()->begin();
  while (SI != getBB()->end()) {
    SILInstruction *I = &*SI;
    ++SI;
    Changed |= visit(I);
  }

  return Changed;
}

bool
BBEnumTagDataflowState::hoistDecrementsIntoSwitchRegions(AliasAnalysis *AA) {
  bool Changed = false;
  unsigned NumPreds = std::distance(getBB()->pred_begin(), getBB()->pred_end());

  for (auto II = getBB()->begin(), IE = getBB()->end(); II != IE;) {
    auto *RVI = dyn_cast<ReleaseValueInst>(&*II);
    ++II;

    // If this instruction is not a release, skip it...
    if (!RVI)
      continue;

    DEBUG(llvm::dbgs() << "        Visiting release: " << *RVI);

    // Grab the operand of the release value inst.
    SILValue Op = RVI->getOperand();

    // Lookup the [(BB, EnumTag)] list for this operand.
    auto R = EnumToEnumBBCaseListMap.find(Op);
    // If we don't have one, skip this release value inst.
    if (R == EnumToEnumBBCaseListMap.end()) {
      DEBUG(llvm::dbgs() << "            Could not find [(BB, EnumTag)] "
            "list for release_value's operand. Bailing!\n");
      continue;
    }

    auto &EnumBBCaseList = R->second;
    // If we don't have an enum tag for each predecessor of this BB, bail since
    // we do not know how to handle that BB.
    if (EnumBBCaseList.size() != NumPreds) {
      DEBUG(llvm::dbgs() << "            Found [(BB, EnumTag)] "
            "list for release_value's operand, but we do not have an enum tag "
            "for each predecessor. Bailing!\n");
      DEBUG(llvm::dbgs() << "            List:\n");
      DEBUG(for (auto P : EnumBBCaseList) {
          llvm::dbgs() << "                "; P.second->dump(llvm::dbgs());
        });
      continue;
    }

    // Finally ensure that we have no users of this operand preceding the
    // release_value in this BB. If we have users like that we can not hoist the
    // release past them unless we know that there is an additional set of
    // releases that together post-dominate this release. If we can not do this,
    // skip this release.
    //
    // TODO: We need information from the ARC optimizer to prove that property
    // if we are going to use it.
    if (valueHasARCUsesInInstructionRange(Op, getBB()->begin(),
                                          SILBasicBlock::iterator(RVI),
                                          AA)) {
      DEBUG(llvm::dbgs() << "            Release value has use that stops "
            "hoisting! Bailing!\n");
      continue;
    }

    DEBUG(llvm::dbgs() << "            Its safe to perform the "
          "transformation!\n");

    // Otherwise perform the transformation.
    for (auto P : EnumBBCaseList) {
      // If we don't have an argument for this case, there is nothing to
      // do... continue...
      if (!P.second->hasArgumentType())
        continue;

      // Otherwise create the release_value before the terminator of the
      // predecessor.
      assert(P.first->getSingleSuccessor() &&
             "Can not hoist release into BB that has multiple successors");
      SILBuilderWithScope<> Builder(P.first->getTerminator(),
                                    RVI->getDebugScope());
      createRefCountOpForPayload(Builder, RVI, P.second);
    }

    RVI->eraseFromParent();
    ++NumHoisted;
    Changed = true;
  }

  return Changed;
}

static SILInstruction *
findLastSinkableMatchingEnumValueRCIncrementInPred(AliasAnalysis *AA,
                                                   RCIdentityAnalysis *RCIA,
                                                   SILValue EnumValue,
                                                   SILBasicBlock *BB) {
    // Otherwise, see if we can find a retain_value or strong_retain associated
    // with that enum in the relevant predecessor.
    auto FirstInc = std::find_if(BB->rbegin(), BB->rend(),
      [&RCIA, &EnumValue](const SILInstruction &I) -> bool {
      // If I is not an increment, ignore it.
      if (!isa<StrongRetainInst>(I) && !isa<RetainValueInst>(I))
        return false;

      // Otherwise, if the increments operand stripped of RC identity preserving
      // ops matches EnumValue, it is the first increment we are interested in.
      return EnumValue == RCIA->getRCIdentityRoot(I.getOperand(0));
    });

    // If we do not find a ref count increment in the relevant BB, skip this
    // enum since there is nothing we can do.
    if (FirstInc == BB->rend())
      return nullptr;

    // Otherwise, see if there are any instructions in between FirstPredInc and
    // the end of the given basic block that could decrement first pred. If such
    // an instruction exists, we can not perform this optimization so continue.
    if (valueHasARCDecrementOrCheckInInstructionRange(EnumValue, &*FirstInc,
                                                      BB->getTerminator(),
                                                      AA))
      return nullptr;

    return &*FirstInc;
}

static bool
findRetainsSinkableFromSwitchRegionForEnum(
  AliasAnalysis *AA, RCIdentityAnalysis *RCIA, SILValue EnumValue,
  EnumBBCaseList &Map, SmallVectorImpl<SILInstruction *> &DeleteList) {

  // For each predecessor with argument type...
  for (auto &P : Map) {
    SILBasicBlock *PredBB = P.first;
    EnumElementDecl *Decl = P.second;

    // If the case does not have an argument type, skip the predecessor since
    // there will not be a retain to sink.
    if (!Decl->hasArgumentType())
      continue;

    // Ok, we found a payloaded predecessor. Look backwards through the
    // predecessor for the first ref count increment on EnumValue. If there
    // are no ref count decrements in between the increment and the terminator
    // of the BB, then we can sink the retain out of the switch enum.
    auto *Inc = findLastSinkableMatchingEnumValueRCIncrementInPred(AA,
                                                                   RCIA,
                                                                   EnumValue,
                                                                   PredBB);
    // If we do not find such an increment, there is nothing we can do, bail.
    if (!Inc)
      return false;

    // Otherwise add the increment to the delete list.
    DeleteList.push_back(Inc);
  }

  // If we were able to process each predecessor successfully, return true.
  return true;
}

bool
BBEnumTagDataflowState::
sinkIncrementsOutOfSwitchRegions(AliasAnalysis *AA, RCIdentityAnalysis *RCIA) {
  bool Changed = false;
  unsigned NumPreds = std::distance(getBB()->pred_begin(), getBB()->pred_end());
  llvm::SmallVector<SILInstruction *, 4> DeleteList;

  // For each (EnumValue, [(BB, EnumTag)]) that we are tracking...
  for (auto &P : EnumToEnumBBCaseListMap) {
    // Clear our delete list.
    DeleteList.clear();

    // If EnumValue is null, we deleted this entry. There is nothing to do for
    // this value... Skip it.
    if (!P.first)
      continue;
    SILValue EnumValue = RCIA->getRCIdentityRoot(P.first);
    EnumBBCaseList &Map = P.second;

    // If we do not have a tag associated with this enum value for each
    // predecessor, we are not a switch region exit for this enum value. Skip
    // this value.
    if (Map.size() != NumPreds)
      continue;

    // Look through our predecessors for a set of ref count increments on our
    // enum value for every payloaded case that *could* be sunk. If we miss an
    // increment from any of the payloaded case there is nothing we can do here,
    // so skip this enum value.
    if (!findRetainsSinkableFromSwitchRegionForEnum(AA, RCIA, EnumValue, Map,
                                                    DeleteList))
      continue;

    // If we do not have any payload arguments, then we should have an empty
    // delete list and there is nothing to do here.
    if (DeleteList.empty())
      continue;

    // Ok, we can perform this transformation! Insert the new retain_value and
    // delete all of the ref count increments from the predecessor BBs.
    //
    // TODO: Which debug loc should we use here? Using one of the locs from the
    // delete list seems reasonable for now...
    SILBuilder(getBB()->begin()).createRetainValue(DeleteList[0]->getLoc(),
                                                   EnumValue);
    for (auto *I : DeleteList)
      I->eraseFromParent();
    ++NumSunk;
    Changed = true;
  }

  return Changed;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool processFunction(SILFunction *F, AliasAnalysis *AA,
                            PostOrderAnalysis *POTA,
                            RCIdentityAnalysis *RCIA,
                            bool HoistReleases) {

  bool Changed = false;

  BBToDataflowStateMap BBToStateMap(POTA, F);
  for (unsigned RPOIdx = 0, RPOEnd = BBToStateMap.size(); RPOIdx < RPOEnd;
       ++RPOIdx) {

    DEBUG(llvm::dbgs() << "Visiting BB RPO#" << RPOIdx << "\n");

    BBEnumTagDataflowState &State = BBToStateMap.getRPOState(RPOIdx);

    DEBUG(llvm::dbgs() << "    Predecessors (empty if no predecessors):\n");
    DEBUG(for (SILBasicBlock *Pred : State.getBB()->getPreds()) {
        llvm::dbgs() << "        BB#" << RPOIdx << "; Ptr: " << Pred << "\n";
    });
    DEBUG(llvm::dbgs() << "    State Addr: " << &State << "\n");

    // Merge in our predecessor states. We relook up our the states for our
    // predecessors to avoid memory invalidation issues due to copying in the
    // dense map.
    DEBUG(llvm::dbgs() << "    Merging predecessors!\n");
    State.mergePredecessorStates(BBToStateMap);

    // If our predecessors cover any of our enum values, attempt to hoist
    // releases up the CFG onto enum payloads or sink retains out of switch
    // regions.
    DEBUG(llvm::dbgs() << "    Attempting to move releases into "
          "predecessors!\n");

    if (HoistReleases)
      Changed |= State.hoistDecrementsIntoSwitchRegions(AA);

    Changed |= State.sinkIncrementsOutOfSwitchRegions(AA, RCIA);


    // Then attempt to sink code from predecessors. This can include retains
    // which is why we always attempt to move releases up the CFG before sinking
    // code from predecessors. We will never sink the hoisted releases from
    // predecessors since the hoisted releases will be on the enum payload
    // instead of the enum itself.
    Changed |= sinkCodeFromPredecessors(State.getBB());
    Changed |= sinkArgumentsFromPredecessors(State.getBB());

    // Then perform the dataflow.
    DEBUG(llvm::dbgs() << "    Performing the dataflow!\n");
    Changed |= State.process();

    // Finally we try to sink retain instructions from this BB to the next BB.
    Changed |= sinkRefCountIncrement(State.getBB(), AA, RCIA);
  }

  return Changed;
}

namespace {
class SILCodeMotion : public SILFunctionTransform {

  bool HoistReleases;

public:

  SILCodeMotion(bool TryReleaseHoisting) : HoistReleases(TryReleaseHoisting) {}

  /// The entry point to the transformation.
  void run() {
    auto *F = getFunction();
    auto *AA = getAnalysis<AliasAnalysis>();
    auto *POTA = getAnalysis<PostOrderAnalysis>();
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();

    DEBUG(llvm::dbgs() << "***** CodeMotion on function: " << F->getName() <<
          " *****\n");

    if (processFunction(F, AA, POTA, RCIA, HoistReleases))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Code Motion"; }
};
} // end anonymous namespace

SILTransform *swift::createCodeMotion(bool HoistReleases) {
  return new SILCodeMotion(HoistReleases);
}
