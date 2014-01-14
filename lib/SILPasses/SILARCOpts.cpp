//===-------- SILARCOpts.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "sil-arc-opts"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumStrongRetains, "Total number of strong_retain seen");
STATISTIC(NumStrongRetainStrongReleasePairsEliminated,
          "Number of strong_retain/strong_release pairs eliminated");
STATISTIC(NumCopyValue, "Total number of copy_value seen");
STATISTIC(NumCopyValueDestroyValuePairsEliminated,
          "Number of swift copy_value/destroy_value pairs eliminated");

//===----------------------------------------------------------------------===//
//                                 Exit Type
//===----------------------------------------------------------------------===//

namespace {

  /// Returned by local code motion visitors to specify the type of action to
  /// take after visiting an instruction.
  enum class ExitType {
    /// We successfully eliminated a +1/-1 pair. Exit successfully.
    RefCountPairEliminated,

    /// Exit, performing any possible code motion.
    PerformCodeMotion,

    /// Set if the two instructions commute. Initiates code motion.
    CommuteWithMotion,

    /// Set if the two instructions commute, but the visited structure can not
    /// cause code motion to be initiated. This is used to prevent infinite code
    /// motion by adjacent retains/releases.
    CommuteWithoutMotion,
  };

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                        Local Retain Motion Visitor
//===----------------------------------------------------------------------===//

namespace {

  /// A visitor that attempts to move retain/copy_value instructions down the
  /// CFG.
  ///
  /// Currently this visitor implements a state machine without any
  /// memory. After visiting an instruction, it specifies what action the caller
  /// should take in response to the newly visited instruction. These actions
  /// are:
  ///
  ///   1. RefCountPairEliminated. We visited a release/destroy_value that can
  ///   be paired with the retain/copy_value that we are tracking. We eliminated
  ///   the pair. The user should return exit with success.
  ///
  ///   2. PerformCodeMotion. We visited an instruction that we can not move a
  ///   retain/copy_value over. Thus do nothing in the case of a copy_value or
  ///   in the case of the retain, move the retain next to this usage.
  ///
  ///   3. CommuteWithMotion. We visited an instruction that we can commute the
  ///   retain/copy_value we are tracking with. Visit the next instruction.
  ///
  ///   4. CommuteWithoutMotion. We visited an instruction that we can commute
  ///   the retain/copy_value we are tracking with, but in order to prevent
  ///   circular code motion, only move the instruction past that instruction if
  ///   there is another instruction beyond it that enables us to commute with
  ///   motion.
  ///
  /// This will be replaced by the dataflow analysis in a later iteration.
  class LocalRetainMotionVisitor
    : public SILVisitor<LocalRetainMotionVisitor, ExitType> {

    /// The module the instruction we are processing is in.
    SILModule &M;

    /// The specific retain or copy_value that we are attempting to eliminate
    /// and or move down the cfg.
    SILInstruction &Inst;

  public:

    LocalRetainMotionVisitor(StrongRetainInst &Retain)
      : M(Retain.getModule()), Inst(Retain) { }

    LocalRetainMotionVisitor(CopyValueInst &CopyValue)
      : M(CopyValue.getModule()), Inst(CopyValue) { }

    ExitType visitValueBase(ValueBase *V) {
      llvm_unreachable("This should never be hit.");
    }

    /// Returns the strong_retain we are tracking or null if we are tracking a
    /// copy_value.
    StrongRetainInst *getRetain() { return dyn_cast<StrongRetainInst>(&Inst); }

    /// Returns the copy_value we are tracking or null if we are tracking a
    /// strong_release.
    CopyValueInst *getCopyValue() const {
      return dyn_cast<CopyValueInst>(&Inst);
    }

    /// Returns the instruction we are tracking.
    SILInstruction *getInstruction() const { return &Inst; }

    /// Currently MayHaveSideEffects has a broader definition than we
    /// require. The following x-macro handles simple cases of instructions that
    /// have side effects, but that we handle as if they do not since they do
    /// not decrement reference counts.
#define SIMPLE_CASE(_Value, _ExitType)                                \
  ExitType visit ## _Value(_Value *I) { return ExitType::_ExitType; }
    SIMPLE_CASE(DeallocStackInst, CommuteWithMotion)
    SIMPLE_CASE(PartialApplyInst, CommuteWithMotion)
    SIMPLE_CASE(CondFailInst, CommuteWithMotion)
    SIMPLE_CASE(UnownedRetainInst, CommuteWithMotion)
    SIMPLE_CASE(UnownedReleaseInst, CommuteWithMotion)
#undef SIMPLE_CASE

    ExitType visitSILInstruction(SILInstruction *I);
    ExitType visitStrongReleaseInst(StrongReleaseInst *Release);
    ExitType visitApplyInst(ApplyInst *AI);
    ExitType visitStrongRetainInst(StrongRetainInst *Retain);
    ExitType visitCopyValueInst(CopyValueInst *CV);
    ExitType visitDestroyValueInst(DestroyValueInst *DV);
    ExitType visitCopyAddrInst(CopyAddrInst *CA);
  };

} // end anonymous namespace

/// Skip over instructions that are guaranteed to not decrement ref
/// counts. In SIL, this is gauranteed by an instruction not having side
/// effects.
ExitType LocalRetainMotionVisitor::visitSILInstruction(SILInstruction *I) {
  // If the instruction does not have any side effects, the retain/copy value
  // and the instruction commute.
  if (I->getMemoryBehavior() !=
      SILInstruction::MemoryBehavior::MayHaveSideEffects)
    return ExitType::CommuteWithMotion;

  // Otherwise assume that something is occuring here we do not understand,
  // so bail, performing any code motion that is possible at this point.
  return ExitType::PerformCodeMotion;
}

ExitType
LocalRetainMotionVisitor::visitStrongRetainInst(StrongRetainInst *InputRetain) {
  // If we see a retain, skip over it since a retain can never cause a release
  // to occur.
  //
  // On the other hand, if we have a retain that is naively on the same pointer,
  // don't move past it since no "progress" has been made. If we remove any
  // pairs in this basic block, we will process it again to allow for this
  // retain to be removed as well.
  if (auto *Retain = getRetain())
    if (Retain->getOperand() == InputRetain->getOperand())
      return ExitType::PerformCodeMotion;
  return ExitType::CommuteWithoutMotion;
}

ExitType
LocalRetainMotionVisitor::visitCopyValueInst(CopyValueInst *InputCV) {
  // If we see a retain, skip over it since a retain can never cause a release
  // to occur.
  //
  // On the other hand, if we have a retain that is naively on the same pointer,
  // don't move past it since no "progress" has been made. If we remove any
  // pairs in this basic block, we will process it again to allow for this
  // retain to be removed as well.
  if (auto *CV = getCopyValue())
    if (CV->getOperand() == InputCV->getOperand())
      return ExitType::PerformCodeMotion;
  return ExitType::CommuteWithoutMotion;
}

ExitType
LocalRetainMotionVisitor::visitApplyInst(ApplyInst *AI) {
  // Ignore any thick functions for now due to us not handling the ref-counted
  // nature of its context.
  if (auto FTy = AI->getCallee().getType().getAs<SILFunctionType>())
    if (!FTy->isThin())
      return ExitType::PerformCodeMotion;

  // If we have a builtin that is side effect free, we can commute the
  // ApplyInst and the retain.
  if (auto *BI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee()))
    if (isSideEffectFree(BI)) {
      DEBUG(llvm::dbgs() << "Builtin Success: " << *BI << "\n");
      return ExitType::CommuteWithMotion;
    }

  // If our apply inst has no arguments, skip over it. Plug in alias analysis
  // here.
  if (AI->getNumOperands() == 1)
    return ExitType::CommuteWithMotion;

  // Otherwise exit, performing any code motion that we can.
  return ExitType::PerformCodeMotion;
}

ExitType
LocalRetainMotionVisitor::visitStrongReleaseInst(StrongReleaseInst *Release) {
  // If we are processing a copy_value, exit performing code motion since the
  // the release could trigger a destructor causing potentially unknown side
  // effects.
  //
  // Otherwise if we have a retain and the input release is naively on the same
  // pointer, eliminate the pair. Otherwise do nothing since the release could
  // affect our retain count indirectly via a destructor.
  StrongRetainInst *Retain = getRetain();
  if (!Retain || Retain->getOperand() != Release->getOperand())
    return ExitType::PerformCodeMotion;

  DEBUG(llvm::dbgs() << "    Eliminating Retain Release Pair!\n");
  DEBUG(llvm::dbgs() << "    Retain: " << *Retain);
  DEBUG(llvm::dbgs() << "    Release: " << *Release);
  Retain->eraseFromParent();
  Release->eraseFromParent();
  ++NumStrongRetainStrongReleasePairsEliminated;
  return ExitType::RefCountPairEliminated;
}

ExitType
LocalRetainMotionVisitor::visitDestroyValueInst(DestroyValueInst *DV) {
  // If we are processing a copy_value, exit performing code motion since the
  // the release could trigger a destructor causing potentially unknown side
  // effects.
  //
  // Otherwise if we have a retain and the input release is naively on the same
  // pointer, eliminate the pair. Otherwise do nothing since the release could
  // affect our retain count indirectly via a destructor.
  CopyValueInst* CV = getCopyValue();
  if (!CV || (CV->getOperand() != DV->getOperand() &&
              CV != DV->getOperand().getDef()))
    return ExitType::PerformCodeMotion;

  DEBUG(llvm::dbgs() << "    Eliminating Retain Release Pair!\n");
  DEBUG(llvm::dbgs() << "    Retain: " << *CV);
  DEBUG(llvm::dbgs() << "    Release: " << *DV);
  SILValue(CV).replaceAllUsesWith(CV->getOperand());
  CV->eraseFromParent();
  DV->eraseFromParent();
  ++NumCopyValueDestroyValuePairsEliminated;
  return ExitType::RefCountPairEliminated;
}

ExitType
LocalRetainMotionVisitor::visitCopyAddrInst(CopyAddrInst *CA) {
  // A copy address that is an initialization does not release the old value, so
  // it always commutes with retains.
  if (CA->isInitializationOfDest() == IsInitialization_t::IsInitialization)
    return ExitType::CommuteWithMotion;

  // Otherwise, the CopyAddr will release a pointer implying we must exit,
  // performing code motion if we can.
  return ExitType::PerformCodeMotion;
}

static bool performLocalCodeMotionDownCFG(LocalRetainMotionVisitor &V) {
  bool Result = true;
  bool ShouldPerformCodeMotion = false;
  SILInstruction *Inst = V.getInstruction();
  SILBasicBlock::iterator II = Inst, IE = Inst->getParent()->getTerminator();
  for (++II; II != IE; ++II) {
    DEBUG(llvm::dbgs() << "  Looking at next instruction: " << *II);
    switch (V.visit(SILValue(&*II))) {
      case ExitType::RefCountPairEliminated:
        DEBUG(llvm::dbgs() << "SUCCESS! Eliminated RR Pair...\n");
        return true;
      case ExitType::PerformCodeMotion:
        DEBUG(llvm::dbgs() << "FAILURE! Can not move retain further. Moving "
              "Retain...\n");
        Result = false;
        goto LoopExit;
      case ExitType::CommuteWithMotion:
        DEBUG(llvm::dbgs() << "    COMMUTES! Checking next instruction...\n");
        ShouldPerformCodeMotion = true;
        continue;
      case ExitType::CommuteWithoutMotion:
        DEBUG(llvm::dbgs() << "    COMMUTES WITHOUT MOTION! next "
              "instruction...\n");
        continue;
    }
  }
LoopExit:

  if (isa<CopyValueInst>(Inst) || !ShouldPerformCodeMotion) {
    DEBUG(llvm::dbgs() << "  Not performing code motion!\n");
    return false;
  }

  // If we found a new position for the retain, perform the code motion and
  // return true. We do not move copy_values since they may have uses.
  DEBUG(llvm::dbgs() << "  Performing code motion!\n");
  Inst->moveBefore(II);
  return Result;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static void processFunction(SILFunction &F) {
  DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  // For each basic block in F...
  for (auto &BB : F) {
    bool Changed;

    // process the basic block until we do not eliminate any retains/releases.
    do {
      Changed = false;
      for (auto II = BB.begin(), IE = BB.end(); II != IE; ) {
        SILInstruction *I = II++;

        if (auto *Retain = dyn_cast<StrongRetainInst>(I)) {
          DEBUG(llvm::dbgs() << "RETAIN Visiting: " << *Retain);
          ++NumStrongRetains;

          // Retain motion is a forward pass over the block. Make sure we don't
          // invalidate our iterators by parking it on the instruction before I.
          SILBasicBlock::iterator Safe = Retain;
          Safe = Safe != BB.begin() ? std::prev(Safe) : BB.end();

          LocalRetainMotionVisitor V(*Retain);
          if (performLocalCodeMotionDownCFG(V)) {
            // If we zapped or moved the retain, reset the iterator on the
            // instruction *newly* after the prev instruction.
            II = Safe != BB.end() ? std::next(Safe) : BB.begin();
            DEBUG(llvm::dbgs() << "\n");
            Changed = true;
          }
        }

        if (auto *CV = dyn_cast<CopyValueInst>(I)) {
          DEBUG(llvm::dbgs() << "COPY VALUE Visiting: " << *CV);
          ++NumCopyValue;

          // Retain motion is a forward pass over the block. Make sure we don't
          // invalidate our iterators by parking it on the instruction before I.
          SILBasicBlock::iterator Safe = CV;
          Safe = Safe != BB.begin() ? std::prev(Safe) : BB.end();

          LocalRetainMotionVisitor V(*CV);
          if (performLocalCodeMotionDownCFG(V)) {
            // If we zapped or moved the retain, reset the iterator on the
            // instruction *newly* after the prev instruction.
            II = Safe != BB.end() ? std::next(Safe) : BB.begin();
            DEBUG(llvm::dbgs() << "\n");
            Changed = true;
          }
        }
      }
    } while (Changed);
  }
}

void swift::performSILARCOpts(SILModule *M) {
  DEBUG(llvm::dbgs() << "*** SIL ARC OPTS ***\n");
  // For each function in the module...
  for (SILFunction &F : *M) {
    // If the function has no basic blocks, skip it...
    if (F.empty())
      continue;

    // Otherwise perform ARC optimizations.
    processFunction(F);
  }
}
