//===----------- ARCOpts.cpp - Perform SIL ARC Optimizations --------------===//
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
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<bool>
EnableARCOpts("enable-arc-opts", llvm::cl::Hidden, llvm::cl::init(true));

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
//                        Local Source Motion Visitor
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
template <class Subclass>
class LocalRefCountIncMotionVisitor
  : public SILVisitor<LocalRefCountIncMotionVisitor<Subclass>, ExitType> {
protected:

  /// The module the instruction we are processing is in.
  SILModule &M;

public:
  LocalRefCountIncMotionVisitor(SILModule &Module) : M(Module) { }

  ExitType visitValueBase(ValueBase *V) {
    llvm_unreachable("This should never be hit.");
  }

  Subclass &asImpl() { return static_cast<Subclass &>(*this); }

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
  ExitType visitApplyInst(ApplyInst *AI);
  ExitType visitCopyAddrInst(CopyAddrInst *CA);

#define DELEGATE_CASE_TO_SUBCLASS(_Value) \
    ExitType visit ## _Value(_Value *I) { return asImpl().visit ## _Value(I); }
    DELEGATE_CASE_TO_SUBCLASS(CopyValueInst)
    DELEGATE_CASE_TO_SUBCLASS(DestroyValueInst)
    DELEGATE_CASE_TO_SUBCLASS(StrongRetainInst)
    DELEGATE_CASE_TO_SUBCLASS(StrongReleaseInst)
#undef DELEGATE_CASE

  bool performLocalCodeMotionDownCFG();
};

} // end anonymous namespace

template <class Subclass>
bool LocalRefCountIncMotionVisitor<Subclass>::performLocalCodeMotionDownCFG() {
  bool Result = true;
  bool ShouldPerformCodeMotion = false;
  SILInstruction *Inst = asImpl().getInstruction();
  SILBasicBlock::iterator II = Inst, IE = Inst->getParent()->getTerminator();
  for (++II; II != IE; ++II) {
    DEBUG(llvm::dbgs() << "  Looking at next instruction: " << *II);
    switch (asImpl().visit(SILValue(&*II))) {
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

  // If we can not perform code motion at all, return false since we did not
  // remove any retain/release.
  if (!ShouldPerformCodeMotion) {
    DEBUG(llvm::dbgs() << "  Not performing code motion!\n");
    return false;
  }

  // Otherwise, attempt to perform code motion and return whether or not we
  // eliminated a retain/release.
  return asImpl().performCodeMotion(II) && Result;
}

/// Skip over instructions that are guaranteed to not decrement ref counts. In
/// SIL, this is guaranteed by an instruction not having side effects.
template <class Subclass>
ExitType
LocalRefCountIncMotionVisitor<Subclass>::
visitSILInstruction(SILInstruction *I) {
  // If the instruction does not have any side effects, the retain/copy value
  // and the instruction commute.
  if (I->getMemoryBehavior() !=
      SILInstruction::MemoryBehavior::MayHaveSideEffects)
    return ExitType::CommuteWithMotion;

  // Otherwise assume that something is occuring here we do not understand,
  // so bail, performing any code motion that is possible at this point.
  return ExitType::PerformCodeMotion;
}

template <class Subclass>
ExitType
LocalRefCountIncMotionVisitor<Subclass>::visitApplyInst(ApplyInst *AI) {
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

  // Plug in alias analysis here.

  // Otherwise exit, performing any code motion that we can.
  return ExitType::PerformCodeMotion;
}

template <class Subclass>
ExitType
LocalRefCountIncMotionVisitor<Subclass>::visitCopyAddrInst(CopyAddrInst *CA) {
  // A copy address that is an initialization does not release the old value, so
  // it always commutes with retains.
  if (CA->isInitializationOfDest() == IsInitialization_t::IsInitialization)
    return ExitType::CommuteWithMotion;

  // Otherwise, the CopyAddr will release a pointer implying we must exit,
  // performing code motion if we can.
  return ExitType::PerformCodeMotion;
}

//===----------------------------------------------------------------------===//
//                        Local Retain Motion Visitor
//===----------------------------------------------------------------------===//

namespace {

class LocalRetainMotionVisitor
  : public LocalRefCountIncMotionVisitor<LocalRetainMotionVisitor> {

  StrongRetainInst *Retain;

public:
  LocalRetainMotionVisitor(StrongRetainInst &SRI)
    : LocalRefCountIncMotionVisitor(SRI.getModule()), Retain(&SRI) { }

  SILInstruction *getInstruction() const { return Retain; }

#define SIMPLE_CASE(_Value, _ExitType)                                \
  ExitType visit ## _Value(_Value *I) { return ExitType::_ExitType; }
  SIMPLE_CASE(CopyValueInst, CommuteWithoutMotion)
  SIMPLE_CASE(DestroyValueInst, PerformCodeMotion)
#undef SIMPLE_CASE

  ExitType visitStrongRetainInst(StrongRetainInst *InputRetain);
  ExitType visitStrongReleaseInst(StrongReleaseInst *Release);

  bool performCodeMotion(SILInstruction *I) {
    DEBUG(llvm::dbgs() << "  Performing code motion!\n");
    Retain->moveBefore(I);
    return true;
  }

};

} // end anonymous namespace.

ExitType
LocalRetainMotionVisitor::visitStrongRetainInst(StrongRetainInst *InputRetain) {
  // If we see a retain, skip over it since a retain can never cause a release
  // to occur.
  //
  // On the other hand, if we have a retain that is naively on the same pointer,
  // don't move past it since no "progress" has been made. If we remove any
  // pairs in this basic block, we will process it again to allow for this
  // retain to be removed as well.
  if (Retain->getOperand() == InputRetain->getOperand())
    return ExitType::PerformCodeMotion;
  return ExitType::CommuteWithoutMotion;
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
  if (Retain->getOperand() != Release->getOperand())
    return ExitType::PerformCodeMotion;

  DEBUG(llvm::dbgs() << "    Eliminating Retain Release Pair!\n");
  DEBUG(llvm::dbgs() << "    Retain: " << *Retain);
  DEBUG(llvm::dbgs() << "    Release: " << *Release);
  Retain->eraseFromParent();
  Release->eraseFromParent();
  ++NumStrongRetainStrongReleasePairsEliminated;
  return ExitType::RefCountPairEliminated;
}

//===----------------------------------------------------------------------===//
//                             Copy Value Visitor
//===----------------------------------------------------------------------===//

namespace {

class LocalCopyValueMotionVisitor
  : public LocalRefCountIncMotionVisitor<LocalCopyValueMotionVisitor> {

  CopyValueInst *CV;

public:
  LocalCopyValueMotionVisitor(CopyValueInst &CV)
    : LocalRefCountIncMotionVisitor(CV.getModule()), CV(&CV) { }

  SILInstruction *getInstruction() const { return CV; }

#define SIMPLE_CASE(_Value, _ExitType)                                \
  ExitType visit ## _Value(_Value *I) { return ExitType::_ExitType; }
  SIMPLE_CASE(StrongRetainInst, CommuteWithoutMotion)
  SIMPLE_CASE(StrongReleaseInst, PerformCodeMotion)
#undef SIMPLE_CASE

  ExitType visitCopyValueInst(CopyValueInst *InputCV);
  ExitType visitDestroyValueInst(DestroyValueInst *DV);

  bool performCodeMotion(SILInstruction *I) {
    DEBUG(llvm::dbgs() << "  Not performing code motion!\n");
    return false;
  }
};

} // end anonymous namespace

ExitType
LocalCopyValueMotionVisitor::visitCopyValueInst(CopyValueInst *InputCV) {
  // If we see a copy_value, skip over it since a copy_value can never cause a
  // release to occur.
  //
  // On the other hand, if we have a copy_value that is naively on the same
  // pointer, don't move past it since no "progress" has been made. If we remove
  // any pairs in this basic block, we will process it again to allow for this
  // copy_value to be removed as well.
  if (CV->getOperand() == InputCV->getOperand())
    return ExitType::PerformCodeMotion;
  return ExitType::CommuteWithoutMotion;
}

ExitType
LocalCopyValueMotionVisitor::visitDestroyValueInst(DestroyValueInst *DV) {
  // If we are processing a copy_value, exit performing code motion since the
  // the release could trigger a destructor causing potentially unknown side
  // effects.
  //
  // Otherwise if we have a retain and the input release is naively on the same
  // pointer, eliminate the pair. Otherwise do nothing since the release could
  // affect our retain count indirectly via a destructor.
  if (CV->getOperand() != DV->getOperand() && CV != DV->getOperand().getDef())
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
          if (V.performLocalCodeMotionDownCFG()) {
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

          LocalCopyValueMotionVisitor V(*CV);
          if (V.performLocalCodeMotionDownCFG()) {
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
  if (!EnableARCOpts)
    return;

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
