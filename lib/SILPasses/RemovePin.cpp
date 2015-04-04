//===------- RemovePin.cpp -  StrongPin/Unpin removal -----*- C++ -*-------===//
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

#define DEBUG_TYPE "sil-remove-pins"

#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/ArraySemantic.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumPinPairsRemoved, "Num pin pairs removed");

using namespace swift;

/// \brief Can this instruction read the pinned bit of the reference count.
/// Reading the pinned prevents us from moving the pin instructions accross it.
static bool mayReadPinFlag(SILInstruction *I) {
  auto Kind = I->getKind();
  if (Kind != ValueKind::ApplyInst)
    return false;
  if (!I->mayReadFromMemory())
    return false;
  // Apply instructions that may read from memory can read the pin bit.
  return true;
}

namespace {
/// Trivial removal of pin/unpin instructions. This removes pin/unpin pairs
/// within a basic block that are not interleaved by a may-release.
class RemovePinInsts : public SILFunctionTransform {

  /// The set of currently available pins that have not been invalidate by an
  /// instruction that mayRelease memory.
  llvm::SmallPtrSet<SILInstruction *, 16> AvailablePins;

  AliasAnalysis *AA;

  RCIdentityFunctionInfo *RCIA;

public:
  RemovePinInsts() {}

  StringRef getName() override { return "StrongPin/Unpin removal"; }

  void run() override {
    AA = PM->getAnalysis<AliasAnalysis>();
    RCIA = PM->getAnalysis<RCIdentityAnalysis>()->get(getFunction());

    DEBUG(llvm::dbgs() << "*** Running Pin Removal on "
                       << getFunction()->getName() << "\n");

    bool Changed = false;
    for (auto &BB : *getFunction()) {

      // This is only a BB local analysis for now.
      AvailablePins.clear();

      DEBUG(llvm::dbgs() << "Visiting new BB!\n");

      for (auto InstIt = BB.begin(), End = BB.end(); InstIt != End; ) {
        auto *CurInst = &*InstIt;
        ++InstIt;

        DEBUG(llvm::dbgs() << "    Visiting: " << *CurInst);

        // Add StrongPinInst to available pins.
        if (isa<StrongPinInst>(CurInst)) {
          DEBUG(llvm::dbgs() << "        Found pin!\n");
          AvailablePins.insert(CurInst);
          continue;
        }

        // Try to remove StrongUnpinInst if its input is available.
        if (auto *Unpin = dyn_cast<StrongUnpinInst>(CurInst)) {
          DEBUG(llvm::dbgs() << "        Found unpin!\n");
          SILValue RCId = RCIA->getRCIdentityRoot(Unpin->getOperand());
          DEBUG(llvm::dbgs() << "        RCID Source: " << *RCId.getDef());
          auto *PinDef = dyn_cast<StrongPinInst>(RCId.getDef());
          if (PinDef && AvailablePins.count(PinDef)){
            DEBUG(llvm::dbgs() << "        Found matching pin: " << *PinDef);
            SmallVector<MarkDependenceInst *, 8> MarkDependentInsts;
            if (areSafePinUsers(PinDef, Unpin, MarkDependentInsts)) {
              DEBUG(llvm::dbgs() << "        Pin users are safe! Removing!\n");
              Changed = true;
              auto *Enum = SILBuilder(PinDef).createOptionalSome(
                  PinDef->getLoc(), PinDef->getOperand(), PinDef->getType(0));
              SILValue(PinDef).replaceAllUsesWith(Enum);
              Unpin->eraseFromParent();
              PinDef->eraseFromParent();
              // Remove this pindef from AvailablePins.
              AvailablePins.erase(PinDef);
              ++NumPinPairsRemoved;
            } else {
              DEBUG(llvm::dbgs()
                    << "        Pin users are not safe! Can not remove!\n");
            }

            continue;
          } else {
            DEBUG(llvm::dbgs() << "        Failed to find matching pin!\n");
          }
          // Otherwise, fall through. An unpin, through destruction of an object
          // can have arbitrary sideeffects.
        }

        // If we have a strong_release or a release_value, see if our parameter
        // is in an array semantic guaranteed self call sequence. If so, we can
        // ignore the release for the retain before the call exactly matches it.
        //
        // Discussion: The guaranteed self call sequence is as follows:
        //
        // retain (rcid)
        // ... no releases ...
        // call arraysemantic_func(@guaranteed_self rcid)
        // ... no instructions conservatively using rcid in a manner that
        // ... requires rcid to stay live.
        // release (rcid)
        //
        // I am purposely restricting this to array semantic functions that we
        // know are well behaved (i.e. the ref counts are the same on both sides
        // of the callsite).
        if (isa<StrongReleaseInst>(CurInst) || isa<ReleaseValueInst>(CurInst)) {
          if (isReleaseEndOfGuaranteedSelfCallSequence(CurInst)) {
            DEBUG(llvm::dbgs() << "        Ignoring exactly balanced "
                                  "release.\n");
            continue;
          }
        }

        // In all other cases check whether this could be a potentially
        // releasing instruction.
        DEBUG(llvm::dbgs()
              << "        Checking if this inst invalidates pins.\n");
        invalidateAvailablePins(CurInst);
      }
    }

    if (Changed)
      PM->invalidateAnalysis(getFunction(),
                             SILAnalysis::PreserveKind::ProgramFlow);
  }

  /// Pin uses are safe if:
  ///
  /// 1. The user marks a dependence.
  /// 2. The user is the unpin we are trying to remove.
  /// 3. The user is an RCIdentical user of our Pin result and only has
  ///    RCIdentity preserving insts, mark dependence, or the unpin we are
  ///    trying
  ///    to remove as users.
  bool areSafePinUsers(StrongPinInst *Pin, StrongUnpinInst *Unpin,
                       SmallVectorImpl<MarkDependenceInst *> &MarkDeps) {
    // Grab all uses looking past RCIdentical uses from RCIdentityAnalysis.
    llvm::SmallVector<SILInstruction *, 8> Users;
    RCIA->getRCUsers(SILValue(Pin), Users);

    for (auto *U : Users) {
      if (auto *MD = dyn_cast<MarkDependenceInst>(U)) {
        MarkDeps.push_back(MD);
        continue;
      }

      if (dyn_cast<StrongUnpinInst>(U) == Unpin)
        continue;

      return false;
    }
    return true;
  }

  /// Certain semantic functions are generally safe because they don't release
  /// the array in unexpected ways.
  bool isSafeArraySemanticFunction(SILInstruction *I) {
    ArraySemanticsCall Call(I);
    if (!Call)
      return false;
    switch (Call.getKind()) {
    default:
      return false;

    case ArrayCallKind::kArrayPropsIsNative:
    case ArrayCallKind::kArrayPropsIsNativeNoTypeCheck:
    case ArrayCallKind::kCheckSubscript:
    case ArrayCallKind::kCheckIndex:
    case ArrayCallKind::kGetCount:
    case ArrayCallKind::kGetCapacity:
    case ArrayCallKind::kGetElement:
    case ArrayCallKind::kGetElementAddress:
    case ArrayCallKind::kMakeMutable:
      return true;
    }
  }

  bool isSafeGuaranteedSemanticFunction(SILInstruction *I) {
    // Make sure that we are safe.
    if (!isSafeArraySemanticFunction(I))
      return false;

    // We do not need to check if call is nullptr, since this was checked
    // earlier in isSafeArraySemanticFunction.
    //
    // TODO: We already created an ArraySemanticsCall in
    // isSafeArraySemanticFunction. I wonder if we can refactor into a third
    // method that takes an array semantic call. Then we can reuse the work.
    ArraySemanticsCall Call(I);

    // If our call does not have guaranteed self, bail.
    if (!Call.hasGuaranteedSelf())
      return false;

    // Success!
    return true;
  }

  /// Removes available pins that could be released by executing of 'I'.
  void invalidateAvailablePins(SILInstruction *I) {
    // Collect pins that we have to clear because they might have been released.
    SmallVector<SILInstruction *, 16> RemovePin;
    for (auto *P : AvailablePins) {
      if (!isSafeArraySemanticFunction(I) &&
          (mayDecrementRefCount(I, P, AA) ||
           mayReadPinFlag(I)))
          RemovePin.push_back(P);
    }

    if (RemovePin.empty()) {
      DEBUG(llvm::dbgs() << "        No pins to invalidate!\n");
      return;
    }

    for (auto *P : RemovePin) {
      DEBUG(llvm::dbgs() << "        Invalidating Pin: " << *P);
      AvailablePins.erase(P);
    }
  }

  bool isReleaseEndOfGuaranteedSelfCallSequence(SILInstruction *I) {
    SILBasicBlock *BB = I->getParent();

    // For now just look at the previous instruction if it exists.
    SILBasicBlock::iterator Start = BB->begin();
    SILBasicBlock::iterator Iter = I;
    if (Iter == Start)
      return false;
    --Iter;

    // Now grab the RCID of this instruction.
    SILValue RCID = RCIA->getRCIdentityRoot(I->getOperand(0));

    // See if iter is an apply inst that is a safe guaranteed semantic
    // function. If not, return false.
    if (!isSafeGuaranteedSemanticFunction(&*Iter))
      return false;
    ApplyInst *AI = cast<ApplyInst>(&*Iter);

    // Make sure that AI's self argument has the same RCID as our
    // instruction. Otherwise, return false.
    if (RCID != RCIA->getRCIdentityRoot(AI->getSelfArgument()))
      return false;

    // Then grab the previous instruction (if it exists).
    if (Iter == Start)
      return false;
    --Iter;

    // See if we have a retain of some sort, if we don't, bail.
    if (!isa<RetainValueInst>(Iter) && !isa<StrongRetainInst>(Iter)) {
      return false;
    }

    // Then make sure that the rcid of the retain is the same as our release. If
    // not bail.
    if (RCID != RCIA->getRCIdentityRoot(Iter->getOperand(0)))
      return false;

    // Success!
    return true;
  }
};
}

SILTransform *swift::createRemovePins() {
  return new RemovePinInsts();
}
