//===--- RemovePin.cpp -  StrongPin/Unpin removal -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-remove-pins"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "llvm/ADT/Statistic.h"
STATISTIC(NumPinPairsRemoved, "Number of pin pairs removed");

using namespace swift;

/// \brief Can this instruction read the pinned bit of the reference count.
/// Reading the pinned prevents us from moving the pin instructions across it.
static bool mayReadPinFlag(SILInstruction *I) {
  if (isa<IsUniqueOrPinnedInst>(I))
    return true;
  if (!isa<ApplyInst>(I))
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
  llvm::SmallPtrSet<StrongPinInst *, 16> AvailablePins;

  AliasAnalysis *AA;

  RCIdentityFunctionInfo *RCIA;

public:
  RemovePinInsts() {}

  void run() override {
    AA = PM->getAnalysis<AliasAnalysis>();
    RCIA = PM->getAnalysis<RCIdentityAnalysis>()->get(getFunction());

    LLVM_DEBUG(llvm::dbgs() << "*** Running Pin Removal on "
                            << getFunction()->getName() << "\n");

    bool Changed = false;
    for (auto &BB : *getFunction()) {

      // This is only a BB local analysis for now.
      AvailablePins.clear();

      LLVM_DEBUG(llvm::dbgs() << "Visiting new BB!\n");

      for (auto InstIt = BB.begin(), End = BB.end(); InstIt != End; ) {
        auto *CurInst = &*InstIt;
        ++InstIt;

        LLVM_DEBUG(llvm::dbgs() << "    Visiting: " << *CurInst);

        // Add StrongPinInst to available pins.
        if (auto pin = dyn_cast<StrongPinInst>(CurInst)) {
          LLVM_DEBUG(llvm::dbgs() << "        Found pin!\n");
          AvailablePins.insert(pin);
          continue;
        }

        // Try to remove StrongUnpinInst if its input is available.
        if (auto *Unpin = dyn_cast<StrongUnpinInst>(CurInst)) {
          LLVM_DEBUG(llvm::dbgs() << "        Found unpin!\n");
          SILValue RCId = RCIA->getRCIdentityRoot(Unpin->getOperand());
          LLVM_DEBUG(llvm::dbgs() << "        RCID Source: " << *RCId);
          auto *PinDef = dyn_cast<StrongPinInst>(RCId);
          if (PinDef && AvailablePins.count(PinDef)) {
            LLVM_DEBUG(llvm::dbgs() << "        Found matching pin: "
                                    << *PinDef);
            SmallVector<MarkDependenceInst *, 8> MarkDependentInsts;
            if (areSafePinUsers(PinDef, Unpin, MarkDependentInsts)) {
              LLVM_DEBUG(llvm::dbgs() << "        Pin users are safe! "
                                         "Removing!\n");
              Changed = true;
              auto *Enum = SILBuilder(PinDef).createOptionalSome(
                  PinDef->getLoc(), PinDef->getOperand(), PinDef->getType());
              PinDef->replaceAllUsesWith(Enum);
              Unpin->eraseFromParent();
              PinDef->eraseFromParent();
              // Remove this pindef from AvailablePins.
              AvailablePins.erase(PinDef);
              ++NumPinPairsRemoved;
            } else {
              LLVM_DEBUG(llvm::dbgs()
                         << "        Pin users are not safe! Cannot remove!\n");
            }

            continue;
          } else {
            LLVM_DEBUG(llvm::dbgs() <<"        Failed to find matching pin!\n");
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
            LLVM_DEBUG(llvm::dbgs() << "        Ignoring exactly balanced "
                                       "release.\n");
            continue;
          }
        }

        // In all other cases check whether this could be a potentially
        // releasing instruction.
        LLVM_DEBUG(llvm::dbgs()
                   << "        Checking if this inst invalidates pins.\n");
        invalidateAvailablePins(CurInst);
      }
    }

    if (Changed)
      PM->invalidateAnalysis(getFunction(),
                             SILAnalysis::InvalidationKind::Instructions);
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
      // A mark_dependence is safe if it is marking a dependence on a base that
      // is the strong_pinned value:
      //    %0 = strong_pin ...
      //    %1 = mark_dependence ... on %0
      // or
      //    %0 = strong_pin ...
      //    %1 = foo ... %0 ...
      //    %2 = mark_dependence ... on %1
      if (auto *MD = dyn_cast<MarkDependenceInst>(U))
        if (Pin == MD->getBase() ||
            std::find_if(Users.begin(), Users.end(),
                         [&](SILInstruction *I) {
                           return MD->getBase()->getDefiningInstruction() == I;
                         }) != Users.end()) {
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

    case ArrayCallKind::kCheckSubscript:
    case ArrayCallKind::kCheckIndex:
    case ArrayCallKind::kGetCount:
    case ArrayCallKind::kGetCapacity:
    case ArrayCallKind::kGetElement:
      // Only arrays that cannot be backed by NSArrays are safe. A method on
      // NSArray may do arbitrary things including releasing the array.
      return !Call.mayHaveBridgedObjectElementType();

    case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
    case ArrayCallKind::kGetElementAddress:
    case ArrayCallKind::kMakeMutable:
      // These do not call NSArray methods.
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
    ArraySemanticsCall Call(cast<ApplyInst>(I));

    // If our call does not have guaranteed self, bail.
    if (!Call.hasGuaranteedSelf())
      return false;

    // Success!
    return true;
  }

  /// Removes available pins that could be released by executing of 'I'.
  void invalidateAvailablePins(SILInstruction *I) {
    // Collect pins that we have to clear because they might have been released.
    SmallVector<StrongPinInst *, 16> RemovePin;
    for (auto *P : AvailablePins) {
      if (!isSafeArraySemanticFunction(I) &&
          (mayDecrementRefCount(I, P, AA) ||
           mayReadPinFlag(I)))
          RemovePin.push_back(P);
    }

    if (RemovePin.empty()) {
      LLVM_DEBUG(llvm::dbgs() << "        No pins to invalidate!\n");
      return;
    }

    for (auto *P : RemovePin) {
      LLVM_DEBUG(llvm::dbgs() << "        Invalidating Pin: " << *P);
      AvailablePins.erase(P);
    }
  }

  bool isReleaseEndOfGuaranteedSelfCallSequence(SILInstruction *I) {
    SILBasicBlock *BB = I->getParent();

    // For now just look at the previous instruction if it exists.
    SILBasicBlock::iterator Start = BB->begin();
    SILBasicBlock::iterator Iter = I->getIterator();
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
} // end anonymous namespace

SILTransform *swift::createRemovePins() {
  return new RemovePinInsts();
}
