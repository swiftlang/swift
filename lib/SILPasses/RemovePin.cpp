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

#define DEBUG_TYPE "remove-pins"

#include "swift/SIL/Dominance.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILAnalysis/LoopAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Debug.h"

#include "llvm/Support/CommandLine.h"

using namespace swift;

namespace {
/// Trivial removal of pin/unpin instructions. This removes pin/unpin pairs
/// within a basic block that are not interleaved by a may-release.
class RemovePinInsts : public SILFunctionTransform {

  /// The set of currently available pins that have not been invalidate by an
  /// instruction that mayRelease memory.
  llvm::SmallPtrSet<SILInstruction *, 16> AvailablePins;

  AliasAnalysis *AA;

public:
  RemovePinInsts() {}

  StringRef getName() override { return "StrongPin/Unpin removal"; }

  void run() override {
    AA = PM->getAnalysis<AliasAnalysis>();

    bool Changed = false;
    for (auto &BB : *getFunction()) {

      // This is only a BB local analysis for now.
      AvailablePins.clear();

      for (auto InstIt = BB.begin(), End = BB.end(); InstIt != End; ) {
        auto *CurInst = &*InstIt;
        ++InstIt;

        // Add StrongPinInst to available pins.
        if (isa<StrongPinInst>(CurInst)) {
          AvailablePins.insert(CurInst);
          continue;
        }

        // Try to remove StrongUnpinInst if its input is available.
        if (auto *Unpin = dyn_cast<StrongUnpinInst>(CurInst)) {
          auto *PinDef = dyn_cast<StrongPinInst>(Unpin->getOperand().getDef());
          if (PinDef && AvailablePins.count(PinDef)){
            SmallVector<MarkDependenceInst *, 8> MarkDependentInsts;
            if (areSafePinUsers(PinDef, Unpin, MarkDependentInsts)) {
              Changed = true;
              auto NewDep = PinDef->getOperand();
              for (auto &MD : MarkDependentInsts)
                MD->setOperand(1, NewDep);
              Unpin->eraseFromParent();
              PinDef->eraseFromParent();
            }
            continue;
          }
          // Otherwise, fall through. An unpin, through destruction of an object
          // can have arbitrary sideeffects.
        }

        // In all other cases check whether this could be a potentially
        // releasing instruction.
        invalidateAvailablePins(CurInst);
      }
    }

    if (Changed)
      PM->invalidateAnalysis(getFunction(),
                             SILAnalysis::InvalidationKind::Instructions);
  }

  /// Pin uses are safe if they either mark a dependence or if it is the unpin we
  /// are trying to remove.
  bool areSafePinUsers(StrongPinInst *Pin, StrongUnpinInst *Unpin,
                       SmallVectorImpl<MarkDependenceInst *> &MarkDeps) {
    for (auto *U : Pin->getUses()) {
      if (auto *MD = dyn_cast<MarkDependenceInst>(U->getUser()))
        MarkDeps.push_back(MD);
      else if (dyn_cast<StrongUnpinInst>(U->getUser()) == Unpin)
        continue;
      else
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

    case ArrayCallKind::kArrayPropsNeedsTypeCheck:
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

  /// Removes available pins that could be released by executing of 'I'.
  void invalidateAvailablePins(SILInstruction *I) {
    // Collect pins that we have to clear because they might have been released.
    SmallVector<SILInstruction *, 16> RemovePin;
    for (auto *P : AvailablePins) {
      if (!isSafeArraySemanticFunction(I) &&
          arc::canDecrementRefCount(I, P, AA))
          RemovePin.push_back(P);
    }

    for (auto P: RemovePin)
      AvailablePins.erase(P);
  }
};
}

SILTransform *swift::createRemovePins() {
  return new RemovePinInsts();
}
