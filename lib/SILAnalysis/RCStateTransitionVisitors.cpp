//===--- RCStateTransitionVisitors.cpp ------------------------------------===//
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

#define DEBUG_TYPE "sil-global-arc-opts"
#include "RCStateTransitionVisitors.h"
#include "ARCBBState.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                      BottomUpRCStateTransitionVisitor
//===----------------------------------------------------------------------===//

BottomUpDataflowRCStateVisitor::BottomUpDataflowRCStateVisitor(
    RCIdentityFunctionInfo *RCFI, ARCBBState &BBState,
    bool FreezeOwnedArgEpilogueReleases,
    ConsumedArgToEpilogueReleaseMatcher &ERM,
    IncToDecStateMapTy &IncToDecStateMap)
    : RCFI(RCFI), BBState(BBState),
      FreezeOwnedArgEpilogueReleases(FreezeOwnedArgEpilogueReleases),
      EpilogueReleaseMatcher(ERM), IncToDecStateMap(IncToDecStateMap) {}

BottomUpDataflowRCStateVisitor::DataflowResult
BottomUpDataflowRCStateVisitor::visitAutoreleasePoolCall(SILInstruction *I) {
  BBState.clear();
  return DataflowResult();
}

BottomUpDataflowRCStateVisitor::DataflowResult
BottomUpDataflowRCStateVisitor::visitStrongDecrement(SILInstruction *I) {
  SILValue Op = RCFI->getRCIdentityRoot(I->getOperand(0));

  // If this instruction is a post dominating release, skip it so we don't pair
  // it up with anything. Do make sure that it does not effect any other
  // instructions.
  if (FreezeOwnedArgEpilogueReleases &&
      EpilogueReleaseMatcher.isReleaseMatchedToArgument(I))
    return DataflowResult(Op);

  BottomUpRefCountState &State = BBState.getBottomUpRefCountState(Op);
  bool NestingDetected = State.initWithInst(I);

  // If we are running with 'frozen' owned arg releases, check if we have a
  // frozen use in the side table. If so, this release must be known safe.
  if (FreezeOwnedArgEpilogueReleases) {
    State.KnownSafe |= EpilogueReleaseMatcher.argumentHasRelease(Op);
  }

  DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT! Known Safe: "
                     << (State.isKnownSafe() ? "yes" : "no") << "\n");

  // Continue on to see if our reference decrement could potentially affect
  // any other pointers via a use or a decrement.
  return DataflowResult(Op, NestingDetected);
}

BottomUpDataflowRCStateVisitor::DataflowResult
BottomUpDataflowRCStateVisitor::visitStrongIncrement(SILInstruction *I) {
  // Look up the state associated with its operand...
  SILValue Op = RCFI->getRCIdentityRoot(I->getOperand(0));
  BottomUpRefCountState &RefCountState = BBState.getBottomUpRefCountState(Op);

  DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT!\n");

  // If we find a state initialized with a matching increment, pair this
  // decrement with a copy of the ref count state and then clear the ref
  // count state in preparation for any future pairs we may see on the same
  // pointer.
  if (RefCountState.isRefCountInstMatchedToTrackedInstruction(I)) {
    // Copy the current value of ref count state into the result map.
    IncToDecStateMap[I] = RefCountState;
    DEBUG(llvm::dbgs() << "    MATCHING DECREMENT:"
                       << RefCountState.getRCRoot());

    // Clear the ref count state so it can be used for future pairs we may
    // see.
    RefCountState.clear();
  }
#ifndef NDEBUG
  else {
    if (RefCountState.isTrackingRefCountInst()) {
      DEBUG(llvm::dbgs() << "    FAILED MATCH DECREMENT:"
                         << RefCountState.getRCRoot());
    } else {
      DEBUG(llvm::dbgs() << "    FAILED MATCH DECREMENT. Not tracking a "
                            "decrement.\n");
    }
  }
#endif
  return DataflowResult(Op);
}
