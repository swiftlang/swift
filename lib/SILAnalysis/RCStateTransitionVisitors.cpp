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
BottomUpDataflowRCStateVisitor::visitAutoreleasePoolCall(ValueBase *V) {
  BBState.clear();

  // We just cleared our BB State so we have no more possible effects.
  return DataflowResult(RCStateTransitionDataflowResultKind::NoEffects);
}

BottomUpDataflowRCStateVisitor::DataflowResult
BottomUpDataflowRCStateVisitor::visitStrongDecrement(ValueBase *V) {
  auto *I = dyn_cast<SILInstruction>(V);
  if (!I)
    return DataflowResult();

  SILValue Op = RCFI->getRCIdentityRoot(I->getOperand(0));

  // If this instruction is a post dominating release, skip it so we don't pair
  // it up with anything. Do make sure that it does not effect any other
  // instructions.
  if (FreezeOwnedArgEpilogueReleases &&
      EpilogueReleaseMatcher.isReleaseMatchedToArgument(I))
    return DataflowResult(Op);

  BottomUpRefCountState &State = BBState.getBottomUpRefCountState(Op);
  bool NestingDetected = State.initWithMutatorInst(I);

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
BottomUpDataflowRCStateVisitor::visitStrongIncrement(ValueBase *V) {
  auto *I = dyn_cast<SILInstruction>(V);
  if (!I)
    return DataflowResult();

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

//===----------------------------------------------------------------------===//
//                       TopDownDataflowRCStateVisitor
//===----------------------------------------------------------------------===//

TopDownDataflowRCStateVisitor::TopDownDataflowRCStateVisitor(
    RCIdentityFunctionInfo *RCFI, ARCBBState &BBState,
    DecToIncStateMapTy &DecToIncStateMap)
    : RCFI(RCFI), BBState(BBState), DecToIncStateMap(DecToIncStateMap) {}

TopDownDataflowRCStateVisitor::DataflowResult
TopDownDataflowRCStateVisitor::visitAutoreleasePoolCall(ValueBase *V) {
  BBState.clear();
  // We just cleared our BB State so we have no more possible effects.
  return DataflowResult(RCStateTransitionDataflowResultKind::NoEffects);
}

TopDownDataflowRCStateVisitor::DataflowResult
TopDownDataflowRCStateVisitor::visitStrongDecrement(ValueBase *V) {
  auto *I = dyn_cast<SILInstruction>(V);
  if (!I)
    return DataflowResult();

  // Look up the state associated with I's operand...
  SILValue Op = RCFI->getRCIdentityRoot(I->getOperand(0));
  TopDownRefCountState &RefCountState = BBState.getTopDownRefCountState(Op);

  DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT!\n");

  // If we are tracking an increment on the ref count root associated with
  // the decrement and the decrement matches, pair this decrement with a
  // copy of the increment state and then clear the original increment state
  // so that we are ready to process further values.
  if (RefCountState.isRefCountInstMatchedToTrackedInstruction(I)) {
    // Copy the current value of ref count state into the result map.
    DecToIncStateMap[I] = RefCountState;
    DEBUG(llvm::dbgs() << "    MATCHING INCREMENT:\n"
                       << RefCountState.getRCRoot());

    // Clear the ref count state in preparation for more pairs.
    RefCountState.clear();
  }
#if NDEBUG
  else {
    if (RefCountState.isTrackingRefCountInst()) {
      DEBUG(llvm::dbgs() << "    FAILED MATCH INCREMENT:\n"
                         << RefCountState.getValue());
    } else {
      DEBUG(llvm::dbgs() << "    FAILED MATCH. NO INCREMENT.\n");
    }
  }
#endif

  // Otherwise we continue processing the reference count decrement to see if
  // the decrement can affect any other pointers that we are tracking.
  return DataflowResult(Op);
}

TopDownDataflowRCStateVisitor::DataflowResult
TopDownDataflowRCStateVisitor::visitStrongIncrement(ValueBase *V) {
  auto *I = dyn_cast<SILInstruction>(V);
  if (!I)
    return DataflowResult();

  // Map the increment's operand to a newly initialized or reinitialized ref
  // count state and continue...
  SILValue Op = RCFI->getRCIdentityRoot(I->getOperand(0));
  TopDownRefCountState &State = BBState.getTopDownRefCountState(Op);
  bool NestingDetected = State.initWithMutatorInst(I);

  DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT! Known Safe: "
                     << (State.isKnownSafe() ? "yes" : "no") << "\n");

  // Continue processing in case this increment could be a CanUse for a
  // different pointer.
  return DataflowResult(Op, NestingDetected);
}

TopDownDataflowRCStateVisitor::DataflowResult
TopDownDataflowRCStateVisitor::visitStrongEntranceArgument(SILArgument *Arg) {
  DEBUG(llvm::dbgs() << "VISITING ENTRANCE ARGUMENT: " << *Arg);

  if (!Arg->hasConvention(ParameterConvention::Direct_Owned)) {
    DEBUG(llvm::dbgs() << "    Not owned! Bailing!\n");
    return DataflowResult();
  }

  DEBUG(llvm::dbgs() << "    Initializing state.\n");

  TopDownRefCountState &State = BBState.getTopDownRefCountState(Arg);
  State.initWithArg(Arg);

  return DataflowResult();
}

TopDownDataflowRCStateVisitor::DataflowResult
TopDownDataflowRCStateVisitor::visitStrongEntranceApply(ApplyInst *AI) {
  DEBUG(llvm::dbgs() << "VISITING ENTRANCE APPLY: " << *AI);

  // We should have checked earlier that AI has an owned result value. To
  // prevent mistakes, assert that here.
  assert(AI->hasResultConvention(ResultConvention::Owned) &&
         "Expected AI to be Owned here");

  // Otherwise, return a dataflow result containing a +1.
  DEBUG(llvm::dbgs() << "    Initializing state.\n");

  TopDownRefCountState &State = BBState.getTopDownRefCountState(AI);
  State.initWithEntranceInst(AI);

  return DataflowResult(AI);
}

TopDownDataflowRCStateVisitor::DataflowResult
TopDownDataflowRCStateVisitor::visitStrongEntrance(ValueBase *V) {
  if (auto *Arg = dyn_cast<SILArgument>(V))
    return visitStrongEntranceArgument(Arg);

  if (auto *AI = dyn_cast<ApplyInst>(V))
    return visitStrongEntranceApply(AI);

  return DataflowResult();
}
