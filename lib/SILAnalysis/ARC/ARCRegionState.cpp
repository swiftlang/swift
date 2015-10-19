//===--- ARCRegionState.cpp -----------------------------------------------===//
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
#include "ARCRegionState.h"
#include "RCStateTransitionVisitors.h"
#include "swift/SILAnalysis/LoopRegionAnalysis.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                               ARCRegionState
//===----------------------------------------------------------------------===//

ARCRegionState::ARCRegionState(LoopRegion *R)
    : Region(R), PtrToTopDownState(), PtrToBottomUpState(), AllowsLeaks(false) {
  if (R->isBlock())
    AllowsLeaks = isARCInertTrapBB(R->getBlock());
}

//===---
// Utility
//

//===---
// Bottom Up Merge
//

/// Initialize this Region with the state of the successor region. This is
/// called on a region's state and then any other successors states are merged
/// in.
void ARCRegionState::initSuccBottomUp(ARCRegionState &SuccRegionState) {
  PtrToBottomUpState = SuccRegionState.PtrToBottomUpState;
}

/// Merge in the state of the successor basic block. Returns true if after the
/// merge operation the region is tracking any state. Returns false otherwise.
///
/// The return value enables an analysis to bail early.
///
/// This is an intersection operation.
void ARCRegionState::mergeSuccBottomUp(ARCRegionState &SuccRegionState) {
  // Otherwise for each [(SILValue, BottomUpState)] that we are tracking...
  for (std::pair<SILValue, BottomUpRefCountState> &Pair : getBottomupStates()) {
    SILValue RefCountedValue = Pair.first;

    // If our SILValue was blotted, skip it. This will be ignored for the rest
    // of the ARC optimization.
    if (!RefCountedValue)
      continue;

    // Then attempt to lookup the corresponding (SILValue, BottomUpState) from
    // SuccRegion. If we fail to do so, blot this SILValue and continue.
    //
    // Since we are already initialized by initSuccBottomUp(), this has the
    // effect of an intersection.
    auto Other = SuccRegionState.PtrToBottomUpState.find(RefCountedValue);
    if (Other == SuccRegionState.PtrToBottomUpState.end()) {
      PtrToBottomUpState.blot(RefCountedValue);
      continue;
    }

    SILValue OtherRefCountedValue = Other->first;

    // If the other ref count value was blotted, blot our value and continue.
    // This has the effect of an intersection since we already checked earlier
    // that RefCountedValue was not blotted.
    if (!OtherRefCountedValue) {
      PtrToBottomUpState.blot(RefCountedValue);
      continue;
    }

    BottomUpRefCountState &RefCountState = Pair.second;
    BottomUpRefCountState &OtherRefCountState = Other->second;

    // Ok, now we know that the merged set can safely represent a set of
    // of instructions which together semantically act as one ref count
    // increment. Merge the two states together.
    if (!RefCountState.merge(OtherRefCountState)) {
      PtrToBottomUpState.blot(RefCountedValue);
    }
  }
}

//===---
// Top Down Merge
//

/// Initialize the state for this Region with the state of its predecessor
/// Region. Used to create an initial state before we merge in other
/// predecessors.
void ARCRegionState::initPredTopDown(ARCRegionState &PredRegionState) {
  PtrToTopDownState = PredRegionState.PtrToTopDownState;
}

/// Merge in the state of the predecessor basic block.
void ARCRegionState::mergePredTopDown(ARCRegionState &PredRegionState) {
  // For each [(SILValue, TopDownState)] that we are tracking...
  for (std::pair<SILValue, TopDownRefCountState> &Pair : getTopDownStates()) {
    SILValue RefCountedValue = Pair.first;

    // If our SILValue was blotted, skip it. This will be ignored in the rest of
    // the optimizer.
    if (!RefCountedValue)
      continue;

    // Then attempt to lookup the corresponding (SILValue, TopDownState) from
    // PredRegion. If we fail to do so, blot this SILValue and continue.
    //
    // Since we are already initialized by initPredTopDown(), this has the
    // effect of an intersection.
    auto Other = PredRegionState.PtrToTopDownState.find(RefCountedValue);
    if (Other == PredRegionState.PtrToTopDownState.end()) {
      PtrToTopDownState.blot(RefCountedValue);
      continue;
    }

    SILValue OtherRefCountedValue = Other->first;

    // If the other ref count value was blotted, blot our value and continue.
    // This has the effect of an intersection.
    if (!OtherRefCountedValue) {
      PtrToTopDownState.blot(RefCountedValue);
      continue;
    }

    // Ok, so now we know that the ref counted value we are tracking was not
    // blotted on either side. Grab the states.
    TopDownRefCountState &RefCountState = Pair.second;
    TopDownRefCountState &OtherRefCountState = Other->second;

    // Attempt to merge Other into this ref count state. If we fail, blot this
    // ref counted value and continue.
    if (!RefCountState.merge(OtherRefCountState)) {
      DEBUG(llvm::dbgs() << "Failed to merge!\n");
      PtrToTopDownState.blot(RefCountedValue);
      continue;
    }

    DEBUG(llvm::dbgs() << "            Partial: "
                       << (RefCountState.isPartial() ? "yes" : "no") << "\n");
  }
}

//===---
// Bottom Up Block Visitor
//

bool ARCRegionState::processBottomUp(
    AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    bool FreezeOwnedArgEpilogueReleases,
    ConsumedArgToEpilogueReleaseMatcher &ConsumedArgToReleaseMap,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap) {
  const LoopRegion *R = getRegion();

  // We only process basic blocks for now. This ensures that we always propagate
  // the empty set from loops.
  if (!R->isBlock())
    return false;

  DEBUG(llvm::dbgs() << ">>>> Bottom Up!\n");
  SILBasicBlock &BB = *R->getBlock();

  bool NestingDetected = false;

  BottomUpDataflowRCStateVisitor<ARCRegionState> DataflowVisitor(
      RCIA, *this, FreezeOwnedArgEpilogueReleases, ConsumedArgToReleaseMap,
      IncToDecStateMap);

  // For each terminator instruction I in BB visited in reverse...
  for (auto II = std::next(BB.rbegin()), IE = BB.rend(); II != IE;) {
    SILInstruction &I = *II;
    ++II;

    DEBUG(llvm::dbgs() << "VISITING:\n    " << I);

    auto Result = DataflowVisitor.visit(&I);

    // If this instruction can have no further effects on another instructions,
    // continue. This happens for instance if we have cleared all of the state
    // we are tracking.
    if (Result.Kind == RCStateTransitionDataflowResultKind::NoEffects)
      continue;

    // Make sure that we propagate out whether or not nesting was detected.
    NestingDetected |= Result.NestingDetected;

    // This SILValue may be null if we were unable to find a specific RCIdentity
    // that the instruction "visits".
    SILValue Op = Result.RCIdentity;

    // For all other (reference counted value, ref count state) we are
    // tracking...
    for (auto &OtherState : getBottomupStates()) {
      // If this is the state associated with the instruction that we are
      // currently visiting, bail.
      if (Op && OtherState.first == Op)
        continue;

      // If the other state's value is blotted, skip it.
      if (!OtherState.first)
        continue;

      // If this state is not tracking anything, skip it.
      if (!OtherState.second.isTrackingRefCount())
        continue;

      // Check if the instruction we are visiting could potentially use our
      // instruction in a way that requires us to guarantee the lifetime of the
      // pointer up to this point. This has the effect of performing a use and a
      // decrement.
      if (OtherState.second.handlePotentialGuaranteedUser(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Guaranteed Use:\n        "
                           << OtherState.second.getRCRoot());
        continue;
      }

      // Check if the instruction we are visiting could potentially decrement
      // the reference counted value we are tracking... in a manner that could
      // cause us to change states. If we do change states continue...
      if (OtherState.second.handlePotentialDecrement(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
                           << OtherState.second.getRCRoot());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      if (OtherState.second.handlePotentialUser(&I, AA))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        "
                           << OtherState.second.getRCRoot());
    }
  }

  return NestingDetected;
}

//===---
// Top Down Block Visitor
//

bool ARCRegionState::processTopDown(
    AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap) {
  const LoopRegion *R = getRegion();

  // We only process basic blocks for now. This ensures that we always propagate
  // the empty set from loops.
  if (!R->isBlock())
    return false;

  DEBUG(llvm::dbgs() << ">>>> Top Down!\n");

  SILBasicBlock &BB = *R->getBlock();

  bool NestingDetected = false;

  TopDownDataflowRCStateVisitor<ARCRegionState> DataflowVisitor(
      RCIA, *this, DecToIncStateMap);

  // If the current BB is the entry BB, initialize a state corresponding to each
  // of its owned parameters. This enables us to know that if we see a retain
  // before any decrements that the retain is known safe.
  //
  // We do not handle guaranteed parameters here since those are handled in the
  // code in GlobalARCPairingAnalysis. This is because even if we don't do
  // anything, we will still pair the retain, releases and then the guaranteed
  // parameter will ensure it is known safe to remove them.
  if (BB.isEntry()) {
    auto Args = BB.getBBArgs();
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      DataflowVisitor.visit(Args[i]);
    }
  }

  // For each instruction I in BB...
  for (auto &I : BB) {

    DEBUG(llvm::dbgs() << "VISITING:\n    " << I);

    auto Result = DataflowVisitor.visit(&I);

    // If this instruction can have no further effects on another instructions,
    // continue. This happens for instance if we have cleared all of the state
    // we are tracking.
    if (Result.Kind == RCStateTransitionDataflowResultKind::NoEffects)
      continue;

    // Make sure that we propagate out whether or not nesting was detected.
    NestingDetected |= Result.NestingDetected;

    // This SILValue may be null if we were unable to find a specific RCIdentity
    // that the instruction "visits".
    SILValue Op = Result.RCIdentity;

    // For all other [(SILValue, TopDownState)] we are tracking...
    for (auto &OtherState : getTopDownStates()) {
      // If we visited an increment or decrement successfully (and thus Op is
      // set), if this is the state for this operand, skip it. We already
      // processed it.
      if (Op && OtherState.first == Op)
        continue;

      // If the other state's value is blotted, skip it.
      if (!OtherState.first)
        continue;

      // If the other state is not tracking anything, bail.
      if (!OtherState.second.isTrackingRefCount())
        continue;

      // Check if the instruction we are visiting could potentially use our
      // instruction in a way that requires us to guarantee the lifetime of the
      // pointer up to this point. This has the effect of performing a use and a
      // decrement.
      if (OtherState.second.handlePotentialGuaranteedUser(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Guaranteed Use:\n        "
                           << OtherState.second.getRCRoot());
        continue;
      }

      // Check if the instruction we are visiting could potentially decrement
      // the reference counted value we are tracking in a manner that could
      // cause us to change states. If we do change states continue...
      if (OtherState.second.handlePotentialDecrement(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
                           << OtherState.second.getRCRoot());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      if (OtherState.second.handlePotentialUser(&I, AA))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        "
                           << OtherState.second.getRCRoot());
    }
  }

  return NestingDetected;
}
