//===--- ARCRegionState.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "arc-sequence-opts"
#include "ARCRegionState.h"
#include "RCStateTransitionVisitors.h"
#include "swift/Basic/Range.h"
#include "swift/SILOptimizer/Analysis/LoopRegionAnalysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                               ARCRegionState
//===----------------------------------------------------------------------===//

ARCRegionState::ARCRegionState(LoopRegion *R, bool AllowsLeaks)
    : Region(R), PtrToTopDownState(), PtrToBottomUpState(),
      AllowsLeaks(AllowsLeaks) {}

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
  for (auto &Pair : getBottomupStates()) {
    if (!Pair.hasValue())
      continue;

    SILValue RefCountedValue = Pair->first;

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

    SILValue OtherRefCountedValue = (*Other)->first;

    // If the other ref count value was blotted, blot our value and continue.
    // This has the effect of an intersection since we already checked earlier
    // that RefCountedValue was not blotted.
    if (!OtherRefCountedValue) {
      PtrToBottomUpState.blot(RefCountedValue);
      continue;
    }

    BottomUpRefCountState &RefCountState = Pair->second;
    BottomUpRefCountState &OtherRefCountState = (*Other)->second;

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
  for (auto &Pair : getTopDownStates()) {
    if (!Pair.hasValue())
      continue;
    SILValue RefCountedValue = Pair->first;

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

    SILValue OtherRefCountedValue = (*Other)->first;

    // If the other ref count value was blotted, blot our value and continue.
    // This has the effect of an intersection.
    if (!OtherRefCountedValue) {
      PtrToTopDownState.blot(RefCountedValue);
      continue;
    }

    // Ok, so now we know that the ref counted value we are tracking was not
    // blotted on either side. Grab the states.
    TopDownRefCountState &RefCountState = Pair->second;
    TopDownRefCountState &OtherRefCountState = (*Other)->second;

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
// Bottom Up Dataflow
//

static bool isARCSignificantTerminator(TermInst *TI) {
  switch (TI->getTermKind()) {
  case TermKind::Invalid:
    llvm_unreachable("Expected a TermInst");
  case TermKind::UnreachableInst:
  // br is a forwarding use for its arguments. It cannot in of itself extend
  // the lifetime of an object (just like a phi-node) cannot.
  case TermKind::BranchInst:
  // A cond_br is a forwarding use for its non-operand arguments in a similar
  // way to br. Its operand must be an i1 that has a different lifetime from any
  // ref counted object.
  case TermKind::CondBranchInst:
    return false;
  // Be conservative for now. These actually perform some sort of operation
  // against the operand or can use the value in some way.
  case TermKind::ThrowInst:
  case TermKind::ReturnInst:
  case TermKind::TryApplyInst:
  case TermKind::SwitchValueInst:
  case TermKind::SwitchEnumInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::DynamicMethodBranchInst:
  case TermKind::CheckedCastBranchInst:
  case TermKind::CheckedCastAddrBranchInst:
    return true;
  }
}

// Visit each one of our predecessor regions and see if any are blocks that can
// use reference counted values. If any of them do, we advance the sequence for
// the pointer and create an insertion point here. This state will be propagated
// into all of our predecessors, allowing us to be conservatively correct in all
// cases.
//
// The key thing to notice is that in general this cannot happen due to
// critical edge splitting. To trigger this, one would need a terminator that
// uses a reference counted value and only has one successor due to critical
// edge splitting. This is just to be conservative when faced with the unknown
// of future changes.
//
// We do not need to worry about loops here, since a loop exit block can only
// have predecessors in the loop itself implying that loop exit blocks at the
// loop region level always have only one predecessor, the loop itself.
void ARCRegionState::processBlockBottomUpPredTerminators(
    const LoopRegion *R, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI) {
  auto &BB = *R->getBlock();
  llvm::TinyPtrVector<SILInstruction *> PredTerminators;
  for (unsigned PredID : R->getPreds()) {
    auto *PredRegion = LRFI->getRegion(PredID);
    if (!PredRegion->isBlock())
      continue;

    auto *TermInst = PredRegion->getBlock()->getTerminator();
    if (!isARCSignificantTerminator(TermInst))
      continue;
    PredTerminators.push_back(TermInst);
  }

  auto *InsertPt = &*BB.begin();
  for (auto &OtherState : getBottomupStates()) {
    // If the other state's value is blotted, skip it.
    if (!OtherState.hasValue())
      continue;

    OtherState->second.updateForPredTerminators(PredTerminators, InsertPt, AA);
  }
}

bool ARCRegionState::processBlockBottomUp(
    const LoopRegion *R, AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    LoopRegionFunctionInfo *LRFI, bool FreezeOwnedArgEpilogueReleases,
    ConsumedArgToEpilogueReleaseMatcher &ConsumedArgToReleaseMap,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap) {
  DEBUG(llvm::dbgs() << ">>>> Bottom Up!\n");

  SILBasicBlock &BB = *R->getBlock();
  bool NestingDetected = false;

  BottomUpDataflowRCStateVisitor<ARCRegionState> DataflowVisitor(
      RCIA, *this, FreezeOwnedArgEpilogueReleases, ConsumedArgToReleaseMap,
      IncToDecStateMap);

  // For each non-terminator instruction I in BB visited in reverse...
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

    auto *InsertPt = &*std::next(SILBasicBlock::iterator(&I));

    // For all other (reference counted value, ref count state) we are
    // tracking...
    for (auto &OtherState : getBottomupStates()) {
      // If the other state's value is blotted, skip it.
      if (!OtherState.hasValue())
        continue;

      // If this is the state associated with the instruction that we are
      // currently visiting, bail.
      if (Op && OtherState->first == Op)
        continue;

      OtherState->second.updateForSameLoopInst(&I, InsertPt, AA);
    }
  }

  // Now visit each one of our predecessor regions and see if any are blocks
  // that can use reference counted values. If any of them do, we advance the
  // sequence for the pointer and create an insertion point here. This state
  // will be propagated into all of our predecessors, allowing us to be
  // conservatively correct in all cases.
  processBlockBottomUpPredTerminators(R, AA, LRFI);

  return NestingDetected;
}

// Find the relevant insertion points for the loop region R in its
// successors. Returns true if we succeeded. Returns false if any of the
// non-local successors of the region are not leaking blocks. We currently do
// not handle early exits, but do handle trapping blocks.
static bool getInsertionPtsForLoopRegionExits(
    const LoopRegion *R, LoopRegionFunctionInfo *LRFI,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo,
    llvm::SmallVectorImpl<SILInstruction *> &InsertPts) {
  assert(R->isLoop() && "Expected a loop region that is representing a loop");

  // Go through all of our non local successors. If any of them cannot be
  // ignored, we bail for simplicity. This means that for now we do not handle
  // early exits.
  if (any_of(R->getNonLocalSuccs(), [&](unsigned SuccID) -> bool {
        return !RegionStateInfo[LRFI->getRegion(SuccID)]->allowsLeaks();
      })) {
    return false;
  }

  // We assume that all of our loops have been canonicalized so that /all/ loop
  // exit blocks only have exiting blocks as predecessors. This means that all
  // successor regions of any region /cannot/ be a region representing a loop.
  for (unsigned SuccID : R->getLocalSuccs()) {
    auto *SuccRegion = LRFI->getRegion(SuccID);
    assert(SuccRegion->isBlock() && "Loop canonicalization failed?!");
    InsertPts.push_back(&*SuccRegion->getBlock()->begin());
  }

  return true;
}

bool ARCRegionState::processLoopBottomUp(
    const LoopRegion *R, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo) {
  ARCRegionState *State = RegionStateInfo[R];

  llvm::SmallVector<SILInstruction *, 2> InsertPts;
  // Try to lookup insertion points for this region. If when checking for
  // insertion points, we find that we have non-leaking early exits, clear state
  // and bail. We do not handle these for now.
  if (!getInsertionPtsForLoopRegionExits(R, LRFI, RegionStateInfo, InsertPts)) {
    clearBottomUpState();
    return false;
  }

  // For each state that we are currently tracking, apply our summarized
  // instructions to it.
  for (auto &OtherState : getBottomupStates()) {
    if (!OtherState.hasValue())
      continue;

    for (auto *I : State->getSummarizedInterestingInsts())
      OtherState->second.updateForDifferentLoopInst(I, InsertPts, AA);
  }

  return false;
}

bool ARCRegionState::processBottomUp(
    AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    LoopRegionFunctionInfo *LRFI, bool FreezeOwnedArgEpilogueReleases,
    ConsumedArgToEpilogueReleaseMatcher &ConsumedArgToReleaseMap,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo) {
  const LoopRegion *R = getRegion();

  // We only process basic blocks for now. This ensures that we always propagate
  // the empty set from loops.
  if (!R->isBlock())
    return processLoopBottomUp(R, AA, LRFI, RegionStateInfo);

  return processBlockBottomUp(R, AA, RCIA, LRFI, FreezeOwnedArgEpilogueReleases,
                              ConsumedArgToReleaseMap, IncToDecStateMap);
}

//===---
// Top Down Dataflow
//

bool ARCRegionState::processBlockTopDown(
    SILBasicBlock &BB, AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap) {
  DEBUG(llvm::dbgs() << ">>>> Top Down!\n");

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
      // If the other state's value is blotted, skip it.
      if (!OtherState.hasValue())
        continue;

      // If we visited an increment or decrement successfully (and thus Op is
      // set), if this is the state for this operand, skip it. We already
      // processed it.
      if (Op && OtherState->first == Op)
        continue;

      OtherState->second.updateForSameLoopInst(&I, &I, AA);
    }
  }

  return NestingDetected;
}

bool ARCRegionState::processLoopTopDown(const LoopRegion *R,
                                        ARCRegionState *State,
                                        AliasAnalysis *AA,
                                        LoopRegionFunctionInfo *LRFI) {

  assert(R->isLoop() && "We assume we are processing a loop");

  // If we have more than 2 predecessors, we do not have a pre-header. We do not
  // support this case since canonicalization failed.
  if (R->pred_size() != 1) {
    clearTopDownState();
    return false;
  }

  auto *PredRegion = LRFI->getRegion(*R->pred_begin());
  assert(PredRegion->isBlock() && "Expected the predecessor region to be a "
                                  "block");

  // Our insert point is going to be the terminator inst.
  SILInstruction *InsertPt = PredRegion->getBlock()->getTerminator();

  // For each state that we are currently tracking, apply our summarized
  // instructions to it.
  for (auto &OtherState : getTopDownStates()) {
    if (!OtherState.hasValue())
      continue;

    for (auto *I : State->getSummarizedInterestingInsts())
      OtherState->second.updateForDifferentLoopInst(I, InsertPt, AA);
  }

  return false;
}

bool ARCRegionState::processTopDown(
    AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    LoopRegionFunctionInfo *LRFI,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo) {
  const LoopRegion *R = getRegion();

  // We only process basic blocks for now. This ensures that we always propagate
  // the empty set from loops.
  if (!R->isBlock())
    return processLoopTopDown(R, RegionStateInfo[R], AA, LRFI);

  return processBlockTopDown(*R->getBlock(), AA, RCIA, DecToIncStateMap);
}

//===---
// Summary
//

void ARCRegionState::summarizeBlock(SILBasicBlock *BB) {
  SummarizedInterestingInsts.clear();

  for (auto &I : *BB)
    if (!canNeverUseValues(&I) || I.mayReleaseOrReadRefCount())
      SummarizedInterestingInsts.push_back(&I);
}

void ARCRegionState::summarizeLoop(
    const LoopRegion *R, LoopRegionFunctionInfo *LRFI,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo) {
  SummarizedInterestingInsts.clear();
  for (unsigned SubregionID : R->getSubregions()) {
    LoopRegion *Subregion = LRFI->getRegion(SubregionID);
    ARCRegionState *SubregionState = RegionStateInfo[Subregion];
    std::copy(SubregionState->summarizedinterestinginsts_begin(),
              SubregionState->summarizedinterestinginsts_end(),
              std::back_inserter(SummarizedInterestingInsts));
  }
}

void ARCRegionState::summarize(
    LoopRegionFunctionInfo *LRFI,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo) {
  const LoopRegion *R = getRegion();

  // We do not need to summarize a function since it is the outermost loop.
  if (R->isFunction())
    return;

  assert(R->isLoop() && "Expected to be called on a loop");
  // Make sure that all subregions that are blocked are summarized. We know that
  // all subloops have already been summarized.
  for (unsigned SubregionID : R->getSubregions()) {
    auto *Subregion = LRFI->getRegion(SubregionID);
    if (!Subregion->isBlock())
      continue;
    auto *SubregionState = RegionStateInfo[Subregion];
    SubregionState->summarizeBlock(Subregion->getBlock());
  }

  summarizeLoop(R, LRFI, RegionStateInfo);
}
