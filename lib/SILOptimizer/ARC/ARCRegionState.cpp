//===--- ARCRegionState.cpp -----------------------------------------------===//
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
  }
}

//===---
// Bottom Up Dataflow
//

static bool isARCSignificantTerminator(TermInst *TI) {
  switch (TI->getTermKind()) {
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
  case TermKind::CheckedCastValueBranchInst:
  case TermKind::CheckedCastAddrBranchInst:
    return true;
  }

  llvm_unreachable("Unhandled TermKind in switch.");
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
    const LoopRegion *R, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI,
    ImmutablePointerSetFactory<SILInstruction> &SetFactory) {
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

  for (auto &OtherState : getBottomupStates()) {
    // If the other state's value is blotted, skip it.
    if (!OtherState.hasValue())
      continue;

    OtherState->second.updateForPredTerminators(PredTerminators,
                                                SetFactory, AA);
  }
}

static bool processBlockBottomUpInsts(
    ARCRegionState &State, SILBasicBlock &BB,
    BottomUpDataflowRCStateVisitor<ARCRegionState> &DataflowVisitor,
    AliasAnalysis *AA, ImmutablePointerSetFactory<SILInstruction> &SetFactory) {

  auto II = State.summarizedinterestinginsts_rbegin();
  auto IE = State.summarizedinterestinginsts_rend();

  // If we do not have any interesting instructions, bail and return false since
  // we can not have any nested instructions.
  if (II == IE)
    return false;

  // If II is the terminator, skip it since our terminator was already processed
  // in our successors.
  if (*II == BB.getTerminator())
    ++II;

  bool NestingDetected = false;
  while (II != IE) {
    SILInstruction *I = *II;
    ++II;

    DEBUG(llvm::dbgs() << "VISITING:\n    " << *I);

    auto Result = DataflowVisitor.visit(I);

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
    for (auto &OtherState : State.getBottomupStates()) {
      // If the other state's value is blotted, skip it.
      if (!OtherState.hasValue())
        continue;

      // If this is the state associated with the instruction that we are
      // currently visiting, bail.
      if (Op && OtherState->first == Op)
        continue;

      OtherState->second.updateForSameLoopInst(I, SetFactory, AA);
    }
  }

  return NestingDetected;
}

bool ARCRegionState::processBlockBottomUp(
    const LoopRegion *R, AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    EpilogueARCFunctionInfo *EAFI, LoopRegionFunctionInfo *LRFI,
    bool FreezeOwnedArgEpilogueReleases,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap,
    ImmutablePointerSetFactory<SILInstruction> &SetFactory) {
  DEBUG(llvm::dbgs() << ">>>> Bottom Up!\n");

  SILBasicBlock &BB = *R->getBlock();
  BottomUpDataflowRCStateVisitor<ARCRegionState> DataflowVisitor(
      RCIA, EAFI, *this, FreezeOwnedArgEpilogueReleases, IncToDecStateMap,
      SetFactory);

  // Visit each non-terminator arc relevant instruction I in BB visited in
  // reverse...
  bool NestingDetected =
      processBlockBottomUpInsts(*this, BB, DataflowVisitor, AA, SetFactory);

  // Now visit each one of our predecessor regions and see if any are blocks
  // that can use reference counted values. If any of them do, we advance the
  // sequence for the pointer and create an insertion point here. This state
  // will be propagated into all of our predecessors, allowing us to be
  // conservatively correct in all cases.
  processBlockBottomUpPredTerminators(R, AA, LRFI, SetFactory);

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

  // Sort and unique the insert points so we can put them into
  // ImmutablePointerSets.
  sortUnique(InsertPts);

  return true;
}

bool ARCRegionState::processLoopBottomUp(
    const LoopRegion *R, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo,
    ImmutablePointerSetFactory<SILInstruction> &SetFactory) {
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
      OtherState->second.updateForDifferentLoopInst(I, SetFactory, AA);
  }

  return false;
}

bool ARCRegionState::processBottomUp(
    AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    EpilogueARCFunctionInfo *EAFI, LoopRegionFunctionInfo *LRFI,
    bool FreezeOwnedArgEpilogueReleases,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo,
    ImmutablePointerSetFactory<SILInstruction> &SetFactory) {
  const LoopRegion *R = getRegion();

  // We only process basic blocks for now. This ensures that we always propagate
  // the empty set from loops.
  if (!R->isBlock())
    return processLoopBottomUp(R, AA, LRFI, RegionStateInfo, SetFactory);

  return processBlockBottomUp(R, AA, RCIA, EAFI, LRFI, FreezeOwnedArgEpilogueReleases,
                              IncToDecStateMap, SetFactory);
}

//===---
// Top Down Dataflow
//

bool ARCRegionState::processBlockTopDown(
    SILBasicBlock &BB, AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
    ImmutablePointerSetFactory<SILInstruction> &SetFactory) {
  DEBUG(llvm::dbgs() << ">>>> Top Down!\n");

  bool NestingDetected = false;

  TopDownDataflowRCStateVisitor<ARCRegionState> DataflowVisitor(
      RCIA, *this, DecToIncStateMap, SetFactory);

  // If the current BB is the entry BB, initialize a state corresponding to each
  // of its owned parameters. This enables us to know that if we see a retain
  // before any decrements that the retain is known safe.
  //
  // We do not handle guaranteed parameters here since those are handled in the
  // code in GlobalARCPairingAnalysis. This is because even if we don't do
  // anything, we will still pair the retain, releases and then the guaranteed
  // parameter will ensure it is known safe to remove them.
  if (BB.isEntry()) {
    auto Args = BB.getArguments();
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      DataflowVisitor.visit(Args[i]);
    }
  }

  // For each instruction I in BB...
  for (auto *I : SummarizedInterestingInsts) {

    DEBUG(llvm::dbgs() << "VISITING:\n    " << *I);

    auto Result = DataflowVisitor.visit(I);

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

      OtherState->second.updateForSameLoopInst(I, SetFactory, AA);
    }
  }

  return NestingDetected;
}

bool ARCRegionState::processLoopTopDown(
    const LoopRegion *R, ARCRegionState *State, AliasAnalysis *AA,
    LoopRegionFunctionInfo *LRFI,
    ImmutablePointerSetFactory<SILInstruction> &SetFactory) {

  assert(R->isLoop() && "We assume we are processing a loop");

  // If we have more than 2 predecessors, we do not have a pre-header. We do not
  // support this case since canonicalization failed.
  if (R->pred_size() != 1) {
    clearTopDownState();
    return false;
  }

  auto *PredRegion = LRFI->getRegion(*R->pred_begin());
  (void) PredRegion;
  assert(PredRegion->isBlock() && "Expected the predecessor region to be a "
                                  "block");

  // For each state that we are currently tracking, apply our summarized
  // instructions to it.
  for (auto &OtherState : getTopDownStates()) {
    if (!OtherState.hasValue())
      continue;

    for (auto *I : State->getSummarizedInterestingInsts())
      OtherState->second.updateForDifferentLoopInst(I, SetFactory, AA);
  }

  return false;
}

bool ARCRegionState::processTopDown(
    AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
    LoopRegionFunctionInfo *LRFI,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
    llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo,
    ImmutablePointerSetFactory<SILInstruction> &SetFactory) {
  const LoopRegion *R = getRegion();

  // We only process basic blocks for now. This ensures that we always propagate
  // the empty set from loops.
  if (!R->isBlock())
    return processLoopTopDown(R, RegionStateInfo[R], AA, LRFI, SetFactory);

  return processBlockTopDown(*R->getBlock(), AA, RCIA, DecToIncStateMap,
                             SetFactory);
}

//===---
// Summary
//

static bool isStrongEntranceInstruction(const SILInstruction &I) {
  switch (I.getKind()) {
  case ValueKind::AllocRefInst:
  case ValueKind::AllocRefDynamicInst:
  case ValueKind::AllocBoxInst:
    return true;
  default:
    return false;
  }
}

void ARCRegionState::summarizeBlock(SILBasicBlock *BB) {
  SummarizedInterestingInsts.clear();

  for (auto &I : *BB)
    if (!canNeverUseValues(&I) || I.mayReleaseOrReadRefCount() ||
        isStrongEntranceInstruction(I))
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
  // We know that all of our sub blocks have the correct interesting insts since
  // we did one scan at the beginning and are updating our interesting inst list
  // as we move around retains/releases. Additionally since we are going through
  // the loop nest bottom up, all of our subloops have already been
  // summarized. Thus all we need to do is gather up the interesting
  // instructions from our subregions.
  summarizeLoop(R, LRFI, RegionStateInfo);
}

void ARCRegionState::addInterestingInst(SILInstruction *TargetInst) {
  // Insert I into its location in the interesting instruction list.
  SILBasicBlock *BB = getRegion()->getBlock();
  assert(TargetInst->getParent() == BB);

  auto II = BB->begin();
  auto IE = BB->end();
  assert(II != IE && "I can not be an element of an empty block");

  auto SI = SummarizedInterestingInsts.begin();
  auto SE = SummarizedInterestingInsts.end();

  while (II != IE) {
    if (SI == SE) {
      // Ok, TargetInst is after all of the interesting insts. Append it to the
      // list.
      SummarizedInterestingInsts.push_back(TargetInst);
      return;
    }

    // Move II down the block until it hits TargetInst or the first
    // SummarizedInterestingInst.
    while (&*II != *SI && &*II != TargetInst) {
      ++II;
    }

    // If II == SI and TargetInst == II then there is nothing further to do.
    if (&*II == TargetInst) {
      assert(&*II != *SI);
      SummarizedInterestingInsts.insert(SI, TargetInst);
      return;
    }

    // If we reach this point, then we know that II == SI and we have not found
    // TargetInst yet. So we move to the next II, SI.
    ++II;
    ++SI;
  }

  llvm_unreachable("Could not find Inst in the block?!");
}

void ARCRegionState::removeInterestingInst(SILInstruction *I) {
  SummarizedInterestingInsts.erase(
      std::remove(SummarizedInterestingInsts.begin(),
                  SummarizedInterestingInsts.end(), I),
      SummarizedInterestingInsts.end());
}
