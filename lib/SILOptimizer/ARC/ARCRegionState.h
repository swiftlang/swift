//===--- ARCRegionState.h ---------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_ARCREGIONSTATE_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_ARCREGIONSTATE_H

#include "GlobalLoopARCSequenceDataflow.h"
#include "swift/Basic/NullablePtr.h"

namespace swift {

class LoopRegion;
class LoopRegionFunctionInfo;
class AliasAnalysis;
class RCIdentityFunctionInfo;

/// Per-Region state.
class ARCRegionState {
public:
  // TODO: These are relatively expensive, find something else to use here.
  using TopDownMapTy = SmallBlotMapVector<SILValue, TopDownRefCountState, 4>;
  using BottomUpMapTy = SmallBlotMapVector<SILValue, BottomUpRefCountState, 4>;

private:
  /// The region that this ARCRegionState summarizes information for.
  ///
  /// The only time that the pointer is null is during initialization. Using
  /// NullablePtr is just a convenient way to make sure that we assert if we
  /// attempt to use Region during initialization before the pointer is set.
  NullablePtr<LoopRegion> Region;

  /// The top-down traversal uses this to record information known about a
  /// pointer at the bottom of each block.
  TopDownMapTy PtrToTopDownState;

  /// The bottom-up traversal uses this to record information known about a
  /// pointer at the top of each block.
  BottomUpMapTy PtrToBottomUpState;

  /// Is this a region from which we can leak ARC values?
  ///
  /// If we know that the program has entered a state from which it is
  /// guaranteed to terminate soon, in our model we allow for all memory to be
  /// leaked since the operating system will soon reclaim the memory. We take
  /// advantage of this to ignore control flow.
  bool AllowsLeaks;

  /// A list of instructions contained in this region that can either use or
  /// decrement reference counts.
  ///
  /// This is flow insensitive since we just add all of the potential
  /// users/decrements in subregions without caring if there is only one along a
  /// path. This is for simplicity in the first iteration.
  ///
  /// TODO: This needs a better name.
  llvm::SmallVector<SILInstruction *, 4> SummarizedInterestingInsts;

public:
  ARCRegionState(LoopRegion *R, bool AllowsLeaks);

  /// Is this Region from which we can leak memory safely?
  bool allowsLeaks() const { return AllowsLeaks; }

  /// Return the region associated with this ARCRegionState.
  ///
  /// Even though Region is a NullablePtr, it is only null during
  /// initialization. This method should not be called then.
  const LoopRegion *getRegion() const { return Region.get(); }
  LoopRegion *getRegion() { return Region.get(); }

  /// Top Down Iterators
  using topdown_iterator = TopDownMapTy::iterator;
  using topdown_const_iterator = TopDownMapTy::const_iterator;
  topdown_iterator topdown_begin() { return PtrToTopDownState.begin(); }
  topdown_iterator topdown_end() { return PtrToTopDownState.end(); }
  topdown_const_iterator topdown_begin() const {
    return PtrToTopDownState.begin();
  }
  topdown_const_iterator topdown_end() const { return PtrToTopDownState.end(); }
  iterator_range<topdown_iterator> getTopDownStates() {
    return make_range(topdown_begin(), topdown_end());
  }

  /// Bottom up iteration.
  using bottomup_iterator = BottomUpMapTy::iterator;
  using bottomup_const_iterator = BottomUpMapTy::const_iterator;
  bottomup_iterator bottomup_begin() { return PtrToBottomUpState.begin(); }
  bottomup_iterator bottomup_end() { return PtrToBottomUpState.end(); }
  bottomup_const_iterator bottomup_begin() const {
    return PtrToBottomUpState.begin();
  }
  bottomup_const_iterator bottomup_end() const {
    return PtrToBottomUpState.end();
  }
  iterator_range<bottomup_iterator> getBottomupStates() {
    return make_range(bottomup_begin(), bottomup_end());
  }

  /// Attempt to find the PtrState object describing the top down state for
  /// pointer Arg. Return a new initialized PtrState describing the top down
  /// state for Arg if we do not find one.
  TopDownRefCountState &getTopDownRefCountState(SILValue Ptr) {
    return PtrToTopDownState[Ptr];
  }

  /// Attempt to find the PtrState object describing the bottom up state for
  /// pointer Arg. Return a new initialized PtrState describing the bottom up
  /// state for Arg if we do not find one.
  BottomUpRefCountState &getBottomUpRefCountState(SILValue Ptr) {
    return PtrToBottomUpState[Ptr];
  }

  /// Blot \p Ptr.
  void clearBottomUpRefCountState(SILValue Ptr) {
    PtrToBottomUpState.erase(Ptr);
  }

  /// Blot \p Ptr.
  void clearTopDownRefCountState(SILValue Ptr) { PtrToTopDownState.erase(Ptr); }

  void clearTopDownState() { PtrToTopDownState.clear(); }
  void clearBottomUpState() { PtrToBottomUpState.clear(); }

  /// Clear both the bottom up *AND* top down state.
  void clear() {
    clearTopDownState();
    clearBottomUpState();
  }

  using const_reverse_summarizedinterestinginsts_iterator =
      decltype(SummarizedInterestingInsts)::const_reverse_iterator;
  const_reverse_summarizedinterestinginsts_iterator
  summarizedinterestinginsts_rbegin() const {
    return SummarizedInterestingInsts.rbegin();
  }
  const_reverse_summarizedinterestinginsts_iterator
  summarizedinterestinginsts_rend() const {
    return SummarizedInterestingInsts.rend();
  }

  using const_summarizedinterestinginsts_iterator =
      decltype(SummarizedInterestingInsts)::const_iterator;
  const_summarizedinterestinginsts_iterator
  summarizedinterestinginsts_begin() const {
    return SummarizedInterestingInsts.begin();
  }
  const_summarizedinterestinginsts_iterator
  summarizedinterestinginsts_end() const {
    return SummarizedInterestingInsts.end();
  }
  iterator_range<const_summarizedinterestinginsts_iterator>
  getSummarizedInterestingInsts() const {
    return {summarizedinterestinginsts_begin(),
            summarizedinterestinginsts_end()};
  }

  /// Merge in the state of the successor basic block. This is currently a stub.
  void mergeSuccBottomUp(ARCRegionState &SuccRegion);

  /// Initialize this Region with the state of the successor basic block. This
  /// is
  /// called on a basic block's state and then any other successors states are
  /// merged in. This is currently a stub.
  void initSuccBottomUp(ARCRegionState &SuccRegion);

  /// Merge in the state of the predecessor basic block. This is currently a
  /// stub.
  void mergePredTopDown(ARCRegionState &PredRegion);

  /// Initialize the state for this Region with the state of its predecessor
  /// Region. Used to create an initial state before we merge in other
  /// predecessors. This is currently a stub.
  void initPredTopDown(ARCRegionState &PredRegion);

  /// If this region is a block, process all instructions top down. Otherwise,
  /// apply the summarized top down information to the merged top down
  /// state. Returns true if nested retains were detected while visiting
  /// instructions. Returns false otherwise.
  bool processTopDown(
      AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
      LoopRegionFunctionInfo *LRFI,
      llvm::DenseSet<SILInstruction *> &UnmatchedRefCountInsts,
      BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
      llvm::DenseMap<const LoopRegion *, ARCRegionState *> &LoopRegionState,
      ImmutablePointerSetFactory<SILInstruction *> &SetFactory);

  /// If this region is a block, process all instructions bottom up. Otherwise,
  /// apply the summarized bottom up information to the merged bottom up
  /// state. Returns true if nested releases were detected while visiting
  /// instructions. Returns false otherwise.
  bool processBottomUp(
      AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
      EpilogueARCFunctionInfo *EAFI, LoopRegionFunctionInfo *LRFI,
      bool FreezeOwnedArgEpilogueReleases,
      llvm::DenseSet<SILInstruction *> &UnmatchedRefCountInsts,
      BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap,
      llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo,
      ImmutablePointerSetFactory<SILInstruction *> &SetFactory);

  void summarizeBlock(SILBasicBlock *BB);

  void summarize(
      LoopRegionFunctionInfo *LRFI,
      llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo);

  /// Add \p I to the interesting instruction list of this region if it is a
  /// block. We assume that I is an instruction in the block.
  void addInterestingInst(SILInstruction *I);

  /// Remove \p I from the interesting instruction list of this region if it is
  /// a block. We assume that I is an instruction in the block.
  void removeInterestingInst(SILInstruction *I);

private:
  bool processBlockBottomUp(
      const LoopRegion *R, AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
      EpilogueARCFunctionInfo *EAFI, LoopRegionFunctionInfo *LRFI,
      bool FreezeOwnedArgEpilogueReleases,
      BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap,
      ImmutablePointerSetFactory<SILInstruction *> &SetFactory);
  bool processLoopBottomUp(
      const LoopRegion *R, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI,
      RCIdentityFunctionInfo *RCIA,
      llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo,
      llvm::DenseSet<SILInstruction *> &UnmatchedRefCountInsts);

  bool processBlockTopDown(
      SILBasicBlock &BB, AliasAnalysis *AA, RCIdentityFunctionInfo *RCIA,
      BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
      ImmutablePointerSetFactory<SILInstruction *> &SetFactory);
  bool processLoopTopDown(
      const LoopRegion *R, ARCRegionState *State, AliasAnalysis *AA,
      LoopRegionFunctionInfo *LRFI, RCIdentityFunctionInfo *RCIA,
      llvm::DenseSet<SILInstruction *> &UnmatchedRefCountInsts);

  void summarizeLoop(
      const LoopRegion *R, LoopRegionFunctionInfo *LRFI,
      llvm::DenseMap<const LoopRegion *, ARCRegionState *> &RegionStateInfo);
};

} // end swift namespace

#endif
