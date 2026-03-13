//===--- GlobalLoopARCSequenceDataflow.h ------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_GLOBALLOOPARCSEQUENCEDATAFLOW_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_GLOBALLOOPARCSEQUENCEDATAFLOW_H

#include "RefCountState.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/SILOptimizer/Analysis/LoopRegionAnalysis.h"
#include "swift/SILOptimizer/Analysis/ProgramTerminationAnalysis.h"
#include "llvm/ADT/MapVector.h"
#include <optional>

namespace swift {

class SILFunction;
class AliasAnalysis;

} // end swift namespace

namespace swift {

// Forward declaration of private classes that are opaque in this header.
class ARCRegionState;

/// A class that implements the ARC sequence data flow.
class LoopARCSequenceDataflowEvaluator {
  /// The bump ptr allocator that is used to allocate memory in the allocator.
  llvm::BumpPtrAllocator Allocator;

  /// The factory that we use to generate immutable pointer sets.
  ImmutablePointerSetFactory<SILInstruction *> SetFactory;

  /// The SILFunction that we are applying the dataflow to.
  SILFunction &F;

  /// The alias analysis that we are using for alias queries.
  AliasAnalysis *AA;

  /// Loop region information that we use to perform dataflow up and down the
  /// loop nest.
  LoopRegionFunctionInfo *LRFI;

  /// The loop info we use for convenience to seed our traversals.
  SILLoopInfo *SLI;

  /// An analysis which computes the identity root of a SILValue(), i.e. the
  /// dominating origin SILValue of the reference count that by retaining or
  /// releasing this value one is affecting.
  RCIdentityFunctionInfo *RCFI;

  /// An analysis to get the epilogue ARC instructions. 
  EpilogueARCFunctionInfo *EAFI;

  /// The map from dataflow terminating decrements -> increment dataflow state.
  BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap;

  /// The map from dataflow terminating increment -> decrement dataflow state.
  BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap;

  /// Stashed information for each region.
  llvm::DenseMap<const LoopRegion *, ARCRegionState *> RegionStateInfo;

  /// Set of unmatched RefCountInsts
  llvm::DenseSet<SILInstruction *> UnmatchedRefCountInsts;

public:
  LoopARCSequenceDataflowEvaluator(
      SILFunction &F, AliasAnalysis *AA, LoopRegionFunctionInfo *LRFI,
      SILLoopInfo *SLI, RCIdentityFunctionInfo *RCIA,
      EpilogueARCFunctionInfo *EAFI,
      ProgramTerminationFunctionInfo *PTFI,
      BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
      BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap);
  ~LoopARCSequenceDataflowEvaluator();

  SILFunction *getFunction() const { return &F; }

  /// Clear all of the state associated with the given loop.
  void clearLoopState(const LoopRegion *R);

  /// Perform the sequence dataflow, bottom up and top down on the loop region
  /// \p R.
  bool runOnLoop(const LoopRegion *R, bool FreezeOwnedArgEpilogueReleases,
                 bool RecomputePostDomReleases);

  /// Summarize the subregions of \p R that are blocks.
  ///
  /// We assume that all subregions that are loops have already been summarized
  /// since we are processing bottom up through the loop nest hierarchy.
  void summarizeSubregionBlocks(const LoopRegion *R);

  /// Summarize the contents of the loop so that loops further up the loop tree
  /// can reason about the loop.
  void summarizeLoop(const LoopRegion *R);

  /// Add \p I to the interesting instruction list of its parent block.
  void addInterestingInst(SILInstruction *I);

  /// Remove \p I from the interesting instruction list of its parent block.
  void removeInterestingInst(SILInstruction *I);

  /// Compute if a RefCountInst was unmatched and populate the persistent
  /// UnmatchedRefCountInsts set.
  void saveMatchingInfo(const LoopRegion *R);

  /// Clear the folding node set of the set factory we have stored internally.
  void clearSetFactory() {
    SetFactory.clear();
  }

private:
  /// Merge in the BottomUp state of any successors of DataHandle.getBB() into
  /// DataHandle.getState().
  void mergeSuccessors(const LoopRegion *R, ARCRegionState &State);

  /// Merge in the TopDown state of any predecessors of DataHandle.getBB() into
  /// DataHandle.getState().
  void mergePredecessors(const LoopRegion *R, ARCRegionState &State);

  void computePostDominatingConsumedArgMap();

  ARCRegionState &getARCState(const LoopRegion *L) {
    auto Iter = RegionStateInfo.find(L);
    assert(Iter != RegionStateInfo.end() && "Should have created state for "
                                            "each region");
    return *Iter->second;
  }

  bool processLoopTopDown(const LoopRegion *R);
  bool processLoopBottomUp(const LoopRegion *R,
                           bool FreezeOwnedArgEpilogueReleases);

  void dumpDataflowResults();
};

} // end swift namespace

#endif
