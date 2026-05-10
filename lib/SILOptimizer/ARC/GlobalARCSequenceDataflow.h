//===- GlobalARCSequenceDataflow.h - ARC Sequence Flow Analysis -*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_GLOBALARCSEQUENCEDATAFLOW_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_GLOBALARCSEQUENCEDATAFLOW_H

#include "RefCountState.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/Analysis/ProgramTerminationAnalysis.h"
#include "llvm/ADT/MapVector.h"
#include <optional>

namespace swift {

class SILFunction;
class AliasAnalysis;

} // end swift namespace

namespace swift {

/// A class that implements the ARC sequence data flow.
class ARCSequenceDataflowEvaluator {
public:
  // Forward declaration of private classes that are opaque in this header.
  class ARCBBStateInfoHandle;
  class ARCBBStateInfo;
  class ARCBBState;

private:
  /// The SILFunction that we are applying the dataflow to.
  SILFunction &F;

  /// The alias analysis that we are using for alias queries.
  AliasAnalysis *AA;

  /// The post order analysis we are using for computing post orders, reverse
  /// post orders.
  PostOrderAnalysis *POA;

  /// An analysis which computes the identity root of a SILValue(), i.e. the
  /// dominating origin SILValue of the reference count that by retaining or
  /// releasing this value one is affecting.
  RCIdentityFunctionInfo *RCIA;

  /// An analysis to get the epilogue ARC instructions. 
  EpilogueARCFunctionInfo *EAFI;

  /// The map from dataflow terminating decrements -> increment dataflow state.
  BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap;

  /// The map from dataflow terminating increment -> decrement dataflow state.
  BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap;

  llvm::BumpPtrAllocator Allocator;
  ImmutablePointerSetFactory<SILInstruction *> SetFactory;

  /// Stashed BB information.
  std::unique_ptr<ARCBBStateInfo> BBStateInfo;

public:
  ARCSequenceDataflowEvaluator(
      SILFunction &F, AliasAnalysis *AA, PostOrderAnalysis *POA,
      RCIdentityFunctionInfo *RCIA, EpilogueARCFunctionInfo *EAFI,
      ProgramTerminationFunctionInfo *PTFI,
      BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
      BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap);
  ~ARCSequenceDataflowEvaluator();

  /// Run the dataflow evaluator.
  bool run(bool FreezePostDomReleases);

  /// Clear all of the states we are tracking for the various basic blocks.
  void clear();

  SILFunction *getFunction() const { return &F; }

private:
  /// Perform the bottom up data flow.
  bool processBottomUp(bool freezePostDomReleases);

  /// Perform the top down dataflow.
  bool processTopDown();

  /// Merge in the BottomUp state of any successors of DataHandle.getBB() into
  /// DataHandle.getState().
  void mergeSuccessors(ARCBBStateInfoHandle &DataHandle);

  /// Merge in the TopDown state of any predecessors of DataHandle.getBB() into
  /// DataHandle.getState().
  void mergePredecessors(ARCBBStateInfoHandle &DataHandle);

  bool processBBBottomUp(ARCBBState &BBState,
                         bool FreezeOwnedArgEpilogueReleases);
  bool processBBTopDown(ARCBBState &BBState);
  void computePostDominatingConsumedArgMap();

  std::optional<ARCBBStateInfoHandle> getBottomUpBBState(SILBasicBlock *BB);
  std::optional<ARCBBStateInfoHandle> getTopDownBBState(SILBasicBlock *BB);

  void dumpDataflowResults();
};

} // end swift namespace

#endif
