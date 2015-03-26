//===- GlobalARCSequenceDataflow.h - ARC Sequence Flow Analysis -*- C++ -*-===//
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

#ifndef SWIFT_SILANALYSIS_GLOBALARCSEQUENCEDATAFLOW_H
#define SWIFT_SILANALYSIS_GLOBALARCSEQUENCEDATAFLOW_H

#include "RefCountState.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/NullablePtr.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Optional.h"

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
  RCIdentityAnalysis *RCIA;

  /// The map from dataflow terminating decrements -> increment dataflow state.
  BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap;

  /// The map from dataflow terminating increment -> decrement dataflow state.
  BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap;

  /// Stashed BB information.
  ARCBBStateInfo *BBStateInfo;

  ConsumedArgToEpilogueReleaseMatcher ConsumedArgToReleaseMap;

public:
  ARCSequenceDataflowEvaluator(
      SILFunction &F, AliasAnalysis *AA, PostOrderAnalysis *POA,
      RCIdentityAnalysis *RCIA,
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

  void computePostDominatingConsumedArgMap();

  llvm::Optional<ARCBBStateInfoHandle> getBottomUpBBState(SILBasicBlock *BB);
  llvm::Optional<ARCBBStateInfoHandle> getTopDownBBState(SILBasicBlock *BB);
};

} // end swift namespace

#endif
