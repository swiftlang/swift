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

#include "ReferenceCountState.h"
#include "BlotMapVector.h"

namespace swift {

class SILFunction;
class AliasAnalysis;

} // end swift namespace

namespace swift {
namespace arc {

/// \brief Per-BasicBlock state.
class ARCBBState {
public:
  using TopDownMapTy = BlotMapVector<SILValue, TopDownRefCountState>;
  using BottomUpMapTy = BlotMapVector<SILValue, BottomUpRefCountState>;

private:
  /// The basic block that this bbstate corresponds to.
  SILBasicBlock *BB;

  /// The top-down traversal uses this to record information known about a
  /// pointer at the bottom of each block.
  TopDownMapTy PtrToTopDownState;

  /// The bottom-up traversal uses this to record information known about a
  /// pointer at the top of each block.
  BottomUpMapTy PtrToBottomUpState;

public:
  ARCBBState() : BB() {}
  ARCBBState(SILBasicBlock *BB) : BB(BB) {}

  void init(SILBasicBlock *NewBB) {
    assert(NewBB && "Cannot set NewBB to a nullptr.");
    BB = NewBB;
  }

  /// Top Down Iterators
  using topdown_iterator = TopDownMapTy::iterator;
  using topdown_const_iterator = TopDownMapTy::const_iterator;
  topdown_iterator topdown_begin() { return PtrToTopDownState.begin(); }
  topdown_iterator topdown_end() { return PtrToTopDownState.end(); }
  topdown_const_iterator topdown_begin() const {
    return PtrToTopDownState.begin();
  }
  topdown_const_iterator topdown_end() const { return PtrToTopDownState.end(); }
  Range<topdown_iterator> getTopDownStates() {
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
  Range<bottomup_iterator> getBottomupStates() {
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

  void clearTopDownState() { PtrToTopDownState.clear(); }

  void clearBottomUpState() { PtrToBottomUpState.clear(); }

  /// Clear both the bottom up *AND* top down state.
  void clear() {
    clearTopDownState();
    clearBottomUpState();
  }

  /// Returns a reference to the basic block that we are tracking.
  SILBasicBlock &getBB() const { return *BB; }

  /// Merge in the state of the successor basic block. This is currently a stub.
  void mergeSuccBottomUp(ARCBBState &SuccBB);

  /// Initialize this BB with the state of the successor basic block. This is
  /// called on a basic block's state and then any other successors states are
  /// merged in. This is currently a stub.
  void initSuccBottomUp(ARCBBState &SuccBB);

  /// Merge in the state of the predecessor basic block. This is currently a stub.
  void mergePredTopDown(ARCBBState &PredBB);

  /// Initialize the state for this BB with the state of its predecessor
  /// BB. Used to create an initial state before we merge in other
  /// predecessors. This is currently a stub.
  void initPredTopDown(ARCBBState &PredBB);
};
bool performARCSequenceDataflow(
    SILFunction &F, AliasAnalysis *AA,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap);

} // end arc namespace
} // end swift namespace
