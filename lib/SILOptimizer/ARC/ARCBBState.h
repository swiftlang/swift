//===--- ARCBBState.h -------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_ARCBBSTATE_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_ARC_ARCBBSTATE_H

#include "GlobalARCSequenceDataflow.h"

namespace swift {

/// Per-BasicBlock state.
class ARCSequenceDataflowEvaluator::ARCBBState {
public:
  using TopDownMapTy = SmallBlotMapVector<SILValue, TopDownRefCountState, 4>;
  using BottomUpMapTy = SmallBlotMapVector<SILValue, BottomUpRefCountState, 4>;

private:
  /// The basic block that this bbstate corresponds to.
  SILBasicBlock *BB;

  /// The top-down traversal uses this to record information known about a
  /// pointer at the bottom of each block.
  TopDownMapTy PtrToTopDownState;

  /// The bottom-up traversal uses this to record information known about a
  /// pointer at the top of each block.
  BottomUpMapTy PtrToBottomUpState;

  /// Is this a BB that is a trap?
  bool IsTrapBB;

public:
  ARCBBState() : BB() {}
  ARCBBState(SILBasicBlock *BB) : BB(BB) {}

  void init(SILBasicBlock *NewBB, bool NewIsTrapBB) {
    assert(NewBB && "Cannot set NewBB to a nullptr.");
    BB = NewBB;
    IsTrapBB = NewIsTrapBB;
  }

  /// Is this BB a BB that fits the canonical form of a trap?
  ///
  /// The canonical form of a trap is:
  ///
  ///   builtin "int_trap"() : $()
  ///   unreachable
  ///
  /// This cannot have any uses of reference counted values since the frontend
  /// just leaks at that point.
  bool isTrapBB() const { return IsTrapBB; }

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

  /// Returns a reference to the basic block that we are tracking.
  SILBasicBlock &getBB() const { return *BB; }

  /// Merge in the state of the successor basic block. This is currently a stub.
  void mergeSuccBottomUp(ARCBBState &SuccBB);

  /// Initialize this BB with the state of the successor basic block. This is
  /// called on a basic block's state and then any other successors states are
  /// merged in. This is currently a stub.
  void initSuccBottomUp(ARCBBState &SuccBB);

  /// Merge in the state of the predecessor basic block. This is currently a
  /// stub.
  void mergePredTopDown(ARCBBState &PredBB);

  /// Initialize the state for this BB with the state of its predecessor
  /// BB. Used to create an initial state before we merge in other
  /// predecessors. This is currently a stub.
  void initPredTopDown(ARCBBState &PredBB);

  void dumpBottomUpState();
  void dumpTopDownState();
};

class ARCSequenceDataflowEvaluator::ARCBBStateInfoHandle {
  friend ARCSequenceDataflowEvaluator::ARCBBStateInfo;

  SILBasicBlock *BB;
  ARCBBState &BBState;
  NullablePtr<llvm::SmallPtrSet<SILBasicBlock *, 4>> BackedgeMap;
  unsigned ID;

  ARCBBStateInfoHandle(SILBasicBlock *BB, unsigned ID, ARCBBState &BBState)
      : BB(BB), BBState(BBState), BackedgeMap(), ID(ID) {}
  ARCBBStateInfoHandle(SILBasicBlock *BB, unsigned ID, ARCBBState &BBState,
                       llvm::SmallPtrSet<SILBasicBlock *, 4> &BackedgeMap)
      : BB(BB), BBState(BBState), BackedgeMap(&BackedgeMap), ID(ID) {}

public:
  ARCBBStateInfoHandle(const ARCBBStateInfoHandle &) = default;
  ARCBBStateInfoHandle(ARCBBStateInfoHandle &&) = default;
  ;
  ~ARCBBStateInfoHandle() = default;

  SILBasicBlock *getBB() const { return BB; }
  unsigned getID() const { return ID; }
  ARCBBState &getState() { return BBState; }
  bool isBackedge(SILBasicBlock *BB) const {
    if (BackedgeMap.isNull())
      return false;
    return BackedgeMap.get()->count(BB);
  }
};

class ARCSequenceDataflowEvaluator::ARCBBStateInfo {
  /// A map from BB -> BBID. A BB's BBID is its RPOT number.
  llvm::DenseMap<SILBasicBlock *, unsigned> BBToBBIDMap;

  /// Map from a BBID to BB's bottom up dataflow state. Meant to be used in
  /// conjunction with BBToBBIDMap.
  std::vector<ARCBBState> BBIDToBottomUpBBStateMap;

  /// Map from a BBID to BB's top down dataflow state. Meant to be used in
  /// conjunction with BBToBBIDMap.
  std::vector<ARCBBState> BBIDToTopDownBBStateMap;

  /// A map mapping the head to a tail of a backedge. We only compute this once
  /// in the lifetime of this class.
  llvm::DenseMap<SILBasicBlock *, llvm::SmallPtrSet<SILBasicBlock *, 4>>
      BackedgeMap;

public:
  ARCBBStateInfo(SILFunction *F, PostOrderAnalysis *POTA,
                 ProgramTerminationFunctionInfo *PTFI);

  std::optional<ARCBBStateInfoHandle> getBottomUpBBHandle(SILBasicBlock *BB);
  std::optional<ARCBBStateInfoHandle> getTopDownBBHandle(SILBasicBlock *BB);

  void clear();

private:
  std::optional<unsigned> getBBID(SILBasicBlock *BB) const;
};

} // end swift namespace

#endif
