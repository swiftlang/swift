//===--- GlobalARCSequenceDataflow.cpp - ARC Sequence Dataflow Analysis ---===//
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
#include "GlobalARCSequenceDataflow.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILSuccessor.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

static bool isAutoreleasePoolCall(SILInstruction &I) {
  ApplyInst *AI = dyn_cast<ApplyInst>(&I);
  if (!AI)
    return false;

  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return false;

  return llvm::StringSwitch<bool>(FRI->getReferencedFunction()->getName())
      .Case("objc_autoreleasePoolPush", true)
      .Case("objc_autoreleasePoolPop", true)
      .Default(false);
}

//===----------------------------------------------------------------------===//
//                                 ARCBBState
//===----------------------------------------------------------------------===//

namespace swift {

/// \brief Per-BasicBlock state.
class ARCSequenceDataflowEvaluator::ARCBBState {
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

  /// Is this a BB that is a trap?
  bool IsTrapBB;

public:
  ARCBBState() : BB() {}
  ARCBBState(SILBasicBlock *BB) : BB(BB) {}

  void init(SILBasicBlock *NewBB) {
    assert(NewBB && "Cannot set NewBB to a nullptr.");
    BB = NewBB;
    IsTrapBB = false;
    initializeTrapStatus();
  }

  /// Is this BB a BB that fits the canonical form of a trap?
  ///
  /// The canonical form of a trap is:
  ///
  ///   builtin "int_trap"() : $()
  ///   unreachable
  ///
  /// This can not have any uses of reference counted values since the frontend
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

  /// Blot \p Ptr.
  void clearBottomUpRefCountState(SILValue Ptr) {
    PtrToBottomUpState.blot(Ptr);
  }

  /// Blot \p Ptr.
  void clearTopDownRefCountState(SILValue Ptr) {
    PtrToTopDownState.blot(Ptr);
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

private:
  void initializeTrapStatus();
};

} // end namespace swift

namespace {

using ARCBBState = ARCSequenceDataflowEvaluator::ARCBBState;

} // end anonymous namespace

/// Merge in the state of the successor basic block. This is an intersection
/// operation.
void ARCBBState::mergeSuccBottomUp(ARCBBState &SuccBBState) {
  // For each [(SILValue, BottomUpState)] that we are tracking...
  for (std::pair<SILValue, BottomUpRefCountState> &Pair : getBottomupStates()) {
    SILValue RefCountedValue = Pair.first;

    // If our SILValue was blotted, skip it. This will be ignored for the rest
    // of the ARC optimization.
    if (!RefCountedValue)
      continue;

    // Then attempt to lookup the corresponding (SILValue, BottomUpState) from
    // SuccBB. If we fail to do so, blot this SILValue and continue.
    //
    // Since we are already initialized by initSuccBottomUp(), this has the
    // effect of an intersection.
    auto Other = SuccBBState.PtrToBottomUpState.find(RefCountedValue);
    if (Other == SuccBBState.PtrToBottomUpState.end()) {
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
    RefCountState.merge(OtherRefCountState);
  }
}

/// Initialize this BB with the state of the successor basic block. This is
/// called on a basic block's state and then any other successors states are
/// merged in.
void ARCBBState::initSuccBottomUp(ARCBBState &SuccBBState) {
  PtrToBottomUpState = SuccBBState.PtrToBottomUpState;
}

/// Merge in the state of the predecessor basic block.
void ARCBBState::mergePredTopDown(ARCBBState &PredBBState) {
  // For each [(SILValue, TopDownState)] that we are tracking...
  for (std::pair<SILValue, TopDownRefCountState> &Pair : getTopDownStates()) {
    SILValue RefCountedValue = Pair.first;

    // If our SILValue was blotted, skip it. This will be ignored in the rest of
    // the optimizer.
    if (!RefCountedValue)
      continue;

    // Then attempt to lookup the corresponding (SILValue, TopDownState) from
    // PredBB. If we fail to do so, blot this SILValue and continue.
    //
    // Since we are already initialized by initPredTopDown(), this has the
    // effect of an intersection.
    auto Other = PredBBState.PtrToTopDownState.find(RefCountedValue);
    if (Other == PredBBState.PtrToTopDownState.end()) {
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

    // We create states for specific arguments when processing top down. If we
    // have one ref count state that has seen a ref count and a different one
    // that was initialized with an argument but has not seen a ref count value,
    // we don't want to merge them. Instead we blot since this is a form of
    // partial merging that we do not support.
    if (RefCountState.Argument.isNull() != OtherRefCountState.Argument.isNull()) {
      DEBUG(llvm::dbgs() << "Can not merge arg and non-arg path!\n");
      PtrToTopDownState.blot(RefCountedValue);
      continue;
    }

    // Ok, now we know that the merged set can safely represent a set of
    // of instructions which together semantically act as one ref count
    // increment. Merge the two states together.
    RefCountState.merge(OtherRefCountState);
    DEBUG(llvm::dbgs() << "                Partial: "
                       << (RefCountState.isPartial()?"yes":"no") << "\n");
  }
}

/// Initialize the state for this BB with the state of its predecessor
/// BB. Used to create an initial state before we merge in other
/// predecessors.
void ARCBBState::initPredTopDown(ARCBBState &PredBBState) {
  PtrToTopDownState = PredBBState.PtrToTopDownState;
}

void ARCBBState::initializeTrapStatus() {
  IsTrapBB = isARCInertTrapBB(BB);
}

//===----------------------------------------------------------------------===//
//                               ARCBBStateInfo
//===----------------------------------------------------------------------===//

namespace swift {

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
  ARCBBStateInfoHandle(ARCBBStateInfoHandle &&) = default;;
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
  llvm::DenseMap<SILBasicBlock *,
                 llvm::SmallPtrSet<SILBasicBlock *, 4>> BackedgeMap;

public:
  ARCBBStateInfo(SILFunction *F, PostOrderAnalysis *POTA);

  llvm::Optional<ARCBBStateInfoHandle> getBottomUpBBHandle(SILBasicBlock *BB);
  llvm::Optional<ARCBBStateInfoHandle> getTopDownBBHandle(SILBasicBlock *BB);

  void clear();

private:
  llvm::Optional<unsigned> getBBID(SILBasicBlock *BB) const;
};

} // end swift namespace

namespace {

using ARCBBStateInfo = ARCSequenceDataflowEvaluator::ARCBBStateInfo;
using ARCBBStateInfoHandle = ARCSequenceDataflowEvaluator::ARCBBStateInfoHandle;

} // end anonymous namespace

ARCBBStateInfo::ARCBBStateInfo(SILFunction *F, PostOrderAnalysis *POA)
  : BBToBBIDMap(), BBIDToBottomUpBBStateMap(POA->size(F)),
    BBIDToTopDownBBStateMap(POA->size(F)), BackedgeMap() {

  // Initialize state for each one of our BB's in the RPOT. *NOTE* This means
  // that unreachable predecessors will not have any BBState associated with
  // them.
  for (SILBasicBlock *BB : POA->getReversePostOrder(F)) {
    unsigned BBID = BBToBBIDMap.size();
    BBToBBIDMap[BB] = BBID;

    BBIDToBottomUpBBStateMap[BBID].init(BB);
    BBIDToTopDownBBStateMap[BBID].init(BB);

    for (auto &Succ : BB->getSuccessors())
      if (SILBasicBlock *SuccBB = Succ.getBB())
        if (BBToBBIDMap.count(SuccBB))
          BackedgeMap[BB].insert(SuccBB);
  }
}

llvm::Optional<ARCBBStateInfoHandle>
ARCBBStateInfo::getBottomUpBBHandle(SILBasicBlock *BB) {
  auto OptID = getBBID(BB);
  if (!OptID.hasValue())
    return None;

  unsigned ID = OptID.getValue();

  auto BackedgeIter = BackedgeMap.find(BB);
  if (BackedgeIter == BackedgeMap.end())
    return ARCBBStateInfoHandle(BB, ID, BBIDToBottomUpBBStateMap[ID]);
  return ARCBBStateInfoHandle(BB, ID, BBIDToBottomUpBBStateMap[ID],
                              BackedgeIter->second);
}

llvm::Optional<ARCBBStateInfoHandle>
ARCBBStateInfo::getTopDownBBHandle(SILBasicBlock *BB) {
  auto MaybeID = getBBID(BB);
  if (!MaybeID.hasValue())
    return None;

  unsigned ID = MaybeID.getValue();

  auto BackedgeIter = BackedgeMap.find(BB);
  if (BackedgeIter == BackedgeMap.end())
    return ARCBBStateInfoHandle(BB, ID, BBIDToTopDownBBStateMap[ID]);
  return ARCBBStateInfoHandle(BB, ID, BBIDToTopDownBBStateMap[ID],
                              BackedgeIter->second);
}

llvm::Optional<unsigned> ARCBBStateInfo::getBBID(SILBasicBlock *BB) const {
  auto Iter = BBToBBIDMap.find(BB);
  if (Iter == BBToBBIDMap.end())
    return None;
  return Iter->second;
}

void ARCBBStateInfo::clear() {
  assert(BBIDToBottomUpBBStateMap.size() == BBIDToTopDownBBStateMap.size()
         && "These should be one to one mapped to basic blocks so should"
         " have the same size");
  for (unsigned i : indices(BBIDToBottomUpBBStateMap)) {
    BBIDToBottomUpBBStateMap[i].clear();
    BBIDToTopDownBBStateMap[i].clear();
  }
}

//===----------------------------------------------------------------------===//
//                             Top Down Dataflow
//===----------------------------------------------------------------------===//

/// Analyze a single BB for refcount inc/dec instructions.
///
/// If anything was found it will be added to DecToIncStateMap.
///
/// NestingDetected will be set to indicate that the block needs to be
/// reanalyzed if code motion occurs.
static bool processBBTopDown(
    ARCBBState &BBState,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
    AliasAnalysis *AA, RCIdentityAnalysis *RCIA) {
  DEBUG(llvm::dbgs() << ">>>> Top Down!\n");

  SILBasicBlock &BB = BBState.getBB();

  bool NestingDetected = false;

  // If the current BB is the entry BB, initialize a state corresponding to each
  // of its owned parameters. This enables us to know that if we see a retain
  // before any decrements that the retain is known safe.
  //
  // We do not handle guaranteed parameters here since those are handled in the
  // code in GlobalARCPairingAnalysis. This is because even if we don't do
  // anything, we will still pair the retain, releases and then the guaranteed
  // parameter will ensure it is known safe to remove them.
  if (&BB == &*BB.getParent()->begin()) {
    auto Args = BB.getBBArgs();
    auto SignatureParams =
        BB.getParent()->getLoweredFunctionType()->getParameters();
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      SILArgument *A = Args[i];
      ParameterConvention P = SignatureParams[i].getConvention();

      DEBUG(llvm::dbgs() << "VISITING ARGUMENT: " << *A);

      if (P != ParameterConvention::Direct_Owned)
        continue;

      TopDownRefCountState &State = BBState.getTopDownRefCountState(Args[i]);
      State.initWithArg(A);
    }
  }

  // For each instruction I in BB...
  for (auto &I : BB) {

    DEBUG(llvm::dbgs() << "VISITING:\n    " << I);

    // If we see an autorelease pool call, be conservative and clear all state
    // that we are currently tracking in the BB.
    if (isAutoreleasePoolCall(I)) {
      BBState.clear();
      continue;
    }

    SILValue Op;

    // If I is a ref count increment instruction...
    if (isRefCountIncrement(I)) {
      // map its operand to a newly initialized or reinitialized ref count
      // state and continue...
      Op = RCIA->getRCIdentityRoot(I.getOperand(0));
      TopDownRefCountState &State = BBState.getTopDownRefCountState(Op);
      NestingDetected |= State.initWithInst(&I);

      DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT! Known Safe: "
                         << (State.isKnownSafe() ? "yes" : "no") << "\n");

      // Continue processing in case this increment could be a CanUse for a
      // different pointer.
    }

    // If we have a reference count decrement...
    if (isRefCountDecrement(I)) {
      // Look up the state associated with its operand...
      Op = RCIA->getRCIdentityRoot(I.getOperand(0));
      TopDownRefCountState &RefCountState = BBState.getTopDownRefCountState(Op);

      DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT!\n");

      // If we are tracking an increment on the ref count root associated with
      // the decrement and the decrement matches, pair this decrement with a
      // copy of the increment state and then clear the original increment state
      // so that we are ready to process further values.
      if (RefCountState.isRefCountInstMatchedToTrackedInstruction(&I)) {
        // Copy the current value of ref count state into the result map.
        DecToIncStateMap[&I] = RefCountState;
        DEBUG(llvm::dbgs() << "    MATCHING INCREMENT:\n"
                           << RefCountState.getValue());

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

      // Otherwise we continue processing the reference count decrement to
      // see if the decrement can affect any other pointers that we are
      // tracking.
    }

    // For all other [(SILValue, TopDownState)] we are tracking...
    for (auto &OtherState : BBState.getTopDownStates()) {
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
                           << OtherState.second.getValue());
        continue;
      }

      // Check if the instruction we are visiting could potentially decrement
      // the reference counted value we are tracking in a manner that could
      // cause us to change states. If we do change states continue...
      if (OtherState.second.handlePotentialDecrement(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
                           << OtherState.second.getValue());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      if (OtherState.second.handlePotentialUser(&I, AA))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        "
                           << OtherState.second.getValue());
    }
  }

  return NestingDetected;
}

void ARCSequenceDataflowEvaluator::mergePredecessors(
    ARCBBStateInfoHandle &DataHandle) {
  bool HasAtLeastOnePred = false;
  llvm::SmallVector<SILBasicBlock *, 4> BBThatNeedInsertPts;

  SILBasicBlock *BB = DataHandle.getBB();
  ARCBBState &BBState = DataHandle.getState();

  // For each successor of BB...
  for (SILBasicBlock *PredBB : BB->getPreds()) {

    // Try to look up the data handle for it. If we don't have any such state,
    // then the predecessor must be unreachable from the entrance and thus is
    // uninteresting to us.
    auto PredDataHandle = getTopDownBBState(PredBB);
    if (!PredDataHandle)
      continue;

    DEBUG(llvm::dbgs() << "    Merging Pred: " << PredDataHandle->getID()
                       << "\n");

    // If the predecessor is the head of a backedge in our traversal, clear any
    // state we are tracking now and clear the state of the basic block. There
    // is some sort of control flow here that we do not understand.
    if (PredDataHandle->isBackedge(BB)) {
      BBState.clear();
      break;
    }

    ARCBBState &PredBBState = PredDataHandle->getState();

    // If we found the state but the state is for a trap BB, skip it. Trap BBs
    // leak all reference counts and do not reference reference semantic objects
    // in any manner.
    //
    // TODO: I think this is a copy paste error, since we a trap BB should have
    // an unreachable at its end. See if this can be removed.
    if (PredBBState.isTrapBB())
      continue;

    if (HasAtLeastOnePred) {
      BBState.mergePredTopDown(PredBBState);
      continue;
    }

    BBState.initPredTopDown(PredBBState);
    HasAtLeastOnePred = true;
  }
}

bool ARCSequenceDataflowEvaluator::processTopDown() {
  bool NestingDetected = false;

  DEBUG(llvm::dbgs() << "<<<< Processing Top Down! >>>>\n");

  // For each BB in our reverse post order...
  for (auto *BB : POA->getReversePostOrder(&F)) {
    // We should always have a value here.
    auto BBDataHandle = getTopDownBBState(BB).getValue();

    // This will always succeed since we have an entry for each BB in our RPOT.
    //
    // TODO: When data handles are introduced, print that instead. This code
    // should not be touching BBIDs directly.
    DEBUG(llvm::dbgs() << "Processing BB#: " << BBDataHandle.getID() << "\n");

    DEBUG(llvm::dbgs() << "Merging Predecessors!\n");
    mergePredecessors(BBDataHandle);

    // Then perform the basic block optimization.
    NestingDetected |=
        processBBTopDown(BBDataHandle.getState(), DecToIncStateMap, AA, RCIA);
  }

  return NestingDetected;
}

//===----------------------------------------------------------------------===//
//                             Bottom Up Dataflow
//===----------------------------------------------------------------------===//

/// Analyze a single BB for refcount inc/dec instructions.
///
/// If anything was found it will be added to DecToIncStateMap.
///
/// NestingDetected will be set to indicate that the block needs to be
/// reanalyzed if code motion occurs.
///
/// An epilogue release is a release that post dominates all other uses of a
/// pointer in a function that implies that the pointer is alive up to that
/// point. We "freeze" (i.e. do not attempt to remove or move) such releases if
/// FreezeOwnedArgEpilogueReleases is set. This is useful since in certain cases
/// due to dataflow issues, we can not properly propagate the last use
/// information. Instead we run an extra iteration of the ARC optimizer with
/// this enabled in a side table so the information gets propgated everywhere in
/// the CFG.
bool ARCSequenceDataflowEvaluator::processBBBottomUp(
    ARCBBState &BBState, bool FreezeOwnedArgEpilogueReleases) {
  DEBUG(llvm::dbgs() << ">>>> Bottom Up!\n");
  SILBasicBlock &BB = BBState.getBB();

  bool NestingDetected = false;

  // For each non terminator instruction I in BB visited in reverse...
  for (auto II = std::next(BB.rbegin()), IE = BB.rend(); II != IE;) {
    SILInstruction &I = *II;
    ++II;

    DEBUG(llvm::dbgs() << "VISITING:\n    " << I);

    // If we see an autorelease pool, be conservative and clear *everything*.
    if (isAutoreleasePoolCall(I)) {
      BBState.clear();
      continue;
    }

    // If this instruction is a post dominating release, skip it so we don't
    // pair it up with anything.
    if (FreezeOwnedArgEpilogueReleases &&
        ConsumedArgToReleaseMap.isReleaseMatchedToArgument(&I))
      continue;

    SILValue Op;

    // If I is a ref count decrement instruction...
    if (isRefCountDecrement(I)) {
      // map its operand to a newly initialized or reinitialized ref count
      // state and continue...
      Op = RCIA->getRCIdentityRoot(I.getOperand(0));
      BottomUpRefCountState &State = BBState.getBottomUpRefCountState(Op);
      NestingDetected |= State.initWithInst(&I);

      // If we are running with 'frozen' owned arg releases, check if we have a
      // frozen use in the side table. If so, this release must be known safe.
      if (FreezeOwnedArgEpilogueReleases) {
        State.KnownSafe |= ConsumedArgToReleaseMap.argumentHasRelease(Op);
      }

      DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT! Known Safe: "
                         << (State.isKnownSafe() ? "yes" : "no") << "\n");

      // Continue on to see if our reference decrement could potentially affect
      // any other pointers via a use or a decrement.
    }

    // If we have a reference count decrement...
    if (isRefCountIncrement(I)) {
      // Look up the state associated with its operand...
      Op = RCIA->getRCIdentityRoot(I.getOperand(0));
      BottomUpRefCountState &RefCountState =
          BBState.getBottomUpRefCountState(Op);

      DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT!\n");

      // If we find a state initialized with a matching increment, pair this
      // decrement with a copy of the ref count state and then clear the ref
      // count state in preparation for any future pairs we may see on the same
      // pointer.
      if (RefCountState.isRefCountInstMatchedToTrackedInstruction(&I)) {
        // Copy the current value of ref count state into the result map.
        IncToDecStateMap[&I] = RefCountState;
        DEBUG(llvm::dbgs() << "    MATCHING DECREMENT:"
                           << RefCountState.getValue());

        // Clear the ref count state so it can be used for future pairs we may
        // see.
        RefCountState.clear();
      }
#ifndef NDEBUG
      else {
        if (RefCountState.isTrackingRefCountInst()) {
          DEBUG(llvm::dbgs()
                << "    FAILED MATCH DECREMENT:" << RefCountState.getValue());
        } else {
          DEBUG(llvm::dbgs() << "    FAILED MATCH DECREMENT. Not tracking a "
                                "decrement.\n");
        }
      }
#endif

      // Otherwise we continue processing the reference count decrement to
      // see if the increment can act as a use for other values.
    }

    // For all other (reference counted value, ref count state) we are
    // tracking...
    for (auto &OtherState : BBState.getBottomupStates()) {
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
                           << OtherState.second.getValue());
        continue;
      }

      // Check if the instruction we are visiting could potentially decrement
      // the reference counted value we are tracking... in a manner that could
      // cause us to change states. If we do change states continue...
      if (OtherState.second.handlePotentialDecrement(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
                           << OtherState.second.getValue());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      if (OtherState.second.handlePotentialUser(&I, AA))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        "
                           << OtherState.second.getValue());
    }
  }

  return NestingDetected;
}

void ARCSequenceDataflowEvaluator::mergeSuccessors(
    ARCBBStateInfoHandle &DataHandle) {
  SILBasicBlock *BB = DataHandle.getBB();
  ARCBBState &BBState = DataHandle.getState();

  // For each successor of BB...
  ArrayRef<SILSuccessor> Succs = BB->getSuccessors();
  bool HasAtLeastOneSucc = false;
  for (unsigned i = 0, e = Succs.size(); i != e; ++i) {
    // If it does not have a basic block associated with it...
    auto *SuccBB = Succs[i].getBB();

    // Skip it.
    if (!SuccBB)
      continue;

    // If the BB is the head of a backedge in our traversal, we have
    // hit a loop boundary. In that case, add any instructions we are
    // tracking or instructions that we have seen to the banned
    // instruction list. Clear the instructions we are tracking
    // currently, but leave that we saw a release on them. In a post
    // order, we know that all of a BB's successors will always be
    // visited before the BB, implying we will know if conservatively
    // we saw a release on the pointer going down all paths.
    if (DataHandle.isBackedge(SuccBB)) {
      BBState.clear();
      break;
    }

    // Otherwise, lookup the BBState associated with the successor and merge
    // the successor in. We know this will always succeed.
    auto SuccDataHandle = *getBottomUpBBState(SuccBB);

    ARCBBState &SuccBBState = SuccDataHandle.getState();

    if (SuccBBState.isTrapBB())
      continue;

    if (HasAtLeastOneSucc) {
      BBState.mergeSuccBottomUp(SuccBBState);
      continue;
    }

    BBState.initSuccBottomUp(SuccBBState);
    HasAtLeastOneSucc = true;
  }
}

bool ARCSequenceDataflowEvaluator::processBottomUp(
    bool FreezeOwnedArgEpilogueReleases) {
  bool NestingDetected = false;

  DEBUG(llvm::dbgs() << "<<<< Processing Bottom Up! >>>>\n");

  // For each BB in our post order...
  for (auto *BB : POA->getPostOrder(&F)) {
    // Grab the BBState associated with it and set it to be the current BB.
    auto BBDataHandle = *getBottomUpBBState(BB);

    // This will always succeed since we have an entry for each BB in our post
    // order.
    DEBUG(llvm::dbgs() << "Processing BB#: " << BBDataHandle.getID() << "\n");

    DEBUG(llvm::dbgs() << "Merging Successors!\n");
    mergeSuccessors(BBDataHandle);

    // Then perform the basic block optimization.
    NestingDetected |= processBBBottomUp(BBDataHandle.getState(),
                                         FreezeOwnedArgEpilogueReleases);
  }

  return NestingDetected;
}

//===----------------------------------------------------------------------===//
//                 Top Level ARC Sequence Dataflow Evaluator
//===----------------------------------------------------------------------===//

ARCSequenceDataflowEvaluator::ARCSequenceDataflowEvaluator(
    SILFunction &F, AliasAnalysis *AA, PostOrderAnalysis *POA,
    RCIdentityAnalysis *RCIA,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap)
    : F(F), AA(AA), POA(POA), RCIA(RCIA), DecToIncStateMap(DecToIncStateMap),
      IncToDecStateMap(IncToDecStateMap),
      // We use a malloced pointer here so we don't need to expose
      // ARCBBStateInfo in the header.
      BBStateInfo(new ARCBBStateInfo(&F, POA)),
      ConsumedArgToReleaseMap(RCIA, &F) {}

bool ARCSequenceDataflowEvaluator::run(bool FreezeOwnedReleases) {
  bool NestingDetected = processBottomUp(FreezeOwnedReleases);
  NestingDetected |= processTopDown();
  return NestingDetected;
}

ARCSequenceDataflowEvaluator::~ARCSequenceDataflowEvaluator() {
  // We use a malloced pointer here so we don't need to expose the type to the
  // outside world.
  delete BBStateInfo;
}

void ARCSequenceDataflowEvaluator::clear() { BBStateInfo->clear(); }

llvm::Optional<ARCBBStateInfoHandle>
ARCSequenceDataflowEvaluator::getBottomUpBBState(SILBasicBlock *BB) {
  return BBStateInfo->getBottomUpBBHandle(BB);
}

llvm::Optional<ARCBBStateInfoHandle>
ARCSequenceDataflowEvaluator::getTopDownBBState(SILBasicBlock *BB) {
  return BBStateInfo->getTopDownBBHandle(BB);
}
