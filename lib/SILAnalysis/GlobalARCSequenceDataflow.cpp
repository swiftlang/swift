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

namespace llvm {
raw_ostream &operator<<(raw_ostream &OS,
                        BottomUpRefCountState::LatticeState S) {
  using LatticeState = BottomUpRefCountState::LatticeState;
  switch (S) {
  case LatticeState::None:
    return OS << "None";
  case LatticeState::Decremented:
    return OS << "Decremented";
  case LatticeState::MightBeUsed:
    return OS << "MightBeUsed";
  case LatticeState::MightBeDecremented:
    return OS << "MightBeDecremented";
  }
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                              TopDownRefCountState::LatticeState S) {
  using LatticeState = TopDownRefCountState::LatticeState;
  switch (S) {
  case LatticeState::None:
    return OS << "None";
  case LatticeState::Incremented:
    return OS << "Incremented";
  case LatticeState::MightBeUsed:
    return OS << "MightBeUsed";
  case LatticeState::MightBeDecremented:
    return OS << "MightBeDecremented";
  }
}
} // end namespace llvm

//===----------------------------------------------------------------------===//
//                           Lattice State Merging
//===----------------------------------------------------------------------===//

static inline BottomUpRefCountState::LatticeState
MergeBottomUpLatticeStates(BottomUpRefCountState::LatticeState L1,
                           BottomUpRefCountState::LatticeState L2) {
  using LatticeState = BottomUpRefCountState::LatticeState;
  // If both states equal, return the first.
  if (L1 == L2)
    return L1;

  // If either are none, return None.
  if (L1 == LatticeState::None || L2 == LatticeState::None)
    return LatticeState::None;

  // Canonicalize.
  if (unsigned(L1) > unsigned(L2))
    std::swap(L1, L2);

  // Choose the side further along in the sequence.
  if ((L1 == LatticeState::Decremented || L1 == LatticeState::MightBeUsed) ||
      (L2 == LatticeState::MightBeUsed ||
       L2 == LatticeState::MightBeDecremented))
    return L2;

  // Otherwise, we don't know what happened, be conservative and return none.
  return LatticeState::None;
}

static inline TopDownRefCountState::LatticeState
MergeTopDownLatticeStates(TopDownRefCountState::LatticeState L1,
                          TopDownRefCountState::LatticeState L2) {
  using LatticeState = TopDownRefCountState::LatticeState;
  // If both states equal, return the first.
  if (L1 == L2)
    return L1;

  // If either are none, return None.
  if (L1 == LatticeState::None || L2 == LatticeState::None)
    return LatticeState::None;

  // Canonicalize.
  if (unsigned(L1) > unsigned(L2))
    std::swap(L1, L2);

  // Choose the side further along in the sequence.
  if ((L1 == LatticeState::Incremented ||
       L1 == LatticeState::MightBeDecremented) ||
      (L2 == LatticeState::MightBeDecremented ||
       L2 == LatticeState::MightBeUsed))
    return L2;

  // Otherwise, we don't know what happened, return none.
  return LatticeState::None;
}

//===----------------------------------------------------------------------===//
//                         ARCBBState Implementation
//===----------------------------------------------------------------------===//

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

//===----------------------------------------------------------------------===//
//                    Reference Count State Implementation
//===----------------------------------------------------------------------===//

bool TopDownRefCountState::merge(const TopDownRefCountState &Other) {
  auto NewState = MergeTopDownLatticeStates(LatState, Other.LatState);
  DEBUG(llvm::dbgs() << "        Performing TopDown Merge.\n");
  DEBUG(llvm::dbgs() << "            Left: " << LatState << "; Right: "
                     << Other.LatState << "; Result: " << NewState << "\n");
  DEBUG(llvm::dbgs() << "            V: ";
        if (hasValue())
          getValue()->dump();
        else
          llvm::dbgs() << "\n";
        llvm::dbgs() << "            OtherV: ";
        if (Other.hasValue())
          Other.getValue()->dump();
        else
          llvm::dbgs() << "\n");

  LatState = NewState;
  KnownSafe &= Other.KnownSafe;

  // If we're doing a merge on a path that's previously seen a partial merge,
  // conservatively drop the sequence, to avoid doing partial RR elimination. If
  // the branch predicates for the two merge differ, mixing them is unsafe since
  // they are not control dependent.
  //
  // TODO: Add support for determining control dependence.
  if (LatState == TopDownRefCountState::LatticeState::None) {
    clear();
    DEBUG(llvm::dbgs() << "            Found LatticeState::None. "
                          "Clearing State!\n");
    return false;
  }

  // We should never have an argument path merge with a non-argument path.
  if (Argument.isNull() != Other.Argument.isNull()) {
    clear();
    DEBUG(llvm::dbgs() << "            Can not merge Argument with "
          "Non-Argument path... Bailing!\n");
    return false;
  }

  Increments.insert(Other.Increments.begin(), Other.Increments.end());

  Partial |= Other.Partial;
  Partial |= InsertPts.size() != Other.InsertPts.size();
  for (auto *SI : Other.InsertPts)
    Partial |= InsertPts.insert(SI).second;

  DEBUG(llvm::dbgs() << "            Partial: " << (Partial?"yes":"no")
        << "\n");

  return true;
}

bool BottomUpRefCountState::merge(const BottomUpRefCountState &Other) {

  auto NewState = MergeBottomUpLatticeStates(LatState, Other.LatState);
  DEBUG(llvm::dbgs() << "            Performing BottomUp Merge.\n");
  DEBUG(llvm::dbgs() << "                Left: " << LatState << "; Right: "
                     << Other.LatState << "; Result: " << NewState << "\n");
  DEBUG(llvm::dbgs() << "                V: ";
        if (hasValue())
          getValue()->dump();
        else
          llvm::dbgs() << "\n";
        llvm::dbgs() << "                OtherV: ";
        if (Other.hasValue())
          Other.getValue()->dump();
        else
          llvm::dbgs() << "\n");

  LatState = NewState;
  KnownSafe &= Other.KnownSafe;

  // If we're doing a merge on a path that's previously seen a partial merge,
  // conservatively drop the sequence, to avoid doing partial RR elimination. If
  // the branch predicates for the two merge differ, mixing them is unsafe since
  // they are not control dependent.
  //
  // TODO: Add support for working around control dependence issues.
  if (LatState == BottomUpRefCountState::LatticeState::None) {
    DEBUG(llvm::dbgs() << "            Found LatticeState::None. "
                          "Clearing State!\n");
    clear();
    return false;
  }

  // Merge previously seen instructions.
  Decrements.insert(Other.Decrements.begin(), Other.Decrements.end());

  Partial |= Other.Partial;
  Partial |= InsertPts.size() != Other.InsertPts.size();
  for (auto *SI : Other.InsertPts)
    Partial |= InsertPts.insert(SI).second;

  DEBUG(llvm::dbgs() << "                Partial: " << (Partial?"yes":"no")
        << "\n");
  return true;
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

void
swift::ARCSequenceDataflowEvaluator::mergePredecessors(ARCBBState &BBState,
                                                            SILBasicBlock *BB) {
  bool HasAtLeastOnePred = false;
  llvm::SmallVector<SILBasicBlock *, 4> BBThatNeedInsertPts;

  // For each successor of BB...
  for (SILBasicBlock *PredBB : BB->getPreds()) {
    DEBUG(llvm::dbgs() << "    Merging Pred: " << BBToBBID[PredBB] << "\n");

    // If the precessor is the head of a backedge in our traversal, clear any
    // state we are tracking now and clear the state of the basic block. There
    // is some sort of control flow here that we do not understand.
    if (BackedgeMap[PredBB].count(BB)) {
      BBState.clear();
      break;
    }

    // Otherwise, lookup the BBState associated with the predecessor and merge
    // the predecessor in.
    auto I = TopDownBBStates.find(PredBB);

    // If we can not lookup the BBState then the BB was not in the post order
    // implying that it is unreachable. LLVM will ensure that the BB is removed
    // if we do not reach it at the SIL level. Since it is unreachable, ignore
    // it.
    if (I == TopDownBBStates.end())
      continue;

    // If we found the state but the state is for a trap BB, skip it. Trap BBs
    // leak all reference counts and do not reference reference semantic objects
    // in any manner.
    if (I->second.isTrapBB())
      continue;

    if (!HasAtLeastOnePred) {
      BBState.initPredTopDown(I->second);
    } else {
      BBState.mergePredTopDown(I->second);
    }
    HasAtLeastOnePred = true;
  }
}

bool swift::ARCSequenceDataflowEvaluator::processTopDown() {
  bool NestingDetected = false;

  DEBUG(llvm::dbgs() << "<<<< Processing Top Down! >>>>\n");

  // For each BB in our reverse post order...
  for (auto *BB : POTA->getReversePostOrder(&F)) {

    DEBUG(llvm::dbgs() << "Processing BB#: " << BBToBBID[BB] << "\n");

    // Grab the BBState associated with it and set it to be the current BB.
    ARCBBState &BBState = TopDownBBStates.find(BB)->second;
    BBState.init(BB);

    DEBUG(llvm::dbgs() << "Merging Predecessors!\n");
    mergePredecessors(BBState, BB);

    // Then perform the basic block optimization.
    NestingDetected |= processBBTopDown(BBState, DecToIncStateMap, AA, RCIA);
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
bool
swift::ARCSequenceDataflowEvaluator::
processBBBottomUp(ARCBBState &BBState, bool FreezeOwnedArgEpilogueReleases) {
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

void
swift::ARCSequenceDataflowEvaluator::mergeSuccessors(ARCBBState &BBState,
                                                          SILBasicBlock *BB) {
  // Grab the backedge set for our BB.
  auto &BackEdgeSet = BackedgeMap[BB];

  // For each successor of BB...
  ArrayRef<SILSuccessor> Succs = BB->getSuccs();
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
    if (BackEdgeSet.count(SuccBB)) {
      BBState.clear();
      break;
    }

    // Otherwise, lookup the BBState associated with the successor and merge
    // the successor in.
    auto I = BottomUpBBStates.find(SuccBB);
    assert(I != BottomUpBBStates.end());

    if (I->second.isTrapBB())
      continue;

    if (!HasAtLeastOneSucc) {
      BBState.initSuccBottomUp(I->second);
    } else {
      BBState.mergeSuccBottomUp(I->second);
    }
    HasAtLeastOneSucc = true;
  }
}

bool swift::ARCSequenceDataflowEvaluator::
processBottomUp(bool FreezeOwnedArgEpilogueReleases) {
  bool NestingDetected = false;

  DEBUG(llvm::dbgs() << "<<<< Processing Bottom Up! >>>>\n");

  // For each BB in our post order...
  for (auto *BB : POTA->getPostOrder(&F)) {
    DEBUG(llvm::dbgs() << "Processing BB#: " << BBToBBID[BB] << "\n");

    // Grab the BBState associated with it and set it to be the current BB.
    ARCBBState &BBState = BottomUpBBStates.find(BB)->second;
    BBState.init(BB);

    DEBUG(llvm::dbgs() << "Merging Successors!\n");
    mergeSuccessors(BBState, BB);

    // Then perform the basic block optimization.
    NestingDetected |= processBBBottomUp(BBState,
                                         FreezeOwnedArgEpilogueReleases);
  }

  return NestingDetected;
}

//===----------------------------------------------------------------------===//
//                 Top Level ARC Sequence Dataflow Evaluator
//===----------------------------------------------------------------------===//

void swift::ARCSequenceDataflowEvaluator::init() {
  // Initialize the post order data structure.
#ifndef NDEBUG
  unsigned Count = 0;
  for (auto &BB : F) {
    BBToBBID[&BB] = Count++;
  }
#endif

  // Then iterate through it in reverse to perform the post order, looking for
  // backedges.
  llvm::DenseSet<SILBasicBlock *> VisitedSet;
  unsigned i = 0;
  for (SILBasicBlock *BB : POTA->getReversePostOrder(&F)) {
    VisitedSet.insert(BB);

    BottomUpBBStates[i].first = BB;
    BottomUpBBStates[i].second.init(BB);
    TopDownBBStates[i].first = BB;
    TopDownBBStates[i].second.init(BB);
    ++i;

    for (auto &Succ : BB->getSuccs())
      if (SILBasicBlock *SuccBB = Succ.getBB())
        if (VisitedSet.count(SuccBB))
          BackedgeMap[BB].insert(SuccBB);
  }

  BottomUpBBStates.sort();
  TopDownBBStates.sort();
}

bool swift::ARCSequenceDataflowEvaluator::run(bool FreezeOwnedReleases) {
  bool NestingDetected = processBottomUp(FreezeOwnedReleases);
  NestingDetected |= processTopDown();
  return NestingDetected;
}

void swift::ARCBBState::initializeTrapStatus() {
  IsTrapBB = isARCInertTrapBB(BB);
}
