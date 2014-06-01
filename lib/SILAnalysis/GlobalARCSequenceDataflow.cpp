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

#define DEBUG_TYPE "global-arc-dataflow-analysis"
#include "GlobalARCSequenceDataflow.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::arc;

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
//                             Top Down Dataflow
//===----------------------------------------------------------------------===//

/// Analyze a single BB for refcount inc/dec instructions.
///
/// If anything was found it will be added to DecToIncStateMap.
///
/// NestingDetected will be set to indicate that the block needs to be
/// reanalyzed if code motion occurs.
static bool
processBBTopDown(ARCBBState &BBState,
                 BlotMapVector<SILInstruction *,
                               TopDownRefCountState> &DecToIncStateMap,
                 AliasAnalysis *AA) {
  DEBUG(llvm::dbgs() << ">>>> Top Down!\n");

  SILBasicBlock &BB = BBState.getBB();

  bool NestingDetected = false;

  // If the current BB is the entry BB, initialize a state corresponding to each
  // of its owned parameters.
  //
  // TODO: Handle gauranteed parameters.
  if (&BB == &*BB.getParent()->begin()) {
    auto Args = BB.getBBArgs();
    auto SignatureParams =
      BB.getParent()->getLoweredFunctionType()->getInterfaceParameters();
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

    if (isAutoreleasePoolCall(I)) {
      BBState.clear();
      continue;
    }

    SILValue Op;

    // If I is a ref count increment instruction...
    if (isRefCountIncrement(I)) {
      // map its operand to a newly initialized or reinitialized ref count
      // state and continue...
      Op = I.getOperand(0).stripCasts();
      TopDownRefCountState &State = BBState.getTopDownRefCountState(Op);
      NestingDetected |= State.initWithInst(&I);

      DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT! Known Safe: "
            << (State.isKnownSafe()?"yes":"no") << "\n");

      // Continue processing in case this increment could be a CanUse for a
      // different pointer.
    }

    // If we have a reference count decrement...
    if (isRefCountDecrement(I)) {
      // Look up the state associated with its operand...
      Op = I.getOperand(0).stripCasts();
      TopDownRefCountState &RefCountState = BBState.getTopDownRefCountState(Op);

      DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT!\n");

      // If the state is already initialized to contain a reference count
      // increment of the same type (i.e. retain_value, release_value or
      // strong_retain, strong_release), then remove the state from the map
      // and add the retain/release pair to the delete list and continue.
      if (RefCountState.isRefCountInstMatchedToTrackedInstruction(&I)) {
        // Copy the current value of ref count state into the result map.
        DecToIncStateMap[&I] = RefCountState;
        DEBUG(llvm::dbgs() << "    MATCHING INCREMENT:\n"
              << **RefCountState.getInstructions().begin());

        // Clear the ref count state in case we see more operations on this
        // ref counted value. This is for safety reasons.
        RefCountState.clear();
      } else {
        if (RefCountState.isTrackingRefCountInst()) {
          DEBUG(llvm::dbgs() << "    FAILED MATCH INCREMENT:\n" <<
                **RefCountState.getInstructions().begin());
        } else {
          DEBUG(llvm::dbgs() << "    FAILED MATCH. NO INCREMENT.\n");
        }
      }

      // Otherwise we continue processing the reference count decrement to
      // see if the decrement can affect any other pointers that we are
      // tracking.
    }

    // For all other (reference counted value, ref count state) we are
    // tracking...
    for (auto &OtherState : BBState.getTopDownStates()) {
      // If the state we are visiting is for the pointer we just visited, bail.
      if (Op && OtherState.first == Op)
        continue;

      // If we are tracking an argument, skip it.
      if (!OtherState.second.isTrackingRefCountInst())
        continue;

      // Check if the instruction we are visiting could potentially decrement
      // the reference counted value we are tracking... in a manner that could
      // cause us to change states. If we do change states continue...
      if (OtherState.second.handlePotentialDecrement(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
              << **OtherState.second.getInstructions().begin());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      SILInstruction *Other = *OtherState.second.getInstructions().begin();
      (void)Other;
      if (OtherState.second.handlePotentialUser(&I, AA))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        " << *Other);
    }
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
static bool
processBBBottomUp(ARCBBState &BBState,
                  BlotMapVector<SILInstruction *,
                                  BottomUpRefCountState> &IncToDecStateMap,
                  AliasAnalysis *AA) {
  DEBUG(llvm::dbgs() << ">>>> Bottom Up!\n");
  SILBasicBlock &BB = BBState.getBB();

  bool NestingDetected = false;

  // For each non terminator instruction I in BB visited in reverse...
  for (auto II = std::next(BB.rbegin()), IE = BB.rend(); II != IE;) {
    SILInstruction &I = *II;
    ++II;

    DEBUG(llvm::dbgs() << "VISITING:\n    " << I);

    if (isAutoreleasePoolCall(I)) {
      BBState.clear();
      continue;
    }

    SILValue Op;

    // If I is a ref count decrement instruction...
    if (isRefCountDecrement(I)) {
      // map its operand to a newly initialized or reinitialized ref count
      // state and continue...
      Op = I.getOperand(0).stripCasts();
      BottomUpRefCountState &State = BBState.getBottomUpRefCountState(Op);
      NestingDetected |= State.initWithInst(&I);

      DEBUG(llvm::dbgs() << "    REF COUNT DECREMENT! Known Safe: "
            << (State.isKnownSafe()?"yes":"no") << "\n");

      // Continue on to see if our reference decrement could potentially affect
      // any other pointers via a use or a decrement.
    }

    // If we have a reference count decrement...
    if (isRefCountIncrement(I)) {
      // Look up the state associated with its operand...
      Op = I.getOperand(0).stripCasts();
      BottomUpRefCountState &RefCountState =
          BBState.getBottomUpRefCountState(Op);

      DEBUG(llvm::dbgs() << "    REF COUNT INCREMENT!\n");

      // If the state is already initialized to contain a reference count
      // increment of the same type (i.e. retain_value, release_value or
      // strong_retain, strong_release), then remove the state from the map
      // and add the retain/release pair to the delete list and continue.
      if (RefCountState.isRefCountInstMatchedToTrackedInstruction(&I)) {
        // Copy the current value of ref count state into the result map.
        IncToDecStateMap[&I] = RefCountState;
        DEBUG(llvm::dbgs() << "    MATCHING DECREMENT:"
              << **RefCountState.getInstructions().begin());

        // Clear the ref count state in case we see more operations on this
        // ref counted value. This is for safety reasons.
        RefCountState.clear();
      } else {
        if (RefCountState.isTrackingRefCountInst()) {
          DEBUG(llvm::dbgs() << "    FAILED MATCH DECREMENT:"
                << **RefCountState.getInstructions().begin());
        } else {
          DEBUG(llvm::dbgs() << "    FAILED MATCH DECREMENT. Not tracking a "
                "decrement.\n");
        }
      }

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

      // If we are tracking an argument, skip it.
      if (!OtherState.second.isTrackingRefCountInst())
        continue;

      // Check if the instruction we are visiting could potentially decrement
      // the reference counted value we are tracking... in a manner that could
      // cause us to change states. If we do change states continue...
      if (OtherState.second.handlePotentialDecrement(&I, AA)) {
        DEBUG(llvm::dbgs() << "    Found Potential Decrement:\n        "
              << **OtherState.second.getInstructions().begin());
        continue;
      }

      // Otherwise check if the reference counted value we are tracking
      // could be used by the given instruction.
      SILInstruction *Other = *OtherState.second.getInstructions().begin();
      (void)Other;
      if (OtherState.second.handlePotentialUser(&I, AA))
        DEBUG(llvm::dbgs() << "    Found Potential Use:\n        " << *Other);
    }
  }

  return NestingDetected;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

bool swift::arc::performARCSequenceDataflow(
    SILFunction &F,
    AliasAnalysis *AA,
    BlotMapVector<SILInstruction *, TopDownRefCountState> &DecToIncStateMap,
    BlotMapVector<SILInstruction *, BottomUpRefCountState> &IncToDecStateMap) {

  bool NestingDetected = false;

  for (auto &BB : F) {
    DEBUG(llvm::dbgs() << "\n<<< Processing New BB! >>>\n");

    ARCBBState state(&BB);

    // Perform the bottom up and then top down dataflow.
    NestingDetected |= processBBBottomUp(state, IncToDecStateMap, AA);
    NestingDetected |= processBBTopDown(state, DecToIncStateMap, AA);
  }

  return NestingDetected;
}
