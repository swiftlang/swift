//===--- GlobalARCPairingAnalysis.cpp - Global ARC Retain Release Pairing -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "arc-sequence-opts"
#include "GlobalARCPairingAnalysis.h"
#include "RefCountState.h"
#include "GlobalARCSequenceDataflow.h"
#include "GlobalLoopARCSequenceDataflow.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                          ARC Matching Set Builder
//===----------------------------------------------------------------------===//

namespace {

struct MatchingSetFlags {
  bool KnownSafe;
  bool Partial;
};
static_assert(std::is_pod<MatchingSetFlags>::value,
              "MatchingSetFlags should be a pod.");

struct ARCMatchingSetBuilder {
  using TDMapTy = BlotMapVector<SILInstruction *, TopDownRefCountState>;
  using BUMapTy = BlotMapVector<SILInstruction *, BottomUpRefCountState>;

  TDMapTy &TDMap;
  BUMapTy &BUMap;

  llvm::SmallVector<SILInstruction *, 8> NewIncrements;
  llvm::SmallVector<SILInstruction *, 8> NewDecrements;
  bool MatchedPair;
  ARCMatchingSet MatchSet;
  bool PtrIsGuaranteedArg;

  RCIdentityFunctionInfo *RCIA;

public:
  ARCMatchingSetBuilder(TDMapTy &TDMap, BUMapTy &BUMap,
                        RCIdentityFunctionInfo *RCIA)
    : TDMap(TDMap), BUMap(BUMap), MatchedPair(false),
      PtrIsGuaranteedArg(false), RCIA(RCIA) {}

  void init(SILInstruction *Inst) {
    clear();
    MatchSet.Ptr = RCIA->getRCIdentityRoot(Inst->getOperand(0));

    // If we have a function argument that is guaranteed, set the guaranteed
    // flag so we know that it is always known safe.
    if (auto *A = dyn_cast<SILArgument>(MatchSet.Ptr)) {
      if (A->isFunctionArg()) {
        auto C = A->getParameterInfo().getConvention();
        PtrIsGuaranteedArg = C == ParameterConvention::Direct_Guaranteed;
      }
    }
    NewIncrements.push_back(Inst);
  }

  void clear() {
    MatchSet.clear();
    MatchedPair = false;
    NewIncrements.clear();
    NewDecrements.clear();
  }

  bool matchUpIncDecSetsForPtr();

  // We only allow for get result when this object is invalidated via a move.
  ARCMatchingSet &getResult() {
    return MatchSet;
  }

  bool matchedPair() const { return MatchedPair; }

private:
  /// Returns .Some(MatchingSetFlags) on success and .None on failure.
  Optional<MatchingSetFlags> matchIncrementsToDecrements();
  Optional<MatchingSetFlags> matchDecrementsToIncrements();
};

} // end anonymous namespace

/// Match retains to releases and return whether or not all of the releases
/// were known safe.
Optional<MatchingSetFlags>
ARCMatchingSetBuilder::matchIncrementsToDecrements() {
  MatchingSetFlags Flags = { true, false };

  // For each increment in our list of new increments.
  //
  // FIXME: Refactor this into its own function.
  for (SILInstruction *Increment : NewIncrements) {
    DEBUG(llvm::dbgs() << "    Looking up state for increment: " << *Increment);

    auto BURefCountState = BUMap.find(Increment);
    assert(BURefCountState != BUMap.end() && "Catch this on debug builds.");
    if (BURefCountState == BUMap.end()) {
      DEBUG(llvm::dbgs() << "        FAILURE! Could not find state for "
            "increment!\n");
      return None;
    }

    DEBUG(llvm::dbgs() << "        SUCCESS! Found state for increment.\n");

    // If we are not tracking a ref count inst for this increment, there is
    // nothing we can pair it with implying we should skip it.
    if (!(*BURefCountState)->second.isTrackingRefCountInst()) {
      DEBUG(llvm::dbgs() << "    SKIPPING INCREMENT! State not tracking any "
            "instruction.\n");
      continue;
    }

    // We need to be known safe over all increments/decrements we are matching up
    // to ignore insertion points.
    bool BUIsKnownSafe = (*BURefCountState)->second.isKnownSafe();
    DEBUG(llvm::dbgs() << "        KNOWNSAFE: "
                       << (BUIsKnownSafe ? "true" : "false") << "\n");
    Flags.KnownSafe &= BUIsKnownSafe;

    // We can only move instructions if we know that we are not partial. We can
    // still delete instructions in such cases though.
    bool BUIsPartial = (*BURefCountState)->second.isPartial();
    DEBUG(llvm::dbgs() << "        PARTIAL: "
                       << (BUIsPartial ? "true" : "false") << "\n");
    Flags.Partial |= BUIsPartial;

    // Now that we know we have an inst, grab the decrement.
    for (auto DecIter : (*BURefCountState)->second.getInstructions()) {
      SILInstruction *Decrement = DecIter;
      DEBUG(llvm::dbgs() << "        Decrement: " << *Decrement);

      // Now grab the increment matched up with the decrement from the bottom up map.
      // If we can't find it, bail we can't match this increment up with anything.
      auto TDRefCountState = TDMap.find(Decrement);
      if (TDRefCountState == TDMap.end()) {
        DEBUG(llvm::dbgs() << "            FAILURE! Could not find state for "
                              "decrement.\n");
        return None;
      }
      DEBUG(llvm::dbgs() << "            SUCCESS! Found state for "
                            "decrement.\n");

      // Make sure the increment we are looking at is also matched to our decrement.
      // Otherwise bail.
      if (!(*TDRefCountState)->second.isTrackingRefCountInst() ||
          !(*TDRefCountState)->second.containsInstruction(Increment)) {
        DEBUG(
            llvm::dbgs() << "            FAILURE! Not tracking instruction or "
                            "found increment that did not match.\n");
        return None;
      }

      // Add the decrement to the decrement to move set. If we don't insert
      // anything, just continue.
      if (!MatchSet.Decrements.insert(Decrement)) {
        DEBUG(llvm::dbgs()
              << "        SKIPPING! Already processed this decrement\n");
        continue;
      }

      // Collect the increment insertion point if it has one.
      for (auto InsertPt : (*TDRefCountState)->second.getInsertPts()) {
        MatchSet.IncrementInsertPts.insert(InsertPt);
      }
      NewDecrements.push_back(Decrement);
    }
  }

  return Flags;
}

Optional<MatchingSetFlags>
ARCMatchingSetBuilder::matchDecrementsToIncrements() {
  MatchingSetFlags Flags = { true, false };

  // For each increment in our list of new increments.
  //
  // FIXME: Refactor this into its own function.
  for (SILInstruction *Decrement : NewDecrements) {
    DEBUG(llvm::dbgs() << "    Looking up state for decrement: " << *Decrement);

    auto TDRefCountState = TDMap.find(Decrement);
    assert(TDRefCountState != TDMap.end() && "Catch this on debug builds.");
    if (TDRefCountState == TDMap.end()) {
      DEBUG(llvm::dbgs() << "        FAILURE! Could not find state for "
            "increment!\n");
      return None;
    }

    DEBUG(llvm::dbgs() << "        SUCCESS! Found state for decrement.\n");

    // If we are not tracking a ref count inst for this increment, there is
    // nothing we can pair it with implying we should skip it.
    if (!(*TDRefCountState)->second.isTrackingRefCountInst()) {
      DEBUG(llvm::dbgs() << "    SKIPPING DECREMENT! State not tracking any "
            "instruction.\n");
      continue;
    }

    // We need to be known safe over all increments/decrements we are matching up
    // to ignore insertion points.
    bool TDIsKnownSafe = (*TDRefCountState)->second.isKnownSafe();
    DEBUG(llvm::dbgs() << "        KNOWNSAFE: "
                       << (TDIsKnownSafe ? "true" : "false") << "\n");
    Flags.KnownSafe &= TDIsKnownSafe;

    // We can only move instructions if we know that we are not partial. We can
    // still delete instructions in such cases though.
    bool TDIsPartial = (*TDRefCountState)->second.isPartial();
    DEBUG(llvm::dbgs() << "        PARTIAL: "
                       << (TDIsPartial ? "true" : "false") << "\n");
    Flags.Partial |= TDIsPartial;

    // Now that we know we have an inst, grab the decrement.
    for (auto IncIter : (*TDRefCountState)->second.getInstructions()) {
      SILInstruction *Increment = IncIter;
      DEBUG(llvm::dbgs() << "        Increment: " << *Increment);

      // Now grab the increment matched up with the decrement from the bottom up map.
      // If we can't find it, bail we can't match this increment up with anything.
      auto BURefCountState = BUMap.find(Increment);
      if (BURefCountState == BUMap.end()) {
        DEBUG(llvm::dbgs() << "            FAILURE! Could not find state for "
                              "increment.\n");
        return None;
      }

      DEBUG(
          llvm::dbgs() << "            SUCCESS! Found state for increment.\n");

      // Make sure the increment we are looking at is also matched to our decrement.
      // Otherwise bail.
      if (!(*BURefCountState)->second.isTrackingRefCountInst() ||
          !(*BURefCountState)->second.containsInstruction(Decrement)) {
        DEBUG(
            llvm::dbgs() << "            FAILURE! Not tracking instruction or "
                            "found increment that did not match.\n");
        return None;
      }

      // Add the decrement to the decrement to move set. If we don't insert
      // anything, just continue.
      if (!MatchSet.Increments.insert(Increment)) {
        DEBUG(llvm::dbgs() << "    SKIPPING! Already processed this increment.\n");
        continue;
      }

      // Collect the decrement insertion point if we have one.
      for (auto InsertPtIter : (*BURefCountState)->second.getInsertPts()) {
        MatchSet.DecrementInsertPts.insert(InsertPtIter);
      }
      NewIncrements.push_back(Increment);
    }
  }

  return Flags;
}

/// Visit each retain/release that is matched up to our operand over and over
/// again until we converge by not adding any more to the set which we can move.
/// If we find a situation that we cannot handle, we bail and return false. If
/// we succeed and it is safe to move increment/releases, we return true.
bool ARCMatchingSetBuilder::matchUpIncDecSetsForPtr() {
  bool KnownSafeTD = true;
  bool KnownSafeBU = true;
  bool Partial = false;

  while (true) {
    DEBUG(llvm::dbgs() << "Attempting to match up increments -> decrements:\n");
    // For each increment in our list of new increments, attempt to match them up
    // with decrements and gather the insertion points of the decrements.
    auto Result = matchIncrementsToDecrements();
    if (!Result) {
      DEBUG(llvm::dbgs() << "    FAILED TO MATCH INCREMENTS -> DECREMENTS!\n");
      return false;
    }
    if (!Result->KnownSafe) {
      DEBUG(llvm::dbgs() << "    NOT KNOWN SAFE!\n");
      KnownSafeTD = false;
    }
    if (Result->Partial) {
      DEBUG(llvm::dbgs() << "    IS PARTIAL!\n");
      Partial = true;
    }
    NewIncrements.clear();

    // If we do not have any decrements to attempt to match up with, bail.
    if (NewDecrements.empty())
      break;

    DEBUG(llvm::dbgs() << "Attempting to match up decrements -> increments:\n");
    Result = matchDecrementsToIncrements();
    if (!Result) {
      DEBUG(llvm::dbgs() << "    FAILED TO MATCH DECREMENTS -> INCREMENTS!\n");
      return false;
    }
    if (!Result->KnownSafe) {
      DEBUG(llvm::dbgs() << "    NOT KNOWN SAFE!\n");
      KnownSafeBU = false;
    }
    if (Result->Partial) {
      DEBUG(llvm::dbgs() << "    IS PARTIAL!\n");
      Partial = true;
    }
    NewDecrements.clear();

    // If we do not have any increment to attempt to match up with again, bail.
    if (NewIncrements.empty())
      break;
  }

  bool UnconditionallySafe = (KnownSafeTD && KnownSafeBU);
  if (UnconditionallySafe) {
    DEBUG(llvm::dbgs() << "UNCONDITIONALLY SAFE! DELETING INSTS.\n");
    MatchSet.IncrementInsertPts.clear();
    MatchSet.DecrementInsertPts.clear();
  } else {
    DEBUG(llvm::dbgs() << "NOT UNCONDITIONALLY SAFE!\n");

  }

  bool HaveIncInsertPts = !MatchSet.IncrementInsertPts.empty();
  bool HaveDecInsertPts = !MatchSet.DecrementInsertPts.empty();

  // If we have insertion points and partial merges, return false to avoid
  // control dependency issues.
  if ((HaveIncInsertPts || HaveDecInsertPts) && Partial) {
    DEBUG(llvm::dbgs() << "Found partial merge and insert pts. Bailing!\n");
    return false;
  }

  // If we have insertion points for increments, but not for decrements (or
  // vis-a-versa), return false. This prevents us from inserting retains and
  // removing releases or vis-a-versa.
  if (HaveIncInsertPts != HaveDecInsertPts)
    return false;

  // If we do not have any insertion points but we do have increments, we must
  // be eliminating pairs.
  if (!HaveIncInsertPts && !MatchSet.Increments.empty())
    MatchedPair = true;

  // Success!
  DEBUG(llvm::dbgs() << "SUCCESS! We can move, remove things.\n");
  return true;
}

//===----------------------------------------------------------------------===//
//                            ARC Pairing Context
//===----------------------------------------------------------------------===//

bool ARCPairingContext::performMatching(CodeMotionOrDeleteCallback &Callback) {
  bool MatchedPair = false;

  DEBUG(llvm::dbgs() << "**** Computing ARC Matching Sets for " << F.getName()
                     << " ****\n");

  /// For each increment that we matched to a decrement, try to match it to a
  /// decrement -> increment pair.
  for (auto Pair : IncToDecStateMap) {
    if (!Pair.hasValue())
      continue;

    SILInstruction *Increment = Pair->first;
    if (!Increment)
      continue; // blotted

    DEBUG(llvm::dbgs() << "Constructing Matching Set For: " << *Increment);
    ARCMatchingSetBuilder Builder(DecToIncStateMap, IncToDecStateMap, RCIA);
    Builder.init(Increment);
    if (Builder.matchUpIncDecSetsForPtr()) {
      MatchedPair |= Builder.matchedPair();
      auto &Set = Builder.getResult();
      for (auto *I : Set.Increments)
        IncToDecStateMap.blot(I);
      for (auto *I : Set.Decrements)
        DecToIncStateMap.blot(I);

      // Add the Set to the callback. *NOTE* No instruction destruction can
      // happen here since we may remove instructions that are insertion points
      // for other instructions.
      Callback.processMatchingSet(Set);
    }
  }

  // Then finalize the callback. This is the only place instructions can be
  // deleted.
  Callback.finalize();
  return MatchedPair;
}

//===----------------------------------------------------------------------===//
//                                  Loop ARC
//===----------------------------------------------------------------------===//

void LoopARCPairingContext::runOnLoop(SILLoop *L) {
  auto *Region = LRFI->getRegion(L);
  if (processRegion(Region, false, false)) {
    // We do not recompute for now since we only look at the top function level
    // for post dominating releases.
    processRegion(Region, true, false);
  }

  // Now that we have finished processing the loop, summarize the loop.
  Evaluator.summarizeLoop(Region);
}

void LoopARCPairingContext::runOnFunction(SILFunction *F) {
  if (processRegion(LRFI->getTopLevelRegion(), false, false)) {
    // We recompute the final post dom release since we may have moved the final
    // post dominated releases.
    processRegion(LRFI->getTopLevelRegion(), true, true);
  }
}

bool LoopARCPairingContext::processRegion(const LoopRegion *Region,
                                          bool FreezePostDomReleases,
                                          bool RecomputePostDomReleases) {
  bool MadeChange = false;
  bool NestingDetected = Evaluator.runOnLoop(Region, FreezePostDomReleases,
                                             RecomputePostDomReleases);
  bool MatchedPair = Context.performMatching(Callback);
  MadeChange |= MatchedPair;
  Evaluator.clearLoopState(Region);
  Context.DecToIncStateMap.clear();
  Context.IncToDecStateMap.clear();

  while (NestingDetected && MatchedPair) {
    NestingDetected = Evaluator.runOnLoop(Region, FreezePostDomReleases, false);
    MatchedPair = Context.performMatching(Callback);
    MadeChange |= MatchedPair;
    Evaluator.clearLoopState(Region);
    Context.DecToIncStateMap.clear();
    Context.IncToDecStateMap.clear();
  }

  return MadeChange;
}
