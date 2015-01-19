//===-- GlobalARCPairingAnalysis.cpp - Global ARC Retain Release Pairing --===//
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
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "ReferenceCountState.h"
#include "GlobalARCSequenceDataflow.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/RCIdentityAnalysis.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

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

  RCIdentityAnalysis *RCIA;

public:
  ARCMatchingSetBuilder(TDMapTy &TDMap, BUMapTy &BUMap,
                        RCIdentityAnalysis *RCIA)
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
    if (!BURefCountState->second.isTrackingRefCountInst()) {
      DEBUG(llvm::dbgs() << "    SKIPPING INCREMENT! State not tracking any "
            "instruction.\n");
      continue;
    }

    // We need to be known safe over all increments/decrements we are matching up
    // to ignore insertion points.
    Flags.KnownSafe &= BURefCountState->second.isKnownSafe();

    // We can only move instructions if we know that we are not partial. We can
    // still delete instructions in such cases though.
    Flags.Partial |= BURefCountState->second.isPartial();

    // Now that we know we have an inst, grab the decrement.
    for (auto DecIter : BURefCountState->second.getInstructions()) {
      SILInstruction *Decrement = DecIter;
      DEBUG(llvm::dbgs() << "    Decrement: " << *Decrement);

      // Now grab the increment matched up with the decrement from the bottom up map.
      // If we can't find it, bail we can't match this increment up with anything.
      auto TDRefCountState = TDMap.find(Decrement);
      if (TDRefCountState == TDMap.end()) {
        DEBUG(llvm::dbgs() << "        FAILURE! Could not find state for "
              "decrement.\n");
        return None;
      }
      DEBUG(llvm::dbgs() << "        SUCCESS! Found state for decrement.\n");

      // Make sure the increment we are looking at is also matched to our decrement.
      // Otherwise bail.
      if (!TDRefCountState->second.isTrackingRefCountInst() ||
          !TDRefCountState->second.containsInstruction(Increment)) {
        DEBUG(llvm::dbgs() << "        FAILURE! Not tracking instruction or "
              "found increment that did not match.\n");
        return None;
      }

      // Add the decrement to the decrement to move set. If we don't insert
      // anything, just continue.
      if (!MatchSet.Decrements.insert(Decrement)) {
        DEBUG(llvm::dbgs() << "    SKIPPING! Already processed this decrement");
        continue;
      }

      // Collect the increment insertion point if it has one.
      for (auto InsertPt : TDRefCountState->second.getInsertPts()) {
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
    if (!TDRefCountState->second.isTrackingRefCountInst()) {
      DEBUG(llvm::dbgs() << "    SKIPPING DECREMENT! State not tracking any "
            "instruction.\n");
      continue;
    }

    // We need to be known safe over all increments/decrements we are matching up
    // to ignore insertion points.
    Flags.KnownSafe &= TDRefCountState->second.isKnownSafe();

    // We can only move instructions if we know that we are not partial. We can
    // still delete instructions in such cases though.
    Flags.Partial |= TDRefCountState->second.isPartial();

    // Now that we know we have an inst, grab the decrement.
    for (auto IncIter : TDRefCountState->second.getInstructions()) {
      SILInstruction *Increment = IncIter;
      DEBUG(llvm::dbgs() << "    Increment: " << *Increment);

      // Now grab the increment matched up with the decrement from the bottom up map.
      // If we can't find it, bail we can't match this increment up with anything.
      auto BURefCountState = BUMap.find(Increment);
      if (BURefCountState == BUMap.end()) {
        DEBUG(llvm::dbgs() << "        FAILURE! Could not find state for "
              "increment.\n");
        return None;
      }

      DEBUG(llvm::dbgs() << "        SUCCESS! Found state for increment.\n");

      // Make sure the increment we are looking at is also matched to our decrement.
      // Otherwise bail.
      if (!BURefCountState->second.isTrackingRefCountInst() ||
          !BURefCountState->second.containsInstruction(Decrement)) {
        DEBUG(llvm::dbgs() << "        FAILURE! Not tracking instruction or "
              "found increment that did not match.\n");
        return None;
      }

      // Add the decrement to the decrement to move set. If we don't insert
      // anything, just continue.
      if (!MatchSet.Increments.insert(Increment)) {
        DEBUG(llvm::dbgs() << "    SKIPPING! Already processed this increment");
        continue;
      }

      // Collect the decrement insertion point if we have one.
      for (auto InsertPtIter : BURefCountState->second.getInsertPts()) {
        MatchSet.DecrementInsertPts.insert(InsertPtIter);
      }
      NewIncrements.push_back(Increment);
    }
  }

  return Flags;
}

/// Visit each retain/release that is matched up to our operand over and over
/// again until we converge by not adding any more to the set which we can move.
/// If we find a situation that we can not handle, we bail and return false. If
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

  bool UnconditionallySafe = (KnownSafeTD && KnownSafeBU) || PtrIsGuaranteedArg;
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
  if ((HaveIncInsertPts || HaveDecInsertPts) && Partial)
    return false;

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
//                                  Context
//===----------------------------------------------------------------------===//

namespace swift {

struct ARCMatchingSetComputationContext {
  BlotMapVector<SILInstruction *, TopDownRefCountState> DecToIncStateMap;
  BlotMapVector<SILInstruction *, BottomUpRefCountState> IncToDecStateMap;
  ARCSequenceDataflowEvaluator Evaluator;
  RCIdentityAnalysis *RCIA;

  ARCMatchingSetComputationContext(SILFunction &F, AliasAnalysis *AA,
                                   PostOrderAnalysis *POTA,
                                   RCIdentityAnalysis *RCIA)
    : DecToIncStateMap(), IncToDecStateMap(),
      Evaluator(F, AA, POTA, RCIA, DecToIncStateMap, IncToDecStateMap),
      RCIA(RCIA) {}
};

} // end namespace swift

ARCMatchingSetComputationContext *
swift::
createARCMatchingSetComputationContext(SILFunction &F, AliasAnalysis *AA,
                                       PostOrderAnalysis *POTA,
                                       RCIdentityAnalysis *RCIA) {
  unsigned Size = F.size();

  // We do not handle CFGs with more than INT_MAX BBs. Fail gracefully.
  if (Size > unsigned(INT_MAX))
    return nullptr;

  // We pass in size to avoid expensively recomputing size over and over
  // again. Currently F has to do a walk to perform that computation.
  auto *C = new ARCMatchingSetComputationContext(F, AA, POTA, RCIA);
  C->Evaluator.init();
  return C;
}

void
swift::
destroyARCMatchingSetComputationContext(ARCMatchingSetComputationContext *Ctx) {
  delete Ctx;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

bool swift::
computeARCMatchingSet(ARCMatchingSetComputationContext *Ctx,
                      bool FreezePostDomReleases,
                      std::function<void (ARCMatchingSet&)> Fun) {

  DEBUG(llvm::dbgs() << "**** Performing ARC Dataflow for "
        << Ctx->Evaluator.getFunction()->getName() << " ****\n");

  bool NestingDetected = Ctx->Evaluator.run(FreezePostDomReleases);
  Ctx->Evaluator.clear();
  bool MatchedPair = false;

  DEBUG(llvm::dbgs() << "**** Computing ARC Matching Sets for "
        << Ctx->Evaluator.getFunction()->getName() << " ****\n");

  /// For each increment that we matched to a decrement...
  for (auto Pair : Ctx->IncToDecStateMap) {
    SILInstruction *Increment = Pair.first;
    if (!Increment)
      continue; // blotted

    DEBUG(llvm::dbgs() << "Constructing Matching Set For: " << *Increment);
    ARCMatchingSetBuilder Builder(Ctx->DecToIncStateMap,
                                  Ctx->IncToDecStateMap,
                                  Ctx->RCIA);
    Builder.init(Increment);
    if (Builder.matchUpIncDecSetsForPtr()) {
      ARCMatchingSet &M = Builder.getResult();
      MatchedPair |= Builder.matchedPair();
      for (auto *I : M.Increments)
        Ctx->IncToDecStateMap.blot(I);
      for (auto *I : M.Decrements)
        Ctx->DecToIncStateMap.blot(I);
      Fun(M);
      M.clear();
    }
  }
  Ctx->DecToIncStateMap.clear();
  Ctx->IncToDecStateMap.clear();

  // If we did not find a matching pair or detected nesting during the dataflow,
  // there are no more increment, decrements that we can optimize.
  return MatchedPair && NestingDetected;
}
