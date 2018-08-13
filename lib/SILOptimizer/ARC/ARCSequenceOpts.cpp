//===--- ARCSequenceOpts.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "arc-sequence-opts"

#include "swift/SILOptimizer/PassManager/Passes.h"
#include "ARCSequenceOpts.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/ProgramTerminationAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopRegionAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

STATISTIC(NumRefCountOpsRemoved, "Total number of increments removed");

llvm::cl::opt<bool> EnableLoopARC("enable-loop-arc", llvm::cl::init(false));

//===----------------------------------------------------------------------===//
//                                Code Motion
//===----------------------------------------------------------------------===//

// This routine takes in the ARCMatchingSet \p MatchSet and inserts new
// increments, decrements at the insertion points and adds the old increment,
// decrements to the delete list. Sets changed to true if anything was moved or
// deleted.
void ARCPairingContext::optimizeMatchingSet(
    ARCMatchingSet &MatchSet, llvm::SmallVectorImpl<SILInstruction *> &NewInsts,
    llvm::SmallVectorImpl<SILInstruction *> &DeadInsts) {
  LLVM_DEBUG(llvm::dbgs() << "**** Optimizing Matching Set ****\n");
  // Add the old increments to the delete list.
  for (SILInstruction *Increment : MatchSet.Increments) {
    MadeChange = true;
    LLVM_DEBUG(llvm::dbgs() << "    Deleting increment: " << *Increment);
    DeadInsts.push_back(Increment);
    ++NumRefCountOpsRemoved;
  }

  // Add the old decrements to the delete list.
  for (SILInstruction *Decrement : MatchSet.Decrements) {
    MadeChange = true;
    LLVM_DEBUG(llvm::dbgs() << "    Deleting decrement: " << *Decrement);
    DeadInsts.push_back(Decrement);
    ++NumRefCountOpsRemoved;
  }
}

bool ARCPairingContext::performMatching(
    llvm::SmallVectorImpl<SILInstruction *> &NewInsts,
    llvm::SmallVectorImpl<SILInstruction *> &DeadInsts) {
  bool MatchedPair = false;

  LLVM_DEBUG(llvm::dbgs() << "**** Computing ARC Matching Sets for "
                          << F.getName() << " ****\n");

  /// For each increment that we matched to a decrement, try to match it to a
  /// decrement -> increment pair.
  for (auto Pair : IncToDecStateMap) {
    if (!Pair.hasValue())
      continue;

    SILInstruction *Increment = Pair->first;
    if (!Increment)
      continue; // blotted

    LLVM_DEBUG(llvm::dbgs() << "Constructing Matching Set For: " << *Increment);
    ARCMatchingSetBuilder Builder(DecToIncStateMap, IncToDecStateMap, RCIA);
    Builder.init(Increment);
    if (Builder.matchUpIncDecSetsForPtr()) {
      MatchedPair |= Builder.matchedPair();
      auto &Set = Builder.getResult();
      for (auto *I : Set.Increments)
        IncToDecStateMap.erase(I);
      for (auto *I : Set.Decrements)
        DecToIncStateMap.erase(I);

      // Add the Set to the callback. *NOTE* No instruction destruction can
      // happen here since we may remove instructions that are insertion points
      // for other instructions.
      optimizeMatchingSet(Set, NewInsts, DeadInsts);
    }
  }

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
  llvm::SmallVector<SILInstruction *, 8> NewInsts;
  llvm::SmallVector<SILInstruction *, 8> DeadInsts;

  // We have already summarized all subloops of this loop. Now summarize our
  // blocks so that we only visit interesting instructions.
  Evaluator.summarizeSubregionBlocks(Region);

  bool MadeChange = false;
  bool NestingDetected = false;
  bool MatchedPair = false;

  do {
    NestingDetected = Evaluator.runOnLoop(Region, FreezePostDomReleases,
                                          RecomputePostDomReleases);
    MatchedPair = Context.performMatching(NewInsts, DeadInsts);

    if (!NewInsts.empty()) {
      LLVM_DEBUG(llvm::dbgs() << "Adding new interesting insts!\n");
      do {
        auto *I = NewInsts.pop_back_val();
        LLVM_DEBUG(llvm::dbgs() << "    " << *I);
        Evaluator.addInterestingInst(I);
      } while (!NewInsts.empty());
    }

    if (!DeadInsts.empty()) {
      LLVM_DEBUG(llvm::dbgs() << "Removing dead interesting insts!\n");
      do {
        SILInstruction *I = DeadInsts.pop_back_val();
        LLVM_DEBUG(llvm::dbgs() << "    " << *I);
        Evaluator.removeInterestingInst(I);
        I->eraseFromParent();
      } while (!DeadInsts.empty());
    }

    MadeChange |= MatchedPair;
    Evaluator.clearLoopState(Region);
    Context.DecToIncStateMap.clear();
    Context.IncToDecStateMap.clear();
    Evaluator.clearSetFactory();

    // This ensures we only ever recompute post dominating releases on the first
    // iteration.
    RecomputePostDomReleases = false;
  } while (NestingDetected && MatchedPair);

  return MadeChange;
}

//===----------------------------------------------------------------------===//
//                             Non Loop Optimizer
//===----------------------------------------------------------------------===//

static bool
processFunctionWithoutLoopSupport(SILFunction &F, bool FreezePostDomReleases,
                                  AliasAnalysis *AA, PostOrderAnalysis *POTA,
                                  RCIdentityFunctionInfo *RCIA,
                                  EpilogueARCFunctionInfo *EAFI,
                                  ProgramTerminationFunctionInfo *PTFI) {
  // GlobalARCOpts seems to be taking up a lot of compile time when running on
  // globalinit_func. Since that is not *that* interesting from an ARC
  // perspective (i.e. no ref count operations in a loop), disable it on such
  // functions temporarily in order to unblock others. This should be removed.
  if (F.getName().startswith("globalinit_"))
    return false;

  LLVM_DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  bool Changed = false;
  BlockARCPairingContext Context(F, AA, POTA, RCIA, EAFI, PTFI);
  // Until we do not remove any instructions or have nested increments,
  // decrements...
  while (true) {
    // Compute matching sets of increments, decrements, and their insertion
    // points.
    //
    // We need to blot pointers we remove after processing an individual pointer
    // so we don't process pairs after we have paired them up. Thus we pass in a
    // lambda that performs the work for us.
    bool ShouldRunAgain = Context.run(FreezePostDomReleases);

    Changed |= Context.madeChange();

    // If we did not remove any instructions or have any nested increments, do
    // not perform another iteration.
    if (!ShouldRunAgain)
      break;

    // Otherwise, perform another iteration.
    LLVM_DEBUG(llvm::dbgs() << "\n<<< Made a Change! "
                               "Reprocessing Function! >>>\n");
  }

  LLVM_DEBUG(llvm::dbgs() << "\n");

  // Return true if we moved or deleted any instructions.
  return Changed;
}

//===----------------------------------------------------------------------===//
//                               Loop Optimizer
//===----------------------------------------------------------------------===//

static bool processFunctionWithLoopSupport(
    SILFunction &F, AliasAnalysis *AA, PostOrderAnalysis *POTA, 
    LoopRegionFunctionInfo *LRFI, SILLoopInfo *LI, RCIdentityFunctionInfo *RCFI,
    EpilogueARCFunctionInfo *EAFI, ProgramTerminationFunctionInfo *PTFI) {
  // GlobalARCOpts seems to be taking up a lot of compile time when running on
  // globalinit_func. Since that is not *that* interesting from an ARC
  // perspective (i.e. no ref count operations in a loop), disable it on such
  // functions temporarily in order to unblock others. This should be removed.
  if (F.getName().startswith("globalinit_"))
    return false;

  LLVM_DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  LoopARCPairingContext Context(F, AA, LRFI, LI, RCFI, EAFI, PTFI);
  return Context.process();
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {
class ARCSequenceOpts : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() override {
    auto *F = getFunction();
    // If ARC optimizations are disabled, don't optimize anything and bail.
    if (!getOptions().EnableARCOptimizations)
      return;

    if (!EnableLoopARC) {
      auto *AA = getAnalysis<AliasAnalysis>();
      auto *POTA = getAnalysis<PostOrderAnalysis>();
      auto *RCFI = getAnalysis<RCIdentityAnalysis>()->get(F);
      auto *EAFI = getAnalysis<EpilogueARCAnalysis>()->get(F);
      ProgramTerminationFunctionInfo PTFI(F);

      if (processFunctionWithoutLoopSupport(*F, false, AA, POTA, RCFI, EAFI, &PTFI)) {
        processFunctionWithoutLoopSupport(*F, true, AA, POTA, RCFI, EAFI, &PTFI);
        invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
      }
      return;
    }

    auto *LA = getAnalysis<SILLoopAnalysis>();
    auto *LI = LA->get(F);
    auto *DA = getAnalysis<DominanceAnalysis>();
    auto *DI = DA->get(F);

    // Canonicalize the loops, invalidating if we need to.
    if (canonicalizeAllLoops(DI, LI)) {
      // We preserve loop info and the dominator tree.
      DA->lockInvalidation();
      LA->lockInvalidation();
      PM->invalidateAnalysis(F, SILAnalysis::InvalidationKind::FunctionBody);
      DA->unlockInvalidation();
      LA->unlockInvalidation();
    }

    auto *AA = getAnalysis<AliasAnalysis>();
    auto *POTA = getAnalysis<PostOrderAnalysis>();
    auto *RCFI = getAnalysis<RCIdentityAnalysis>()->get(F);
    auto *EAFI = getAnalysis<EpilogueARCAnalysis>()->get(F);
    auto *LRFI = getAnalysis<LoopRegionAnalysis>()->get(F);
    ProgramTerminationFunctionInfo PTFI(F);

    if (processFunctionWithLoopSupport(*F, AA, POTA, LRFI, LI, RCFI, EAFI, &PTFI)) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
    }

  }
};

} // end anonymous namespace


SILTransform *swift::createARCSequenceOpts() {
  return new ARCSequenceOpts();
}
