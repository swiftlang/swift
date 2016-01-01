//===--- ARCSequenceOpts.cpp ----------------------------------------------===//
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
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "GlobalARCPairingAnalysis.h"
#include "ProgramTerminationAnalysis.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
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

STATISTIC(NumRefCountOpsMoved, "Total number of increments moved");
STATISTIC(NumRefCountOpsRemoved, "Total number of increments removed");

llvm::cl::opt<bool> EnableLoopARC("enable-loop-arc", llvm::cl::init(true));

//===----------------------------------------------------------------------===//
//                                Code Motion
//===----------------------------------------------------------------------===//

/// Creates an increment on \p Ptr at insertion point \p InsertPt that creates a
/// strong_retain if \p Ptr has reference semantics itself or a retain_value if
/// \p Ptr is a non-trivial value without reference-semantics.
static SILInstruction *createIncrement(SILValue Ptr, SILInstruction *InsertPt) {
  // Set up the builder we use to insert at our insertion point.
  SILBuilder B(InsertPt);
  auto Loc = SILFileLocation(SourceLoc());

  // If Ptr is refcounted itself, create the strong_retain and
  // return.
  if (Ptr.getType().isReferenceCounted(B.getModule()))
    return B.createStrongRetain(Loc, Ptr);

  // Otherwise, create the retain_value.
  return B.createRetainValue(Loc, Ptr);
}

/// Creates a decrement on \p Ptr at insertion point \p InsertPt that creates a
/// strong_release if \p Ptr has reference semantics itself or a release_value
/// if \p Ptr is a non-trivial value without reference-semantics.
static SILInstruction *createDecrement(SILValue Ptr, SILInstruction *InsertPt) {
  // Setup the builder we will use to insert at our insertion point.
  SILBuilder B(InsertPt);
  auto Loc = SILFileLocation(SourceLoc());

  // If Ptr has reference semantics itself, create a strong_release.
  if (Ptr.getType().isReferenceCounted(B.getModule()))
    return B.createStrongRelease(Loc, Ptr);

  // Otherwise create a release value.
  return B.createReleaseValue(Loc, Ptr);
}

//===----------------------------------------------------------------------===//
//                             Mutating Callback
//===----------------------------------------------------------------------===//

// This routine takes in the ARCMatchingSet \p MatchSet and inserts new
// increments, decrements at the insertion points and adds the old increment,
// decrements to the delete list. Sets changed to true if anything was moved or
// deleted.
void CodeMotionOrDeleteCallback::processMatchingSet(ARCMatchingSet &MatchSet) {
  DEBUG(llvm::dbgs() << "**** Optimizing Matching Set ****\n");

  // Insert the new increments.
  for (SILInstruction *InsertPt : MatchSet.IncrementInsertPts) {
    if (!InsertPt) {
      DEBUG(llvm::dbgs() << "    No insertion point, not inserting increment "
            "into new position.\n");
      continue;
    }

    Changed = true;
    SILInstruction *NewIncrement = createIncrement(MatchSet.Ptr, InsertPt);
    (void)NewIncrement;
    DEBUG(llvm::dbgs() << "    Inserting new increment: " << *NewIncrement
                       << "        At insertion point: " << *InsertPt);
    ++NumRefCountOpsMoved;
  }

  // Insert the new decrements.
  for (SILInstruction *InsertPt : MatchSet.DecrementInsertPts) {
    if (!InsertPt) {
      DEBUG(llvm::dbgs() << "    No insertion point, not inserting decrement "
            "into its new position.\n");
      continue;
    }

    Changed = true;
    SILInstruction *NewDecrement = createDecrement(MatchSet.Ptr, InsertPt);
    (void)NewDecrement;
    DEBUG(llvm::dbgs() << "    Inserting new NewDecrement: " << *NewDecrement
                       << "        At insertion point: " << *InsertPt);
    ++NumRefCountOpsMoved;
  }

  // Add the old increments to the delete list.
  for (SILInstruction *Increment : MatchSet.Increments) {
    Changed = true;
    DEBUG(llvm::dbgs() << "    Deleting increment: " << *Increment);
    InstructionsToDelete.push_back(Increment);
    ++NumRefCountOpsRemoved;
  }

  // Add the old decrements to the delete list.
  for (SILInstruction *Decrement : MatchSet.Decrements) {
    Changed = true;
    DEBUG(llvm::dbgs() << "    Deleting decrement: " << *Decrement);
    InstructionsToDelete.push_back(Decrement);
    ++NumRefCountOpsRemoved;
  }
}

//===----------------------------------------------------------------------===//
//                             Non Loop Optimizer
//===----------------------------------------------------------------------===//

static bool
processFunctionWithoutLoopSupport(SILFunction &F, bool FreezePostDomReleases,
                                  AliasAnalysis *AA, PostOrderAnalysis *POTA,
                                  RCIdentityFunctionInfo *RCIA,
                                  ProgramTerminationFunctionInfo *PTFI) {
  // GlobalARCOpts seems to be taking up a lot of compile time when running on
  // globalinit_func. Since that is not *that* interesting from an ARC
  // perspective (i.e. no ref count operations in a loop), disable it on such
  // functions temporarily in order to unblock others. This should be removed.
  if (F.getName().startswith("globalinit_"))
    return false;

  DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  bool Changed = false;
  BlockARCPairingContext Context(F, AA, POTA, RCIA, PTFI);
  CodeMotionOrDeleteCallback Callback;
  // Until we do not remove any instructions or have nested increments,
  // decrements...
  while (true) {
    // Compute matching sets of increments, decrements, and their insertion
    // points.
    //
    // We need to blot pointers we remove after processing an individual pointer
    // so we don't process pairs after we have paired them up. Thus we pass in a
    // lambda that performs the work for us.
    bool ShouldRunAgain = Context.run(FreezePostDomReleases, Callback);

    Changed |= Callback.madeChange();

    // If we did not remove any instructions or have any nested increments, do
    // not perform another iteration.
    if (!ShouldRunAgain)
      break;

    // Otherwise, perform another iteration.
    DEBUG(llvm::dbgs() << "\n<<< Made a Change! Reprocessing Function! >>>\n");
  }

  DEBUG(llvm::dbgs() << "\n");

  // Return true if we moved or deleted any instructions.
  return Changed;
}

//===----------------------------------------------------------------------===//
//                               Loop Optimizer
//===----------------------------------------------------------------------===//

static bool processFunctionWithLoopSupport(
    SILFunction &F, AliasAnalysis *AA, PostOrderAnalysis *POTA,
    LoopRegionFunctionInfo *LRFI, SILLoopInfo *LI, RCIdentityFunctionInfo *RCFI,
    ProgramTerminationFunctionInfo *PTFI) {
  // GlobalARCOpts seems to be taking up a lot of compile time when running on
  // globalinit_func. Since that is not *that* interesting from an ARC
  // perspective (i.e. no ref count operations in a loop), disable it on such
  // functions temporarily in order to unblock others. This should be removed.
  if (F.getName().startswith("globalinit_"))
    return false;

  DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  LoopARCPairingContext Context(F, AA, LRFI, LI, RCFI, PTFI);
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
      ProgramTerminationFunctionInfo PTFI(F);

      if (processFunctionWithoutLoopSupport(*F, false, AA, POTA, RCFI, &PTFI)) {
        processFunctionWithoutLoopSupport(*F, true, AA, POTA, RCFI, &PTFI);
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
    auto *LRFI = getAnalysis<LoopRegionAnalysis>()->get(F);
    ProgramTerminationFunctionInfo PTFI(F);

    if (processFunctionWithLoopSupport(*F, AA, POTA, LRFI, LI, RCFI, &PTFI)) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }

  StringRef getName() override { return "ARC Sequence Opts"; }
};

} // end anonymous namespace


SILTransform *swift::createARCSequenceOpts() {
  return new ARCSequenceOpts();
}
