//===-------- GlobalARCOpts.cpp - Global SIL ARC Optimizations ------------===//
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
#include "swift/SILPasses/Passes.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
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

STATISTIC(NumRefCountOpsMoved, "Total number of increments moved");
STATISTIC(NumRefCountOpsRemoved, "Total number of increments removed");

//===----------------------------------------------------------------------===//
//                                Code Motion
//===----------------------------------------------------------------------===//

/// Creates an increment on \p Ptr at insertion point \p InsertPt that creates a
/// strong_retain if \p Ptr has reference semantics itself or a retain_value if
/// \p Ptr is a non-trivial value without reference-semantics.
static SILInstruction *createIncrement(SILValue Ptr, SILInstruction *InsertPt) {
  // Set up the builder we use to insert at our insertion point.
  SILBuilder B(InsertPt);

  // TODO: What is the correct SILLocation to use here? If the InsertPt is a
  // terminator, then we will have the wrong location type and hit an assertion.
  // Also: what scope?
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

  // TODO: What is the correct SILLocation to use here? If the InsertPt is a
  // terminator, then we will have the wrong location type and hit an assertion.
  // Also: what scope?
  auto Loc = SILFileLocation(SourceLoc());

  // If Ptr has reference semantics itself, create a strong_release.
  if (Ptr.getType().isReferenceCounted(B.getModule()))
    return B.createStrongRelease(Loc, Ptr);

  // Otherwise create a release value.
  return B.createReleaseValue(Loc, Ptr);
}

/// This routine takes in the ARCMatchingSet \p MatchSEt and inserts new
/// increments, decrements at the insertion points and adds the old increment,
/// decrements to the delete list \p DelList.
static bool
optimizeReferenceCountMatchingSet(ARCMatchingSet &MatchSet,
                                  SmallVectorImpl<SILInstruction *> &DelList) {
  DEBUG(llvm::dbgs() << "**** Optimizing Matching Set ****\n");

  bool Changed = false;

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
    DelList.push_back(Increment);
    ++NumRefCountOpsRemoved;
  }

  // Add the old decrements to the delete list.
  for (SILInstruction *Decrement : MatchSet.Decrements) {
    Changed = true;
    DEBUG(llvm::dbgs() << "    Deleting decrement: " << *Decrement);
    DelList.push_back(Decrement);
    ++NumRefCountOpsRemoved;
  }

  // Return if we made any changes.
  return Changed;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool processFunction(SILFunction &F, bool FreezePostDomRelease,
                            AliasAnalysis *AA,
                            PostOrderAnalysis *POTA,
                            RCIdentityAnalysis *RCIA) {
  // GlobalARCOpts seems to be taking up a lot of compile time when running on
  // globalinit_func. Since that is not *that* interesting from an ARC
  // perspective (i.e. no ref count operations in a loop), disable it on such
  // functions temporarily in order to unblock others. This should be removed.
  if (F.getName().startswith("globalinit_"))
    return false;

  DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  bool Changed = false;
  llvm::SmallVector<SILInstruction *, 16> InstructionsToDelete;

  // Construct our context once. A context contains the RPOT as well as maps
  // that contain state for each BB in F. This is a major place where the
  // optimizer allocates memory in one large chunk.
  auto *Ctx =  createARCMatchingSetComputationContext(F, AA, POTA, RCIA);

  // If Ctx is null, we failed to initialize and can not do anything so just
  // return false.
  if (!Ctx) {
    DEBUG(llvm::dbgs() << "    Failed to initialize matching set computation "
          "context! Bailing!\n");
    return false;
  }

  // Until we do not remove any instructions or have nested increments,
  // decrements...
  while(true) {
    // Compute matching sets of increments, decrements, and their insertion
    // points.
    //
    // We need to blot pointers we remove after processing an individual pointer
    // so we don't process pairs after we have paired them up. Thus we pass in a
    // lambda that performs the work for us.
    bool ShouldRunAgain = computeARCMatchingSet(Ctx, FreezePostDomRelease,
      // Remove the increments, decrements and insert new increments, decrements
      // at the insertion points associated with a specific pointer.
      [&Changed, &InstructionsToDelete](ARCMatchingSet &Set) {
        Changed |= optimizeReferenceCountMatchingSet(Set, InstructionsToDelete);
      }
    );

    // Now that it is safe to do so and avoid iterator invalidation, delete all
    // instructions from the delete list.
    while (!InstructionsToDelete.empty()) {
      InstructionsToDelete.pop_back_val()->eraseFromParent();
    }

    // If we did not remove any instructions or have any nested increments, do
    // not perform another iteration.
    if (!ShouldRunAgain)
      break;

    // Otherwise, perform another iteration.
    DEBUG(llvm::dbgs() << "\n<<< Made a Change! Reprocessing Function! >>>\n");
  }

  // Now that we have finished our computation, destroy the matching set
  // computation context.
  destroyARCMatchingSetComputationContext(Ctx);

  DEBUG(llvm::dbgs() << "\n");

  // Return true if we moved or deleted any instructions.
  return Changed;
}

namespace {
class GlobalARCOpts : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() override {
    // If ARC optimizations are disabled, don't optimize anything and bail.
    if (!getOptions().EnableARCOptimizations)
      return;

    auto *AA = getAnalysis<AliasAnalysis>();
    auto *POTA = getAnalysis<PostOrderAnalysis>();
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    if (processFunction(*getFunction(), false, AA, POTA, RCIA)) {
      processFunction(*getFunction(), true, AA, POTA, RCIA);
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

  StringRef getName() override { return "Global ARC Optimization"; }
};
} // end anonymous namespace


SILTransform *swift::createGlobalARCOpts() {
  return new GlobalARCOpts();
}
