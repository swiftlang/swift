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
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::arc;

STATISTIC(NumRefCountOpsMoved, "Total number of increments moved");
STATISTIC(NumRefCountOpsRemoved, "Total number of increments removed");

//===----------------------------------------------------------------------===//
//                                Code Motion
//===----------------------------------------------------------------------===//

static SILInstruction *createIncrement(SILValue Ptr, SILInstruction *InsertPt) {
  SILBuilder B(InsertPt);

  // TODO: What is the correct SILLocation to use here? If the InsertPt is a
  // terminator, then we will have the wrong location type and hit an assertion.
  if (Ptr.getType().hasReferenceSemantics())
    return B.createStrongRetain(SILFileLocation(SourceLoc()), Ptr);
  return B.createRetainValue(SILFileLocation(SourceLoc()), Ptr);
}

static SILInstruction *createDecrement(SILValue Ptr, SILInstruction *InsertPt) {
  SILBuilder B(InsertPt);

  // TODO: What is the correct SILLocation to use here? If the InsertPt is a
  // terminator, then we will have the wrong location type and hit an assertion.
  if (Ptr.getType().hasReferenceSemantics())
    return B.createStrongRelease(SILFileLocation(SourceLoc()), Ptr);
  return B.createReleaseValue(SILFileLocation(SourceLoc()), Ptr);
}

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

  return Changed;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool processFunction(SILFunction &F, AliasAnalysis *AA) {
  // GlobalARCOpts seems to be taking up a lot of compile time when running on
  // globalinit_func. Since that is not *that* interesting from an ARC
  // perspective (i.e. no ref count operations in a loop), disable it on such
  // functions temporarily in order to unblock others. This should be removed.
  if (F.getName().startswith("globalinit_func"))
    return false;

  DEBUG(llvm::dbgs() << "***** Processing " << F.getName() << " *****\n");

  bool Changed = false;
  llvm::SmallVector<SILInstruction *, 16> InstructionsToDelete;

  // Construct our context once.
  auto *Ctx =  createARCMatchingSetComputationContext(F, AA);

  // Until we do not remove any instructions or have nested increments,
  // decrements...
  while(true) {
    // Compute matching sets of increments, decrements, and their insertion
    // points.
    //
    // We need to blot pointers we remove after processing an individual pointer
    // so we don't process pairs after we have paired them up. Thus we pass in a
    // lambda that performs the work for us.
    bool ShouldRunAgain = computeARCMatchingSet(Ctx,
      // Remove the increments, decrements and insert new increments, decrements
      // at the insertion points associated with a specific pointer.
      [&Changed, &InstructionsToDelete](ARCMatchingSet &Set) {
        Changed |= optimizeReferenceCountMatchingSet(Set, InstructionsToDelete);
      }
    );

    while (!InstructionsToDelete.empty()) {
      InstructionsToDelete.pop_back_val()->eraseFromParent();
    }

    // If we did not remove any instructions or have any nested increments, do
    // not perform another iteration.
    if (!ShouldRunAgain)
      break;

    DEBUG(llvm::dbgs() << "\n<<< Made a Change! Reprocessing Function! >>>\n");
  }
  destroyARCMatchingSetComputationContext(Ctx);

  DEBUG(llvm::dbgs() << "\n");

  return Changed;
}

namespace {
class GlobalARCOpts : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() {
    auto *AA = getAnalysis<AliasAnalysis>();
    if (processFunction(*getFunction(), AA))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "Global ARC Optimization"; }
};
} // end anonymous namespace


SILTransform *swift::createGlobalARCOpts() {
  return new GlobalARCOpts();
}
