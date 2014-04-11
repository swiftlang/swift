//===--- DeadFunctionElimination.cpp - Eliminate dead functions -----------===//
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

#define DEBUG_TYPE "sil-dead-function-elimination"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumDeadFunc, "Number of dead functions eliminated");

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

bool tryToRemoveFunction(SILFunction *F) {
  // Remove internal functions that are not referenced by anything.
  // TODO: top_level_code is currently marked as internal so we explicitly check
  // for functions with this name and keep them around.
  if (isPossiblyUsedExternally(F->getLinkage()) || F->getRefCount() ||
      F->getName() == SWIFT_ENTRY_POINT_FUNCTION)
    return false;

  DEBUG(llvm::dbgs() << "DEAD FUNCTION ELIMINATION: Erasing:" << F->getName()
                     << "\n");
  F->getModule().eraseFunction(F);
  NumDeadFunc++;
  return true;
}

//===----------------------------------------------------------------------===//
//                      Pass Definition and Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILDeadFuncElimination : public SILModuleTransform {

  void run() override {
    CallGraphAnalysis *CGA = PM->getAnalysis<CallGraphAnalysis>();
    SILModule *M = getModule();
    bool Changed = false;

    // Erase trivially dead functions that may not be a part of the call graph.
    for (auto FI = M->begin(), EI = M->end(); FI != EI;) {
      SILFunction *F = FI++;
      Changed |= tryToRemoveFunction(F);
    }

    if (Changed)
      CGA->invalidate(SILAnalysis::InvalidationKind::CallGraph);

    // If we are debugging serialization, don't eliminate any dead functions.
    if (getOptions().DebugSerialization)
      return;

    // A bottom-up list of functions, leafs first.
    const std::vector<SILFunction *> &Order = CGA->bottomUpCallGraphOrder();

    // Scan the call graph top-down (caller first) because eliminating functions
    // can generate more opportunities.
    for (int i = Order.size() - 1; i >= 0; i--)
      Changed |= tryToRemoveFunction(Order[i]);

    // Invalidate the call graph.
    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }

  StringRef getName() override { return "Dead Function Elimination"; }
};

} // end anonymous namespace

SILTransform *swift::createDeadFunctionElimination() {
  return new SILDeadFuncElimination();
}
