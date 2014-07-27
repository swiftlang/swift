//===----- PassManager.cpp - Swift Pass Manager ---------------------------===//
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

#define DEBUG_TYPE "sil-passmanager"

#include "swift/SILPasses/PassManager.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TimeValue.h"

using namespace swift;

STATISTIC(NumOptzIterations, "Number of optimization iterations");

bool SILPassManager::
runFunctionPasses(llvm::ArrayRef<SILFunctionTransform*> FuncTransforms) {
  CompleteFunctions *CompleteFuncs = getAnalysis<CompleteFunctions>();

  DEBUG(for (auto SFT : FuncTransforms) {
      llvm::dbgs() << "        Running: " << SFT->getName() << "\n";
  });

  for (auto &F : *Mod) {
    if (F.empty() || CompleteFuncs->isComplete(&F))
      continue;

    for (auto SFT : FuncTransforms) {
      CompleteFuncs->resetChanged();
      SFT->injectPassManager(this);
      SFT->injectFunction(&F);
      llvm::sys::TimeValue StartTime = llvm::sys::TimeValue::now();
      SFT->run();
      if (Options.TimeTransforms) {
        auto Delta = llvm::sys::TimeValue::now().nanoseconds() -
          StartTime.nanoseconds();
        llvm::dbgs() << Delta << " (" << SFT->getName() << "," <<
        F.getName() << ")\n";
      }

      // If this pass invalidated anything, print and verify.
      if (CompleteFuncs->hasChanged()) {
        if (Options.PrintAll) {
          llvm::dbgs() << "*** SIL function after " << SFT->getName()
                       << " (" << NumOptimizationIterations << ") ***\n";
          F.dump();
        }
        if (Options.VerifyAll) {
          F.verify();
        }
      }

      // If the pass asked us to stop running, return false.
      if (StopRunning)
        return false;
    }
  }

  // We executed all of the passes that we were asked to run without being
  // signaled to stop.
  return true;
}

bool SILPassManager::runOneIteration() {
  assert(!ShouldStop && "Should never call this if we were asked to stop "
         "without reseting the pass manager.");

  DEBUG(llvm::dbgs() << "*** Optimizing the module *** \n");
  NumOptzIterations++;
  NumOptimizationIterations++;
  CompleteFunctions *CompleteFuncs = getAnalysis<CompleteFunctions>();
  SmallVector<SILFunctionTransform*, 16> PendingFuncTransforms;

  // For each transformation:
  for (SILTransform *ST : Transformations) {
    // Bail out if we've hit the optimization pass limit.
    if (Mod->getStage() == SILStage::Canonical
        && NumPassesRun >= Options.NumOptPassesToRun)
      break;

    ++NumPassesRun;

    // Run module transformations on the module.
    if (SILModuleTransform *SMT = llvm::dyn_cast<SILModuleTransform>(ST)) {
      // Run all function passes that we've seen since the last module pass. If
      // one of the passes asked us to stop the pass pipeline, return false.
      if (!runFunctionPasses(PendingFuncTransforms)) {
        DEBUG(llvm::dbgs() << "        PASS ASKED US TO STOP! BAILING!\n");
        return false;
      }

      PendingFuncTransforms.clear();

      DEBUG(llvm::dbgs() << "        Running: " << SMT->getName() << "\n");

      CompleteFuncs->resetChanged();
      SMT->injectPassManager(this);
      SMT->injectModule(Mod);

      llvm::sys::TimeValue StartTime = llvm::sys::TimeValue::now();
      SMT->run();
      if (Options.TimeTransforms) {
        auto Delta = llvm::sys::TimeValue::now().nanoseconds() -
          StartTime.nanoseconds();
        llvm::dbgs() << Delta << " (" << SMT->getName() << ",Module)\n";
      }

      // If this pass invalidated anything, print and verify.
      if (CompleteFuncs->hasChanged()) {
        if (Options.PrintAll) {
          llvm::dbgs() << "*** SIL module after " << SMT->getName()
                       << " (" << NumOptimizationIterations << ") ***\n";
          Mod->dump();
        }
        if (Options.VerifyAll) {
          Mod->verify();
        }
      }

      if (StopRunning) {
        DEBUG(llvm::dbgs() << "        PASSED ASKED US TO STOP! BAILING!\n");
        return false;
      }

      continue;
    }

    // Run function transformation on all functions.
    if (SILFunctionTransform *SFT = llvm::dyn_cast<SILFunctionTransform>(ST)) {
      PendingFuncTransforms.push_back(SFT);
      continue;
    }

    llvm_unreachable("Unknown pass kind.");
  }

  bool AskedToStop = !runFunctionPasses(PendingFuncTransforms);
  (void)AskedToStop;
  CompleteFuncs->setComplete();

  DEBUG(if (AskedToStop) {
    llvm::dbgs() << "        PASS ASKED US TO STOP! BAILING!\n";
  });
  return !AskedToStop;
}

bool SILPassManager::run() {
  if (Options.PrintAll) {
    llvm::dbgs() << "*** SIL module before transformation ("
                 << NumOptimizationIterations << ") ***\n";
    Mod->dump();
  }
  // Keep optimizing the module until no pass requested another iteration
  // of the pass or we reach the maximum.
  const unsigned IterationLimit = 20;
  do {
    anotherIteration = false;
    // Run an iteration. If a pass asked us to stop, return false.
    if (!runOneIteration()) {
      return false;
    }
  } while (anotherIteration && NumOptimizationIterations < IterationLimit);

  // We iterated until convergence and no passes asked us to stop! Return true.
  return true;
}

/// D'tor.
SILPassManager::~SILPassManager() {
  // Free all transformations.
  for (auto T : Transformations)
    delete T;

  // delete the analyis.
  for (auto A : Analysis)
    delete A;
}

/// \brief Reset the state of the pass manager and remove all transformation
/// owned by the pass manager. Anaysis passes will be kept.
void SILPassManager::resetAndRemoveTransformations() {
  for (auto T : Transformations)
    delete T;

  Transformations.clear();
  NumPassesRun = 0;
  NumOptimizationIterations = 0;
  anotherIteration = false;
  StopRunning = false;
  CompleteFunctions *CompleteFuncs = getAnalysis<CompleteFunctions>();
  CompleteFuncs->reset();
}
