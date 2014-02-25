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

void SILPassManager::
runFunctionPasses(llvm::ArrayRef<SILFunctionTransform*> FuncTransforms) {
  CompleteFunctions *CompleteFuncs = getAnalysis<CompleteFunctions>();
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
                       << " (" << NumOptzIterations << ") ***\n";
          F.dump();
        }
        if (Options.VerifyAll) {
          DEBUG(Mod->verify());
        }
      }
    }
  }
}

void SILPassManager::runOneIteration() {
  DEBUG(llvm::dbgs() << "*** Optimizing the module *** \n");
  NumOptzIterations++;
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
      // Run all function passes that we've seen since the last module pass.
      runFunctionPasses(PendingFuncTransforms);
      PendingFuncTransforms.clear();

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
                       << " (" << NumOptzIterations << ") ***\n";
          Mod->dump();
        }
        if (Options.VerifyAll) {
          DEBUG(Mod->verify());
        }
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
  runFunctionPasses(PendingFuncTransforms);
  CompleteFuncs->setComplete();
}

void SILPassManager::run() {
  if (Options.PrintAll) {
    llvm::dbgs() << "*** SIL module before transformation ("
                 << NumOptzIterations << ") ***\n";
    Mod->dump();
  }
  // Keep optimizing the module untill no pass requested another iteration
  // of the pass.
  do {
    anotherIteration = false;

    runOneIteration();

  } while (anotherIteration);
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


