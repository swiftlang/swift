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

using namespace swift;

STATISTIC(NumOptzIterations, "Number of optimization iterations");

void SILPassManager::runOneIteration() {
  DEBUG(llvm::dbgs() << "*** Optimizing the module *** \n");
  NumOptzIterations++;

  // For each transformation:
  for (SILTransform *ST : Transformations) {
    // Run module transformations on the module.
    if (SILModuleTransform *SMT = llvm::dyn_cast<SILModuleTransform>(ST)) {
      SMT->injectPassManager(this);
      SMT->injectModule(Mod);
      SMT->run();
      if (VerifyAfterAll) {
        DEBUG(Mod->verify());
      }
      continue;
    }

    // Run function transformation on all functions.
    if (SILFunctionTransform *SFT = llvm::dyn_cast<SILFunctionTransform>(ST)) {
      for (auto &F : *Mod)
        if (!F.empty()) {
          SFT->injectPassManager(this);
          SFT->injectFunction(&F);
          SFT->run();
          if (VerifyAfterAll) {
            DEBUG(Mod->verify());
          }
        }
      continue;
    }

    llvm_unreachable("Unknown pass kind.");
  }
}

void SILPassManager::run() {
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


