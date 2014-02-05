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
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumOptzIterations, "Number of optimization iterations");

void SILPassManager::run() {
  // Keep optimizing the module untill no one requested another iteration
  // of the pass.
  do {
    DEBUG(llvm::dbgs() << "*** Optimizing the module *** \n");
    NumOptzIterations++;
    anotherIteration = false;

    // For each transformation:
    for (SILTransform *ST : Transformations) {

      // Run module transformations on the module.
      if (SILModuleTrans *SMT = llvm::dyn_cast<SILModuleTrans>(ST)) {
        SMT->runOnModule(*Mod, this);
        continue;
      }

      // Run function transformation on all functions.
      if (SILFunctionTrans *SFT = llvm::dyn_cast<SILFunctionTrans>(ST)) {
        for (auto &F : *Mod)
          if (!F.empty())
            SFT->runOnFunction(F, this);
        continue;
      }

      llvm_unreachable("Unknown pass kind.");
    }
  } while (anotherIteration);
}

