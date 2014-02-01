//===-------- Passes.cpp - Swift Compiler SIL Pass Entrypoints ------------===//
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
///
///  \file
///  \brief This file provides implementations of a few helper functions
///  which provide abstracted entrypoints to the SILPasses stage.
///
///  \note The actual SIL passes should be implemented in per-pass source files,
///  not in this file.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-optimizer"

#include "swift/SILPasses/Passes.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumOptzIter, "Number of optimizer iterations");

using namespace swift;

static void performParanoidVerification(SILModule &Module,
                                        const SILOptions &Options) {
  if (!Options.EnableParanoidVerification)
    return;
  Module.verify();
}

bool swift::runSILDiagnosticPasses(SILModule &Module,
                                   const SILOptions &Options) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;

  auto &Ctx = Module.getASTContext();

  DEBUG(performParanoidVerification(Module, Options));

  performSILMandatoryInlining(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  performSILCapturePromotion(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  performSILAllocBoxToStackPromotion(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  performInOutDeshadowing(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  performSILDefiniteInitialization(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  performSILPredictableMemoryOptimizations(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  performSILConstantPropagation(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  performSILDeadCodeElimination(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  // Generate diagnostics.
  emitSILDataflowDiagnostics(&Module);
  DEBUG(performParanoidVerification(Module, Options));

  Module.setStage(SILStage::Canonical);

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

void swift::runSILOptimizationPasses(SILModule &Module,
                                     const SILOptions &Options) {
  // Continue to optimize the code until we can't specialize any more.
  bool Changed = true;
  while (Changed) {
    // Specialize generic functions.
    Changed = performSILSpecialization(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    // Inline the specialized functions.
    performSILPerformanceInlining(&Module, Options.InlineThreshold);
    DEBUG(performParanoidVerification(Module, Options));

    // Cleanup after inlining.
    performSILCombine(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    // Transition to SSA form.
    performSILLowerAggregateInstrs(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    performSILSROA(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    performSILMem2Reg(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    // Perform scalar optimizations.
    performSILCSE(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    performSILCombine(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    performSILCodeMotion(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    performSimplifyCFG(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    Changed |= performSILDevirtualization(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    if (Options.EnableARCOptimizations) {
      performSILARCOpts(&Module);
      DEBUG(performParanoidVerification(Module, Options));
    }

    performSILAllocBoxToStackPromotion(&Module);
    DEBUG(performParanoidVerification(Module, Options));

    performSILAllocRefElimination(&Module);

    DEBUG(Module.verify());
    // Stats
    NumOptzIter++;
  }
}
