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
#ifndef NDEBUG
  if (!Options.EnableParanoidVerification)
    return;
  Module.verify();
#endif
}

bool swift::runSILDiagnosticPasses(SILModule &Module,
                                   const SILOptions &Options) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;

  auto &Ctx = Module.getASTContext();

  performParanoidVerification(Module, Options);

  performSILMandatoryInlining(&Module);
  performParanoidVerification(Module, Options);

  performSILCapturePromotion(&Module);
  performParanoidVerification(Module, Options);

  performSILAllocBoxToStackPromotion(&Module);
  performParanoidVerification(Module, Options);

  performInOutDeshadowing(&Module);
  performParanoidVerification(Module, Options);

  performSILDefiniteInitialization(&Module);
  performParanoidVerification(Module, Options);

  performSILPredictableMemoryOptimizations(&Module);
  performParanoidVerification(Module, Options);

  performSILConstantPropagation(&Module);
  performParanoidVerification(Module, Options);

  performSILDeadCodeElimination(&Module);
  performParanoidVerification(Module, Options);

  // Generate diagnostics.
  emitSILDataflowDiagnostics(&Module);
  performParanoidVerification(Module, Options);

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
    performParanoidVerification(Module, Options);

    // Inline the specialized functions.
    performSILPerformanceInlining(&Module, Options.InlineThreshold);
    performParanoidVerification(Module, Options);

    // Cleanup after inlining.
    performSILCombine(&Module);
    performParanoidVerification(Module, Options);

    // Transition to SSA form.
    performSILLowerAggregateInstrs(&Module);
    performParanoidVerification(Module, Options);

    performSILSROA(&Module);
    performParanoidVerification(Module, Options);

    performSILMem2Reg(&Module);
    performParanoidVerification(Module, Options);

    // Perform scalar optimizations.
    performSILCSE(&Module);
    performParanoidVerification(Module, Options);

    performSILCombine(&Module);
    performParanoidVerification(Module, Options);

    performSILCodeMotion(&Module);
    performParanoidVerification(Module, Options);

    performSimplifyCFG(&Module);
    performParanoidVerification(Module, Options);

    Changed |= performSILDevirtualization(&Module);
    performParanoidVerification(Module, Options);

    if (Options.EnableARCOptimizations) {
      performSILARCOpts(&Module);
      performParanoidVerification(Module, Options);
    }

    performSILAllocBoxToStackPromotion(&Module);
    performParanoidVerification(Module, Options);

    performSILAllocRefElimination(&Module);

    DEBUG(Module.verify());
    // Stats
    NumOptzIter++;
  }
}
