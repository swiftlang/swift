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
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumOptzIter, "Number of optimizer iterations");

using namespace swift;

bool swift::runSILDiagnosticPasses(SILModule &Module) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;
  
  auto &Ctx = Module.getASTContext();
 
  performSILMandatoryInlining(&Module);

  performSILCapturePromotion(&Module);
  performSILAllocBoxToStackPromotion(&Module);
  performInOutDeshadowing(&Module);
  performSILDefiniteInitialization(&Module);
  performSILPredictableMemoryOptimizations(&Module);

  performSILConstantPropagation(&Module);
  performSILDeadCodeElimination(&Module);

  // Generate diagnostics.
  emitSILDataflowDiagnostics(&Module);

  Module.setStage(SILStage::Canonical);

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

void swift::runSILOptimizationPasses(SILModule &Module) {

  // Continue to optimize the code until we can't specialize any more.
  bool Changed = true;
  while (Changed) {
    // Specialize generic functions.
    Changed = performSILSpecialization(&Module);
    // Inline the specialized functions.
    performSILPerformanceInlining(&Module);
    // Cleanup after inlining.
    performSILCombine(&Module);

    // Transition to SSA form.
    performSILLowerAggregateInstrs(&Module);
    performSILSROA(&Module);
    performSILMem2Reg(&Module);

    // Perform scalar optimizations.
    performSILCSE(&Module);
    performSILCombine(&Module);
    performSimplifyCFG(&Module);

    // Stats
    NumOptzIter++;
  }


}
