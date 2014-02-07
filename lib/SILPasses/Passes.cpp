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
#include "swift/SILPasses/PassManager.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

bool swift::runSILDiagnosticPasses(SILModule &Module,
                                   const SILOptions &Options) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;

  auto &Ctx = Module.getASTContext();

  SILPassManager PM(&Module, Options);
  PM.registerAnalysis(createCallGraphAnalysis(&Module));
  PM.registerAnalysis(createAliasAnalysis(&Module));
  PM.registerAnalysis(createDominanceAnalysis(&Module));
  PM.add(createMandatoryInlining());
  PM.add(createCapturePromotion());
  PM.add(createAllocBoxToStack());
  PM.add(createInOutDeshadowing());
  PM.add(createDefiniteInitialization());
  PM.add(createPredictableMemoryOptimizations());
  PM.add(createConstantPropagation());
  PM.add(createDCE());
  PM.add(createEmitDFDiagnostics());
  PM.run();

  // Generate diagnostics.
  Module.setStage(SILStage::Canonical);

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

void swift::runSILOptimizationPasses(SILModule &Module,
                                     const SILOptions &Options) {
    SILPassManager PM(&Module, Options);
    PM.registerAnalysis(createCallGraphAnalysis(&Module));
    PM.registerAnalysis(createAliasAnalysis(&Module));
    PM.registerAnalysis(createDominanceAnalysis(&Module));
    PM.add(createGenericSpecializer());
    PM.add(createPerfInliner());
    PM.add(createSILCombine());
    PM.add(createDeadFunctionElimination());
    PM.add(createLowerAggregate());
    PM.add(createSROA());
    PM.add(createMem2Reg());
    PM.add(createCSE());
    PM.add(createSILCombine());
    PM.add(createCodeMotion());
    PM.add(createSimplifyCFG());
    PM.add(createDevirtualization());
    PM.add(createARCOpts());
    PM.add(createAllocBoxToStack());
    PM.add(createAllocRefElimination());
    PM.run();

    DEBUG(Module.verify());
}
