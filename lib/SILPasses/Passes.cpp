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

  // If we are asked do debug serialization, instead of running all diagnostic
  // passes, just run mandatory inlining with dead transparent function cleanup
  // disabled.
  PM.add(createMandatoryInlining());
  if (Options.DebugSerialization) {
    PM.run();
    return Ctx.hadError();
  }

  // Otherwise run the rest of diagnostics.
  PM.add(createCapturePromotion());
  PM.add(createAllocBoxToStack());
  PM.add(createInOutDeshadowing());
  PM.add(createNoReturnFolding());
  PM.add(createDefiniteInitialization());
  PM.add(createPredictableMemoryOptimizations());
  PM.add(createDiagnosticConstantPropagation());
  PM.add(createDiagnoseUnreachable());
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
    PM.add(createSILLinker());
    if (Options.DebugSerialization) {
      PM.run();
      return;
    }
    PM.add(createGenericSpecializer());
    PM.add(createPerfInliner());
    PM.add(createSILCombine());
    PM.add(createDeadFunctionElimination());
    PM.add(createGlobalOpt());
    PM.add(createLowerAggregate());
    PM.add(createSROA());
    PM.add(createMem2Reg());
    PM.add(createPerformanceConstantPropagation());
    PM.add(createDCE());
    PM.add(createCSE());
    PM.add(createSILCombine());
    PM.add(createLoadStoreOpts());
    PM.add(createCodeMotion());
    PM.add(createSimplifyCFG());
    PM.add(createDevirtualization());
    PM.add(createARCOpts());
    PM.add(createAllocBoxToStack());
    PM.add(createDeadObjectElimination());
    PM.add(createDCE());
    PM.run();

    // We have a tradeoff here on how often we should clean up the deserialized
    // SILFunctions. We can remove them at dead function elimination of each
    // optimization iteration, the down side is that we will pay the cost of
    // deserializing them again if they are needed later on; the plus side is
    // that passes have fewer function to process.
    // We first release the reference from the deserializer.
    Module.invalidateSILLoader();

    // Remove unused SIL Functions, VTables and WitnessTables.
    performSILElimination(&Module);

    DEBUG(Module.verify());
}

