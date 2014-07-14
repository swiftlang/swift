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

static void registerAnalysisPasses(SILPassManager &PM, SILModule *Mod) {
  PM.registerAnalysis(createCallGraphAnalysis(Mod));
  PM.registerAnalysis(createAliasAnalysis(Mod));
  PM.registerAnalysis(createDominanceAnalysis(Mod));
  PM.registerAnalysis(createLoopInfoAnalysis(Mod, &PM));
  PM.registerAnalysis(createInductionVariableAnalysis(Mod));
  PM.registerAnalysis(createPostOrderAnalysis(Mod));
}

bool swift::runSILDiagnosticPasses(SILModule &Module,
                                   const SILOptions &Options) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;

  auto &Ctx = Module.getASTContext();

  SILPassManager PM(&Module, Options);
  registerAnalysisPasses(PM, &Module);
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

/// Perform semantic annotation/loop base optimizations.
void ConstructHighLevelLoopOptPassManager(SILPassManager &PM, SILModule &Mod) {
  registerAnalysisPasses(PM, &Mod);
  // Get rid of int->enum->uncheck_enum_data.
  PM.add(createSILCombine());
  PM.add(createLoopRotatePass());
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  PM.add(createSimplifyCFG());
}

void ConstructLowLevelLoopOptPassManager(SILPassManager &PM, SILModule &Mod) {
  registerAnalysisPasses(PM, &Mod);
  PM.add(createLICMPass());
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  PM.add(createSimplifyCFG());
}

void ConstructSSAPassManager(SILPassManager &PM, SILModule &Module,
                             bool useEarlyInliner) {

  registerAnalysisPasses(PM, &Module);
  PM.add(createSimplifyCFG());
  PM.add(createAllocBoxToStack());
  PM.add(createLowerAggregate());
  PM.add(createSILCombine());
  PM.add(createSROA());
  PM.add(createMem2Reg());

  // Perform classsic SSA optimizations.
  PM.add(createPerformanceConstantPropagation());
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  PM.add(createSimplifyCFG());

  // Perform retain/release code motion and run the first ARC optimizer.
  PM.add(createLoadStoreOpts());
  PM.add(createCodeMotion());
  PM.add(createEnumSimplification());
  PM.add(createGlobalARCOpts());

  // Devirtualize.
  PM.add(createDevirtualization());
  PM.add(createGenericSpecializer());
  PM.add(createSILLinker());

  // Use either the early inliner that does not inline functions with defined
  // semantics or the late performance inliner that inlines everything.
  PM.add(useEarlyInliner ? createEarlyInliner() : createPerfInliner());
  PM.add(createSimplifyCFG());
  PM.add(createGlobalARCOpts());
}


void swift::runSILOptimizationPasses(SILModule &Module,
                                     const SILOptions &Options) {
  if (Options.DebugSerialization) {
    SILPassManager PM(&Module, Options);
    registerAnalysisPasses(PM, &Module);
    PM.add(createSILLinker());
    PM.run();
    return;
  }

  // Start by specializing generics and by cloning functions from stdlib.
  SILPassManager GenericsPM(&Module, Options);
  registerAnalysisPasses(GenericsPM, &Module);
  GenericsPM.add(createSILLinker());
  GenericsPM.add(createGenericSpecializer());
  GenericsPM.run();

  // Run two iteration of the high-level SSA pass mananger.
  SILPassManager HighLevelSILPM(&Module, Options);
  ConstructSSAPassManager(HighLevelSILPM, Module, true);
  HighLevelSILPM.runOneIteration();
  HighLevelSILPM.runOneIteration();

#if 0
  // TODO: figure out what is really needed here to simplify the enums I still
  // see sticking around without this iteration.
  HighLevelSILPM.runOneIteration();
  // TODO: this should run as part of the last iteration of the previous
  // pass manager so that we can reuse analysis info.
  SILPassManager LoopPM(&Module, Options);
  ConstructHighLevelLoopOptPassManager(LoopPM, Module);
  LoopPM.runOneIteration();
#endif


  // Run two iteration of the low-level SSA pass mananger.
  SILPassManager LowLevelSILPM(&Module, Options);
  ConstructSSAPassManager(LowLevelSILPM, Module, false);
  LowLevelSILPM.runOneIteration();
  LowLevelSILPM.runOneIteration();

  // Perform lowering optimizations.
  SILPassManager LoweringPM(&Module, Options);
  registerAnalysisPasses(LoweringPM, &Module);
  LoweringPM.add(createDeadFunctionElimination());
  LoweringPM.add(createDeadObjectElimination());

  // Hoist globals out of loops.
  LoweringPM.add(createGlobalOpt());

  // Insert inline caches for virtual calls.
  LoweringPM.add(createDevirtualization());
  LoweringPM.add(createInlineCaches());
  LoweringPM.run();

  // Run another iteration of the SSA optimizations to optimize the
  // devirtualized inline caches.
  LowLevelSILPM.invalidateAnalysis(SILAnalysis::InvalidationKind::All);
  LowLevelSILPM.runOneIteration();

#if 0
  SILPassManager LowLevelLoopPM(&Module, Options);
  // TODO: this should run as part of the last iteration of the previous
  // pass manager so that we can reuse analysis info.
  ConstructLowLevelLoopOptPassManager(LowLevelLoopPM, Module);
  LowLevelLoopPM.runOneIteration();
#endif

  // Invalidate the SILLoader and allow it to drop references to SIL functions.
  Module.invalidateSILLoader();
  performSILElimination(&Module);

  // Gather instruction counts if we are asked to do so.
  if (Options.PrintInstCounts) {
    SILPassManager PrinterPM(&Module, Options);
    PrinterPM.add(createSILInstCount());
    PrinterPM.runOneIteration();
  }

  DEBUG(Module.verify());
}
