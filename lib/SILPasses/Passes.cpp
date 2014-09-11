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
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

llvm::cl::opt<bool>
    SILViewCFG("sil-view-cfg", llvm::cl::init(false),
               llvm::cl::desc("Enable the sil cfg viewer pass"));

using namespace swift;

// Enumerates the optimization kinds that we do in SIL.
enum OptimizationLevelKind {
  LowLevel,
  HighLevel,
};

static void registerAnalysisPasses(SILPassManager &PM) {
  SILModule *Mod = PM.getModule();
  PM.registerAnalysis(createCallGraphAnalysis(Mod));
  PM.registerAnalysis(createAliasAnalysis(Mod));
  PM.registerAnalysis(createDominanceAnalysis(Mod));
  PM.registerAnalysis(createLoopInfoAnalysis(Mod, &PM));
  PM.registerAnalysis(createInductionVariableAnalysis(Mod));
  PM.registerAnalysis(createPostOrderAnalysis(Mod));
  PM.registerAnalysis(createClassHierarchyAnalysis(Mod));
  PM.registerAnalysis(createRCIdentityAnalysis(Mod));
}

bool swift::runSILDiagnosticPasses(SILModule &Module,
                                   const SILOptions &Options) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;

  auto &Ctx = Module.getASTContext();

  SILPassManager PM(&Module, Options);
  registerAnalysisPasses(PM);
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

void AddSimplifyCFGSILCombine(SILPassManager &PM) {
  PM.add(createSimplifyCFG());
  // Jump threading can expose opportunity for silcombine (enum -> is_enum_tag->
  // cond_br).
  PM.add(createSILCombine());
  // Which can expose opportunity for simplifcfg.
  PM.add(createSimplifyCFG());
}

/// Perform semantic annotation/loop base optimizations.
void AddHighLevelLoopOptPasses(SILPassManager &PM) {
  // Perform classsic SSA optimizations for cleanup.
  PM.add(createLowerAggregate());
  PM.add(createSILCombine());
  PM.add(createSROA());
  PM.add(createMem2Reg());
  PM.add(createDCE());
  PM.add(createSILCombine());
  AddSimplifyCFGSILCombine(PM);

  // Run high-level loop opts.
  PM.add(createLoopRotatePass());

  // Cleanup.
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  PM.add(createSimplifyCFG());
  PM.add(createABCOpt());
  // Cleanup.
  PM.add(createDCE());
  PM.add(createCOWArrayOpts());
}

void AddLowLevelLoopOptPasses(SILPassManager &PM) {
  PM.add(createLICMPass());
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  PM.add(createSimplifyCFG());
}

void AddSSAPasses(SILPassManager &PM, OptimizationLevelKind OpLevel) {
  AddSimplifyCFGSILCombine(PM);
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
  AddSimplifyCFGSILCombine(PM);

  // Perform retain/release code motion and run the first ARC optimizer.
  PM.add(createGlobalLoadStoreOpts());
  PM.add(createCodeMotion());
  PM.add(createGlobalARCOpts());

  // Devirtualize.
  PM.add(createDevirtualization());
  PM.add(createGenericSpecializer());
  PM.add(createSILLinker());

  // Use either the early inliner that does not inline functions with defined
  // semantics or the late performance inliner that inlines everything.
  PM.add(OpLevel == OptimizationLevelKind::HighLevel ? createEarlyInliner() :
         createPerfInliner());
  PM.add(createSimplifyCFG());
  PM.add(createGlobalARCOpts());
}


void swift::runSILOptimizationPasses(SILModule &Module,
                                     const SILOptions &Options) {
  if (Options.DebugSerialization) {
    SILPassManager PM(&Module, Options);
    registerAnalysisPasses(PM);
    PM.add(createSILLinker());
    PM.run();
    return;
  }

  SILPassManager PM(&Module, Options);
  registerAnalysisPasses(PM);

  // Start by specializing generics and by cloning functions from stdlib.
  PM.add(createSILLinker());
  PM.add(createGenericSpecializer());
  PM.run();
  PM.resetAndRemoveTransformations();

  // Run two iterations of the high-level SSA passes.
  AddSSAPasses(PM, OptimizationLevelKind::HighLevel);
  PM.runOneIteration();
  PM.runOneIteration();

  // Run the high-level loop optimization passes.
  PM.resetAndRemoveTransformations();
  AddHighLevelLoopOptPasses(PM);
  PM.runOneIteration();
  PM.resetAndRemoveTransformations();

  // Run two iterations of the low-level SSA passes.
  AddSSAPasses(PM, OptimizationLevelKind::LowLevel);
  PM.runOneIteration();
  PM.runOneIteration();
  PM.resetAndRemoveTransformations();

  // Perform lowering optimizations.
  PM.add(createDeadFunctionElimination());
  PM.add(createDeadObjectElimination());

  // Hoist globals out of loops.
  PM.add(createGlobalOpt());

  // Propagate constants into closures and convert to static dispatch.  This
  // should run after specialization and inlining because we don't want to
  // specialize a call that can be inlined. It should run before
  // ClosureSpecialization, because constant propagation is more effective.  At
  // least one round of SSA optimization and inlining should run after this to
  // take advantage of static dispatch.
  PM.add(createCapturePropagation());

  // Specialize closure.
  PM.add(createClosureSpecializer());

  // Insert inline caches for virtual calls.
  PM.add(createDevirtualization());
  PM.add(createInlineCaches());

  // Optimize function signatures if we are asked to. This is disabled by
  // default.
  //
  // We do this late since it is a pass like the inline caches that we only want
  // to run once very late. Make sure to run at least one round of the ARC
  // optimizer after this.
  if (Options.EnableFuncSigOpts)
    PM.add(createFunctionSignatureOpts());

  PM.run();
  PM.resetAndRemoveTransformations();

  // Run another iteration of the SSA optimizations to optimize the
  // devirtualized inline caches and constants propagated into closures
  // (CapturePropagation).
  AddSSAPasses(PM, OptimizationLevelKind::LowLevel);
  PM.runOneIteration();

  PM.resetAndRemoveTransformations();
  AddLowLevelLoopOptPasses(PM);
  PM.runOneIteration();

  // Invalidate the SILLoader and allow it to drop references to SIL functions.
  Module.invalidateSILLoader();
  performSILElimination(&Module);

  // Gather instruction counts if we are asked to do so.
  if (Options.PrintInstCounts) {
    SILPassManager PrinterPM(&Module, Options);
    PrinterPM.add(createSILInstCount());
    PrinterPM.runOneIteration();
  }

  // Call the CFG viewer.
  if (SILViewCFG) {
    PM.resetAndRemoveTransformations();
    PM.add(createSILCFGPrinter());
    PM.runOneIteration();
  }

  DEBUG(Module.verify());
}
