//===--- PassManager.h  - Swift Pass Manager --------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "llvm/Support/Casting.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/ErrorHandling.h"
#include <vector>

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_PASSMANAGER_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_PASSMANAGER_H

namespace swift {

class SILFunction;
class SILFunctionTransform;
class SILModule;
class SILModuleTransform;
class SILOptions;
class SILTransform;

/// \brief The SIL pass manager.
class SILPassManager {
  /// The module that the pass manager will transform.
  SILModule *Mod;

  /// The list of transformations to run.
  llvm::SmallVector<SILTransform *, 16> Transformations;

  /// A list of registered analysis.
  llvm::SmallVector<SILAnalysis *, 16> Analysis;

  /// The worklist of functions to be processed by function passes.
  std::vector<SILFunction *> FunctionWorklist;

  // Name of the current optimization stage for diagnostics.
  std::string StageName;

  /// The number of passes run so far.
  unsigned NumPassesRun = 0;

  /// Number of optimization iterations run.
  unsigned NumOptimizationIterations = 0;

  /// A mask which has one bit for each pass. A one for a pass-bit means that
  /// the pass doesn't need to run, because nothing has changed since the
  /// previous run of that pass.
  typedef std::bitset<(size_t)PassKind::AllPasses_Last + 1> CompletedPasses;
  
  /// A completed-passes mask for each function.
  llvm::DenseMap<SILFunction *, CompletedPasses> CompletedPassesMap;

  /// Set to true when a pass invalidates an analysis.
  bool CurrentPassHasInvalidated = false;

  /// True if we need to stop running passes and restart again on the
  /// same function.
  bool RestartPipeline = false;

public:
  /// C'tor. It creates and registers all analysis passes, which are defined
  /// in Analysis.def.
  SILPassManager(SILModule *M, llvm::StringRef Stage = "");

  const SILOptions &getOptions() const;

  /// \brief Searches for an analysis of type T in the list of registered
  /// analysis. If the analysis is not found, the program terminates.
  template<typename T>
  T *getAnalysis() {
    for (SILAnalysis *A : Analysis)
      if (T *R = llvm::dyn_cast<T>(A))
        return R;

    llvm_unreachable("Unable to find analysis for requested type.");
  }

  /// \returns the module that the pass manager owns.
  SILModule *getModule() { return Mod; }

  /// \brief Run the transformations on the module.
  void run();

  /// \brief Run one iteration of the optimization pipeline.
  void runOneIteration();

  /// \brief Add a function to the function pass worklist.
  void addFunctionToWorklist(SILFunction *F) {
    assert(F && F->isDefinition() && F->shouldOptimize() &&
           "Expected optimizable function definition!");
    FunctionWorklist.push_back(F);
  }

  /// \brief Restart the function pass pipeline on the same function
  /// that is currently being processed.
  void restartWithCurrentFunction(SILTransform *T);
  void clearRestartPipeline() { RestartPipeline = false; }
  bool shouldRestartPipeline() { return RestartPipeline; }

  ///  \brief Broadcast the invalidation of the module to all analysis.
  void invalidateAnalysis(SILAnalysis::InvalidationKind K) {
    assert(K != SILAnalysis::InvalidationKind::Nothing &&
           "Invalidation call must invalidate some trait");

    for (auto AP : Analysis)
      if (!AP->isLocked())
        AP->invalidate(K);

    CurrentPassHasInvalidated = true;

    // Assume that all functions have changed. Clear all masks of all functions.
    CompletedPassesMap.clear();
  }

  /// \brief Add the function to the function pass worklist.
  void notifyTransformationOfFunction(SILFunction *F) {
    addFunctionToWorklist(F);
  }

  /// \brief Iterate over all analysis and notify them of the function.
  /// This function does not necessarily have to be newly created function. It
  /// is the job of the analysis to make sure no extra work is done if the
  /// particular analysis has been done on the function.
  void notifyAnalysisOfFunction(SILFunction *F) {
    for (auto AP : Analysis)
      AP->notifyAnalysisOfFunction(F);
  }

  /// \brief Broadcast the invalidation of the function to all analysis.
  void invalidateAnalysis(SILFunction *F,
                          SILAnalysis::InvalidationKind K) {
    // Invalidate the analysis (unless they are locked)
    for (auto AP : Analysis)
      if (!AP->isLocked())
        AP->invalidate(F, K);
    
    CurrentPassHasInvalidated = true;
    // Any change let all passes run again.
    CompletedPassesMap[F].reset();
  }

  /// \brief Broadcast the invalidation of the function to all analysis.
  /// And we also know this function is dead and will be removed from the
  /// module.
  void invalidateAnalysisForDeadFunction(SILFunction *F,
                                         SILAnalysis::InvalidationKind K) {
    // Invalidate the analysis (unless they are locked)
    for (auto AP : Analysis)
      if (!AP->isLocked())
        AP->invalidateForDeadFunction(F, K);
    
    CurrentPassHasInvalidated = true;
    // Any change let all passes run again.
    CompletedPassesMap[F].reset();
  }

  /// \brief Reset the state of the pass manager and remove all transformation
  /// owned by the pass manager. Analysis passes will be kept.
  void resetAndRemoveTransformations();

  // Sets the name of the current optimization stage used for debugging.
  void setStageName(llvm::StringRef NextStage = "");

  /// D'tor.
  ~SILPassManager();

  /// Verify all analyses.
  void verifyAnalyses() const {
    for (auto *A : Analysis) {
      A->verify();
    }
  }

  /// Verify all analyses, limiting the verification to just this one function
  /// if possible.
  ///
  /// Discussion: We leave it up to the analyses to decide how to implement
  /// this. If no override is provided the SILAnalysis should just call the
  /// normal verify method.
  void verifyAnalyses(SILFunction *F) const {
    for (auto *A : Analysis) {
      A->verify(F);
    }
  }

  /// Add a pass of a specific kind.
  void addPass(PassKind Kind);

  /// Add a pass with a given name.
  void addPassForName(StringRef Name);

  // Each pass gets its own add-function.
#define PASS(ID, NAME, DESCRIPTION) void add##ID();
#include "Passes.def"

  typedef llvm::ArrayRef<SILFunctionTransform *> PassList;
private:
  /// Run the SIL module transform \p SMT over all the functions in
  /// the module.
  void runModulePass(SILModuleTransform *SMT);

  /// Run the passes in \p FuncTransforms on the function \p F.
  void runPassesOnFunction(PassList FuncTransforms, SILFunction *F,
                           bool runToCompletion);

  /// Run the passes in \p FuncTransforms. Return true
  /// if the pass manager requested to stop the execution
  /// of the optimization cycle (this is a debug feature).
  void runFunctionPasses(PassList FuncTransforms);

  /// A helper function that returns (based on SIL stage and debug
  /// options) whether we should continue running passes.
  bool continueTransforming();

  /// Return true if all analyses are unlocked.
  bool analysesUnlocked();

  /// Displays the call graph in an external dot-viewer.
  /// This function is meant for use from the debugger.
  /// When asserts are disabled, this is a NoOp.
  void viewCallGraph();
};

} // end namespace swift

#endif
