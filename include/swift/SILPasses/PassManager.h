//===-- PassManager.h  - Swift Pass Manager ---------------------*- C++ -*-===//
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

#include "llvm/Support/Casting.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/ErrorHandling.h"
#include "swift/SILAnalysis/Analysis.h"
#include "Passes.h"

#ifndef SWIFT_SILPASSES_PASSMANAGER_H
#define SWIFT_SILPASSES_PASSMANAGER_H

namespace swift {

class SILFunction;
class SILFunctionTransform;
class SILModule;
class SILOptions;
class SILTransform;

/// \brief The SIL pass manager.
class SILPassManager {
  /// When set to True the pass manager will re-run the transformation pipe.
  bool anotherIteration;

  /// The module that the pass manager will transform.
  SILModule *Mod;

  /// The list of transformations to run.
  llvm::SmallVector<SILTransform*, 16> Transformations;

  /// A list of registered analysis.
  llvm::SmallVector<SILAnalysis*, 16> Analysis;

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
  bool currentPassHasInvalidated = false;
  
  public:
  /// C'tor
  SILPassManager(SILModule *M, llvm::StringRef Stage = "") :
    anotherIteration(false), Mod(M), StageName(Stage) {
  }

  const SILOptions &getOptions() const;

  /// \brief Searches for an analysis of type T in the list of registered
  /// analysis. If the analysis is not found, the program terminates.
  template<typename T>
  T* getAnalysis() {
    for (SILAnalysis *A : Analysis)
      if (T* R = llvm::dyn_cast<T>(A))
        return R;

    llvm_unreachable("Unable to find analysis for requested type.");
  }

  /// \returns the module that the pass manager owns.
  SILModule *getModule() { return Mod; }

  /// \brief  Register another analysis. This transfers the ownership of the
  /// analysis to the pass manager that will delete it when done.
  void registerAnalysis(SILAnalysis *A) { Analysis.push_back(A); }

  /// \brief Run the transformations on the module.
  void run();

  /// \brief Run one iteration of the optimization pipeline.
  void runOneIteration();

  /// \brief Request another invocation of the transformation pipeline.
  void scheduleAnotherIteration() { anotherIteration = true; }

  ///  \brief Broadcast the invalidation of the module to all analysis.
  void invalidateAnalysis(SILAnalysis::PreserveKind K) {
    for (auto AP : Analysis)
      if (!AP->isLocked())
        AP->invalidate(K);
    
    currentPassHasInvalidated = true;
    
    // Assume that all functions have changed. Clear all masks of all functions.
    CompletedPassesMap.clear();
  }

  /// \brief Broadcast the invalidation of the function to all analysis.
  void invalidateAnalysis(SILFunction *F,
                          SILAnalysis::PreserveKind K) {
    // Invalidate the analysis (unless they are locked)
    for (auto AP : Analysis)
      if (!AP->isLocked())
        AP->invalidate(F, K);
    
    currentPassHasInvalidated = true;
    // Any change let all passes run again.
    CompletedPassesMap[F].reset();
  }

  /// \brief Reset the state of the pass manager and remove all transformation
  /// owned by the pass manager. Anaysis passes will be kept.
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
#define PASS(ID) void add##ID();
#include "Passes.def"

  protected:
  bool runFunctionPasses(
    llvm::ArrayRef<SILFunctionTransform*> FuncTransforms);
};

} // end namespace swift

#endif
