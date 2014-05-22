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
#include "swift/AST/SILOptions.h"
#include "swift/SILAnalysis/Analysis.h"

#ifndef SWIFT_SILPASSES_PASSMANAGER_H
#define SWIFT_SILPASSES_PASSMANAGER_H

namespace swift {
  class SILModule;
  class SILFunction;
  class SILTransform;
  class SILFunctionTransform;

  /// \brief The SIL pass manager.
  class SILPassManager {
    /// When set to True the pass manager will re-run the transformation pipe.
    bool anotherIteration;
    /// The module that the pass manager will transform.
    SILModule *Mod;
    /// Keep a copy of the options used to configure passes. This is mutable.
    SILOptions Options;

    /// The list of transformations to run.
    llvm::SmallVector<SILTransform*, 8> Transformations;

    /// A list of registered analysis.
    llvm::SmallVector<SILAnalysis*, 4> Analysis;
    
    /// The number of passes run so far.
    unsigned NumPassesRun = 0;

    /// Number of optimization iterations run.
    unsigned NumOptimizationIterations = 0;

  public:
    /// C'tor
    SILPassManager(SILModule *M, SILOptions Opts) :
      anotherIteration(false), Mod(M), Options(std::move(Opts)) {
      registerAnalysis(new CompleteFunctions(Mod));
    }

    SILOptions &getOptions() {
      return Options;
    }

    /// \brief Searches for an analysis of type T in the list of registered
    /// analysis. If the analysis is not found, the program terminates.
    template<typename T>
    T* getAnalysis() {
      for (SILAnalysis *A : Analysis)
        if (T* R = llvm::dyn_cast<T>(A))
          return R;

      llvm_unreachable("Unable to find analysis for requested type.");
    }

    /// \brief Add another transformation to the pipe. This transfers the
    /// ownership of the transformation to the pass manager that will delete it
    /// when done.
    void add(SILTransform *T) { Transformations.push_back(T); }

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
    void invalidateAnalysis(SILAnalysis::InvalidationKind K) {
      for (auto AP : Analysis)
        AP->invalidate(K);
    }

    /// \brief Broadcast the invalidation of the function to all analysis.
    void invalidateAnalysis(SILFunction *F,
                            SILAnalysis::InvalidationKind K) {
      for (auto AP : Analysis)
        AP->invalidate(F, K);
    }

    /// D'tor.
    ~SILPassManager();

  protected:
    void runFunctionPasses(
      llvm::ArrayRef<SILFunctionTransform*> FuncTransforms);
  };

} // end namespace swift

#endif
