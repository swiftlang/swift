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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/SILPasses/Analysis.h"
#include "swift/SILPasses/Transforms.h"

#ifndef SWIFT_SILPASSES_PASSMANAGER_H
#define SWIFT_SILPASSES_PASSMANAGER_H

namespace swift {
  class SILModule;
  class SILFunction;
  class SILOptions;
  class SILTransform;
  class SILPassManager;

  /// The SIL pass manager.
  class SILPassManager {
  private:
    /// When set to True the pass manager will re-run the transformation pipe.
    bool anotherIteration;
    SILModule *Mod;

    /// The list of transformations to run.
    llvm::SmallVector<SILTransform*, 8> Transformations;

    /// A list of registered analysis.
    llvm::SmallVector<SILAnalysis*, 4> Analysis;

  public:
    /// C'tor
    SILPassManager(SILModule *M) : anotherIteration(false), Mod(M) {}

    template<typename T>
    T* getAnalysis() {
      for (SILAnalysis *A : Analysis)
        if (T* R = llvm::dyn_cast<T>(A))
          return R;

      return nullptr;
    }

    /// Add another transformation to the pipe. This transfers the ownership
    /// of the transformation to the pass manager that will delete it when done.
    void add(SILTransform *T) { Transformations.push_back(T); }

    /// Register another analysis. This transfers the ownership of the analysis
    /// to the pass manager that will delete it when done.
    void registerAnalysis(SILAnalysis *A) {
      Analysis.push_back(A);
    }

    /// Run the transformations on the module \p Mod.
    void run();

    /// Request another invocation of the transformation pipeline.
    void scheduleAnotherIteration() { anotherIteration = true; }

    /// Broadcast the invalidation of the module to all analysis.
    void invalidateAllAnalysis(SILAnalysis::InvalidationKind K) {
      for (auto AP : Analysis)
        AP->invalidate(K);
    }

    /// Broadcast the invalidation of the function to all analysis.
    void invalidateAllAnalisys(SILFunction *F,
                               SILAnalysis::InvalidationKind K) {
      for (auto AP : Analysis)
        AP->invalidate(F, K);
    }

    /// D'tor.
    ~SILPassManager() {
      // Free all transformations.
      for (auto T : Transformations)
        delete T;

      // delete the analyis.
      for (auto A : Analysis)
        delete A;
    }
  };

} // end namespace swift

#endif

