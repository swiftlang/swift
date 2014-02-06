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
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILPasses/Transforms.h"

#ifndef SWIFT_SILPASSES_PASSMANAGER_H
#define SWIFT_SILPASSES_PASSMANAGER_H

namespace swift {
  class SILModule;
  class SILFunction;
  class SILTransform;
  
  /// \brief The SIL pass manager.
  class SILPassManager {
    /// When set to True the pass manager will re-run the transformation pipe.
    bool anotherIteration;
    /// The module that the pass manager will transform.
    SILModule *Mod;

    /// The list of transformations to run.
    llvm::SmallVector<SILTransform*, 8> Transformations;

    /// A list of registered analysis.
    llvm::SmallVector<SILAnalysis*, 4> Analysis;

  public:
    /// C'tor
    SILPassManager(SILModule *M) : anotherIteration(false), Mod(M) {}

    /// \brief Searches for an analysis of type T in the list of registered
    /// analysis.
    template<typename T>
    T* getAnalysis() {
      for (SILAnalysis *A : Analysis)
        if (T* R = llvm::dyn_cast<T>(A))
          return R;

      return nullptr;
    }

    /// \brief Add another transformation to the pipe. This transfers the
    /// ownership of the transformation to the pass manager that will delete it
    /// when done.
    void add(SILTransform *T) { Transformations.push_back(T); }

    /// \brief  Register another analysis. This transfers the ownership of the
    /// analysis to the pass manager that will delete it when done.
    void registerAnalysis(SILAnalysis *A) {
      Analysis.push_back(A);
    }

    /// \brief Run the transformations on the module \p Mod.
    void run();

    /// \brief Request another invocation of the transformation pipeline.
    void scheduleAnotherIteration() { anotherIteration = true; }

    ///  \brief Broadcast the invalidation of the module to all analysis.
    void invalidateAllAnalysis(SILAnalysis::InvalidationKind K) {
      for (auto AP : Analysis)
        AP->invalidate(K);
    }

    /// \brief Broadcast the invalidation of the function to all analysis.
    void invalidateAllAnalysis(SILFunction *F,
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

