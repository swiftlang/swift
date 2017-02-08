//===--- Analysis.h  - Swift Analysis ---------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/Casting.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/SIL/Notifications.h"
#include <vector>

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_ANALYSIS_H

namespace swift {
  class SILModule;
  class SILFunction;
  class SILPassManager;

  /// The base class for all SIL-level analysis.
  class SILAnalysis : public DeleteNotificationHandler {
  public:
    /// This is a list of values that allow passes to communicate to analysis
    /// which traits of the code were invalidated. Based on this information
    /// the analysis can decide if it needs to be invalidated. This information
    /// may refer to a specific function or to the whole module depending on
    /// the context in which it is used.
    enum InvalidationKind : unsigned {
      /// The pass does not change anything.
      Nothing = 0x0,

      /// The pass created, deleted or rearranged some instructions in a
      /// function.
      Instructions = 0x1,

      /// The pass modified some calls (apply instructions).
      ///
      /// The intention of this invalidation kind is to allow analysis that
      /// rely on a specific call graph structure to recompute themselves.
      Calls = 0x2,

      /// A pass has invalidated some branches in the program.
      ///
      /// The intention of this invalidation kind is to tell analyses like the
      /// Dominance Analysis and the PostOrder Analysis that the underlying CFG
      /// has been modified.
      Branches = 0x4,

      /// The pass delete or created new functions.
      ///
      /// The intent behind this is so that analyses that cache
      /// SILFunction* to be able to be invalidated and later
      /// recomputed so that they are not holding dangling pointers.
      Functions = 0x8,

      /// Convenience states:
      FunctionBody = Calls | Branches | Instructions,

      CallsAndInstructions = Calls | Instructions,

      BranchesAndInstructions = Branches | Instructions,

      Everything = Functions | Calls | Branches | Instructions,
    };

    /// A list of the known analysis.
    enum class AnalysisKind {
#define ANALYSIS(NAME) NAME,
#include "Analysis.def"
    };

  private:
    /// Stores the kind of derived class.
    const AnalysisKind Kind;

    /// A lock that prevents the invalidation of this analysis. When this
    /// variable is set to True then the PassManager should not invalidate
    /// this analysis.
    bool invalidationLock;

  public:

    /// Returns the kind of derived class.
    AnalysisKind getKind() const { return Kind; }

    /// C'tor.
    SILAnalysis(AnalysisKind K) : Kind(K), invalidationLock(false) {}

    /// D'tor.
    virtual ~SILAnalysis() {}
    
    /// Can be used to retrieve other analysis passes from \p PM, which this
    /// analysis depends on.
    virtual void initialize(SILPassManager *PM) { }

    /// Lock the analysis. This means that invalidation messages are ignored.
    void lockInvalidation() {invalidationLock = true; }

    /// Unlock the analysis. This means that invalidation messages are handled.
    void unlockInvalidation() {invalidationLock = false; }

    /// Return True if this analysis is locked and should not be invalidated.
    bool isLocked() { return invalidationLock; }

    /// Invalidate all information in this analysis.
    virtual void invalidate(InvalidationKind K) {}

    /// Invalidate all of the information for a specific function.
    virtual void invalidate(SILFunction *F, InvalidationKind K) {}

    /// Invalidate all of the information for a specific function. Also, we
    /// know that this function is a dead function and going to be deleted from
    /// the module.
    virtual void invalidateForDeadFunction(SILFunction *F, InvalidationKind K) {
      // Call the normal invalidate function unless overridden by specific
      // analysis.
      invalidate(F, K);
    }
    
    /// Notify the analysis about a newly created function.
    virtual void notifyAnalysisOfFunction(SILFunction *F) {}

    /// Verify the state of this analysis.
    virtual void verify() const {}

    /// Verify the state of this analysis limited to this one function if
    /// possible.
    ///
    /// By default this just calls the module verify function as a sensible
    /// default so that only functions which are able to provide the function
    /// specific verification will do so.
    virtual void verify(SILFunction *F) const { verify(); }

    /// Verify that the function \p F can be used by the analysis.
    static void verifyFunction(SILFunction *F);
  };

  // RAII helper for locking analyses.
  class AnalysisPreserver {
    SILAnalysis *Analysis;
    public:
    AnalysisPreserver(SILAnalysis *A) : Analysis(A) {
      Analysis->lockInvalidation();
    }
    ~AnalysisPreserver() {
      Analysis->unlockInvalidation();
    }
  };

  /// An abstract base class that implements the boiler plate of caching and
  /// invalidating analysis for specific functions.
  template<typename AnalysisTy>
  class FunctionAnalysisBase : public SILAnalysis {
  protected:
    typedef llvm::DenseMap<SILFunction *, AnalysisTy *> StorageTy;

    /// Maps functions to their analysis provider.
    StorageTy Storage;

    /// Construct a new empty analysis for a specific function \p F.
    virtual AnalysisTy *newFunctionAnalysis(SILFunction *F) = 0;

    /// Return True if the analysis should be invalidated given trait \K is
    /// preserved.
    virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) = 0;

    /// A stub function that verifies the specific AnalysisTy \p A. This is
    /// meant to be overridden by subclasses.
    virtual void verify(AnalysisTy *A) const {}

  public:
    /// Returns an analysis provider for a specific function \p F.
    AnalysisTy *get(SILFunction *F) {

      // Check that the analysis can handle this function.
      verifyFunction(F);

      auto &it = Storage.FindAndConstruct(F);
      if (!it.second)
        it.second = newFunctionAnalysis(F);
      return it.second;
    }

    virtual void invalidate(SILAnalysis::InvalidationKind K) override {
      if (!shouldInvalidate(K)) return;

      for (auto D : Storage)
        delete D.second;

      Storage.clear();
    }

    virtual void invalidate(SILFunction *F,
                            SILAnalysis::InvalidationKind K) override {
      if (!shouldInvalidate(K)) return;

      auto &it = Storage.FindAndConstruct(F);
      if (it.second) {
        delete it.second;
        it.second = nullptr;
      }
    }

    FunctionAnalysisBase() {}
    virtual ~FunctionAnalysisBase() {
      for (auto D : Storage)
        delete D.second;
    }
    FunctionAnalysisBase(AnalysisKind K) : SILAnalysis(K), Storage() {}
    FunctionAnalysisBase(const FunctionAnalysisBase &) = delete;
    FunctionAnalysisBase &operator=(const FunctionAnalysisBase &) = delete;

    /// Verify all of the AnalysisTy for all functions.
    ///
    /// This is not meant to be overridden by subclasses. See "void
    /// verify(AnalysisTy *A)".
    virtual void verify() const override final {
      for (auto Iter : Storage) {
        if (!Iter.second)
          continue;
        verify(Iter.second);
      }
    }

    /// Verify the AnalysisTy that we have stored for the specific function \p
    /// F.
    ///
    /// This is not meant to be overridden by subclasses. See "void
    /// verify(AnalysisTy *A)".
    virtual void verify(SILFunction *F) const override final {
      auto Iter = Storage.find(F);
      if (Iter == Storage.end())
        return;
      if (!Iter->second)
        return;
      verify(Iter->second);
    }
  };

#define ANALYSIS(NAME)                                                         \
  SILAnalysis *create##NAME##Analysis(SILModule *);
#include "Analysis.def"

} // end namespace swift

#endif

