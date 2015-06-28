//===-- Analysis.h  - Swift Analysis ----------------------------*- C++ -*-===//
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
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include <vector>

#ifndef SWIFT_SILANALYSIS_ANALYSIS_H
#define SWIFT_SILANALYSIS_ANALYSIS_H

namespace swift {
  class SILModule;
  class SILFunction;
  class SILPassManager;

  /// The base class for all SIL-level analysis.
  class SILAnalysis {
  public:
    /// This is a list of values that allow passes to communicate to analysis
    /// which traits of the code were preserved. Based on this information
    /// the analysis can decide if it needs to be invalidated. This information
    /// may refer to a specific function or to a list of functions depending
    /// on the context in which it is used.
    enum PreserveKind : unsigned {
      /// The pass does not preserve any analysis trait.
      Nothing = 0x0,

      /// The pass did not modify any call instructions.
      ///
      /// The intention of this preserve kind is twofold:
      ///
      /// 1. Allow analyses like the CallGraphAnalysis that store pointers to
      /// apply instructions and SILFunctions to invalidate their stored
      /// information, preventing dangling pointers.
      /// 2. Allow analyses like the CallGraphAnalysis to know to search for new
      /// edges in the callgraph.
      Calls = 0x1,

      /// The pass did not modify any branch edges in the CFG beyond reordering
      /// them in the successor or predecessor list of a BB.
      ///
      /// The intention of this preserve kind is to tell analyses like the
      /// Dominance Analysis and the Post Order Analysis that the underlying CFG
      /// has been changed up to reordering of branch edges in the successor or
      /// predecessor lists of a BB. Unlike the "Calls" preservation kind this
      /// is not meant to prevent dangling pointers. This is because all CFG
      /// related analyses are able to store basic blocks instead of
      /// terminators. This allows for certain useful transformations to occur
      /// without requiring recomputation of the dominator tree or CFG post
      /// order. Some of these transformations are:
      ///
      /// 1. Converting a branch from one type of branch to another type that
      /// preserves the CFG. Ex: Convering a checked_cast_addr_br =>
      /// checked_cast_br.
      /// 2. Canonicalizing a conditional branch by inverting its branch
      /// condition.
      Branches = 0x2,

      /// This is a list of combined traits that is defined to make the use of
      /// the invalidation API more convenient.
      ProgramFlow = Calls | Branches, // The pass changed some instructions but
                                      // did not change the overall flow
                                      // of the code.

      /// This Top in case we add a different top besides ProgramFlow.
      All = ProgramFlow,
    };

    /// A list of the known analysis.
    enum class AnalysisKind {
      CompleteFuncs,
      CallGraph,
      Dominance,
      PostDominance,
      Alias,
      LoopInfo,
      IVAnalysis,
      PostOrder,
      ClassHierarchyAnalysis,
      RCIdentity,
      Destructor
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

    /// Lock the analysis. This means that invalidation messages are ignored.
    void lockInvalidation() {invalidationLock = true; }

    /// Unlock the analysis. This means that invalidation messages are handled.
    void unlockInvalidation() {invalidationLock = false; }

    /// Return True if this analysis is locked and should not be invalidated.
    bool isLocked() { return invalidationLock; }

    /// Invalidate all information in this analysis.
    virtual void invalidate(PreserveKind K) {}

    /// Invalidate all of the information for a specific function.
    virtual void invalidate(SILFunction *F, PreserveKind K) {}

    /// Verify the state of this analysis.
    virtual void verify() const {}

    /// Verify the state of this analysis limited to this one function if
    /// possible.
    ///
    /// By default this just calls the module verify function as a sensible
    /// default so that only functions which are able to provide the function
    /// specific verification will do so.
    virtual void verify(SILFunction *F) const { verify(); }
  };

  /// An abstract base class that implements the boiler plate of cacheing and
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
    virtual bool shouldInvalidate(SILAnalysis::PreserveKind K) = 0;

    /// A stub function that verifies the specific AnalysisTy \p A. This is
    /// meant to be overridden by subclasses.
    virtual void verify(AnalysisTy *A) const {}

  public:
    /// Returns an analysis provider for a specific function \p F.
    AnalysisTy *get(SILFunction *F) {
      auto &it = Storage.FindAndConstruct(F);
      if (!it.second)
        it.second = newFunctionAnalysis(F);
      return it.second;
    }

    virtual void invalidate(SILAnalysis::PreserveKind K) override {
      if (!shouldInvalidate(K)) return;

      for (auto D : Storage)
        delete D.second;

      Storage.clear();
    }

    virtual void invalidate(SILFunction *F,
                            SILAnalysis::PreserveKind K) override {
      if (!shouldInvalidate(K)) return;

      auto &it = Storage.FindAndConstruct(F);
      if (it.second) {
        delete it.second;
        it.second = nullptr;
      }
    }

    FunctionAnalysisBase(AnalysisKind K) : SILAnalysis(K), Storage() {}
    FunctionAnalysisBase(const FunctionAnalysisBase &) = delete;
    FunctionAnalysisBase &operator=(const FunctionAnalysisBase &) = delete;
    FunctionAnalysisBase() {}

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

  SILAnalysis *createCallGraphAnalysis(SILModule *M);
  SILAnalysis *createAliasAnalysis(SILModule *M);
  SILAnalysis *createDominanceAnalysis(SILModule *M);
  SILAnalysis *createPostDominanceAnalysis(SILModule *M);
  SILAnalysis *createLoopInfoAnalysis(SILModule *M, SILPassManager *PM);
  SILAnalysis *createInductionVariableAnalysis(SILModule *M);
  SILAnalysis *createPostOrderAnalysis(SILModule *M);
  SILAnalysis *createClassHierarchyAnalysis(SILModule *M);
  SILAnalysis *createRCIdentityAnalysis(SILModule *M, SILPassManager *PM);
  SILAnalysis *createDestructorAnalysis(SILModule *M);

  /// A builder struct that can be used by passes to manage the building of a
  /// SILAnalysis::PreserveKind when multiple SILAnalysis::PreserveKind can be
  /// produced by the pass.
  class PreserveKindBuilder {
    using PreserveKind = SILAnalysis::PreserveKind;
    llvm::Optional<PreserveKind> Kind;

    /// Initialize Kind if it is None with PreserveKind::All and unset the bits
    /// corresponding to K.
    void invalidate(PreserveKind K) {
      unsigned NewValue = !Kind ? PreserveKind::All : Kind.getValue();
      Kind = PreserveKind(NewValue & ~K);
    }

  public:
    /// Returns None if no changes were made. Returns the kind that is preserved
    /// otherwise.
    llvm::Optional<PreserveKind> getKind() const { return Kind; }

    void invalidateProgramFlow() { invalidate(PreserveKind::ProgramFlow); }
    void invalidateCalls() { invalidate(PreserveKind::Calls); }
    void invalidateBranches() { invalidate(PreserveKind::Branches); }
    void invalidateInstructions() { invalidate(PreserveKind::Nothing); }
  };
} // end namespace swift

#endif

