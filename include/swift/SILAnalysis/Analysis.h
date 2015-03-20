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
    /// The invalidation Lattice.
    /// This is a hierarchy of invalidation messages that are sent to analysis
    /// objects. Every invalidation kind invalidates the levels before it. For
    /// example, CFG invalidates Instructions and CFG.
    enum class InvalidationKind {
      Instructions,  // Invalidate instruction-related analysis.
      CFG,           // The control flow changes.
      CallGraph,     // The call graph changed.
      All,           // Invalidate everything.
    };

    /// A list of the known analysis.
    enum class AnalysisKind {
      CompleteFuncs,
      CallGraph,
      Dominance,
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
    virtual void invalidate(InvalidationKind K) {}

    /// Invalidate all of the information for a specific function.
    virtual void invalidate(SILFunction *F, InvalidationKind K) {}

    /// Verify the state of this analysis.
    virtual void verify() const {}
  };

  /// Keep track of functions that are completely optimized.
  class CompleteFunctions : public SILAnalysis {
    SILModule *M = nullptr;
    llvm::DenseSet<SILFunction*> CompleteFuncs;
    llvm::DenseSet<SILFunction*> PendingFuncs;
    bool IsModulePending = false;
    bool HasChanged = false;

  public:
    CompleteFunctions(SILModule *MM)
      : SILAnalysis(AnalysisKind::CompleteFuncs), M(MM) {}

    virtual ~CompleteFunctions();

    static bool classof(const SILAnalysis *S) {
      return S->getKind() == AnalysisKind::CompleteFuncs;
    }

    virtual void invalidate(InvalidationKind K) {
      IsModulePending = true;
      HasChanged = true;
    }

    virtual void invalidate(SILFunction* F, InvalidationKind) {
      PendingFuncs.insert(F);
      HasChanged = true;
    }

    /// Report whether anything was invalidated since the last reset.
    bool hasChanged() const { return HasChanged; }
    void resetChanged() { HasChanged = false; }

    bool isComplete(SILFunction *F) const {
      return CompleteFuncs.count(F);
    }
    /// Mark functions complete at the end of the optimization pipeline if they
    /// haven't changed.
    void setComplete();

    /// Reset the state of this analysis.
    void reset() {
      CompleteFuncs.clear();
      PendingFuncs.clear();
      IsModulePending = false;
      HasChanged = false;
    }
  };

  SILAnalysis *createCallGraphAnalysis(SILModule *M);
  SILAnalysis *createAliasAnalysis(SILModule *M);
  SILAnalysis *createDominanceAnalysis(SILModule *M);
  SILAnalysis *createLoopInfoAnalysis(SILModule *M, SILPassManager *PM);
  SILAnalysis *createInductionVariableAnalysis(SILModule *M);
  SILAnalysis *createPostOrderAnalysis(SILModule *M);
  SILAnalysis *createClassHierarchyAnalysis(SILModule *M);
  SILAnalysis *createRCIdentityAnalysis(SILModule *M, SILPassManager *PM);
  SILAnalysis *createDestructorAnalysis(SILModule *M);
} // end namespace swift

#endif

