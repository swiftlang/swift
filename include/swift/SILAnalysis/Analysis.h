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

  /// The base class for all SIL-level analysis.
  class SILAnalysis {
  public:
    /// The invalidation Lattice.
    /// This is a hierarchy of invalidation messages that are sent to analysis
    /// objects. Every invalidation kind invalidates the levels below it.
    enum class InvalidationKind {
      Instructions,  // Invalidate instruction-related analysis.
      CFG,           // The control flow changes.
      CallGraph,     // The call graph changed.
      All,           // Invalidate everything.
    };

    /// The class hierarchy
    enum class AnalysisKind {
      CompleteFuncs,
      CallGraph,
      Dominance,
      Alias,
      SpecializedArgs,
    };

    /// Stores the kind of derived class.
    const AnalysisKind Kind;

    /// Returns the kind of derived class.
    AnalysisKind getKind() const { return Kind; }

    /// C'tor.
    SILAnalysis(AnalysisKind K) : Kind(K) {}

    /// D'tor.
    virtual ~SILAnalysis() {}

    /// Invalidate all information in this analysis.
    virtual void invalidate(InvalidationKind K) {}

    /// Invalidate all of the information for a specific function.
    virtual void invalidate(SILFunction *F, InvalidationKind K) {}
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
  };

  /// The Call Graph Analysis provides information about the call graph.
  class CallGraphAnalysis : public SILAnalysis {
    SILModule *M;
    std::vector<SILFunction *> BottomUpFunctionOrder;

  public:
    virtual ~CallGraphAnalysis() {}
    CallGraphAnalysis(SILModule *MM) : SILAnalysis(AnalysisKind::CallGraph),
                                       M(MM) {}

    static bool classof(const SILAnalysis *S) {
      return S->getKind() == AnalysisKind::CallGraph;
    }

    /// \brief return a bottom-up function order.
    const std::vector<SILFunction*> &bottomUpCallGraphOrder();

    virtual void invalidate(InvalidationKind K) {
      if (K >= InvalidationKind::CallGraph)
        BottomUpFunctionOrder.clear();
    }

    virtual void invalidate(SILFunction*, InvalidationKind K) { invalidate(K); }
  };


  SILAnalysis *createCallGraphAnalysis(SILModule *M);
  SILAnalysis *createAliasAnalysis(SILModule *M);
  SILAnalysis *createDominanceAnalysis(SILModule *M);
  SILAnalysis *createSpecializedArgsAnalysis(SILModule *M);

} // end namespace swift

#endif

