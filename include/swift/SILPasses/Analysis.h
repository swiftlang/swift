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
#include "llvm/ADT/SmallVector.h"
#include <vector>

#ifndef SWIFT_SILPASSES_ANALYSIS_H
#define SWIFT_SILPASSES_ANALYSIS_H

namespace swift {
  class SILModule;
  class SILFunction;

  /// The base class for all SIL-level analysis.
  class SILAnalysis {
  public:
    /// The invalidation Lattice.
    /// This is a heiarchy of invalidation messages that are sent to analysis
    /// objects. Every invalidation kind invalidates the levels below it.
    enum InvalidationKind {
      IK_Instructions,  // Invalidate instruction-related analysis.
      IK_CFG,           // The control flow changes.
      IK_CallGraph,     // The call graph changed.
      IK_All,           // Invalidate everything.
    };

    /// The class heiarchy
    enum AnalysisKind {
      AK_CallGraph,
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

  /// The Call Graph Analysis provides information about the call graph.
  class CallGraphAnalysis : public SILAnalysis {
    SILModule *M;

  public:
    virtual ~CallGraphAnalysis() {}
    CallGraphAnalysis(SILModule *MM) : SILAnalysis(AK_CallGraph), M(MM) {}

    static bool classof(const SILAnalysis *S) {
      return S->getKind() == AK_CallGraph;
    }

    void bottomUpCallGraphOrder(std::vector<SILFunction*> &order);
    void topDownCallGraphOrder(std::vector<SILFunction*> &order);

    virtual void invalidate(InvalidationKind K) {
    // TODO: invalidate the cache once we implement one.
      if (K == IK_CallGraph) { /* ... */ }
    }

    virtual void invalidate(SILFunction*, InvalidationKind K) { invalidate(K); }
  };


  SILAnalysis *createCallGraphAnalysis(SILModule *M);
} // end namespace swift

#endif

