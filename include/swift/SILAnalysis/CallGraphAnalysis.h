//===--- CallGraphAnalysis.h - Analysis of the Call Graph ------*- C++ -*--===//
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

#ifndef SWIFT_SILANALYSIS_CALLGRAPHANALYSIS_H
#define SWIFT_SILANALYSIS_CALLGRAPHANALYSIS_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/CallGraph.h"

namespace swift {

class SILFunction;
class SILModule;

/// The Call Graph Analysis provides information about the call graph.
class CallGraphAnalysis : public SILAnalysis {
  SILModule *M;
  CallGraph *CG;

public:
  virtual ~CallGraphAnalysis() {
    delete CG;
  }
  CallGraphAnalysis(SILModule *MM) : SILAnalysis(AnalysisKind::CallGraph),
                                     M(MM), CG(nullptr) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::CallGraph;
  }

  bool haveCallGraph() { return CG; }
  CallGraph *getCallGraphOrNull() { return CG; }
  CallGraph &getCallGraph() {
    assert(haveCallGraph() && "Expected constructed call graph!");
    return *CG;
  }

  CallGraph &getOrBuildCallGraph() {
    if (!CG)
      CG = new CallGraph(M, false);
    return *CG;
  }

  virtual void invalidate(SILAnalysis::InvalidationKind K) {
    bool invalidateCalls = K & InvalidationKind::Calls;
    bool invalidateFuncs = K & InvalidationKind::Functions;
    if (invalidateCalls || invalidateFuncs) {
      delete CG;
      CG = nullptr;
    }
  }

  virtual void invalidate(SILFunction *, SILAnalysis::InvalidationKind K) {
    invalidate(K);
  }

  virtual void verify() const {
#ifndef NDEBUG
    // If we don't have a callgraph, return.
    if (!CG)
      return;
    CG->verify();
#endif
  }
  
  virtual void verify(SILFunction *F) const {
#ifndef NDEBUG
    // If we don't have a callgraph, return.
    if (!CG)
      return;
    CG->verify(F);
#endif
  }
};

} // end namespace swift

#endif
