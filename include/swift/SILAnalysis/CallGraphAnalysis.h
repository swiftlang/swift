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
#include "swift/Basic/Range.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/DenseMap.h"
#include <vector>

namespace swift {

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

} // end namespace swift

#endif

