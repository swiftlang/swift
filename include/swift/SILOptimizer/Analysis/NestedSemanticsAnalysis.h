//===--- NestedSemanticsAnalysis.h ----------------------------------------===//
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

// Determine if a semantic function has calls to other semantic functions

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_NestedSemanticsAnalysis_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_NestedSemanticsAnalysis_H

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class NestedSemanticsAnalysis final : public SILAnalysis {
private:
  /// Map to store the result of the analysis
  llvm::DenseMap<SILFunction *, bool> callsSemanticFunctions;

public:
  NestedSemanticsAnalysis()
    : SILAnalysis(SILAnalysisKind::NestedSemantics) {}

  static bool classof(const SILAnalysis *s) {
    return s->getKind() == SILAnalysisKind::NestedSemantics;
  }

  /// Invalidate all of the information for a specific function.
  /// Nothing can invalidate NestedSemanticsAnalysis
  void invalidate(SILFunction *f, InvalidationKind k) override {
  }
 
  /// Invalidate all information in this analysis.
  /// Nothing can invalidate NestedSemanticsAnalysis
  virtual void invalidate() override {
  }

  /// Notify the analysis about a newly created function.
  virtual void notifyAddedOrModifiedFunction(SILFunction *F) override {}

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyWillDeleteFunction(SILFunction *F) override {}

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override { }

  /// Returns if a function is a nested semantic function
  bool isNestedSemanticFunction(SILFunction *f);

private:
  bool hasCallsToSemanticFunctions(SILFunction *f,
                                   SmallPtrSetImpl<SILFunction *> &visited);
};

} // end namespace swift

#endif
