//===--- PassManagerVerifierAnalysis.h ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_PASSMANAGERVERIFIERANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_PASSMANAGERVERIFIERANALYSIS_H

#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseSet.h"

namespace swift {

/// An analysis that validates that the pass manager properly sends add/delete
/// messages as functions are added/deleted from the module.
///
/// All methods are no-ops when asserts are disabled.
class PassManagerVerifierAnalysis : public SILAnalysis {
  /// The module that we are processing.
  SILModule &mod;

  /// The set of "live" functions that we are tracking.
  ///
  /// All functions in mod must be in liveFunctions and vis-a-versa.
  llvm::DenseSet<SILFunction *> liveFunctions;

public:
  PassManagerVerifierAnalysis(SILModule *mod);

  static bool classof(const SILAnalysis *analysis) {
    return analysis->getKind() == SILAnalysisKind::PassManagerVerifier;
  }

  /// Validate that the analysis is able to look up all functions and that those
  /// functions are live.
  void invalidate() override final;

  /// Validate that the analysis is able to look up the given function.
  void invalidate(SILFunction *f, InvalidationKind k) override final;

  /// If a function has not yet been seen start tracking it.
  void notifyAddedOrModifiedFunction(SILFunction *f) override final;

  /// Stop tracking a function.
  void notifyWillDeleteFunction(SILFunction *f) override final;

  /// Make sure that when we invalidate a function table, make sure we can find
  /// all functions for all witness tables.
  void invalidateFunctionTables() override final;

  /// Run the entire verification.
  void verify() const override final;
};

} // namespace swift

#endif
