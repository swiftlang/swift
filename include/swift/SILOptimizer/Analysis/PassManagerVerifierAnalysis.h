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
#include "llvm/ADT/StringSet.h"

namespace swift {

/// An analysis that validates that the pass manager properly sends add/delete
/// messages as functions are added/deleted from the module.
///
/// This is accomplished by the analysis initializing its own set of "live
/// functions" from the SILModules state upon pass manager construction. Then as
/// the passmanager sends add/delete messages to analyses, this analysis updates
/// the live function set by adding/removing functions as appropriate. Finally,
/// when the verify method is called, the analysis checks that its internal live
/// function set matches the current list of functions in the SILModule's
/// function list.
///
/// NOTE: All methods are no-ops when asserts are disabled.
class PassManagerVerifierAnalysis : public SILAnalysis {
  /// The module that we are processing.
  SILModule &mod;

  /// The set of "live" functions that we are tracking.
  ///
  /// All functions in mod must be in liveFunctions and vis-a-versa. We use the
  /// name of the function as its id to ensure that if a function is deleted and
  /// we are not notified, we have access to the function's name independent of
  /// the function's liveness itself. Otherwise, we could touch deallocated
  /// memory =><=.
  llvm::StringSet<> liveFunctions;

public:
  PassManagerVerifierAnalysis(SILModule *mod);

  static bool classof(const SILAnalysis *analysis) {
    return analysis->getKind() == SILAnalysisKind::PassManagerVerifier;
  }

  void invalidate() override final {}
  void invalidate(SILFunction *f, InvalidationKind k) override final {}
  void invalidateFunctionTables() override final {}

  /// If a function has not yet been seen start tracking it.
  void notifyAddedOrModifiedFunction(SILFunction *f) override final;

  /// Stop tracking a function.
  void notifyWillDeleteFunction(SILFunction *f) override final;

  /// Verify that our live function set matches the set of live functions in the
  /// SILModule.
  void verify() const override final;
};

} // namespace swift

#endif
