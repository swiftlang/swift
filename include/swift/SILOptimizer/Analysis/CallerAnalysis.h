//===--- CallerAnalysis.h - Determine callees per call site -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_CALLERANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_CALLERANALYSIS_H

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

/// NOTE: this can be extended to contain the callsites of the function.
struct CallerAnalysisFunctionInfo {
  /// A list of all the functions this function calls.
  llvm::SetVector<SILFunction *> Callees;
  /// A list of all the callers this function has.
  llvm::SetVector<SILFunction *> Callers;
};

/// CallerAnalysis relies on keeping the Caller/Callee relation up-to-date
/// lazily. i.e. when a function is invalidated, instead of recomputing the
/// function it calls right away, its kept in a recompute list and
/// CallerAnalysis recomputes and empty the recompute list before any query.
///
/// We also considered the possibility of keeping a computed list, instead of
/// recompute Every time we need to complete the computed list (i.e. we want
/// to the computed list to contain every function in the module). We need to
/// walk through every function in the module. This leads to O(n) And we need
/// to run every function through the a sequence of function passes which might
/// invalidate the functions and make the computed list incomplete. So
/// O(n) * O(n) = O(n^2).
class CallerAnalysis : public SILAnalysis {

  /// Current module we are analyzing.
  SILModule &Mod;

  /// A map between all the functions and their callsites in the module.
  llvm::DenseMap<SILFunction *, CallerAnalysisFunctionInfo> CallInfo;

  /// A list of functions that needs to be recomputed.
  llvm::SetVector<SILFunction *> RecomputeFunctionList;

  /// Iterate over all the call sites in the function and update
  /// CallInfo.
  void processFunctionCallSites(SILFunction *F); 

  /// This function is about to become "unknown" to us. Invalidate any 
  /// callsite information related to it.
  void invalidateExistingCalleeRelation(SILFunction *F); 

  void processRecomputeFunctionList() {
    for (auto &F : RecomputeFunctionList) {
      processFunctionCallSites(F);
    }
    RecomputeFunctionList.clear(); 
  }

public:
  CallerAnalysis(SILModule *M) : SILAnalysis(AnalysisKind::Caller), Mod(*M) {
    // Make sure we compute everything first time called.
    for (auto &F : Mod) {
      CallInfo.FindAndConstruct(&F);
      RecomputeFunctionList.insert(&F);
    }
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Caller;
  }

  virtual void notifyAnalysisOfFunction(SILFunction *F) {
    RecomputeFunctionList.insert(F);
  }

  virtual void invalidate(SILFunction *F, InvalidationKind K) {
    // Should we invalidate based on the invalidation kind.
    bool shouldInvalidate = K & InvalidationKind::Calls;
    if (!shouldInvalidate)
      return;

    // This function has become "unknown" to us. Invalidate any callsite
    // information related to this function.
    invalidateExistingCalleeRelation(F);
    // Make sure this function is recomputed next time.
    RecomputeFunctionList.insert(F);
  }

  virtual void invalidateForDeadFunction(SILFunction *F, InvalidationKind K) {
    invalidateExistingCalleeRelation(F);
    RecomputeFunctionList.remove(F);
  }

  virtual void invalidate(InvalidationKind K) {
    // Should we invalidate based on the invalidation kind.
    bool shouldInvalidate = K & InvalidationKind::Calls;
    if (!shouldInvalidate)
      return;

    CallInfo.clear();
    RecomputeFunctionList.clear();
    for (auto &F : Mod) {
      RecomputeFunctionList.insert(&F);
    }
  }

  /// Return true if the function has a caller inside current module.
  bool hasCaller(SILFunction *F) {
    // Recompute every function in the invalidated function list and empty the
    // list.
    processRecomputeFunctionList();
    auto Iter = CallInfo.FindAndConstruct(F);
    return !Iter.second.Callers.empty();
  }
};

} // end namespace swift

#endif
