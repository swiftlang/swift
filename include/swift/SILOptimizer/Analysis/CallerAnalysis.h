//===--- CallerAnalysis.h - Determine callees per call site -----*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_CALLERANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_CALLERANALYSIS_H

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

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
///
/// In addition of caller information this analysis also provides information
/// about partial applies of a function.
class CallerAnalysis : public SILAnalysis {

public:

  /// NOTE: this can be extended to contain the callsites of the function.
  class FunctionInfo {
    friend class CallerAnalysis;

    /// A list of all the functions this function calls or partially applies.
    llvm::SetVector<SILFunction *> Callees;
    /// A list of all the callers this function has.
    llvm::SmallSet<SILFunction *, 4> Callers;

    /// The number of partial applied arguments of this function.
    ///
    /// Specifically, it stores the minimum number of partial applied arguments
    /// of each function which contain one or multiple partial_applys of this
    /// function.
    /// This is a little bit off-topic because a partial_apply is not really
    /// a "call" of this function.
    llvm::DenseMap<SILFunction *, int> PartialAppliers;

  public:
    /// Returns true if this function has at least one caller.
    bool hasCaller() const {
      return !Callers.empty();
    }

    /// Returns non zero if this function is partially applied anywhere.
    ///
    /// The return value is the minimum number of partially applied arguments.
    /// Usually all partial applies of a function partially apply the same
    /// number of arguments anyway.
    int getMinPartialAppliedArgs() const {
      int minArgs = 0;
      for (auto Iter : PartialAppliers) {
        int numArgs = Iter.second;
        if (minArgs == 0 || numArgs < minArgs)
          minArgs = numArgs;
      }
      return minArgs;
    }
  };
  
private:
  /// Current module we are analyzing.
  SILModule &Mod;

  /// A map between all the functions and their callsites in the module.
  llvm::DenseMap<SILFunction *, FunctionInfo> FuncInfos;

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
      FuncInfos.FindAndConstruct(&F);
      RecomputeFunctionList.insert(&F);
    }
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Caller;
  }

  /// Invalidate all information in this analysis.
  virtual void invalidate() override {
    FuncInfos.clear();
    RecomputeFunctionList.clear();
    for (auto &F : Mod) {
      RecomputeFunctionList.insert(&F);
    }
  }

  /// Invalidate all of the information for a specific function.
  virtual void invalidate(SILFunction *F, InvalidationKind K) override {
    // Should we invalidate based on the invalidation kind.
    bool shouldInvalidate = K & InvalidationKind::CallsAndInstructions;
    if (!shouldInvalidate)
      return;

    // This function has become "unknown" to us. Invalidate any callsite
    // information related to this function.
    invalidateExistingCalleeRelation(F);
    // Make sure this function is recomputed next time.
    RecomputeFunctionList.insert(F);
  }

  /// Notify the analysis about a newly created function.
  virtual void notifyAddFunction(SILFunction *F) override {
    RecomputeFunctionList.insert(F);
  }

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyDeleteFunction(SILFunction *F) override {
    invalidateExistingCalleeRelation(F);
    RecomputeFunctionList.remove(F);
  }

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override { }

  const FunctionInfo &getCallerInfo(SILFunction *F) {
    // Recompute every function in the invalidated function list and empty the
    // list.
    processRecomputeFunctionList();
    return FuncInfos[F];
  }
};

} // end namespace swift

#endif
