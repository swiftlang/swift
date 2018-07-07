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
  class FunctionInfo;

private:
  struct CallerInfo;

  /// Current module we are analyzing.
  SILModule &mod;

  /// A map between all the functions and their callsites in the module.
  ///
  /// We use a map vector to ensure that when we dump the state of the caller
  /// analysis,
  llvm::DenseMap<SILFunction *, FunctionInfo> funcInfos;

  /// A list of functions that needs to be recomputed.
  llvm::SetVector<SILFunction *> recomputeFunctionList;

public:
  CallerAnalysis(SILModule *m);

  static bool classof(const SILAnalysis *s) {
    return s->getKind() == AnalysisKind::Caller;
  }

  /// Invalidate all information in this analysis.
  virtual void invalidate() override {
    funcInfos.clear();
    recomputeFunctionList.clear();
    for (auto &f : mod) {
      recomputeFunctionList.insert(&f);
    }
  }

  /// Invalidate all of the information for a specific caller function.
  virtual void invalidate(SILFunction *caller, InvalidationKind k) override {
    // Should we invalidate based on the invalidation kind.
    bool shouldInvalidate = k & InvalidationKind::CallsAndInstructions;
    if (!shouldInvalidate)
      return;

    // This function has become "unknown" to us. Invalidate any callsite
    // information related to this function.
    invalidateExistingCalleeRelation(caller);

    // Make sure this function is recomputed next time.
    recomputeFunctionList.insert(caller);
  }

  /// Notify the analysis about a newly created function.
  virtual void notifyAddFunction(SILFunction *f) override {
    recomputeFunctionList.insert(f);
  }

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyDeleteFunction(SILFunction *f) override {
    invalidateExistingCalleeRelation(f);
    recomputeFunctionList.remove(f);
  }

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override {}

  /// Look up the function info that we have stored for f, recomputing all
  /// invalidating parts of the call graph.
  const FunctionInfo &getCallerInfo(SILFunction *f) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "Only for use in the debugger");

  /// Print the state of the caller analysis as a sequence of yaml documents for
  /// each callee we are tracking.
  void print(llvm::raw_ostream &os) const;

  /// Print the state of the caller analysis as a sequence of yaml documents for
  /// each callee we are tracking to the passed in file path.
  LLVM_ATTRIBUTE_DEPRECATED(void print(const char *filePath)
                                const LLVM_ATTRIBUTE_USED,
                            "Only for use in the debugger");

private:
  /// Iterate over all the call sites in the function and update
  /// CallInfo.
  void processFunctionCallSites(SILFunction *f);

  /// This function is about to become "unknown" to us. Invalidate any
  /// callsite information related to it.
  void invalidateExistingCalleeRelation(SILFunction *f);

  void processRecomputeFunctionList() {
    for (auto &f : recomputeFunctionList) {
      processFunctionCallSites(f);
    }
    recomputeFunctionList.clear();
  }

  /// Internal only way for getting a caller info. Will insert f if needed and
  /// _WILL NOT_ preform any recomputation of the callgraph.
  FunctionInfo &getOrInsertCallerInfo(SILFunction *f);
};

/// Auxillary information that we store about a specific caller.
struct CallerAnalysis::CallerInfo {
  /// Given a SILFunction F that contains at least one partial apply of the
  /// given function, map F to the minimum number of partial applied
  /// arguments of any partial application in F.
  ///
  /// By storing the minimum number of partial applied arguments, we are able
  /// to decide quickly if we are able to eliminate dead captured arguments.
  Optional<unsigned> numPartialAppliedArguments;

  /// True if this caller performs at least one full application of the
  /// callee.
  bool hasFullApply : 1;

  /// True if this caller can guarantee that all direct caller's of this
  /// function inside of it can be found.
  ///
  /// NOTE: This does not imply that a function can not be called
  /// indirectly. That is a separate query that is type system specific.
  bool isDirectCallerSetComplete : 1;

  CallerInfo()
      : numPartialAppliedArguments(), hasFullApply(false),
        isDirectCallerSetComplete(false) {}
};

/// This is a representation of the caller information that we have associated
/// with a specific function.
///
/// NOTE: this can be extended to contain the callsites of the function. For
/// now there is no need for the exact call sites due to us using only the
/// caller information. By not implementing this we save memory and get rid of
/// dead code.
class CallerAnalysis::FunctionInfo {
  friend class CallerAnalysis;
  using CallerInfo = CallerAnalysis::CallerInfo;
  struct YAMLRepresentation;

public:
  /// FIXME: Upstream in LLVM this is a public using declaration on
  /// MapVector (MapVector::value_type). In the version of LLVM that
  /// Swift compiles against currently this is not true, so we provide
  /// this for ease of use now.
  ///
  /// This is meant to be an internal implementation detail.
  using CallerStatesValueType = std::pair<SILFunction *, CallerInfo>;

private:
  /// A map from a function containing uses of a function_ref of the callee to
  /// the state that we store about the caller's body.
  llvm::SmallMapVector<SILFunction *, CallerInfo, 1> callerStates;

  /// True if this function is something that could be called via a vtable or
  /// a witness table. This does not include escaping uses.
  ///
  /// For now this is very conservative and is only set to not be true if we
  /// have a function whose representation never can escape. In future cases,
  /// we should consider refining this to take into account the compilation
  /// visibility of a protocol conformance or class (and thus if we have
  /// enough visibility to).
  bool mayHaveIndirectCallers : 1;

  /// This is a special set vector that is an abuse as a performance
  /// optimization. We in this case are treating the function info data
  /// structure as a source of info about callers so that we can update a
  /// caller's callees when we invalidate a caller. (See
  /// invalidateExistingCalleeRelation).
  llvm::SmallSetVector<SILFunction *, 1> calleeStates;

public:
  FunctionInfo(SILFunction *f);

  bool hasAllCallers() const {
    return hasOnlyCompleteDirectCallerSets() && !mayHaveIndirectCallers;
  }

  /// Returns true if this function has at least one caller.
  bool hasCaller() const {
    return callerStates.size() &&
           llvm::any_of(callerStates, [](const CallerStatesValueType &v) {
             return v.second.hasFullApply;
           });
  }

  /// Returns non zero if this function is partially applied anywhere.
  ///
  /// The return value is the minimum number of partially applied arguments.
  /// Usually all partial applies of a function partially apply the same
  /// number of arguments anyway.
  unsigned getMinPartialAppliedArgs() const {
    if (callerStates.empty())
      return 0;

    bool foundArg = false;
    unsigned minArgs = UINT_MAX;
    for (const auto &iter : callerStates) {
      if (auto numArgs = iter.second.numPartialAppliedArguments) {
        foundArg = true;
        minArgs = std::min(minArgs, numArgs.getValue());
      }
    }

    return foundArg ? minArgs : 0;
  }

  bool hasOnlyCompleteDirectCallerSets() const {
    return llvm::all_of(callerStates, [](const CallerStatesValueType &v) {
      return v.second.isDirectCallerSetComplete;
    });
  }

  auto getAllReferencingCallers() const
      -> decltype(llvm::make_range(callerStates.begin(), callerStates.end())) {
    return llvm::make_range(callerStates.begin(), callerStates.end());
  }

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "Only for use in the debugger");

  void print(llvm::raw_ostream &os) const;
};

} // end namespace swift

#endif
