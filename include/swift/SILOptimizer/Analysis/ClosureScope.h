//===--- ClosureScope.h - Determines closure's defining scope ---*- C++ -*-===//
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
///
/// Map each non-escaping closure SILFunction to a set of SILFunctions
/// corresponding to its parent scopes.
///
/// This is a lightweight analysis specific to non-escaping closures. Unlike
/// CallerAnalysis, this does not reflect the call tree. A closure's scope is a
/// function that directly references the closure. It may directly invoke the
/// closure (as a caller) or may simply pass it off as an argument.
///
/// Like CallerAnalysis, ClosureScope is top-down, but unlike CallerAnalysis, it
/// does not require complex invalidation and recomputation. The underlying
/// assumption is that no transformation will add new references to existing
/// non-escaping closures, with some exceptions like SILCloner.
///
/// TODO: When this analysis is used across passes, fix SILCloner to update or
/// invalidate. In SILVerifier, if this analysis is marked valid, check that no
/// new unseen closure references have been added.
///
/// We do not currently mark SILFunctions as non-escaping. However, only
/// closures that are not assigned to an lvalue and are never passed as escaping
/// closures can use the @inout_aliasable convention. For now, we simply limit
/// this analysis to such closures. This covers the set of closures that
/// SILOptimizer must view as non-escaping, which must in turn be a subset of
/// Sema's non-escaping closures.
///
/// NOTE: a non-escaping function can be passed as an escaping function via
/// withoutActuallyEscaping. However, using that API to introduce recursion is
/// disallowed according to exclusivity semantics. That is, non-escaping
/// function types cannot be reentrant (SE-0176). In this analysis, we assert
/// that closure scopes are acyclic. Although the language does not currently
/// enforce non-reentrant, non-escaping closures, the scope graph cannot be
/// cyclic because there's no way to name a non-escaping closure. So, in the
/// long term the acyclic assumption made by this analysis is protected by
/// non-reentrant semantics, and in the short-term it's safe because of the
/// language's practical limitations.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_CLOSURESCOPE_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_CLOSURESCOPE_H

#include "swift/Basic/Assertions.h"
#include "swift/Basic/BlotSetVector.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/iterator.h"

namespace swift {

/// Return true if this function is known to be non-escaping.
///
/// This must only return true if the closure was also deemed non-escaping in
/// Sema. Sema will eventually guarantee non-reentrance.
///
/// This must always return true for any closure with @inout_aliasable parameter
/// conventions, because passes like AccessEnforcementSelection assume that
/// convention only applies to non-escaping closures.
///
/// (Hence, SILGen may only use @inout_aliasable for closures that Sema deems
/// non-escaping.)
///
/// This may conservatively return false for any non-escaping closures that
/// don't happen to use @inout_aliasable.
inline bool isNonEscapingClosure(CanSILFunctionType funcTy) {
  auto isInoutAliasable = [](SILParameterInfo paramInfo) {
    return paramInfo.getConvention()
    == ParameterConvention::Indirect_InoutAliasable;
  };
  return llvm::any_of(funcTy->getParameters(), isInoutAliasable);
}

class ClosureGraph;

class ClosureScopeAnalysis : public SILAnalysis {
  friend class ClosureGraph;

  SILModule *M;

  // The analysis data. nullptr if it has never been computed.
  std::unique_ptr<ClosureGraph> scopeGraph;

public:
  ClosureScopeAnalysis(SILModule *M);
  ~ClosureScopeAnalysis();

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::ClosureScope;
  }

  SILModule *getModule() const { return M; }

  /// Visit the parent scopes of \p closure if it has any. If \p visitor returns
  /// false, exit early and return false. Otherwise return true.
  bool visitClosureScopes(SILFunction *closure,
                          std::function<bool(SILFunction *scopeFunc)> visitor);

  /// Visit the closures directly referenced by \p scopeFunc.
  bool visitClosures(SILFunction *scopeFunc,
                     std::function<bool(SILFunction *closure)> visitor);

  /// Return true if this function is a reachable closure.
  bool isReachableClosure(SILFunction *function) {
    // This visitor returns false immediately on the first scope.
    return !visitClosureScopes(function, [](SILFunction *) { return false; });
  }

  /// Invalidate all information in this analysis.
  virtual void invalidate() override;

  /// Invalidate all of the information for a specific function.
  virtual void invalidate(SILFunction *F, InvalidationKind K) override {
    // No invalidation needed because the analysis does not cache anything
    // per call-site in functions, and we assume that references to closures
    // cannot be added to functions, aside from cloning.
  }

  /// Notify the analysis about a newly created function.
  virtual void notifyAddedOrModifiedFunction(SILFunction *F) override {
    // Nothing to be done because the analysis does not cache anything
    // per call-site in functions.
  }

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyWillDeleteFunction(SILFunction *F) override;

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override {
    // witness tables have no effect on closure scopes.
  }

protected:
  ClosureScopeAnalysis(const ClosureScopeAnalysis &) = delete;
  ClosureScopeAnalysis &operator=(const ClosureScopeAnalysis &) = delete;

  ClosureGraph *getOrComputeGraph();
};

// ClosureScopeAnalysis utility for visiting functions top-down or bottom-up in
// closure scope order.
class ClosureFunctionOrder {
  class ClosureDFS;
  friend class ClosureDFS;

  ClosureScopeAnalysis *csa;

  // All functions in this module in top-down order (RPO) following the closure
  // scope graph. Functions that define a closure occur before the closure.
  std::vector<SILFunction *> topDownFunctions;

  // If the closure scope graph has any cycles, record each function at the
  // head of a cycle. This does not include all the functions in the
  // strongly-connected component. This is extremely rare. It is always a local
  // function that refers to itself either directly or indirectly.
  SmallPtrSet<SILFunction *, 4> closureCycleHeads;

public:
  ClosureFunctionOrder(ClosureScopeAnalysis *csa) : csa(csa) {}

  void compute();

  ArrayRef<SILFunction *> getTopDownFunctions() const {
    assert(!topDownFunctions.empty()
           || csa->getModule()->getFunctions().empty());
    return topDownFunctions;
  }

  bool isHeadOfClosureCycle(SILFunction *function) const {
    return closureCycleHeads.contains(function);
  }

  SWIFT_ASSERT_ONLY_DECL(void dump());

protected:
  ClosureFunctionOrder(const ClosureFunctionOrder &) = delete;
  ClosureFunctionOrder &operator=(const ClosureFunctionOrder &) = delete;
};

} // end namespace swift

#endif
