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
/// Helpers for closures and their scopes.
///   - isNonEscapingClosure recognizes a non-escaping closure by its type
///   - getCaptureEdge finds the function that an apply or partial_apply passes
///     a capture to
///   - ClosureFunctionOrder orders a list of functions so that every
///     non-escaping closure comes after its scopes.
///
/// A capture edge is an apply site that passes its callee a capture, a box or
/// an @inout_aliasable address. Passes follow a capture into another function
/// only along a capture edge.
///
/// ClosureFunctionOrder's closure graph holds the capture edges that pass an
/// address to a callee that can't be replaced at runtime. It is specific to
/// non-escaping closures. Unlike CallerAnalysis, it does not reflect the call
/// tree. A closure's scope is a function that directly references the closure.
///
/// We do not currently mark SILFunctions as non-escaping. However, only
/// closures that are not assigned to an lvalue and are never passed as escaping
/// closures can use the @inout_aliasable convention. For now, we simply limit
/// the graph to such closures. This covers the set of closures that
/// SILOptimizer must view as non-escaping, which must in turn be a subset of
/// Sema's non-escaping closures.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_CLOSURESCOPE_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_CLOSURESCOPE_H

#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include <optional>

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

class ApplySite;

/// A capture edge, from an apply site to the function it passes a capture to.
struct CaptureEdge {
  SILFunction *callee;
  /// The callee value's type has an @inout_aliasable parameter, so the callee
  /// is a non-escaping closure. Otherwise the site passes a box.
  bool byAddress;
  /// The callee is reached through a dynamic_function_ref, so it can be
  /// replaced at runtime.
  bool dynamicallyReplaceable;
};

/// Returns \p apply's capture edge, if it has one. An apply site without a
/// statically known callee has none.
std::optional<CaptureEdge> getCaptureEdge(ApplySite apply);

// Orders functions top-down in closure scope order.
class ClosureFunctionOrder {
  class ClosureDFS;
  friend class ClosureDFS;

  // The functions passed to the constructor in top-down order (RPO) following
  // the closure scope graph. Functions that define a closure occur before the
  // closure.
  SmallVector<SILFunction *, 8> topDownFunctions;

  // If the closure scope graph has any cycles, record each function at the
  // head of a cycle. This does not include all the functions in the
  // strongly-connected component. This is extremely rare. It is always a local
  // function that refers to itself either directly or indirectly.
  SmallPtrSet<SILFunction *, 4> closureCycleHeads;

public:
  /// Orders \p functions, which must be in module function-list order and
  /// closed under scope edges to definitions.
  explicit ClosureFunctionOrder(ArrayRef<SILFunction *> functions);

  ArrayRef<SILFunction *> getTopDownFunctions() const {
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
