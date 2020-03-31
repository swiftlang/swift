//===--- EvaluatorDependencies.h - Auto-Incremental Dependencies -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines data structures to support the request evaluator's
// automatic incremental dependency tracking functionality.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_EVALUATOR_DEPENDENCIES_H
#define SWIFT_AST_EVALUATOR_DEPENDENCIES_H

#include "swift/AST/AttrKind.h"
#include "swift/AST/SourceFile.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {

namespace evaluator {

/// The "scope" of a dependency edge tracked by the evaluator.
///
/// Dependency scopes come in two flavors: cascading and private. A private
/// edge captures dependencies discovered in contexts that are not visible to
/// to other files. For example, a conformance to a private protocol, or the use
/// of any names inside of a function body. A cascading edge, by contrast,
/// captures dependencies discovered in the remaining visible contexts. These
/// are types with at least \c internal visibility, names defined or used
/// outside of function bodies with at least \c internal visibility, etc. A
/// dependency that has cascading scope is so-named because upon traversing the
/// edge, a reader such as the driver should continue transitively evaluating
/// further dependency edges.
///
/// A cascading edge is always conservatively correct to produce, but it comes
/// at the cost of increased resources spent (and possibly even wasted!) during
/// incremental compilation. A private edge, by contrast, is more efficient for
/// incremental compilation but it is harder to safely use.
///
/// To ensure that these edges are registered consistently with the correct
/// scopes, requests that act as the source of dependency edges are required
/// to specify a \c DependencyScope under which all evaluated sub-requests will
/// register their dependency edges. In this way, \c DependencyScope values
/// form a stack-like structure and are pushed and popped by the evaluator
/// during the course of request evaluation.
///
/// When determining the kind of scope a request should use, always err on the
/// side of a cascading scope unless there is absolute proof any discovered
/// dependencies will be private. Inner requests may also defensively choose to
/// flip the dependency scope from private to cascading in the name of safety.
enum class DependencyScope : bool {
  Private = false,
  Cascading = true,
};

/// Returns a \c DependencyScope appropriate for the given (formal) access level.
///
/// :warning: This function exists to bridge the old manual reference
/// dependencies code to the new evaluator-based reference dependencies code.
/// The manual code often made private/cascading scope judgements based on the
/// access level of a declaration. While this makes some sense intuitively, it
/// does not necessarily capture an accurate picture of where real incremental
/// dependencies lie. For example, references to formally private types can
/// "escape" to contexts that have no reference to the private name if, say,
/// the layout of that private type is taken into consideration by
/// SILGen or IRGen in a separate file that references the declaration
/// transitively. However, due to the density of the current dependency
/// graph, redundancy in registered dependency edges, and the liberal use of
/// cascading edges, we may be saved from the worst consequences of this
/// modelling choice.
///
/// The use of access-levels for dependency decisions is an anti-pattern that
/// should be revisited once finer-grained dependencies are explored more
/// thoroughly.
inline DependencyScope getScopeForAccessLevel(AccessLevel l) {
  switch (l) {
  case AccessLevel::Private:
  case AccessLevel::FilePrivate:
    return DependencyScope::Private;
  case AccessLevel::Internal:
  case AccessLevel::Public:
  case AccessLevel::Open:
    return DependencyScope::Cascading;
  }
}

// A \c DependencySource is currently defined to be a parent source file and
// an associated dependency scope.
//
// The \c SourceFile instance is an artifact of the current dependency system,
// and should be scrapped if possible. It currently encodes the idea that
// edges in the incremental dependency graph invalidate entire files instead
// of individual contexts.
using DependencySource = llvm::PointerIntPair<SourceFile *, 1, DependencyScope>;
} // end namespace evaluator

} // end namespace swift

#endif // SWIFT_AST_EVALUATOR_DEPENDENCIES_H
