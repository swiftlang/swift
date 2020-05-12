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

namespace detail {
// Remove this when the compiler bumps to C++17.
template <typename...> using void_t = void;
} // namespace detail

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
  llvm_unreachable("invalid access level kind");
}

// A \c DependencySource is currently defined to be a parent source file and
// an associated dependency scope.
//
// The \c SourceFile instance is an artifact of the current dependency system,
// and should be scrapped if possible. It currently encodes the idea that
// edges in the incremental dependency graph invalidate entire files instead
// of individual contexts.
using DependencySource = llvm::PointerIntPair<SourceFile *, 1, DependencyScope>;

/// A \c DependencyCollector is an aggregator of named references discovered in a
/// particular \c DependencyScope during the course of request evaluation.
struct DependencyCollector {
private:
  /// A stack of dependency sources in the order they were evaluated.
  llvm::SmallVector<evaluator::DependencySource, 8> dependencySources;

public:
  enum class Mode {
    // Enables the current "status quo" behavior of the dependency collector.
    //
    // By default, the dependency collector moves to register dependencies in
    // the referenced name trackers at the top of the active dependency stack.
    StatusQuo,
    // Enables an experimental mode to only register private dependencies.
    //
    // This mode restricts the dependency collector to ignore changes of
    // scope. This has practical effect of charging all unqualified lookups to
    // the primary file being acted upon instead of to the destination file.
    ExperimentalPrivateDependencies,
  };
  Mode mode;

  explicit DependencyCollector(Mode mode) : mode{mode} {};

public:
  /// Registers a named reference from the current dependency scope to a member
  /// defined in the given \p subject type.
  ///
  /// Used member constraints are typically the by-product of direct lookups,
  /// where the name being looked up and the target of the lookup are known
  /// up front. A used member dependency causes the file to be rebuilt if the
  /// definition of that member changes in any way - via
  /// deletion, addition, or mutation of a member with that same name.
  void addUsedMember(NominalTypeDecl *subject, DeclBaseName name);
  /// Registers a reference from the current dependency scope to a
  /// "potential member" of the given \p subject type.
  ///
  /// A single potential member dependency can be thought of as many used member
  /// dependencies - one for each current member of the subject type, but also
  /// one for every member that will be added or removed from the type in the
  /// future. As such, these dependencies cause rebuilds when any members are
  /// added, removed, or changed in the \p subject type. It also indicates a
  /// dependency on the \p subject type's existence, so deleting the \p subject
  /// type will also cause a rebuild.
  ///
  /// These dependencies are most appropriate for protocol conformances,
  /// superclass constraints, and other requirements involving entire types.
  void addPotentialMember(NominalTypeDecl *subject);
  /// Registers a reference from the current dependency scope to a given
  /// top-level \p name.
  ///
  /// A top level dependency causes a rebuild when another top-level entity with
  /// that name is added, removed, or modified.
  void addTopLevelName(DeclBaseName name);
  /// Registers a reference from the current dependency scope to a given
  /// dynamic member \p name.
  ///
  /// A dynamic lookup dependency is a special kind of member dependency on
  /// a name that is found by \c AnyObject lookup.
  void addDynamicLookupName(DeclBaseName name);

public:
  /// Returns the scope of the current active scope.
  ///
  /// If there is no active scope, the result always cascades.
  evaluator::DependencyScope getActiveSourceScope() const {
    if (dependencySources.empty()) {
      return evaluator::DependencyScope::Cascading;
    }
    return dependencySources.back().getInt();
  }

  /// Returns the active dependency's source file, or \c nullptr if no
  /// dependency source is active.
  ///
  /// The use of this accessor is strongly discouraged, as it implies that a
  /// dependency sink is seeking to filter out names based on the files they
  /// come from. Existing callers are being migrated to more reasonable ways
  /// of judging the relevancy of a dependency.
  SourceFile *getActiveDependencySourceOrNull() const {
    if (dependencySources.empty())
      return nullptr;
    return dependencySources.back().getPointer();
  }

public:
  /// An RAII type that manages manipulating the evaluator's
  /// dependency source stack. It is specialized to be zero-cost for
  /// requests that are not dependency sources.
  template <typename Request, typename = detail::void_t<>> struct StackRAII {
    StackRAII(DependencyCollector &DC, const Request &Req) {}
  };

  template <typename Request>
  struct StackRAII<Request,
                   typename std::enable_if<Request::isDependencySource>::type> {
    NullablePtr<DependencyCollector> Coll;
    StackRAII(DependencyCollector &coll, const Request &Req) {
      auto Source = Req.readDependencySource(coll);
      // If there is no source to introduce, bail. This can occur if
      // a request originates in the context of a module.
      if (!Source.getPointer()) {
        return;
      }
      coll.dependencySources.emplace_back(Source);
      Coll = &coll;
    }

    ~StackRAII() {
      if (Coll.isNonNull())
        Coll.get()->dependencySources.pop_back();
    }
  };

private:
  /// Returns the first dependency source registered with the tracker, or
  /// \c nullptr if no dependency sources have been registered.
  SourceFile *getFirstDependencySourceOrNull() const {
    if (dependencySources.empty())
      return nullptr;
    return dependencySources.front().getPointer();
  }

  /// If there is an active dependency source, returns its
  /// \c ReferencedNameTracker. Else, returns \c nullptr.
  ReferencedNameTracker *getActiveDependencyTracker() const {
    SourceFile *source = nullptr;
    switch (mode) {
    case Mode::StatusQuo:
      source = getActiveDependencySourceOrNull();
      break;
    case Mode::ExperimentalPrivateDependencies:
      source = getFirstDependencySourceOrNull();
      break;
    }
    
    if (!source)
      return nullptr;

    return source->getRequestBasedReferencedNameTracker();
  }

  /// Returns \c true if the scope of the current active source cascades.
  ///
  /// If there is no active scope, the result always cascades.
  bool isActiveSourceCascading() const {
    switch (mode) {
    case Mode::StatusQuo:
      return getActiveSourceScope() == evaluator::DependencyScope::Cascading;
    case Mode::ExperimentalPrivateDependencies:
      return false;
    }
    llvm_unreachable("invalid mode");
  }
};
} // end namespace evaluator

} // end namespace swift

#endif // SWIFT_AST_EVALUATOR_DEPENDENCIES_H
