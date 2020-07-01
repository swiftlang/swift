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

#include "swift/AST/AnyRequest.h"
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

struct DependencyRecorder;

/// A \c DependencyCollector defines an abstract write-only buffer of
/// \c Reference objects. References are added to a collector during the write
/// phase of request evaluation (in \c writeDependencySink) with the various
/// \c add* functions below..
///
/// A \c DependencyCollector cannot be created directly. You must invoke
/// \c DependencyRecorder::record, which will wire a dependency collector into
/// the provided continuation block.
struct DependencyCollector {
  friend DependencyRecorder;

  struct Reference {
  public:
    enum class Kind {
      Empty,
      Tombstone,
      UsedMember,
      PotentialMember,
      TopLevel,
      Dynamic,
    } kind;

    NominalTypeDecl *subject;
    DeclBaseName name;
    bool cascades;

  private:
    Reference(Kind kind, NominalTypeDecl *subject, DeclBaseName name,
              bool cascades)
        : kind(kind), subject(subject), name(name), cascades(cascades) {}

  public:
    static Reference empty() {
      return {Kind::Empty, llvm::DenseMapInfo<NominalTypeDecl *>::getEmptyKey(),
              llvm::DenseMapInfo<DeclBaseName>::getEmptyKey(), false};
    }

    static Reference tombstone() {
      return {Kind::Tombstone,
              llvm::DenseMapInfo<NominalTypeDecl *>::getTombstoneKey(),
              llvm::DenseMapInfo<DeclBaseName>::getTombstoneKey(), false};
    }

  public:
    static Reference usedMember(NominalTypeDecl *subject, DeclBaseName name,
                                bool cascades) {
      return {Kind::UsedMember, subject, name, cascades};
    }

    static Reference potentialMember(NominalTypeDecl *subject, bool cascades) {
      return {Kind::PotentialMember, subject, DeclBaseName(), cascades};
    }

    static Reference topLevel(DeclBaseName name, bool cascades) {
      return {Kind::TopLevel, nullptr, name, cascades};
    }

    static Reference dynamic(DeclBaseName name, bool cascades) {
      return {Kind::Dynamic, nullptr, name, cascades};
    }

  public:
    struct Info {
      static inline Reference getEmptyKey() { return Reference::empty(); }
      static inline Reference getTombstoneKey() {
        return Reference::tombstone();
      }
      static inline unsigned getHashValue(const Reference &Val) {
        return llvm::hash_combine(Val.kind, Val.subject,
                                  Val.name.getAsOpaquePointer());
      }
      static bool isEqual(const Reference &LHS, const Reference &RHS) {
        return LHS.kind == RHS.kind && LHS.subject == RHS.subject &&
               LHS.name == RHS.name;
      }
    };
  };

public:
  using ReferenceSet = llvm::DenseSet<Reference, Reference::Info>;

private:
  DependencyRecorder &parent;
  ReferenceSet scratch;

public:
  explicit DependencyCollector(DependencyRecorder &parent) : parent(parent) {}

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
  /// Retrieves the dependency recorder that created this dependency collector.
  const DependencyRecorder &getRecorder() const { return parent; }

  /// Returns \c true if this collector has not accumulated
  /// any \c Reference objects.
  bool empty() const { return scratch.empty(); }
};

/// A \c DependencyRecorder is an aggregator of named references discovered in a
/// particular \c DependencyScope during the course of request evaluation.
struct DependencyRecorder {
  friend DependencyCollector;

  enum class Mode {
    // Enables the status quo of recording direct dependencies.
    //
    // This mode restricts the dependency collector to ignore changes of
    // scope. This has practical effect of charging all unqualified lookups to
    // the primary file being acted upon instead of to the destination file.
    DirectDependencies,
    // Enables a legacy mode of dependency tracking that makes a distinction
    // between private and cascading edges, and does not directly capture
    // transitive dependencies.
    //
    // By default, the dependency collector moves to register dependencies in
    // the referenced name trackers at the top of the active dependency stack.
    LegacyCascadingDependencies,
  };

private:
  /// A stack of dependency sources in the order they were evaluated.
  llvm::SmallVector<evaluator::DependencySource, 8> dependencySources;
  llvm::DenseMap<SourceFile *, DependencyCollector::ReferenceSet>
      fileReferences;
  llvm::DenseMap<AnyRequest, DependencyCollector::ReferenceSet>
      requestReferences;
  Mode mode;
  bool isRecording;

public:
  explicit DependencyRecorder(Mode mode) : mode{mode}, isRecording{false} {};

private:
  /// Records the given \c Reference as a dependency of the current dependency
  /// source.
  ///
  /// This is as opposed to merely collecting a \c Reference, which may just buffer
  /// it for realization or replay later.
  void realize(const DependencyCollector::Reference &ref);

public:
  /// Begins the recording of references by invoking the given continuation
  /// with a fresh \c DependencyCollector object. This object should be used
  /// to buffer dependency-relevant references to names looked up by a
  /// given request.
  ///
  /// Recording only occurs for requests that are dependency sinks.
  void record(const llvm::SetVector<swift::ActiveRequest> &stack,
              llvm::function_ref<void(DependencyCollector &)> rec);

  /// Replays the \c Reference objects collected by a given cached request and
  /// its sub-requests into the current dependency scope.
  ///
  /// Dependency replay ensures that cached requests do not "hide" names from
  /// the active dependency scope. This would otherwise occur frequently in
  /// batch mode, where cached requests effectively block the re-evaluation of
  /// a large quantity of computations that perform name lookups by design.
  ///
  /// Replay need only occur for requests that are (separately) cached.
  void replay(const llvm::SetVector<swift::ActiveRequest> &stack,
              const swift::ActiveRequest &req);
private:
  /// Given the current stack of requests and a buffer of \c Reference objects
  /// walk the active stack looking for the next-innermost cached request. If
  /// found, insert the buffer of references into that request's known reference
  /// set.
  ///
  /// This algorithm ensures that references propagate lazily up the request
  /// graph from cached sub-requests to their cached parents. Once this process
  /// completes, all cached requests in the request graph will see the
  /// union of all references recorded while evaluating their sub-requests.
  ///
  /// This algorithm *must* be tail-called during
  /// \c DependencyRecorder::record or \c DependencyRecorder::replay
  /// or the corresponding set of references for the active dependency scope
  /// will become incoherent.
  void
  unionNearestCachedRequest(ArrayRef<swift::ActiveRequest> stack,
                            const DependencyCollector::ReferenceSet &scratch);

public:
  using ReferenceEnumerator =
      llvm::function_ref<void(const DependencyCollector::Reference &)>;

  /// Enumerates the set of references associated with a given source file,
  /// passing them to the given enumeration callback.
  ///
  /// The order of enumeration is completely undefined. It is the responsibility
  /// of callers to ensure they are order-invariant or are sorting the result.
  void enumerateReferencesInFile(const SourceFile *SF,
                                 ReferenceEnumerator f) const ;

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
    switch (mode) {
    case Mode::LegacyCascadingDependencies:
      return dependencySources.back().getPointer();
    case Mode::DirectDependencies:
      return dependencySources.front().getPointer();
    }
  }

public:
  /// An RAII type that manages manipulating the evaluator's
  /// dependency source stack. It is specialized to be zero-cost for
  /// requests that are not dependency sources.
  template <typename Request, typename = detail::void_t<>> struct StackRAII {
    StackRAII(DependencyRecorder &DR, const Request &Req) {}
  };

  template <typename Request>
  struct StackRAII<Request,
                   typename std::enable_if<Request::isDependencySource>::type> {
    NullablePtr<DependencyRecorder> Coll;
    StackRAII(DependencyRecorder &coll, const Request &Req) {
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
  /// Returns \c true if the scope of the current active source cascades.
  ///
  /// If there is no active scope, the result always cascades.
  bool isActiveSourceCascading() const {
    switch (mode) {
    case Mode::LegacyCascadingDependencies:
      return getActiveSourceScope() == evaluator::DependencyScope::Cascading;
    case Mode::DirectDependencies:
      return false;
    }
    llvm_unreachable("invalid mode");
  }
};
} // end namespace evaluator

} // end namespace swift

#endif // SWIFT_AST_EVALUATOR_DEPENDENCIES_H
