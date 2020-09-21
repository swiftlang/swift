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
#include "swift/Basic/NullablePtr.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {

namespace evaluator {

namespace detail {
// Remove this when the compiler bumps to C++17.
template <typename...> using void_t = void;
} // namespace detail

// A \c DependencySource is currently defined to be a primary source file.
//
// The \c SourceFile instance is an artifact of the current dependency system,
// and should be scrapped if possible. It currently encodes the idea that
// edges in the incremental dependency graph invalidate entire files instead
// of individual contexts.
using DependencySource = swift::NullablePtr<SourceFile>;

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

  private:
    Reference(Kind kind, NominalTypeDecl *subject, DeclBaseName name)
        : kind(kind), subject(subject), name(name) {}

  public:
    static Reference empty() {
      return {Kind::Empty, llvm::DenseMapInfo<NominalTypeDecl *>::getEmptyKey(),
              llvm::DenseMapInfo<DeclBaseName>::getEmptyKey()};
    }

    static Reference tombstone() {
      return {Kind::Tombstone,
              llvm::DenseMapInfo<NominalTypeDecl *>::getTombstoneKey(),
              llvm::DenseMapInfo<DeclBaseName>::getTombstoneKey()};
    }

  public:
    static Reference usedMember(NominalTypeDecl *subject, DeclBaseName name) {
      return {Kind::UsedMember, subject, name};
    }

    static Reference potentialMember(NominalTypeDecl *subject) {
      return {Kind::PotentialMember, subject, DeclBaseName()};
    }

    static Reference topLevel(DeclBaseName name) {
      return {Kind::TopLevel, nullptr, name};
    }

    static Reference dynamic(DeclBaseName name) {
      return {Kind::Dynamic, nullptr, name};
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

private:
  /// A stack of dependency sources in the order they were evaluated.
  llvm::SmallVector<evaluator::DependencySource, 8> dependencySources;
  llvm::DenseMap<SourceFile *, DependencyCollector::ReferenceSet>
      fileReferences;
  llvm::DenseMap<AnyRequest, DependencyCollector::ReferenceSet>
      requestReferences;
  bool isRecording;

public:
  explicit DependencyRecorder() : isRecording{false} {};

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
  /// Returns the active dependency's source file, or \c nullptr if no
  /// dependency source is active.
  ///
  /// The use of this accessor is strongly discouraged, as it implies that a
  /// dependency sink is seeking to filter out names based on the files they
  /// come from. Existing callers are being migrated to more reasonable ways
  /// of judging the relevancy of a dependency.
  evaluator::DependencySource getActiveDependencySourceOrNull() const {
    if (dependencySources.empty())
      return nullptr;
    return dependencySources.front();
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
      if (Source.isNull() || !Source.get()->isPrimary()) {
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
};
} // end namespace evaluator

} // end namespace swift

#endif // SWIFT_AST_EVALUATOR_DEPENDENCIES_H
