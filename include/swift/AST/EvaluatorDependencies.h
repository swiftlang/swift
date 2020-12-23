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
#include <vector>

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

private:
  DependencyRecorder &parent;

public:
  explicit DependencyCollector(DependencyRecorder &parent);
  ~DependencyCollector();

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
};

/// A \c DependencyRecorder is an aggregator of named references discovered in a
/// particular \c DependencyScope during the course of request evaluation.
struct DependencyRecorder {
  friend DependencyCollector;

private:
  llvm::DenseMap<SourceFile *,
                 llvm::DenseSet<DependencyCollector::Reference,
                                DependencyCollector::Reference::Info>>
      fileReferences;
  llvm::DenseMap<AnyRequest, std::vector<DependencyCollector::Reference>>
      requestReferences;
  std::vector<llvm::DenseSet<DependencyCollector::Reference,
                             DependencyCollector::Reference::Info>>
      activeRequestReferences;

#ifndef NDEBUG
  bool isRecording = false;
#endif

public:
  void beginRequest(const swift::ActiveRequest &req);
  void endRequest(const swift::ActiveRequest &req);
  void replayCachedRequest(const swift::ActiveRequest &req);
  void handleDependencySourceRequest(const swift::ActiveRequest &req,
                                     SourceFile *source);

private:
  void recordDependency(const DependencyCollector::Reference &ref);

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
};
} // end namespace evaluator

} // end namespace swift

#endif // SWIFT_AST_EVALUATOR_DEPENDENCIES_H
