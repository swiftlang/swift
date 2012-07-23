//===--- ArchetypeBuilder.h - Generic Archetype Builder -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Support for collecting a set of generic requirements, both explicitly stated
// and inferred, and computing the archetypes and required witness tables from
// those requirements.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Optional.h"
#include "llvm/ADT/DenseMap.h"
#include <memory>

namespace swift {

class ArchetypeType;
class ProtocolDecl;
class Requirement;
class SourceLoc;
class Type;
class TypeAliasDecl;
class TypeChecker;

/// \brief Collects a set of requirements of generic parameters, both explicitly
/// stated and inferred, and determines the set of archetypes for each of
/// the generic parameters.
class ArchetypeBuilder {
  struct PotentialArchetype;

  TypeChecker &TC;
  struct Implementation;
  std::unique_ptr<Implementation> Impl;

  ArchetypeBuilder(const ArchetypeBuilder &) = delete;
  ArchetypeBuilder &operator=(const ArchetypeBuilder &) = delete;

  /// \brief Resolve the given type to the potential archetype it names.
  ///
  /// This routine will synthesize nested types as required to refer to a
  /// potential archetype, even in cases where no requirement specifies the
  /// requirement for such an archetype. FIXME: The failure to include such a
  /// requirement will be diagnosed at some point later (when the types in the
  /// signature are fully resolved).
  ///
  /// For any type that cannot refer to an archetype, this routine returns null.
  PotentialArchetype *resolveType(Type T);

  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype conforms to the given protocol.
  bool addConformanceRequirement(PotentialArchetype *T,
                                 ProtocolDecl *Proto);

  /// \brief Add a new same-type requirement specifying that the given potential
  /// archetypes should map to the equivalent archetype.
  bool addSameTypeRequirement(PotentialArchetype *T1,
                              SourceLoc EqualLoc,
                              PotentialArchetype *T2);

public:
  ArchetypeBuilder(TypeChecker &TC);
  ArchetypeBuilder(ArchetypeBuilder &&) = default;
  ArchetypeBuilder &operator=(ArchetypeBuilder &&) = default;
  ~ArchetypeBuilder();

  /// \brief Add a new generic parameter for which there may be requirements.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool addGenericParameter(TypeAliasDecl *GenericParam,
                           Optional<unsigned> Index = Nothing);

  /// \brief Add a new requirement.
  ///
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  bool addRequirement(const Requirement &Req);

  /// \brief Assign archetypes
  ///
  /// This operation should only be performed after all generic parameters and
  /// requirements have been added to the builder.
  llvm::DenseMap<TypeAliasDecl *, ArchetypeType *> assignArchetypes();

  // FIXME: Infer requirements from signatures
  // FIXME: Compute the set of 'extra' witness tables needed to express this
  // requirement set.

  /// \brief Dump all of the requirements, both specified and inferred.
  void dump();
};

} // end namespace swift
