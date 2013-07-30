//===--- ProtocolConformance.h - AST Protocol Conformance -------*- C++ -*-===//
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
// This file defines the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PROTOCOLCONFORMANCE_H
#define SWIFT_AST_PROTOCOLCONFORMANCE_H

#include "swift/AST/Substitution.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <utility>

namespace swift {

class ProtocolConformance;
class ProtocolDecl;
class SubstitutableType;
class TypeAliasDecl;
class ValueDecl;
class Decl;
  
/// \brief Type substitution mapping from substitutable types to their
/// replacements.
typedef llvm::DenseMap<SubstitutableType *, Type> TypeSubstitutionMap;

/// \brief Describes a declaration used to satisfy, or "witness", a protocol
/// requirement.
struct ProtocolConformanceWitness {
  /// The declaration that satisfies the requirement.
  ValueDecl *Decl;
  
  /// The substitutions necessary to map the declaration's type to the
  /// requirement.
  ArrayRef<Substitution> Substitutions;
};

/// Map from non-type requirements to the corresponding conformance witnesses.
typedef llvm::DenseMap<ValueDecl *, ProtocolConformanceWitness> WitnessMap;

/// Map from associated type requirements to the corresponding substitution,
/// which captures the replacement type along with any conformances it requires.
typedef llvm::DenseMap<TypeAliasDecl *, Substitution> TypeWitnessMap;

/// Map from a directly-inherited protocol to its corresponding protocol
/// conformance.
typedef llvm::DenseMap<ProtocolDecl *, ProtocolConformance *>
  InheritedConformanceMap;

/// \brief Describes how a particular type conforms to a given protocol,
/// providing the mapping from the protocol members to the type (or extension)
/// members that provide the functionality for the concrete type.
class ProtocolConformance {
  /// \brief The type that conforms to the protocol.
  Type ConformingType;
  
  /// \brief The protocol being conformed to.
  ProtocolDecl *Protocol;
  
  /// \brief The ExtensionDecl or NominalTypeDecl that declares the conformance.
  Decl *ConformingDecl;
  
  /// \brief The mapping of individual requirements in the protocol over to
  /// the declarations that satisfy those requirements.
  WitnessMap Mapping;

  /// The mapping from associated type requirements to their substitutions.
  TypeWitnessMap TypeWitnesses;
  
  /// \brief The mapping from any directly-inherited protocols over to the
  /// protocol conformance structures that indicate how the given type meets
  /// the requirements of those protocols.
  InheritedConformanceMap InheritedMapping;

  /// The set of requirements for which we have used default definitions or
  /// otherwise deduced the result.
  llvm::SmallPtrSet<ValueDecl *, 4> DefaultedDefinitions;

public:
  ProtocolConformance(Type conformingType,
                      ProtocolDecl *protocol,
                      Decl *conformingDecl,
                      WitnessMap &&witnesses,
                      TypeWitnessMap &&typeWitnesses,
                      InheritedConformanceMap &&inheritedConformances,
                      llvm::ArrayRef<ValueDecl *> defaultedDefinitions)
    : ConformingType(conformingType),
      Protocol(protocol),
      ConformingDecl(conformingDecl),
      Mapping(std::move(witnesses)),
      TypeWitnesses(std::move(typeWitnesses)),
      InheritedMapping(std::move(inheritedConformances))
  {
    for (auto def : defaultedDefinitions)
      DefaultedDefinitions.insert(def);
  }
  
  /// Get the conforming type.
  Type getType() const { return ConformingType; }
  
  /// Get the protocol being conformed to.
  ProtocolDecl *getProtocol() const { return Protocol; }
  
  /// Get the declaration that provides the conformance. This can be either the
  /// original NominalTypeDecl for the type, or an ExtensionDecl.
  Decl *getConformingDecl() const { return ConformingDecl; }

  /// Retrieve the type witness for the given associated type.
  const Substitution &getTypeWitness(TypeAliasDecl *assocType) const {
    auto known = TypeWitnesses.find(assocType);
    assert(known != TypeWitnesses.end());
    return known->second;
  }

  /// Retrieve the complete set of type witnesses.
  const TypeWitnessMap &getTypeWitnesses() const {
    return TypeWitnesses;
  }

  /// Retrieve the non-type witness for the given requirement.
  ProtocolConformanceWitness getWitness(ValueDecl *requirement) const {
    auto known = Mapping.find(requirement);
    assert(known != Mapping.end());
    return known->second;
  }

  /// Retrieve the complete set of non-type witnesses.
  WitnessMap &getWitnesses() { return Mapping; }

  /// Retrieve the complete set of non-type witnesses.
  const WitnessMap &getWitnesses() const { return Mapping; }

  /// Retrieve the protocol conformance for a directly-inherited protocol.
  ProtocolConformance *getInheritedConformance(ProtocolDecl *protocol) const {
    auto known = InheritedMapping.find(protocol);
    assert(known != InheritedMapping.end());
    return known->second;
  }

  /// Retrieve the complete set of protocol conformances for directly inherited
  /// protocols.
  InheritedConformanceMap &getInheritedConformances() {
    return InheritedMapping;
  }

  /// Retrieve the complete set of protocol conformances for directly inherited
  /// protocols.
  const InheritedConformanceMap &getInheritedConformances() const {
    return InheritedMapping;
  }

  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  bool usesDefaultDefinition(ValueDecl *requirement) const {
    return DefaultedDefinitions.count(requirement) > 0;
  }

  /// Retrieve the complete set of defaulted definitions.
  llvm::SmallPtrSet<ValueDecl *, 4> &getDefaultedDefinitions() {
    return DefaultedDefinitions;
  }

  /// Retrieve the complete set of defaulted definitions.
  const llvm::SmallPtrSet<ValueDecl *, 4> &getDefaultedDefinitions() const {
    return DefaultedDefinitions;
  }
};

} // end namespace swift

#endif // LLVM_SWIFT_AST_PROTOCOLCONFORMANCE_H
