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

#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <utility>

namespace swift {

class ProtocolConformance;
class ProtocolDecl;
class SubstitutableType;
class Substitution;
class TypeAliasDecl;
class ValueDecl;

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

/// Map from value requirements to the corresponding conformance witnesses.
typedef llvm::DenseMap<ValueDecl *, ProtocolConformanceWitness> ValueWitnessMap;

/// Map from a directly-inherited protocol to its corresponding protocol
/// conformance.
typedef llvm::DenseMap<ProtocolDecl *, ProtocolConformance *>
  InheritedConformanceMap;

/// \brief Describes how a particular type conforms to a given protocol,
/// providing the mapping from the protocol members to the type (or extension)
/// members that provide the functionality for the concrete type.
class ProtocolConformance {
  /// \brief The mapping of individual requirements in the protocol over to
  /// the declarations that satisfy those requirements.
  ValueWitnessMap Mapping;
  
  /// \brief The mapping of individual archetypes in the protocol over to
  /// the types used to satisy the type requirements.
  TypeSubstitutionMap TypeMapping;
  
  /// \brief The mapping from any directly-inherited protocols over to the
  /// protocol conformance structures that indicate how the given type meets
  /// the requirements of those protocols.
  InheritedConformanceMap InheritedMapping;

  /// The set of requirements for which we have used default definitions or
  /// otherwise deduced the result.
  llvm::SmallPtrSet<ValueDecl *, 4> DefaultedDefinitions;

public:
  ProtocolConformance(ValueWitnessMap &&valueWitnesses,
                      TypeSubstitutionMap &&typeWitnesses,
                      InheritedConformanceMap &&inheritedConformances,
                      llvm::ArrayRef<ValueDecl *> defaultedDefinitions)
    : Mapping(std::move(valueWitnesses)),
      TypeMapping(std::move(typeWitnesses)),
      InheritedMapping(std::move(inheritedConformances))
  {
    for (auto def : defaultedDefinitions)
      DefaultedDefinitions.insert(def);
  }


  /// Retrieve the type witness for the given associated type.
  Type getTypeWitness(TypeAliasDecl *assocType) const;

  /// Retrieve the complete set of type witnesses.
  const TypeSubstitutionMap &getTypeWitnesses() const {
    return TypeMapping;
  }

  /// Retrieve the value witness for the given requirement.
  ProtocolConformanceWitness getValueWitness(ValueDecl *requirement) const {
    auto known = Mapping.find(requirement);
    assert(known != Mapping.end());
    return known->second;
  }

  /// Retrieve the complete set of value witnesses.
  ValueWitnessMap &getValueWitnesses() { return Mapping; }

  /// Retrieve the complete set of value witnesses.
  const ValueWitnessMap &getValueWitnesses() const { return Mapping; }

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
