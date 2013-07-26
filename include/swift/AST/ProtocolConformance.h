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

namespace swift {

class SubstitutableType;
class Substitution;
class ValueDecl;
class ProtocolDecl;

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
  
/// \brief Describes how a particular type conforms to a given protocol,
/// providing the mapping from the protocol members to the type (or extension)
/// members that provide the functionality for the concrete type.
class ProtocolConformance {
public:
  /// \brief The mapping of individual requirements in the protocol over to
  /// the declarations that satisfy those requirements.
  llvm::DenseMap<ValueDecl *, ProtocolConformanceWitness> Mapping;
  
  /// \brief The mapping of individual archetypes in the protocol over to
  /// the types used to satisy the type requirements.
  TypeSubstitutionMap TypeMapping;
  
  /// \brief The mapping from any directly-inherited protocols over to the
  /// protocol conformance structures that indicate how the given type meets
  /// the requirements of those protocols.
  llvm::DenseMap<ProtocolDecl *, ProtocolConformance *> InheritedMapping;

  /// The set of requirements for which we have used default definitions or
  /// otherwise deduced the result.
  llvm::SmallPtrSet<ValueDecl *, 4> DefaultedDefinitions;
};

} // end namespace swift

#endif // LLVM_SWIFT_AST_PROTOCOLCONFORMANCE_H
