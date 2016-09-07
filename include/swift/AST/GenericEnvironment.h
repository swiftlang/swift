//===--- GenericEnvironment.h - Generic Environment AST ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the GenericEnvironment class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_GENERIC_ENVIRONMENT_H
#define SWIFT_AST_GENERIC_ENVIRONMENT_H

#include "swift/AST/ProtocolConformance.h"

namespace swift {

class ASTContext;

/// Describes the mapping between archetypes and interface types for the
/// generic parameters of a DeclContext.
class GenericEnvironment final {
  TypeSubstitutionMap ArchetypeToInterfaceMap;
  TypeSubstitutionMap InterfaceToArchetypeMap;

public:
  const TypeSubstitutionMap &getArchetypeToInterfaceMap() const {
    return ArchetypeToInterfaceMap;
  }

  const TypeSubstitutionMap &getInterfaceToArchetypeMap() const {
    return InterfaceToArchetypeMap;
  }

  explicit GenericEnvironment(TypeSubstitutionMap interfaceToArchetypeMap);

  static GenericEnvironment *get(ASTContext &ctx,
                                 TypeSubstitutionMap interfaceToArchetypeMap);

  /// Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  /// Only allow allocation of GenericEnvironments using the allocator
  /// in ASTContext.
  void *operator new(size_t bytes, const ASTContext &ctx);

  /// Map a contextual type to an interface type.
  Type mapTypeOutOfContext(ModuleDecl *M, Type type) const;

  /// Map an interface type to a contextual type.
  Type mapTypeIntoContext(ModuleDecl *M, Type type) const;

  /// Derive a contextual type substitution map from a substitution array.
  /// This is just like GenericSignature::getSubstitutionMap(), except
  /// with contextual types instead of interface types.
  void
  getSubstitutionMap(ModuleDecl *mod,
                     GenericSignature *sig,
                     ArrayRef<Substitution> subs,
                     TypeSubstitutionMap &subsMap,
                     ArchetypeConformanceMap &conformanceMap) const;

  ArrayRef<Substitution>
  getForwardingSubstitutions(ModuleDecl *M, GenericSignature *sig) const;

  void dump() const;
};
  
} // end namespace swift

#endif // SWIFT_AST_GENERIC_ENVIRONMENT_H

