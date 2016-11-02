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

#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/GenericSignature.h"

namespace swift {

class ASTContext;
class GenericTypeParamType;

/// Describes the mapping between archetypes and interface types for the
/// generic parameters of a DeclContext.
class GenericEnvironment final {
  GenericSignature *Signature;
  TypeSubstitutionMap ArchetypeToInterfaceMap;
  TypeSubstitutionMap InterfaceToArchetypeMap;

public:
  GenericSignature *getGenericSignature() const {
    return Signature;
  }

  ArrayRef<GenericTypeParamType *> getGenericParams() const {
    return Signature->getGenericParams();
  }

  const TypeSubstitutionMap &getArchetypeToInterfaceMap() const {
    return ArchetypeToInterfaceMap;
  }

  const TypeSubstitutionMap &getInterfaceToArchetypeMap() const {
    return InterfaceToArchetypeMap;
  }

  GenericEnvironment(GenericSignature *signature,
                     TypeSubstitutionMap interfaceToArchetypeMap);

  static
  GenericEnvironment * get(ASTContext &ctx,
                           GenericSignature *signature,
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

  /// Map a generic parameter type to a contextual type.
  Type mapTypeIntoContext(GenericTypeParamType *type) const;

  /// Get the sugared form of a generic parameter type.
  GenericTypeParamType *getSugaredType(GenericTypeParamType *type) const;

  /// Derive a contextual type substitution map from a substitution array.
  /// This is just like GenericSignature::getSubstitutionMap(), except
  /// with contextual types instead of interface types.
  SubstitutionMap
  getSubstitutionMap(ModuleDecl *mod,
                     ArrayRef<Substitution> subs) const;

  /// Same as above, but updates an existing map.
  void
  getSubstitutionMap(ModuleDecl *mod,
                     ArrayRef<Substitution> subs,
                     SubstitutionMap &subMap) const;

  ArrayRef<Substitution>
  getForwardingSubstitutions(ModuleDecl *M) const;

  void dump() const;
};
  
} // end namespace swift

#endif // SWIFT_AST_GENERIC_ENVIRONMENT_H

