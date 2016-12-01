//===--- GenericEnvironment.h - Generic Environment AST ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the GenericEnvironment class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_GENERIC_ENVIRONMENT_H
#define SWIFT_AST_GENERIC_ENVIRONMENT_H

#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/GenericParamKey.h"
#include "swift/AST/GenericSignature.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {

class ArchetypeBuilder;
class ASTContext;
class GenericTypeParamType;
class SILModule;
class SILType;

/// Describes the mapping between archetypes and interface types for the
/// generic parameters of a DeclContext.
class alignas(1 << DeclAlignInBits) GenericEnvironment final
                  : private llvm::TrailingObjects<GenericEnvironment, Type> {
  GenericSignature *Signature;
  ArchetypeBuilder *Builder;
  TypeSubstitutionMap ArchetypeToInterfaceMap;

  friend TrailingObjects;

  size_t numTrailingObjects(OverloadToken<Type>) const {
    return Signature->getGenericParams().size();
  }

  /// Retrieve the array containing the context types associated with the
  /// generic parameters, stored in parallel with the generic parameters of the
  /// generic signature.
  MutableArrayRef<Type> getContextTypes() {
    return MutableArrayRef<Type>(getTrailingObjects<Type>(),
                                 Signature->getGenericParams().size());
  }

  /// Retrieve the array containing the context types associated with the
  /// generic parameters, stored in parallel with the generic parameters of the
  /// generic signature.
  ArrayRef<Type> getContextTypes() const {
    return ArrayRef<Type>(getTrailingObjects<Type>(),
                          Signature->getGenericParams().size());
  }

  GenericEnvironment(GenericSignature *signature,
                     ArchetypeBuilder *builder,
                     TypeSubstitutionMap interfaceToArchetypeMap);

  friend class ArchetypeType;
  friend class ArchetypeBuilder;
  
  ArchetypeBuilder *getArchetypeBuilder() const { return Builder; }
  void clearArchetypeBuilder() { Builder = nullptr; }

  /// Query function suitable for use as a \c TypeSubstitutionFn that queries
  /// the mapping of interface types to archetypes.
  class QueryInterfaceTypeSubstitutions {
    const GenericEnvironment *self;

  public:
    QueryInterfaceTypeSubstitutions(const GenericEnvironment *self)
      : self(self) { }

    Type operator()(SubstitutableType *type) const;
  };
  friend class QueryInterfaceTypeSubstitutions;
  
public:
  GenericSignature *getGenericSignature() const {
    return Signature;
  }

  ArrayRef<GenericTypeParamType *> getGenericParams() const {
    return Signature->getGenericParams();
  }

  /// Determine whether this generic environment contains the given
  /// primary archetype.
  bool containsPrimaryArchetype(ArchetypeType *archetype) const;

  static
  GenericEnvironment *get(GenericSignature *signature,
                          TypeSubstitutionMap interfaceToArchetypeMap);

  /// Create a new, "incomplete" generic environment that will be populated
  /// by calls to \c addMapping().
  static
  GenericEnvironment *getIncomplete(GenericSignature *signature,
                                    ArchetypeBuilder *builder);

  /// Add a mapping of a generic parameter to a specific type (which may be
  /// an archetype)
  void addMapping(GenericParamKey key, Type contextType);

  /// Retrieve the mapping for the given generic parameter, if present.
  ///
  /// This is only useful when lazily populating a generic environment.
  Optional<Type> getMappingIfPresent(GenericParamKey key) const;

  /// Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  /// Only allow placement new.
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem); 
    return Mem; 
  }

  /// Map a contextual type to an interface type.
  Type mapTypeOutOfContext(ModuleDecl *M, Type type) const;

  /// Map an interface type to a contextual type.
  Type mapTypeIntoContext(ModuleDecl *M, Type type) const;

  /// Map a generic parameter type to a contextual type.
  Type mapTypeIntoContext(GenericTypeParamType *type) const;

  /// \brief Map the given SIL interface type to a contextual type.
  ///
  /// This operation will also reabstract dependent types according to the
  /// abstraction level of their associated type requirements.
  SILType mapTypeIntoContext(SILModule &M, SILType type) const;

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

  ArrayRef<Substitution> getForwardingSubstitutions(ModuleDecl *M) const;

  void dump() const;
};
  
} // end namespace swift

#endif // SWIFT_AST_GENERIC_ENVIRONMENT_H

