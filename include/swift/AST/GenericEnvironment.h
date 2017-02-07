//===--- GenericEnvironment.h - Generic Environment AST ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
#include "swift/Basic/Compiler.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TrailingObjects.h"
#include <utility>

namespace swift {

class ArchetypeBuilder;
class ASTContext;
class GenericTypeParamType;
class SILModule;
class SILType;

/// Describes the mapping between archetypes and interface types for the
/// generic parameters of a DeclContext.
class alignas(1 << DeclAlignInBits) GenericEnvironment final
        : private llvm::TrailingObjects<GenericEnvironment, Type,
                                        std::pair<ArchetypeType *,
                                                  GenericTypeParamType *>> {
  GenericSignature *Signature = nullptr;
  ArchetypeBuilder *Builder = nullptr;
  DeclContext *OwningDC = nullptr;

  // The number of generic type parameter -> context type mappings we have
  // recorded so far. This saturates at the number of generic type parameters,
  // at which point the archetype-to-interface trailing array is sorted.
  unsigned NumMappingsRecorded : 16;

  // The number of archetype-to-interface type mappings. This is always <=
  // \c NumMappingsRecorded.
  unsigned NumArchetypeToInterfaceMappings : 16;

  friend TrailingObjects;

  /// An entry in the array mapping from archetypes to their corresponding
  /// generic type parameters.
  typedef std::pair<ArchetypeType *, GenericTypeParamType *>
                                                    ArchetypeToInterfaceMapping;

  size_t numTrailingObjects(OverloadToken<Type>) const {
    return Signature->getGenericParams().size();
  }

  size_t numTrailingObjects(OverloadToken<ArchetypeToInterfaceMapping>) const {
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

  /// Retrieve the active set of archetype-to-interface mappings.
  ArrayRef<ArchetypeToInterfaceMapping>
                                getActiveArchetypeToInterfaceMappings() const {
    return { getTrailingObjects<ArchetypeToInterfaceMapping>(),
             NumArchetypeToInterfaceMappings };
  }

  /// Retrieve the active set of archetype-to-interface mappings.
  MutableArrayRef<ArchetypeToInterfaceMapping>
                                      getActiveArchetypeToInterfaceMappings() {
    return { getTrailingObjects<ArchetypeToInterfaceMapping>(),
             NumArchetypeToInterfaceMappings };
  }

  /// Retrieve the buffer for the archetype-to-interface mappings.
  ///
  /// Only the first \c NumArchetypeToInterfaceMappings elements in the buffer
  /// are valid.
  MutableArrayRef<ArchetypeToInterfaceMapping>
                                      getArchetypeToInterfaceMappingsBuffer() {
    return { getTrailingObjects<ArchetypeToInterfaceMapping>(),
             Signature->getGenericParams().size() };
  }

  GenericEnvironment(GenericSignature *signature,
                     ArchetypeBuilder *builder);

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

  /// Query function suitable for use as a \c TypeSubstitutionFn that queries
  /// the mapping of archetypes back to interface types.
  class QueryArchetypeToInterfaceSubstitutions {
    const GenericEnvironment *self;

  public:
    QueryArchetypeToInterfaceSubstitutions(const GenericEnvironment *self)
      : self(self) { }

    Type operator()(SubstitutableType *type) const;
  };
  friend class QueryArchetypeToInterfaceSubstitutions;

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

  /// Create a new, "incomplete" generic environment that will be populated
  /// by calls to \c addMapping().
  static
  GenericEnvironment *getIncomplete(GenericSignature *signature,
                                    ArchetypeBuilder *builder);

  /// Set the owning declaration context for this generic environment.
  void setOwningDeclContext(DeclContext *owningDC);

  /// Retrieve the declaration context that owns this generic environment, if
  /// there is one.
  ///
  /// Note that several generic environments may refer to the same declaration
  /// context, because non-generic declarations nested within generic ones
  /// inherit the enclosing generic environment. In such cases, the owning
  /// context is the outermost context.
  DeclContext *getOwningDeclContext() const { return OwningDC; }

  /// Add a mapping of a generic parameter to a specific type (which may be
  /// an archetype)
  void addMapping(GenericParamKey key, Type contextType);

  /// Retrieve the mapping for the given generic parameter, if present.
  ///
  /// This is only useful when lazily populating a generic environment.
  Optional<Type> getMappingIfPresent(GenericParamKey key) const;

  /// Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) SWIFT_DELETE_OPERATOR_DELETED;

  /// Only allow placement new.
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem); 
    return Mem; 
  }

  /// Map an interface type to a contextual type.
  static Type mapTypeIntoContext(ModuleDecl *M,
                                 GenericEnvironment *genericEnv,
                                 Type type);

  /// Map a contextual type to an interface type.
  static Type mapTypeOutOfContext(GenericEnvironment *genericEnv,
                                  Type type);

  /// Map a contextual type to an interface type.
  Type mapTypeOutOfContext(Type type) const;

  /// Map an interface type to a contextual type.
  Type mapTypeIntoContext(ModuleDecl *M, Type type) const;

  /// Map an interface type to a contextual type.
  Type mapTypeIntoContext(Type type,
                          LookupConformanceFn lookupConformance) const;

  /// Map a generic parameter type to a contextual type.
  Type mapTypeIntoContext(GenericTypeParamType *type) const;

  /// \brief Map the given SIL interface type to a contextual type.
  ///
  /// This operation will also reabstract dependent types according to the
  /// abstraction level of their associated type requirements.
  SILType mapTypeIntoContext(SILModule &M, SILType type) const;

  /// Get the sugared form of a generic parameter type.
  GenericTypeParamType *getSugaredType(GenericTypeParamType *type) const;

  /// Get the sugared form of a type by substituting any
  /// generic parameter types by their sugared form.
  Type getSugaredType(Type type) const;

  /// Derive a contextual type substitution map from a substitution array.
  /// This is just like GenericSignature::getSubstitutionMap(), except
  /// with contextual types instead of interface types.
  SubstitutionMap
  getSubstitutionMap(ModuleDecl *mod,
                     SubstitutionList subs) const;

  /// Same as above, but updates an existing map.
  void
  getSubstitutionMap(ModuleDecl *mod,
                     SubstitutionList subs,
                     SubstitutionMap &subMap) const;

  /// Build a contextual type substitution map from a type substitution function
  /// and conformance lookup function.
  SubstitutionMap
  getSubstitutionMap(ModuleDecl *mod,
                     TypeSubstitutionFn subs,
                     LookupConformanceFn lookupConformance) const;

  SubstitutionList getForwardingSubstitutions() const;

  void dump() const;
};
  
} // end namespace swift

#endif // SWIFT_AST_GENERIC_ENVIRONMENT_H

