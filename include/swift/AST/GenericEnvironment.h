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
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericParamKey.h"
#include "swift/Basic/Compiler.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TrailingObjects.h"
#include <utility>

namespace swift {

class ArchetypeType;
class GenericSignatureBuilder;
class ASTContext;
class GenericTypeParamType;
class SILModule;
class SILType;

/// Query function suitable for use as a \c TypeSubstitutionFn that queries
/// the mapping of interface types to archetypes.
class QueryInterfaceTypeSubstitutions {
  const GenericEnvironment *self;
  
public:
  QueryInterfaceTypeSubstitutions(const GenericEnvironment *self)
  : self(self) { }
  
  Type operator()(SubstitutableType *type) const;
};

/// Describes the mapping between archetypes and interface types for the
/// generic parameters of a DeclContext.
///
/// The most frequently used method here is mapTypeIntoContext(), which
/// maps an interface type to a type written in terms of the generic
/// environment's archetypes; to go in the other direction, use
/// TypeBase::mapTypeOutOfContext().
///
class alignas(1 << DeclAlignInBits) GenericEnvironment final
        : private llvm::TrailingObjects<GenericEnvironment, Type> {
  GenericSignature Signature = GenericSignature();
  GenericSignatureBuilder *Builder = nullptr;

  friend TrailingObjects;

  size_t numTrailingObjects(OverloadToken<Type>) const;

  /// Retrieve the array containing the context types associated with the
  /// generic parameters, stored in parallel with the generic parameters of the
  /// generic signature.
  MutableArrayRef<Type> getContextTypes();

  /// Retrieve the array containing the context types associated with the
  /// generic parameters, stored in parallel with the generic parameters of the
  /// generic signature.
  ArrayRef<Type> getContextTypes() const;

  GenericEnvironment(GenericSignature signature,
                     GenericSignatureBuilder *builder);

  friend ArchetypeType;
  friend GenericSignatureBuilder;
  
  GenericSignatureBuilder *getGenericSignatureBuilder() const { return Builder; }

  friend QueryInterfaceTypeSubstitutions;

public:
  GenericSignature getGenericSignature() const {
    return Signature;
  }

  TypeArrayView<GenericTypeParamType> getGenericParams() const;

  /// Create a new, "incomplete" generic environment that will be populated
  /// by calls to \c addMapping().
  static
  GenericEnvironment *getIncomplete(GenericSignature signature,
                                    GenericSignatureBuilder *builder);

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
  static Type mapTypeIntoContext(GenericEnvironment *genericEnv,
                                 Type type);

  /// Map an interface type to a contextual type.
  Type mapTypeIntoContext(Type type) const;

  /// Map an interface type to a contextual type.
  Type mapTypeIntoContext(Type type,
                          LookupConformanceFn lookupConformance) const;

  /// Map a generic parameter type to a contextual type.
  Type mapTypeIntoContext(GenericTypeParamType *type) const;

  /// Map the given SIL interface type to a contextual type.
  ///
  /// This operation will also reabstract dependent types according to the
  /// abstraction level of their associated type requirements.
  SILType mapTypeIntoContext(SILModule &M, SILType type) const;

  /// Map an interface type's protocol conformance into the corresponding
  /// conformance for the contextual type.
  static std::pair<Type, ProtocolConformanceRef>
  mapConformanceRefIntoContext(GenericEnvironment *genericEnv,
                               Type conformingType,
                               ProtocolConformanceRef conformance);

  /// Map an interface type's protocol conformance into the corresponding
  /// conformance for the contextual type.
  std::pair<Type, ProtocolConformanceRef>
  mapConformanceRefIntoContext(Type conformingType,
                               ProtocolConformanceRef conformance) const;
          
  /// Get the sugared form of a generic parameter type.
  GenericTypeParamType *getSugaredType(GenericTypeParamType *type) const;

  /// Get the sugared form of a type by substituting any
  /// generic parameter types by their sugared form.
  Type getSugaredType(Type type) const;

  SubstitutionMap getForwardingSubstitutionMap() const;

  void dump(raw_ostream &os) const;

  void dump() const;
};
  
} // end namespace swift

#endif // SWIFT_AST_GENERIC_ENVIRONMENT_H

