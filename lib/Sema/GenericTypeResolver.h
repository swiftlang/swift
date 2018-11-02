//===-- GenericTypeResolver.h - Generic Type Resolver Interface -*- C++ -*-===//
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
//  This file defines the GenericTypeResolver abstract interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_GENERICTYPERESOLVER_H
#define SWIFT_SEMA_GENERICTYPERESOLVER_H

#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {

class GenericSignatureBuilder;
class AssociatedTypeDecl;
class Identifier;
class ParamDecl;
class TypeChecker;
class TypeDecl;

/// Abstract class that resolves references into generic types during
/// type resolution.
class GenericTypeResolver {
public:
  virtual ~GenericTypeResolver();

  virtual bool usesArchetypes() = 0;

  /// Resolve the given interface type to a contextual type if necessary.
  virtual Type mapTypeIntoContext(Type type) = 0;

  /// Resolve a qualified reference to a type member within a dependent type.
  ///
  /// \param baseTy The base of the member access.
  /// \param baseRange The source range covering the base type.
  /// \param ref The reference to the dependent member type.
  ///
  /// \returns A type that refers to the dependent member type, or an error
  /// type if such a reference is ill-formed.
  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref) = 0;

  /// Determine whether the given types are equivalent within the generic
  /// context.
  virtual bool areSameType(Type type1, Type type2) = 0;
};

/// Generic type resolver that leaves all generic types dependent.
///
/// This generic type resolver leaves generic type parameter types alone
/// and only trivially resolves dependent member types.
class DependentGenericTypeResolver : public GenericTypeResolver {
public:
  virtual bool usesArchetypes() { return false; }

  virtual Type mapTypeIntoContext(Type type);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref);

  virtual bool areSameType(Type type1, Type type2);
};

/// Generic type resolver that maps a generic type parameter type to its
/// archetype.
///
/// This generic type resolver replaces generic type parameter types with their
/// corresponding archetypes, eliminating all dependent types in the process.
class GenericTypeToArchetypeResolver : public GenericTypeResolver {
  GenericEnvironment *GenericEnv;

public:
  explicit GenericTypeToArchetypeResolver(GenericEnvironment *env)
      : GenericEnv(env) { }

  explicit GenericTypeToArchetypeResolver(DeclContext *dc)
      : GenericEnv(dc->getGenericEnvironmentOfContext()) { }

  virtual bool usesArchetypes() { return true; }

  virtual Type mapTypeIntoContext(Type type);

  virtual Type resolveDependentMemberType(Type baseTy, DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref);

  virtual bool areSameType(Type type1, Type type2);
};

/// Generic type resolver that only handles what can appear in a protocol
/// definition, i.e. Self, and Self.A.B.C dependent types.
///
/// This should only be used when resolving/validating where clauses in
/// protocols.
class ProtocolRequirementTypeResolver : public GenericTypeResolver {
public:
  virtual bool usesArchetypes() { return false; }

  virtual Type mapTypeIntoContext(Type type);

  virtual Type resolveDependentMemberType(Type baseTy, DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref);

  virtual bool areSameType(Type type1, Type type2);
};

/// Generic type resolver that performs complete resolution of dependent
/// types based on a given generic signature builder.
///
/// This generic type resolver should be used after all requirements have been
/// introduced into the generic signature builder, including inferred requirements,
/// to check the signature of a generic declaration and resolve (for example)
/// all dependent member refers to archetype members.
class CompleteGenericTypeResolver : public GenericTypeResolver {
  TypeChecker &tc;
  GenericSignature *genericSig;
  GenericSignatureBuilder *builder;

public:
  CompleteGenericTypeResolver(TypeChecker &tc, GenericSignature *genericSig);

  virtual bool usesArchetypes() { return false; }

  virtual Type mapTypeIntoContext(Type type);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref);

  virtual bool areSameType(Type type1, Type type2);
};

} // end namespace swift

#endif
