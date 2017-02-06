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

class ArchetypeBuilder;
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

  /// Resolve the given generic type parameter to its type.
  ///
  /// This routine is used whenever type checking encounters a reference to a
  /// generic parameter. It can replace the generic parameter with (for example)
  /// a concrete type or an archetype, depending on context.
  ///
  /// \param gp The generic parameter to resolve.
  ///
  /// \returns The resolved generic type parameter type, which may be \c gp.
  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp) = 0;

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

  /// Resolve an unqualified reference to an associated type of the 'Self' type
  /// of a protocol.
  ///
  /// \param selfTy The base of the member access.
  /// \param assocType The associated type.
  ///
  /// \returns A type that refers to the dependent member type, or an error
  /// type if such a reference is ill-formed.
  virtual Type resolveSelfAssociatedType(Type selfTy,
                                         AssociatedTypeDecl *assocType) = 0;

  /// Resolve the self type within the given context.
  ///
  /// \param dc A context in which type checking occurs, which must be a type
  /// context (i.e., nominal type or extension thereof).
  ///
  /// \returns the type of context.
  virtual Type resolveTypeOfContext(DeclContext *dc) = 0;

  /// Retrieve the type when referring to the given type declaration within
  /// its context.
  ///
  /// \param decl A type declaration.
  ///
  /// \returns the type of the declaration in context..
  virtual Type resolveTypeOfDecl(TypeDecl *decl) = 0;

  /// Determine whether the given types are equivalent within the generic
  /// context.
  virtual bool areSameType(Type type1, Type type2) = 0;

  /// Set the contextual type or the interface type of the parameter.
  virtual void recordParamType(ParamDecl *decl, Type ty) = 0;
};

/// Generic type resolver that leaves all generic types dependent.
///
/// This generic type resolver leaves generic type parameter types alone
/// and only trivially resolves dependent member types.
class DependentGenericTypeResolver : public GenericTypeResolver {
  ArchetypeBuilder &Builder;
  ArrayRef<GenericTypeParamType *> GenericParams;

public:
  DependentGenericTypeResolver(ArchetypeBuilder &builder,
                               ArrayRef<GenericTypeParamType *> genericParams)
    : Builder(builder), GenericParams(genericParams) { }

  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref);

  virtual Type resolveSelfAssociatedType(Type selfTy,
                                         AssociatedTypeDecl *assocType);

  virtual Type resolveTypeOfContext(DeclContext *dc);

  virtual Type resolveTypeOfDecl(TypeDecl *decl);

  virtual bool areSameType(Type type1, Type type2);

  virtual void recordParamType(ParamDecl *decl, Type ty);
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

  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref);

  virtual Type resolveSelfAssociatedType(Type selfTy,
                                         AssociatedTypeDecl *assocType);

  virtual Type resolveTypeOfContext(DeclContext *dc);

  virtual Type resolveTypeOfDecl(TypeDecl *decl);

  virtual bool areSameType(Type type1, Type type2);

  virtual void recordParamType(ParamDecl *decl, Type ty);
};

/// Generic type resolver that performs complete resolution of dependent
/// types based on a given archetype builder.
///
/// This generic type resolver should be used after all requirements have been
/// introduced into the archetype builder, including inferred requirements,
/// to check the signature of a generic declaration and resolve (for example)
/// all dependent member refers to archetype members.
class CompleteGenericTypeResolver : public GenericTypeResolver {
  TypeChecker &TC;
  ArchetypeBuilder &Builder;
  ArrayRef<GenericTypeParamType *> GenericParams;

public:
  CompleteGenericTypeResolver(TypeChecker &tc, ArchetypeBuilder &builder,
                              ArrayRef<GenericTypeParamType *> genericParams)
    : TC(tc), Builder(builder), GenericParams(genericParams) { }

  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          ComponentIdentTypeRepr *ref);

  virtual Type resolveSelfAssociatedType(Type selfTy,
                                         AssociatedTypeDecl *assocType);

  virtual Type resolveTypeOfContext(DeclContext *dc);

  virtual Type resolveTypeOfDecl(TypeDecl *decl);

  virtual bool areSameType(Type type1, Type type2);

  virtual void recordParamType(ParamDecl *decl, Type ty);
};

} // end namespace swift

#endif
