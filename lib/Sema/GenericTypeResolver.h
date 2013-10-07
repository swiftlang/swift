//===-- GenericTypeResolver.h - Generic Type Resolver Interface -*- C++ -*-===//
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
//  This file defines the GenericTypeResolver abstract interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_GENERICTYPERESOLVER_H
#define SWIFT_SEMA_GENERICTYPERESOLVER_H

#include "swift/AST/Type.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {

class ArchetypeBuilder;
class AssociatedTypeDecl;
class Identifier;

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

  /// Resolve a reference to a member within a dependent type.
  ///
  /// \param baseTy The base of the member access.
  /// \param baseRange The source range covering the base type.
  /// \param name The name of the member type.
  /// \param nameLoc The location of the member name.
  ///
  /// \returns A type that refers to the dependent member type, or an error
  /// type if such a reference is ill-formed.
  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          Identifier name,
                                          SourceLoc nameLoc) = 0;
};

/// Generic type resolver that leaves all generic types dependent.
///
/// This generic type resolver leaves generic type parameter types alone
/// and only trivially resolves dependent member types.
class DependentGenericTypeResolver : public GenericTypeResolver {
  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          Identifier name,
                                          SourceLoc nameLoc);
};

/// Generic type resolver that maps a generic type parameter type to its
/// archetype.
///
/// This generic type resolver replaces generic type parameter types with their
/// corresponding archetypes, eliminating all dependent types in the process.
class GenericTypeToArchetypeResolver : public GenericTypeResolver {
  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          Identifier name,
                                          SourceLoc nameLoc);
};

/// Generic type resolver that maps any generic type parameter type that
/// has an underlying archetype to its corresponding archetype.
///
/// This generic type resolver replaces generic type parameter types that
/// have archetypes with their archetypes, and leaves all other generic
/// type parameter types unchanged. It is used for the initial type-checks of
/// generic functions (and other generic declarations).
///
/// FIXME: This is not a long-term solution.
class PartialGenericTypeToArchetypeResolver : public GenericTypeResolver {
  TypeChecker &TC;

public:
  PartialGenericTypeToArchetypeResolver(TypeChecker &tc) : TC(tc) { }

  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          Identifier name,
                                          SourceLoc nameLoc);
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

public:
  CompleteGenericTypeResolver(TypeChecker &tc, ArchetypeBuilder &builder)
    : TC(tc), Builder(builder) { }

  virtual Type resolveGenericTypeParamType(GenericTypeParamType *gp);

  virtual Type resolveDependentMemberType(Type baseTy,
                                          DeclContext *DC,
                                          SourceRange baseRange,
                                          Identifier name,
                                          SourceLoc nameLoc);
};

} // end namespace swift

#endif
