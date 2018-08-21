//===--- TypeCheckType.h - Type Resolution Code ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines utilities for resolving types.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPE_CHECK_TYPE_H
#define SWIFT_SEMA_TYPE_CHECK_TYPE_H

namespace swift {

/// Handles the resolution of types within a given declaration context,
/// which might involve resolving generic parameters to a particular
/// stage.
class TypeResolution {
  DeclContext *dc;
  TypeResolutionStage stage;

  union {
    /// The generic environment used to map to archetypes.
    GenericEnvironment *genericEnv;

    /// The generic signature
    struct {
      /// The generic signature to use for type resolution.
      GenericSignature *genericSig;

      /// The generic signature builder that will answer queries about
      /// generic types.
      mutable GenericSignatureBuilder *builder;
    } complete;
  };

  TypeResolution(DeclContext *dc, TypeResolutionStage stage)
    : dc(dc), stage(stage) { }

  GenericSignatureBuilder *getGenericSignatureBuilder() const;

public:
  /// Form a type resolution for the structure of a type, which does not
  /// attempt to resolve member types of type parameters to a particular
  /// associated type.
  static TypeResolution forStructural(DeclContext *dc);

  /// Form a type resolution for an interface type, which is a complete
  /// description of the type using generic parameters.
  static TypeResolution forInterface(DeclContext *dc,
                                     GenericSignature *genericSig);

  /// Form a type resolution for a contextual type, which is a complete
  /// description of the type using the archetypes of the given declaration
  /// context.
  static TypeResolution forContextual(DeclContext *dc);

  /// Form a type resolution for a contextual type, which is a complete
  /// description of the type using the archetypes of the given generic
  /// environment.
  static TypeResolution forContextual(DeclContext *dc,
                                      GenericEnvironment *genericEnv);

  /// Retrieve the ASTContext in which this resolution occurs.
  ASTContext &getASTContext() const { return dc->getASTContext(); }

  /// Retrieve the declaration context in which type resolution will be
  /// performed.
  DeclContext *getDeclContext() const { return dc; }

  /// Retrieves the generic signature for the context, or NULL if there is
  /// no generic signature to resolve types.
  GenericSignature *getGenericSignature() const;

  /// Whether this type resolution uses archetypes (vs. generic parameters).
  bool usesArchetypes() const;

  /// Map the given type (that involves generic parameters)
  Type mapTypeIntoContext(Type type) const;

  Type resolveDependentMemberType(Type baseTy, DeclContext *DC,
                                  SourceRange baseRange,
                                  ComponentIdentTypeRepr *ref) const;

  bool areSameType(Type type1, Type type2) const;
};

} // end namespace swift

#endif /* SWIFT_SEMA_TYPE_CHECK_TYPE_H */
