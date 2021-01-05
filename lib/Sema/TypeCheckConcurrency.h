//===--- TypeCheckConcurrency.h - Concurrency -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides type checking support for Swift's concurrency model.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKCONCURRENCY_H
#define SWIFT_SEMA_TYPECHECKCONCURRENCY_H

#include "swift/AST/Type.h"
#include <cassert>

namespace swift {

class AbstractFunctionDecl;
class ActorIsolation;
class ASTContext;
class ClassDecl;
class ConcreteDeclRef;
class Decl;
class DeclContext;
class EnumElementDecl;
class Expr;
class FuncDecl;
class Initializer;
class PatternBindingDecl;
class TopLevelCodeDecl;
class TypeBase;
class ValueDecl;

/// Add notes suggesting the addition of 'async' or '@asyncHandler', as
/// appropriate, to a diagnostic for a function that isn't an async context.
void addAsyncNotes(AbstractFunctionDecl const* func);

/// Check actor isolation rules.
void checkTopLevelActorIsolation(TopLevelCodeDecl *decl);
void checkFunctionActorIsolation(AbstractFunctionDecl *decl);
void checkInitializerActorIsolation(Initializer *init, Expr *expr);
void checkEnumElementActorIsolation(EnumElementDecl *element, Expr *expr);
void checkPropertyWrapperActorIsolation(
    PatternBindingDecl *binding, Expr *expr);

/// The isolation restriction in effect for a given declaration that is
/// referenced from source.
class ActorIsolationRestriction {
public:
  enum Kind {
    /// There is no restriction on references to the given declaration,
    /// e.g., because it's immutable.
    Unrestricted,

    /// Access to the declaration is unsafe in a concurrent context.
    Unsafe,

    /// The declaration is a local entity whose capture could introduce
    /// data races. The context in which the local was defined is provided.
    LocalCapture,

    /// References to this member of an actor are only permitted on 'self'.
    ActorSelf,

    /// References to a declaration that is part of a global actor are only
    /// permitted from other declarations with that same global actor.
    GlobalActor,
  };

private:
  /// The kind of restriction
  Kind kind;

  union {
    /// The local context that an entity is tied to.
    DeclContext *localContext;

    /// The actor class that the entity is declared in.
    ClassDecl *actorClass;

    /// The global actor type.
    TypeBase *globalActor;
  } data;

  explicit ActorIsolationRestriction(Kind kind) : kind(kind) { }

public:
  Kind getKind() const { return kind; }

  /// Retrieve the declaration context in which a local was defined.
  DeclContext *getLocalContext() const {
    assert(kind == LocalCapture);
    return data.localContext;
  }

  /// Retrieve the actor class that the declaration is within.
  ClassDecl *getActorClass() const {
    assert(kind == ActorSelf);
    return data.actorClass;
  }

  /// Retrieve the actor class that the declaration is within.
  Type getGlobalActor() const {
    assert(kind == GlobalActor);
    return Type(data.globalActor);
  }

  /// There are no restrictions on the use of the entity.
  static ActorIsolationRestriction forUnrestricted() {
    return ActorIsolationRestriction(Unrestricted);
  }

  /// Accesses to the given declaration are unsafe.
  static ActorIsolationRestriction forUnsafe() {
    return ActorIsolationRestriction(Unsafe);
  }

  /// Accesses to the given declaration can only be made via the 'self' of
  /// the current actor.
  static ActorIsolationRestriction forActorSelf(ClassDecl *actorClass) {
    ActorIsolationRestriction result(ActorSelf);
    result.data.actorClass = actorClass;
    return result;
  }

  /// Access is restricted to code running within the given local context.
  static ActorIsolationRestriction forLocalCapture(DeclContext *dc) {
    ActorIsolationRestriction result(LocalCapture);
    result.data.localContext = dc;
    return result;
  }

  /// Accesses to the given declaration can only be made via this particular
  /// global actor.
  static ActorIsolationRestriction forGlobalActor(Type globalActor) {
    ActorIsolationRestriction result(GlobalActor);
    result.data.globalActor = globalActor.getPointer();
    return result;
  }

  /// Determine the isolation rules for a given declaration.
  static ActorIsolationRestriction forDeclaration(ConcreteDeclRef declRef);

  operator Kind() const { return kind; };
};

/// Check that the actor isolation of an override matches that of its
/// overridden declaration.
void checkOverrideActorIsolation(ValueDecl *value);

} // end namespace swift

#endif /* SWIFT_SEMA_TYPECHECKCONCURRENCY_H */
