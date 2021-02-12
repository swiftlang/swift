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

#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Type.h"
#include <cassert>

namespace swift {

class AbstractFunctionDecl;
class ActorIsolation;
class AnyFunctionType;
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
class ProtocolConformance;
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

/// Describes the kind of operation that introduced the concurrent refernece.
enum class ConcurrentReferenceKind {
  /// A synchronous operation that was "promoted" to an asynchronous call
  /// because it was out of the actor's domain.
  SynchronousAsAsyncCall,
  /// A cross-actor reference.
  CrossActor,
  /// A local capture referenced from concurrent code.
  LocalCapture,
  /// Concurrent function
  ConcurrentFunction,
};

/// Describes why or where a particular entity has a non-concurrent-value type.
struct NonConcurrentType {
  enum Kind {
    /// A function parameter is a non-concurrent-value type.
    Parameter,
    /// The result of a function is a non-concurrent-value type.
    Result,
    /// The type of a property is a non-concurrent-value type.
    Property,
  } kind;

  /// The declaration reference.
  ConcreteDeclRef declRef;

  /// The non-concurrent-value type being diagnosed.
  Type type;

  /// Determine whether a reference to the given declaration involves a
  /// non-concurrent-value type. If it does, return the reason. Otherwise,
  /// return \c None.
  ///
  /// \param dc The declaration context from which the reference occurs.
  /// \param declRef The reference to the declaration.
  static Optional<NonConcurrentType> get(
      const DeclContext *dc, ConcreteDeclRef declRef);

  /// Diagnose the non-concurrent-value type at the given source location.
  void diagnose(SourceLoc loc);
};

/// The isolation restriction in effect for a given declaration that is
/// referenced from source.
class ActorIsolationRestriction {
public:
  enum Kind {
    /// There is no restriction on references to the given declaration.
    Unrestricted,

    /// Access to the declaration is unsafe in a concurrent context.
    Unsafe,

    /// The declaration is a local entity whose capture could introduce
    /// data races. The context in which the local was defined is provided.
    LocalCapture,

    /// References to this entity are allowed from anywhere, but doing so
    /// may cross an actor boundary if it is not on \c self.
    CrossActorSelf,

    /// References to this member of an actor are only permitted on 'self'.
    ActorSelf,

    /// References to a declaration that is part of a global actor are only
    /// permitted from other declarations with that same global actor.
    GlobalActor,

    /// Referneces to this entity are allowed from anywhere, but doing so may
    /// cross an actor bounder if it is not from the same global actor.
    CrossGlobalActor,
  };

private:
  /// The kind of restriction.
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
    assert(kind == ActorSelf || kind == CrossActorSelf);
    return data.actorClass;
  }

  /// Retrieve the actor class that the declaration is within.
  Type getGlobalActor() const {
    assert(kind == GlobalActor || kind == CrossGlobalActor);
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
  /// the current actor or is a cross-actor access.
  static ActorIsolationRestriction forActorSelf(
      ClassDecl *actorClass, bool isCrossActor) {
    ActorIsolationRestriction result(isCrossActor? CrossActorSelf : ActorSelf);
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
  /// global actor or is a cross-actor access.
  static ActorIsolationRestriction forGlobalActor(
      Type globalActor, bool isCrossActor) {
    ActorIsolationRestriction result(
        isCrossActor ? CrossGlobalActor : GlobalActor);
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

/// Diagnose the presence of any non-concurrent types when referencing a
/// given declaration from a particular declaration context.
///
/// This function should be invoked any time that the given declaration
/// reference is will move values of the declaration's types across a
/// concurrency domain, whether in/out of an actor or in/or of a concurrent
/// function or closure.
///
/// \param declRef The declaration being referenced from another concurrency
/// domain, including the substitutions so that (e.g.) we can consider the
/// specific types at the use site.
///
/// \param dc The declaration context from which the reference occurs. This is
/// used to perform lookup of conformances to the \c ConcurrentValue protocol.
///
/// \param loc The location at which the reference occurs, which will be
/// used when emitting diagnostics.
///
/// \param refKind Describes what kind of reference is being made, which is
/// used to tailor the diagnostic.
///
/// \returns true if an problem was detected, false otherwise.
bool diagnoseNonConcurrentTypesInReference(
    ConcreteDeclRef declRef, const DeclContext *dc, SourceLoc loc,
    ConcurrentReferenceKind refKind);

/// Check the correctness of the given ConcurrentValue conformance.
void checkConcurrentValueConformance(ProtocolConformance *conformance);

} // end namespace swift

#endif /* SWIFT_SEMA_TYPECHECKCONCURRENCY_H */
