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

#include "swift/AST/ASTContext.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/Type.h"
#include <cassert>

namespace swift {

class AbstractFunctionDecl;
class ConstructorDecl;
class ActorIsolation;
class AnyFunctionType;
class ASTContext;
class ClassDecl;
class ClosureActorIsolation;
class ClosureExpr;
class ConcreteDeclRef;
class CustomAttr;
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
class VarDecl;

/// Add notes suggesting the addition of 'async', as appropriate,
/// to a diagnostic for a function that isn't an async context.
void addAsyncNotes(AbstractFunctionDecl const* func);

/// Check actor isolation rules.
void checkTopLevelActorIsolation(TopLevelCodeDecl *decl);
void checkFunctionActorIsolation(AbstractFunctionDecl *decl);
void checkInitializerActorIsolation(Initializer *init, Expr *expr);
void checkEnumElementActorIsolation(EnumElementDecl *element, Expr *expr);
void checkPropertyWrapperActorIsolation(VarDecl *wrappedVar, Expr *expr);

/// Determine the isolation of a particular closure.
///
/// This forwards to \c ActorIsolationChecker::determineClosureActorIsolation
/// and thus assumes that enclosing closures have already had their isolation
/// checked.
///
/// This does not set the closure's actor isolation
ClosureActorIsolation
determineClosureActorIsolation(AbstractClosureExpr *closure);

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
  /// Nonisolated declaration.
  Nonisolated,
};

/// The isolation restriction in effect for a given declaration that is
/// referenced from source.
class ActorIsolationRestriction {
public:
  enum Kind {
    /// There is no restriction on references to the given declaration.
    Unrestricted,

    /// Access to the declaration is unsafe in any concurrent context.
    Unsafe,

    /// References to this entity are allowed from anywhere, but doing so
    /// may cross an actor boundary if it is not from within the same actor's
    /// isolation domain.
    CrossActorSelf,

    /// References to this member of an actor are only permitted from within
    /// the actor's isolation domain.
    ActorSelf,

    /// References to a declaration that is part of a global actor are
    /// permitted from other declarations with that same global actor or
    /// are permitted from elsewhere as a cross-actor reference.
    GlobalActor,

    /// References to a declaration that is part of a global actor are
    /// permitted from other declarations with that same global actor or
    /// are permitted from elsewhere as a cross-actor reference, but
    /// contexts with unspecified isolation won't diagnose anything.
    GlobalActorUnsafe,
  };

private:
  union {
    /// The local context that an entity is tied to.
    DeclContext *localContext;

    /// The actor that the entity is declared in.
    NominalTypeDecl *actorType;

    /// The global actor type.
    TypeBase *globalActor;
  } data;

  explicit ActorIsolationRestriction(Kind kind, bool isCrossActor)
      : kind(kind), isCrossActor(isCrossActor) { }

public:
  /// The kind of restriction.
  const Kind kind;

  /// Whether referencing this from another actor constitutes a cross-acter
  /// reference.
  const bool isCrossActor;

  Kind getKind() const { return kind; }

  /// Retrieve the actor type that the declaration is within.
  NominalTypeDecl *getActorType() const {
    assert(kind == ActorSelf || 
           kind == CrossActorSelf);
    return data.actorType;
  }

  /// Retrieve the actor that the declaration is within.
  Type getGlobalActor() const {
    assert(kind == GlobalActor || kind == GlobalActorUnsafe);
    return Type(data.globalActor);
  }

  /// There are no restrictions on the use of the entity.
  static ActorIsolationRestriction forUnrestricted() {
    return ActorIsolationRestriction(Unrestricted, /*isCrossActor=*/false);
  }

  /// Accesses to the given declaration are unsafe.
  static ActorIsolationRestriction forUnsafe() {
    return ActorIsolationRestriction(Unsafe, /*isCrossActor=*/false);
  }

  /// Accesses to the given declaration can only be made via the 'self' of
  /// the current actor or is a cross-actor access.
  static ActorIsolationRestriction forActorSelf(
      NominalTypeDecl *actor, bool isCrossActor) {

    ActorIsolationRestriction result(isCrossActor? CrossActorSelf : ActorSelf,
                                     isCrossActor);
    result.data.actorType = actor;
    return result;
  }

  /// Accesses to the given declaration can only be made via the 'self' of
  /// the current actor.
  static ActorIsolationRestriction forDistributedActorSelf(
      NominalTypeDecl *actor, bool isCrossActor) {
    ActorIsolationRestriction result(isCrossActor ? CrossActorSelf : ActorSelf,
                                     isCrossActor);
    result.data.actorType = actor;
    return result;
  }

  /// Accesses to the given declaration can only be made via this particular
  /// global actor or is a cross-actor access.
  static ActorIsolationRestriction forGlobalActor(
      Type globalActor, bool isCrossActor, bool isUnsafe) {
    ActorIsolationRestriction result(
        isUnsafe ? GlobalActorUnsafe : GlobalActor, isCrossActor);
    result.data.globalActor = globalActor.getPointer();
    return result;
  }

  /// Determine the isolation rules for a given declaration.
  ///
  /// \param fromExpression Indicates that the reference is coming from an
  /// expression.
  static ActorIsolationRestriction forDeclaration(
      ConcreteDeclRef declRef, const DeclContext *fromDC,
      bool fromExpression = true);

  operator Kind() const { return kind; };
};

/// Check that the actor isolation of an override matches that of its
/// overridden declaration.
void checkOverrideActorIsolation(ValueDecl *value);

/// Determine whether the given context requires strict concurrency checking,
/// e.g., because it uses concurrency features directly or because it's in
/// code where strict checking has been enabled.
bool contextRequiresStrictConcurrencyChecking(const DeclContext *dc);

/// Diagnose the presence of any non-sendable types when referencing a
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
/// \param fromDC The context from which the reference occurs.
///
/// \param loc The location at which the reference occurs, which will be
/// used when emitting diagnostics.
///
/// \param refKind Describes what kind of reference is being made, which is
/// used to tailor the diagnostic.
///
/// \returns true if an problem was detected, false otherwise.
bool diagnoseNonSendableTypesInReference(
    ConcreteDeclRef declRef, const DeclContext *fromDC, SourceLoc loc,
    ConcurrentReferenceKind refKind);

/// Produce a diagnostic for a missing conformance to Sendable.
void diagnoseMissingSendableConformance(
    SourceLoc loc, Type type, const DeclContext *fromDC);

/// If the given nominal type is public and does not explicitly
/// state whether it conforms to Sendable, provide a diagnostic.
void diagnoseMissingExplicitSendable(NominalTypeDecl *nominal);

/// How the Sendable check should be performed.
enum class SendableCheck {
  /// Sendable conformance was explicitly stated and should be
  /// fully checked.
  Explicit,

  /// Sendable conformance was implied by a protocol that inherits from
  /// Sendable and also predates concurrency.
  ImpliedByStandardProtocol,

  /// Implicit conformance to Sendable.
  Implicit,
};

/// Describes the context in which a \c Sendable check occurs.
struct SendableCheckContext {
  const DeclContext * const fromDC;
  const Optional<SendableCheck> conformanceCheck;

  SendableCheckContext(
      const DeclContext * fromDC,
      Optional<SendableCheck> conformanceCheck = None
  ) : fromDC(fromDC), conformanceCheck(conformanceCheck) { }

  /// Determine the default diagnostic behavior for a missing/unavailable
  /// Sendable conformance in this context.
  DiagnosticBehavior defaultDiagnosticBehavior() const;

  /// Determine the diagnostic behavior when referencing the given nominal
  /// type in this context.
  DiagnosticBehavior diagnosticBehavior(NominalTypeDecl *nominal) const;

  /// Whether we are in an explicit conformance to Sendable.
  bool isExplicitSendableConformance() const;
};

/// Diagnose any non-Sendable types that occur within the given type, using
/// the given diagnostic.
///
/// \param diagnose Emit a diagnostic indicating that the current type
/// is non-Sendable, with the suggested behavior limitation. Returns \c true
/// if an error was produced.
///
/// \returns \c true if any diagnostics were produced, \c false otherwise.
bool diagnoseNonSendableTypes(
    Type type, SendableCheckContext fromContext, SourceLoc loc,
    llvm::function_ref<bool(Type, DiagnosticBehavior)> diagnose);

namespace detail {
  template<typename T>
  struct Identity {
    typedef T type;
  };
}

/// Diagnose any non-Sendable types that occur within the given type, using
/// the given diagnostic.
///
/// \returns \c true if any errors were produced, \c false otherwise.
template<typename ...DiagArgs>
bool diagnoseNonSendableTypes(
    Type type, SendableCheckContext fromContext, SourceLoc loc,
    Diag<Type, DiagArgs...> diag,
    typename detail::Identity<DiagArgs>::type ...diagArgs) {
  ASTContext &ctx = fromContext.fromDC->getASTContext();
  return diagnoseNonSendableTypes(
      type, fromContext, loc, [&](Type specificType,
      DiagnosticBehavior behavior) {
    if (behavior != DiagnosticBehavior::Ignore) {
      ctx.Diags.diagnose(loc, diag, type, diagArgs...)
        .limitBehavior(behavior);
    }

    return false;
  });
}

/// Given a set of custom attributes, pick out the global actor attributes
/// and perform any necessary resolution and diagnostics, returning the
/// global actor attribute and type it refers to (or \c None).
Optional<std::pair<CustomAttr *, NominalTypeDecl *>>
checkGlobalActorAttributes(
    SourceLoc loc, DeclContext *dc, ArrayRef<CustomAttr *> attrs);

/// Get the explicit global actor specified for a closure.
Type getExplicitGlobalActor(ClosureExpr *closure);

/// Adjust the type of the variable for concurrency.
Type adjustVarTypeForConcurrency(Type type, VarDecl *var, DeclContext *dc);

/// Adjust the function type of a function / subscript / enum case for
/// concurrency.
AnyFunctionType *adjustFunctionTypeForConcurrency(
    AnyFunctionType *fnType, ValueDecl *decl, DeclContext *dc,
    unsigned numApplies, bool isMainDispatchQueue);

/// Adjust the given function type to account for concurrency-specific
/// attributes whose affect on the type might differ based on context.
/// This includes adjustments for unsafe parameter attributes like
/// `@_unsafeSendable` and `@_unsafeMainActor` as well as a global actor
/// on the declaration itself.
AnyFunctionType *adjustFunctionTypeForConcurrency(
    AnyFunctionType *fnType, ValueDecl *funcOrEnum, DeclContext *dc,
    unsigned numApplies, bool isMainDispatchQueue);

/// Determine whether the given name is that of a DispatchQueue operation that
/// takes a closure to be executed on the queue.
bool isDispatchQueueOperationName(StringRef name);

/// Check the correctness of the given Sendable conformance.
///
/// \returns true if an error occurred.
bool checkSendableConformance(
    ProtocolConformance *conformance, SendableCheck check);

/// Check whether we are in an actor's initializer or deinitializer.
/// \returns nullptr iff we are not in such a declaration. Otherwise,
///          returns a pointer to the declaration.
AbstractFunctionDecl const *isActorInitOrDeInitContext(
    const DeclContext *dc,
    llvm::function_ref<bool(const AbstractClosureExpr *)> isSendable);

/// Find the directly-referenced parameter or capture of a parameter for
/// for the given expression.
VarDecl *getReferencedParamOrCapture(
    Expr *expr,
    llvm::function_ref<Expr *(OpaqueValueExpr *)> getExistentialValue);

/// Check whether given variable references to a potentially
/// isolated actor.
bool isPotentiallyIsolatedActor(
    VarDecl *var, llvm::function_ref<bool(ParamDecl *)> isIsolated =
                      [](ParamDecl *P) { return P->isIsolated(); });

} // end namespace swift

#endif /* SWIFT_SEMA_TYPECHECKCONCURRENCY_H */
