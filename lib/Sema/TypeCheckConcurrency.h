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
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Type.h"
#include "swift/Sema/Concurrency.h"

#include <cassert>

namespace swift {

class AbstractFunctionDecl;
class ConstructorDecl;
class ActorIsolation;
class AnyFunctionType;
class ASTContext;
class ClassDecl;
class ClosureExpr;
class ConcreteDeclRef;
class CustomAttr;
class Decl;
class DeclContext;
class EnumElementDecl;
class Expr;
class FuncDecl;
class Initializer;
class LookupResult;
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
void checkEnumElementActorIsolation(EnumElementDecl *element, Expr *expr);
void checkPropertyWrapperActorIsolation(VarDecl *wrappedVar, Expr *expr);

/// States the reason for checking the Sendability of a given declaration.
enum class SendableCheckReason {
  /// A reference to an actor from outside that actor.
  CrossActor,

  /// Exiting an actor to non-isolated async code.
  ExitingActor,

  /// A synchronous operation that was "promoted" to an asynchronous one
  /// because it was out of the actor's domain.
  SynchronousAsAsync,

  /// A protocol conformance where the witness/requirement have different
  /// actor isolation.
  Conformance,

  /// An override of a function.
  Override,

  /// The declaration is being exposed to Objective-C.
  ObjC,
};

/// Check that the actor isolation of an override matches that of its
/// overridden declaration.
void checkOverrideActorIsolation(ValueDecl *value);

/// Diagnose global state that is not either immutable plus Sendable or isolated
/// to a global actor.
void checkGlobalIsolation(VarDecl *var);

/// Determine whether the given context requires strict concurrency checking,
/// e.g., because it uses concurrency features directly or because it's in
/// code where strict checking has been enabled.
bool contextRequiresStrictConcurrencyChecking(
    const DeclContext *dc,
    llvm::function_ref<Type(const AbstractClosureExpr *)> getType,
    llvm::function_ref<bool(const ClosureExpr *)> isolatedByPreconcurrency);

/// Describes a referenced actor variable and whether it is isolated.
struct ReferencedActor {
  /// Describes whether the actor variable is isolated or, if it is not
  /// isolated, why it is not isolated.
  enum Kind {
    /// It is isolated.
    Isolated = 0,

    /// It is not an isolated parameter at all.
    NonIsolatedParameter,

    // It is within a Sendable function.
    SendableFunction,

    // It is within a Sendable closure.
    SendableClosure,

    // It is within an 'async let' initializer.
    AsyncLet,

    // It is within a global actor.
    GlobalActor,

    // It is within the main actor.
    MainActor,

    // It is within a nonisolated context.
    NonIsolatedContext,

    // It is within a nonisolated autoclosure argument. This is primarily here
    // to aid in giving specific diagnostics, because autoclosures are not
    // always easy for programmers to notice.
    NonIsolatedAutoclosure
  };

  VarDecl * const actor;
  /// The outer scope is known to be running on an actor.
  /// We may be isolated to the actor or not, depending on the exact expression
  const bool isPotentiallyIsolated;
  const Kind kind;
  const Type globalActor;

  ReferencedActor(VarDecl *actor, bool isPotentiallyIsolated, Kind kind, Type globalActor = Type())
    : actor(actor),
      isPotentiallyIsolated(isPotentiallyIsolated),
      kind(kind),
      globalActor(globalActor) {}

  static ReferencedActor forGlobalActor(VarDecl *actor,
                                        bool isPotentiallyIsolated,
                                        Type globalActor);

  bool isIsolated() const { return kind == Isolated; }

  /// Whether the variable is "self" or a capture therefore.
  bool isSelf() const {
    if (!actor)
      return false;

    return actor->isSelfParameter() || actor->isSelfParamCapture();
  }

  /// Whether the referenced actor is known to be local.
  bool isKnownToBeLocal() const;

  explicit operator bool() const { return isIsolated(); }
};

/// The values for each case in this enum correspond to %select numbers
/// in a diagnostic, so be sure to update it if you add new cases.
enum class VarRefUseEnv {
  Read = 0,
  Mutating = 1,
  Inout = 2 // means Mutating; having a separate kind helps diagnostics
};

/// Describes the result of referencing a declaration.
struct ActorReferenceResult {
  enum Kind: uint8_t {
    /// The declaration being referenced is within the same concurrency
    /// domain as where the reference occurs, so no checking is required.
    SameConcurrencyDomain,
    /// Using the declaration requires entering a new actor, which might also
    /// mean switching away from the current actor.
    EntersActor,
    /// Using the declaration means leaving the current actor, exiting to
    /// a nonisolated context.
    ExitsActorToNonisolated,
  };

  /// Flags describing any additional adjustments or requirements when
  /// accessing the declaration.
  enum class Flags: uint8_t {
    /// A synchronous declaration must be accessed asynchronously to enter its
    /// actor.
    AsyncPromotion = 1 << 0,

    /// A non-throwing declaration that is potentially being accessed from
    /// another node, and can therefore throw an error due to transport
    /// failure.
    ThrowsPromotion = 1 << 1,

    /// The declaration is being accessed from outside the actor and
    /// potentially from a different node, so it must be marked 'distributed'.
    Distributed = 1 << 2,

    /// The declaration is being accessed from a @preconcurrency context.
    Preconcurrency = 1 << 3,

    /// Only arguments cross an isolation boundary, e.g. because they
    /// escape into an actor in a nonisolated actor initializer.
    OnlyArgsCrossIsolation = 1 << 4,
  };

  using Options = OptionSet<Flags>;

  const Kind kind;
  const Options options;
  const ActorIsolation isolation;

private:
  static ActorReferenceResult
  forSameConcurrencyDomain(ActorIsolation isolation,
                           Options options = std::nullopt);

  static ActorReferenceResult forEntersActor(
      ActorIsolation isolation, Options options);

  static ActorReferenceResult
  forExitsActorToNonisolated(ActorIsolation isolation,
                             Options options = std::nullopt);

public:
  /// Determine what happens when referencing the given declaration from the
  /// given declaration context.
  ///
  ///
  /// \param declRef The declaration that is being referenced.
  ///
  /// \param fromDC The declaration context from which the reference occurs.
  ///
  /// \param actorInstance When not \c None, the actor instance value that is
  /// provided when referencing the declaration. This can be either the base
  /// of a member access or a parameter passed to a function.
  static ActorReferenceResult forReference(
      ConcreteDeclRef declRef, SourceLoc declRefLoc, const DeclContext *fromDC,
      std::optional<VarRefUseEnv> useKind = std::nullopt,
      std::optional<ReferencedActor> actorInstance = std::nullopt,
      std::optional<ActorIsolation> knownDeclIsolation = std::nullopt,
      std::optional<ActorIsolation> knownContextIsolation = std::nullopt,
      llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
          getClosureActorIsolation = __AbstractClosureExpr_getActorIsolation);

  operator Kind() const { return kind; }
};

struct IsolationError {

  SourceLoc loc;

  bool preconcurrency;
  
  Diagnostic diag;

public:
  IsolationError(SourceLoc loc, bool preconcurrency, Diagnostic diag) : loc(loc), preconcurrency(preconcurrency), diag(diag) {}

};

/// Individual options used with \c FunctionCheckOptions
enum class FunctionCheckKind {
  /// Check params
  Params = 1 << 0,
  /// Check results
  Results = 1 << 1,
};

/// Specifies whether checks applied to function types should apply to
/// their parameters, their results, both, or neither.
using FunctionCheckOptions = OptionSet<FunctionCheckKind>;

/// Diagnose the presence of any non-sendable types when referencing a
/// given declaration from a particular declaration context.
///
/// This function should be invoked any time that the given declaration
/// reference is will move values of the declaration's types across a
/// concurrency domain, whether in/out of an actor or in/or of a concurrent
/// function or closure.
///
/// \param base The base expression of the reference, which must be 'Sendable'
/// in order to cross actor isolation boundaries.
///
/// \param declRef The declaration being referenced from another concurrency
/// domain, including the substitutions so that (e.g.) we can consider the
/// specific types at the use site.
///
/// \param fromDC The context from which the reference occurs.
///
/// \param refLoc The location at which the reference occurs, which will be
/// used when emitting diagnostics.
///
/// \param refKind Describes what kind of reference is being made, which is
/// used to tailor the diagnostic.
///
/// \param funcCheckOptions Describes whether function types in this reference
/// should be checked for sendability of their results, params, or both
///
/// \param diagnoseLoc Provides an alternative source location to `refLoc`
/// to be used for reporting the top level diagnostic while auxiliary
/// warnings and diagnostics are reported at `refLoc`.
///
/// \returns true if an problem was detected, false otherwise.
bool diagnoseNonSendableTypesInReference(
    Expr *base, ConcreteDeclRef declRef, const DeclContext *fromDC,
    SourceLoc refLoc, SendableCheckReason refKind,
    std::optional<ActorIsolation> knownIsolation = std::nullopt,
    FunctionCheckOptions funcCheckOptions = (FunctionCheckOptions() |
                                             FunctionCheckKind::Params |
                                             FunctionCheckKind::Results),
    SourceLoc diagnoseLoc = SourceLoc());

/// Produce a diagnostic for a missing conformance to Sendable.
void diagnoseMissingSendableConformance(
    SourceLoc loc, Type type, const DeclContext *fromDC, bool preconcurrency);

/// If the given nominal type is public and does not explicitly
/// state whether it conforms to Sendable, provide a diagnostic.
void diagnoseMissingExplicitSendable(NominalTypeDecl *nominal);

/// Warn about deprecated `Executor.enqueue` implementations.
void tryDiagnoseExecutorConformance(ASTContext &C, const NominalTypeDecl *nominal, ProtocolDecl *proto);

/// Whether to suppress deprecation diagnostics for \p decl in \p declContext
/// because \p decl is a deprecated decl from the `_Concurrency` module and is
/// being referenced from the implementation of the `_Concurrency` module. This
/// prevents unaddressable warnings in the standard library build. Ideally, a
/// language feature would obviate the need for this.
bool shouldIgnoreDeprecationOfConcurrencyDecl(const Decl *decl,
                                              DeclContext *declContext);

// Get a concrete reference to a declaration
ConcreteDeclRef getDeclRefInContext(ValueDecl *value);

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

  /// Implicit conformance to Sendable that would be externally-visible, i.e.,
  /// for a public @_frozen type.
  ImplicitForExternallyVisible,
};

/// Whether this sendable check is implicit.
static inline bool isImplicitSendableCheck(SendableCheck check) {
  switch (check) {
  case SendableCheck::Explicit:
  case SendableCheck::ImpliedByStandardProtocol:
    return false;

  case SendableCheck::Implicit:
  case SendableCheck::ImplicitForExternallyVisible:
    return true;
  }
}

/// Describes the context in which a \c Sendable check occurs.
struct SendableCheckContext {
  const DeclContext * const fromDC;
  bool preconcurrencyContext;
  const std::optional<SendableCheck> conformanceCheck;

  SendableCheckContext(
      const DeclContext *fromDC,
      std::optional<SendableCheck> conformanceCheck = std::nullopt)
      : fromDC(fromDC),
        preconcurrencyContext(false),
        conformanceCheck(conformanceCheck) {}

  SendableCheckContext(
      const DeclContext *fromDC,
      bool preconcurrencyContext,
      std::optional<SendableCheck> conformanceCheck = std::nullopt)
      : fromDC(fromDC),
        preconcurrencyContext(preconcurrencyContext),
        conformanceCheck(conformanceCheck) {}

  /// Determine the default diagnostic behavior for a missing/unavailable
  /// Sendable conformance in this context.
  DiagnosticBehavior defaultDiagnosticBehavior() const;

  /// Determine the diagnostic behavior for an implicitly non-Sendable type.
  DiagnosticBehavior implicitSendableDiagnosticBehavior() const;

  /// Determine the diagnostic behavior when referencing the given nominal
  /// type in this context.
  DiagnosticBehavior diagnosticBehavior(NominalTypeDecl *nominal) const;

  std::optional<DiagnosticBehavior> preconcurrencyBehavior(
      Decl *decl,
      bool ignoreExplicitConformance = false) const;

  /// Whether to warn about a Sendable violation even in minimal checking.
  bool warnInMinimalChecking() const;
};

/// Diagnose any non-Sendable types that occur within the given type, using
/// the given diagnostic.
///
/// \param diagnose Emit a diagnostic indicating that the current type
/// is non-Sendable, with the suggested behavior limitation. Returns \c true
/// if it did not emit any diagnostics.
///
/// \returns \c true if any errors were produced, \c false if no diagnostics or
/// only warnings and notes were produced.
bool diagnoseNonSendableTypes(
    Type type, SendableCheckContext fromContext,
    Type inDerivedConformance, SourceLoc loc,
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
/// \param typeLoc is the source location of the type being diagnosed
///
/// \param diagnoseLoc is the source location at which the main diagnostic should
/// be reported, which can differ from typeLoc
///
/// \returns \c true if any errors were produced, \c false if no diagnostics or
/// only warnings and notes were produced.
template<typename ...DiagArgs>
bool diagnoseNonSendableTypes(
    Type type, SendableCheckContext fromContext,
    Type derivedConformance,
    SourceLoc typeLoc, SourceLoc diagnoseLoc,
    Diag<Type, DiagArgs...> diag,
    typename detail::Identity<DiagArgs>::type ...diagArgs) {

  ASTContext &ctx = fromContext.fromDC->getASTContext();
  return diagnoseNonSendableTypes(
      type, fromContext, derivedConformance, typeLoc,
      [&](Type specificType, DiagnosticBehavior behavior) {
        // FIXME: Reconcile preconcurrency declaration vs preconcurrency
        // import behavior.
        auto preconcurrency =
          fromContext.preconcurrencyBehavior(specificType->getAnyNominal());

        ctx.Diags.diagnose(diagnoseLoc, diag, type, diagArgs...)
            .limitBehaviorWithPreconcurrency(behavior,
                                             fromContext.preconcurrencyContext)
            .limitBehaviorIf(preconcurrency);

        return (behavior == DiagnosticBehavior::Ignore ||
                preconcurrency == DiagnosticBehavior::Ignore);
      });
}

/// Emit a diagnostic if there are any non-Sendable types for which
/// the Sendable diagnostic wasn't suppressed. This diagnostic will
/// only be emitted once, but there might be additional notes for the
/// various occurrences of Sendable types.
///
/// \param typeLoc is the source location of the type being diagnosed
///
/// \param diagnoseLoc is the source location at which the main diagnostic should
/// be reported, which can differ from typeLoc
///
/// \returns \c true if any diagnostics.
template<typename ...DiagArgs>
bool diagnoseIfAnyNonSendableTypes(
    Type type, SendableCheckContext fromContext,
    Type derivedConformance,
    SourceLoc typeLoc, SourceLoc diagnoseLoc,
    Diag<Type, DiagArgs...> diag,
    typename detail::Identity<DiagArgs>::type ...diagArgs) {

  ASTContext &ctx = fromContext.fromDC->getASTContext();
  bool diagnosed = false;
  diagnoseNonSendableTypes(
      type, fromContext, derivedConformance, typeLoc,
      [&](Type specificType, DiagnosticBehavior behavior) {
        auto preconcurrency =
          fromContext.preconcurrencyBehavior(specificType->getAnyNominal());

        if (behavior == DiagnosticBehavior::Ignore ||
            preconcurrency == DiagnosticBehavior::Ignore)
          return true;

        if (!diagnosed) {
          ctx.Diags.diagnose(diagnoseLoc, diag, type, diagArgs...)
              .limitBehaviorUntilSwiftVersion(behavior, 6)
              .limitBehaviorIf(preconcurrency);
          diagnosed = true;
        }

        return false;
      });

  return diagnosed;
}

/// Diagnose any non-Sendable types that occur within the given type, using
/// the given diagnostic.
///
/// \returns \c true if any errors were produced, \c false if no diagnostics or
/// only warnings and notes were produced.
template<typename ...DiagArgs>
bool diagnoseNonSendableTypes(
    Type type, SendableCheckContext fromContext,
    Type derivedConformance, SourceLoc loc,
    Diag<Type, DiagArgs...> diag,
    typename detail::Identity<DiagArgs>::type ...diagArgs) {

    return diagnoseNonSendableTypes(
        type, fromContext, derivedConformance, loc, loc, diag,
        std::forward<decltype(diagArgs)>(diagArgs)...);
}

/// Diagnose this sendability error with behavior based on the import of
/// \p nominal . For instance, depending on how \p nominal is imported into
/// \p fromContext , the diagnostic behavior limitation may be lower or the
/// compiler might emit a fix-it adding \c \@preconcurrency to the \c import .
///
/// \param nominal The declaration whose import we should check, or \c nullptr
/// to get behavior for a non-nominal type.
///
/// \param fromContext The context where the error will be emitted.
///
/// \param diagnose Emit a diagnostic indicating a sendability problem,
/// with the suggested behavior limitation. Returns \c true
/// if it did \em not emit any diagnostics.
///
/// \returns \c true if any errors were produced, \c false if no diagnostics or
/// only warnings and notes were produced.
bool diagnoseSendabilityErrorBasedOn(
    NominalTypeDecl *nominal, SendableCheckContext fromContext,
    llvm::function_ref<bool(DiagnosticBehavior)> diagnose);

/// Given a set of custom attributes, pick out the global actor attributes
/// and perform any necessary resolution and diagnostics, returning the
/// global actor attribute and type it refers to (or \c std::nullopt).
std::optional<std::pair<CustomAttr *, NominalTypeDecl *>>
checkGlobalActorAttributes(SourceLoc loc, DeclContext *dc,
                           ArrayRef<CustomAttr *> attrs);

/// Get the explicit global actor specified for a closure.
Type getExplicitGlobalActor(ClosureExpr *closure);

/// Adjust the type of the variable for concurrency.
Type adjustVarTypeForConcurrency(
    Type type, VarDecl *var, DeclContext *dc,
    llvm::function_ref<Type(const AbstractClosureExpr *)> getType,
    llvm::function_ref<bool(const ClosureExpr *)> isolatedByPreconcurrency);

/// Adjust the given function type to account for concurrency-specific
/// attributes whose affect on the type might differ based on context.
/// This includes adjustments for unsafe parameter attributes like
/// `@_unsafeSendable` and `@_unsafeMainActor` as well as a global actor
/// on the declaration itself.
AnyFunctionType *adjustFunctionTypeForConcurrency(
    AnyFunctionType *fnType, ValueDecl *decl, DeclContext *dc,
    unsigned numApplies, bool isMainDispatchQueue,
    llvm::function_ref<Type(const AbstractClosureExpr *)> getType,
    llvm::function_ref<bool(const ClosureExpr *)> isolatedByPreconcurrency,
    llvm::function_ref<Type(Type)> openType);

/// Classifies known dispatch queue operations.
enum class DispatchQueueOperation {
  /// This is a dispatch operation we know about.
  Normal,
  /// The closure passed to this dispatch queue operation should be Sendable.
  Sendable,
};

/// Determine whether the given name is that of a DispatchQueue operation that
/// takes a closure to be executed on the queue.
std::optional<DispatchQueueOperation>
isDispatchQueueOperationName(StringRef name);

/// Check the correctness of the given Sendable conformance.
///
/// \returns true if an error occurred.
bool checkSendableConformance(
    ProtocolConformance *conformance, SendableCheck check);

/// Derive an implicit conformance for the given nominal type to
/// the Sendable protocol. You want to be using
/// `ImplicitKnownProtocolConformanceRequest` instead of this.
ProtocolConformance *deriveImplicitSendableConformance(Evaluator &evaluator,
                                                       NominalTypeDecl *nominal);

/// Check whether we are in an actor's initializer or deinitializer.
/// \returns nullptr iff we are not in such a declaration. Otherwise,
///          returns a pointer to the declaration.
const AbstractFunctionDecl *isActorInitOrDeInitContext(const DeclContext *dc);

/// Determine whether this declaration is always accessed asynchronously.
bool isAsyncDecl(ConcreteDeclRef declRef);

/// Determine whether this declaration can throw errors.
bool isThrowsDecl(ConcreteDeclRef declRef);

/// Find the directly-referenced parameter or capture of a parameter for
/// for the given expression.
VarDecl *getReferencedParamOrCapture(
    Expr *expr,
    llvm::function_ref<Expr *(OpaqueValueExpr *)> getExistentialValue,
    llvm::function_ref<VarDecl *()> getCurrentIsolatedVar);

/// Determine whether the given value can be accessed across actors
/// without from normal synchronous code.
///
/// \param value The value we are checking.
/// \param isolation The actor isolation of the value.
/// \param fromDC The context where we are performing the access.
/// \param options The reference options, such as whether reference
/// violations should be downgraded to warnings prior to Swift 6.
bool isAccessibleAcrossActors(
    ValueDecl *value, const ActorIsolation &isolation,
    const DeclContext *fromDC, ActorReferenceResult::Options &options,
    std::optional<ReferencedActor> actorInstance = std::nullopt);

/// Determine whether the given value can be accessed across actors
/// without from normal synchronous code.
///
/// \param value The value we are checking.
/// \param isolation The actor isolation of the value.
/// \param fromDC The context where we are performing the access.
bool isAccessibleAcrossActors(
    ValueDecl *value, const ActorIsolation &isolation,
    const DeclContext *fromDC,
    std::optional<ReferencedActor> actorInstance = std::nullopt);

/// Determines if the 'let' can be read from anywhere within the given module,
/// regardless of the isolation or async-ness of the context in which
/// the var is read.
bool isLetAccessibleAnywhere(const ModuleDecl *fromModule,
                             VarDecl *let,
                             ActorReferenceResult::Options &options);

/// Check whether given variable references to a potentially
/// isolated actor.
bool isPotentiallyIsolatedActor(
    VarDecl *var, llvm::function_ref<bool(ParamDecl *)> isIsolated =
                      [](ParamDecl *P) { return P->isIsolated(); });

/// If the enclosing function has @_unsafeInheritExecutorAttr, return it.
AbstractFunctionDecl *enclosingUnsafeInheritsExecutor(const DeclContext *dc);

/// Add Fix-Its to the given function to replace the @_unsafeInheritExecutor
/// attribute with a defaulted isolation parameter.
void replaceUnsafeInheritExecutorWithDefaultedIsolationParam(
    AbstractFunctionDecl *func, InFlightDiagnostic &diag);

/// Replace any functions in this list that were found in the _Concurrency
/// module and have _unsafeInheritExecutor_-prefixed versions with those
/// _unsafeInheritExecutor_-prefixed versions.
///
/// This function is an egregious hack that allows us to introduce the
/// #isolation-based versions of functions into the concurrency library
/// without breaking clients that use @_unsafeInheritExecutor. Since those
/// clients can't use #isolation (it doesn't work with @_unsafeInheritExecutor),
/// we route them to the @_unsafeInheritExecutor versions implicitly.
void introduceUnsafeInheritExecutorReplacements(
    const DeclContext *dc, SourceLoc loc, SmallVectorImpl<ValueDecl *> &decls);

/// Replace any functions in this list that were found in the _Concurrency
/// module as a member on "base" and have _unsafeInheritExecutor_-prefixed
/// versions with those _unsafeInheritExecutor_-prefixed versions.
///
/// This function is an egregious hack that allows us to introduce the
/// #isolation-based versions of functions into the concurrency library
/// without breaking clients that use @_unsafeInheritExecutor. Since those
/// clients can't use #isolation (it doesn't work with @_unsafeInheritExecutor),
/// we route them to the @_unsafeInheritExecutor versions implicitly.
void introduceUnsafeInheritExecutorReplacements(
    const DeclContext *dc, Type base, SourceLoc loc, LookupResult &result);

} // end namespace swift

namespace llvm {

template <>
struct DenseMapInfo<swift::ReferencedActor::Kind> {
  using RefActorKind = swift::ReferencedActor::Kind;

  static RefActorKind getEmptyKey() {
   return RefActorKind::NonIsolatedContext;
  }

  static RefActorKind getTombstoneKey() {
   return RefActorKind::NonIsolatedContext;
  }

  static unsigned getHashValue(RefActorKind Val) {
   return static_cast<unsigned>(Val);
  }

  static bool isEqual(const RefActorKind &LHS, const RefActorKind &RHS) {
   return LHS == RHS;
  }
 };

  template <>
  struct DenseMapInfo<swift::ActorIsolation> {
    using RefActor = swift::ActorIsolation;

    static RefActor getEmptyKey() {
      return RefActor(swift::ActorIsolation::Kind::Unspecified);
    }

    static RefActor getTombstoneKey() {
     return RefActor(swift::ActorIsolation::Kind::Unspecified);
    }

    static unsigned getHashValue(RefActor Val) {
     return hash_value(Val);
    }

    static bool isEqual(const RefActor &LHS, const RefActor &RHS) {
     return LHS == RHS;
    }
  };
} // end namespace llvm

#endif /* SWIFT_SEMA_TYPECHECKCONCURRENCY_H */
