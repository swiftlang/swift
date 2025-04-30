//===--- ActorIsolation.h - Actor isolation ---------------------*- C++ -*-===//
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
// This file provides a description of actor isolation state.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_ACTORISOLATIONSTATE_H
#define SWIFT_AST_ACTORISOLATIONSTATE_H

#include "swift/AST/Type.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
class raw_ostream;
}

namespace swift {
class DeclContext;
class ModuleDecl;
class VarDecl;
class NominalTypeDecl;
class SubstitutionMap;
class AbstractFunctionDecl;
class AbstractClosureExpr;

/// Determine whether the given types are (canonically) equal, declared here
/// to avoid having to include Types.h.
bool areTypesEqual(Type type1, Type type2);

/// Determines if the 'let' can be read from anywhere within the given module,
/// regardless of the isolation or async-ness of the context in which
/// the var is read.
bool isLetAccessibleAnywhere(const ModuleDecl *fromModule, VarDecl *let);

/// Describes the actor isolation of a given declaration, which determines
/// the actors with which it can interact.
class ActorIsolation {
public:
  enum Kind : uint8_t {
    /// The actor isolation has not been specified. It is assumed to be
    /// unsafe to interact with this declaration from any actor.
    Unspecified = 0,
    /// The declaration is isolated to the instance of an actor.
    /// For example, a mutable stored property or synchronous function within
    /// the actor is isolated to the instance of that actor.
    ActorInstance,
    /// The declaration is explicitly specified to be not isolated to any actor,
    /// meaning that it can be used from any actor but is also unable to
    /// refer to the isolated state of any given actor.
    Nonisolated,
    /// The declaration is explicitly specified to be not isolated and with the
    /// "unsafe" annotation, which means that we do not enforce isolation.
    NonisolatedUnsafe,
    /// The declaration is isolated to a global actor. It can refer to other
    /// entities with the same global actor.
    GlobalActor,
    /// The actor isolation iss statically erased, as for a call to
    /// an isolated(any) function.  This is not possible for declarations.
    Erased,
    /// Inherits isolation from the caller of the given function.
    ///
    /// DISCUSSION: This is used for nonisolated asynchronous functions that we
    /// want to inherit from their context the context's actor isolation.
    CallerIsolationInheriting,
  };

private:
  union {
    llvm::PointerUnion<NominalTypeDecl *, VarDecl *, Expr *> actorInstance;
    Type globalActor;
    void *pointer;
  };
  unsigned kind : 3;
  unsigned isolatedByPreconcurrency : 1;

  /// Set to true if this was parsed from SIL.
  unsigned silParsed : 1;

  unsigned parameterIndex : 27;

  ActorIsolation(Kind kind, NominalTypeDecl *actor, unsigned parameterIndex);

  ActorIsolation(Kind kind, VarDecl *actor, unsigned parameterIndex);

  ActorIsolation(Kind kind, Expr *actor, unsigned parameterIndex);

  ActorIsolation(Kind kind, Type globalActor);

public:
  // No-argument constructor needed for DenseMap use in PostfixCompletion.cpp
  explicit ActorIsolation(Kind kind = Unspecified, bool isSILParsed = false)
      : pointer(nullptr), kind(kind), isolatedByPreconcurrency(false),
        silParsed(isSILParsed), parameterIndex(0) {}

  static ActorIsolation forUnspecified() {
    return ActorIsolation(Unspecified);
  }

  static ActorIsolation forNonisolated(bool unsafe) {
    return ActorIsolation(unsafe ? NonisolatedUnsafe : Nonisolated);
  }

  static ActorIsolation forCallerIsolationInheriting() {
    // NOTE: We do not use parameter indices since the parameter is implicit
    // from the perspective of the AST.
    return ActorIsolation(CallerIsolationInheriting);
  }

  static ActorIsolation forActorInstanceSelf(ValueDecl *decl);

  /// Create an ActorIsolation appropriate for a type that is self.
  static ActorIsolation forActorInstanceSelf(NominalTypeDecl *decl);

  static ActorIsolation forActorInstanceParameter(NominalTypeDecl *actor,
                                                  unsigned parameterIndex) {
    return ActorIsolation(ActorInstance, actor, parameterIndex + 1);
  }

  static ActorIsolation forActorInstanceParameter(VarDecl *actor,
                                                  unsigned parameterIndex) {
    return ActorIsolation(ActorInstance, actor, parameterIndex + 1);
  }

  static ActorIsolation forActorInstanceParameter(Expr *actor,
                                                  unsigned parameterIndex);

  static ActorIsolation forActorInstanceCapture(VarDecl *capturedActor) {
    return ActorIsolation(ActorInstance, capturedActor, 0);
  }

  static ActorIsolation forGlobalActor(Type globalActor) {
    return ActorIsolation(GlobalActor, globalActor);
  }

  static ActorIsolation forMainActor(ASTContext &ctx);

  static ActorIsolation forErased() {
    return ActorIsolation(Erased);
  }

  static std::optional<ActorIsolation> forSILString(StringRef string) {
    auto kind =
        llvm::StringSwitch<std::optional<ActorIsolation::Kind>>(string)
            .Case("unspecified",
                  std::optional<ActorIsolation>(ActorIsolation::Unspecified))
            .Case("actor_instance",
                  std::optional<ActorIsolation>(ActorIsolation::ActorInstance))
            .Case("nonisolated",
                  std::optional<ActorIsolation>(ActorIsolation::Nonisolated))
            .Case("nonisolated_unsafe", std::optional<ActorIsolation>(
                                            ActorIsolation::NonisolatedUnsafe))
            .Case("global_actor",
                  std::optional<ActorIsolation>(ActorIsolation::GlobalActor))
            .Case("global_actor_unsafe",
                  std::optional<ActorIsolation>(ActorIsolation::GlobalActor))
            .Case("caller_isolation_inheriting",
                  std::optional<ActorIsolation>(
                      ActorIsolation::CallerIsolationInheriting))
            .Default(std::nullopt);
    if (kind == std::nullopt)
      return std::nullopt;
    return ActorIsolation(*kind, true /*is sil parsed*/);
  }

  Kind getKind() const { return (Kind)kind; }

  operator Kind() const { return getKind(); }

  bool isUnspecified() const { return kind == Unspecified; }

  bool isNonisolated() const {
    return (kind == Nonisolated) || (kind == NonisolatedUnsafe);
  }

  bool isNonisolatedUnsafe() const { return kind == NonisolatedUnsafe; }

  /// Retrieve the parameter to which actor-instance isolation applies.
  ///
  /// Parameter 0 is `self`.
  unsigned getActorInstanceParameter() const {
    assert(getKind() == ActorInstance);
    return parameterIndex;
  }

  /// Returns true if this is an actor-instance isolation that additionally
  /// applies to the self parameter of a method.
  bool isActorInstanceForSelfParameter() const {
    return getActorInstanceParameter() == 0;
  }

  bool isSILParsed() const { return silParsed; }

  bool isActorIsolated() const {
    switch (getKind()) {
    case ActorInstance:
    case GlobalActor:
    case Erased:
      return true;

    case Unspecified:
    case Nonisolated:
    case NonisolatedUnsafe:
    case CallerIsolationInheriting:
      return false;
    }
  }

  /// In the debugger return the index for the stored actorInstance pointer
  /// union index. Asserts if not an actor instance.
  SWIFT_DEBUG_HELPER(unsigned getActorInstanceUnionIndex() const);

  NominalTypeDecl *getActor() const;

  VarDecl *getActorInstance() const;

  Expr *getActorInstanceExpr() const;

  bool isGlobalActor() const {
    return getKind() == GlobalActor;
  }

  bool isActorInstanceIsolated() const { return getKind() == ActorInstance; }

  bool isMainActor() const;

  bool isDistributedActor() const;

  bool isCallerIsolationInheriting() const {
    return getKind() == CallerIsolationInheriting;
  }

  Type getGlobalActor() const {
    assert(isGlobalActor());

    if (silParsed)
      return Type();

    return globalActor;
  }

  bool preconcurrency() const {
    return isolatedByPreconcurrency;
  }

  ActorIsolation withPreconcurrency(bool value) const {
    auto copy = *this;
    copy.isolatedByPreconcurrency = value;
    return copy;
  }

  /// Determine whether this isolation will require substitution to be
  /// evaluated.
  bool requiresSubstitution() const;

  /// Substitute into types within the actor isolation.
  ActorIsolation subst(SubstitutionMap subs) const;

  static bool isEqual(const ActorIsolation &lhs,
               const ActorIsolation &rhs);

  friend bool operator==(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    return ActorIsolation::isEqual(lhs, rhs);
  }

  friend bool operator!=(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    return !(lhs == rhs);
  }

  void Profile(llvm::FoldingSetNodeID &id) {
    id.AddInteger(getKind());
    id.AddPointer(pointer);
    id.AddBoolean(isolatedByPreconcurrency);
    id.AddBoolean(silParsed);
    id.AddInteger(parameterIndex);
  }

  friend llvm::hash_code hash_value(const ActorIsolation &state) {
    return llvm::hash_combine(state.kind, state.pointer,
                              state.isolatedByPreconcurrency, state.silParsed,
                              state.parameterIndex);
  }

  void print(llvm::raw_ostream &os) const;

  void printForSIL(llvm::raw_ostream &os) const;
  
  /// Print the given isolation for diagnostics. If \c asNoun is \c false,
  /// the participle adjective form is printed, e.g. "main actor-isolated".
  /// Otherwise, the noun form is printed, e.g. "main actor isolation".
  void printForDiagnostics(llvm::raw_ostream &os,
                           StringRef openingQuotationMark = "'",
                           bool asNoun = false) const;

  SWIFT_DEBUG_DUMPER(dump());

  // Defined out of line to prevent linker errors since libswiftBasic would
  // include this header exascerbating a layering violation where libswiftBasic
  // depends on libswiftAST.
  SWIFT_DEBUG_DUMPER(dumpForDiagnostics());
};

struct IsolationSource {
  enum Kind : uint8_t {
    /// Isolation is written in an explicit attribute.
    Explicit,
    /// Isolation is inferred from the enclosing lexical context.
    LexicalContext,
    /// Isolation is inferred from conformance to a protocol.
    Conformance,
    /// Isolation is inherited from a superclass.
    Superclass,
    /// Isolation is inferred from an overridden superclass method.
    Override,
    /// Isolation is inferred from \c @main.
    MainFunction,
    /// Isolation is inferred in top-level code.
    TopLevelCode,
    /// Unspecified isolation, which defaults to \c nonisolated.
    None,
  };

  using InferenceSource =
      llvm::PointerUnion<Decl *, AbstractClosureExpr *>;

  /// The declaration with the original isolation attribute.
  InferenceSource inferenceSource;
  Kind kind;

  IsolationSource(InferenceSource inferenceSource = nullptr,
                  Kind kind = Kind::None)
      : inferenceSource(inferenceSource), kind(kind) {}

  bool isInferred() const {
    return (kind != None) && (kind != Explicit);
  }

  void printForDiagnostics(llvm::raw_ostream &os,
                           StringRef openingQuotationMark = "'") const;
};

struct InferredActorIsolation {
  ActorIsolation isolation;
  IsolationSource source;

  static InferredActorIsolation forUnspecified() {
    return {
      ActorIsolation::forUnspecified(),
      IsolationSource()
    };
  }

  bool preconcurrency() const {
    return isolation.preconcurrency();
  }
};

/// Determine how the given value declaration is isolated.
ActorIsolation getActorIsolation(ValueDecl *value);

/// Infer the actor isolation of the given declaration, including
/// the source of isolation inference.
InferredActorIsolation getInferredActorIsolation(ValueDecl *value);

/// Trampoline for AbstractClosureExpr::getActorIsolation.
ActorIsolation
__AbstractClosureExpr_getActorIsolation(AbstractClosureExpr *CE);

/// Determine how the given declaration context is isolated.
/// \p getClosureActorIsolation allows the specification of actor isolation for
/// closures that haven't been saved been saved to the AST yet. This is useful
/// for solver-based code completion which doesn't modify the AST but stores the
/// actor isolation of closures in the constraint system solution.
ActorIsolation getActorIsolationOfContext(
    DeclContext *dc,
    llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
        getClosureActorIsolation = __AbstractClosureExpr_getActorIsolation);

/// Check if both the value, and context are isolated to the same actor.
bool isSameActorIsolated(ValueDecl *value, DeclContext *dc);

/// Determines whether this function's body uses flow-sensitive isolation.
bool usesFlowSensitiveIsolation(AbstractFunctionDecl const *fn);

void simple_display(llvm::raw_ostream &out, const ActorIsolation &state);

// ApplyIsolationCrossing records the source and target of an isolation crossing
// within an ApplyExpr. In particular, it stores the isolation of the caller
// and the callee of the ApplyExpr, to be used for inserting implicit actor
// hops for implicitly async functions and to be used for diagnosing potential
// data races that could arise when non-Sendable values are passed to calls
// that cross isolation domains.
struct ApplyIsolationCrossing {
  ActorIsolation CallerIsolation;
  ActorIsolation CalleeIsolation;

  ApplyIsolationCrossing()
      : CallerIsolation(ActorIsolation::forUnspecified()),
        CalleeIsolation(ActorIsolation::forUnspecified()) {}

  ApplyIsolationCrossing(ActorIsolation CallerIsolation,
                         ActorIsolation CalleeIsolation)
      : CallerIsolation(CallerIsolation), CalleeIsolation(CalleeIsolation) {}

  // If the callee is not actor isolated, then this crossing exits isolation.
  // This method returns true iff this crossing exits isolation.
  bool exitsIsolation() const { return !CalleeIsolation.isActorIsolated(); }

  // Whether to use the isolation of the caller or callee for generating
  // informative diagnostics depends on whether this crossing is an exit.
  // In particular, we tend to use the callee isolation for diagnostics,
  // but if this crossing is an exit from isolation then the callee isolation
  // is not very informative, so we use the caller isolation instead.
  ActorIsolation getDiagnoseIsolation() const {
    return exitsIsolation() ? CallerIsolation : CalleeIsolation;
  }

  ActorIsolation getCallerIsolation() const { return CallerIsolation; }
  ActorIsolation getCalleeIsolation() const { return CalleeIsolation; }
};

} // end namespace swift

namespace llvm {

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const swift::ActorIsolation &other) {
  other.print(os);
  return os;
}

} // namespace llvm

#endif /* SWIFT_AST_ACTORISOLATIONSTATE_H */
