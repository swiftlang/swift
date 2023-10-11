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

/// Determine whether the given type is suitable as a concurrent value type.
bool isSendableType(ModuleDecl *module, Type type);

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
    /// The declaration is isolated to a global actor but with the "unsafe"
    /// annotation, which means that we only enforce the isolation if we're
    /// coming from something with specific isolation.
    GlobalActorUnsafe,
  };

private:
  union {
    llvm::PointerUnion<NominalTypeDecl *, VarDecl *> actorInstance;
    Type globalActor;
    void *pointer;
  };
  unsigned kind : 3;
  unsigned isolatedByPreconcurrency : 1;
  unsigned parameterIndex : 28;

  ActorIsolation(Kind kind, NominalTypeDecl *actor, unsigned parameterIndex);

  ActorIsolation(Kind kind, VarDecl *capturedActor);

  ActorIsolation(Kind kind, Type globalActor)
      : globalActor(globalActor), kind(kind), isolatedByPreconcurrency(false),
        parameterIndex(0) { }

public:
  // No-argument constructor needed for DenseMap use in PostfixCompletion.cpp
  explicit ActorIsolation(Kind kind = Unspecified)
      : pointer(nullptr), kind(kind), isolatedByPreconcurrency(false),
        parameterIndex(0) { }

  static ActorIsolation forUnspecified() {
    return ActorIsolation(Unspecified, nullptr);
  }

  static ActorIsolation forNonisolated(bool unsafe) {
    return ActorIsolation(unsafe ? NonisolatedUnsafe : Nonisolated, nullptr);
  }

  static ActorIsolation forActorInstanceSelf(NominalTypeDecl *actor) {
    return ActorIsolation(ActorInstance, actor, 0);
  }

  static ActorIsolation forActorInstanceParameter(NominalTypeDecl *actor,
                                                  unsigned parameterIndex) {
    return ActorIsolation(ActorInstance, actor, parameterIndex + 1);
  }

  static ActorIsolation forActorInstanceCapture(VarDecl *capturedActor) {
    return ActorIsolation(ActorInstance, capturedActor);
  }

  static ActorIsolation forGlobalActor(Type globalActor, bool unsafe) {
    return ActorIsolation(
        unsafe ? GlobalActorUnsafe : GlobalActor, globalActor);
  }

  Kind getKind() const { return (Kind)kind; }

  operator Kind() const { return getKind(); }

  bool isUnspecified() const { return kind == Unspecified; }

  bool isNonisolated() const {
    return (kind == Nonisolated) || (kind == NonisolatedUnsafe);
  }

  /// Retrieve the parameter to which actor-instance isolation applies.
  ///
  /// Parameter 0 is `self`.
  unsigned getActorInstanceParameter() const {
    assert(getKind() == ActorInstance);
    return parameterIndex;
  }

  bool isActorIsolated() const {
    switch (getKind()) {
    case ActorInstance:
    case GlobalActor:
    case GlobalActorUnsafe:
      return true;

    case Unspecified:
    case Nonisolated:
    case NonisolatedUnsafe:
      return false;
    }
  }

  NominalTypeDecl *getActor() const;

  VarDecl *getActorInstance() const;

  bool isGlobalActor() const {
    return getKind() == GlobalActor || getKind() == GlobalActorUnsafe;
  }

  bool isMainActor() const;

  bool isDistributedActor() const;

  Type getGlobalActor() const {
    assert(isGlobalActor());
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

  friend bool operator==(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    if (lhs.isGlobalActor() && rhs.isGlobalActor())
      return areTypesEqual(lhs.globalActor, rhs.globalActor);

    if (lhs.getKind() != rhs.getKind())
      return false;

    switch (lhs.getKind()) {
    case Nonisolated:
    case NonisolatedUnsafe:
    case Unspecified:
      return true;

    case ActorInstance:
      return (lhs.getActor() == rhs.getActor() &&
              lhs.parameterIndex == rhs.parameterIndex);

    case GlobalActor:
    case GlobalActorUnsafe:
      llvm_unreachable("Global actors handled above");
    }
  }

  friend bool operator!=(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    return !(lhs == rhs);
  }

  friend llvm::hash_code hash_value(const ActorIsolation &state) {
    return llvm::hash_combine(
        state.kind, state.pointer, state.isolatedByPreconcurrency,
        state.parameterIndex);
  }
};

/// Determine how the given value declaration is isolated.
ActorIsolation getActorIsolation(ValueDecl *value);

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

} // end namespace swift

#endif /* SWIFT_AST_ACTORISOLATIONSTATE_H */
