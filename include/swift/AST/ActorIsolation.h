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
class NominalTypeDecl;
class SubstitutionMap;

/// Determine whether the given types are (canonically) equal, declared here
/// to avoid having to include Types.h.
bool areTypesEqual(Type type1, Type type2);

/// Determine whether the given type is suitable as a concurrent value type.
bool isSendableType(const DeclContext *dc, Type type);

/// Describes the actor isolation of a given declaration, which determines
/// the actors with which it can interact.
class ActorIsolation {
public:
  enum Kind {
    /// The actor isolation has not been specified. It is assumed to be
    /// unsafe to interact with this declaration from any actor.
    Unspecified = 0,
    /// The declaration is isolated to the instance of an actor.
    /// For example, a mutable stored property or synchronous function within
    /// the actor is isolated to the instance of that actor.
    ActorInstance,
    /// The declaration is explicitly specified to be independent of any actor,
    /// meaning that it can be used from any actor but is also unable to
    /// refer to the isolated state of any given actor.
    Independent,
    /// The declaration is isolated to a global actor. It can refer to other
    /// entities with the same global actor.
    GlobalActor,
    /// The declaration is isolated to a global actor but with the "unsafe"
    /// annotation, which means that we only enforce the isolation if we're
    /// coming from something with specific isolation.
    GlobalActorUnsafe,
  };

private:
  Kind kind;
  union {
    NominalTypeDecl *actor;
    Type globalActor;
    void *pointer;
  };

  ActorIsolation(Kind kind, NominalTypeDecl *actor)
      : kind(kind), actor(actor) { }

  ActorIsolation(Kind kind, Type globalActor)
      : kind(kind), globalActor(globalActor) { }

public:
  static ActorIsolation forUnspecified() {
    return ActorIsolation(Unspecified, nullptr);
  }

  static ActorIsolation forIndependent() {
    return ActorIsolation(Independent, nullptr);
  }

  static ActorIsolation forActorInstance(NominalTypeDecl *actor) {
    return ActorIsolation(ActorInstance, actor);
  }

  static ActorIsolation forGlobalActor(Type globalActor, bool unsafe) {
    return ActorIsolation(
        unsafe ? GlobalActorUnsafe : GlobalActor, globalActor);
  }

  Kind getKind() const { return kind; }

  operator Kind() const { return getKind(); }

  bool isUnspecified() const { return kind == Unspecified; }

  NominalTypeDecl *getActor() const {
    assert(getKind() == ActorInstance);
    return actor;
  }

  bool isGlobalActor() const {
    return getKind() == GlobalActor || getKind() == GlobalActorUnsafe;
  }

  Type getGlobalActor() const {
    assert(isGlobalActor());
    return globalActor;
  }

  /// Determine whether this isolation will require substitution to be
  /// evaluated.
  bool requiresSubstitution() const;

  /// Substitute into types within the actor isolation.
  ActorIsolation subst(SubstitutionMap subs) const;

  friend bool operator==(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    if (lhs.kind != rhs.kind)
      return false;

    switch (lhs.kind) {
    case Independent:
    case Unspecified:
      return true;

    case ActorInstance:
      return lhs.actor == rhs.actor;

    case GlobalActor:
    case GlobalActorUnsafe:
      return areTypesEqual(lhs.globalActor, rhs.globalActor);
    }
  }

  friend bool operator!=(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    return !(lhs == rhs);
  }

  friend llvm::hash_code hash_value(const ActorIsolation &state) {
    return llvm::hash_combine(state.kind, state.pointer);
  }
};

/// Determine how the given value declaration is isolated.
ActorIsolation getActorIsolation(ValueDecl *value);

/// Determine how the given declaration context is isolated.
ActorIsolation getActorIsolationOfContext(DeclContext *dc);

void simple_display(llvm::raw_ostream &out, const ActorIsolation &state);

} // end namespace swift

#endif /* SWIFT_AST_ACTORISOLATIONSTATE_H */
