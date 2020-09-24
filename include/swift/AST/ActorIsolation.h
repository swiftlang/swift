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

#include "llvm/ADT/Hashing.h"

namespace llvm {
class raw_ostream;
}

namespace swift {
class ClassDecl;

/// Describes the actor isolation of a given declaration, which determines
/// the actors with which it can interact.
class ActorIsolation {
public:
  enum Kind {
    /// The actor isolation has not been specified. It is assumed to be
    /// unsafe to interact with this declaration from any actor.
    Unspecified = 0,
    /// The declaration is isolated to the instance of an actor class.
    /// For example, a mutable stored property or synchronous function within
    /// the actor is isolated to the instance of that actor.
    ActorInstance,
    /// The declaration can refer to actor-isolated state, but can also be
    //// referenced from outside the actor.
    ActorPrivileged,
    /// The declaration is explicitly specified to be independent of any actor,
    /// meaning that it can be used from any actor but is also unable to
    /// refer to the isolated state of any given actor.
    Independent,
  };

private:
  Kind kind;
  ClassDecl *actor;

  ActorIsolation(Kind kind, ClassDecl *actor) : kind(kind), actor(actor) { }

public:
  static ActorIsolation forUnspecified() {
    return ActorIsolation(Unspecified, nullptr);
  }

  static ActorIsolation forIndependent() {
    return ActorIsolation(Independent, nullptr);
  }

  static ActorIsolation forActorPrivileged(ClassDecl *actor) {
    return ActorIsolation(ActorPrivileged, actor);
  }

  static ActorIsolation forActorInstance(ClassDecl *actor) {
    return ActorIsolation(ActorInstance, actor);
  }

  Kind getKind() const { return kind; }

  operator Kind() const { return getKind(); }

  ClassDecl *getActor() const {
    assert(getKind() == ActorInstance || getKind() == ActorPrivileged);
    return actor;
  }

  friend bool operator==(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    if (lhs.kind != rhs.kind)
      return false;

    switch (lhs.kind) {
    case Independent:
    case Unspecified:
      return true;

    case ActorInstance:
    case ActorPrivileged:
      return lhs.actor == rhs.actor;
    }
  }

  friend bool operator!=(const ActorIsolation &lhs,
                         const ActorIsolation &rhs) {
    return !(lhs == rhs);
  }

  friend llvm::hash_code hash_value(const ActorIsolation &state) {
    return llvm::hash_combine(state.kind, state.actor);
  }
};

void simple_display(llvm::raw_ostream &out, const ActorIsolation &state);

} // end namespace swift

#endif /* SWIFT_AST_ACTORISOLATIONSTATE_H */
