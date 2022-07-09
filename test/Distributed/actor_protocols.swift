// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== -----------------------------------------------------------------------

actor A: Actor {} // ok

class C: Actor, UnsafeSendable {
  // expected-error@-1{{non-actor type 'C' cannot conform to the 'AnyActor' protocol}} {{1-6=actor}}
  // expected-error@-2{{non-actor type 'C' cannot conform to the 'Actor' protocol}} {{1-6=actor}}
  // expected-warning@-3{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

struct S: Actor {
  // expected-error@-1{{non-class type 'S' cannot conform to class protocol 'AnyActor'}}
  // expected-error@-2{{non-class type 'S' cannot conform to class protocol 'Actor'}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

struct E: Actor {
  // expected-error@-1{{non-class type 'E' cannot conform to class protocol 'AnyActor'}}
  // expected-error@-2{{non-class type 'E' cannot conform to class protocol 'Actor'}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

// ==== -----------------------------------------------------------------------

distributed actor DA: DistributedActor {
  typealias ActorSystem = FakeActorSystem
}

actor A2: DistributedActor {
// FIXME(distributed): error reporting is a bit whacky here; needs cleanup
// expected-error@-2{{actor type 'A2' cannot conform to the 'DistributedActor' protocol. Isolation rules of these actor types are not interchangeable.}}
// expected-error@-3{{actor type 'A2' cannot conform to the 'DistributedActor' protocol. Isolation rules of these actor types are not interchangeable.}}
  nonisolated var id: ID {
    fatalError()
  }
  nonisolated var actorSystem: ActorSystem {
    fatalError()
  }

  init(system: FakeActorSystem) {
    fatalError()
  }

  static func resolve(id: ID, using system: FakeActorSystem) throws -> Self {
    fatalError()
  }
}

final class DA2: DistributedActor {
// expected-error@-1{{non-actor type 'DA2' cannot conform to the 'AnyActor' protocol}}
// expected-error@-2{{non-distributed actor type 'DA2' cannot conform to the 'DistributedActor' protocol}}
  nonisolated var id: ID {
    fatalError()
  }
  nonisolated var actorSystem: ActorSystem {
    fatalError()
  }

  required init(system: FakeActorSystem) {
    fatalError()
  }
  static func resolve(id: ID, using system: FakeActorSystem) throws -> Self {
    fatalError()
  }
}

struct S2: DistributedActor {
  // expected-error@-1{{non-class type 'S2' cannot conform to class protocol 'DistributedActor'}}
  // expected-error@-2{{non-class type 'S2' cannot conform to class protocol 'AnyActor'}}
  // expected-error@-3{{type 'S2' does not conform to protocol 'Identifiable'}}
}

// ==== -----------------------------------------------------------------------

actor A3: AnyActor {} // ok
distributed actor DA3: AnyActor {} // ok

class C3: AnyActor, @unchecked Sendable {
  // expected-error@-1{{non-actor type 'C3' cannot conform to the 'AnyActor' protocol}} {{1-6=actor}}
}

struct S3: AnyActor {
  // expected-error@-1{{non-class type 'S3' cannot conform to class protocol 'AnyActor'}}
}

enum E3: AnyActor {
  // expected-error@-1{{non-class type 'E3' cannot conform to class protocol 'AnyActor'}}
}
