// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== -----------------------------------------------------------------------

actor A: Actor {} // ok

class C: Actor, UnsafeSendable {
  // expected-error@-1{{non-actor type 'C' cannot conform to the 'Actor' protocol}} {{1-6=actor}}
  // expected-warning@-2{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

struct S: Actor {
  // expected-error@-1{{non-class type 'S' cannot conform to class protocol 'Actor'}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

struct E: Actor {
  // expected-error@-1{{non-class type 'E' cannot conform to class protocol 'Actor'}}
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
// expected-note@-4 {{add stubs for conformance}}
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
// expected-error@-1{{non-distributed actor type 'DA2' cannot conform to the 'DistributedActor' protocol}}
// expected-note@-2 {{add stubs for conformance}}
  nonisolated var id: ID {
    fatalError()
  }
  nonisolated var actorSystem: ActorSystem {
    fatalError()
  }
  nonisolated var unownedExecutor: UnownedSerialExecutor {
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
  // expected-error@-2{{type 'S2' does not conform to protocol 'Identifiable'}}
  // expected-note@-3 {{add stubs for conformance}}
}

// ==== -----------------------------------------------------------------------

actor A3: AnyActor {} // expected-warning {{'AnyActor' is deprecated: Use 'any Actor' with 'DistributedActor.asLocalActor' instead}}
distributed actor DA3: AnyActor {} // expected-warning {{'AnyActor' is deprecated: Use 'any Actor' with 'DistributedActor.asLocalActor' instead}}

class C3: AnyActor { // expected-warning {{'AnyActor' is deprecated: Use 'any Actor' with 'DistributedActor.asLocalActor' instead}}
  // expected-warning@-1 {{non-final class 'C3' cannot conform to the 'Sendable' protocol}}
}

struct S3: AnyActor { // expected-warning {{'AnyActor' is deprecated: Use 'any Actor' with 'DistributedActor.asLocalActor' instead}}
  // expected-error@-1{{only protocols can inherit from 'AnyObject'}}
}

enum E3: AnyActor { // expected-warning {{'AnyActor' is deprecated: Use 'any Actor' with 'DistributedActor.asLocalActor' instead}}
  // expected-error@-1{{only protocols can inherit from 'AnyObject'}}
}
