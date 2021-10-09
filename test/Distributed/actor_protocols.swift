// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

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

distributed actor DA: DistributedActor {} // ok

actor A2: DistributedActor {
  // expected-error@-1{{non-distributed actor type 'A2' cannot conform to the 'DistributedActor' protocol}} {{1-1=distributed }}
  nonisolated var id: AnyActorIdentity {
    fatalError()
  }
  nonisolated var actorTransport: ActorTransport {
    fatalError()
  }

  init(transport: ActorTransport) {
    fatalError()
  }

  static func resolve(_ identity: AnyActorIdentity, using transport: ActorTransport) throws -> Self {
    fatalError()
  }
}

class C2: DistributedActor {
  // expected-error@-1{{non-actor type 'C2' cannot conform to the 'Actor' protocol}}
  // expected-error@-2{{non-final class 'C2' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  nonisolated var id: AnyActorIdentity {
    fatalError()
  }
  nonisolated var actorTransport: ActorTransport {
    fatalError()
  }

  required init(transport: ActorTransport) {
    fatalError()
  }
  static func resolve(_ identity: AnyActorIdentity, using transport: ActorTransport) throws -> Self {
    fatalError()
  }
}

struct S2: DistributedActor {
  // expected-error@-1{{non-class type 'S2' cannot conform to class protocol 'DistributedActor'}}
  // expected-error@-2{{non-class type 'S2' cannot conform to class protocol 'AnyActor'}}
  // expected-error@-3{{type 'S2' does not conform to protocol 'Identifiable'}}
}

