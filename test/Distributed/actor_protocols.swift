// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// ==== -----------------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
actor A: Actor {} // ok

@available(SwiftStdlib 5.5, *)
class C: Actor, UnsafeSendable {
  // expected-error@-1{{non-actor type 'C' cannot conform to the 'Actor' protocol}} {{1-6=actor}}
  // expected-warning@-2{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

@available(SwiftStdlib 5.5, *)
struct S: Actor {
  // expected-error@-1{{non-class type 'S' cannot conform to class protocol 'Actor'}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

@available(SwiftStdlib 5.5, *)
struct E: Actor {
  // expected-error@-1{{non-class type 'E' cannot conform to class protocol 'Actor'}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError()
  }
}

// ==== -----------------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
distributed actor DA: DistributedActor {} // ok

@available(SwiftStdlib 5.5, *)
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
  init(resolve id: AnyActorIdentity, using transport: ActorTransport) throws {
    fatalError()
  }
}

@available(SwiftStdlib 5.5, *)
class C2: DistributedActor {
  // expected-error@-1{{non-actor type 'C2' cannot conform to the 'Actor' protocol}}
  nonisolated var id: AnyActorIdentity {
    fatalError()
  }
  nonisolated var actorTransport: ActorTransport {
    fatalError()
  }

  required init(transport: ActorTransport) {
    fatalError()
  }
  required init(resolve id: AnyActorIdentity, using transport: ActorTransport) throws {
    fatalError()
  }
}

@available(SwiftStdlib 5.5, *)
struct S2: DistributedActor {
  // expected-error@-1{{non-class type 'S2' cannot conform to class protocol 'DistributedActor'}}
  // expected-error@-2{{non-class type 'S2' cannot conform to class protocol 'AnyActor'}}
  // expected-error@-3{{type 'S2' does not conform to protocol 'Identifiable'}}
}

