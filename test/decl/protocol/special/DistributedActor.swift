// RUN: %target-typecheck-verify-swift -disable-availability-checking -verify-ignore-unknown
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

/// Use the existential wrapper as the default actor system.
typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

distributed actor D0 {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  var x: Int = 17
}

distributed actor D1 {
  var x: Int = 17
}

class X: Identifiable {
  // should work as expected, synthesis not triggering
  func test() {
    let _: ObjectIdentifier = self.id
  }
}

protocol DAP: DistributedActor {
  // should work as expected, synthesis not triggering
  func test()
}

extension DAP where ActorSystem.ActorID == String {
  func test() {
    _ = self.id == ""
  }
}

distributed actor D2 {
  let actorSystem: String
  // expected-error@-1{{property 'actorSystem' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
}

distributed actor D3 {
  var id: Int { 0 }
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
}

struct OtherActorIdentity: Sendable, Hashable, Codable {}

distributed actor D4 {
  let actorSystem: String
  // expected-error@-1{{property 'actorSystem' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  let id: OtherActorIdentity
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
}

protocol P1: DistributedActor {
  distributed func dist() -> String
  // expected-note@-1{{requirement 'dist()' declared here}}
}

distributed actor D5: P1 {
  func dist() -> String { "" }
  // expected-error@-1{{distributed actor-isolated instance method 'dist()' cannot be used to satisfy actor-isolated requirement from protocol 'P1'}}
  // expected-note@-2{{add 'distributed' to 'dist()' to make this instance method satisfy the protocol requirement}}{{3-3=distributed }}
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances have been added implicitly.
func acceptDistributedActor<Act: DistributedActor>(_: Act.Type) { }
func acceptAnyActor<Act: AnyActor>(_: Act.Type) { } // expected-warning {{'AnyActor' is deprecated: Use 'any Actor' with 'DistributedActor.asLocalActor' instead}}

func testConformance() {
  acceptDistributedActor(D1.self)
  acceptAnyActor(D1.self)
}

// https://github.com/apple/swift/issues/69244
protocol P {
  func foo() -> Void
  // expected-note@-1{{mark the protocol requirement 'foo()' 'async throws' to allow actor-isolated conformances}}{{13-13= async throws}}
}

distributed actor A: P {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed func foo() { }
  // expected-error@-1{{actor-isolated distributed instance method 'foo()' cannot be used to satisfy nonisolated requirement from protocol 'P'}}
}
// ---
