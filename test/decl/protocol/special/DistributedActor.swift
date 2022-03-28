// RUN: %target-typecheck-verify-swift -disable-availability-checking -verify-ignore-unknown
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

/// Use the existential wrapper as the default actor system.
typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

distributed actor D1 {
  var x: Int = 17
}

distributed actor D2 {
  // expected-error@-1{{actor 'D2' has no initializers}}
  let actorSystem: String
  // expected-error@-1{{property 'actorSystem' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized implementation for protocol requirement 'actorSystem'}}
  // expected-note@-3{{stored property 'actorSystem' without initial value prevents synthesized initializers}}
}

distributed actor D3 {
  // expected-error@-1{{type 'D3' does not conform to protocol 'Identifiable'}}
  // expected-error@-2{{type 'D3' does not conform to protocol 'DistributedActor'}}
  // Codable synthesis also will fail since the ID mismatch:
  // expected-error@-4{{type 'D3' does not conform to protocol 'Decodable'}}
  // expected-error@-5{{type 'D3' does not conform to protocol 'Encodable'}}

  var id: Int { 0 }
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized property 'id'}}
  // expected-note@-3{{matching requirement 'id' to this declaration inferred associated type to 'Int'}}
}

struct OtherActorIdentity: Sendable, Hashable, Codable {}

distributed actor D4 {
  // expected-error@-1{{actor 'D4' has no initializers}}
  // expected-error@-2{{type 'D4' does not conform to protocol 'DistributedActor'}}
  // expected-error@-3{{type 'D4' does not conform to protocol 'Identifiable'}}
  // Codable synthesis also will fail since the ID errors:
  // expected-error@-5{{type 'D4' does not conform to protocol 'Decodable'}}
  // expected-error@-6{{type 'D4' does not conform to protocol 'Encodable'}}

  let actorSystem: String
  // expected-error@-1{{property 'actorSystem' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2 {{invalid redeclaration of synthesized property 'actorSystem'}}
  // expected-note@-3{{stored property 'actorSystem' without initial value prevents synthesized initializers}}
  let id: OtherActorIdentity
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized property 'id'}}
  // expected-note@-3{{stored property 'id' without initial value prevents synthesized initializers}}
  // expected-note@-4{{matching requirement 'id' to this declaration inferred associated type to 'OtherActorIdentity'}}
}

protocol P1: DistributedActor {
  distributed func dist() -> String
  // expected-note@-1{{distributed instance method requirement 'dist()' declared here}}
}

distributed actor D5: P1 {
  func dist() -> String { "" }
  // expected-error@-1{{distributed actor-isolated instance method 'dist()' cannot be used to satisfy a protocol requirement}}
  // expected-note@-2{{add 'distributed' to 'dist()' to make this instance method witness the protocol requirement}}{{3-3=distributed }}
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances have been added implicitly.
func acceptDistributedActor<Act: DistributedActor>(_: Act.Type) { }
func acceptAnyActor<Act: AnyActor>(_: Act.Type) { }

func testConformance() {
  acceptDistributedActor(D1.self)
  acceptAnyActor(D1.self)
}
