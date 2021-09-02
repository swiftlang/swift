// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -verify-ignore-unknown
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor D1 {
  var x: Int = 17
}

@available(SwiftStdlib 5.5, *)
distributed actor D2 {
  // expected-error@-1{{actor 'D2' has no initializers}}
  let actorTransport: String
  // expected-error@-1{{property 'actorTransport' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized implementation for protocol requirement 'actorTransport'}}
  // expected-note@-3{{stored property 'actorTransport' without initial value prevents synthesized initializers}}
}

@available(SwiftStdlib 5.5, *)
distributed actor D3 {
  var id: Int { 0 }
  // expected-error@-1{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-2{{invalid redeclaration of synthesized implementation for protocol requirement 'id'}}
}

@available(SwiftStdlib 5.5, *)
distributed actor D4 {
  // expected-error@-1{{actor 'D4' has no initializers}}
  let actorTransport: String
  // expected-error@-1{{invalid redeclaration of synthesized implementation for protocol requirement 'actorTransport'}}
  // expected-error@-2{{property 'actorTransport' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-note@-3{{stored property 'actorTransport' without initial value prevents synthesized initializers}}
  let id: AnyActorIdentity
  // expected-error@-1{{actor-isolated property 'id' cannot be used to satisfy a protocol requirement}}
  // expected-error@-2{{property 'id' cannot be defined explicitly, as it conflicts with distributed actor synthesized stored property}}
  // expected-error@-3{{actor-isolated property 'id' cannot be used to satisfy a protocol requirement}}
  // expected-note@-4{{stored property 'id' without initial value prevents synthesized initializers}}
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances have been added implicitly.
@available(SwiftStdlib 5.5, *)
func acceptDistributedActor<Act: DistributedActor>(_: Act.Type) { }
@available(SwiftStdlib 5.5, *)
func acceptAnyActor<Act: AnyActor>(_: Act.Type) { }

@available(SwiftStdlib 5.5, *)
func testConformance() {
  acceptDistributedActor(D1.self)
  acceptAnyActor(D1.self)
}
