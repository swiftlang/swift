// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency

// Synthesis of distributed actors.

distributed actor D1 {
  var x: Int = 17
}

distributed actor D2 {
  let actorTransport: String // expected-error{{invalid redeclaration of synthesized implementation for protocol requirement 'actorTransport'}}
  let actorAddress: String // expected-error{{invalid redeclaration of synthesized implementation for protocol requirement 'actorAddress'}}
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances actually happen.
func acceptActor<Act: DistributedActor>(_: Act.Type) { }

func testConformance() {
  acceptActor(D1.self)
}
