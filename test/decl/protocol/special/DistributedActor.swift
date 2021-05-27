// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// Synthesis of distributed actors.

@available(SwiftStdlib 5.5, *)
distributed actor D1 {
  var x: Int = 17
}

@available(SwiftStdlib 5.5, *)
distributed actor D2 {
  let actorTransport: String // expected-error{{invalid redeclaration of synthesized implementation for protocol requirement 'actorTransport'}}
  let actorAddress: String // expected-error{{invalid redeclaration of synthesized implementation for protocol requirement 'actorAddress'}}
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances actually happen.
@available(SwiftStdlib 5.5, *)
func acceptActor<Act: DistributedActor>(_: Act.Type) { }

@available(SwiftStdlib 5.5, *)
func testConformance() {
  acceptActor(D1.self)
}
