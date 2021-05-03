// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

// Synthesis of distributed actors.

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
distributed actor D1 {
  var x: Int = 17
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
distributed actor D2 {
  let actorTransport: String // expected-error{{invalid redeclaration of synthesized implementation for protocol requirement 'actorTransport'}}
  let actorAddress: String // expected-error{{invalid redeclaration of synthesized implementation for protocol requirement 'actorAddress'}}
}

// ==== Tests ------------------------------------------------------------------

// Make sure the conformances actually happen.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func acceptActor<Act: DistributedActor>(_: Act.Type) { }

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func testConformance() {
  acceptActor(D1.self)
}
