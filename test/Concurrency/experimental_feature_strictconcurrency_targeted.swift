// RUN: %target-swift-frontend -enable-experimental-feature StrictConcurrency=targeted %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -strict-concurrency=complete
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

class C { // expected-note {{class 'C' does not conform to the 'Sendable' protocol}}
  // expected-complete-note @-1 {{class 'C' does not conform to the 'Sendable' protocol}}
  var counter = 0
}

func acceptsSendable<T: Sendable>(_: T) { }

func testNoConcurrency(c: C) {
  acceptsSendable(c) // expected-complete-warning {{type 'C' does not conform to the 'Sendable' protocol}}
}

@available(SwiftStdlib 5.1, *)
func testConcurrency(c: C) async {
  acceptsSendable(c) // expected-warning{{type 'C' does not conform to the 'Sendable' protocol}}
}

