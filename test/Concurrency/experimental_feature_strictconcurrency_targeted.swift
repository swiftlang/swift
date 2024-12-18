// RUN: %target-swift-frontend -enable-experimental-feature StrictConcurrency=targeted %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -strict-concurrency=complete
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -enable-experimental-feature StrictConcurrency
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -enable-experimental-feature StrictConcurrency=targeted -enable-experimental-feature StrictConcurrency=complete
// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation
// REQUIRES: swift_feature_StrictConcurrency

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

