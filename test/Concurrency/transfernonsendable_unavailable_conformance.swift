// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -disable-availability-checking -verify  %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

// This test makes sure that we treat types with an unavailable Sendable
// conformance as being non-Sendable.

public class NonSendable {
  func foo() {
  }
}

@available(*, unavailable)
extension NonSendable: Sendable {}

actor Bar {
  init(_ _: NonSendable) {
  }
  func bar() async {
    let ns = NonSendable()
    _ = Bar(ns) // expected-warning {{transferring 'ns' may cause a race}}
    // TODO: This needs to be:
    // disconnected 'ns' is transferred to actor-isolated callee. Later local uses could race with uses in callee.
    // expected-note @-3 {{transferring disconnected 'ns' to actor-isolated callee could cause races in between callee actor-isolated and local actor-isolated uses}}
    ns.foo() // expected-note {{use here could race}}
  }
}
