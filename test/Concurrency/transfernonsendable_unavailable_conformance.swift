// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -disable-availability-checking -verify  %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability

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
    _ = Bar(ns) // expected-warning {{sending 'ns' risks causing data races}}
    // TODO: This needs to be:
    // 'ns' is transferred to actor-isolated callee. Later local uses could race with uses in callee.
    // expected-note @-3 {{sending 'ns' to actor-isolated initializer 'init(_:)' risks causing data races between actor-isolated and local actor-isolated uses}}
    ns.foo() // expected-note {{access can happen concurrently}}
  }
}
