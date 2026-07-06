// This test validates that WITHOUT the FlowIsolationGlobalActor experimental
// feature, we retain the old (pre-feature) Sema behavior: the global-actor
// flow-isolation extension is gated off, so Sema directly diagnoses
// cross-isolation initialization of global-actor-isolated stored properties
// instead of deferring the check to the SIL flow-isolation pass.
//
// NOTE: This is intentionally a small, standalone test so that it is trivial to
// delete once FlowIsolationGlobalActor becomes the default behavior. It should
// NOT pass -enable-experimental-feature FlowIsolationGlobalActor.

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-version 6 -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

@MainActor
protocol GloballyIsolated {}

class NonSendable {}

// A nonisolated struct removes global-actor isolation from its stored
// properties, so initializing them from a nonisolated init is always fine. This
// does not depend on FlowIsolationGlobalActor.
nonisolated struct StructRemovesGlobalActor: GloballyIsolated {
  var x: NonSendable
  init(x: NonSendable) {
    self.x = x // okay
  }
}

// A global-actor-isolated struct keeps its stored properties isolated to the
// global actor. Without FlowIsolationGlobalActor, Sema rejects writing the
// isolated property from a nonisolated init rather than deferring to flow
// isolation.
struct GlobalActorIsolated: GloballyIsolated {
  // expected-note@+1 {{mutation of this property is only permitted within the actor}}
  var z: NonSendable
  nonisolated init(z: NonSendable) {
    // expected-error@+1 {{main actor-isolated property 'z' can not be mutated from a nonisolated context}}
    self.z = z
  }
}
