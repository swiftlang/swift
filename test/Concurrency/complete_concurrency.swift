// RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -enable-upcoming-feature CompleteConcurrency -emit-sil -o /dev/null -verify %s

// REQUIRES: concurrency

// expected-note@+1 {{class 'NonSendable' does not conform to the 'Sendable' protocol}}
class NonSendable {}

@MainActor func onMain(ns: NonSendable) async {
  // expected-warning@+1 {{passing argument of non-sendable type 'NonSendable' outside of main actor-isolated context may introduce data races}}
  await onGeneric(ns: ns)
}

@propertyWrapper
struct Wrapper {
  // expected-note@+1 {{mutation of this property is only permitted within the actor}}
  @preconcurrency @MainActor var wrappedValue: NonSendable

  init(wrappedValue: NonSendable) {
    // expected-warning@+1 {{main actor-isolated property 'wrappedValue' can not be mutated from a non-isolated context; this is an error in Swift 6}}
    self.wrappedValue = wrappedValue
  }
}

struct S {
  @Wrapper var value: NonSendable = NonSendable()
}

nonisolated func onGeneric(ns: NonSendable) async {
  let _ = S()
}
