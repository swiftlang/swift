// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -strict-concurrency=targeted
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -strict-concurrency=complete

// REQUIRES: concurrency

// Test that global actor annotations are rejected on class base types in
// inheritance clauses but allowed on protocol conformances (SE-0466 isolated
// conformances).

actor SomeActorInstance {}

@globalActor
struct SomeActor {
  static let shared = SomeActorInstance()
}

class Base {}
protocol P {}
protocol Q {}

// ERROR: global actor on a class base type.
class Sub1: @SomeActor Base {} // expected-error {{global actor 'SomeActor' cannot apply to non-protocol type 'Base'}}

// ERROR: @MainActor on a class base type.
class Sub2: @MainActor Base {} // expected-error {{global actor 'MainActor' cannot apply to non-protocol type 'Base'}}

// OK: global actor on a protocol conformance (SE-0466 isolated conformances).
class Sub3: @SomeActor P {}

// OK: @MainActor on a protocol conformance.
class Sub4: @MainActor P {}

// OK: global actor on a protocol composition.
class Sub5: @MainActor P & Q {}

// ERROR: global actor on a class base type, with additional conformances.
class Sub6: @SomeActor Base, P {} // expected-error {{global actor 'SomeActor' cannot apply to non-protocol type 'Base'}}

// OK: no global actor annotation.
class Sub7: Base {}
class Sub8: Base, P {}
