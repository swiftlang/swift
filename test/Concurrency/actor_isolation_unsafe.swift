// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -strict-concurrency=complete

// REQUIRES: concurrency

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

// expected-warning@+1 {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
@MainActor(unsafe) func globalMain() { } // expected-note {{calls to global function 'globalMain()' from outside of its actor context are implicitly asynchronous}}

// expected-warning@+1 {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
@SomeGlobalActor(unsafe) func globalSome() { } // expected-note 2{{calls to global function 'globalSome()' from outside of its actor context are implicitly asynchronous}}
// expected-complete-note @-1 {{calls to global function 'globalSome()' from outside of its actor context are implicitly asynchronous}}

// ----------------------------------------------------------------------
// Witnessing and unsafe global actor
// ----------------------------------------------------------------------
protocol P1 {
  // expected-warning@+1 {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
  @MainActor(unsafe) func onMainActor()
}

struct S1_P1: P1 {
  func onMainActor() { }
}

struct S2_P1: P1 {
  @MainActor func onMainActor() { }
}

struct S3_P1: P1 {
  nonisolated func onMainActor() { }
}

// expected-warning@+1{{conformance of 'S4_P1_not_quietly' to protocol 'P1' involves isolation mismatches and can cause data races}}
struct S4_P1_not_quietly: P1 {
  // expected-note@-1{{turn data races into runtime errors with '@preconcurrency'}}

  @SomeGlobalActor func onMainActor() { }
  // expected-note @-1 {{global actor 'SomeGlobalActor'-isolated instance method 'onMainActor()' cannot satisfy main actor-isolated requirement}}
}

// expected-warning@+2{{conformance of 'S4_P1' to protocol 'P1' involves isolation mismatches and can cause data races}}
@SomeGlobalActor
struct S4_P1: P1 {
  // expected-note@-1{{turn data races into runtime errors with '@preconcurrency'}}

  @SomeGlobalActor func onMainActor() { } // expected-note{{global actor 'SomeGlobalActor'-isolated instance method 'onMainActor()' cannot satisfy main actor-isolated requirement}}
}

// expected-warning@+1 {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
@MainActor(unsafe)
protocol P2 {
  func f() // expected-note{{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
  // expected-complete-note @-1 {{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
  nonisolated func g()
}

struct S5_P2: P2 {
  func f() { } // expected-note{{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{main actor isolation inferred from conformance to protocol 'P2'}}
  // expected-complete-note @-2 {{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
  // expected-complete-note @-3 {{main actor isolation inferred from conformance to protocol 'P2'}}}
  func g() { }
}

nonisolated func testP2(x: S5_P2, p2: P2) {
  p2.f() // expected-warning{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  p2.g() // OKAY
  x.f() // expected-warning{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  x.g() // OKAY
}

func testP2_noconcurrency(x: S5_P2, p2: P2) {
  // expected-complete-note @-1 2{{add '@MainActor' to make global function 'testP2_noconcurrency(x:p2:)' part of global actor 'MainActor'}}
  p2.f() // okay without complete. with targeted/minimal not concurrency-related code.
  // expected-complete-warning @-1 {{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  p2.g() // okay
  x.f() // okay without complete. with targeted/minimal not concurrency-related code
  // expected-complete-warning @-1 {{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  x.g() // OKAY
}

// ----------------------------------------------------------------------
// Overriding and unsafe global actor
// ----------------------------------------------------------------------
class C1 {
  // expected-warning@+1 {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
  @MainActor(unsafe) func method() { } // expected-note{{overridden declaration is here}}
}

class C2: C1 {
  override func method() { // expected-note 2{{overridden declaration is here}}
    globalSome() // okay when not in complete
    // expected-complete-warning @-1 {{call to global actor 'SomeGlobalActor'-isolated global function 'globalSome()' in a synchronous main actor-isolated context}}
  }
}

class C3: C1 {
  nonisolated override func method() {
    globalSome() // expected-warning{{call to global actor 'SomeGlobalActor'-isolated global function 'globalSome()' in a synchronous nonisolated context}}
  }
}

class C4: C1 {
  @MainActor override func method() {
    globalSome() // expected-warning{{call to global actor 'SomeGlobalActor'-isolated global function 'globalSome()' in a synchronous main actor-isolated context}}
  }
}

class C5: C1 {
  @SomeGlobalActor override func method() { // expected-error{{global actor 'SomeGlobalActor'-isolated instance method 'method()' has different actor isolation from main actor-isolated overridden declaration}}
  }
}

class C6: C2 {
  // We didn't infer any actor isolation for C2.method().
  @SomeGlobalActor override func method() { // expected-error{{global actor 'SomeGlobalActor'-isolated instance method 'method()' has different actor isolation from main actor-isolated overridden declaration}}
  }
}

class C7: C2 {
  // expected-warning@+1 {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
  @SomeGlobalActor(unsafe) override func method() { // expected-error{{global actor 'SomeGlobalActor'-isolated instance method 'method()' has different actor isolation from main actor-isolated overridden declaration}}
    globalMain() // expected-warning{{call to main actor-isolated global function 'globalMain()' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
    globalSome() // okay
  }
}

// expected-warning@+1 {{'(unsafe)' global actors are deprecated; use '@preconcurrency' instead}}
@MainActor(unsafe) // expected-note {{'GloballyIsolatedProto' is isolated to global actor 'MainActor' here}}
protocol GloballyIsolatedProto {
}

// rdar://75849035 - trying to conform an actor to a global-actor-isolated protocol should result in an error
func test_conforming_actor_to_global_actor_protocol() {
  actor MyValue : GloballyIsolatedProto {}
  // expected-error@-1 {{actor 'MyValue' cannot conform to global-actor-isolated protocol 'GloballyIsolatedProto'}}
}
