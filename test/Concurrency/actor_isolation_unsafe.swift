// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

@MainActor(unsafe) func globalMain() { } // expected-note {{calls to global function 'globalMain()' from outside of its actor context are implicitly asynchronous}}

@SomeGlobalActor(unsafe) func globalSome() { } // expected-note 2{{calls to global function 'globalSome()' from outside of its actor context are implicitly asynchronous}}

// ----------------------------------------------------------------------
// Witnessing and unsafe global actor
// ----------------------------------------------------------------------
protocol P1 {
  @MainActor(unsafe) func onMainActor() // expected-note{{'onMainActor()' declared here}}
}

struct S1_P1: P1 {
  func onMainActor() { }
}

struct S2_P1: P1 {
  @MainActor func onMainActor() { }
}

struct S3_P1: P1 {
  @actorIndependent func onMainActor() { }
}

struct S4_P1: P1 {
  @SomeGlobalActor func onMainActor() { } // expected-error{{instance method 'onMainActor()' isolated to global actor 'SomeGlobalActor' can not satisfy corresponding requirement from protocol 'P1' isolated to global actor 'MainActor'}}
}

// ----------------------------------------------------------------------
// Overriding and unsafe global actor
// ----------------------------------------------------------------------
class C1 {
  @MainActor(unsafe) func method() { } // expected-note{{overridden declaration is here}}
}

class C2: C1 {
  override func method() { // expected-note{{overridden declaration is here}}
    globalSome() // okay
  }
}

class C3: C1 {
  @actorIndependent override func method() {
    globalSome() // expected-error{{global function 'globalSome()' isolated to global actor 'SomeGlobalActor' can not be referenced from a non-isolated synchronous context}}
  }
}

class C4: C1 {
  @MainActor override func method() {
    globalSome() // expected-error{{global function 'globalSome()' isolated to global actor 'SomeGlobalActor' can not be referenced from different global actor 'MainActor'}}
  }
}

class C5: C1 {
  @SomeGlobalActor override func method() { // expected-error{{global actor 'SomeGlobalActor'-isolated instance method 'method()' has different actor isolation from global actor 'MainActor'-isolated overridden declaration}}
  }
}

class C6: C2 {
  // We didn't infer any actor isolation for C2.method().
  @SomeGlobalActor override func method() { // expected-error{{global actor 'SomeGlobalActor'-isolated instance method 'method()' has different actor isolation from non-actor-isolated overridden declaration}}
  }
}

class C7: C2 {
  // We didn't infer any actor isolation for C2.method(), but it's okay to be
  // explicitly unsafe.
  @SomeGlobalActor(unsafe) override func method() {
    globalMain() // expected-error{{global function 'globalMain()' isolated to global actor 'MainActor' can not be referenced from different global actor 'SomeGlobalActor'}}
    globalSome() // okay
  }
}
