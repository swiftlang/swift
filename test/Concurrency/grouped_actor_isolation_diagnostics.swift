// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature GroupActorErrors  -strict-concurrency=complete
// REQUIRES: concurrency
// REQUIRES: swift_feature_GroupActorErrors

@MainActor
protocol P {
  func f()
  nonisolated func g()
}

@preconcurrency @MainActor
protocol Q {
  func f()
  nonisolated func g()
}

struct S: P {
  func f() { }
  func g() { }
}

@preconcurrency
struct NonConcurrentS: Q {
  func f() { }
  func g() { }
}

// expected-note@+1{{add '@MainActor' to make global function 'testP(s:p:)' part of global actor 'MainActor'}}
func testP(s: S, p: P) { // expected-error {{calls to '@MainActor'-isolated' code in global function 'testP(s:p:)'}}
  p.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  p.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  p.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  p.g() // OKAY
  s.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  s.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  s.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  s.g() // OKAY
}
// expected-note @+1{{add '@MainActor' to make global function 'testPreconcurrency(ncs:s:)' part of global actor 'MainActor'}}
func testPreconcurrency(ncs: NonConcurrentS, s:S ) { // expected-error {{calls to '@MainActor'-isolated' code in global function 'testPreconcurrency(ncs:s:)'}}
  ncs.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  ncs.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  ncs.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  ncs.g() // OKAY
  s.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  s.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  s.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  s.g() // OKAY
}

// expected-note @+1{{add '@MainActor' to make global function 'testOnlyPreconcurrency(ncs:)' part of global actor 'MainActor'}}
func testOnlyPreconcurrency(ncs: NonConcurrentS) { // expected-warning {{calls to '@MainActor'-isolated' code in global function 'testOnlyPreconcurrency(ncs:)'}}
  ncs.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  ncs.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  ncs.f() // expected-note{{call to main actor-isolated instance method 'f()' in a synchronous nonisolated context}}
  ncs.g() // OKAY
}

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@propertyWrapper
struct WrapperOnActor<Wrapped: Sendable> {
  private var stored: Wrapped

  nonisolated init(wrappedValue: Wrapped) {
    stored = wrappedValue
  }

  @MainActor var wrappedValue: Wrapped {
    get { }
    set { }
  }

  @SomeGlobalActor var projectedValue: Wrapped {
    get { }
    set { }
  }
}

struct HasWrapperOnActor {
  @WrapperOnActor var x: Int = 0
  @WrapperOnActor var y: String = ""
  @WrapperOnActor var z: (Double, Double) = (1.0,2.0)

  // expected-error@+2{{calls to '@MainActor'-isolated' code in instance method 'testWrapped()'}}
  // expected-note@+1{{add '@MainActor' to make instance method 'testWrapped()' part of global actor 'MainActor'}}
  func testWrapped() {
    _ = x // expected-note{{main actor-isolated property 'x' can not be referenced from a nonisolated context}}
    _ = y // expected-note{{main actor-isolated property 'y' can not be referenced from a nonisolated context}}
    _ = z // expected-note{{main actor-isolated property 'z' can not be referenced from a nonisolated context}}
  }

  // expected-error@+2{{calls to '@SomeGlobalActor'-isolated' code in instance method 'testProjected()'}}
  // expected-note@+1{{add '@SomeGlobalActor' to make instance method 'testProjected()' part of global actor 'SomeGlobalActor'}}
  func testProjected(){
    _ = $x // expected-note{{global actor 'SomeGlobalActor'-isolated property '$x' can not be referenced from a nonisolated context}}
    _ = $y // expected-note{{global actor 'SomeGlobalActor'-isolated property '$y' can not be referenced from a nonisolated context}}
    _ = $z // expected-note{{global actor 'SomeGlobalActor'-isolated property '$z' can not be referenced from a nonisolated context}}
  }

  @MainActor
  func testMA(){ }

  // expected-note@+1{{add '@MainActor' to make instance method 'testErrors()' part of global actor 'MainActor'}}
  func testErrors() {  // expected-error{{calls to '@MainActor'-isolated' code in instance method 'testErrors()'}}
    testMA() // expected-error{{call to main actor-isolated instance method 'testMA()' in a synchronous nonisolated context}}
  }
}

@preconcurrency @MainActor
class MainActorPreconcurrency {}

class InferMainActorPreconcurrency: MainActorPreconcurrency {
  static func predatesConcurrency() {}
  func predatesConcurrency (s: String) -> String { return s }
  func predatesConcurrency (n: Int) -> Int  { return n }
}

nonisolated func testPreconcurrency() {
  InferMainActorPreconcurrency.predatesConcurrency()
  // expected-warning@-1 {{call to main actor-isolated static method 'predatesConcurrency()' in a synchronous nonisolated context}}
}

func testPreconcurrencyGrouped() {  // expected-warning {{calls to '@MainActor'-isolated' code in global function 'testPreconcurrencyGrouped()'}}
  // expected-note@-1 {{add '@MainActor' to make global function 'testPreconcurrencyGrouped()' part of global actor 'MainActor'}}
  InferMainActorPreconcurrency.predatesConcurrency()
  // expected-note@-1 {{call to main actor-isolated static method 'predatesConcurrency()' in a synchronous nonisolated context}}
  let _ = InferMainActorPreconcurrency().predatesConcurrency(s:"swift 6")
  // expected-note@-1 {{call to main actor-isolated instance method 'predatesConcurrency(s:)' in a synchronous nonisolated context}}
  let _ = InferMainActorPreconcurrency().predatesConcurrency(n:4)
  // expected-note@-1 {{call to main actor-isolated instance method 'predatesConcurrency(n:)' in a synchronous nonisolated context}}
}
