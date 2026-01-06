// RUN: %target-swift-frontend -swift-version 5 -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -enable-upcoming-feature DynamicActorIsolation -strict-concurrency=complete -verify-additional-prefix complete-tns-

// REQUIRES: concurrency
// REQUIRES: swift_feature_DynamicActorIsolation

protocol Q {
}

do {
  class K {}

  struct A : @preconcurrency Q {} // Ok
  // expected-warning@-1 {{'@preconcurrency' on conformance to 'Q' has no effect}}

  struct B : @preconcurrency K {
    // expected-error@-1 {{'@preconcurrency' cannot apply to non-protocol type 'K'}}
    var x: @preconcurrency Int
    // expected-error@-1 {{'@preconcurrency' only applies in inheritance clauses}}
  }

  typealias T = @preconcurrency Q
  // expected-error@-1 {{'@preconcurrency' only applies in inheritance clauses}}

  func test(_: @preconcurrency K) {}
  // expected-error@-1 {{'@preconcurrency' only applies in inheritance clauses}}
}

protocol InvalidUseOfPreconcurrencyAttr : @preconcurrency Q {
  // expected-error@-1 {{'@preconcurrency' only applies in inheritance clauses}}
}

struct TestPreconcurrencyAttr {}
extension TestPreconcurrencyAttr : @preconcurrency Q { // Ok
  // expected-warning@-1 {{'@preconcurrency' on conformance to 'Q' has no effect}}
}

class NonSendable {}

protocol TestSendability {
  var x: NonSendable { get }
  func test(_: NonSendable?) -> [NonSendable]
}

// Make sure that preconcurrency conformances suppress Sendable diagnostics,
// because @preconcurrency assumes that the witness will always be called
// from within the same isolation domain (with a dynamic check).
@MainActor
struct Value : @preconcurrency TestSendability {
  var x: NonSendable { NonSendable() }
  // expected-note@-1 2 {{property declared here}}

  func test(_: NonSendable?) -> [NonSendable] {
    // expected-note@-1 2 {{calls to instance method 'test' from outside of its actor context are implicitly asynchronous}}
    []
  }
}

// Make sure we don't spuriously say the @preconcurrency is unnecessary.
@MainActor
struct OtherValue : @preconcurrency TestSendability {
  var x: NonSendable { NonSendable() }

  nonisolated func test(_: NonSendable?) -> [NonSendable] {
    []
  }
}

// Make sure that references to actor isolated witness is diagnosed

// expected-note@+1 2 {{add '@MainActor' to make global function 'test(value:)' part of global actor 'MainActor'}}
func test(value: Value) {
  _ = value.x
  // expected-error@-1 {{main actor-isolated property 'x' can not be referenced from a nonisolated context}}
  _ = value.test(nil)
  // expected-error@-1 {{call to main actor-isolated instance method 'test' in a synchronous nonisolated context}}
}

actor MyActor {
  var value: Value? = nil
}

extension MyActor : @preconcurrency TestSendability {
  var x: NonSendable { NonSendable() }

  func test(_: NonSendable?) -> [NonSendable] {
    []
  }

  func test_ref_diagnostics() {
    _ = value?.x
    // expected-error@-1 {{main actor-isolated property 'x' can not be referenced on a nonisolated actor instance}}
    _ = value?.test(nil)
    // expected-error@-1 {{call to main actor-isolated instance method 'test' in a synchronous actor-isolated context}}
  }
}

protocol Initializable {
  init()
}

final class K : @preconcurrency Initializable {
  // expected-warning@-1 {{'@preconcurrency' on conformance to 'Initializable' has no effect}}
  init() {} // Ok
}

// expected-warning@+2{{conformance of 'MainActorK' to protocol 'Initializable' crosses into main actor-isolated code and can cause data races}}
@MainActor
final class MainActorK: Initializable {
  // expected-note@-1{{turn data races into runtime errors with '@preconcurrency'}}{{25-25=@preconcurrency }}
  // expected-note@-2{{mark all declarations used in the conformance 'nonisolated'}}
  // expected-note@-3{{isolate this conformance to the main actor with '@MainActor'}}
  init() { } // expected-note{{main actor-isolated initializer 'init()' cannot satisfy nonisolated requirement}}
}

protocol WithAssoc {
  associatedtype T
  func test() -> T
}

struct TestConditional<T> {}

extension TestConditional : @preconcurrency WithAssoc where T == Int {
  @MainActor func test() -> T { 42 } // Ok
}

@globalActor
struct GlobalActor {
  static let shared: MyActor = MyActor()
}

protocol WithIndividuallyIsolatedRequirements {
  @MainActor var a: Int { get set }
  @GlobalActor var b: Int { get set }

  @GlobalActor func test()
}

do {
  // expected-warning@+2{{conformance of 'TestExplicitGlobalActorAttrs' to protocol 'WithIndividuallyIsolatedRequirements' involves isolation mismatches and can cause data races}}
  @MainActor
  struct TestExplicitGlobalActorAttrs : @preconcurrency WithIndividuallyIsolatedRequirements {
    // expected-warning@-1 {{'@preconcurrency' on conformance to 'WithIndividuallyIsolatedRequirements' has no effect}}

    var a: Int = 42

    @MainActor var b: Int {
      // expected-note@-1 {{main actor-isolated property 'b' cannot satisfy global actor 'GlobalActor'-isolated requirement}}
      get { 0 }
      set {}
    }

    @MainActor func test() {
      // expected-note@-1 {{main actor-isolated instance method 'test()' cannot satisfy global actor 'GlobalActor'-isolated requirement}}
    }
  }
}

@MainActor
protocol WithNonIsolated {
  var prop: Int { get set }
  nonisolated func test()
}

do {
  // expected-warning@+1{{conformance of 'TestExplicitOtherIsolation' to protocol 'WithNonIsolated' involves isolation mismatches and can cause data races}}
  class TestExplicitOtherIsolation : @preconcurrency WithNonIsolated {
    // expected-warning@-1 {{'@preconcurrency' on conformance to 'WithNonIsolated' has no effect}}{{38-54=}}

    @GlobalActor var prop: Int = 42
    // expected-note@-1 {{global actor 'GlobalActor'-isolated property 'prop' cannot satisfy main actor-isolated requirement}}

    @MainActor func test() {}
    // expected-note@-1 {{main actor-isolated instance method 'test()' cannot satisfy nonisolated requirement}}
  }
}

do {
  class InferredGlobalActorAttrs : @preconcurrency WithNonIsolated {
    // expected-warning@-1 {{'@preconcurrency' on conformance to 'WithNonIsolated' has no effect}}{{36-52=}}
    var prop: Int = 42
    func test() {}
  }
}

// https://github.com/apple/swift/issues/74294
protocol Parent {
  func a()
}

protocol Child: Parent {
  func b()
}

do {
    actor Test: @preconcurrency Child {
      func a() {} // Ok
      func b() {} // Ok
    }
}

do {
  protocol P1 {}
  protocol P2 {}
  protocol P3: P1, P2 {}

  // expected-warning@+1 {{'@preconcurrency' on conformance to 'P3' has no effect}}
  @MainActor struct S: @preconcurrency P3 {}
}

// rdar://137794903
do {
  protocol P1 {}
  protocol P2 {
    func foo()
  }
  protocol P3: P1, P2 {}

  // OK, preconcurrency effectful because it is used by (implied) conformance
  // to inherited protocol 'P2'.
  @MainActor struct S1: @preconcurrency P3 {
    func foo() {}
  }
  // OK.
  @MainActor struct S2: @preconcurrency P2, P3 {
    func foo() {}
  }
  // OK.
  @MainActor struct S3: P3, @preconcurrency P2 {
    func foo() {}
  }

  // Explicit conformances to inherited protocols do not contribute to whether
  // preconcurrency has effect on the conformance to the refined protocol, so
  // preconcurrency has no effect here.
  // expected-warning@+1{{conformance of 'S4' to protocol 'P2' crosses into main actor-isolated code and can cause data races}}
  @MainActor struct S4: @preconcurrency P3, P2 {
    // expected-warning@-1:21 {{'@preconcurrency' on conformance to 'P3' has no effect}}
    // expected-note@-2:45 {{turn data races into runtime errors with '@preconcurrency'}}
    // expected-note@-3{{mark all declarations used in the conformance 'nonisolated'}}
    // expected-note@-4{{isolate this conformance to the main actor with '@MainActor'}}
    func foo() {}
    // expected-note@-1 {{main actor-isolated instance method 'foo()' cannot satisfy nonisolated requirement}}
  }
  // expected-warning@+1{{conformance of 'S5' to protocol 'P2' crosses into main actor-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  @MainActor struct S5: P2, @preconcurrency P3 {
    // expected-warning@-1:21 {{'@preconcurrency' on conformance to 'P3' has no effect}}
    // expected-note@-2:25 {{turn data races into runtime errors with '@preconcurrency'}}
    // expected-note@-3{{mark all declarations used in the conformance 'nonisolated'}}
    // expected-note@-4{{isolate this conformance to the main actor with '@MainActor'}}
    func foo() {}
    // expected-note@-1 {{main actor-isolated instance method 'foo()' cannot satisfy nonisolated requirement}}
  }
  // expected-warning@+1 {{'@preconcurrency' on conformance to 'P3' has no effect}}
  @MainActor struct S6: @preconcurrency P2, @preconcurrency P3 {
    func foo() {}
  }
}
do {
  protocol P1 {}
  protocol P2 {
    func foo()
  }
  protocol P3: P1, P2 {}
  protocol P4 {}

  // OK, preconcurrency effectful because it is used by implied conformance to
  // inherited protocol 'P2'.
  @MainActor struct S1: P4, @preconcurrency P3 {
    func foo() {}
  }
  @MainActor struct S2: @preconcurrency P3, P4 {
    func foo() {}
  }

  // Preconcurrency effectful for 'P3' only.
  @MainActor struct S3: @preconcurrency P3 & P4 {
  // expected-warning@-1:21 {{'@preconcurrency' on conformance to 'P4' has no effect}}
    func foo() {}
  }
}
do {
  protocol P1 {}
  protocol P2 {
    func foo()
  }
  protocol P3: P1, P2 {}
  protocol P5: P3 {}
  protocol P6: P3 {}

  // OK, preconcurrency effectful for both 'P5' and 'P6' because it is used
  // by implied conformance to mutually inherited protocol 'P2'.
  @MainActor struct S1: @preconcurrency P5 & P6 {
    func foo() {}
  }
  @MainActor struct S2: @preconcurrency P5, @preconcurrency P6 {
    func foo() {}
  }

  // OK, preconcurrency effectful because it is used by implied conformance to
  // inherited protocol 'P2'.
  @MainActor struct S3: @preconcurrency P5, P6 {
    func foo() {}
  }

  // expected-warning@+1{{conformance of 'S4' to protocol 'P2' crosses into main actor-isolated code and can cause data races}}
  @MainActor struct S4: P6, @preconcurrency P5 {
    // expected-warning@-1:21 {{'@preconcurrency' on conformance to 'P5' has no effect}}
    // expected-note@-2{{turn data races into runtime errors with '@preconcurrency'}}
    // expected-note@-3{{mark all declarations used in the conformance 'nonisolated'}}
    // expected-note@-4{{isolate this conformance to the main actor with '@MainActor'}}
    func foo() {}
    // expected-note@-1 {{main actor-isolated instance method 'foo()' cannot satisfy nonisolated requirement}}
  }
}

