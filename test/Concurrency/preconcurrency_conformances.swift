// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -enable-experimental-feature PreconcurrencyConformances -strict-concurrency=complete -verify-additional-prefix complete-tns-

// REQUIRES: concurrency
// REQUIRES: asserts

protocol Q {
}

do {
  class K {}

  struct A : @preconcurrency Q {} // Ok
  struct B : @preconcurrency K {
    // expected-error@-1 {{'preconcurrency' attribute cannot apply to non-protocol type 'K'}}
    var x: @preconcurrency Int
    // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}
  }

  typealias T = @preconcurrency Q
  // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}

  func test(_: @preconcurrency K) {}
  // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}
}

protocol InvalidUseOfPreconcurrencyAttr : @preconcurrency Q {
  // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}
}

struct TestPreconcurrencyAttr {}
extension TestPreconcurrencyAttr : @preconcurrency Q { // Ok
}

class NonSendable {}
// expected-note@-1 6 {{class 'NonSendable' does not conform to the 'Sendable' protocol}}

protocol TestSendability {
  var x: NonSendable { get }
  func test(_: NonSendable?) -> [NonSendable]
}

// Make sure that preconcurrency conformances don't suppress Sendable diagnostics
@MainActor
struct Value : @preconcurrency TestSendability {
  var x: NonSendable { NonSendable() }
  // expected-warning@-1 {{non-sendable type 'NonSendable' in conformance of main actor-isolated property 'x' to protocol requirement cannot cross actor boundary}}
  // expected-note@-2 2 {{property declared here}}

  // expected-warning@+2 {{non-sendable type '[NonSendable]' returned by main actor-isolated instance method 'test' satisfying protocol requirement cannot cross actor boundary}}
  // expected-warning@+1 {{non-sendable type 'NonSendable?' in parameter of the protocol requirement satisfied by main actor-isolated instance method 'test' cannot cross actor boundary}}
  func test(_: NonSendable?) -> [NonSendable] {
    // expected-note@-1 2 {{calls to instance method 'test' from outside of its actor context are implicitly asynchronous}}
    []
  }
}

// Make sure that references to actor isolated witness is diagnosed

// expected-note@+1 2 {{add '@MainActor' to make global function 'test(value:)' part of global actor 'MainActor'}}
func test(value: Value) {
  _ = value.x
  // expected-error@-1 {{main actor-isolated property 'x' can not be referenced from a non-isolated context}}
  _ = value.test(nil)
  // expected-error@-1 {{call to main actor-isolated instance method 'test' in a synchronous nonisolated context}}
}

actor MyActor {
  var value: Value? = nil
}

extension MyActor : @preconcurrency TestSendability {
  var x: NonSendable { NonSendable() }
  // expected-warning@-1 {{non-sendable type 'NonSendable' in conformance of actor-isolated property 'x' to protocol requirement cannot cross actor boundary}}

  // expected-warning@+2 {{non-sendable type '[NonSendable]' returned by actor-isolated instance method 'test' satisfying protocol requirement cannot cross actor boundary}}
  // expected-warning@+1 {{non-sendable type 'NonSendable?' in parameter of the protocol requirement satisfied by actor-isolated instance method 'test' cannot cross actor boundary}}
  func test(_: NonSendable?) -> [NonSendable] {
    []
  }

  func test_ref_diagnostics() {
    _ = value?.x
    // expected-error@-1 {{main actor-isolated property 'x' can not be referenced on a non-isolated actor instance}}
    _ = value?.test(nil)
    // expected-error@-1 {{call to main actor-isolated instance method 'test' in a synchronous actor-isolated context}}
  }
}

protocol Initializable {
  init()
}

final class K : @preconcurrency Initializable {
  init() {} // Ok
}

protocol WithAssoc {
  associatedtype T
  func test() -> T
}

struct TestConditional<T> {}

extension TestConditional : @preconcurrency WithAssoc where T == Int {
  @MainActor func test() -> T { 42 } // Ok
}
