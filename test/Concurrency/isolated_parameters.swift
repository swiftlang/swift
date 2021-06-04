// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -warn-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
actor A {
  func f() { } // expected-note{{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
}

@available(SwiftStdlib 5.5, *)
extension Actor {
  func g() { }  // expected-note 2{{calls to instance method 'g()' from outside of its actor context are implicitly asynchronous}}
}

@available(SwiftStdlib 5.5, *)
func testA<T: Actor>(
  a: isolated A,
  b: isolated T,
  c: isolated Int // expected-error{{'isolated' parameter has non-actor type 'Int'}}
) { 
  a.f() // FIXME: expected-error{{actor-isolated instance method 'f()' can only be referenced from inside the actor}}
  a.g() // FIXME: expected-error{{actor-isolated instance method 'g()' can only be referenced from inside the actor}}
  b.g() // FIXME: expected-error{{actor-isolated instance method 'g()' can only be referenced from inside the actor}}
}

@available(SwiftStdlib 5.5, *)
typealias Fn = (isolated A) -> Void

@available(SwiftStdlib 5.5, *)
func globalFunc(_: A) { }

@available(SwiftStdlib 5.5, *)
func globalFuncIsolated(_: isolated A) {
  let _: Int = globalFuncIsolated // expected-error{{cannot convert value of type '(isolated A) -> ()' to specified type 'Int'}}
  let _: (A) -> Void = globalFuncIsolated // expected-error{{cannot convert value of type '(isolated A) -> ()' to specified type '(A) -> Void'}}
  let _: Fn = globalFunc // okay
}
