// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -warn-concurrency
// REQUIRES: concurrency

actor A {
  func f() { } // expected-note{{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}
}

extension Actor {
  func g() { }  // expected-note 2{{calls to instance method 'g()' from outside of its actor context are implicitly asynchronous}}
}

func testA<T: Actor>(
  a: isolated A,
  b: isolated T,
  c: isolated Int // expected-error{{'isolated' parameter has non-actor type 'Int'}}
) { 
  a.f() // FIXME: expected-error{{actor-isolated instance method 'f()' can only be referenced from inside the actor}}
  a.g() // FIXME: expected-error{{actor-isolated instance method 'g()' can only be referenced from inside the actor}}
  b.g() // FIXME: expected-error{{actor-isolated instance method 'g()' can only be referenced from inside the actor}}
}
