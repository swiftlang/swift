// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// Synthesis of for actor classes.
import _Concurrency

actor class A1 {
  var x: Int = 17
}

actor class A2: Actor {
  var x: Int = 17
}

actor class A3<T>: Actor {
  var x: Int = 17
}

actor class A4: A1 {
}

actor class A5: A2 {
}

actor class A6: A1, Actor { // expected-error{{redundant conformance of 'A6' to protocol 'Actor'}}
  // expected-note@-1{{'A6' inherits conformance to protocol 'Actor' from superclass here}}
}

actor class A7 {
  // Okay: satisfy the requirement explicitly
  @actorIndependent func enqueue(partialTask: PartialAsyncTask) { }
}

// Bad actors, that incorrectly try to satisfy the various requirements.

// Method that is not usable as a witness.
actor class BA1 {
  func enqueue(partialTask: PartialAsyncTask) { } // expected-error{{invalid redeclaration of synthesized implementation for protocol requirement 'enqueue(partialTask:)'}}
}

// Method that isn't part of the main class definition cannot be used to
// satisfy the requirement, because then it would not have a vtable slot.
actor class BA2 { }

extension BA2 {
  // expected-error@+1{{'enqueue(partialTask:)' can only be implemented in the definition of actor class 'BA2'}}
  @actorIndependent func enqueue(partialTask: PartialAsyncTask) { }
}


// Make sure the conformances actually happen.
func acceptActor<T: Actor>(_: T.Type) { }

func testConformance() {
  acceptActor(A1.self)
  acceptActor(A2.self)
  acceptActor(A3<Int>.self)
  acceptActor(A4.self)
  acceptActor(A5.self)
  acceptActor(A6.self)
  acceptActor(A7.self)
}
