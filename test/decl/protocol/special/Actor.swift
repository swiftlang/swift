// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

// Synthesis of for actor classes.

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

// Explicitly satisfying the requirement.

actor class A7 {
  // Okay: satisfy the requirement explicitly
  @actorIndependent func enqueue(partialTask: PartialAsyncTask) { }
}

// A non-actor class can conform to the Actor protocol, if it does it properly.
class C1: Actor {
  func enqueue(partialTask: PartialAsyncTask) { }
}

// Bad actors, that incorrectly try to satisfy the various requirements.

// Method that is not usable as a witness.
actor class BA1 {
  func enqueue(partialTask: PartialAsyncTask) { } // expected-error{{actor-isolated instance method 'enqueue(partialTask:)' cannot be used to satisfy a protocol requirement}}
  //expected-note@-1{{add '@asyncHandler' to function 'enqueue(partialTask:)' to create an implicit asynchronous context}}{{3-3=@asyncHandler }}
  //expected-note@-2{{add '@actorIndependent' to 'enqueue(partialTask:)' to make this instance method independent of the actor}}{{3-3=@actorIndependent }}
}

// Method that isn't part of the main class definition cannot be used to
// satisfy the requirement, because then it would not have a vtable slot.
actor class BA2 { }

extension BA2 {
  // expected-error@+1{{'enqueue(partialTask:)' can only be implemented in the definition of actor class 'BA2'}}
  @actorIndependent func enqueue(partialTask: PartialAsyncTask) { }
}

// No synthesis for non-actor classes.
class C2: Actor { // expected-error{{type 'C2' does not conform to protocol 'Actor'}}
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
