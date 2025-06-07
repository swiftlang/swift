// RUN: %target-typecheck-verify-swift 
// REQUIRES: concurrency

// Synthesis of conformances for actors.

@available(SwiftStdlib 5.1, *)
actor A1 {
  var x: Int = 17
}

@available(SwiftStdlib 5.1, *)
actor A2: Actor {
  var x: Int = 17
}

@available(SwiftStdlib 5.1, *)
actor A3<T>: Actor {
  var x: Int = 17
}

@available(SwiftStdlib 5.1, *)
actor A4: A1 { // expected-error{{actor types do not support inheritance}}
}

@available(SwiftStdlib 5.1, *)
actor A5: A2 { // expected-error{{actor types do not support inheritance}}
}

@available(SwiftStdlib 5.1, *)
actor A6: A1, Actor { // expected-error{{redundant conformance of 'A6' to protocol 'Actor'}}
  // expected-note@-1{{'A6' inherits conformance to protocol 'Actor' from superclass here}}
  // expected-error@-2{{actor types do not support inheritance}}
}

// Explicitly satisfying the requirement.

@available(SwiftStdlib 5.1, *)
actor A7 {
  // Okay: satisfy the requirement explicitly
  nonisolated func enqueue(_ job: UnownedJob) { }
}

// A non-actor can conform to the Actor protocol, if it does it properly.
@available(SwiftStdlib 5.1, *)
class C1: Actor {
  // expected-error@-1{{non-actor type 'C1' cannot conform to the 'Actor' protocol}}
  // expected-warning@-2{{non-final class 'C1' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    fatalError("")
  }
}

@available(SwiftStdlib 5.1, *)
class C2: Actor {
  // expected-error@-1{{non-actor type 'C2' cannot conform to the 'Actor' protocol}}
// expected-warning@-2{{non-final class 'C2' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  // FIXME: this should be an isolation violation
  var unownedExecutor: UnownedSerialExecutor {
    fatalError("")
  }
}

@available(SwiftStdlib 5.1, *)
class C3: Actor {
  // expected-error@-1{{type 'C3' does not conform to protocol 'Actor'}}
  // expected-note@-2{{add stubs for conformance}}
  // expected-error@-3{{non-actor type 'C3' cannot conform to the 'Actor' protocol}}
  // expected-warning@-4{{non-final class 'C3' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  nonisolated func enqueue(_ job: UnownedJob) { }
}

// Make sure the conformances actually happen.
@available(SwiftStdlib 5.1, *)
func acceptActor<T: Actor>(_: T.Type) { }

@available(SwiftStdlib 5.1, *)
func testConformance() {
  acceptActor(A1.self)
  acceptActor(A2.self)
  acceptActor(A3<Int>.self)
  acceptActor(A4.self)
  acceptActor(A5.self)
  acceptActor(A6.self)
  acceptActor(A7.self)
}
