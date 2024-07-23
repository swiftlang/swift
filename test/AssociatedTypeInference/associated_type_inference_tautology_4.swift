// RUN: %target-typecheck-verify-swift

// An inner generic parameter of the protocol requirement should not
// match against a concrete type in the witness.

protocol P2 {
  associatedtype A
  func f<T>(_: T, _: A)
}

struct S2<A>: P2 {
  // These are not candiate witnesses for the requirement.
  func f(_: Float, _: Array<A>) {}
  func f(_: String, _: Array<A>) {}

  func f<T>(_: T, _: A) {}
}

let x: Int.Type = S2<Int>.A.self
