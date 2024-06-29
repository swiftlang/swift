// RUN: %target-typecheck-verify-swift

// We can't infer anything from 'Self', but we must still validate the
// binding.

protocol P1 {
  associatedtype A

  func f(_: Self, _: A)
}

struct S1: P1 {
  // Don't consider this f(); A := Float is not a candidate type witness.
  func f(_: Int, _: Float) {}

  func f(_: S1, _: String) {}
}

let x: String.Type = S1.A.self
