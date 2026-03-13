// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A: Q = Int

  func f(_: A)
}

protocol Q {}

extension Int: Q {}
extension String: Q {}

struct S: P {
  // The presence of this overload where Float does not conform to Q would confuse
  // the solver and it would find two ambiguous solutions, A := Int and A := String.
  func f(_: Float) {}
  func f(_: String) {}
}

let x: String.Type = S.A.self
