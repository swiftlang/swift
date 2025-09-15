// RUN: %target-typecheck-verify-swift

// A := G<A> is unsatisfiable and not a tautology!

protocol P1 {
  associatedtype A
  associatedtype B

  func f(_: A, _: B)
  func g(_: A, _: B)
}

struct G<T> {}

extension P1 {
  // These two are not candidate witnesses at all!
  func f(_: G<A>, _: Int) {}
  func g(_: Float, _: G<B>) {}

  // We can infer A and B from these two:
  func f(_: A, _: String) {}
  func g(_: String, _: B) {}
}

struct S1: P1 {}

let x1: String.Type = S1.A.self
let y1: String.Type = S1.B.self

// Potential witness has innermost generic parameters.

protocol P2 {
  associatedtype A
  associatedtype B

  func f(_: A, _: B)
  func g(_: A, _: B)
}

extension P2 {
  // These two are not candidate witnesses at all!
  func f<T>(_: G<T>, _: Int) {}
  func g<T>(_: Float, _: G<T>) {}

  // We can infer A and B from these two:
  func f<T>(_: T, _: String) {}
  func g<T>(_: String, _: T) {}
}

struct S2: P2 {}

let x2: String.Type = S2.A.self
let y2: String.Type = S2.B.self

// If all type witness bindings were tautological, we must still consider the
// witness as introducing no bindings.

protocol P3 {
  associatedtype A

  func f(_: A)
  func g(_: A)
}

extension P3 {
  func g(_: A) {}

  // We should not be forced to choose g().
  func g(_: String) {}
}

struct S3: P3 {
  func f(_: Int) {}
}

let x3: Int.Type = S3.A.self