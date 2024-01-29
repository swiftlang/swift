// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference
// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference

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