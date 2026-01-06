// RUN: %target-typecheck-verify-swift

// While we disallow explicitly-stated requirements like
// `T: C<T>`, there are ways to sneak them past the
// diagnostic.
//
// We could relax the diagnostic eventually by rejecting
// the truly invalid cases, where the superclass contains
// a member type of a conformance made redundant by the
// superclass requirement itself.

class C<T> {
  var t: T

  init(t: T) {
    self.t = t
  }
}

protocol P1 {
  associatedtype A: C<B.A>
  associatedtype B: P1
}

func f<T: P1>(_: T, t: T.A) -> C<T.B.B.B.B.A> {
  return t.t.t.t
}

protocol P2 {
  associatedtype A: C<B>
  associatedtype B
}

extension P2 where Self == A {
  func f() -> C<B> {
    return self
  }
}

extension P2 where Self == A, A == B {
  func g() -> C<Self> {
    return self
  }
}
