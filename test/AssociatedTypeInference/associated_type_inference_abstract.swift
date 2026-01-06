// RUN: %target-typecheck-verify-swift

protocol Q {}

extension Int: Q {}

///

protocol P1 {
  associatedtype A: Q
  associatedtype B: Q

  func f(_: A)
  func g(_: B)
}

extension P1 {
  func f(_: B) {}
  func g(_: A) {}
}

struct S1a: P1 {
  func f(_: Int) {}
}

struct S1b: P1 {
  func g(_: Int) {}
}

///

protocol P2 {
  associatedtype A: Q
  associatedtype B: Q

  func f(_: A)
  func g(_: B)
}

extension P2 where A == B {
  func f(_: B) {}
}

struct S2a: P2 {
  func g(_: Int) {}
}

extension P2 where A == B {
  func g(_: A) {}
}

struct S2b: P2 {
  func f(_: Int) {}
}

///

protocol P3 {
  associatedtype A: Q

  func f(_: A)
}

extension P3 {
  func f(_: Self) {}
}

struct S3: P3, Q {}

///

protocol P4 {
  associatedtype A: Q

  func f(_: A)
}

extension P4 where Self == A {
  func f(_: A) {}
}

struct S4: P4, Q {}

///

protocol P5 {
  associatedtype A: Q

  func f(_: A)
}

extension P5 where Self: Q {
  func f(_: Self) {}
}

struct S5: P5, Q {}