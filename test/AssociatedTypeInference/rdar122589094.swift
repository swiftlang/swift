// RUN: %target-typecheck-verify-swift

protocol P1 {
  associatedtype A

  func f1(_: C) -> A
  func f2(_: A, _: C)

  typealias C = S1<Self>
}

struct S1<T> {}

protocol P2: P1 where A == B {
  associatedtype B

  func g1(_: C) -> B
  func g2(_: B, _: C)
}

extension P2 {
  func f1(_: C) -> B { fatalError() }
  func f2(_: B, _: C) { fatalError() }
}

extension P2 {
  func g2(_: B, _: C) {}
}

struct S2: P2 {
  func g1(_: C) -> Int {
    fatalError()
  }
}
