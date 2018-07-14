// RUN: %target-typecheck-verify-swift

protocol P0 {
    associatedtype A
}

protocol P1 {
  associatedtype B : P3 = S0<S2>
  associatedtype C = ()
}

protocol P2 {
  associatedtype D : P1
  associatedtype E : P3 = S0<S2>
}

protocol P3 : P0 where A : P2 {}

struct S0<T> : P0 {
    typealias A = T
}

extension S0 : P3 where T : P2 {}

struct S2 : P2 {
  struct D : P1 {
    let value: S2
  }
}

extension P1 where C : P2 {
  typealias B = C.E
}

extension P3 {
  func foo() {
    _ = A.D.B.self
  }
}
