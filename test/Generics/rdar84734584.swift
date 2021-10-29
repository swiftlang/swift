// RUN: %target-typecheck-verify-swift

protocol P1 {
  associatedtype T1
}

protocol P2 {
  associatedtype T2 : P1
}

struct S1 : P1 {
  typealias T1 = S2
}

struct S2 {}

func foo<X1: P2, X2: P1>(_: X1, _: X2)
    where X2.T1 == S2 { }

func bar<X1: P2, X2: P1>(x: X1, u: X2, uu: X2.T1)
    where X1.T2 == S1, X2.T1: P1, X2.T1.T1 == S2 {
  // this call should type-check successfully
  foo(x, uu)

  // so should this
  let _: S2.Type = X2.T1.T1.self
  let _: S2.Type = X1.T2.T1.self
}
