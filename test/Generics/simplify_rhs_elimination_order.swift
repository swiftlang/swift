// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

public protocol P1 {
  associatedtype T
}

public protocol P2 : P1 {}

public protocol P3 : P2 {}

public protocol Q1 {}

public protocol Q2 : Q1 {}

public protocol Q3 : Q1 {}

public protocol R {
  associatedtype U: Q2, Q3
}

struct G<X: P3, Y: R> where X.T == Y.U {
  // CHECK-LABEL: .G.Nested0@
  // CHECK-NEXT: <X, Y, Z where X : P3, Y : R, X.[P1]T == Y.[R]U>
  struct Nested0<Z> {}

  // CHECK-LABEL: .G.Nested1@
  // CHECK-NEXT: <X, Y, Z where X : P3, Y : R, X.[P1]T == Y.[R]U>
  struct Nested1<Z> where X.T : Q1 {}

  // CHECK-LABEL: .G.Nested2@
  // CHECK-NEXT: <X, Y, Z where X : P3, Y : R, X.[P1]T == Y.[R]U>
  struct Nested2<Z> where X.T : Q2 {}

  // CHECK-LABEL: .G.Nested3@
  // CHECK-NEXT: <X, Y, Z where X : P3, Y : R, X.[P1]T == Y.[R]U>
  struct Nested3<Z> where X.T : Q3 {}
}
