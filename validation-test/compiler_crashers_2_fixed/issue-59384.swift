// RUN: not %target-swift-frontend -emit-ir %s

protocol P1 {}
protocol Q1 {
  associatedtype A
  associatedtype B
}

protocol Q2: Q1 where B == S<Self>, B.C == Self {}

protocol P2: P1 {
  associatedtype C: Q2 where C.A == Void
}

struct S<C: Q2>: P2 {
}
