// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T
}

protocol P2 {
  associatedtype T : P3
}

protocol P3 {}

struct G<T : P3> : P2 {}

extension P1 where T : P2 {
  // CHECK-LABEL: .foo@
  // CHECK-NEXT: <Self, X where Self : P1, X : P3, Self.[P1]T == G<X>>
  func foo<X>(_: X) where T == G<X> {}
}

