// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

struct G<T> {}

protocol P1 {
  associatedtype X where X == G<Y>
  associatedtype Y
}

// CHECK-LABEL: concrete_redundancy_via_adjustment.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]T : P1>
protocol P2 {
  associatedtype T : P1 where T.X == G<T.Y>
}
