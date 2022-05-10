// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

class C<T> {}

protocol P0 {}

protocol P1 {
  associatedtype T where T == C<U> & P0
  associatedtype U
}

protocol P2 {
  associatedtype T where T == C<Int> & P0
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3]T : P1, Self.[P3]T : P2>
protocol P3 {
  associatedtype T : P1 & P2 where T.U == Int
}