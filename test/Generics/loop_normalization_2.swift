// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype A
}

protocol P2 : P1 where A == S0 {
  associatedtype B : P2 = S2
    where B.A == A, B.B == B
}

struct S0 {}

struct S1 : P2 {}

struct S2 : P2 {}

// CHECK-LABEL: .Q1@
// CHECK-NEXT: Requirement signature: <Self where Self.[Q1]T == S1>
protocol Q1 {
  associatedtype T where T : P2, T == S1
}

// CHECK-LABEL: .Q2@
// CHECK-NEXT: Requirement signature: <Self where Self.[Q2]T == S2>
protocol Q2 {
  associatedtype T where T : P2, T == S2
}
