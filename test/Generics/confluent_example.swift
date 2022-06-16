// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// The GSB crashes with P1 and P2, and rejects P3.

// CHECK-LABEL: confluent_example.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self.[P1]A : P1, Self.[P1]A == Self.[P1]A.[P1]B, Self.[P1]B : P1, Self.[P1]B == Self.[P1]B.[P1]C, Self.[P1]C : P1>
protocol P1 {
  associatedtype A : P1
  associatedtype B : P1
  associatedtype C : P1 where A.B == A, B.C == B
}

// CHECK-LABEL: confluent_example.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.[P2]A : P2, Self.[P2]A == Self.[P2]A.[P2]B, Self.[P2]B : P2, Self.[P2]B == Self.[P2]B.[P2]C, Self.[P2]C : P2>
protocol P2 {
  associatedtype A : P2
  associatedtype B : P2
  associatedtype C : P2 where A.B == A, B.C == B, A.C == A
}

// CHECK-LABEL: confluent_example.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self.[P3]A : P3, Self.[P3]A == Self.[P3]A.[P3]B, Self.[P3]B : P3, Self.[P3]B == Self.[P3]B.[P3]C, Self.[P3]C : P3>
protocol P3 {
  associatedtype A where A.B : P3
  associatedtype B where B.C : P3
  associatedtype C : P3 where A.B == A, B.C == B, A.C == A
}
