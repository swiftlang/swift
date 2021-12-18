// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// The GSB crashes with P1 and P2, and rejects P3.

// CHECK-LABEL: confluent_example.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self.A : P1, Self.A == Self.A.B, Self.B : P1, Self.B == Self.B.C, Self.C : P1>
protocol P1 {
  associatedtype A : P1
  associatedtype B : P1
  associatedtype C : P1 where A.B == A, B.C == B
}

// CHECK-LABEL: confluent_example.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.A : P2, Self.A == Self.A.B, Self.B : P2, Self.B == Self.B.C, Self.C : P2>
protocol P2 {
  associatedtype A : P2
  associatedtype B : P2
  associatedtype C : P2 where A.B == A, B.C == B, A.C == A
}

// CHECK-LABEL: confluent_example.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self.A : P3, Self.A == Self.A.B, Self.B : P3, Self.B == Self.B.C, Self.C : P3>
protocol P3 {
  associatedtype A where A.B : P3
  associatedtype B where B.C : P3
  associatedtype C : P3 where A.B == A, B.C == B, A.C == A
}
