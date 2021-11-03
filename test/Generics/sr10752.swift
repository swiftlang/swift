// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr10752.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self.A : P, Self.A == Self.A.A>
protocol P {
  associatedtype A : P where A.A == A
}

// CHECK: sr10752.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self.A == Self.C.A, Self.C : P>
protocol Q {
  associatedtype A : P
  associatedtype C : P where A == C.A
}
