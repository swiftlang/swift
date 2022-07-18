// RUN: %target-typecheck-verify-swift -debug-generic-signatures -warn-redundant-requirements 2>&1 | %FileCheck %s

// CHECK: sr10752.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self.[P]A : P, Self.[P]A == Self.[P]A.[P]A>
protocol P {
  associatedtype A : P where A.A == A
}

// CHECK: sr10752.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self.[Q]A == Self.[Q]C.[P]A, Self.[Q]C : P>
protocol Q {
  associatedtype A : P // expected-warning {{redundant conformance constraint 'Self.A' : 'P'}}
  associatedtype C : P where A == C.A
}
