// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/53142

// CHECK-LABEL: .P@
// CHECK-NEXT: Requirement signature: <Self where Self.[P]A : P, Self.[P]A == Self.[P]A.[P]A>
protocol P {
  associatedtype A : P where A.A == A
}

// CHECK-LABEL: .Q@
// CHECK-NEXT: Requirement signature: <Self where Self.[Q]A == Self.[Q]C.[P]A, Self.[Q]C : P>
protocol Q {
  associatedtype A : P
  associatedtype C : P where A == C.A
}
