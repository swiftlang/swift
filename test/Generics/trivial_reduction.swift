// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: trivial_reduction.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[P1]C.[P1]C, Self.[P1]C : P1, Self.[P1]C == Self.[P1]R>
protocol P1 {
  associatedtype R : P1 where R.R == Self
  associatedtype C : P1 where C.C == Self, C.R == Self, R.C.R.C == Self
}

// CHECK-LABEL: trivial_reduction.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[P2]C, Self.[P2]C == Self.[P2]R>
protocol P2 {
  associatedtype R : P2 where R.R == Self
  associatedtype C : P2 where C.C.C == Self, C.C.R.C.C.R == Self, R.C.R.C.R.C == Self
}
