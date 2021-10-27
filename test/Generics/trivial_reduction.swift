// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK-LABEL: trivial_reduction.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C, Self.C : P1, Self.C == Self.R>
protocol P1 {
  associatedtype R : P1 where R.R == Self
  associatedtype C : P1 where C.C == Self, C.R == Self, R.C.R.C == Self
}

// CHECK-LABEL: trivial_reduction.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C, Self.C == Self.R>
protocol P2 {
  associatedtype R : P2 where R.R == Self
  associatedtype C : P2 where C.C.C == Self, C.C.R.C.C.R == Self, R.C.R.C.R.C == Self
}
