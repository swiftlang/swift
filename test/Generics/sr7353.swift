// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK-LABEL: sr7353.(file).P@
// CHECK-LABEL: <Self where Self == Self.A.B, Self.A : Q>
protocol P {
  associatedtype A: Q where A.B == Self
}

// CHECK-LABEL: sr7353.(file).Q@
// CHECK-LABEL: <Self where Self == Self.B.A, Self.B : P, Self.B == Self.C>
protocol Q {
  associatedtype B: P where B.A == Self
  associatedtype C: P where C.A == Self
}
