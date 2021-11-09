// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK-LABEL: identity_conformance.(file).P1@
// CHECK: Requirement signature: <Self where Self == Self.X>
protocol P1 {
  associatedtype X : P1 where X == Self
}

// CHECK-LABEL: identity_conformance.(file).P2@
// CHECK: Requirement signature: <Self where Self == Self.X>
protocol P2 {
  associatedtype X where X == Self
}
