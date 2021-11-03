// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype A
}

protocol P2 {
  associatedtype C: P1
}

// CHECK: sr11100.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self.X == Self.X.A, Self.Y : P2, Self.X.A == Self.Y.C>
protocol Q {
  associatedtype X
  associatedtype Y : P2 where X == X.A, X.A == Y.C
}
