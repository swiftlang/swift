// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr15009.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.A.B, Self.A : Q>
protocol P { associatedtype A: Q where A.B == Self }

// CHECK: sr15009.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self : CaseIterable, Self == Self.B.A, Self.B : P>
protocol Q: CaseIterable { associatedtype B: P where B.A == Self }
