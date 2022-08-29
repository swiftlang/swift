// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/57339

// CHECK: sr15009.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[P]A.[Q]B, Self.[P]A : Q>
protocol P { associatedtype A: Q where A.B == Self }

// CHECK: sr15009.(file).Q@
// CHECK-NEXT: Requirement signature: <Self where Self : CaseIterable, Self == Self.[Q]B.[P]A, Self.[Q]B : P>
protocol Q: CaseIterable { associatedtype B: P where B.A == Self }
