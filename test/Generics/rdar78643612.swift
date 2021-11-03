// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: 78643612.(file).Z1@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.T>
protocol Z1 {
  associatedtype T where T == Self
}

// CHECK: 78643612.(file).Z2@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.T.T, Self.T : Z2>
protocol Z2 {
  associatedtype T : Z2 where T.T == Self
}

// CHECK: 78643612.(file).Z3@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.T.T.T, Self.T : Z3>
protocol Z3 {
  associatedtype T : Z3 where T.T.T == Self
}

// CHECK: 78643612.(file).Z4@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.T.T.T.T, Self.T : Z4>
protocol Z4 {
  associatedtype T : Z4 where T.T.T.T == Self
}

// CHECK: 78643612.(file).Z5@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.T.T.T.T.T, Self.T : Z5>
protocol Z5 {
  associatedtype T : Z5 where T.T.T.T.T == Self
}

// CHECK: 78643612.(file).Z6@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.T.T.T.T.T.T, Self.T : Z6>
protocol Z6 {
  associatedtype T : Z6 where T.T.T.T.T.T == Self
}

