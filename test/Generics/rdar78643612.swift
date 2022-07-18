// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: 78643612.(file).Z1@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Z1]T>
protocol Z1 {
  associatedtype T where T == Self
}

// CHECK: 78643612.(file).Z2@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Z2]T.[Z2]T, Self.[Z2]T : Z2>
protocol Z2 {
  associatedtype T : Z2 where T.T == Self
}

// CHECK: 78643612.(file).Z3@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Z3]T.[Z3]T.[Z3]T, Self.[Z3]T : Z3>
protocol Z3 {
  associatedtype T : Z3 where T.T.T == Self
}

// CHECK: 78643612.(file).Z4@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Z4]T.[Z4]T.[Z4]T.[Z4]T, Self.[Z4]T : Z4>
protocol Z4 {
  associatedtype T : Z4 where T.T.T.T == Self
}

// CHECK: 78643612.(file).Z5@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Z5]T.[Z5]T.[Z5]T.[Z5]T.[Z5]T, Self.[Z5]T : Z5>
protocol Z5 {
  associatedtype T : Z5 where T.T.T.T.T == Self
}

// CHECK: 78643612.(file).Z6@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Z6]T.[Z6]T.[Z6]T.[Z6]T.[Z6]T.[Z6]T, Self.[Z6]T : Z6>
protocol Z6 {
  associatedtype T : Z6 where T.T.T.T.T.T == Self
}

