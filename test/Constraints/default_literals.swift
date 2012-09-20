// RUN: %swift -verify -constraint-checker -debug-constraints %s > %t
// RUN: FileCheck %s < %t

// CHECK: ---Solution---
// CHECK: Assumptions:
// CHECK:     assuming $T0 == IntegerLiteralType
// CHECK: Type Variables:
// CHECK:   $T0 as IntegerLiteralType
// CHECK: Constraints:
// CHECK: SOLVED (completely)
var i1 = 1

// CHECK: SOLVED (completely)
// CHECK: Found 2 potential solutions.
// FIXME: There are multiple interpretations. Should we prefer the one that
// uses default types?
var i2 = 1 + 2.0 + 1 // expected-error{{expression does not type-check}}

