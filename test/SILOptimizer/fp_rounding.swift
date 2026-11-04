// RUN: %target-swift-frontend -parse-as-library -O -Xllvm -sil-print-types -emit-sil  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer can propagate
// resilient enum cases (FloatingPointRoundingRule) and produces optimal
// code for Float.rounded().

// CHECK-LABEL: sil @{{.*}}propagate_roundingmode
// CHECK:      bb0:
// CHECK:        [[ZERO:%[0-9]+]] = integer_literal {{.*}}, 0
// CHECK-NEXT:   [[INT:%[0-9]+]] = struct $Int ([[ZERO]] {{.*}})
// CHECK-NEXT:   return [[INT]]
public func propagate_roundingmode() -> Int {
  let rm = FloatingPointRoundingRule.toNearestOrEven
  switch rm {
    case .toNearestOrAwayFromZero:
      return 1
    default:
      return 0
  }
}

// CHECK-LABEL: sil @{{.*}}round_floating_point
// CHECK: bb0({{.*}}):
// CHECK:   [[R:%[0-9]+]] = builtin "int_round{{.*}}"
// CHECK:   [[F:%[0-9]+]] = struct $Float ([[R]]
// CHECK:   return [[F]]
public func round_floating_point(_ x: Float) -> Float {
  return x.rounded()
}
