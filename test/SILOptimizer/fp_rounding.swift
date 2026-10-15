// RUN: %target-swift-frontend -parse-as-library -O -Xllvm -sil-print-types -emit-sil  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer can propagate
// resilient enum cases (FloatingPointRoundingRule) and produces optimal
// code for Float.rounded(), including when an explicit rounding rule is passed.

// CHECK-LABEL: sil @{{.*}}propagate_roundingmode
// CHECK:      bb0:
// CHECK-NEXT:   %0 = integer_literal {{.*}}, 0
// CHECK-NEXT:   %1 = struct $Int (%0 {{.*}})
// CHECK-NEXT:   return %1
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

// CHECK-LABEL: sil @{{.*}}round_up
// CHECK: bb0({{.*}}):
// CHECK:   [[R:%[0-9]+]] = builtin "int_ceil{{.*}}"
// CHECK:   [[F:%[0-9]+]] = struct $Double ([[R]]
// CHECK:   return [[F]]
public func round_up(_ x: Double) -> Double {
  return x.rounded(.up)
}

// CHECK-LABEL: sil @{{.*}}round_down
// CHECK: bb0({{.*}}):
// CHECK:   [[R:%[0-9]+]] = builtin "int_floor{{.*}}"
// CHECK:   [[F:%[0-9]+]] = struct $Double ([[R]]
// CHECK:   return [[F]]
public func round_down(_ x: Double) -> Double {
  return x.rounded(.down)
}

// CHECK-LABEL: sil @{{.*}}round_towardZero
// CHECK: bb0({{.*}}):
// CHECK:   [[R:%[0-9]+]] = builtin "int_trunc{{.*}}"
// CHECK:   [[F:%[0-9]+]] = struct $Double ([[R]]
// CHECK:   return [[F]]
public func round_towardZero(_ x: Double) -> Double {
  return x.rounded(.towardZero)
}
