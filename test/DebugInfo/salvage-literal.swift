// RUN: %target-swift-frontend -O -g -emit-sil %s | %FileCheck %s

// In optimized code, a + b will be folded to 5, but we should still keep their
// debug values.

// CHECK-LABEL: sil @$s4main1fSiyF
public func f() -> Int {
  let a = 2
  let b = 3
  // CHECK: debug_value (), let, name "a", transform {
  // CHECK:   %0 = integer_literal $Builtin.Int{{32|64}}, 2
  // CHECK:   %1 = struct $Int (%0)
  // CHECK:   return %1
  // CHECK: }
  // CHECK: debug_value (), let, name "b", transform {
  // CHECK:   %0 = integer_literal $Builtin.Int{{32|64}}, 3
  // CHECK:   %1 = struct $Int (%0)
  // CHECK:   return %1
  // CHECK: }
  return a + b
}

// CHECK-LABEL: sil @$s4main1gSdyF
public func g() -> Double {
  let a = 2.0
  let b = 3.0
  // CHECK: debug_value (), let, name "a", transform {
  // CHECK:   %0 = float_literal $Builtin.FPIEEE64, 0x4000000000000000
  // CHECK:   %1 = struct $Double (%0)
  // CHECK:   return %1
  // CHECK: }
  // CHECK: debug_value (), let, name "b", transform {
  // CHECK:   %0 = float_literal $Builtin.FPIEEE64, 0x4008000000000000
  // CHECK:   %1 = struct $Double (%0)
  // CHECK:   return %1
  // CHECK: }
  return a + b
}
