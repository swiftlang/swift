// RUN: %target-swift-frontend -O -g -emit-sil %s | %FileCheck %s

// In optimized code, a + b will be folded to 5, but we should still keep their
// debug values.

// CHECK-LABEL: sil @$s4main1fSiyF
public func f() -> Int {
  let a = 2
  let b = 3
  // CHECK: debug_value undef : $Builtin.Int{{32|64}}, let, name "a", type $Int, expr op_fragment:#Int._value, transform {
  // CHECK:   %0 = integer_literal $Builtin.Int{{32|64}}, 2
  // CHECK:   return %0
  // CHECK: }
  // CHECK: debug_value undef : $Builtin.Int{{32|64}}, let, name "b", type $Int, expr op_fragment:#Int._value, transform {
  // CHECK:   %0 = integer_literal $Builtin.Int{{32|64}}, 3
  // CHECK:   return %0
  // CHECK: }
  return a + b
}

// CHECK-LABEL: sil @$s4main1gSdyF
public func g() -> Double {
  let a = 2.0
  let b = 3.0
  // CHECK: debug_value undef : $Builtin.FPIEEE64, let, name "a", type $Double, expr op_fragment:#Double._value, transform {
  // CHECK:   %0 = float_literal $Builtin.FPIEEE64, 0x4000000000000000
  // CHECK:   return %0
  // CHECK: }
  // CHECK: debug_value undef : $Builtin.FPIEEE64, let, name "b", type $Double, expr op_fragment:#Double._value, transform {
  // CHECK:   %0 = float_literal $Builtin.FPIEEE64, 0x4008000000000000
  // CHECK:   return %0
  // CHECK: }
  return a + b
}
