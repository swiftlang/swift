// RUN: %target-swift-frontend -O -g -emit-sil %s | %FileCheck %s

// In optimized code, a + b will be folded to 5, but we should still keep their
// debug values.

// CHECK-LABEL: sil
public func f() -> Int {
  let a = 2
  let b = 3
  // CHECK: debug_value undef : $Builtin.Int{{32|64}}, let, name "a", type $Int, expr op_constu:2:op_fragment:#Int._value
  // CHECK: debug_value undef : $Builtin.Int{{32|64}}, let, name "b", type $Int, expr op_constu:3:op_fragment:#Int._value
  return a + b
}
