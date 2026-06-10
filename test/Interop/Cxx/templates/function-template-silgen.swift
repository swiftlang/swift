// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import FunctionTemplates

// CHECK-LABEL: sil @$s4main4test1xs5Int32VAE_tF

// CHECK: bb0(%0 : $Int32):
// CHECK:   [[IL_ZERO:%.*]] = integer_literal $Builtin.Int32, 0
// CHECK:   [[ZERO:%.*]] = struct $Int32 ([[IL_ZERO]] : $Builtin.Int32)
// CHECK:   [[PASS_THROUGH_CONST_FN:%.*]] = function_ref @$sSo16passThroughConstys5Int32VACFTo : $@convention(c) (Int32) -> Int32
// CHECK:   [[A:%.*]] = apply [[PASS_THROUGH_CONST_FN]]([[ZERO]]) : $@convention(c) (Int32) -> Int32

// CHECK:   [[PASS_THROUGH_FN:%.*]] = function_ref @$sSo11passThroughys5Int32VACFTo : $@convention(c) (Int32) -> Int32
// CHECK:   [[B:%.*]] = apply [[PASS_THROUGH_FN]](%0) : $@convention(c) (Int32) -> Int32

// CHECK:   [[ADD_TWO_FN:%.*]] = function_ref @$sSo18addMixedTypeParamsys5Int32VAC_ACtFTo : $@convention(c) (Int32, Int32) -> Int32
// CHECK:   [[C:%.*]] = apply [[ADD_TWO_FN]]([[A]], [[B]]) : $@convention(c) (Int32, Int32) -> Int32

// CHECK:   [[ADD_FN:%.*]] = function_ref @$sSo17addSameTypeParamsys5Int32VAC_ACtFTo : $@convention(c) (Int32, Int32) -> Int32
// CHECK:   [[OUT:%.*]] = apply [[ADD_FN]]([[B]], [[C_32:%.*]]) : $@convention(c) (Int32, Int32) -> Int32
// CHECK:   return [[OUT]] : $Int32

// CHECK-LABEL: end sil function '$s4main4test1xs5Int32VAE_tF'
public func test(x: Int32) -> Int32 {
  let a = passThroughConst(Int32(0))
  let b = passThrough(x)
  let c = addMixedTypeParams(a, b)
  return addSameTypeParams(b, Int32(c))
}
