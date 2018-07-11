// RUN: %target-swift-emit-silgen -enable-sil-ownership -emit-sorted-sil -enable-objc-interop -disable-objc-attr-requires-foundation-module %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-sil-ownership -emit-sorted-sil -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-resilience %s | %FileCheck -check-prefix=CHECK-RESILIENT %s

@objc public enum CLike: Int {
  case a, b, c
}

// CHECK-LABEL: sil [serialized] @$S27enum_raw_representable_objc5CLikeO0B5ValueACSgSi_tcfC

// CHECK-LABEL: sil [serialized] @$S27enum_raw_representable_objc5CLikeO0B5ValueSivg
// CHECK-DAG: [[RESULT_BOX:%.+]] = alloc_stack $Int
// CHECK-DAG: [[INPUT_BOX:%.+]] = alloc_stack $CLike
// CHECK: [[RAW_TYPE:%.+]] = metatype $@thick Int.Type
// CHECK: [[CAST_FUNC:%.+]] = function_ref @$Ss13unsafeBitCast_2toq_x_q_mtr0_lF
// CHECK: = apply [[CAST_FUNC]]<CLike, Int>([[RESULT_BOX]], [[INPUT_BOX]], [[RAW_TYPE]])
// CHECK: [[RESULT:%.+]] = load [trivial] [[RESULT_BOX]]
// CHECK: return [[RESULT]]
// CHECK: end sil function '$S27enum_raw_representable_objc5CLikeO0B5ValueSivg'

// CHECK-RESILIENT-DAG: sil @$S27enum_raw_representable_objc5CLikeO0B5ValueSivg
// CHECK-RESILIENT-DAG: sil @$S27enum_raw_representable_objc5CLikeO0B5ValueACSgSi_tcfC
