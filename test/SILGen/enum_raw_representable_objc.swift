// RUN: %target-swift-emit-silgen -emit-sorted-sil %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -emit-sorted-sil -enable-library-evolution %s | %FileCheck -check-prefix=CHECK-RESILIENT %s

#if os(Windows) && (arch(x86_64) || arch(arm64))
@objc public enum CLike: Int32 {
  case a, b, c
}
#else
@objc public enum CLike: Int {
  case a, b, c
}
#endif

// CHECK-LABEL: sil [serialized] [ossa] @$s27enum_raw_representable_objc5CLikeO0B5ValueACSg{{Si|s5Int32V}}_tcfC

// CHECK-LABEL: sil [serialized] [ossa] @$s27enum_raw_representable_objc5CLikeO0B5Value{{Si|s5Int32V}}vg
// CHECK-DAG: [[RESULT_BOX:%.+]] = alloc_stack $Int
// CHECK-DAG: [[INPUT_BOX:%.+]] = alloc_stack $CLike
// CHECK: [[RAW_TYPE:%.+]] = metatype $@thick Int{{(32)?}}.Type
// CHECK: [[CAST_FUNC:%.+]] = function_ref @$ss13unsafeBitCast_2toq_x_q_mtr0_lF
// CHECK: = apply [[CAST_FUNC]]<CLike, Int{{(32)?}}>([[RESULT_BOX]], [[INPUT_BOX]], [[RAW_TYPE]])
// CHECK: [[RESULT:%.+]] = load [trivial] [[RESULT_BOX]]
// CHECK: return [[RESULT]]
// CHECK: end sil function '$s27enum_raw_representable_objc5CLikeO0B5Value{{Si|s5Int32V}}vg'

// CHECK-RESILIENT-DAG: sil [ossa] @$s27enum_raw_representable_objc5CLikeO0B5Value{{Si|s5Int32V}}vg
// CHECK-RESILIENT-DAG: sil [ossa] @$s27enum_raw_representable_objc5CLikeO0B5ValueACSg{{Si|s5Int32V}}_tcfC

