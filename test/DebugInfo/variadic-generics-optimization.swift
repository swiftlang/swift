// RUN: %target-swift-frontend -emit-sil %s -g -O -o - \
// RUN:    -parse-as-library -module-name a | %IRGenFileCheck %s

public func adder<each T: BinaryInteger>(xs: repeat each T) -> (repeat each T) {
  return (repeat each xs + 1)
}

// CHECK: sil_scope [[CALL_ADDER_FUNCTION:[0-9]+]] {{.*}} parent @$s1a9callAdder1x1y1zs5Int32V_s5Int16Vs4Int8VtAG_AiKtF
// CHECK: sil_scope [[CALL_ADDER_BODY:[0-9]+]] {{.*}} parent [[CALL_ADDER_FUNCTION]]
// CHECK: sil_scope [[INLINED_ADDER:[0-9]+]] {{.*}} parent @$s1a5adder2xsxxQp_txxQp_tRvzSzRzlFs5Int32V_s5Int16Vs4Int8VQP_Tg5Tf8xx_n {{.*}} inlined_at [[CALL_ADDER_BODY]]

// CHECK-LABEL: sil @$s1a9callAdder1x1y1zs5Int32V_s5Int16Vs4Int8VtAG_AiKtF : $@convention(thin) (Int32, Int16, Int8) -> (Int32, Int16, Int8) {
// CHECK: bb0(%0 : $Int32, %1 : $Int16, %2 : $Int8):
// CHECK: debug_value %0, let, name "x", {{.*}} scope [[CALL_ADDER_FUNCTION]]
// CHECK: debug_value %1, let, name "y", {{.*}} scope [[CALL_ADDER_FUNCTION]]
// CHECK: debug_value %2, let, name "z", {{.*}} scope [[CALL_ADDER_FUNCTION]]
// CHECK: debug_value %0, let, name "xs", argno 1, type $(Int32, Int16, Int8), expr op_tuple_fragment:$(Int32, Int16, Int8):0, {{.*}} scope [[INLINED_ADDER]]
// CHECK: debug_value %1, let, name "xs", argno 1, type $(Int32, Int16, Int8), expr op_tuple_fragment:$(Int32, Int16, Int8):1, {{.*}} scope [[INLINED_ADDER]]
// CHECK: debug_value %2, let, name "xs", argno 1, type $(Int32, Int16, Int8), expr op_tuple_fragment:$(Int32, Int16, Int8):2, {{.*}} scope [[INLINED_ADDER]]
// CHECK: } // end sil function '$s1a9callAdder1x1y1zs5Int32V_s5Int16Vs4Int8VtAG_AiKtF'
public func callAdder(x: Int32, y: Int16, z: Int8) -> (Int32, Int16, Int8) {
  adder(xs: x, y, z)
}

