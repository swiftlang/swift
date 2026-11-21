// RUN: %target-swift-emit-silgen -disable-objc-interop %s | %FileCheck %s

// @c enums behave like @objc enums: they're represented by their raw value,
// and can hold values that don't correspond to any case.

@c enum CEnum: Int8 { case a, b, c }

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}5CEnumO{{.*}}s4Int8Vvg
// CHECK: function_ref @$ss13unsafeBitCast_2toq_x_q_mtr0_lF
// CHECK-NOT: switch_enum
// CHECK: } // end sil function
func raw(_ e: CEnum) -> Int8 { e.rawValue }

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}6switchySiAA5CEnumOF
// CHECK: unchecked_trivial_bit_cast {{.*}} to $Int8
// CHECK: switch_enum {{.*}}default bb
// CHECK: function_ref @$ss32_diagnoseUnexpectedEnumCaseValue
func `switch`(_ e: CEnum) -> Int {
  switch e {
  case .a: return 0
  case .b: return 1
  case .c: return 2
  }
}
