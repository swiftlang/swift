// RUN: %target-swift-frontend -parse-as-library -module-name test %s -O -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// CHECK-LABEL: sil @$s4test17dont_fold_inf_cmpySbSfF :
// CHECK:         builtin "fcmp_olt_FPIEEE32"
// CHECK:       } // end sil function '$s4test17dont_fold_inf_cmpySbSfF'
public func dont_fold_inf_cmp(_ f: Float) -> Bool {
  (f + 0) < .infinity
}

// CHECK-LABEL: sil @$s4test014dont_fold_inf_D4_cmpSbyF :
// CHECK:         builtin "fcmp_olt_FPIEEE32"
// CHECK:       } // end sil function '$s4test014dont_fold_inf_D4_cmpSbyF'
public func dont_fold_inf_inf_cmp() -> Bool {
  0x1.0p128 < Float.infinity
}

