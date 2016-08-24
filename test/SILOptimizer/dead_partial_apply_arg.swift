// RUN: %target-swift-frontend -parse-as-library -module-name=test -primary-file %s -O -emit-sil | %FileCheck %s


// Check if the compiler can convert a partial_apply of a dead argument (an
// unused metatype) to a thin_to_thick_function

extension Int32 {

// This function has an unused metatype argument.

// CHECK-LABEL: sil [thunk] [always_inline] @_TZFE4testVs5Int328lessthanfT3lhsS0_3rhsS0__Sb : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
  public static func lessthan (lhs: Int32, rhs: Int32) -> Bool {
    return lhs < rhs
  }
}

// CHECK-LABEL: sil hidden @_TF4test6callitFT_FTVs5Int32S0__Sb : $@convention(thin) () -> @owned @callee_owned (Int32, Int32) -> Bool
// CHECK: [[F:%[0-9]+]] = function_ref @_TTSf4n_n_d___TZFE4testVs5Int328lessthanfT3lhsS0_3rhsS0__Sb
// CHECK: [[R:%[0-9]+]] = thin_to_thick_function [[F]]
// CHECK: return [[R]]
func callit()  -> (Int32, Int32) -> Bool {
  return (Int32.lessthan)
}

