// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-as-library -module-name=test -primary-file %s -O -emit-sil | %FileCheck %s


// Check if the compiler can convert a partial_apply of a dead argument (an
// unused metatype) to a thin_to_thick_function

extension Int32 {

// This function has an unused metatype argument.

// CHECK-LABEL: sil [thunk] [always_inline] @_T0s5Int32V4testE8lessthanSbAB3lhs_AB3rhstFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
  public static func lessthan (lhs: Int32, rhs: Int32) -> Bool {
    return lhs < rhs
  }
}

// CHECK-LABEL: sil hidden @_T04test6callitSbs5Int32V_ADtcyF : $@convention(thin) () -> @owned @callee_owned (Int32, Int32) -> Bool
// CHECK: [[F:%[0-9]+]] = function_ref @_T0s5Int32V4testE8lessthanSbAB3lhs_AB3rhstFZTf4nnd_n
// CHECK: [[R:%[0-9]+]] = thin_to_thick_function [[F]]
// CHECK: return [[R]]
func callit()  -> (Int32, Int32) -> Bool {
  return (Int32.lessthan)
}

