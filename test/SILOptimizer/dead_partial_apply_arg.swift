// RUN: %target-swift-frontend -parse-as-library -module-name=test -primary-file %s -O -emit-sil | %FileCheck %s


// Check if the compiler can convert a partial_apply of a dead argument (an
// unused metatype) to a thin_to_thick_function

extension Int32 {

// This function has an unused metatype argument.

// CHECK-LABEL: sil [signature_optimized_thunk] [always_inline] @$ss5Int32V4testE8lessthan3lhs3rhsSbAB_ABtFZ : $@convention(method) (Int32, Int32, @thin Int32.Type) -> Bool
  public static func lessthan (lhs: Int32, rhs: Int32) -> Bool {
    return lhs < rhs
  }
}

// CHECK-LABEL: sil hidden @$s4test6callitSbs5Int32V_ADtcyF : $@convention(thin) () -> @owned @callee_guaranteed (Int32, Int32) -> Bool
// CHECK: [[F:%[0-9]+]] = function_ref @$ss5Int32V4testE8lessthan3lhs3rhsSbAB_ABtFZTf4nnd_n
// CHECK: [[R:%[0-9]+]] = thin_to_thick_function [[F]]
// CHECK: return [[R]]
func callit()  -> (Int32, Int32) -> Bool {
  return (Int32.lessthan)
}

