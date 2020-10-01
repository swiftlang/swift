// RUN: %target-swift-frontend -parse-as-library -module-name=test -primary-file %s -O -emit-sil | %FileCheck %s


// Check if the compiler can convert a partial_apply of a dead argument (an
// unused metatype) to a thin_to_thick_function

extension Int32 {
  // This function has an unused metatype argument.
  public static func lessthan (lhs: Int32, rhs: Int32) -> Bool {
    return lhs < rhs
  }
}

// CHECK-LABEL: sil hidden @$s4test6callitSbs5Int32V_ADtcyF : $@convention(thin) () -> @owned @callee_guaranteed (Int32, Int32) -> Bool
// CHECK: [[F:%[0-9]+]] = function_ref @$s4test6callitSbs5Int32V_ADtcyFSbAD_ADtcADmcfu_SbAD_ADtcfu0_Tf4nnd_n : $@convention(thin) (Int32, Int32) -> Bool
// CHECK: [[R:%[0-9]+]] = thin_to_thick_function [[F]]
// CHECK: return [[R]]

// The function gets inlined into the curry thunk, and the
// unused metatype argument deleted.

// CHECK-LABEL: sil private @$s4test6callitSbs5Int32V_ADtcyFSbAD_ADtcADmcfu_SbAD_ADtcfu0_Tf4nnd_n : $@convention(thin) (Int32, Int32) -> Bool
// CHECK: builtin "cmp_slt_Int32"(%6 : $Builtin.Int32, %7 : $Builtin.Int32) : $Builtin.Int1
// CHECK: return
func callit()  -> (Int32, Int32) -> Bool {
  return (Int32.lessthan)
}

