// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s

// Test a simple opaque parameter and return value.
//
// CHECK-LABEL: sil hidden @_TF20opaque_values_silgen8identityurFT1tx_x : $@convention(thin) <T> (@in T) -> @out T {
// CHECK: bb0(%0 : $T):
// CHECK: %[[CPY:.*]] = copy_value %0 : $T
// CHECK: destroy_value %0 : $T
// CHECK: return %[[CPY]] : $T
// CHECK-LABEL: } // end sil function '_TF20opaque_values_silgen8identityurFT1tx_x'
func identity<T>(t: T) -> T {
  return t
}

protocol Foo {
  func foo()
}

// Test a guaranteed opaque parameter.
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV20opaque_values_silgen1SS_3FooS_FS1_3foofT_T_ : $@convention(witness_method) (@in_guaranteed S) -> () {
// CHECK: bb0(%0 : $S):
// CHECK: %[[F:.*]] = function_ref @_TFV20opaque_values_silgen1S3foofT_T_ : $@convention(method) (S) -> ()
// CHECK: apply %[[F]](%0) : $@convention(method) (S) -> ()
// CHECK: return
// CHECK-LABEL: } // end sil function '_TTWV20opaque_values_silgen1SS_3FooS_FS1_3foofT_T_'
struct S : Foo {
  func foo() {}
}
