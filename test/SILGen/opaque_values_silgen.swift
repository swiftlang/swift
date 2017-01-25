// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s

protocol Foo {
  func foo()
}

// SILGen, prepareArchetypeCallee. Materialize a
// non-class-constrainted self from a class-constrained archetype.
// CHECK-LABEL: sil hidden @_TF20opaque_values_silgen15materializeSelfuRxs9AnyObjectxS_3FoorFT1tx_T_ : $@convention(thin) <T where T : AnyObject, T : Foo> (@owned T) -> () {
// CHECK: bb0(%0 : $T):
// CHECK: witness_method $T, #Foo.foo!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply %{{[0-9]+}}<T>(%0) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()
// CHECK: destroy_value %0 : $T
// CHECK: return %{{[0-9]+}} : $()
// CHECK: } // end sil function '_TF20opaque_values_silgen15materializeSelfuRxs9AnyObjectxS_3FoorFT1tx_T_'
func materializeSelf<T: Foo>(t: T) where T: AnyObject {
  t.foo()
}

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

// Test a guaranteed opaque parameter.
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV20opaque_values_silgen17HasGuaranteedSelfS_3FooS_FS1_3foofT_T_ : $@convention(witness_method) (@in_guaranteed HasGuaranteedSelf) -> () {
// CHECK: bb0(%0 : $HasGuaranteedSelf):
// CHECK: %[[F:.*]] = function_ref @_TFV20opaque_values_silgen17HasGuaranteedSelf3foofT_T_ : $@convention(method) (HasGuaranteedSelf) -> ()
// CHECK: apply %[[F]](%0) : $@convention(method) (HasGuaranteedSelf) -> ()
// CHECK: return
// CHECK-LABEL: } // end sil function '_TTWV20opaque_values_silgen17HasGuaranteedSelfS_3FooS_FS1_3foofT_T_'
struct HasGuaranteedSelf : Foo {
  func foo() {}
}

