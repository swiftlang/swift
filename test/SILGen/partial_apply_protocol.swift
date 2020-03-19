
// RUN: %target-swift-emit-silgen -module-name partial_apply_protocol -primary-file %s | %FileCheck %s
// RUN: %target-swift-emit-ir -module-name partial_apply_protocol -primary-file %s

protocol Clonable {
  func clone() -> Self
  func maybeClone() -> Self?
  func cloneMetatype() -> Self.Type
  func getCloneFn() -> () -> Self

  func genericClone<T>(t: T) -> Self
  func genericGetCloneFn<T>(t: T) -> () -> Self
}

//===----------------------------------------------------------------------===//
// Partial apply of methods returning Self-derived types
//===----------------------------------------------------------------------===//

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_protocol12testClonable1cyAA0E0_p_tF : $@convention(thin) (@in_guaranteed Clonable) -> ()
func testClonable(c: Clonable) {
  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol12testClonable1cyAA0E0_p_tFAaD_pycAaD_pcfu_ : $@convention(thin) (@in_guaranteed Clonable) -> @owned @callee_guaranteed () -> @out Clonable
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> Clonable = c.clone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol12testClonable1cyAA0E0_p_tFAaD_pSgycAaD_pcfu1_ : $@convention(thin) (@in_guaranteed Clonable) -> @owned @callee_guaranteed () -> @out Optional<Clonable>
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> Clonable? = c.maybeClone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol12testClonable1cyAA0E0_p_tFAaD_pXpycAaD_pcfu3_ : $@convention(thin) (@in_guaranteed Clonable) -> @owned @callee_guaranteed () -> @thick Clonable.Type
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> Clonable.Type = c.cloneMetatype

  // CHECK: [[METHOD_FN:%.*]] = witness_method $@opened("{{.*}}") Clonable, #Clonable.getCloneFn :
  // CHECK: [[RESULT:%.*]] = apply [[METHOD_FN]]<@opened("{{.*}}") Clonable>({{.*}})
  // CHECK: [[CONV_RESULT:%.*]] = convert_function [[RESULT]]
  // CHECK: [[THUNK_FN:%.*]] = function_ref @$sxIegr_22partial_apply_protocol8Clonable_pIegr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@guaranteed @callee_guaranteed () -> @out τ_0_0) -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<@opened("{{.*}}") Clonable>([[CONV_RESULT]])
  let _: () -> Clonable = c.getCloneFn()

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol12testClonable1cyAA0E0_p_tFAaD_pycycAaD_pcfu5_
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> () -> Clonable = c.getCloneFn
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] [ossa] @$sxIegr_22partial_apply_protocol8Clonable_pIegr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@guaranteed @callee_guaranteed () -> @out τ_0_0) -> @out Clonable
// CHECK:       bb0(%0 : $*Clonable, %1 : @guaranteed $@callee_guaranteed () -> @out τ_0_0):
// CHECK-NEXT:    [[INNER_RESULT:%.*]] = alloc_stack $τ_0_0
// CHECK-NEXT:    apply %1([[INNER_RESULT]])
// CHECK-NEXT:    [[OUTER_RESULT:%.*]] = init_existential_addr %0
// CHECK-NEXT:    copy_addr [take] [[INNER_RESULT]] to [initialization] [[OUTER_RESULT]]
// CHECK-NEXT:    [[EMPTY:%.*]] = tuple ()
// CHECK-NEXT:    dealloc_stack [[INNER_RESULT]]
// CHECK-NEXT:    return [[EMPTY]]

//===----------------------------------------------------------------------===//
// Partial apply of methods returning Self-derived types from generic context
//
// Make sure the thunk only has the context generic parameters if needed!
//===----------------------------------------------------------------------===//

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_protocol28testClonableInGenericContext1c1tyAA0E0_p_xtlF : $@convention(thin) <T> (@in_guaranteed Clonable, @in_guaranteed T) -> ()
func testClonableInGenericContext<T>(c: Clonable, t: T) {
  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol28testClonableInGenericContext1c1tyAA0E0_p_xtlFAaE_pycAaE_pcfu_ : $@convention(thin) (@in_guaranteed Clonable) -> @owned @callee_guaranteed () -> @out Clonable
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> Clonable = c.clone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol28testClonableInGenericContext1c1tyAA0E0_p_xtlFAaE_pSgycAaE_pcfu1_ : $@convention(thin) (@in_guaranteed Clonable) -> @owned @callee_guaranteed () -> @out Optional<Clonable> // user: %8
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> Clonable? = c.maybeClone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol28testClonableInGenericContext1c1tyAA0E0_p_xtlFAaE_pXpycAaE_pcfu3_ : $@convention(thin) (@in_guaranteed Clonable) -> @owned @callee_guaranteed () -> @thick Clonable.Type
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> Clonable.Type = c.cloneMetatype

  // CHECK: [[METHOD_FN:%.*]] = witness_method $@opened("{{.*}}") Clonable, #Clonable.getCloneFn :
  // CHECK: [[RESULT:%.*]] = apply [[METHOD_FN]]<@opened("{{.*}}") Clonable>({{.*}})
  // CHECK: [[RESULT_CONV:%.*]] = convert_function [[RESULT]]
  // CHECK: [[THUNK_FN:%.*]] = function_ref @$sxIegr_22partial_apply_protocol8Clonable_pIegr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@guaranteed @callee_guaranteed () -> @out τ_0_0) -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<@opened("{{.*}}") Clonable>([[RESULT_CONV]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@guaranteed @callee_guaranteed () -> @out τ_0_0) -> @out Clonable
  let _: () -> Clonable = c.getCloneFn()

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol28testClonableInGenericContext1c1tyAA0E0_p_xtlFAaE_pycycAaE_pcfu5_ : $@convention(thin) (@in_guaranteed Clonable) -> @owned @callee_guaranteed () -> @owned @callee_guaranteed () -> @out Clonable
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]({{.*}})
  let _: () -> () -> Clonable = c.getCloneFn

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol28testClonableInGenericContext1c1tyAA0E0_p_xtlFAaE_pxcAaE_pcfu7_ : $@convention(thin) <τ_0_0> (@in_guaranteed Clonable) -> @owned @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> @out Clonable for <τ_0_0>
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]<T>({{.*}})
  let _: (T) -> Clonable = c.genericClone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @$s22partial_apply_protocol28testClonableInGenericContext1c1tyAA0E0_p_xtlFAaE_pycxcAaE_pcfu9_ : $@convention(thin) <τ_0_0> (@in_guaranteed Clonable) -> @owned @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> (@owned @callee_guaranteed () -> @out Clonable) for <τ_0_0>
  // CHECK: [[THUNK:%.*]] = apply [[THUNK_FN]]<T>({{.*}})
  let _: (T) -> () -> Clonable = c.genericGetCloneFn
}
