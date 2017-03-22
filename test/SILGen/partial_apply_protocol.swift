// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @_T022partial_apply_protocol12testClonableyAA0E0_p1c_tF : $@convention(thin) (@in Clonable) -> ()
func testClonable(c: Clonable) {
  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xIxr_22partial_apply_protocol8Clonable_pIxr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  let _: () -> Clonable = c.clone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xSgIxr_22partial_apply_protocol8Clonable_pSgIxr_AbCRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out Optional<τ_0_0>) -> @out Optional<Clonable>
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out Optional<τ_0_0>) -> @out Optional<Clonable>
  let _: () -> Clonable? = c.maybeClone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xXMTIxd_22partial_apply_protocol8Clonable_pXmTIxd_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @thick τ_0_0.Type) -> @thick Clonable.Type
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @thick τ_0_0.Type) -> @thick Clonable.Type
  let _: () -> Clonable.Type = c.cloneMetatype

  // CHECK: [[METHOD_FN:%.*]] = witness_method $@opened("{{.*}}") Clonable, #Clonable.getCloneFn!1 : {{.*}}, {{.*}} : $*@opened("{{.*}}") Clonable : $@convention(witness_method) <τ_0_0 where τ_0_0 : Clonable> (@in_guaranteed τ_0_0) -> @owned @callee_owned () -> @out τ_0_0
  // CHECK: [[RESULT:%.*]] = apply [[METHOD_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Clonable> (@in_guaranteed τ_0_0) -> @owned @callee_owned () -> @out τ_0_0
  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xIxr_22partial_apply_protocol8Clonable_pIxr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>([[RESULT]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  let _: () -> Clonable = c.getCloneFn()

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xIxr_Ixo_22partial_apply_protocol8Clonable_pIxr_Ixo_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @owned @callee_owned () -> @out τ_0_0) -> @owned @callee_owned () -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @owned @callee_owned () -> @out τ_0_0) -> @owned @callee_owned () -> @out Clonable
  let _: () -> () -> Clonable = c.getCloneFn
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0xIxr_22partial_apply_protocol8Clonable_pIxr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
// CHECK:       bb0(%0 : $*Clonable, %1 : $@callee_owned () -> @out τ_0_0):
// CHECK-NEXT:    [[INNER_RESULT:%.*]] = alloc_stack $τ_0_0
// CHECK-NEXT:    apply %1([[INNER_RESULT]])
// CHECK-NEXT:    [[OUTER_RESULT:%.*]] = init_existential_addr %0
// CHECK-NEXT:    copy_addr [take] [[INNER_RESULT]] to [initialization] [[OUTER_RESULT]]
// CHECK-NEXT:    [[EMPTY:%.*]] = tuple ()
// CHECK-NEXT:    dealloc_stack [[INNER_RESULT]]
// CHECK-NEXT:    return [[EMPTY]]

// FIXME: This is horribly inefficient, too much alloc_stack / copy_addr!

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0xSgIxr_22partial_apply_protocol8Clonable_pSgIxr_AbCRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out Optional<τ_0_0>) -> @out Optional<Clonable>
// CHECK:       bb0(%0 : $*Optional<Clonable>, %1 : $@callee_owned () -> @out Optional<τ_0_0>):
// CHECK-NEXT:    [[INNER_RESULT:%.*]] = alloc_stack $Optional<τ_0_0>
// CHECK-NEXT:    apply %1([[INNER_RESULT]])
// CHECK-NEXT:    [[OUTER_RESULT:%.*]] = alloc_stack $Optional<Clonable>
// CHECK-NEXT:    switch_enum_addr [[INNER_RESULT]] : $*Optional<{{.*}}>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]],
// CHECK:       [[SOME_BB]]:
// CHECK-NEXT:    [[INNER_RESULT_ADDR:%.*]] = unchecked_take_enum_data_addr [[INNER_RESULT]]
// CHECK-NEXT:    [[SOME_PAYLOAD:%.*]] = alloc_stack $Clonable
// CHECK-NEXT:    [[SOME_PAYLOAD_ADDR:%.*]] = init_existential_addr [[SOME_PAYLOAD]]
// CHECK-NEXT:    copy_addr [take] [[INNER_RESULT_ADDR]] to [initialization] [[SOME_PAYLOAD_ADDR]]
// CHECK-NEXT:    [[OUTER_RESULT_ADDR:%.*]] = init_enum_data_addr [[OUTER_RESULT]]
// CHECK-NEXT:    copy_addr [take] [[SOME_PAYLOAD]] to [initialization] [[OUTER_RESULT_ADDR]]
// CHECK-NEXT:    inject_enum_addr [[OUTER_RESULT]]
// CHECK-NEXT:    dealloc_stack [[SOME_PAYLOAD]]
// CHECK-NEXT:    br bb3
// CHECK:       bb2:
// CHECK-NEXT:    inject_enum_addr [[OUTER_RESULT]]
// CHECK-NEXT:    br bb3
// CHECK:       bb3:
// CHECK-NEXT:    copy_addr [take] [[OUTER_RESULT]] to [initialization] %0
// CHECK-NEXT:    [[EMPTY:%.*]] = tuple ()
// CHECK-NEXT:    dealloc_stack [[OUTER_RESULT]]
// CHECK-NEXT:    dealloc_stack [[INNER_RESULT]]
// CHECK-NEXT:    return [[EMPTY]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0xXMTIxd_22partial_apply_protocol8Clonable_pXmTIxd_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @thick τ_0_0.Type) -> @thick Clonable.Type
// CHECK:       bb0(%0 : $@callee_owned () -> @thick τ_0_0.Type):
// CHECK-NEXT:    [[INNER_RESULT:%.*]] = apply %0()
// CHECK-NEXT:    [[OUTER_RESULT:%.*]] = init_existential_metatype [[INNER_RESULT]]
// CHECK-NEXT:    return [[OUTER_RESULT]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0xIxr_Ixo_22partial_apply_protocol8Clonable_pIxr_Ixo_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @owned @callee_owned () -> @out τ_0_0) -> @owned @callee_owned () -> @out Clonable
// CHECK:       bb0(%0 : $@callee_owned () -> @owned @callee_owned () -> @out τ_0_0):
// CHECK-NEXT:    [[INNER_RESULT:%.*]] = apply %0()
// CHECK:         [[THUNK_FN:%.*]] = function_ref @_T0xIxr_22partial_apply_protocol8Clonable_pIxr_AaBRzlTR
// CHECK-NEXT:    [[OUTER_RESULT:%.*]] = partial_apply [[THUNK_FN]]<τ_0_0>([[INNER_RESULT]])
// CHECK-NEXT:    return [[OUTER_RESULT]]

//===----------------------------------------------------------------------===//
// Partial apply of methods returning Self-derived types from generic context
//
// Make sure the thunk only has the context generic parameters if needed!
//===----------------------------------------------------------------------===//

// CHECK-LABEL: sil hidden @_T022partial_apply_protocol28testClonableInGenericContextyAA0E0_p1c_x1ttlF : $@convention(thin) <T> (@in Clonable, @in T) -> ()
func testClonableInGenericContext<T>(c: Clonable, t: T) {
  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xIxr_22partial_apply_protocol8Clonable_pIxr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  let _: () -> Clonable = c.clone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xSgIxr_22partial_apply_protocol8Clonable_pSgIxr_AbCRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out Optional<τ_0_0>) -> @out Optional<Clonable>
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out Optional<τ_0_0>) -> @out Optional<Clonable>
  let _: () -> Clonable? = c.maybeClone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xXMTIxd_22partial_apply_protocol8Clonable_pXmTIxd_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @thick τ_0_0.Type) -> @thick Clonable.Type
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @thick τ_0_0.Type) -> @thick Clonable.Type
  let _: () -> Clonable.Type = c.cloneMetatype

  // CHECK: [[METHOD_FN:%.*]] = witness_method $@opened("{{.*}}") Clonable, #Clonable.getCloneFn!1 : {{.*}}, {{.*}} : $*@opened("{{.*}}") Clonable : $@convention(witness_method) <τ_0_0 where τ_0_0 : Clonable> (@in_guaranteed τ_0_0) -> @owned @callee_owned () -> @out τ_0_0
  // CHECK: [[RESULT:%.*]] = apply [[METHOD_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Clonable> (@in_guaranteed τ_0_0) -> @owned @callee_owned () -> @out τ_0_0
  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xIxr_22partial_apply_protocol8Clonable_pIxr_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>([[RESULT]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @out τ_0_0) -> @out Clonable
  let _: () -> Clonable = c.getCloneFn()

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xIxr_Ixo_22partial_apply_protocol8Clonable_pIxr_Ixo_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @owned @callee_owned () -> @out τ_0_0) -> @owned @callee_owned () -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<@opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : Clonable> (@owned @callee_owned () -> @owned @callee_owned () -> @out τ_0_0) -> @owned @callee_owned () -> @out Clonable
  let _: () -> () -> Clonable = c.getCloneFn

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xqd__Ixir_x22partial_apply_protocol8Clonable_pIxir_AaBRd__r__lTR : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : Clonable> (@in τ_0_0, @owned @callee_owned (@in τ_0_0) -> @out τ_1_0) -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<T, @opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : Clonable> (@in τ_0_0, @owned @callee_owned (@in τ_0_0) -> @out τ_1_0) -> @out Clonable
  let _: (T) -> Clonable = c.genericClone

  // CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xqd__Ixr_Ixio_x22partial_apply_protocol8Clonable_pIxr_Ixio_AaBRd__r__lTR : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : Clonable> (@in τ_0_0, @owned @callee_owned (@in τ_0_0) -> @owned @callee_owned () -> @out τ_1_0) -> @owned @callee_owned () -> @out Clonable
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]<T, @opened("{{.*}}") Clonable>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : Clonable> (@in τ_0_0, @owned @callee_owned (@in τ_0_0) -> @owned @callee_owned () -> @out τ_1_0) -> @owned @callee_owned () -> @out Clonable
  let _: (T) -> () -> Clonable = c.genericGetCloneFn
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0xqd__Ixir_x22partial_apply_protocol8Clonable_pIxir_AaBRd__r__lTR : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : Clonable> (@in τ_0_0, @owned @callee_owned (@in τ_0_0) -> @out τ_1_0) -> @out Clonable
// CHECK:         bb0(%0 : $*Clonable, %1 : $*τ_0_0, %2 : $@callee_owned (@in τ_0_0) -> @out τ_1_0):
// CHECK-NEXT:      [[INNER_RESULT:%.*]] = alloc_stack $τ_1_0
// CHECK-NEXT:      apply %2([[INNER_RESULT]], %1)
// CHECK-NEXT:      [[OUTER_RESULT:%.*]] = init_existential_addr %0
// CHECK-NEXT:      copy_addr [take] [[INNER_RESULT]] to [initialization] [[OUTER_RESULT]]
// CHECK-NEXT:      [[EMPTY:%.*]] = tuple ()
// CHECK-NEXT:      dealloc_stack [[INNER_RESULT]]
// CHECK-NEXT:      return [[EMPTY]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0xqd__Ixr_Ixio_x22partial_apply_protocol8Clonable_pIxr_Ixio_AaBRd__r__lTR : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : Clonable> (@in τ_0_0, @owned @callee_owned (@in τ_0_0) -> @owned @callee_owned () -> @out τ_1_0) -> @owned @callee_owned () -> @out Clonable
// CHECK:         bb0(%0 : $*τ_0_0, %1 : $@callee_owned (@in τ_0_0) -> @owned @callee_owned () -> @out τ_1_0):
// CHECK-NEXT:      apply %1(%0)
// CHECK:           [[THUNK_FN:%.*]] = function_ref @_T0qd__Ixr_22partial_apply_protocol8Clonable_pIxr_AaBRd__r__lTR
// CHECK-NEXT:      [[RESULT:%.*]] = partial_apply [[THUNK_FN]]<τ_0_0, τ_1_0>(%2)
// CHECK-NEXT:      return [[RESULT]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0qd__Ixr_22partial_apply_protocol8Clonable_pIxr_AaBRd__r__lTR : $@convention(thin) <τ_0_0><τ_1_0 where τ_1_0 : Clonable> (@owned @callee_owned () -> @out τ_1_0) -> @out Clonable {
// CHECK:         bb0(%0 : $*Clonable, %1 : $@callee_owned () -> @out τ_1_0):
// CHECK-NEXT:      [[INNER_RESULT:%.*]] = alloc_stack $τ_1_0
// CHECK-NEXT:      apply %1([[INNER_RESULT]])
// CHECK-NEXT:      [[OUTER_RESULT:%.*]] = init_existential_addr %0
// CHECK-NEXT:      copy_addr [take] [[INNER_RESULT]] to [initialization] [[OUTER_RESULT]]
// CHECK-NEXT:      [[EMPTY:%.*]] = tuple ()
// CHECK-NEXT:      dealloc_stack [[INNER_RESULT:%.*]]
// CHECK-NEXT:      return [[EMPTY]]
