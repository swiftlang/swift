// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

class OpaqueClass<T> {
  typealias ObnoxiousTuple = (T, (T.Type, (T) -> T))

  func inAndOut(x: T) -> T { return x }
  func variantOptionalityTuples(x: ObnoxiousTuple) -> ObnoxiousTuple? { return x }
}

class OpaqueTupleClass<U>: OpaqueClass<(U, U)> {
  override func inAndOut(x: (U, U)) -> (U, U) { return x }
}

class StillOpaqueClass<T>: OpaqueClass<T> {
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

// Test vtables - OpaqueTupleClass
// ---
// CHECK-LABEL: sil private [thunk] [ossa] @$s27opaque_values_silgen_vtable16OpaqueTupleClassC8inAndOut1xx_xtx_xt_tFAA0eG0CAdExx_tFTV : $@convention(method) <τ_0_0> (@in_guaranteed (τ_0_0, τ_0_0), @guaranteed OpaqueTupleClass<τ_0_0>) -> @out (τ_0_0, τ_0_0) {
// HECK: bb0([[ARG0:%.*]] : $(U, U), [[ARG1:%.*]] : $OpaqueTupleClass<U>):
// HECK:   ([[TELEM0:%.*]], [[TELEM1:%.*]]) = destructure_tuple [[ARG0]] : $(U, U)
// HECK:   [[APPLY:%.*]] = apply {{.*}}<U>([[TELEM0]], [[TELEM1]], [[ARG1]]) : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @guaranteed OpaqueTupleClass<τ_0_0>) -> (@out τ_0_0, @out τ_0_0)
// HECK:   [[BORROWED_CALL:%.*]] = begin_borrow [[APPLY]]
// HECK:   [[BORROWED_CALL_EXT0:%.*]] = tuple_extract [[BORROWED_CALL]] : $(U, U), 0
// HECK:   [[RETVAL0:%.*]] = copy_value [[BORROWED_CALL_EXT0]] : $U
// HECK:   [[BORROWED_CALL_EXT1:%.*]] = tuple_extract [[BORROWED_CALL]] : $(U, U), 1
// HECK:   [[RETVAL1:%.*]] = copy_value [[BORROWED_CALL_EXT1]] : $U
// HECK:   end_borrow [[BORROWED_CALL]]
// HECK:   [[RETVAL:%.*]] = tuple ([[RETVAL0]] : $U, [[RETVAL1]] : $U)
// HECK:   return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$s27opaque_values_silgen_vtable16OpaqueTupleClassC8inAndOut1xx_xtx_xt_tFAA0eG0CAdExx_tFTV'

// Test vtables - StillOpaqueClass
// ---
// CHECK-LABEL: sil private [thunk] [ossa] @$s27opaque_values_silgen_vtable16StillOpaqueClassC24variantOptionalityTuples1xx_xm_xxcttx_xm_xxcttSg_tFAA0fG0CAdeFx_xm_xxctt_tFTV : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @thick τ_0_0.Type, @guaranteed @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_0, τ_0_0>, @guaranteed StillOpaqueClass<τ_0_0>) -> @out Optional<(τ_0_0, (@thick τ_0_0.Type, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_0, τ_0_0>))> {
// HECK: bb0([[ARG0:%.*]] : $T, [[ARG1:%.*]] : $@thick T.Type, [[ARG2:%.*]] : $@callee_guaranteed (@in_guaranteed T) -> @out T, [[ARG3:%.*]] : $StillOpaqueClass<T>):
// HECK:   [[TELEM0:%.*]] = tuple ([[ARG1]] : $@thick T.Type, [[ARG2]] : $@callee_guaranteed (@in_guaranteed T) -> @out T)
// HECK:   [[TELEM1:%.*]] = tuple ([[ARG0]] : $T, [[TELEM0]] : $(@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// HECK:   [[ENUMOPT0:%.*]] = enum $Optional<(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))>, #Optional.some!enumelt, [[TELEM1]] : $(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// HECK:   [[APPLY:%.*]] = apply {{.*}}<T>([[ENUMOPT0]], [[ARG3]]) : $@convention(method) <τ_0_0> (@in_guaranteed Optional<(τ_0_0, (@thick τ_0_0.Type, @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0))>, @guaranteed StillOpaqueClass<τ_0_0>) -> (@out τ_0_0, @thick τ_0_0.Type, @owned @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0)
// HECK:   [[BORROWED_T:%.*]] = begin_borrow [[APPLY]]
// HECK:   [[BORROWED_T_EXT0:%.*]] = tuple_extract [[BORROWED_T]] : $(T, @thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T), 0
// HECK:   [[RETVAL0:%.*]] = copy_value [[BORROWED_T_EXT0]]
// HECK:   [[BORROWED_T_EXT1:%.*]] = tuple_extract [[BORROWED_T]] : $(T, @thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T), 1
// HECK:   [[BORROWED_T_EXT2:%.*]] = tuple_extract [[BORROWED_T]] : $(T, @thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T), 2
// HECK:   [[RETVAL1:%.*]] = copy_value [[BORROWED_T_EXT2]]
// HECK:   end_borrow [[BORROWED_T]]
// HECK:   [[RETTUPLE0:%.*]] = tuple ([[BORROWED_T_EXT1]] : $@thick T.Type, [[RETVAL1]] : $@callee_guaranteed (@in_guaranteed T) -> @out T)
// HECK:   [[RETTUPLE1:%.*]] = tuple ([[RETVAL0]] : $T, [[RETTUPLE0]] : $(@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// HECK:   [[RETVAL:%.*]] = enum $Optional<(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))>, #Optional.some!enumelt, [[RETTUPLE1]] : $(T, (@thick T.Type, @callee_guaranteed (@in_guaranteed T) -> @out T))
// HECK:   return [[RETVAL]]
// CHECK-LABEL:  // end sil function '$s27opaque_values_silgen_vtable16StillOpaqueClassC24variantOptionalityTuples1xx_xm_xxcttx_xm_xxcttSg_tFAA0fG0CAdeFx_xm_xxctt_tFTV'
