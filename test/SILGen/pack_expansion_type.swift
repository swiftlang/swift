// RUN: %target-swift-emit-silgen %s -enable-experimental-feature VariadicGenerics | %FileCheck %s

// CHECK-LABEL: sil [ossa] @$s19pack_expansion_type16variadicFunction1t1ux_q_txQp_txxQp_q_q_Qptq_Rhzr0_lF : $@convention(thin) <T..., U... where ((T, U)...) : Any> (@in_guaranteed T..., @in_guaranteed U...) -> @out (T, U)... {
// CHECK: bb0(%0 : $*(T, U)..., %1 : $*T..., %2 : $*U...):
public func variadicFunction<T..., U...>(t: T..., u: U...) -> ((T, U)...) {}

public struct VariadicType<T...> {
  // CHECK-LABEL: sil [ossa] @$s19pack_expansion_type12VariadicTypeV14variadicMethod1t1ux_qd__txQp_txxQp_qd__qd__Qptqd__RhzlF : $@convention(method) <T...><U... where ((T, U)...) : Any> (@in_guaranteed T..., @in_guaranteed U..., VariadicType<T>) -> @out (T, U)... {
  // CHECK: bb0(%0 : $*(T, U)..., %1 : $*T..., %2 : $*U..., %3 : $VariadicType<T>):
  public func variadicMethod<U...>(t: T..., u: U...) -> ((T, U)...) {}
}