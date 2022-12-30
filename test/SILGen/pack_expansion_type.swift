// RUN: %target-swift-emit-silgen %s -enable-experimental-feature VariadicGenerics | %FileCheck %s

// Experimental features require an asserts compiler
// REQUIRES: asserts

// XFAIL: OS=windows-msvc

// CHECK-LABEL: sil [ossa] @$s19pack_expansion_type16variadicFunction1t1ux_q_txQp_txxQp_q_q_Qptq_Rhzr0_lF : $@convention(thin) <T..., U... where ((T, U)...) : Any> (@in_guaranteed T..., @in_guaranteed U...) -> @out (T, U)... {
// CHECK: bb0(%0 : $*(T, U)..., %1 : $*T..., %2 : $*U...):
public func variadicFunction<T..., U...>(t: T..., u: U...) -> ((T, U)...) {}

public struct VariadicType<T...> {
  // CHECK-LABEL: sil [ossa] @$s19pack_expansion_type12VariadicTypeV14variadicMethod1t1ux_qd__txQp_txxQp_qd__qd__Qptqd__RhzlF : $@convention(method) <T...><U... where ((T, U)...) : Any> (@in_guaranteed T..., @in_guaranteed U..., VariadicType<T...>) -> @out (T, U)... {
  // CHECK: bb0(%0 : $*(T, U)..., %1 : $*T..., %2 : $*U..., %3 : $VariadicType<T...>):
  public func variadicMethod<U...>(t: T..., u: U...) -> ((T, U)...) {}

  // CHECK-LABEL: sil [ossa] @$s19pack_expansion_type12VariadicTypeV13takesFunction1tyqd__qd__Qp_txxQpXE_tlF : $@convention(method) <T...><U...> (@noescape @callee_guaranteed @substituted <τ_0_0..., τ_0_1..., τ_0_2..., τ_0_3...> (@in_guaranteed τ_0_0...) -> @out τ_0_2... for <T, T, U, U>, VariadicType<T...>) -> () {
  // CHECK: bb0(%0 : $@noescape @callee_guaranteed @substituted <τ_0_0..., τ_0_1..., τ_0_2..., τ_0_3...> (@in_guaranteed τ_0_0...) -> @out τ_0_2... for <T, T, U, U>, %1 : $VariadicType<T...>):
  public func takesFunction<U...>(t: (T...) -> (U...)) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s19pack_expansion_type17variadicMetatypesyyxxQplF : $@convention(thin) <T...> (@in_guaranteed T...) -> () {
// CHECK: bb0(%0 : $*T...):
// CHECK: metatype $@thin VariadicType<>.Type
// CHECK: metatype $@thin VariadicType<Int>.Type
// CHECK: metatype $@thin VariadicType<Int, String>.Type
// CHECK: metatype $@thin VariadicType<T...>.Type
// CHECK: metatype $@thin VariadicType<Int, Array<T>...>.Type
// CHECK: metatype $@thin (T...).Type
// CHECK: metatype $@thin (Int, Array<T>...).Type
// CHECK: metatype $@thin ((T...) -> ()).Type
// CHECK: metatype $@thin ((Int, Array<T>...) -> ()).Type
// CHECK: return

func variadicMetatypes<T...>(_: T...) {
  _ = VariadicType< >.self
  _ = VariadicType<Int>.self
  _ = VariadicType<Int, String>.self
  _ = VariadicType<T... >.self
  _ = VariadicType<Int, (Array<T>)... >.self
  _ = (T...).self
  _ = (Int, (Array<T>)...).self
  _ = ((T...) -> ()).self
  _ = ((Int, (Array<T>)...) -> ()).self
}