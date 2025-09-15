// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

// rdar://problem/67419937

class Class<T> {
  var fn: ((T) -> ())? = nil

  init(_: T) where T == Int {}
}

// CHECK-LABEL: sil hidden [ossa] @$s34stored_property_init_reabstraction5ClassCyACySiGSicSiRszlufc : $@convention(method) (Int, @owned Class<Int>) -> @owned Class<Int> {
// CHECK: [[SELF:%.*]] = mark_uninitialized [rootself] %1 : $Class<Int>
// CHECK: [[BORROW:%.*]] = begin_borrow [[SELF]] : $Class<Int>
// CHECK: [[ADDR:%.*]] = ref_element_addr [[BORROW]] : $Class<Int>, #Class.fn
// CHECK: [[INIT:%.*]] = function_ref @$s34stored_property_init_reabstraction5ClassC2fnyxcSgvpfi : $@convention(thin) <τ_0_0> () -> @owned Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>>
// CHECK: [[VALUE:%.*]] = apply [[INIT]]<Int>() : $@convention(thin) <τ_0_0> () -> @owned Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>>
// CHECK: store [[VALUE]] to [init] [[ADDR]] : $*Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <Int>>
// CHECK: end_borrow [[BORROW]] : $Class<Int>

struct Struct<T> {
  var fn: ((T) -> ())? = nil

  init(_: T) where T == Int {}
}

// CHECK-LABEL: sil hidden [ossa] @$s34stored_property_init_reabstraction6StructVyACySiGSicSiRszlufC : $@convention(method) (Int, @thin Struct<Int>.Type) -> @owned Struct<Int> {
// CHECK: [[SELF_BOX:%.*]] = mark_uninitialized [rootself] {{%.*}} : ${ var Struct<Int> }
// CHECK: [[SELF_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[SELF_BOX]]
// CHECK: [[SELF:%.*]] = project_box [[SELF_LIFETIME]] : ${ var Struct<Int> }, 0
// CHECK: [[ADDR:%.*]] = struct_element_addr [[SELF]] : $*Struct<Int>, #Struct.fn
// CHECK: [[INIT:%.*]] = function_ref @$s34stored_property_init_reabstraction6StructV2fnyxcSgvpfi : $@convention(thin) <τ_0_0> () -> @owned Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>>
// CHECK: [[VALUE:%.*]] = apply [[INIT]]<Int>() : $@convention(thin) <τ_0_0> () -> @owned Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>>
// CHECK: store [[VALUE]] to [init] [[ADDR]] : $*Optional<@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <Int>>

struct ComplexExample<T, U> {
  var (fn, value): (((T) -> ())?, U?) = (nil, nil)
  var anotherValue: (((T) -> ())?, U?) = (nil, nil)

  init(_: T) where T == String {}
}
