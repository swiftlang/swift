// RUN: %target-swift-frontend -parse-as-library -O -module-name=test %s -emit-sil | %FileCheck %s

// specialized user_simple<A>(_:c:)

// CHECK-LABEL: sil shared @$s4test11user_simple_1cxx_xxXEtlF36$s4test12entry_simple1axx_tlFxxXEfU_Tf1nnc_n
// function_ref closure #1 in entry_simple<A>(a:)
// CHECK: [[FN:%[0-9]+]] = function_ref @$s4test12entry_simple1axx_tlFxxXEfU_ : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: apply [[FN]]<A>(%0, %1)
// CHECK-LABEL: end sil function '$s4test11user_simple_1cxx_xxXEtlF36$s4test12entry_simple1axx_tlFxxXEfU_Tf1nnc_n'

// CHECK-LABEL: sil shared @$s4test11user_simple_1cxx_xxXEtlF44$s4test3FooV16entry_foo_simple1axx_tFxxXEfU_Tf1nnc_n
// function_ref closure #1 in Foo.entry_foo_simple(a:)
// CHECK: [[FN:%[0-9]+]] = function_ref @$s4test3FooV16entry_foo_simple1axx_tFxxXEfU_ : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: apply [[FN]]<A>(%0, %1)
// CHECK-LABEL: end sil function '$s4test11user_simple_1cxx_xxXEtlF44$s4test3FooV16entry_foo_simple1axx_tFxxXEfU_Tf1nnc_n'

public func user_simple<A>(_ a: A, c: (A) -> A) -> A {
  return c(a)
}

// CHECK-LABEL: sil @$s4test12entry_simple1axx_tlF
// function_ref specialized user_simple<A>(_:c:)
// CHECK: [[FN:%[0-9]+]] = function_ref @$s4test11user_simple_1cxx_xxXEtlF36$s4test12entry_simple1axx_tlFxxXEfU_Tf1nnc_n : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: apply [[FN]]<A>(%0, %1)
// CHECK-LABEL: end sil function '$s4test12entry_simple1axx_tlF'
public func entry_simple<A>(a: A) -> A {
  return user_simple(a) { a1 in a1 }
}

// specialized user_two_generics<A, B>(_:_:c:)

// CHECK-LABEL: sil shared @$s4test17user_two_generics__1cx_q_tx_q_x_q_tx_q_tXEtr0_lF015$s4test36entry_c1_d32_generic_and_tuple1ax_yttx_tlFx_I9_yttXEfU_yXlTf1nnnnc_n
// CHECK: bb0(%0 : $*A, %1 : $*B, %2 : $*A, %3 : $*B, %4 : $*A)
// function_ref closure #1 in entry_two_generics_generic_and_tuple<A>(a:)
// CHECK: [[CLOSURE_FN:%[0-9]+]] = function_ref @$s4test36entry_two_generics_generic_and_tuple1ax_yttx_tlFx_yttx_yttXEfU_
// CHECK: [[CLOSURE_PA:%[0-9]+]] = partial_apply [callee_guaranteed] [on_stack] [[CLOSURE_FN]]<A>(%4)
// CHECK: [[CLOSURE:%[0-9]+]] = mark_dependence [[CLOSURE_PA]]
// CHECK: [[THUNK_FN:%[0-9]+]] = function_ref @$sxxIgnr_xytxytIegnnrr_lTR
// CHECK: [[THUNK_PA:%[0-9]+]] = partial_apply [callee_guaranteed] [on_stack] [[THUNK_FN]]<A>([[CLOSURE]])
// CHECK: [[THUNK:%[0-9]+]] = convert_function [[THUNK_PA]]
// CHECK: apply [[THUNK]](%0, %1, %2, %3)
// CHECK-LABEL: end sil function '$s4test17user_two_generics__1cx_q_tx_q_x_q_tx_q_tXEtr0_lF015$s4test36entry_c1_d32_generic_and_tuple1ax_yttx_tlFx_I9_yttXEfU_yXlTf1nnnnc_n'

// CHECK-LABEL: sil shared @$s4test17user_two_generics__1cx_q_tx_q_x_q_tx_q_tXEtr0_lF015$s4test18entry_c32_generics1a1bx_q_tx_q_tr0_lFx_q_G9_q_tXEfU_yXlyXlTf1nnnnc_n
// CHECK: bb0(%0 : $*A, %1 : $*B, %2 : $*A, %3 : $*B, %4 : $*A, %5 : $*B
// function_ref closure #1 in entry_two_generics<A, B>(a:b:)
// CHECK: [[CLOSURE:%[0-9]+]] = function_ref @$s4test18entry_two_generics1a1bx_q_tx_q_tr0_lFx_q_tx_q_tXEfU_
// CHECK: apply [[CLOSURE]]<A, B>(%0, %1, %2, %3, %4, %5)
// CHECK-LABEL: end sil function '$s4test17user_two_generics__1cx_q_tx_q_x_q_tx_q_tXEtr0_lF015$s4test18entry_c32_generics1a1bx_q_tx_q_tr0_lFx_q_G9_q_tXEfU_yXlyXlTf1nnnnc_n'

public func user_two_generics<A, B>(_ a : A, _ b : B, c: (A, B) -> (A, B)) -> (A, B) {
    return c(a, b)
}

// CHECK-LABEL: sil @$s4test36entry_two_generics_generic_and_tuple1ax_yttx_tlF
// function_ref specialized user_two_generics<A, B>(_:_:c:)
// CHECK: [[FN:%[0-9]+]] = function_ref @$s4test17user_two_generics__1cx_q_tx_q_x_q_tx_q_tXEtr0_lF015$s4test36entry_c1_d32_generic_and_tuple1ax_yttx_tlFx_I9_yttXEfU_yXlTf1nnnnc_n
// CHECK: apply [[FN]]<T, ()>
// CHECK-LABEL: end sil function '$s4test36entry_two_generics_generic_and_tuple1ax_yttx_tlF'
public func entry_two_generics_generic_and_tuple<T>(a: T) -> (T, ()) {
  return user_two_generics(a, (), c: { _, _ in (a, ()) })
}

// CHECK-LABEL: sil @$s4test18entry_two_generics1a1bx_q_tx_q_tr0_lF
// function_ref specialized user_two_generics<A, B>(_:_:c:)
// CHECK: [[FN:%[0-9]+]] = function_ref @$s4test17user_two_generics__1cx_q_tx_q_x_q_tx_q_tXEtr0_lF015$s4test18entry_c32_generics1a1bx_q_tx_q_tr0_lFx_q_G9_q_tXEfU_yXlyXlTf1nnnnc_n
// CHECK: apply [[FN]]<T, U>
// CHECK-LABEL: end sil function '$s4test18entry_two_generics1a1bx_q_tx_q_tr0_lF'
public func entry_two_generics<T, U>(a: T, b: U) -> (T, U) {
  return user_two_generics(a, b, c: { _, _ in (a, b) })
}

// specialized user_generic_and_tuple<A>(_:_:c:)

// CHECK-LABEL: sil shared @$s4test22user_generic_and_tuple__1cx_yttx_ytx_yttx_yttXEtlF015$s4test23entry_c1_d20_tuple1ax_yttx_tlFx_H9_yttXEfU_yXlTf1nnc_n
// CHECK: bb0(%0 : $*A, %1 : $*A, %2 : $*A)
// function_ref closure #1 in entry_generic_and_tuple<A>(a:)
// CHECK: [[CLOSURE:%[0-9]+]] = function_ref @$s4test23entry_generic_and_tuple1ax_yttx_tlFx_yttx_yttXEfU_
// CHECK: apply [[CLOSURE]]<A>(%0, %1, %2)
// CHECK-LABEL: end sil function '$s4test22user_generic_and_tuple__1cx_yttx_ytx_yttx_yttXEtlF015$s4test23entry_c1_d20_tuple1ax_yttx_tlFx_H9_yttXEfU_yXlTf1nnc_n'

public func user_generic_and_tuple<A>(_ a : A, _ b : (), c: (A, ()) -> (A, ())) -> (A, ()) {
    return c(a, b)
}

// CHECK-LABEL: sil @$s4test23entry_generic_and_tuple1ax_yttx_tlF
// function_ref specialized user_generic_and_tuple<A>(_:_:c:)
// CHECK: [[FN:%[0-9]+]] = function_ref @$s4test22user_generic_and_tuple__1cx_yttx_ytx_yttx_yttXEtlF015$s4test23entry_c1_d20_tuple1ax_yttx_tlFx_H9_yttXEfU_yXlTf1nnc_n
// CHECK: apply [[FN]]<T>
// CHECK-LABEL: end sil function '$s4test23entry_generic_and_tuple1ax_yttx_tlF'
public func entry_generic_and_tuple<T>(a: T) -> (T, ()) {
  return user_generic_and_tuple(a, (), c: { _, _ in (a, ()) })
}

public struct Foo<T> {
  // CHECK-LABEL: sil shared @$s4test3FooV8user_foo_1cxx_xxXEtF08$s4test3B22V9entry_fooyxxFxxXEfU_Tf1nncn_n
  // CHECK: bb0(%0 : $*T, %1 : $*T, %2 : $Foo<T>)
  // function_ref closure #1 in Foo.entry_foo(_:)
  // CHECK: [[CLOSURE:%[0-9]+]] = function_ref @$s4test3FooV9entry_fooyxxFxxXEfU_
  // CHECK: apply [[CLOSURE]]<T>(%0, %1)
  // CHECK-LABEL: end sil function '$s4test3FooV8user_foo_1cxx_xxXEtF08$s4test3B22V9entry_fooyxxFxxXEfU_Tf1nncn_n'
  
  public func user_foo(_ a: T, c: (T) -> T) -> T {
    return c(a)
  }

  // CHECK-LABEL: sil @$s4test3FooV9entry_fooyxxF
  // function_ref specialized Foo.user_foo(_:c:)
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s4test3FooV8user_foo_1cxx_xxXEtF08$s4test3B22V9entry_fooyxxFxxXEfU_Tf1nncn_n
  // CHECK: apply [[FN]]<T>
  // CHECK-LABEL: end sil function '$s4test3FooV9entry_fooyxxF'
  public func entry_foo(_ a: T) -> T {
    return user_foo(a) { a1 in a1 }
  }
  
  // CHECK-LABEL: sil @$s4test3FooV16entry_foo_simple1axx_tF
  // function_ref specialized user_simple<A>(_:c:)
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s4test11user_simple_1cxx_xxXEtlF44$s4test3FooV16entry_foo_simple1axx_tFxxXEfU_Tf1nnc_n
  // CHECK: apply [[FN]]<T>
  // CHECK-LABEL: end sil function '$s4test3FooV16entry_foo_simple1axx_tF'
  public func entry_foo_simple(a: T) -> T {
    return user_simple(a) { a1 in a1 }
  }
}
