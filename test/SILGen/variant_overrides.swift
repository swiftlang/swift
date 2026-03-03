// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s17variant_overrides1AC3foo5blockyyACc_tF :
// CHECK-SAME:    $@convention(method) (@guaranteed @callee_guaranteed (@guaranteed A) -> (), @guaranteed A) -> ()
class A {
  func foo(block: @escaping (A) -> Void) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s17variant_overrides1BC3foo5blockyyACyxGc_tF :
// CHECK-SAME:     $@convention(method) <T> (@guaranteed @callee_guaranteed @substituted <τ_0_0> (@guaranteed B<τ_0_0>) -> () for <T>, @guaranteed B<T>) -> ()
class B<T> : A {
  override func foo(block: @escaping (B<T>) -> Void) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s17variant_overrides12useAtGeneric1byAA1BCyxG_tlF
func useAtGeneric<T>(b: B<T>) {
  // CHECK: [[CLOSURE_FUNC:%.*]] = function_ref @$s17variant_overrides12useAtGeneric1byAA1BCyxG_tlFyAFcfU_ : $@convention(thin) <τ_0_0> (@guaranteed B<τ_0_0>) -> ()
  // CHECK: [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FUNC]]<T>() : $@convention(thin) <τ_0_0> (@guaranteed B<τ_0_0>) -> ()
  // CHECK: [[METHOD:%.*]] =  class_method %0 : $B<T>, #B.foo : <T> (B<T>) -> (@escaping (B<T>) -> ()) -> (), $@convention(method) <τ_0_0> (@guaranteed @callee_guaranteed (@guaranteed B<τ_0_0>) -> (), @guaranteed B<τ_0_0>) -> ()
  // CHECK: apply [[METHOD]]<T>([[CLOSURE]], %0) : $@convention(method) <τ_0_0> (@guaranteed @callee_guaranteed (@guaranteed B<τ_0_0>) -> (), @guaranteed B<τ_0_0>) -> ()

  b.foo {_ in ()}
}

// CHECK-LABEL: sil hidden [ossa] @$s17variant_overrides13useAtConcrete1byAA1BCySiG_tF
func useAtConcrete(b: B<Int>) {
  // CHECK: [[CLOSURE_FUNC:%.*]] = function_ref @$s17variant_overrides13useAtConcrete1byAA1BCySiG_tFyAFcfU_ :
  // CHECK: [[CLOSURE_THICK:%.*]] = thin_to_thick_function [[CLOSURE_FUNC]] :
  // CHECK: [[METHOD:%.*]] =  class_method %0 : $B<Int>, #B.foo : <T> (B<T>) -> (@escaping (B<T>) -> ()) -> (), $@convention(method) <τ_0_0> (@guaranteed @callee_guaranteed (@guaranteed B<τ_0_0>) -> (), @guaranteed B<τ_0_0>) -> ()
  // CHECK: apply [[METHOD]]<Int>([[CLOSURE_THICK]], %0) : $@convention(method) <τ_0_0> (@guaranteed @callee_guaranteed (@guaranteed B<τ_0_0>) -> (), @guaranteed B<τ_0_0>) -> ()

  b.foo {_ in ()}
}

// CHECK-LABEL: sil_vtable B {
// CHECK:          #A.foo: (A) -> (@escaping (A) -> ()) -> () : @$s17variant_overrides1BC3foo5blockyyACyxGc_tFAA1ACAdEyyAHc_tFTV [override]
