// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s17variant_overrides1AC3foo5blockyyACc_tF :
// CHECK-SAME:    $@convention(method) (@guaranteed @callee_guaranteed (@guaranteed A) -> (), @guaranteed A) -> ()
class A {
  func foo(block: @escaping (A) -> Void) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s17variant_overrides1BC3foo5blockyyACyxGc_tF :
// CHECK-SAME:    $@convention(method) <T> (@guaranteed @callee_guaranteed @substituted <τ_0_0> (@guaranteed B<τ_0_0>) -> () for <T>, @guaranteed B<T>) -> () 
class B<T> : A {
  override func foo(block: @escaping (B<T>) -> Void) {}
}

//   FIXME: allowing this without a thunk silently reinterprets the function to a different abstraction!
// CHECK-LABEL: sil_vtable B {
// CHECK:          #A.foo!1: (A) -> (@escaping (A) -> ()) -> () : @$s17variant_overrides1BC3foo5blockyyACyxGc_tF [override]
