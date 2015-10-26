// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class B {
  func foo() { }
  func bar()() { }
}

class D: B {
  override func foo() { }
  override func bar()() { }

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super1D7getFoos
  // CHECK:         function_ref @_TFC19partial_apply_super1D3foo
  // CHECK:         function_ref @_TTdFC19partial_apply_super1B3foo
  func getFoos() -> (() -> (), () -> ()) {
    return (self.foo, super.foo)
  }

  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3foo
  // CHECK:         class_method %0 : $D, #D.foo!1

  // CHECK-LABEL: sil shared @_TTdFC19partial_apply_super1B3foo
  // CHECK:         function_ref @_TFC19partial_apply_super1B3foo

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super1D7getBars
  // CHECK:         function_ref @_TFC19partial_apply_super1D3bar
  // CHECK:         function_ref @_TTdFC19partial_apply_super1B3bar
  func getBars() -> (() -> () -> (), () -> () -> ()) {
    return (self.bar, super.bar)
  }

  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3bar
  // CHECK:         function_ref @_TFC19partial_apply_super1D3bar
  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3bar
  // CHECK:         class_method %0 : $D, #D.bar!2

  // CHECK-LABEL: sil shared @_TTdFC19partial_apply_super1B3bar
  // CHECK:         function_ref @_TTdFC19partial_apply_super1B3bar
  // CHECK-LABEL: sil shared @_TTdFC19partial_apply_super1B3bar
  // CHECK:         function_ref @_TFC19partial_apply_super1B3bar
}
