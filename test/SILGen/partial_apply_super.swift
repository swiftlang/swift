// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class B {
  func foo() { }
  func bar()() { }
}

class D: B {
  override func foo() { }
  override func bar()() { }

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super1D7getFoosfS0_FT_TFT_T_FT_T__
  // CHECK:         function_ref @_TFC19partial_apply_super1D3fooFS0_FT_T_
  // CHECK:         function_ref @_TTdFC19partial_apply_super1B3fooFS0_FT_T_
  func getFoos() -> (() -> (), () -> ()) {
    return (self.foo, super.foo)
  }

  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3fooFS0_FT_T_
  // CHECK:         class_method %0 : $D, #D.foo!1

  // CHECK-LABEL: sil shared @_TTdFC19partial_apply_super1B3fooFS0_FT_T_
  // CHECK:         function_ref @_TFC19partial_apply_super1B3foofS0_FT_T_

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super1D7getBarsfS0_FT_TFT_FT_T_FT_FT_T__
  // CHECK:         function_ref @_TFC19partial_apply_super1D3barFS0_FT_FT_T_
  // CHECK:         function_ref @_TTdFC19partial_apply_super1B3barFS0_FT_FT_T_
  func getBars() -> (() -> () -> (), () -> () -> ()) {
    return (self.bar, super.bar)
  }

  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3barFS0_FT_FT_T_
  // CHECK:         function_ref @_TFC19partial_apply_super1D3barfS0_FT_FT_T_
  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3barfS0_FT_FT_T_
  // CHECK:         class_method %0 : $D, #D.bar!2

  // CHECK-LABEL: sil shared @_TTdFC19partial_apply_super1B3barFS0_FT_FT_T_
  // CHECK:         function_ref @_TTdFC19partial_apply_super1B3barfS0_FT_FT_T_
  // CHECK-LABEL: sil shared @_TTdFC19partial_apply_super1B3barfS0_FT_FT_T_
  // CHECK:         function_ref @_TFC19partial_apply_super1B3barfS0_fT_FT_T_
}
