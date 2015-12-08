// RUN: %target-swift-frontend -use-native-super-method -emit-silgen %s | FileCheck %s

class B {
  func foo() { }
  func bar()() { }
}

class D: B {
  override func foo() { }
  override func bar()() { }

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super1D7getFoos
  // CHECK:         function_ref @_TFC19partial_apply_super1D3foo
  // CHECK:         super_method %0 : $D, #B.foo!1 : B -> () -> ()
  func getFoos() -> (() -> (), () -> ()) {
    return (self.foo, super.foo)
  }

  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3fooFT_T_
  // CHECK:         class_method %0 : $D, #D.foo!1

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super1D6getBar
  // CHECK:         function_ref @_TFC19partial_apply_super1D3bar
  func getBar() -> (() -> () -> ()) {
    return self.bar
  }

  // CHECK-LABEL: sil shared @_TFC19partial_apply_super1D3bar
  // CHECK:         function_ref @_TFC19partial_apply_super1D3bar
}

func doFoo(f: () -> ()) {
  f()
}

class Base {
  func foo() {}
}

class Derived : Base {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super7Derived3foofT_T_
  // CHECK:         [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Derived to $Base
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $Derived, #Base.foo!1
  // CHECK:         [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]])
  // CHECK:         apply [[DOFOO]]([[PARTIAL_APPLY]])
  override func foo() {
    doFoo(super.foo)
  }
}

// Test partial application of super with local types
let c = {
  class Base {
    func foo() {
      print("c.A.foo()")
    }
  }
  class Derived : Base {
    // CHECK-LABEL: sil shared @_TFCF19partial_apply_superU_FT_T_L_7Derived3foofT_T_
    // CHECK:         [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_
    // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Derived to $Base
    // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $Derived, #<anonymous function>Base.foo!1
    // CHECK:         [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]])
    // CHECK:         apply [[DOFOO]]([[PARTIAL_APPLY]])
    override func foo() {
      print("c.B.foo()")
      doFoo(super.foo)
    }
  }
}
