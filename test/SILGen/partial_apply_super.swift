// RUN: %target-swift-frontend -use-native-super-method -emit-silgen %s | FileCheck %s

func doFoo(f: () -> ()) {
  f()
}

class Base {
  func method() { }
  func bar()() { }
  class func classMethod() {}
}

class Derived : Base {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super7Derived6methodfT_T_
  // CHECK:         [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Derived to $Base
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $Derived, #Base.method!1
  // CHECK:         [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]])
  // CHECK:         apply [[DOFOO]]([[PARTIAL_APPLY]])
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super7Derived11classMethodfT_T_
  // CHECK:         [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Derived.Type to $@thick Base.Type
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick Derived.Type, #Base.classMethod!1
  // CHECK:         [[PARTIAL_APPLY:%[0-9]+]] = partial_apply %4(%3) : $@convention(thin) (@thick Base.Type) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super7Derived10getMethodsfT_TFT_T_FT_T__
  // CHECK:         function_ref @_TFC19partial_apply_super7Derived6method
  // CHECK:         super_method %0 : $Derived, #Base.method!1 : Base -> () -> ()
  func getMethods() -> (() -> (), () -> ()) {
    return (self.method, super.method)
  }

  // CHECK-LABEL: sil shared @_TFC19partial_apply_super7Derived6methodFT_T_
  // CHECK:         class_method %0 : $Derived, #Derived.method!1

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super7Derived6getBar
  // CHECK:         function_ref @_TFC19partial_apply_super4Base3barFT_FT_T_
  func getBar() -> (() -> () -> ()) {
    return self.bar
  }
}

// Test partial application of super with local types
let c = {
  class Base {
    func method() {}
    class func classMethod() {}
  }
  class Derived : Base {
    // CHECK-LABEL: sil shared @_TFCF19partial_apply_superU_FT_T_L_7Derived6methodfT_T_
    // CHECK:         [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_
    // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Derived to $Base
    // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $Derived, #<anonymous function>Base.method!1
    // CHECK:         [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]])
    // CHECK:         apply [[DOFOO]]([[PARTIAL_APPLY]])
    override func method() {
      doFoo(super.method)
    }

    // CHECK-LABEL: sil shared @_TZFCF19partial_apply_superU_FT_T_L_7Derived11classMethodfT_T_
    // CHECK:         [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_
    // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Derived.Type to $@thick Base.Type
    // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick Derived.Type, #<anonymous function>Base.classMethod!1
    // CHECK:         [[PARTIAL_APPLY:%[0-9]+]] = partial_apply %4(%3) : $@convention(thin) (@thick Base.Type) -> ()
    // CHECK:         apply [[DOFOO]]([[PARTIAL_APPLY]])
    override class func classMethod() {
      doFoo(super.classMethod)
    }
  }
}
