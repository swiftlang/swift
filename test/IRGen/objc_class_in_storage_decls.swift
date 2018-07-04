// RUN: %target-swift-frontend -emit-ir -enable-objc-interop -disable-objc-attr-requires-foundation-module %s | %FileCheck %s

struct WrapperStruct {
  subscript() -> Void {
    unsafeAddress {
      // CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructlu9subscriptFT_T_L_31ClassInSubscriptUnsafeAddressor
      @objc class ClassInSubscriptUnsafeAddressor {}
      fatalError()
    }
    unsafeMutableAddress {
      // CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructau9subscriptFT_T_L_38ClassInSubscriptUnsafeMutableAddressor
      @objc class ClassInSubscriptUnsafeMutableAddressor {}
      fatalError()
    }
  }

  subscript(x: Void) -> Void {
    get {
      // CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructg9subscriptFTT__T_L_22ClassInSubscriptGetter
      @objc class ClassInSubscriptGetter {}
    }
    set {
      // CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructs9subscriptFTT__T_L_22ClassInSubscriptSetter
      @objc class ClassInSubscriptSetter {}
    }
  }

  private subscript(privateSubscript x: Void) -> Void {
    // CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructgP33_B60F3A889B72E7873A11885C46E5DF289subscriptFT16privateSubscriptT__T_L_23ClassInPrivateSubscript
    @objc class ClassInPrivateSubscript {}
  }

  var foo: Void {
    get {
      // CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructg3fooT_L_21ClassInVariableGetter
      @objc class ClassInVariableGetter {}
    }
    set {
      // CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructs3fooT_L_21ClassInVariableSetter
      @objc class ClassInVariableSetter {}
    }
  }

  private var privateVariable: Void {
	// CHECK: @_METACLASS_DATA__TtCFV27objc_class_in_storage_decls13WrapperStructgP33_B60F3A889B72E7873A11885C46E5DF2815privateVariableT_L_22ClassInPrivateVariable
    @objc class ClassInPrivateVariable {}
  }
}

