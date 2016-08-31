// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs/ -I %S/Inputs -enable-source-import %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import objc_extensions_helper

class Sub : Base {}

extension Sub {
  override var prop: String! {
    didSet {
      // Ignore it.
    }
    // CHECK-LABEL: sil hidden [transparent] [thunk] @_TToFC15objc_extensions3Subg4propGSQSS_
    // CHECK: = super_method [volatile] %1 : $Sub, #Base.prop!setter.1.foreign
    // CHECK: = function_ref @_TFC15objc_extensions3SubW4propGSQSS_
    // CHECK: }
  }

  func foo() {
  }

  override func objCBaseMethod() {}
}

// CHECK-LABEL: sil hidden @_TF15objc_extensions20testOverridePropertyFCS_3SubT_
func testOverrideProperty(_ obj: Sub) {
  // CHECK: = class_method [volatile] %0 : $Sub, #Sub.prop!setter.1.foreign : (Sub) -> (String!) -> ()
  obj.prop = "abc"
} // CHECK: }

testOverrideProperty(Sub())

// CHECK-LABEL: sil shared [thunk] @_TFC15objc_extensions3Sub3fooFT_T_
// CHECK:         function_ref @_TTDFC15objc_extensions3Sub3foofT_T_
// CHECK:       sil shared [transparent] [thunk] @_TTDFC15objc_extensions3Sub3foofT_T_
// CHECK:         class_method [volatile] %0 : $Sub, #Sub.foo!1.foreign
func testCurry(_ x: Sub) {
  _ = x.foo
}

extension Sub {
  var otherProp: String {
    get { return "hello" }
    set { }
  }
}

class SubSub : Sub {
// CHECK-LABEL: sil hidden @_TFC15objc_extensions6SubSub14objCBaseMethodfT_T_
// CHECK:  super_method [volatile] %0 : $SubSub, #Sub.objCBaseMethod!1.foreign : (Sub) -> () -> () , $@convention(objc_method) (Sub) -> ()
  override func objCBaseMethod() {
    super.objCBaseMethod()
  }
}

extension SubSub {
  // CHECK-LABEL: sil hidden @_TFC15objc_extensions6SubSubs9otherPropSS
  // CHECK: = super_method [volatile] %1 : $SubSub, #Sub.otherProp!getter.1.foreign
  // CHECK: = super_method [volatile] %1 : $SubSub, #Sub.otherProp!setter.1.foreign
  override var otherProp: String {
    didSet {
      // Ignore it.
    }
  }
}

// SR-1025
extension Base {
  fileprivate static var x = 1
}

// CHECK-LABEL: sil hidden @_TF15objc_extensions19testStaticVarAccessFT_T_
func testStaticVarAccess() {
  // CHECK: [[F:%.*]] = function_ref @_TFE15objc_extensionsCSo4BaseauP33_1F05E59585E0BB585FCA206FBFF1A92D1xSi
  // CHECK: [[PTR:%.*]] = apply [[F]]()
  // CHECK: [[ADDR:%.*]] = pointer_to_address [[PTR]]
  _ = Base.x
}
