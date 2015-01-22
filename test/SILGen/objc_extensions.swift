// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs/ -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import objc_extensions_helper

class Sub : Base {}

extension Sub {
  override var prop: String! {
    didSet {
      println("set!")
    }
    // CHECK-LABEL: sil hidden [transparent] @_TToFC15objc_extensions3Subg4propGSQSS_
    // CHECK: = super_method [volatile] %1 : $Sub, #Base.prop!setter.1.foreign
    // CHECK: = function_ref @_TFC15objc_extensions3SubW4propGSQSS_
    // CHECK: }
  }

  func foo() {
  }
}

// CHECK-LABEL: sil hidden @_TF15objc_extensions20testOverridePropertyFCS_3SubT_
func testOverrideProperty(obj: Sub) {
  // CHECK: = class_method [volatile] %0 : $Sub, #Sub.prop!setter.1
  obj.prop = "abc"
} // CHECK: }

testOverrideProperty(Sub())

// CHECK-LABEL: sil shared @_TFC15objc_extensions3Sub3fooFS0_FT_T_
// CHECK:         function_ref @_TTDFC15objc_extensions3Sub3foofS0_FT_T_
// CHECK:       sil shared [transparent] @_TTDFC15objc_extensions3Sub3foofS0_FT_T_
// CHECK:         class_method [volatile] %0 : $Sub, #Sub.foo!1.foreign
func testCurry(x: Sub) {
  let _ = x.foo
}
