// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | FileCheck %s

import c_layout

@inline(never)
func blackHole<T>(t: T) { }

public func testStaticGlobal() {
  blackHole(c_layout.staticFloat)
  doubleTrouble()
  blackHole(c_layout.staticFloat)
}
// CHECK: @staticFloat = internal global float 1.700000e+01, align 4

// rdar://problem/21361469
public func testCaptureGlobal() {
  var f: Float = 0
  var i: CInt = 0
  var s: UnsafePointer<CChar> = nil
  // CHECK-LABEL: define linkonce_odr hidden void @_TFF9c_globals17testCaptureGlobalFT_T_U_FT_T_{{.*}} {
  blackHole({ () -> Void in
    // CHECK: @staticFloat
    // CHECK: @staticInt
    // CHECK: @staticString
    f = c_layout.staticFloat
    i = c_layout.staticInt
    s = c_layout.staticString
  }) // CHECK: {{^}$}}
}
