// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | FileCheck %s

import c_layout

@inline(never)
func blackHole<T>(t: T) { }

// CHECK: @staticFloat = internal global float 1.700000e+01, align 4
// CHECK: define {{.*}}void @doubleTrouble() [[CLANG_FUNC_ATTR:#[0-9]+]] {

public func testStaticGlobal() {
  blackHole(c_layout.staticFloat)
  doubleTrouble()
  blackHole(c_layout.staticFloat)
}

// rdar://problem/21361469
// CHECK: define {{.*}}void @_TF9c_globals17testCaptureGlobalFT_T_() [[SWIFT_FUNC_ATTR:#[0-9]+]] {
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

// CHECK: attributes [[SWIFT_FUNC_ATTR]] = { "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "target-cpu"
// CHECK: attributes [[CLANG_FUNC_ATTR]] = { inlinehint nounwind ssp uwtable "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "target-cpu"
