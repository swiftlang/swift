// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | %FileCheck %s

import c_layout

@inline(never)
func blackHole<T>(_ t: T) { }

// CHECK: @staticFloat = internal global float 1.700000e+01, align 4
// CHECK: define internal void @doubleTrouble() [[CLANG_FUNC_ATTR:#[0-9]+]] {

public func testStaticGlobal() {
  blackHole(c_layout.staticFloat)
  doubleTrouble()
  blackHole(c_layout.staticFloat)
}

// rdar://problem/21361469
// CHECK: define {{.*}}void @_T09c_globals17testCaptureGlobalyyF() [[SWIFT_FUNC_ATTR:#[0-9]+]] {
public func testCaptureGlobal() {
  var f: Float = 0
  var i: CInt = 0
  var s: UnsafePointer<CChar>! = nil
  // CHECK-LABEL: define linkonce_odr hidden swiftcc void @_T09c_globals17testCaptureGlobalyyFyycfU_{{.*}} {
  blackHole({ () -> Void in
    // CHECK: @staticFloat
    // CHECK: @staticInt
    // CHECK: @staticString
    f = c_layout.staticFloat
    i = c_layout.staticInt
    s = c_layout.staticString
  }) // CHECK: {{^}$}}
}

// CHECK-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"no-frame-pointer-elim"="true"{{.*}}
// CHECK-DAG: attributes [[SWIFT_FUNC_ATTR]] = { "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "target-cpu"
