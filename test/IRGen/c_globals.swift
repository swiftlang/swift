// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir -Xcc -mno-omit-leaf-frame-pointer | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-cpu

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
// CHECK: define {{.*}}void @"$s9c_globals17testCaptureGlobalyyF"() [[SWIFT_FUNC_ATTR:#[0-9]+]] {
public func testCaptureGlobal() {
  var f: Float = 0
  var i: CInt = 0
  var s: UnsafePointer<CChar>! = nil
  // CHECK-LABEL: define internal swiftcc void @"$s9c_globals17testCaptureGlobalyyFyycfU_{{.*}} {
  blackHole({ () -> Void in
    // CHECK: @staticFloat
    // CHECK: @staticInt
    // CHECK: @staticString
    f = c_layout.staticFloat
    i = c_layout.staticInt
    s = c_layout.staticString
  }) // CHECK: {{^}$}}
}

// CHECK-i386-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-i386-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-x86_64-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-x86_64-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-armv7-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-armv7s-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7s-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-armv7k-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7k-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-arm64-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="non-leaf"{{.*}}
// CHECK-arm64-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="non-leaf" {{.*}}"target-cpu"

// CHECK-arm64e-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="non-leaf"{{.*}}
// CHECK-arm64e-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="non-leaf" {{.*}}"target-cpu"
