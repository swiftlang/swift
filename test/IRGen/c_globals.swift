// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir -Xcc -mno-omit-leaf-frame-pointer | %FileCheck %s --check-prefix=CHECK -check-prefix CHECK-%target-cpu-%target-abi

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

// CHECK-i386-SYSV-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-i386-SYSV-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
// CHECK-i386-WIN-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-i386-WIN-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-x86_64-SYSV-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-x86_64-SYSV-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
// CHECK-x86_64-WIN-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}
// CHECK-x86_64-WIN-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"target-cpu"

// CHECK-armv7-SYSV-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7-SYSV-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
// CHECK-armv7-WIN-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7-WIN-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-armv7s-SYSV-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7s-SYSV-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
// CHECK-armv7s-WIN-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7s-WIN-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-armv7k-SYSV-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7k-SYSV-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
// CHECK-armv7k-WIN-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-armv7k-WIN-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-arm64-SYSV-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-arm64-SYSV-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
// CHECK-arm64-WIN-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-arm64-WIN-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"

// CHECK-arm64e-SYSV-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-arm64e-SYSV-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
// CHECK-arm64e-WIN-DAG: attributes [[CLANG_FUNC_ATTR]] = { noinline nounwind {{.*}}"frame-pointer"="all"{{.*}}
// CHECK-arm64e-WIN-DAG: attributes [[SWIFT_FUNC_ATTR]] = { {{.*}}"frame-pointer"="all" {{.*}}"target-cpu"
