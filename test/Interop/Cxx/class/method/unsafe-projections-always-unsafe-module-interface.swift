// RUN: %target-swift-ide-test -print-module -module-to-print=UnsafeProjections -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-experimental-feature ImportUnsafeCxxMethodsAsAlwaysUnsafe | %FileCheck %s

// REQUIRES: swift_feature_ImportUnsafeCxxMethodsAsAlwaysUnsafe

// With ImportUnsafeCxxMethodsAsAlwaysUnsafe, an unsafe projection keeps its
// original name. The '__<name>Unsafe' spelling is still imported, as a
// deprecated migration stub that renames to the original.

// 'View' is not self-contained, so its projections were never unsafe and are
// untouched by the feature.
// CHECK: struct View {
// CHECK-DAG:   func data() -> UnsafeMutableRawPointer!
// CHECK-DAG:   func name() -> std{{.*}}string
// CHECK: }

// CHECK: struct SelfContained {
// CHECK-DAG:   func name() -> std{{.*}}string
// CHECK-DAG:   func value() -> CInt
// CHECK-DAG:   @available(*, deprecated, renamed: "view()")
// CHECK-DAG:   func __viewUnsafe() -> View
// CHECK-DAG:   func view() -> View
// CHECK-DAG:   @available(*, deprecated, renamed: "pointer()")
// CHECK-DAG:   func __pointerUnsafe() -> UnsafeMutablePointer<CInt>!
// CHECK-DAG:   func pointer() -> UnsafeMutablePointer<CInt>!
// CHECK: }

// Safe methods keep their name and gain no stub.
// CHECK-NOT: __nameUnsafe
// CHECK-NOT: __valueUnsafe
// CHECK-NOT: __selfContainedUnsafe
// CHECK-NOT: __nestedUnsafe
