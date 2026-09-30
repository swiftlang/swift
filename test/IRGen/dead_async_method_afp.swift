// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -O -module-name A -emit-ir %s -o - | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64
// REQUIRES: concurrency

// Check the _swift_dead_method_async_stub / swift_deletedAsyncMethodError codegen.

private class Base {
  func defAsync() async {}
}
private class Derived : Base {
  override func defAsync() async {}
}
public func makeDerived() -> AnyObject { Derived() }

// The AFP itself references the stub directly, no extra indirection.
// CHECK: @_swift_dead_async_method_error_afp{{[^ ]*}} = {{.*}}constant %swift.async_func_pointer <{ i32 trunc (i64 sub (i64 ptrtoint (ptr @_swift_dead_method_async_stub to i64), i64 ptrtoint (ptr @_swift_dead_async_method_error_afp

// The vtable slot for the eliminated method points directly at the AFP.
// CHECK: ptr @_swift_dead_async_method_error_afp{{[^,]*}},

// Never route through a GOT-like indirection cell.
// CHECK-NOT: @"got.{{.*}}dead_async_method_error_afp{{.*}}"

// The stub tail-calls the runtime function.
// CHECK-LABEL: define {{.*}}swifttailcc void @_swift_dead_method_async_stub(ptr swiftasync %0)
// CHECK: musttail call swifttailcc void @swift_deletedAsyncMethodError(ptr %0)
