// When a callee-allocated (yield_once_2) coroutine accessor is removed by
// dead-method elimination, its vtable slot is filled with a locally-emitted
// stub coro function pointer (rather than a shared runtime symbol, which would
// be unresolved in Embedded Swift).  Verify that the metadata references the
// stub and that a full link+run works, both in Embedded Swift and outside it.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature CoroutineAccessors -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature CoroutineAccessors -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefix=CHECK-OUT

// Also works outside of Embedded, though dead-method elimination only runs here
// under optimization (Embedded always runs it).  The full non-embedded link+run
// is skipped on wasm, where the SDK lacks non-embedded static-executable link
// args (the -emit-ir check below still exercises the stub there).
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature CoroutineAccessors -O -wmo -emit-ir | %FileCheck %s
// RUN: %if !CPU=wasm32 %{ %target-build-swift %s -parse-as-library -enable-experimental-feature CoroutineAccessors -O -wmo -o %t/a.desktop.out %}
// RUN: %if !CPU=wasm32 %{ %target-run %t/a.desktop.out | %FileCheck %s --check-prefix=CHECK-OUT %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CoroutineAccessors

public class C {
  var _storage: Int = 42

  // Never accessed below, so its coroutine accessors are dead-eliminated and
  // their vtable slots are filled with the dead-method coro stub.
  var value: Int {
    yielding borrow { yield _storage }
    yielding mutate { yield &_storage }
  }

  func use() { print("storage=\(_storage)") }
}

@main
struct Main {
  static func main() {
    let c = C()
    c.use()
  }
}

// The dead coroutine accessor slot references the locally emitted stub rather
// than an undefined external runtime symbol.
// CHECK: @_swift_dead_method_coro_stub

// CHECK-OUT: storage=42
