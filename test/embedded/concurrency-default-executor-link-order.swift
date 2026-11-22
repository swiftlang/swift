// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o

// RUN: not %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/none.out %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a %target-embedded-concurrency-threading-shim -dead_strip 2>&1 | %FileCheck %s --check-prefix=NONE

// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/after.out %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_ConcurrencyDefaultExecutor.a %target-embedded-concurrency-threading-shim -dead_strip -Wl,-y,_swift_concurrency_enqueueGlobal 2>&1 | %FileCheck %s --check-prefix=CXX-EXECUTOR
// RUN: %target-run %t/after.out | %FileCheck %s

// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/before.out %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_ConcurrencyDefaultExecutor.a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a %target-embedded-concurrency-threading-shim -dead_strip -Wl,-y,_swift_concurrency_enqueueGlobal 2>&1 | %FileCheck %s --check-prefix=CXX-EXECUTOR
// RUN: %target-run %t/before.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

// NONE-DAG: libswift_Concurrency.a(ConcurrencyHooks.cpp.o): undefined symbol: _swift_concurrency_enqueueGlobal
// NONE-DAG: libswift_Concurrency.a(ConcurrencyHooks.cpp.o): undefined symbol: _swift_concurrency_enqueueMainExecutor
// NONE-DAG: libswift_Concurrency.a(Task.cpp.o): undefined symbol: _swift_concurrency_asyncMain
// CXX-EXECUTOR: libswift_ConcurrencyDefaultExecutor.a(CooperativeGlobalExecutor.cpp.o): definition of _swift_concurrency_enqueueGlobal{{$}}

import _Concurrency

@main
struct Main {
  static func main() async {
    let task = Task { 42 }
    print(await task.value == 42 ? "42" : "???")
    // CHECK: 42
  }
}
