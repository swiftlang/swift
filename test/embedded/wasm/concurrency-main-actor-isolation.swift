// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/swift-executor.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/cxx-executor.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_ConcurrencyDefaultExecutor -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/swift-executor.out | %FileCheck %s
// RUN: %target-run %t/cxx-executor.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

@main
struct Main {
  static func main() async {
    MainActor.assumeIsolated { print("main: assumed") }
    // CHECK: main: assumed
    await MainActor.run {
      MainActor.assumeIsolated { print("MainActor.run: assumed") }
    }
    // CHECK-NEXT: MainActor.run: assumed
    await Task.detached {
      await MainActor.run {
        MainActor.assumeIsolated { print("detached hop: assumed") }
      }
    }.value
    // CHECK-NEXT: detached hop: assumed
    await Task { @MainActor in
      MainActor.assumeIsolated { print("main-actor task: assumed") }
    }.value
    // CHECK-NEXT: main-actor task: assumed
    print("done")
    // CHECK-NEXT: done
  }
}
