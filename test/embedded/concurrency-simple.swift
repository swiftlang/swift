// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %if OS=macosx %{ %llvm-nm -a %t/a.out | %FileCheck %s --check-prefix=PAL %}
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %if embedded_dispatch_executor %{ %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/dispatch.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-dispatch-executor-opt %target-embedded-concurrency-threading-shim -dead_strip %}
// RUN: %if embedded_dispatch_executor %{ %llvm-nm -a %t/dispatch.out | %FileCheck %s --check-prefix=PAL %}
// RUN: %if embedded_dispatch_executor %{ %target-run %t/dispatch.out | %FileCheck %s %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

// PAL-DAG: {{_?}}_swift_mutexRecursive_lock
// PAL-DAG: {{_?}}_swift_tls_get
// PAL-DAG: {{_?}}_swift_tls_set
// PAL-DAG: {{_?}}_swift_thread_isMain

public func test() async -> Int {
  print("test")
  let t = Task {
    print("return 42")
    return 42
  }
  print("await")
  let v = await t.value
  print("return")
  return v
}

@main
struct Main {
  static func main() async {
    print("main")
    // CHECK: main
    let t = Task {
      print("task")
      let x = await test()
      print(x == 42 ? "42" : "???")
    }
    print("after task")
    await t.value
    // CHECK-NEXT: after task
    // CHECK-NEXT: task
    // CHECK-NEXT: test
    // CHECK-NEXT: await
    // CHECK-NEXT: return 42
    // CHECK-NEXT: return
    // CHECK-NEXT: 42
  }
}
