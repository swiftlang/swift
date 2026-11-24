// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Onone -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %if embedded_dispatch_executor %{ %target-embedded-link %t/a.o -o %t/dispatch.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-dispatch-executor-opt %target-embedded-concurrency-threading-shim -dead_strip %}
// RUN: %if embedded_dispatch_executor %{ %target-run %t/dispatch.out | %FileCheck %s %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import _Concurrency

@_silgen_name("swift_task_enqueueMainExecutor")
func enqueueMainExecutor(_ job: UnownedJob)

func keep(_ function: (UnownedJob) -> Void) {}

actor Counter {
  var value = 0

  func bump() {
    value += 1
  }

  // The runtime calls the executor's _swift_concurrency_isMainExecutor before
  // running this deinit.
  isolated deinit {
    print("isolated deinit, value \(value)")
  }
}

@main
struct Main {
  static func main() async {
    keep(enqueueMainExecutor)
    do {
      let counter = Counter()
      await counter.bump()
    }
    print("done")
  }
}

// CHECK: isolated deinit, value 1
// CHECK-NEXT: done
