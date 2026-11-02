// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-cxx-lib -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim %if OS=linux-gnu %{ -Wl,--gc-sections %} %else %{ -dead_strip %}
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

final class InlineExecutor: SerialExecutor {
  static let shared = InlineExecutor()

  func enqueue(_ job: consuming ExecutorJob) {
    print("enqueued on InlineExecutor")
    UnownedJob(job).runSynchronously(on: asUnownedSerialExecutor())
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

actor Counter {
  var value = 0

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    InlineExecutor.shared.asUnownedSerialExecutor()
  }

  func increment() -> Int {
    value += 1
    return value
  }
}

@main
struct Main {
  static func main() async {
    let counter = Counter()
    print("value: \(await counter.increment())")
    // CHECK: enqueued on InlineExecutor
    // CHECK-NEXT: value: 1
  }
}
