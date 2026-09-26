// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %if embedded_dispatch_executor %{ %target-embedded-link %t/a.o -o %t/dispatch.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-dispatch-executor-opt %target-embedded-concurrency-threading-shim -dead_strip %}
// RUN: %if embedded_dispatch_executor %{ %target-run %t/dispatch.out | %FileCheck %s %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

actor TestOutput {
  func write(_ message: String) {
    print(message)
  }
}

protocol Go: Actor {
  var name: String { get }
  func go(times: Int, output: TestOutput) async -> Int
}

extension Go {
  func go(times: Int, output: TestOutput) async -> Int {
    for i in 0..<times {
      await output.write("\(name) @ \(i)")
      await Task.yield()
    }
    return times
  }
}

actor One: Go { var name = "One" }
actor Two: Go { var name = "Two" }

func yielding() async {
  let one = One()
  let two = Two()
  let output = TestOutput()
  await withTaskGroup(of: Int.self) { group in
    group.addTask {
      await one.go(times: 5, output: output)
    }
    group.addTask {
      await two.go(times: 5, output: output)
    }
  }
}

enum HomeworkError: Error {
case dogAteIt
}

func throwing() async throws {
  print("Ready to throw")
  let one = One()
  let two = Two()
  let output = TestOutput()
  try await withThrowingTaskGroup(of: Int.self) { group in
    group.addTask {
      await one.go(times: 5, output: output)
    }
    group.addTask {
      throw HomeworkError.dogAteIt
    }

    _ = try await group.next()
    _ = try await group.next()
  }
}

@main struct Main {
  static func main() async {
    await yielding()
    print("All done!")
    // CHECK-DAG: One @ 0
    // CHECK-DAG: Two @ 0
    // CHECK-DAG: One @ 1
    // CHECK-DAG: Two @ 1
    // CHECK-DAG: One @ 2
    // CHECK-DAG: Two @ 2
    // CHECK-DAG: One @ 3
    // CHECK-DAG: Two @ 3
    // CHECK-DAG: One @ 4
    // CHECK-DAG: Two @ 4
    // CHECK: All done!

    // CHECK: Ready to throw
    do {
      try await throwing()
    } catch let error as HomeworkError {
      // CHECK: Caught HomeworkError
      print("Caught HomeworkError")
    } catch {
      fatalError("Couldn't match HomeworkError")
    }
  }
}
