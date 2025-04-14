// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

@available(SwiftStdlib 6.0, *)
func test_withTaskGroup_addUnlessCancelled() async throws {
  let task = Task {
    await withTaskGroup(of: Void.self) { group in
      print("Inner: Sleep...")
      try? await Task.sleep(nanoseconds: 2_000_000_000)
      print("Inner: Task.isCancelled: \(Task.isCancelled)")

      let added = group.addTaskUnlessCancelled {
        print("Added Task! Child Task.isCancelled: \(Task.isCancelled)")
      }
      print("Inner: Task added = \(added)")  // CHECK: Task added = false
    }
  }

  try? await Task.sleep(nanoseconds: 1_000_000)
  print("Outer: Cancel!")
  task.cancel()
  print("Outer: Cancelled")

  await task.value
}

@available(SwiftStdlib 6.0, *)
func test_withDiscardingTaskGroup_addUnlessCancelled() async throws {
  let task = Task {
    await withDiscardingTaskGroup { group in
      print("Inner: Sleep...")
      try? await Task.sleep(nanoseconds: 2_000_000_000)
      print("Inner: Task.isCancelled: \(Task.isCancelled)")

      let added = group.addTaskUnlessCancelled {
        print("Added Task! Child Task.isCancelled: \(Task.isCancelled)")
      }
      print("Inner: Task added = \(added)")  // CHECK: Task added = false
    }
  }

  try? await Task.sleep(nanoseconds: 1_000_000)
  print("Outer: Cancel!")
  task.cancel()
  print("Outer: Cancelled")

  await task.value
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async {
    try! await test_withTaskGroup_addUnlessCancelled()
    try! await test_withDiscardingTaskGroup_addUnlessCancelled()
  }
}
