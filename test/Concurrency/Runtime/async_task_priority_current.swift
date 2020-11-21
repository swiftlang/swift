// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx

import Dispatch

// ==== ------------------------------------------------------------------------
// MARK: "Infrastructure" for the tests

extension DispatchQueue {
  func async<R>(operation: @escaping () async -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: operation)

    // Run the task
    _ = { self.async { handle.run() } }() // force invoking the non-async version

    return handle
  }
}

// ==== ------------------------------------------------------------------------
// MARK: Tests

func test_currentPriority() {
  _ = DispatchQueue.main.async { () async in
    let p = await Task.currentPriority()
    // CHECK: priority: default
    print("priority: \(p)")
    assert(p == Task.Priority.default)
    exit(0)
  }
}

test_currentPriority()

dispatchMain()
