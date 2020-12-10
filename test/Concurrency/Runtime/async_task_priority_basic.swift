// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

import Dispatch

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

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

func test_getPriority() {
  _ = DispatchQueue.main.async { () async in
    let p = await Task.currentPriority()
    // CHECK: priority: default
    print("priority: \(p)")
    assert(p == Task.Priority.default)
    exit(0)
  }
}

test_getPriority()

dispatchMain()
