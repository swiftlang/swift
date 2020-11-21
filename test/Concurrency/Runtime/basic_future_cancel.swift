// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import Dispatch

extension DispatchQueue {
  func async<R>(execute: @escaping () async throws -> R) -> Task.Handle<R> {
    let handle = Task.runDetached(operation: execute)

    // Run the task
    _ = { self.async { handle.run() } }()

    return handle
  }
}

func testCancel(shouldThrow: Bool) {
  let queue = DispatchQueue(label: "concurrent", attributes: .concurrent)
  let group = DispatchGroup()
  var cancelled = false

  group.enter()
  let taskHandle = queue.async { () async throws -> String in
    defer {
      group.leave()
    }

    print("- Future sleeping")
    sleep(1)

    if await Task.isCancelled() {
      cancelled = true
      print("Future was cancelled")
      if shouldThrow {
        try await Task.checkCancellation()
      } else {
        return "cancelled"
      }
    }

    print("- Future returning normally")
    return "done"
  }

  print("Cancel")
  taskHandle.cancel()

  group.wait()
  assert(cancelled)

  group.enter()
  queue.async { () async -> () in
    defer {
      group.leave()
    }

    do {
      let result = await try taskHandle.get()
      if !shouldThrow {
        assert(result == "cancelled")
      }
    } catch {
      if shouldThrow {
        assert("\(error)" == "CancellationError()", "Unexpected error: \(error)")
      }
    }
  }

  // CHECK: Cancel
  // CHECK: Future was cancelled
  // CHECK: Finished test


  group.wait()
  assert(cancelled)

  print("Finished test")
}

testCancel(shouldThrow: false)
testCancel(shouldThrow: true)

print("Done")
