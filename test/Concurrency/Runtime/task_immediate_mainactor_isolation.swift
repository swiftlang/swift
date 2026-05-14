// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -swift-version 6 -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: foundation
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// Task.immediate closure must preserve @MainActor isolation
// across await suspension points. Regression: after await withCheckedContinuation,
// the task resumes on the cooperative pool instead of @MainActor.

import Foundation

@MainActor
final class MyController {
  var value = 0
  var doneContinuation: CheckedContinuation<Void, Never>?

  func applySnapshot() {
    print("applySnapshot: isMainThread=\(_isMainThread())")
    // CHECK: applySnapshot: isMainThread=true

    Task.immediate {
      print("Task.immediate start: isMainThread=\(_isMainThread())")
      // CHECK: Task.immediate start: isMainThread=true

      await withCheckedContinuation { (continuation: CheckedContinuation<Void, Never>) in
        // Resume from a background thread to force a thread hop
        // Avoid Task to prevent Dispatch sneakily work stealing the new task using the main thread anyway.
        DispatchQueue.global().asyncAfter(deadline: .now() + 0.1) {
          continuation.resume()
        }
      }

      // After resuming from await — must be back on @MainActor (main thread)
      // In the bug, this resumes on com.apple.root.user-initiated-qos.cooperative
      print("after withCheckedContinuation: isMainThread=\(_isMainThread())")
      // CHECK: after withCheckedContinuation: isMainThread=true

      self.value = 2
      self.doneContinuation?.resume()
    }
  }

  func waitUntilDone() async {
    await withCheckedContinuation { self.doneContinuation = $0 }
  }
}

nonisolated func _isMainThread() -> Bool {
  return Thread.isMainThread
}

@main
struct Main {
  static func main() async {
    let c = MyController()
    c.applySnapshot()
    await c.waitUntilDone()

    print("value=\(c.value)")
    // CHECK: value=2
  }
}
