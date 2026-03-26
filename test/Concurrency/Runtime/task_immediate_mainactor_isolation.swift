// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -swift-version 6 -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// rdar://173181913 — Task.immediate closure must preserve @MainActor isolation
// across await suspension points. Regression: after await withCheckedContinuation,
// the task resumes on the cooperative pool instead of @MainActor.

import Foundation

@MainActor
final class MyController {
  var value = 0

  // Inherits @MainActor from the class — no explicit annotation

  func applySnapshot() {
    print("[swift] \(#fileID):\(#line) applySnapshot() — isMainThread: \(Thread.isMainThread)")
    guard _isMainThread() else {
      fatalError("BUG: Expected to be on main actor in applySnapshot()")
    }

    Task.immediate {
      print("[swift] \(#fileID):\(#line) Task.immediate start — isMainThread: \(Thread.isMainThread)")
      guard _isMainThread() else {
        fatalError("BUG: Expected to be on main actor in applySnapshot/Task.immediate intro")
      }

      print("[swift] \(#fileID):\(#line) before withCheckedContinuation — isMainThread: \(Thread.isMainThread)")
      await withCheckedContinuation { (continuation: CheckedContinuation<Void, Never>) in
        print("[swift] \(#fileID):\(#line) inside continuation body — isMainThread: \(Thread.isMainThread)")
        DispatchQueue.global().asyncAfter(deadline: .now() + 1) {
          print("[swift] \(#fileID):\(#line) resuming continuation from DispatchQueue.global() — isMainThread: \(Thread.isMainThread)")
          continuation.resume()
        }
      }

      // After resuming from await — must be back on @MainActor (main thread)
      // In the bug, this resumes on com.apple.root.user-initiated-qos.cooperative
      print("[swift] \(#fileID):\(#line) after withCheckedContinuation — isMainThread: \(Thread.isMainThread)")
      guard _isMainThread() else {
        fatalError("BUG: Task.immediate lost @MainActor isolation after await! Resumed on wrong thread.")
      }

      print("[swift] \(#fileID):\(#line) Task.immediate done — isMainThread: \(Thread.isMainThread)")
      self.value = 2
    }
  }
}

// Helper to check main thread without triggering
// "unavailable from asynchronous contexts" errors

nonisolated func _isMainThread() -> Bool {
  return Thread.isMainThread
}

@main
struct Main {
  static func main() async throws {
    print("[swift] \(#fileID):\(#line) main() start — isMainThread: \(Thread.isMainThread)")
    MainActor.preconditionIsolated()

    let c = MyController()
    c.applySnapshot()

    print("[swift] \(#fileID):\(#line) waiting for Task.immediate to complete...")
    try await Task.sleep(for: .milliseconds(2000))

    let val = c.value  // just to keep alive
    precondition(val == 2, "Expected value to be 2, was \(val)")
    print("OK: Task.immediate preserved @MainActor isolation across await")
  }
}
