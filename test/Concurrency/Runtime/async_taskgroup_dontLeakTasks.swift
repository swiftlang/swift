// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s

// Task group addTask is not supported in freestanding mode
// UNSUPPORTED: freestanding

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency

actor SimpleCountDownLatch {
  let from: Int
  var count: Int

  var continuations: [CheckedContinuation<Void, Never>] = []

  init(from: Int) {
    self.from = from
    self.count = from
  }

  func hit() {
    defer { count -= 1 }
    if count == 0 {
      fatalError("Counted down more times than expected! (From: \(from))")
    } else if count == 1 {
      for continuation in continuations {
        continuation.resume()
      }
      continuations = []
    }
  }

  func wait() async {
    guard self.count > 0 else {
      return // we're done
    }

    return await withCheckedContinuation { cc in
      self.continuations.append(cc)
    }
  }
}

// Use this class to detect if the values are retained longer than necessary
final class Something {
  let int: Int
  let latch: SimpleCountDownLatch

  init(int: Int, latch: SimpleCountDownLatch) {
    self.int = int
    self.latch = latch
  }

  deinit {
    Task { [latch] in await latch.hit() }
  }
}

func test_taskGroup_next(latch: SimpleCountDownLatch) async {
  let tasks = 5
  _ = await withTaskGroup(of: Something.self, returning: Int.self) { group in
    for n in 0..<tasks {
      group.addTask {
        Something(int: n, latch: latch)
      }
    }

    var sum = 0
    for await value in group {
      // Uncomment to simulate a leak and verify the detection actually works:
      // _ = Unmanaged.passRetained(value)
      sum += value.int
    }

    return sum
  }
}

// Wait until all instances have been released
@available(SwiftStdlib 6.0, *)
func waitForReleaseOrTimeout(latch: SimpleCountDownLatch) async {
  await withTaskGroup(of: Bool.self) { group in
    group.addTask {
      await latch.wait()
      return false // completed: not a timeout
    }
    group.addTask {
      // 10s is far longer than the ms-scale deinit timing, so this never
      // trips for a non-leak even on slow CI; woken early via cancelAll()
      try? await Task.sleep(for: .seconds(10))
      return true // timed out
    }

    let timedOut = await group.next()! // take first completed task
    group.cancelAll() // wakeup the sleeping task

    if timedOut {
      print("LEAKED: not all Something instances were released")
      fatalError("Leak detected: latch did not reach zero within deadline")
    }
  }
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async {
    let tasks = 5
    let latch = SimpleCountDownLatch(from: tasks)

    // Taskgroup that returns some instances:
    await test_taskGroup_next(latch: latch)

    // Wait for the instances deinit to actually run:
    await waitForReleaseOrTimeout(latch: latch)

    print("done") // CHECK: done
  }
}
