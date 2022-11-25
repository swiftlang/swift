// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
import Darwin

actor Waiter {
  let until: Int
  var count: Int

  var cc: CheckedContinuation<Int, Never>?

  init(until: Int) {
    self.until = until
    self.count = 0
  }

  func increment() {
    self.count += 1
    fputs("> increment (\(self.count)/\(self.until))\n", stderr);
    if self.until <= self.count {
      if let cc = self.cc {
        cc.resume(returning: self.count)
      }
    }
  }

  func wait() async -> Int {
    if self.until <= self.count {
      fputs("> RETURN in Waiter\n", stderr);
      return self.count
    }

    return await withCheckedContinuation { cc in
      fputs("> WAIT in Waiter\n", stderr);
      self.cc = cc
    }
  }
}

@available(SwiftStdlib 5.1, *)
func test_taskPool_neverConsume() async {
  let until = 100
  let waiter = Waiter(until: until)

  let allTasks = await withTaskPool(returning: Int.self) { pool in
    for n in 1...until {
    fputs("> enqueue: \(n)\n", stderr);
      pool.addTask {
        fputs("> run: \(n) (cancelled: \(Task.isCancelled))\n", stderr);
//        try? await Task.sleep(until: .now + .milliseconds(100), clock: .continuous)
        await waiter.increment()
        fputs("> done: \(n) (cancelled: \(Task.isCancelled))\n", stderr);
      }
    }

    // wait a little bit, so some tasks complete before we hit the implicit "wait at end of task group scope"
    try? await Task.sleep(until: .now + .milliseconds(500), clock: .continuous)

    pool.cancelAll()
    return until
  }

  // CHECK: all tasks: 100
  print("all tasks: \(allTasks)")
  print("actor: \(allTasks)")
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskPool_neverConsume()
  }
}
