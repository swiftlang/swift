// RUN: %target-run-simple-swift(%import-libdispatch -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

import Dispatch
import StdlibUnittest
@_spi(CustomDefaultExecutors) import _Concurrency

@available(SwiftStdlib 6.2, *)
actor MyActor {
  public func doSleep() async {
    try! await Task.sleep(for: .seconds(0.1))
  }
}

@available(SwiftStdlib 6.2, *)
final class TestExecutor: TaskExecutor, SchedulableExecutor, @unchecked Sendable {
  var asSchedulable: SchedulableExecutor? {
    return self
  }

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    DispatchQueue.main.async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  public func enqueue<C: Clock>(_ _job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    // Convert to `Swift.Duration`
    let duration = clock.convert(from: delay)!

    // Now turn that into nanoseconds
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000

    // Get a Dispatch time
    let deadline = DispatchTime.now()
      + .seconds(Int(seconds))
      + .nanoseconds(Int(nanoseconds))

    let job = UnownedJob(_job)
    DispatchQueue.main.asyncAfter(deadline: deadline) {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }
}

@available(SwiftStdlib 6.2, *)
@main struct Main {
  static func main() async {
    let tests = TestSuite("sleep_executor")

    tests.test("Task.sleep on the main executor") {
      try! await Task.sleep(for: .seconds(0.1))
    }

    tests.test("Task.sleep on the global executor") {
      let task = Task.detached {
        try! await Task.sleep(for: .seconds(0.1))
      }

      await task.value
    }

    tests.test("Task.sleep on a custom executor") {
      let taskExecutor = TestExecutor()

      let task = Task(executorPreference: taskExecutor) {
        try! await Task.sleep(for: .seconds(0.1))
      }

      await task.value
    }

    tests.test("Task.sleep on an actor") {
      await MyActor().doSleep()
    }

    await runAllTestsAsync()
  }
}
