// RUN: %target-run-simple-swift(%import-libdispatch -parse-as-library)
// RUN: %target-run-simple-swift(%import-libdispatch -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

import Dispatch
import StdlibUnittest
@_spi(ExperimentalScheduling) import _Concurrency

@available(SwiftStdlib 6.5, *)
actor MyActor {
  public func doSleep() async {
    try! await Task.sleep(for: .seconds(0.1))
  }
}

@available(SwiftStdlib 6.5, *)
final class TestExecutor: TaskExecutor, SchedulingExecutor, @unchecked Sendable {
  var asScheduling: SchedulingExecutor? {
    return self
  }

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    DispatchQueue.main.async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  public func supportsScheduling(on clock: SystemClockID) -> Bool {
    return true
  }

  public func schedule<C: Clock>(_ _job: consuming ExecutorJob,
                                 at fireTime: SchedulingExecutorFireTime<C>,
                                 tolerance: C.Duration?,
                                 clock: C) -> JobCancellationToken {
    // Convert to `Swift.Duration`
    let duration = fireTime.asDuration(clock: clock) as! Swift.Duration

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

    return JobCancellationToken(canceller: self, opaqueData: [0, 0])
  }

  // We can't remove jobs from `asyncAfter`, so we never cancel.
  public func cancel(jobWithToken: borrowing JobCancellationToken)
    -> ExecutorJob? {
    return nil
  }
}

@available(SwiftStdlib 6.5, *)
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
