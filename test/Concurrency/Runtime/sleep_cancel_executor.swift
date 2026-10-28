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

/// A task executor that supports cancelling delayed jobs, and which lets us
/// see how many timers it is currently holding.
@available(SwiftStdlib 6.5, *)
final class CancellableExecutor: TaskExecutor, SchedulingExecutor,
                                 @unchecked Sendable {
  private let queue = DispatchQueue(label: "CancellableExecutor")
  private var pending: [UInt64: UnownedJob] = [:]
  private var nextID: UInt64 = 1
  private var _scheduledResumptions = 0
  private var _enqueuedResumptions = 0

  /// The number of delayed jobs that haven't yet run or been cancelled.
  var pendingCount: Int {
    return queue.sync { pending.count }
  }

  /// How many of the jobs given to `schedule` were resumptions.
  var scheduledResumptions: Int {
    return queue.sync { _scheduledResumptions }
  }

  /// How many of the jobs given to `enqueue` were resumptions.
  var enqueuedResumptions: Int {
    return queue.sync { _enqueuedResumptions }
  }

  public func enqueue(_ _job: consuming ExecutorJob) {
    if _job.isResumption {
      queue.sync { _enqueuedResumptions += 1 }
    }
    let job = UnownedJob(_job)
    DispatchQueue.global().async {
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

    let deadline = DispatchTime.now()
      + .seconds(Int(seconds))
      + .nanoseconds(Int(nanoseconds))

    let isResumption = _job.isResumption
    let job = UnownedJob(_job)
    let id: UInt64 = queue.sync {
      if isResumption {
        _scheduledResumptions += 1
      }
      let id = nextID
      nextID += 1
      pending[id] = job
      return id
    }

    DispatchQueue.global().asyncAfter(deadline: deadline) {
      // Whoever removes the job from `pending` owns it.
      let job = self.queue.sync { self.pending.removeValue(forKey: id) }
      if let job {
        job.runSynchronously(on: self.asUnownedTaskExecutor())
      }
    }

    return JobCancellationToken(canceller: self, opaqueData: [Int(id), 0])
  }

  public func cancel(jobWithToken token: borrowing JobCancellationToken)
    -> Bool {
    let id = UInt64(token.opaqueData[0])
    // Run the job now, if the timer hasn't taken it: here, if all it does is
    // resume a task, and otherwise by enqueuing it.
    guard let job = queue.sync(execute: { pending.removeValue(forKey: id) })
    else {
      return false
    }
    if job.isResumption {
      job.runSynchronously(on: asUnownedTaskExecutor())
    } else {
      enqueue(ExecutorJob(job))
    }
    return true
  }
}

@available(SwiftStdlib 6.5, *)
func waitUntil(_ condition: () -> Bool) async throws {
  var attempts = 0
  while !condition() && attempts < 1000 {
    try await Task.sleep(for: .milliseconds(10))
    attempts += 1
  }
}

@available(SwiftStdlib 6.5, *)
@main struct Main {
  static func main() async {
    let tests = TestSuite("sleep_cancel_executor")

    tests.test("A sleep that isn't cancelled completes and leaves no timer") {
      let executor = CancellableExecutor()

      let task = Task(executorPreference: executor) {
        try await Task.sleep(for: .milliseconds(50))
      }

      try! await task.value
      expectEqual(0, executor.pendingCount)
    }

    tests.test("Cancelling sleeps removes their timers immediately") {
      let executor = CancellableExecutor()
      let count = 100

      var tasks: [Task<Void, any Error>] = []
      for _ in 0..<count {
        tasks.append(Task(executorPreference: executor) {
          try await Task.sleep(for: .seconds(3600))
        })
      }

      try! await waitUntil { executor.pendingCount == count }
      expectEqual(count, executor.pendingCount)

      for task in tasks {
        task.cancel()
      }

      for task in tasks {
        do {
          try await task.value
          expectUnreachable("sleep should have been cancelled")
        } catch is CancellationError {
          // Expected
        } catch {
          expectUnreachable("unexpected error \(error)")
        }
      }

      // The timers must be gone now, rather than at their deadline.
      expectEqual(0, executor.pendingCount)
    }

    tests.test("The job that wakes a sleeping task is a resumption") {
      let executor = CancellableExecutor()

      let task = Task(executorPreference: executor) {
        try await Task.sleep(for: .milliseconds(10))
      }
      try! await task.value

      // The task's own jobs aren't resumptions; the sleep's timer job is, and
      // so are no others.
      expectEqual(1, executor.scheduledResumptions)
      expectEqual(0, executor.enqueuedResumptions)
    }

    tests.test("A cancelled sleep reports the reason the task was cancelled") {
      let executor = CancellableExecutor()

      let task = Task(executorPreference: executor) {
        try await Task.sleep(for: .seconds(3600))
      }

      try! await waitUntil { executor.pendingCount == 1 }
      task.cancel(reason: .deadlineExpired)

      do {
        try await task.value
        expectUnreachable("sleep should have been cancelled")
      } catch let error as CancellationError {
        expectEqual(.deadlineExpired, error.reason)
      } catch {
        expectUnreachable("unexpected error \(error)")
      }
      expectEqual(0, executor.pendingCount)
    }

    tests.test("A sleep cancelled before it starts throws without waiting") {
      let executor = CancellableExecutor()

      let task = Task(executorPreference: executor) {
        while !Task.isCancelled {
          await Task.yield()
        }
        try await Task.sleep(for: .seconds(3600))
      }

      task.cancel()

      do {
        try await task.value
        expectUnreachable("sleep should have been cancelled")
      } catch is CancellationError {
        // Expected
      } catch {
        expectUnreachable("unexpected error \(error)")
      }

      expectEqual(0, executor.pendingCount)
    }

    tests.test("A very short sleep in a cancelled task still throws") {
      // The default executor doesn't create a timer for sleeps this short, so
      // it has nothing to cancel; the sleep must still notice the cancellation.
      for duration in [Duration.nanoseconds(1), .microseconds(10)] {
        let task = Task {
          while !Task.isCancelled {
            await Task.yield()
          }
          try await Task.sleep(for: duration)
        }

        task.cancel()

        do {
          try await task.value
          expectUnreachable("sleep should have been cancelled")
        } catch is CancellationError {
          // Expected
        } catch {
          expectUnreachable("unexpected error \(error)")
        }
      }
    }

    tests.test("Cancelling a sleep as its timer fires resumes it exactly once") {
      let executor = CancellableExecutor()

      for i in 0..<500 {
        let task = Task(executorPreference: executor) {
          try await Task.sleep(for: .microseconds(100))
        }

        if i % 2 == 0 {
          await Task.yield()
        }
        task.cancel()

        // Either outcome is fine, but we must always get one.
        switch await task.result {
        case .success:
          break
        case .failure(let error):
          expectTrue(error is CancellationError)
        }
      }

      expectEqual(0, executor.pendingCount)
    }

    await runAllTestsAsync()
  }
}
