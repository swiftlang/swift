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
@available(SwiftStdlib 6.3, *)
final class CancellableExecutor: TaskExecutor, SchedulingExecutor,
                                 @unchecked Sendable {
  private let queue = DispatchQueue(label: "CancellableExecutor")
  private var pending: [UInt64: UnownedJob] = [:]
  private var nextID: UInt64 = 1

  /// The number of delayed jobs that haven't yet run or been cancelled.
  var pendingCount: Int {
    return queue.sync { pending.count }
  }

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    DispatchQueue.global().async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  public func enqueue<C: Clock>(_ _job: consuming ExecutorJob,
                                run: SchedulingExecutorFireTime<C>,
                                tolerance: C.Duration?,
                                clock: C) -> JobCancellationToken {
    // Convert to `Swift.Duration`
    let duration = run.asDuration(clock: clock) as! Swift.Duration

    // Now turn that into nanoseconds
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000

    let deadline = DispatchTime.now()
      + .seconds(Int(seconds))
      + .nanoseconds(Int(nanoseconds))

    let job = UnownedJob(_job)
    let id: UInt64 = queue.sync {
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

    return JobCancellationToken(canceller: self, jobID: id, opaqueData: (0, 0))
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    let id = token.jobID
    if let job = queue.sync(execute: { pending.removeValue(forKey: id) }) {
      return ExecutorJob(job)
    }
    return nil
  }
}

/// Holds a continuation that has been scheduled, until it fires or is taken.
@available(SwiftStdlib 6.3, *)
final class ContinuationBox: @unchecked Sendable {
  var continuation: TimerContinuation?
  init(_ continuation: consuming TimerContinuation) {
    self.continuation = consume continuation
  }
}

/// Like `CancellableExecutor`, but implements the continuation-based
/// requirements natively, so no job is ever created for a sleep.
@available(SwiftStdlib 6.3, *)
final class NativeContinuationExecutor: TaskExecutor, SchedulingExecutor,
                                        @unchecked Sendable {
  private let queue = DispatchQueue(label: "NativeContinuationExecutor")
  private var pending: [UInt64: ContinuationBox] = [:]
  private var nextID: UInt64 = 1

  /// The number of continuations that haven't yet been resumed or taken back.
  var pendingCount: Int {
    return queue.sync { pending.count }
  }

  /// The number of times the job-based requirements were used.
  var jobCount: Int {
    return queue.sync { _jobCount }
  }
  private var _jobCount = 0

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    DispatchQueue.global().async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                run: SchedulingExecutorFireTime<C>,
                                tolerance: C.Duration?,
                                clock: C) -> JobCancellationToken {
    queue.sync { _jobCount += 1 }
    fatalError("Task.sleep should not need a job")
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    return nil
  }

  public func enqueue<C: Clock>(resuming continuation: consuming TimerContinuation,
                                run: SchedulingExecutorFireTime<C>,
                                tolerance: C.Duration?,
                                clock: C) -> JobCancellationToken {
    let duration = run.asDuration(clock: clock) as! Swift.Duration
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000

    let deadline = DispatchTime.now()
      + .seconds(Int(seconds))
      + .nanoseconds(Int(nanoseconds))

    let box = ContinuationBox(continuation)
    let id: UInt64 = queue.sync {
      let id = nextID
      nextID += 1
      pending[id] = box
      return id
    }

    DispatchQueue.global().asyncAfter(deadline: deadline) {
      // Whoever removes the box from `pending` owns the continuation.
      let box = self.queue.sync { self.pending.removeValue(forKey: id) }
      if let box, let continuation = box.continuation.take() {
        continuation.resume()
      }
    }

    return JobCancellationToken(canceller: self, jobID: id, opaqueData: (0, 0))
  }

  public func cancel(continuationWithToken token: consuming JobCancellationToken)
    -> TimerContinuation? {
    let id = token.jobID
    if let box = queue.sync(execute: { pending.removeValue(forKey: id) }) {
      return box.continuation.take()
    }
    return nil
  }
}

@available(SwiftStdlib 6.3, *)
func waitUntil(_ condition: () -> Bool) async throws {
  var attempts = 0
  while !condition() && attempts < 1000 {
    try await Task.sleep(for: .milliseconds(10))
    attempts += 1
  }
}

@available(SwiftStdlib 6.3, *)
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

    tests.test("Continuation-based executors resume sleeps without jobs") {
      let executor = NativeContinuationExecutor()

      let task = Task(executorPreference: executor) {
        try await Task.sleep(for: .milliseconds(50))
      }

      try! await task.value
      expectEqual(0, executor.pendingCount)
      expectEqual(0, executor.jobCount)
    }

    tests.test("Continuation-based executors get cancelled sleeps back") {
      let executor = NativeContinuationExecutor()
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

      expectEqual(0, executor.pendingCount)
      expectEqual(0, executor.jobCount)
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
