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
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif
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
    -> JobCancellationResult {
    let id = UInt64(token.opaqueData[0])
    // Run the job now, if the timer hasn't taken it: here, if all it does is
    // resume a task, and otherwise by enqueuing it.  (This executor doesn't give
    // jobs back, which is allowed.)
    guard let job = queue.sync(execute: { pending.removeValue(forKey: id) })
    else {
      return .cancelled
    }
    if job.isResumption {
      job.runSynchronously(on: asUnownedTaskExecutor())
    } else {
      enqueue(ExecutorJob(job))
    }
    return .cancelled
  }
}

/// An executor that handles its timers and its cancellations on one serial
/// queue, which stands in for an event loop's thread.  Because they are
/// serialised, it needs no atomic of its own to decide which of the timer and
/// `cancel` gets a job: it asks the job, with `claimForTimer()`.  It also
/// identifies a job in its tokens by its address, instead of by an ID that it
/// has to look up, which is only safe because a job that is cancelled is not
/// run or freed until `cancel` runs it.
@available(SwiftStdlib 6.5, *)
final class LoopExecutor: TaskExecutor, SchedulingExecutor,
                          @unchecked Sendable {
  private let loop = DispatchQueue(label: "LoopExecutor")

  // These are only touched on `loop`.  A job's address can be reused once it
  // has run, so a timer is identified by the address and a generation number.
  private var timers: [UInt: UInt64] = [:]
  private var _refusals = 0

  private let generationQueue = DispatchQueue(label: "LoopExecutor.generation")
  private var nextGeneration: UInt64 = 1
  private var _storageChecks = 0
  private var _storageProblems = 0
  private var _cancelCalls = 0

  private var _afterFiring: (() -> Void)?

  /// Called on the loop, straight after the job for a timer that has fired has
  /// been run.
  var afterFiring: (() -> Void)? {
    get { generationQueue.sync { _afterFiring } }
    set { generationQueue.sync { _afterFiring = newValue } }
  }

  /// How many times we have been asked to cancel a job.
  var cancelCalls: Int { generationQueue.sync { _cancelCalls } }

  /// How many times we have checked the scheduled storage that jobs have, and
  /// how many of those found something wrong with it.
  var storageChecks: Int { generationQueue.sync { _storageChecks } }
  var storageProblems: Int { generationQueue.sync { _storageProblems } }

  private func checked(_ ok: Bool) {
    generationQueue.sync {
      _storageChecks += 1
      if !ok { _storageProblems += 1 }
    }
  }

  /// Keep `generation` in the job's own storage for us, instead of in a
  /// dictionary, checking that the job has eight words of it, and that it
  /// starts out zeroed.
  private func store(_ generation: UInt64, in job: UnownedJob) {
    let ok: Bool = job.withUnsafeScheduledPrivateData { storage in
      guard let storage else {
        return false
      }
      var zeroed = true
      for i in 0..<8 {
        zeroed = zeroed && storage.load(fromByteOffset: i * MemoryLayout<UInt>.stride,
                                        as: UInt.self) == 0
      }
      storage.storeBytes(of: generation, as: UInt64.self)
      return storage.count == 8 * MemoryLayout<UInt>.stride && zeroed
    }
    checked(ok)
  }

  /// Check that the job still has the generation that we stored in it.  This
  /// can only be done when we know that the job hasn't been run.
  private func expect(_ generation: UInt64, in job: UnownedJob) {
    let ok: Bool = job.withUnsafeScheduledPrivateData { storage in
      guard let storage else {
        return false
      }
      return storage.load(as: UInt64.self) == generation
    }
    checked(ok)
  }

  /// The number of timers that haven't fired or been cancelled.
  var pendingCount: Int {
    return loop.sync { timers.count }
  }

  /// How many times a timer found that it had lost the job to a cancellation.
  var refusals: Int {
    return loop.sync { _refusals }
  }

  /// Wait, by polling, until there are `count` timers pending, and say whether
  /// there are.
  func waitForPendingCount(_ count: Int) -> Bool {
    var attempts = 0
    while pendingCount != count && attempts < 5000 {
      usleep(1000)
      attempts += 1
    }
    return pendingCount == count
  }

  /// Stop the loop from running anything until the returned function is
  /// called.  Anything that is queued in the meantime, including timers that
  /// fire, runs in the order that it was queued when the loop resumes.
  func pauseLoop() -> () -> Void {
    let paused = DispatchSemaphore(value: 0)
    let resume = DispatchSemaphore(value: 0)
    loop.async {
      paused.signal()
      resume.wait()
    }
    paused.wait()
    return { resume.signal() }
  }

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    // The job of a task has no storage of this kind, and the continuation of a
    // sleeping task, which does, comes to `schedule` and not to this.  (A clock
    // that hops through a trampoline would enqueue one here, and none of these
    // tests uses one.)
    checked(job.withUnsafeScheduledPrivateData { $0 == nil })
    loop.async {
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
    let duration = fireTime.asDuration(clock: clock) as! Swift.Duration
    let (seconds, attoseconds) = duration.components
    let deadline = DispatchTime.now()
      + .seconds(Int(seconds))
      + .nanoseconds(Int(attoseconds / 1_000_000_000))

    let job = UnownedJob(_job)
    let address = unsafeBitCast(job, to: UInt.self)
    let generation: UInt64 = generationQueue.sync {
      let generation = nextGeneration
      nextGeneration += 1
      return generation
    }

    store(generation, in: job)
    loop.async {
      self.timers[address] = generation
    }
    loop.asyncAfter(deadline: deadline) {
      // `cancel` has already taken the job if it isn't here, and it may have
      // been run and freed, so we mustn't touch it.
      guard self.timers[address] == generation else {
        return
      }
      self.timers[address] = nil

      if job.claimForTimer() {
        // The job is ours, so its storage is too.
        self.expect(generation, in: job)
        job.runSynchronously(on: self.asUnownedTaskExecutor())
        self.afterFiring?()
      } else {
        // The job was cancelled first, so `cancel` has been, or is about to be,
        // called, and it will run the job.
        self._refusals += 1
      }
    }

    return JobCancellationToken(
      canceller: self,
      opaqueData: [Int(bitPattern: address), Int(generation)])
  }

  public func cancel(jobWithToken token: borrowing JobCancellationToken)
    -> JobCancellationResult {
    generationQueue.sync { _cancelCalls += 1 }
    let address = UInt(bitPattern: token.opaqueData[0])
    let generation = UInt64(token.opaqueData[1])
    let job = unsafeBitCast(address, to: UnownedJob.self)

    // Do this on the loop, which is the only place that the timers are
    // touched, and don't wait for it: we mustn't block here.  The job runs
    // whether or not the timer has already taken it out of `timers`.
    loop.async {
      if self.timers[address] == generation {
        self.timers[address] = nil
      }
      // The job can't have been run, so it is still there.
      self.expect(generation, in: job)
      if job.isResumption {
        job.runSynchronously(on: self.asUnownedTaskExecutor())
      } else {
        self.enqueue(ExecutorJob(job))
      }
    }
    return .cancelled
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

    tests.test("A timer that loses the job to a cancellation leaves it to cancel") {
      let executor = LoopExecutor()

      for _ in 0..<10 {
        let refusals = executor.refusals
        let task = Task(executorPreference: executor) {
          try await Task.sleep(for: .milliseconds(100))
        }
        guard executor.waitForPendingCount(1) else {
          expectUnreachable("the sleep's timer was never scheduled")
          task.cancel()
          _ = await task.result
          return
        }

        // Hold the loop while the deadline passes, so that the timer's
        // closure is queued before anything that cancelling queues; then
        // cancel, so that the timer finds the job already claimed.
        let resume = executor.pauseLoop()
        usleep(250_000)
        task.cancel()
        resume()

        switch await task.result {
        case .success:
          expectUnreachable("the cancellation should have won")
        case .failure(let error):
          expectTrue(error is CancellationError)
        }
        expectEqual(refusals + 1, executor.refusals)
        expectEqual(0, executor.pendingCount)
        expectEqual(0, executor.storageProblems)
      }
    }

    tests.test("A cancellation that is queued before its timer fires runs the job") {
      let executor = LoopExecutor()

      for _ in 0..<10 {
        let refusals = executor.refusals
        let task = Task(executorPreference: executor) {
          try await Task.sleep(for: .milliseconds(100))
        }
        guard executor.waitForPendingCount(1) else {
          expectUnreachable("the sleep's timer was never scheduled")
          task.cancel()
          _ = await task.result
          return
        }

        // This time `cancel` is queued first, so the timer's closure finds
        // the timer gone, and must leave the job, which may be gone too,
        // alone.
        let resume = executor.pauseLoop()
        task.cancel()
        usleep(250_000)
        resume()

        switch await task.result {
        case .success:
          expectUnreachable("the cancellation should have won")
        case .failure(let error):
          expectTrue(error is CancellationError)
        }
        expectEqual(refusals, executor.refusals)
        expectEqual(0, executor.pendingCount)
        expectEqual(0, executor.storageProblems)
      }
    }

    tests.test("The runtime does not cancel the job of a timer that has fired") {
      // Tokens for the continuation of a sleeping task are only the address of
      // the job, so an executor relies on the runtime not to ask it to cancel
      // one that has run.  Cancel the task in the moment between the timer's
      // job running, which resumes the continuation, and the task getting going
      // again, when its cancellation handler is still there.
      final class Holder: @unchecked Sendable {
        private let queue = DispatchQueue(label: "Holder")
        private var _task: Task<Bool, any Error>?
        var task: Task<Bool, any Error>? {
          get { queue.sync { _task } }
          set { queue.sync { _task = newValue } }
        }
        private var _hookRan = false
        var hookRan: Bool {
          get { queue.sync { _hookRan } }
          set { queue.sync { _hookRan = newValue } }
        }
      }
      let holder = Holder()
      let executor = LoopExecutor()
      executor.afterFiring = {
        holder.hookRan = true
        holder.task?.cancel()
      }

      let task = Task(executorPreference: executor) { () -> Bool in
        // Don't start the sleep until the hook can see the task, however long
        // that takes.
        while holder.task == nil {
          await Task.yield()
        }
        try await Task.sleep(for: .milliseconds(20))
        return Task.isCancelled
      }
      holder.task = task

      // The sleep finishes normally, and the task has been cancelled by the
      // time that it knows.
      let wasCancelled = try! await task.value
      // (If the timer's job resumed the task inline, it would be finished
      // before the hook cancelled it, and this is what would say so.)
      expectTrue(holder.hookRan, "the task finished before the timer's hook ran")
      expectTrue(wasCancelled)
      expectEqual(0, executor.cancelCalls)
      expectEqual(0, executor.storageProblems)
    }

    tests.test("A scheduler that serialises its timers needs no claim of its own") {
      let executor = LoopExecutor()

      for i in 0..<1000 {
        let task = Task(executorPreference: executor) {
          try await Task.sleep(for: .microseconds(100))
        }

        // Vary when we cancel, so that it lands before, around and after the
        // time that the timer fires.
        let start = ContinuousClock.now
        while ContinuousClock.now - start < .microseconds((i * 7) % 200) {}
        task.cancel()

        // Either outcome is fine, but we must always get exactly one.
        switch await task.result {
        case .success:
          break
        case .failure(let error):
          expectTrue(error is CancellationError)
        }
      }

      expectEqual(0, executor.pendingCount)
      expectEqual(0, executor.storageProblems)
      expectTrue(executor.storageChecks > 0)
      print("timer lost the job to a cancellation \(executor.refusals) times")
    }

    await runAllTestsAsync()
  }
}
