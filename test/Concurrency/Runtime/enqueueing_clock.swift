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
@_spi(ExperimentalScheduling) @_spi(ExperimentalCustomExecutors) import _Concurrency

/// A clock that only moves when it is told to, and which schedules the work
/// that is given to it itself.
@available(SwiftStdlib 6.5, *)
final class ManualClock: EnqueueingClock, JobCanceller, @unchecked Sendable {
  struct Instant: InstantProtocol {
    typealias Duration = Swift.Duration

    var offset: Duration

    func advanced(by duration: Duration) -> Instant {
      return Instant(offset: offset + duration)
    }

    func duration(to other: Instant) -> Duration {
      return other.offset - offset
    }

    static func < (lhs: Instant, rhs: Instant) -> Bool {
      return lhs.offset < rhs.offset
    }
  }

  typealias Duration = Swift.Duration

  private let queue = DispatchQueue(label: "ManualClock")
  private var _now = Instant(offset: .zero)
  private var pending: [UInt64: (deadline: Instant, job: UnownedJob,
                                destination: (any Executor)?)] = [:]
  private var nextID: UInt64 = 1

  /// Where jobs run once the clock has advanced past their deadlines.
  private let target: any Executor

  /// Whether to use the default `enqueue(_:on:...)`, which makes a trampoline
  /// job.  A trampoline is a task, and so doesn't necessarily run on a
  /// single-threaded executor; without it the clock holds the job itself and
  /// enqueues it on the executor directly.
  private let useTrampoline: Bool

  init(target: any Executor = Task.defaultExecutor, useTrampoline: Bool = true) {
    self.target = target
    self.useTrampoline = useTrampoline
  }

  var now: Instant {
    return queue.sync { _now }
  }

  var minimumResolution: Duration { .nanoseconds(1) }

  /// The number of jobs that are waiting for the clock to advance.
  var pendingCount: Int {
    return queue.sync { pending.count }
  }

  func run(_ job: consuming ExecutorJob,
           run: SchedulingExecutorFireTime<ManualClock>,
           tolerance: Duration?) -> JobCancellationToken {
    let deadline = run.asInstant(clock: self)
    let unowned = UnownedJob(job)
    let id: UInt64 = queue.sync {
      let id = nextID
      nextID += 1
      pending[id] = (deadline: deadline, job: unowned, destination: nil)
      return id
    }
    return JobCancellationToken(canceller: self, jobID: id, opaqueData: [0, 0])
  }

  func enqueue(_ job: consuming ExecutorJob,
               on executor: some Executor,
               run: SchedulingExecutorFireTime<ManualClock>,
               tolerance: Duration?) -> JobCancellationToken {
    if useTrampoline {
      let trampoline = job.createTrampoline(to: executor)
      return self.run(trampoline, run: run, tolerance: tolerance)
    }

    let deadline = run.asInstant(clock: self)
    let unowned = UnownedJob(job)
    let id: UInt64 = queue.sync {
      let id = nextID
      nextID += 1
      pending[id] = (deadline: deadline, job: unowned, destination: executor)
      return id
    }
    return JobCancellationToken(canceller: self, jobID: id, opaqueData: [0, 0])
  }

  func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    let id = token.jobID
    if let entry = queue.sync(execute: { pending.removeValue(forKey: id) }) {
      return ExecutorJob(entry.job)
    }
    return nil
  }

  /// Move the clock forward, running everything that has become due.
  func advance(by duration: Duration) {
    let due: [(job: UnownedJob, destination: (any Executor)?)] = queue.sync {
      _now = _now.advanced(by: duration)
      var due: [(job: UnownedJob, destination: (any Executor)?)] = []
      for (id, entry) in pending where !(_now < entry.deadline) {
        due.append((job: entry.job, destination: entry.destination))
        pending.removeValue(forKey: id)
      }
      return due
    }
    for entry in due {
      (entry.destination ?? target).enqueue(ExecutorJob(entry.job))
    }
  }
}

/// A task executor that doesn't run jobs, but passes them to a scheduling
/// executor to be run against some clock.
@available(SwiftStdlib 6.5, *)
final class SchedulingProxy: TaskExecutor, @unchecked Sendable {
  private let lock = DispatchSemaphore(value: 1)
  private var _token: JobCancellationToken?
  private var _scheduled = false
  let clock: ManualClock
  let scheduler: any SchedulingExecutor

  init(clock: ManualClock,
       scheduler: any SchedulingExecutor = Task.defaultExecutor as! any SchedulingExecutor) {
    self.clock = clock
    self.scheduler = scheduler
  }

  func enqueue(_ job: consuming ExecutorJob) {
    lock.wait()
    let alreadyScheduled = _scheduled
    _scheduled = true
    lock.signal()

    if alreadyScheduled {
      // The job has been through the clock; now it can run.  (A task that
      // prefers this executor comes back here whenever it needs to resume.)
      UnownedJob(job).runSynchronously(on: asUnownedTaskExecutor())
      return
    }

    let token = scheduler.enqueue(job, run: .after(.seconds(5)),
                                  tolerance: nil, clock: clock)
    lock.wait()
    _token = consume token
    lock.signal()
  }

  /// Take the token for the job that we scheduled.
  func takeToken() -> JobCancellationToken? {
    lock.wait()
    defer { lock.signal() }
    return _token.take()
  }
}

final class Flag: @unchecked Sendable {
  private let queue = DispatchQueue(label: "Flag")
  private var _value = false
  var value: Bool { queue.sync { _value } }
  func set() { queue.sync { _value = true } }
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
    let tests = TestSuite("enqueueing_clock")

    tests.test("Task.sleep on an EnqueueingClock waits for the clock to advance") {
      let clock = ManualClock()
      let done = Flag()

      let task = Task {
        try await Task.sleep(for: .seconds(10), clock: clock)
        done.set()
      }

      try! await waitUntil { clock.pendingCount == 1 }
      expectEqual(1, clock.pendingCount)

      clock.advance(by: .seconds(9))
      try! await Task.sleep(for: .milliseconds(50))
      expectFalse(done.value)
      expectEqual(1, clock.pendingCount)

      clock.advance(by: .seconds(1))
      try! await task.value
      expectTrue(done.value)
      expectEqual(0, clock.pendingCount)
    }

    tests.test("Cancelling a sleep on an EnqueueingClock takes the timer back") {
      let clock = ManualClock()
      let count = 50

      var tasks: [Task<Void, any Error>] = []
      for _ in 0..<count {
        tasks.append(Task {
          try await Task.sleep(for: .seconds(3600), clock: clock)
        })
      }

      try! await waitUntil { clock.pendingCount == count }
      expectEqual(count, clock.pendingCount)

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

      // The timers are gone now, not at their deadline.
      expectEqual(0, clock.pendingCount)
    }

    tests.test("A sleep in a cancelled task on an EnqueueingClock throws at once") {
      let clock = ManualClock()

      let task = Task {
        while !Task.isCancelled {
          await Task.yield()
        }
        try await Task.sleep(for: .seconds(3600), clock: clock)
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

      expectEqual(0, clock.pendingCount)
    }

    tests.test("A scheduling executor hands jobs for an unknown clock to the clock") {
      let clock = ManualClock()
      let proxy = SchedulingProxy(clock: clock)
      let ran = Flag()

      let task = Task(executorPreference: proxy) { () -> Int in
        ran.set()
        return 42
      }

      // The executor has given the job to the clock, so it hasn't run yet.
      try! await waitUntil { clock.pendingCount == 1 }
      expectEqual(1, clock.pendingCount)
      try! await Task.sleep(for: .milliseconds(50))
      expectFalse(ran.value)

      clock.advance(by: .seconds(5))
      expectEqual(42, await task.value)
      expectTrue(ran.value)
      expectEqual(0, clock.pendingCount)
    }

    tests.test("Cancelling a job scheduled via an unknown clock gives back a job that still runs the original") {
      let clock = ManualClock()
      let proxy = SchedulingProxy(clock: clock)

      let task = Task(executorPreference: proxy) { () -> Int in
        return 7
      }

      try! await waitUntil { clock.pendingCount == 1 }
      let token = proxy.takeToken()!
      guard let job = token.cancel() else {
        expectUnreachable("the clock should have given the job back")
        return
      }
      expectEqual(0, clock.pendingCount)

      // What we get back stands in for the original job; running it runs the
      // task.
      Task.defaultExecutor.enqueue(job)
      expectEqual(7, await task.value)
    }

    tests.test("A scheduling executor hands continuations for an unknown clock to the clock") {
      let clock = ManualClock()
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor

      let advancer = Task {
        try await waitUntil { clock.pendingCount == 1 }
        clock.advance(by: .seconds(5))
      }

      var token: JobCancellationToken? = nil
      try! await withUnsafeThrowingContinuation { (c: UnsafeContinuation<Void, any Error>) in
        token = scheduler.enqueue(resuming: TimerContinuation(c),
                                  run: .after(.seconds(5)),
                                  tolerance: nil, clock: clock)
      }
      try! await advancer.value
      expectEqual(0, clock.pendingCount)
      token = nil
    }

    tests.test("A continuation scheduled via an unknown clock can be taken back") {
      let clock = ManualClock()
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor

      do {
        try await withUnsafeThrowingContinuation { (c: UnsafeContinuation<Void, any Error>) in
          let token = scheduler.enqueue(resuming: TimerContinuation(c),
                                        run: .after(.seconds(3600)),
                                        tolerance: nil, clock: clock)
          expectEqual(1, clock.pendingCount)

          // We get the continuation back, and the clock no longer holds it.
          guard let timer = token.cancelContinuation() else {
            expectUnreachable("the clock should have given the continuation back")
            c.resume(throwing: CancellationError())
            return
          }
          expectEqual(0, clock.pendingCount)
          timer.resume(throwing: CancellationError())
        }
        expectUnreachable("should have thrown")
      } catch is CancellationError {
        // Expected
      } catch {
        expectUnreachable("unexpected error \(error)")
      }
    }

    tests.test("CooperativeExecutor: a sleep completes, then run() returns") {
      let exec = CooperativeExecutor()
      let done = Flag()

      _ = Task(executorPreference: exec) {
        try await Task.sleep(for: .milliseconds(20))
        done.set()
      }

      try! exec.run()
      expectTrue(done.value)
    }

    tests.test("CooperativeExecutor: cancelling a sleep removes its timer") {
      let exec = CooperativeExecutor()
      let started = Flag()

      let sleeper = Task(executorPreference: exec) { () -> Void in
        started.set()
        try await Task.sleep(for: .seconds(3600))
      }

      _ = Task(executorPreference: exec) {
        while !started.value {
          await Task.yield()
        }
        // Give the sleeper time to start waiting.
        for _ in 0..<3 {
          await Task.yield()
        }
        sleeper.cancel()
      }

      // If the timer were left behind, run() would wait for it for an hour.
      let start = ContinuousClock.now
      try! exec.run()
      expectTrue(ContinuousClock.now - start < .seconds(30))

      switch await sleeper.result {
      case .success:
        expectUnreachable("sleep should have been cancelled")
      case .failure(let error):
        expectTrue(error is CancellationError)
      }
    }

    tests.test("CooperativeExecutor: jobs for an unknown clock go to the clock") {
      let exec = CooperativeExecutor()
      let clock = ManualClock(target: exec, useTrampoline: false)
      let proxy = SchedulingProxy(clock: clock, scheduler: exec)
      let ran = Flag()

      _ = Task(executorPreference: proxy) {
        ran.set()
      }
      expectEqual(1, clock.pendingCount)

      _ = Task(executorPreference: exec) {
        expectFalse(ran.value)
        clock.advance(by: .seconds(5))
      }

      try! exec.run()
      expectTrue(ran.value)
      expectEqual(0, clock.pendingCount)
    }

    tests.test("CooperativeExecutor: continuations for an unknown clock go to the clock") {
      let exec = CooperativeExecutor()
      let clock = ManualClock(target: exec)
      let done = Flag()

      _ = Task(executorPreference: exec) {
        try await withUnsafeThrowingContinuation { (c: UnsafeContinuation<Void, any Error>) in
          _ = exec.enqueue(resuming: TimerContinuation(c),
                           run: .after(.seconds(5)),
                           tolerance: nil, clock: clock)
        }
        done.set()
      }

      _ = Task(executorPreference: exec) {
        while clock.pendingCount == 0 {
          await Task.yield()
        }
        expectFalse(done.value)
        clock.advance(by: .seconds(5))
      }

      try! exec.run()
      expectTrue(done.value)
      expectEqual(0, clock.pendingCount)
    }

    tests.test("Cancelling a Dispatch continuation timer as a job gives back a job that resumes it") {
      // A token can end up being cancelled as the other kind of thing from
      // what it was issued for; the executor must not hand back garbage.
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor

      try! await withUnsafeThrowingContinuation { (c: UnsafeContinuation<Void, any Error>) in
        let token = scheduler.enqueue(resuming: TimerContinuation(c),
                                      run: .after(.seconds(3600)),
                                      tolerance: nil, clock: ContinuousClock())
        guard let job = token.cancel() else {
          expectUnreachable("the executor should have given the timer back")
          c.resume()
          return
        }

        // Running the job resumes the continuation.
        Task.defaultExecutor.enqueue(job)
      }
    }

    tests.test("CooperativeExecutor: cancelling a continuation timer as a job gives back a job that resumes it") {
      let exec = CooperativeExecutor()
      let done = Flag()

      _ = Task(executorPreference: exec) {
        try await withUnsafeThrowingContinuation { (c: UnsafeContinuation<Void, any Error>) in
          let token = exec.enqueue(resuming: TimerContinuation(c),
                                   run: .after(.seconds(3600)),
                                   tolerance: nil, clock: ContinuousClock())
          guard let job = token.cancel() else {
            expectUnreachable("the executor should have given the timer back")
            c.resume()
            return
          }

          // The timer is gone, rather than lost, so running the job resumes
          // the task.
          exec.enqueue(job)
        }
        done.set()
      }

      let start = ContinuousClock.now
      try! exec.run()
      expectTrue(done.value)
      expectTrue(ContinuousClock.now - start < .seconds(30))
    }

    await runAllTestsAsync()
  }
}
