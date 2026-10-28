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
final class ManualClock: SchedulingClock, JobCanceller, @unchecked Sendable {
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

  init(target: any Executor = Task.defaultExecutor) {
    self.target = target
  }

  var now: Instant {
    return queue.sync { _now }
  }

  var minimumResolution: Duration { .nanoseconds(1) }

  /// The number of jobs that are waiting for the clock to advance.
  var pendingCount: Int {
    return queue.sync { pending.count }
  }

  func schedule(_ job: consuming ExecutorJob,
                at fireTime: SchedulingExecutorFireTime<ManualClock>,
                tolerance: Duration?) -> JobCancellationToken {
    let deadline = fireTime.asInstant(clock: self)
    let unowned = UnownedJob(job)
    let id: UInt64 = queue.sync {
      let id = nextID
      nextID += 1
      pending[id] = (deadline: deadline, job: unowned, destination: nil)
      return id
    }
    return JobCancellationToken(canceller: self, opaqueData: [Int(id), 0])
  }

  func cancel(jobWithToken token: borrowing JobCancellationToken) -> Bool {
    let id = UInt64(token.opaqueData[0])
    // Run the job now, where it would have run when the clock got there.
    if let entry = queue.sync(execute: { pending.removeValue(forKey: id) }) {
      (entry.destination ?? target).enqueue(ExecutorJob(entry.job))
      return true
    }
    return false
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

/// A clock that only moves when it is told to, and which schedules the work
/// that is given to it itself.
@available(SwiftStdlib 6.5, *)
final class HoldingClock: SchedulingClock, JobCanceller, @unchecked Sendable {
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

  init(target: any Executor) {
    self.target = target
  }

  var now: Instant {
    return queue.sync { _now }
  }

  var minimumResolution: Duration { .nanoseconds(1) }

  /// The number of jobs that are waiting for the clock to advance.
  var pendingCount: Int {
    return queue.sync { pending.count }
  }

  func schedule(_ job: consuming ExecutorJob,
                at fireTime: SchedulingExecutorFireTime<HoldingClock>,
                tolerance: Duration?) -> JobCancellationToken {
    let deadline = fireTime.asInstant(clock: self)
    let unowned = UnownedJob(job)
    let id: UInt64 = queue.sync {
      let id = nextID
      nextID += 1
      pending[id] = (deadline: deadline, job: unowned, destination: nil)
      return id
    }
    return JobCancellationToken(canceller: self, opaqueData: [Int(id), 0])
  }

  func schedule(_ job: consuming ExecutorJob,
                on executor: some Executor,
                at fireTime: SchedulingExecutorFireTime<HoldingClock>,
                tolerance: Duration?) -> JobCancellationToken {
    let deadline = fireTime.asInstant(clock: self)
    let unowned = UnownedJob(job)
    let id: UInt64 = queue.sync {
      let id = nextID
      nextID += 1
      pending[id] = (deadline: deadline, job: unowned, destination: executor)
      return id
    }
    return JobCancellationToken(canceller: self, opaqueData: [Int(id), 0])
  }

  func cancel(jobWithToken token: borrowing JobCancellationToken) -> Bool {
    let id = UInt64(token.opaqueData[0])
    // Run the job now, where it would have run when the clock got there.
    if let entry = queue.sync(execute: { pending.removeValue(forKey: id) }) {
      (entry.destination ?? target).enqueue(ExecutorJob(entry.job))
      return true
    }
    return false
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
final class SchedulingProxy<C: SchedulingClock>: TaskExecutor, @unchecked Sendable
  where C.Duration == Duration {
  private let lock = DispatchSemaphore(value: 1)
  private var _token: JobCancellationToken?
  private var _scheduled = false
  let clock: C
  let scheduler: any SchedulingExecutor

  /// Whether to ask the clock to enqueue the job on the scheduler, rather than
  /// asking the scheduler to schedule it against the clock.
  let askClock: Bool

  init(clock: C,
       scheduler: any SchedulingExecutor = Task.defaultExecutor as! any SchedulingExecutor,
       askClock: Bool = false) {
    self.clock = clock
    self.scheduler = scheduler
    self.askClock = askClock
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

    let token = askClock
      ? clock.schedule(job, on: scheduler, at: .after(.seconds(5)),
                       tolerance: nil)
      : scheduler.schedule(job, at: .after(.seconds(5)),
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

/// An executor that runs jobs, but doesn't schedule against any clock, and
/// says so.
@available(SwiftStdlib 6.5, *)
final class RefusingExecutor: TaskExecutor, SchedulingExecutor,
                              @unchecked Sendable {
  func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    DispatchQueue.global().async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  func supportsScheduling(on clock: SystemClockID) -> Bool {
    return false
  }

  func schedule<C: Clock>(_ job: consuming ExecutorJob,
                          at fireTime: SchedulingExecutorFireTime<C>,
                          tolerance: C.Duration?,
                          clock: C) -> JobCancellationToken {
    fatalError("a standard clock should never be scheduled on this executor")
  }

  func cancel(jobWithToken token: borrowing JobCancellationToken) -> Bool {
    return false
  }
}

final class Counter: @unchecked Sendable {
  private let queue = DispatchQueue(label: "Counter")
  private var _value = 0
  var value: Int { queue.sync { _value } }
  func increment() { queue.sync { _value += 1 } }
}

final class Log: @unchecked Sendable {
  private let queue = DispatchQueue(label: "Log")
  private var _entries: [String] = []
  var entries: [String] { queue.sync { _entries } }
  func append(_ entry: String) { queue.sync { _entries.append(entry) } }
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
    let tests = TestSuite("scheduling_clock")

    tests.test("Task.sleep on a SchedulingClock waits for the clock to advance") {
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

    tests.test("Cancelling a sleep on a SchedulingClock fires the timer early") {
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

    tests.test("A sleep in a cancelled task on a SchedulingClock throws at once") {
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

    tests.test("Cancelling a job scheduled via an unknown clock runs the original job now") {
      let clock = ManualClock()
      let proxy = SchedulingProxy(clock: clock)
      let runs = Counter()

      let task = Task(executorPreference: proxy) { () -> Int in
        runs.increment()
        return 7
      }

      try! await waitUntil { clock.pendingCount == 1 }
      let token = proxy.takeToken()!
      expectTrue(token.cancel())
      expectEqual(0, clock.pendingCount)

      // Cancelling doesn't consume the token, and a second cancel does nothing.
      expectFalse(token.cancel())

      // The clock runs the trampoline that it was holding early, which runs
      // the original job, once.
      expectEqual(7, await task.value)
      try! await Task.sleep(for: .milliseconds(50))
      expectEqual(1, runs.value)
    }

    tests.test("A built-in clock schedules a job on an executor directly, and cancelling runs it now") {
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor
      let proxy = SchedulingProxy(clock: ContinuousClock(), scheduler: scheduler,
                                  askClock: true)
      let runs = Counter()

      let task = Task(executorPreference: proxy) { () -> Int in
        runs.increment()
        return 9
      }

      try! await Task.sleep(for: .milliseconds(50))
      expectEqual(0, runs.value)

      expectTrue(proxy.takeToken()!.cancel())
      expectEqual(9, await task.value)
      expectEqual(1, runs.value)
    }

    tests.test("A scheduling executor hands a continuation for an unknown clock to the clock") {
      let clock = ManualClock()
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor

      let advancer = Task {
        try await waitUntil { clock.pendingCount == 1 }
        clock.advance(by: .seconds(5))
      }

      // A continuation can be scheduled directly.
      try! await withContinuation(
        of: Void.self, throwing: CancellationError.self
      ) { (continuation: consuming Continuation<Void, CancellationError>,
         waiter: consuming ContinuationWaiter<Void, CancellationError>)
        async throws(CancellationError) -> Void in
        _ = scheduler.schedule(continuation,
                               at: .after(.seconds(5)),
                               tolerance: nil, clock: clock)
        try await waiter.wait()
      }

      try! await advancer.value
      expectEqual(0, clock.pendingCount)
    }

    tests.test("A continuation scheduled via an unknown clock can be cancelled") {
      let clock = ManualClock()
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor

      let task = Task { () async throws -> Void in
        try await withContinuation(
          of: Void.self, throwing: CancellationError.self
        ) { (continuation: consuming Continuation<Void, CancellationError>,
           waiter: consuming ContinuationWaiter<Void, CancellationError>)
          async throws(CancellationError) -> Void in
          let scheduled = scheduler.schedule(continuation,
                                             at: .after(.seconds(3600)),
                                             tolerance: nil, clock: clock)
          expectEqual(1, clock.pendingCount)

          // The task is already cancelled, so the timer is cancelled as soon
          // as we wait.
          withUnsafeCurrentTask { $0!.cancel() }
          try await waiter.wait(for: scheduled)
        }
      }

      // The continuation is resumed with a cancellation error, and the clock
      // no longer holds the job.
      switch await task.result {
      case .success:
        expectUnreachable("the continuation should have thrown")
      case .failure(let error):
        expectTrue(error is CancellationError)
      }
      expectEqual(0, clock.pendingCount)
    }

    tests.test("Cancelling a scheduled continuation after its timer fires does nothing") {
      let clock = ManualClock()
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor

      let task = Task { () async throws -> Void in
        try await withContinuation(
          of: Void.self, throwing: CancellationError.self
        ) { (continuation: consuming Continuation<Void, CancellationError>,
           waiter: consuming ContinuationWaiter<Void, CancellationError>)
          async throws(CancellationError) -> Void in
          let scheduled = scheduler.schedule(continuation,
                                             at: .after(.seconds(5)),
                                             tolerance: nil, clock: clock)
          // Fire the timer, and give its job time to run, before we wait, so
          // that the continuation has been resumed when we are cancelled.
          clock.advance(by: .seconds(5))
          try? await Task.sleep(for: .milliseconds(50))
          withUnsafeCurrentTask { $0!.cancel() }
          try await waiter.wait(for: scheduled)
        }
      }

      switch await task.result {
      case .success:
        break
      case .failure:
        expectUnreachable("the timer had already fired")
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

    tests.test("CooperativeExecutor: a cancelled sleep in an escalated task runs ahead of lower-priority work") {
      let exec = CooperativeExecutor()
      let started = Flag()
      let log = Log()

      let sleeper = Task(executorPreference: exec, priority: .low) { () -> Void in
        started.set()
        try? await Task.sleep(for: .seconds(3600))
        log.append("sleeper")
      }

      _ = Task(executorPreference: exec, priority: .low) {
        while !started.value {
          await Task.yield()
        }
        // Give the sleeper time to start waiting.
        for _ in 0..<3 {
          await Task.yield()
        }

        // The job that wakes the sleeper still has its original, low,
        // priority.  Cancelling must wake the sleeper at its escalated priority,
        // rather than leaving the job to wait behind the filler.
        sleeper.escalatePriority(to: .high)
        for i in 0..<10 {
          _ = Task(executorPreference: exec, priority: .low) {
            log.append("filler \(i)")
          }
        }
        sleeper.cancel()
      }

      try! exec.run()
      expectEqual(11, log.entries.count)
      expectEqual("sleeper", log.entries.first)
    }

    tests.test("CooperativeExecutor: jobs for an unknown clock go to the clock") {
      let exec = CooperativeExecutor()
      let clock = HoldingClock(target: exec)
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
      let clock = HoldingClock(target: exec)
      let done = Flag()

      _ = Task(executorPreference: exec) {
        try await withContinuation(
          of: Void.self, throwing: CancellationError.self
        ) { (continuation: consuming Continuation<Void, CancellationError>,
         waiter: consuming ContinuationWaiter<Void, CancellationError>)
        async throws(CancellationError) -> Void in
          _ = exec.schedule(continuation,
                            at: .after(.seconds(5)),
                            tolerance: nil, clock: clock)
          try await waiter.wait()
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

    tests.test("CooperativeExecutor: a continuation timer can be cancelled") {
      let exec = CooperativeExecutor()
      let done = Flag()

      _ = Task(executorPreference: exec) {
        try await withContinuation(
          of: Void.self, throwing: CancellationError.self
        ) { (continuation: consuming Continuation<Void, CancellationError>,
         waiter: consuming ContinuationWaiter<Void, CancellationError>)
        async throws(CancellationError) -> Void in
          let scheduled = exec.schedule(continuation,
                                        at: .after(.seconds(3600)),
                                        tolerance: nil, clock: ContinuousClock())

          // Cancelling fires the timer now, rather than in an hour, and the
          // task is resumed with the error.
          withUnsafeCurrentTask { $0!.cancel() }
          var threw = false
          do {
            try await waiter.wait(for: scheduled)
          } catch {
            threw = true
          }
          expectTrue(threw)
        }
        done.set()
      }

      let start = ContinuousClock.now
      try! exec.run()
      expectTrue(done.value)
      expectTrue(ContinuousClock.now - start < .seconds(30))
    }

    tests.test("CooperativeExecutor: cancelling many timers in any order keeps the rest intact") {
      let exec = CooperativeExecutor()
      let cancelled = Counter()
      let finished = Counter()

      _ = Task(executorPreference: exec) {
        var tasks: [Task<Void, Never>] = []
        for i in 0..<300 {
          tasks.append(Task(executorPreference: exec) {
            do {
              try await Task.sleep(for: .seconds(3600) + .milliseconds(i))
              fatalError("a cancelled sleep should not complete")
            } catch {
              cancelled.increment()
            }
          })
        }

        // Timers that are not cancelled must still fire, in order, however the
        // heap has been shuffled by the cancellations.
        let survivors = Counter()
        for i in 0..<5 {
          tasks.append(Task(executorPreference: exec) {
            try! await Task.sleep(for: .milliseconds(20 * (i + 1)))
            survivors.increment()
            finished.increment()
          })
        }

        for _ in 0..<4 { await Task.yield() }

        // Cancel the long sleeps in a scrambled order, then reuse the slots
        // they were in.
        var order = Array(0..<300)
        var seed: UInt64 = 12345
        for i in stride(from: order.count - 1, to: 0, by: -1) {
          seed = seed &* 6364136223846793005 &+ 1442695040888963407
          order.swapAt(i, Int(seed >> 33) % (i + 1))
        }
        for i in order.prefix(150) { tasks[i].cancel() }
        for _ in 0..<2 { await Task.yield() }

        var more: [Task<Void, Never>] = []
        for i in 0..<100 {
          more.append(Task(executorPreference: exec) {
            do {
              try await Task.sleep(for: .seconds(3600) + .milliseconds(i))
            } catch {
              cancelled.increment()
            }
          })
        }
        for _ in 0..<2 { await Task.yield() }
        for i in order.suffix(150) { tasks[i].cancel() }
        for t in more { t.cancel() }

        for t in tasks { await t.value }
        for t in more { await t.value }
        expectEqual(5, survivors.value)
      }

      try! exec.run()
      expectEqual(400, cancelled.value)
      expectEqual(5, finished.value)
    }

    tests.test("SchedulingExecutorFireTime is Equatable and Hashable") {
      let a = SchedulingExecutorFireTime<ContinuousClock>.after(.seconds(5))
      let b = SchedulingExecutorFireTime<ContinuousClock>.after(.seconds(5))
      let c = SchedulingExecutorFireTime<ContinuousClock>.after(.seconds(6))
      let now = ContinuousClock.now
      let d = SchedulingExecutorFireTime<ContinuousClock>.at(now)
      let e = SchedulingExecutorFireTime<ContinuousClock>.at(now)

      expectEqual(a, b)
      expectNotEqual(a, c)
      expectEqual(d, e)
      expectNotEqual(a, d)
      expectEqual(a.hashValue, b.hashValue)
      expectEqual(d.hashValue, e.hashValue)
      expectEqual(3, Set([a, b, c, d, e]).count)
    }

    tests.test("Task.sleep doesn't ask an executor that doesn't declare support to schedule") {
      let executor = RefusingExecutor()
      let task = Task(executorPreference: executor) {
        try await Task.sleep(for: .milliseconds(30))
      }
      try! await task.value
    }

    tests.test("A standard clock runs a job on an executor that doesn't declare support, via the default executor") {
      let refusing = RefusingExecutor()
      let proxy = SchedulingProxy(clock: ContinuousClock(), scheduler: refusing,
                                  askClock: true)
      let runs = Counter()

      let task = Task(executorPreference: proxy) { () -> Int in
        runs.increment()
        return 11
      }

      // The job is waiting for its time, rather than in the executor.
      try! await Task.sleep(for: .milliseconds(50))
      expectEqual(0, runs.value)

      // Cancelling runs the trampoline now, which runs the original job.
      expectTrue(proxy.takeToken()!.cancel())
      expectEqual(11, await task.value)
      try! await Task.sleep(for: .milliseconds(50))
      expectEqual(1, runs.value)
    }

    await runAllTestsAsync()
  }
}
