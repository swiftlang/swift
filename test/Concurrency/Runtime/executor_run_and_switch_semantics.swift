// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// A pre-existing OptimizeHopToExecutor bug.
// Tracked by https://github.com/swiftlang/swift/issues/92356
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// Pins down how the runtime runs jobs and switches executors. Each test runs
// jobs passing a chosen execution context, and checks what the following hops
// do: run inline, switch, or enqueue. The rules are described in
// docs/ConcurrencyRuntimeExecution.md.

@_spi(Concurrency) import _Concurrency
@_spi(ExperimentalCustomExecutors) import _Concurrency
import StdlibUnittest
import Synchronization

// MARK: - Runtime entry points

// The serial executor of the current context.
@_silgen_name("swift_task_getCurrentExecutor")
func _currentSerialExecutor() -> UnownedSerialExecutor

// Runs a job the way the Dispatch-based default executor does: `swift_job_run`
// with the generic executor.
@_silgen_name("swift_job_run")
func _runJob(_ job: UnownedJob, on executor: UnownedSerialExecutor)

let genericSerialExecutor = unsafeBitCast((0, 0), to: UnownedSerialExecutor.self)

// MARK: - Names in logs

// The names of the executors whose jobs are running, innermost last.
let executorsRunningJobs = Mutex<[String]>([])

func runningJob(of executorName: String, _ body: () -> Void) {
  executorsRunningJobs.withLock { $0.append(executorName) }
  body()
  executorsRunningJobs.withLock { _ = $0.removeLast() }
}

// `event`, suffixed with the serial executor of the current context and the
// executor whose job the code runs in.
func here(_ event: String) -> String {
  let jobExecutor = executorsRunningJobs.withLock { $0.last } ?? "none"
  return "\(event) [\(nameOfCurrentSerialExecutor()), \(jobExecutor)]"
}

func nameOfCurrentSerialExecutor() -> String {
  let (identity, implementation) =
    unsafeBitCast(_currentSerialExecutor(), to: (Int, Int).self)
  guard let identity = UnsafeRawPointer(bitPattern: identity) else {
    // The context has no serial executor. The executor kind bits mark the
    // generic executor `Task.immediate` runs its synchronous start with.
    return implementation == 0 ? "none" : "Task.immediate"
  }
  // The identity is the executor, or the actor for a default actor.
  switch Unmanaged<AnyObject>.fromOpaque(identity).takeUnretainedValue() {
  case let executor as ManualExecutor: return executor.name
  case let actor as DefaultActor: return actor.name
  case is CountingMainExecutor: return "main"
  default: return "unknown"
  }
}

// MARK: - Event log

final class EventLog: Sendable {
  private let events = Mutex<(recorded: [String], checked: Int)>(([], 0))

  func record(_ event: String) {
    events.withLock { $0.recorded.append(event) }
  }

  // Checks the events recorded since the previous check.
  func expectNext(
    _ expected: [String], file: String = #file, line: UInt = #line
  ) {
    let next = events.withLock { events in
      defer { events.checked = events.recorded.count }
      return Array(events.recorded[events.checked...])
    }
    expectEqual(expected, next, file: file, line: line)
  }
}

// MARK: - Executors

// The roles an executor passes when it runs a job.
enum PassedRoles {
  case serialExecutorOnly
  case taskExecutorOnly
  case both
}

// An executor that queues the jobs enqueued on it, so that a test runs them one
// by one. It is both a serial and a task executor, and receives the jobs of
// both roles through the same `enqueue`. It runs a job passing itself in one or
// both of its roles.
//
// A loop of a pool, created with `pool`, is one of the pool's threads. It
// passes the pool as its task executor, and also runs the jobs enqueued on the
// pool.
//
// With `runsJobsUnlessHeld`, it runs a job as soon as it is enqueued, passing
// itself as the task executor, unless it is held. The default executor of the
// tests works like that, so that the jobs of the test harness itself run.
final class ManualExecutor: SerialExecutor, TaskExecutor, Sendable {
  let name: String
  private let pool: ManualExecutor?
  private let state = Mutex<(jobs: [UnownedJob], held: Bool)>(([], false))
  private let runsJobsUnlessHeld: Bool
  var pendingJobCount: Int { state.withLock { $0.jobs.count } }

  init(
    _ name: String, pool: ManualExecutor? = nil,
    runsJobsUnlessHeld: Bool = false
  ) {
    self.pool = pool
    self.runsJobsUnlessHeld = runsJobsUnlessHeld
    self.name = name
  }

  func enqueue(_ job: UnownedJob) {
    let runsJobNow = state.withLock { state in
      if runsJobsUnlessHeld && !state.held {
        return true
      }
      state.jobs.append(job)
      return false
    }
    // Run the job outside the lock, because it can enqueue more jobs.
    if runsJobNow {
      runningJob(of: name) {
        unsafe job.runSynchronously(on: asUnownedTaskExecutor())
      }
    }
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }

  // Queues the jobs enqueued from now on, for an executor with
  // `runsJobsUnlessHeld`.
  func hold() {
    state.withLock { $0.held = true }
  }

  // Runs all queued jobs, passing this executor as the task executor, then
  // stops holding.
  func drainAndRelease() {
    while let job = popJob() {
      runningJob(of: name) {
        unsafe job.runSynchronously(on: asUnownedTaskExecutor())
      }
    }
    state.withLock { $0.held = false }
  }

  private func popJob() -> UnownedJob? {
    state.withLock { $0.jobs.isEmpty ? nil : $0.jobs.removeFirst() }
  }

  // Runs the next job, passing this executor in the given roles. A loop of a
  // pool passes the pool as its task executor, and runs the jobs of its pool
  // once it has none of its own.
  func runNextJob(passing roles: PassedRoles) {
    guard let job = popJob() ?? pool?.popJob() else {
      preconditionFailure("no job enqueued")
    }
    let taskExecutor = (pool ?? self).asUnownedTaskExecutor()
    runningJob(of: name) {
      switch roles {
      case .serialExecutorOnly:
        unsafe job.runSynchronously(on: asUnownedSerialExecutor())
      case .taskExecutorOnly:
        unsafe job.runSynchronously(on: taskExecutor)
      case .both:
        unsafe job.runSynchronously(
          isolatedTo: asUnownedSerialExecutor(), taskExecutor: taskExecutor)
      }
    }
  }

  // Runs the next job the way the Dispatch-based default executor does, with
  // `swift_job_run` and the generic executor.
  func runNextJobLikeDispatch() {
    guard let job = popJob() else {
      preconditionFailure("no job enqueued")
    }
    runningJob(of: name) {
      _runJob(job, on: genericSerialExecutor)
    }
  }
}

// Runs main actor jobs as soon as they are enqueued, counting how many were
// enqueued.
final class CountingMainExecutor: MainExecutor, Sendable {
  private let enqueues = Atomic<Int>(0)
  var enqueueCount: Int { enqueues.load(ordering: .relaxed) }

  func enqueue(_ job: consuming ExecutorJob) {
    enqueues.add(1, ordering: .relaxed)
    let job = UnownedJob(job)
    runningJob(of: "main") {
      unsafe job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
  func checkIsolated() {}
  func run() throws {}
  func stop() {}
}

let testMainExecutor = CountingMainExecutor()
let testDefaultExecutor = ManualExecutor("default executor", runsJobsUnlessHeld: true)

struct TestFactory: ExecutorFactory {
  static var mainExecutor: any MainExecutor { testMainExecutor }
  static var defaultExecutor: any TaskExecutor { testDefaultExecutor }
}

// MARK: - Actors and functions to hop to

// A default actor.
actor DefaultActor {
  let name: String
  init(_ name: String) { self.name = name }
}

// An actor isolated to a `ManualExecutor`.
actor CustomActor {
  private let executor: ManualExecutor
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }
  init(_ executor: ManualExecutor) { self.executor = executor }
}

extension Actor {
  @inline(never)
  func call(_ log: EventLog, _ event: String) async {
    log.record(here(event))
  }

  func run<T: Sendable>(
    _ body: sending nonisolated(nonsending) () async -> T
  ) async -> T {
    await body()
  }

  // Records `event`, then calls concurrent code that records `concurrentEvent`.
  @inline(never)
  func callThenConcurrent(
    _ log: EventLog, _ event: String, _ concurrentEvent: String
  ) async {
    log.record(here(event))
    await concurrentCall(log, concurrentEvent)
  }

  // Calls `other`, then concurrent code, recording where it runs in between.
  @inline(never)
  func callOtherThenConcurrent(_ log: EventLog, _ other: some Actor) async {
    log.record(here("on the actor"))
    await other.call(log, "on the other actor")
    log.record(here("back on the actor"))
    await concurrentCall(log, "concurrent")
    log.record(here("back on the actor again"))
  }

  // Calls `custom`, recording where it runs before and after.
  @inline(never)
  func callCustomActor(_ log: EventLog, _ custom: CustomActor) async {
    log.record(here("on the default actor"))
    await custom.call(log, "on the custom actor")
    log.record(here("back on the default actor"))
  }

  // Starts a `Task.immediate` isolated to this actor, from this actor.
  func startImmediate(
    _ log: EventLog, executorPreference: (any TaskExecutor)?
  ) -> Task<Void, Never> {
    let task = Task.immediate(executorPreference: executorPreference) {
      log.record(here("immediate task started"))
      await self.call(log, "same-actor call")
      log.record(here("immediate task done"))
    }
    log.record("Task.immediate returned")
    return task
  }

  // Starts a `Task.immediate` isolated to this actor, from this actor, that
  // calls concurrent code and then `other`.
  func startImmediateThatHops(
    _ log: EventLog, _ other: some Actor
  ) -> Task<Void, Never> {
    let task = Task.immediate {
      await self.call(log, "immediate task same-actor call")
      await concurrentCall(log, "immediate task concurrent")
      await other.call(log, "immediate task on the other actor")
      log.record(here("immediate task done"))
    }
    log.record(here("Task.immediate returned"))
    self.preconditionIsolated()
    return task
  }
}

@inline(never)
@concurrent func concurrentCall(_ log: EventLog, _ event: String) async {
  log.record(here(event))
}

@MainActor
@inline(never)
func mainActorCall(_ log: EventLog, _ event: String) async {
  log.record(here(event))
}

@MainActor
@inline(never)
func mainActorCallingConcurrent(_ log: EventLog) async {
  log.record(here("on the main actor"))
  await concurrentCall(log, "concurrent")
  log.record(here("back on the main actor"))
}

@MainActor
@inline(never)
func mainActorCallingMainActor(_ log: EventLog) async {
  log.record(here("on the main actor"))
  await mainActorCall(log, "same-actor call")
  log.record(here("back on the main actor"))
}

// Starts a nonisolated `Task.immediate` that calls concurrent code and then an
// idle default actor.
nonisolated func startNonisolatedImmediate(
  _ log: EventLog, _ actor: DefaultActor, executorPreference: (any TaskExecutor)?
) -> Task<Void, Never> {
  let task = Task.immediate(executorPreference: executorPreference) {
    log.record(here("immediate task started"))
    await concurrentCall(log, "concurrent")
    await actor.call(log, "on the default actor")
    log.record(here("immediate task done"))
  }
  log.record("Task.immediate returned")
  return task
}

// MARK: - Tests

@main
struct Main {
  typealias DefaultExecutorFactory = TestFactory

  static func main() async {
    let tests = TestSuite("Executor run entry points and switching")

    // MARK: Passing only a custom serial executor

    tests.test("passing only S, no preference: a same-actor call runs inline, concurrent code is enqueued on the default executor") {
      let executor = ManualExecutor("S")
      let isolated = CustomActor(executor)
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached {
        await isolated.run {
          log.record(here("on the actor"))
          await isolated.call(log, "same-actor call")
          await concurrentCall(log, "concurrent")
          log.record(here("back on the actor"))
        }
      }
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      executor.runNextJob(passing: .serialExecutorOnly)
      // The context `(S, none)` doesn't match the task's preference for the
      // default executor, so the concurrent code is enqueued there.
      log.expectNext([
        "on the actor [S, S]",
        "same-actor call [S, S]",
      ])
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      expectEqual(1, executor.pendingJobCount)
      executor.runNextJob(passing: .serialExecutorOnly)
      log.expectNext([
        "concurrent [none, default executor]",
        "back on the actor [S, S]",
      ])
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("passing only S, no preference: calls to another custom actor and to an idle default actor are enqueued") {
      let executor = ManualExecutor("S")
      let otherExecutor = ManualExecutor("S2")
      let isolated = CustomActor(executor)
      let other = CustomActor(otherExecutor)
      let idle = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached {
        await isolated.run {
          await other.call(log, "on the other actor")
          await idle.call(log, "on the default actor")
          log.record(here("back on the actor"))
        }
      }
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      executor.runNextJob(passing: .serialExecutorOnly)
      expectEqual(1, otherExecutor.pendingJobCount)
      otherExecutor.runNextJob(passing: .serialExecutorOnly)
      expectEqual(1, executor.pendingJobCount)
      executor.runNextJob(passing: .serialExecutorOnly)
      // The context `(S, none)` doesn't match the task's preference for the
      // default executor, so we have to enqueue.
      log.expectNext(["on the other actor [S2, S2]"])
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      expectEqual(1, executor.pendingJobCount)
      executor.runNextJob(passing: .serialExecutorOnly)
      log.expectNext([
        "on the default actor [default actor, default executor]",
        "back on the actor [S, S]",
      ])
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("passing only S, preferring P: same-actor calls run inline, leaving the actor is enqueued on P") {
      let executor = ManualExecutor("S")
      let preference = ManualExecutor("P")
      let isolated = CustomActor(executor)
      let log = EventLog()
      let task = Task.detached(executorPreference: preference) {
        await isolated.run {
          log.record(here("on the actor"))
          await isolated.call(log, "same-actor call")
          log.record(here("back on the actor"))
        }
        log.record(here("left the actor"))
      }
      preference.runNextJob(passing: .taskExecutorOnly)
      executor.runNextJob(passing: .serialExecutorOnly)
      // Code isolated to an actor with a custom serial executor runs inline in
      // a context with that executor, whatever the task's preference.
      log.expectNext([
        "on the actor [S, S]",
        "same-actor call [S, S]",
        "back on the actor [S, S]",
      ])
      expectEqual(0, executor.pendingJobCount)
      // The context `(S, none)` doesn't match the preference, so leaving the
      // actor enqueues the task on `P`.
      expectEqual(1, preference.pendingJobCount)
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["left the actor [none, P]"])
      await task.value
    }

    tests.test("passing only S, preferring the default executor: a same-actor call runs inline") {
      let executor = ManualExecutor("S")
      let isolated = CustomActor(executor)
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached(executorPreference: globalConcurrentExecutor) {
        await isolated.run {
          log.record(here("on the actor"))
          await isolated.call(log, "same-actor call")
          log.record(here("back on the actor"))
        }
      }
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      executor.runNextJob(passing: .serialExecutorOnly)
      // Code isolated to an actor with a custom serial executor runs inline in
      // a context with that executor, whatever the task's preference.
      log.expectNext([
        "on the actor [S, S]",
        "same-actor call [S, S]",
        "back on the actor [S, S]",
      ])
      expectEqual(0, executor.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    // MARK: Passing only a task executor

    tests.test("passing only P, preferring P: concurrent code runs inline, an idle default actor is locked") {
      let preference = ManualExecutor("P")
      let idle = DefaultActor("default actor")
      let log = EventLog()
      let task = Task.detached(executorPreference: preference) {
        log.record(here("started"))
        await concurrentCall(log, "concurrent")
        await idle.call(log, "on the default actor")
        log.record(here("back"))
      }
      preference.runNextJob(passing: .taskExecutorOnly)
      // The context `(none, P)` matches the preference, which is where a
      // default actor called by this task runs. The runtime locks the idle
      // actor on top of the context, and unlocks it when the task leaves it.
      log.expectNext([
        "started [none, P]",
        "concurrent [none, P]",
        "on the default actor [default actor, P]",
        "back [none, P]",
      ])
      expectEqual(0, preference.pendingJobCount)
      await task.value
    }

    tests.test("passing only P, preferring P: a call to a custom actor is enqueued on its executor and comes back to P") {
      let preference = ManualExecutor("P")
      let executor = ManualExecutor("S")
      let isolated = CustomActor(executor)
      let log = EventLog()
      let task = Task.detached(executorPreference: preference) {
        await isolated.call(log, "on the actor")
        log.record(here("back"))
      }
      preference.runNextJob(passing: .taskExecutorOnly)
      expectEqual(1, executor.pendingJobCount)
      executor.runNextJob(passing: .serialExecutorOnly)
      log.expectNext(["on the actor [S, S]"])
      expectEqual(1, preference.pendingJobCount)
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["back [none, P]"])
      await task.value
    }

    // MARK: The default executor

    tests.test("Dispatch-style, no preference: an idle default actor is locked") {
      let idle = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached {
        await concurrentCall(log, "concurrent")
        await idle.call(log, "on the default actor")
        log.record(here("back"))
      }
      testDefaultExecutor.runNextJobLikeDispatch()
      log.expectNext([
        "concurrent [none, default executor]",
        "on the default actor [default actor, default executor]",
        "back [none, default executor]",
      ])
      expectEqual(0, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("passing only the default executor, no preference: an idle default actor is locked") {
      let idle = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached {
        await concurrentCall(log, "concurrent")
        await idle.call(log, "on the default actor")
        log.record(here("back"))
      }
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      // A custom default executor, installed with an `ExecutorFactory`, runs
      // its jobs with `runSynchronously(on:)` and itself as the task executor.
      // The context `(none, default)` matches the task's preference, as with
      // the Dispatch-based default executor above.
      log.expectNext([
        "concurrent [none, default executor]",
        "on the default actor [default actor, default executor]",
        "back [none, default executor]",
      ])
      expectEqual(0, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("Dispatch-style, preferring the default executor: an idle default actor is locked") {
      let idle = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached(executorPreference: globalConcurrentExecutor) {
        await idle.call(log, "on the default actor")
        log.record(here("back"))
      }
      testDefaultExecutor.runNextJobLikeDispatch()
      // A preference for the default executor counts as no preference.
      log.expectNext([
        "on the default actor [default actor, default executor]",
        "back [none, default executor]",
      ])
      expectEqual(0, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("preferring the default executor inside a preference for P: runs like no preference") {
      let preference = ManualExecutor("P")
      let idle = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached(executorPreference: preference) {
        await concurrentCall(log, "preferring P")
        await withTaskExecutorPreference(globalConcurrentExecutor) {
          await concurrentCall(log, "preferring the default executor")
          await idle.call(log, "on the default actor")
        }
        await concurrentCall(log, "preferring P again")
      }
      // The task starts on `P`. The context `(none, P)` doesn't match the
      // preference for the default executor, so the task is enqueued there.
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["preferring P [none, P]"])
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      // The context `(none, default)` matches the preference for the default
      // executor like no preference: concurrent code runs inline, and the
      // runtime locks the idle actor and unlocks it again.
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "preferring the default executor [none, default executor]",
        "on the default actor [default actor, default executor]",
      ])
      expectEqual(0, testDefaultExecutor.pendingJobCount)
      // Leaving the scope brings back the preference for `P`, which the context
      // `(none, default)` doesn't match.
      expectEqual(1, preference.pendingJobCount)
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["preferring P again [none, P]"])
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    // MARK: Default actor processing

    tests.test("default actor processing, no preference: switches to another idle default actor and to concurrent code") {
      let first = DefaultActor("first actor")
      let second = DefaultActor("second actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached {
        await first.callOtherThenConcurrent(log, second)
      }
      // The task starts in the context `(none, default)`, which matches its
      // preference, so every hop switches without enqueuing. The runtime locks
      // the idle first actor, then locks the second and unlocks the first,
      // locks the first again and unlocks the second, unlocks the first for
      // the concurrent code, and locks it again.
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "on the actor [first actor, default executor]",
        "on the other actor [second actor, default executor]",
        "back on the actor [first actor, default executor]",
        "concurrent [none, default executor]",
        "back on the actor again [first actor, default executor]",
      ])
      expectEqual(0, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("default actor processing, preferring P: switches to another idle default actor and to concurrent code") {
      let preference = ManualExecutor("P")
      let first = DefaultActor("first actor")
      let second = DefaultActor("second actor")
      let log = EventLog()
      let task = Task.detached(executorPreference: preference) {
        await first.callOtherThenConcurrent(log, second)
      }
      // The task starts on `P`, whose context matches its preference, so every
      // hop switches without enqueuing, as without a preference above.
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "on the actor [first actor, P]",
        "on the other actor [second actor, P]",
        "back on the actor [first actor, P]",
        "concurrent [none, P]",
        "back on the actor again [first actor, P]",
      ])
      expectEqual(0, preference.pendingJobCount)
      await task.value
    }

    tests.test("default actor processing on P: concurrent code of a task without a preference is enqueued on the default executor") {
      let preference = ManualExecutor("P")
      let shared = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      // The first task only schedules the actor's processing job on `P`. A
      // processing job goes to the preference of the task whose job scheduled
      // the actor, so a task without a preference needs another task to run its
      // code on `P`. Both tasks call the actor from the main actor, so that
      // they are enqueued on it. The main executor runs each job as soon as it
      // is enqueued.
      let scheduling = Task.detached(executorPreference: preference) { @MainActor in
        log.record(here("scheduling task on the main actor"))
        await shared.call(log, "scheduling task on the actor")
      }
      // Potentially unexpected: Unlike `Task.detached`,
      // `Task.detached(executorPreference:)` takes a closure without
      // `@isolated(any)`, so the runtime doesn't see that the closure is
      // isolated to the main actor. The task starts as concurrent code on `P`,
      // and only the closure's first hop goes to the main actor, from where it
      // enqueues the task on the actor.
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["scheduling task on the main actor [main, main]"])
      // The actor's processing job.
      expectEqual(1, preference.pendingJobCount)
      // The task without a preference starts on the main actor right away and
      // joins the actor's queue. The actor is scheduled already, so this
      // enqueues no job.
      let task = Task.detached { @MainActor in
        log.record(here("task on the main actor"))
        await shared.callThenConcurrent(log, "task on the actor", "task concurrent")
      }
      log.expectNext(["task on the main actor [main, main]"])
      expectEqual(1, preference.pendingJobCount)
      expectEqual(0, testDefaultExecutor.pendingJobCount)
      // Unexpected: This should not run both jobs on the actor's queue since
      // one job has a preference and the other doesn't.
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "scheduling task on the actor [default actor, P]",
        "task on the actor [default actor, P]",
      ])
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["task concurrent [none, default executor]"])
      expectEqual(0, preference.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await scheduling.value
      await task.value
    }

    tests.test("default actor processing on the default executor: a task preferring P runs its isolated code there") {
      let preference = ManualExecutor("P")
      let shared = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      // The first task only schedules the actor's processing job on the
      // default executor. It prefers none and calls the actor from the main
      // actor, so that it is enqueued on it. The main executor runs each job as
      // soon as it is enqueued, so this happens right away.
      let scheduling = Task.detached { @MainActor in
        log.record(here("scheduling task on the main actor"))
        await shared.call(log, "scheduling task on the actor")
      }
      log.expectNext(["scheduling task on the main actor [main, main]"])
      // The actor's processing job.
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      // A task preferring `P` starts on `P` and joins the actor's queue. The
      // actor is scheduled already, so this enqueues no job.
      let task = Task.detached(executorPreference: preference) {
        await shared.call(log, "task on the actor")
        await concurrentCall(log, "task concurrent")
      }
      preference.runNextJob(passing: .taskExecutorOnly)
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      expectEqual(0, preference.pendingJobCount)
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      // Unexpected: This is running both jobs on the default executor even though
      // the second task had a preference set.
      log.expectNext([
        "scheduling task on the actor [default actor, default executor]",
        "task on the actor [default actor, default executor]",
      ])
      expectEqual(1, preference.pendingJobCount)
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["task concurrent [none, P]"])
      testDefaultExecutor.drainAndRelease()
      await scheduling.value
      await task.value
    }

    tests.test("default actor processing: the processing job continues with the queue of the actor a task switched to") {
      let preference = ManualExecutor("P")
      let executor = ManualExecutor("E")
      let custom = CustomActor(executor)
      let first = DefaultActor("first actor")
      let second = DefaultActor("second actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      // Calling the first actor from the main actor schedules its processing
      // job on the task's preference.
      let switching = Task.detached(executorPreference: preference) { @MainActor in
        log.record(here("switching task on the main actor"))
        await first.run {
          log.record(here("switching task on the first actor"))
          await second.run {
            log.record(here("switching task on the second actor"))
            // Starts the joining task while this task runs on the second
            // actor.
            preference.runNextJob(passing: .taskExecutorOnly)
            await custom.call(log, "switching task on the custom actor")
            log.record(here("switching task back on the second actor"))
          }
          log.record(here("switching task back on the first actor"))
        }
      }
      // Potentially unexpected: `Task.detached(executorPreference:)` takes a
      // closure without `@isolated(any)`, so the task starts as concurrent code
      // on its preference, and only the closure's first hop goes to the main
      // actor. From there it enqueues the task on the first actor.
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["switching task on the main actor [main, main]"])
      let joining = Task.detached(executorPreference: preference) {
        await second.call(log, "joining task on the second actor")
        log.record(here("joining task left the second actor"))
      }
      expectEqual(2, preference.pendingJobCount)
      // The processing job of the first actor runs the switching task, which
      // locks the second actor. The joining task joins the second actor's
      // queue, and the switching task is enqueued on `E`. The processing job
      // then runs the second actor's queue in the same job.
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "switching task on the first actor [first actor, P]",
        "switching task on the second actor [second actor, P]",
        "joining task on the second actor [second actor, P]",
        "joining task left the second actor [none, P]",
      ])
      expectEqual(0, preference.pendingJobCount)
      expectEqual(1, executor.pendingJobCount)
      expectEqual(0, testDefaultExecutor.pendingJobCount)
      // `E` runs the switching task's call. The context `(E, none)` doesn't
      // match the task's preference, so the call back to the idle second actor
      // schedules a processing job on the preference, which then switches to
      // the idle first actor.
      executor.runNextJob(passing: .serialExecutorOnly)
      expectEqual(1, preference.pendingJobCount)
      preference.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "switching task on the custom actor [E, E]",
        "switching task back on the second actor [second actor, P]",
        "switching task back on the first actor [first actor, P]",
      ])
      expectEqual(0, preference.pendingJobCount)
      expectEqual(0, executor.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await switching.value
      await joining.value
    }

    tests.test("default actor processing on E passing both roles: a call to an actor isolated to E unlocks the default actor") {
      let both = ManualExecutor("E")
      let isolated = CustomActor(both)
      let shared = DefaultActor("default actor")
      let log = EventLog()
      let task = Task.detached(executorPreference: both) { @MainActor in
        log.record(here("on the main actor"))
        await shared.callCustomActor(log, isolated)
      }
      // Potentially unexpected: `Task.detached(executorPreference:)` takes a
      // closure without `@isolated(any)`, so the task starts as concurrent code
      // on `E`, and only the closure's first hop goes to the main actor. From
      // there it enqueues the task on the default actor, whose processing job
      // goes to `E`, the task's preference.
      both.runNextJob(passing: .both)
      log.expectNext(["on the main actor [main, main]"])
      expectEqual(1, both.pendingJobCount)
      // `E` runs the processing job passing both roles. The call into the
      // actor isolated to `E` unlocks the default actor, which returns to the
      // base context `(E, E)`, and the call back locks the default actor
      // again, all inline.
      both.runNextJob(passing: .both)
      log.expectNext([
        "on the default actor [default actor, E]",
        "on the custom actor [E, E]",
        "back on the default actor [default actor, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      await task.value
    }

    // MARK: Main actor

    tests.test("main actor, no preference: a same-actor call runs inline") {
      let log = EventLog()
      let before = testMainExecutor.enqueueCount
      let task = Task.detached {
        await mainActorCallingMainActor(log)
      }
      // Only the hop onto the main actor is a job.
      expectEqual(1, testMainExecutor.enqueueCount - before)
      log.expectNext([
        "on the main actor [main, main]",
        "same-actor call [main, main]",
        "back on the main actor [main, main]",
      ])
      await task.value
    }

    tests.test("main actor, no preference: concurrent code is enqueued on the default executor") {
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached {
        await mainActorCallingConcurrent(log)
      }
      // The task starts and hops onto the main actor. The context
      // `(main, none)` doesn't match the task's preference for the default
      // executor, so the concurrent call is enqueued there.
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      log.expectNext(["on the main actor [main, main]"])
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "concurrent [none, default executor]",
        "back on the main actor [main, main]",
      ])
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("main actor, preferring P: a same-actor call runs inline") {
      let preference = ManualExecutor("P")
      let log = EventLog()
      let before = testMainExecutor.enqueueCount
      let task = Task.detached(executorPreference: preference) {
        await mainActorCallingMainActor(log)
      }
      preference.runNextJob(passing: .taskExecutorOnly)
      // The main actor has a custom serial executor, so code isolated to it
      // runs inline whatever the task's preference. Only the hop onto the main
      // actor is a job, as without a preference.
      expectEqual(1, testMainExecutor.enqueueCount - before)
      log.expectNext([
        "on the main actor [main, main]",
        "same-actor call [main, main]",
        "back on the main actor [main, main]",
      ])
      expectEqual(0, preference.pendingJobCount)
      await task.value
    }

    // MARK: Task.immediate

    tests.test("Task.immediate, nonisolated, no preference: the synchronous start doesn't switch") {
      let idle = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = startNonisolatedImmediate(log, idle, executorPreference: nil)
      // Potentially unexpected: The nonisolated immediate task runs the
      // `@concurrent` method inline, on the caller's thread. Started from the
      // main actor, as here, that is the main thread. An immediate task
      // isolated to the main actor does leave it for a `@concurrent` method.
      // The synchronous start doesn't switch, because the caller needs its
      // thread back, so the call to the idle default actor is enqueued.
      log.expectNext([
        "immediate task started [Task.immediate, main]",
        "concurrent [Task.immediate, main]",
        "Task.immediate returned",
      ])
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "on the default actor [default actor, default executor]",
        "immediate task done [none, default executor]",
      ])
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("Task.immediate, nonisolated, preferring P: concurrent code runs on the caller's thread") {
      let preference = ManualExecutor("P")
      let idle = DefaultActor("default actor")
      let log = EventLog()
      let task = startNonisolatedImmediate(log, idle, executorPreference: preference)
      // Potentially unexpected: The nonisolated immediate task runs the
      // `@concurrent` method inline, on the caller's thread rather than on its
      // preference `P`, although its context records `P`. Started from the
      // main actor, as here, that is the main thread. The task only reaches
      // `P` after its first suspension. An immediate task isolated to the main
      // actor does leave it for a `@concurrent` method.
      log.expectNext([
        "immediate task started [Task.immediate, main]",
        "concurrent [Task.immediate, main]",
        "Task.immediate returned",
      ])
      // The synchronous start doesn't switch, so the call to the actor is
      // enqueued, and the actor's processing job goes to `P`. Its context
      // matches the preference, so the task leaves the actor inline.
      expectEqual(1, preference.pendingJobCount)
      preference.runNextJob(passing: .taskExecutorOnly)
      expectEqual(0, preference.pendingJobCount)
      log.expectNext([
        "on the default actor [default actor, P]",
        "immediate task done [none, P]",
      ])
      await task.value
    }

    tests.test("Task.immediate, isolated to a custom actor, no preference: runs synchronously") {
      let executor = ManualExecutor("S")
      let isolated = CustomActor(executor)
      let log = EventLog()
      let starter = Task.detached {
        await isolated.startImmediate(log, executorPreference: nil)
      }
      executor.runNextJob(passing: .serialExecutorOnly)
      log.expectNext([
        "immediate task started [S, S]",
        "same-actor call [S, S]",
        "immediate task done [S, S]",
        "Task.immediate returned",
      ])
      expectEqual(0, executor.pendingJobCount)
      await starter.value.value
    }

    tests.test("Task.immediate, isolated to a custom actor, preferring P: runs synchronously") {
      let executor = ManualExecutor("S")
      let preference = ManualExecutor("P")
      let isolated = CustomActor(executor)
      let log = EventLog()
      let starter = Task.detached {
        await isolated.startImmediate(log, executorPreference: preference)
      }
      executor.runNextJob(passing: .serialExecutorOnly)
      // The operation starts on the actor in the caller's context, as
      // `Task.immediate` documents. Code isolated to an actor with a custom
      // serial executor runs inline whatever the task's preference.
      log.expectNext([
        "immediate task started [S, S]",
        "same-actor call [S, S]",
        "immediate task done [S, S]",
        "Task.immediate returned",
      ])
      expectEqual(0, executor.pendingJobCount)
      expectEqual(0, preference.pendingJobCount)
      await starter.value.value
    }

    tests.test("Task.immediate, isolated to a default actor, no preference: runs synchronously") {
      let isolated = DefaultActor("default actor")
      let log = EventLog()
      let starter = Task.detached {
        await isolated.startImmediate(log, executorPreference: nil)
      }
      log.expectNext([
        "immediate task started [default actor, default executor]",
        "same-actor call [default actor, default executor]",
        "immediate task done [default actor, default executor]",
        "Task.immediate returned",
      ])
      await starter.value.value
    }

    tests.test("Task.immediate, isolated to a default actor, preferring P: runs synchronously") {
      let preference = ManualExecutor("P")
      let isolated = DefaultActor("default actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let starter = Task.detached {
        await isolated.startImmediate(log, executorPreference: preference)
      }
      // The starter locks the idle actor.
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      // The operation starts on the actor in the caller's context, as
      // `Task.immediate` documents.
      log.expectNext([
        "immediate task started [default actor, default executor]",
        "same-actor call [default actor, default executor]",
        "immediate task done [default actor, default executor]",
        "Task.immediate returned",
      ])
      expectEqual(0, preference.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await starter.value.value
    }

    tests.test("Task.immediate, isolated to a default actor: hops never unlock the caller's actor") {
      let shared = DefaultActor("default actor")
      let other = DefaultActor("other actor")
      let log = EventLog()
      testDefaultExecutor.hold()
      let outer = Task.detached {
        let immediate = await shared.startImmediateThatHops(log, other)
        log.record(here("outer task left the actor"))
        await immediate.value
      }
      // The outer task locks the idle actor and starts the immediate task on
      // it. The immediate task's base context has the actor, so its hop to
      // concurrent code doesn't unlock the actor but enqueues the task.
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      log.expectNext([
        "immediate task same-actor call [default actor, default executor]",
        "Task.immediate returned [default actor, default executor]",
        "outer task left the actor [none, default executor]",
      ])
      expectEqual(1, testDefaultExecutor.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      log.expectNext([
        "immediate task concurrent [none, default executor]",
        "immediate task on the other actor [other actor, default executor]",
        "immediate task done [default actor, default executor]",
      ])
      await outer.value
    }

    // MARK: An executor that is both a serial and a task executor

    tests.test("passing only E as serial executor, preferring E: isolated and concurrent code run inline") {
      let both = ManualExecutor("E")
      let isolated = CustomActor(both)
      let log = EventLog()
      let task = Task.detached(executorPreference: both) {
        log.record(here("started"))
        await isolated.call(log, "on the actor")
        await concurrentCall(log, "concurrent")
      }
      both.runNextJob(passing: .serialExecutorOnly)
      // The context `(E, none)` has `E`, so the call to the actor isolated to
      // `E` runs inline. It also matches the preference for `E`, so the
      // concurrent code runs inline too, and the context keeps `E` as its
      // serial executor.
      log.expectNext([
        "started [E, E]",
        "on the actor [E, E]",
        "concurrent [E, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      await task.value
    }

    tests.test("passing E in both roles, preferring E: same-actor calls and leaving the actor run inline") {
      let both = ManualExecutor("E")
      let isolated = CustomActor(both)
      let log = EventLog()
      let task = Task.detached(executorPreference: both) {
        await isolated.run {
          log.record(here("on the actor"))
          await isolated.call(log, "same-actor call")
          log.record(here("back on the actor"))
        }
        await concurrentCall(log, "concurrent")
      }
      both.runNextJob(passing: .both)
      // The context `(E, E)` has `E`, so code isolated to the actor runs
      // inline. It also matches the preference, so leaving the actor runs
      // inline as well, and the context keeps `E` as its serial executor.
      log.expectNext([
        "on the actor [E, E]",
        "same-actor call [E, E]",
        "back on the actor [E, E]",
        "concurrent [E, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      await task.value
    }

    tests.test("passing E in both roles, no preference: same-actor calls run inline") {
      let both = ManualExecutor("E")
      let isolated = CustomActor(both)
      let log = EventLog()
      testDefaultExecutor.hold()
      let task = Task.detached {
        await isolated.run {
          log.record(here("on the actor"))
          await isolated.call(log, "same-actor call")
          log.record(here("back on the actor"))
        }
      }
      testDefaultExecutor.runNextJob(passing: .taskExecutorOnly)
      both.runNextJob(passing: .both)
      // Code isolated to an actor runs inline in a context with its serial
      // executor, whatever the task executor of the context.
      log.expectNext([
        "on the actor [E, E]",
        "same-actor call [E, E]",
        "back on the actor [E, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      testDefaultExecutor.drainAndRelease()
      await task.value
    }

    tests.test("passing E in both roles, preferring P: same-actor calls run inline") {
      let both = ManualExecutor("E")
      let preference = ManualExecutor("P")
      let isolated = CustomActor(both)
      let log = EventLog()
      let task = Task.detached(executorPreference: preference) {
        await isolated.run {
          log.record(here("on the actor"))
          await isolated.call(log, "same-actor call")
          log.record(here("back on the actor"))
        }
      }
      preference.runNextJob(passing: .taskExecutorOnly)
      both.runNextJob(passing: .both)
      // Code isolated to an actor runs inline in a context with its serial
      // executor, whatever the task's preference.
      log.expectNext([
        "on the actor [E, E]",
        "same-actor call [E, E]",
        "back on the actor [E, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      expectEqual(0, preference.pendingJobCount)
      await task.value
    }

    tests.test("passing E in both roles, preferring E: an idle default actor is locked and unlocked for concurrent code, inline") {
      let both = ManualExecutor("E")
      let idle = DefaultActor("default actor")
      let log = EventLog()
      let task = Task.detached(executorPreference: both) {
        await idle.callThenConcurrent(log, "on the default actor", "concurrent")
        log.record(here("done"))
      }
      // The one job: creating the task enqueues it on `E`. The context
      // `(E, E)` matches the preference, which is where a default actor runs,
      // so the runtime locks the idle actor on top of the context, and unlocks
      // it again for the concurrent code.
      both.runNextJob(passing: .both)
      log.expectNext([
        "on the default actor [default actor, E]",
        "concurrent [E, E]",
        "done [E, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      await task.value
    }

    tests.test("passing E in both roles, preferring E: code on a locked default actor calls an actor isolated to E inline") {
      let both = ManualExecutor("E")
      let isolated = CustomActor(both)
      let idle = DefaultActor("default actor")
      let log = EventLog()
      let task = Task.detached(executorPreference: both) {
        await idle.callCustomActor(log, isolated)
        log.record(here("done"))
      }
      // The one job runs in the context `(E, E)`. The task locks the idle
      // default actor on top of it. The call into the actor isolated to `E`
      // unlocks the default actor, which returns to the base context, and the
      // call back locks the default actor again, all inline.
      both.runNextJob(passing: .both)
      log.expectNext([
        "on the default actor [default actor, E]",
        "on the custom actor [E, E]",
        "back on the default actor [default actor, E]",
        "done [E, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      await task.value
    }

    tests.test("passing E in both roles, preferring E: concurrent code running inline is isolated to E") {
      let both = ManualExecutor("E")
      let isolated = CustomActor(both)
      let log = EventLog()
      let task = Task.detached(executorPreference: both) {
        await concurrentCall(log, "concurrent")
        isolated.assumeIsolated { _ in log.record(here("assumed isolated")) }
        isolated.preconditionIsolated()
      }
      both.runNextJob(passing: .both)
      log.expectNext([
        "concurrent [E, E]",
        "assumed isolated [E, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      await task.value
    }

    tests.test("passing only E as task executor, preferring E: code isolated to E isn't tracked as isolated") {
      let both = ManualExecutor("E")
      let isolated = CustomActor(both)
      let log = EventLog()
      let task = Task.detached(executorPreference: both) {
        log.record(here("started"))
        await isolated.call(log, "on the actor")
        await concurrentCall(log, "concurrent")
      }
      both.runNextJob(passing: .taskExecutorOnly)
      expectEqual(1, both.pendingJobCount)
      both.runNextJob(passing: .taskExecutorOnly)
      // Potentially unexpected: An executor that is both receives the jobs of
      // both roles through the same `enqueue`. Running the actor's job passing
      // only the task executor gives a context without a serial executor, so
      // the actor's code isn't tracked as isolated, although `E` runs it
      // exclusively. `#isolation` doesn't see the actor, and `assumeIsolated`
      // or `preconditionIsolated` on it would trap. Such an executor should run
      // its jobs passing both roles.
      log.expectNext([
        "started [none, E]",
        "on the actor [none, E]",
        "concurrent [none, E]",
      ])
      expectEqual(0, both.pendingJobCount)
      await task.value
    }

    // MARK: A pool whose threads are serial executors

    tests.test("a loop of a pool passing both roles, preferring the pool: concurrent code and code isolated to the loop run inline") {
      let pool = ManualExecutor("pool")
      let loop = ManualExecutor("L1", pool: pool)
      let isolated = CustomActor(loop)
      let log = EventLog()
      let task = Task.detached(executorPreference: pool) {
        log.record(here("started"))
        await concurrentCall(log, "concurrent")
        await isolated.call(log, "on the loop's actor")
        log.record(here("back"))
      }
      // The task is enqueued on the pool. Its loop `L1` runs it, passing itself
      // as the serial executor and the pool as the task executor.
      expectEqual(1, pool.pendingJobCount)
      loop.runNextJob(passing: .both)
      // The context `(L1, pool)` matches the preference, so concurrent code
      // runs inline. It has `L1`, so code isolated to `L1` runs inline too.
      log.expectNext([
        "started [L1, L1]",
        "concurrent [L1, L1]",
        "on the loop\'s actor [L1, L1]",
        "back [L1, L1]",
      ])
      expectEqual(0, pool.pendingJobCount)
      expectEqual(0, loop.pendingJobCount)
      await task.value
    }

    tests.test("a loop of a pool passing both roles, preferring the loop: concurrent code runs inline") {
      let pool = ManualExecutor("pool")
      let loop = ManualExecutor("L1", pool: pool)
      let log = EventLog()
      let task = Task.detached(executorPreference: loop) {
        log.record(here("started"))
        await concurrentCall(log, "concurrent")
        log.record(here("back"))
      }
      // The task is enqueued on `L1`, which runs it passing itself as the
      // serial executor and its pool as the task executor. The context
      // `(L1, pool)` matches the preference for `L1`, because its serial
      // executor is `L1`.
      loop.runNextJob(passing: .both)
      log.expectNext([
        "started [L1, L1]",
        "concurrent [L1, L1]",
        "back [L1, L1]",
      ])
      expectEqual(0, pool.pendingJobCount)
      expectEqual(0, loop.pendingJobCount)
      await task.value
    }

    await runAllTestsAsync()
  }
}
