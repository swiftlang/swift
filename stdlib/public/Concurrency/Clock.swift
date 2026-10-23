//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import Swift

/// A mechanism in which to measure time, and delay work until a given point
/// in time.
///
/// Types that conform to the `Clock` protocol define a concept of "now" which
/// is the specific instant in time that property is accessed. Any pair of calls
/// to the `now` property may have a minimum duration between them - this
/// minimum resolution is exposed by the `minimumResolution` property to inform
/// any user of the type the expected granularity of accuracy.
///
/// One of the primary uses for clocks is to schedule task sleeping. This method
/// resumes the calling task after a given deadline has been met or passed with
/// a given tolerance value. The tolerance is expected as a leeway around the
/// deadline. The clock may reschedule tasks within the tolerance to ensure
/// efficient execution of resumptions by reducing potential operating system
/// wake-ups. If no tolerance is specified (i.e. nil is passed in) the sleep
/// function is expected to schedule with a default tolerance strategy.
///
/// For more information about specific clocks see `ContinuousClock` and
/// `SuspendingClock`.
@available(StdlibDeploymentTarget 5.7, *)
public protocol Clock<Duration>: Sendable {
  associatedtype Duration
  associatedtype Instant: InstantProtocol where Instant.Duration == Duration

  var now: Instant { get }
  var minimumResolution: Instant.Duration { get }

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  func sleep(until deadline: Instant, tolerance: Instant.Duration?) async throws
#endif
}

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
/// A clock that can schedule work itself.
///
/// Executors only know how to schedule work against the clocks they
/// recognise.  When a `SchedulingExecutor` is asked to schedule against some
/// other clock, and that clock is an `EnqueueingClock`, the executor hands the
/// work over to the clock, which is responsible for running it at the right
/// time.
///
/// A clock should implement `run` by arranging to run the job on some executor
/// at the fire time.  It must not do that by asking the current executor to
/// schedule the job against the clock itself, as the executor would simply
/// hand the job straight back; instead it should either schedule against a
/// clock the executor does know, as `ContinuousClock` and `SuspendingClock`
/// do, or hold the job itself and enqueue it when the time arrives.
///
/// The tokens that these methods return are cancelled through
/// `JobCancellationToken.canceller`.  If a clock keeps the job itself, it
/// should also conform to `JobCanceller` and name itself as the canceller.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.3, *)
public protocol EnqueueingClock<Duration>: Clock<Duration> {
  /// Run the given job on an unspecified executor at some point after the
  /// fire time.
  ///
  /// Parameters:
  ///
  /// - job:       The job we wish to run.
  /// - run:       The time at which we would like it to run.
  /// - tolerance: The ideal maximum delay we are willing to tolerate.
  ///
  /// Returns a ``JobCancellationToken`` that can be used to cancel the job
  /// before it runs.
  func run(_ job: consuming ExecutorJob,
           run: SchedulingExecutorFireTime<Self>,
           tolerance: Duration?) -> JobCancellationToken

  /// Resume the given continuation at some point after the fire time.
  ///
  /// Resuming a continuation schedules the task on whatever executor it is
  /// bound to, so no executor needs to be named.  The default implementation
  /// creates a job that resumes the continuation, and passes it to
  /// `run(_:run:tolerance:)`; a clock that implements this natively must also
  /// implement `cancel(continuationWithToken:)` on whatever it names as the
  /// canceller.
  ///
  /// Returns a ``JobCancellationToken`` that can be used to take the
  /// continuation back, using `cancelContinuation()`, before it is resumed.
  func run(resuming continuation: consuming TimerContinuation,
           run: SchedulingExecutorFireTime<Self>,
           tolerance: Duration?) -> JobCancellationToken

  /// Enqueue the given job on the specified executor at some point after the
  /// fire time.
  ///
  /// The default implementation uses `run` to run a job that does
  /// `executor.enqueue(job)`.  If a clock knows that the executor it has been
  /// asked to use is the one it would run jobs on anyway, it can short-circuit
  /// that and use `run` with the original job.
  ///
  /// Cancelling the job hands back the trampoline job rather than `job`
  /// itself; running the trampoline enqueues `job` on the executor, so that
  /// is equivalent to running `job`, but it must not simply be discarded.
  ///
  /// Returns a ``JobCancellationToken`` that can be used to cancel the job
  /// before it runs.
  func enqueue(_ job: consuming ExecutorJob,
               on executor: some Executor,
               run: SchedulingExecutorFireTime<Self>,
               tolerance: Duration?) -> JobCancellationToken
}

@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.3, *)
extension EnqueueingClock {
  public func enqueue(_ job: consuming ExecutorJob,
                      on executor: some Executor,
                      run at: SchedulingExecutorFireTime<Self>,
                      tolerance: Duration?) -> JobCancellationToken {
    let trampoline = job.createTrampoline(to: executor)
    return run(trampoline, run: at, tolerance: tolerance)
  }

  public func run(resuming continuation: consuming TimerContinuation,
                  run at: SchedulingExecutorFireTime<Self>,
                  tolerance: Duration?) -> JobCancellationToken {
    let raw = unsafe continuation.continuation.context
    let job = _taskCreateNullaryContinuationJob(
      priority: Int(Task.currentPriority.rawValue),
      continuation: raw)
    return run(ExecutorJob(context: job), run: at, tolerance: tolerance)
  }

  /// A default `sleep` that resumes the task by scheduling its continuation
  /// with the clock, so that a clock need only implement `run` to be usable
  /// with `Task.sleep()`.  Cancelling the task takes the timer back out of the
  /// clock immediately.
  public func sleep(until deadline: Instant, tolerance: Duration?) async throws {
    guard #available(StdlibDeploymentTarget 6.5, *) else {
      fatalError("we shouldn't get here; if we have, availability is broken")
    }

    try Task.checkCancellation()
    try await Task<Never, Never>._sleepScheduling { timer in
      self.run(resuming: timer, run: .at(deadline), tolerance: tolerance)
    }
  }
}

@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.3, *)
extension ContinuousClock: EnqueueingClock {
  public func run(_ job: consuming ExecutorJob,
                  run at: SchedulingExecutorFireTime<ContinuousClock>,
                  tolerance: Duration?) -> JobCancellationToken {
    guard let executor = Task<Never, Never>.currentSchedulingExecutor else {
      fatalError("no scheduling executor is available to run a job")
    }
    return executor.enqueue(job, run: at, tolerance: tolerance, clock: self)
  }

  public func run(resuming continuation: consuming TimerContinuation,
                  run at: SchedulingExecutorFireTime<ContinuousClock>,
                  tolerance: Duration?) -> JobCancellationToken {
    guard let executor = Task<Never, Never>.currentSchedulingExecutor else {
      fatalError("no scheduling executor is available to resume a continuation")
    }
    return executor.enqueue(resuming: continuation, run: at,
                            tolerance: tolerance, clock: self)
  }
}

@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.3, *)
extension SuspendingClock: EnqueueingClock {
  public func run(_ job: consuming ExecutorJob,
                  run at: SchedulingExecutorFireTime<SuspendingClock>,
                  tolerance: Duration?) -> JobCancellationToken {
    guard let executor = Task<Never, Never>.currentSchedulingExecutor else {
      fatalError("no scheduling executor is available to run a job")
    }
    return executor.enqueue(job, run: at, tolerance: tolerance, clock: self)
  }

  public func run(resuming continuation: consuming TimerContinuation,
                  run at: SchedulingExecutorFireTime<SuspendingClock>,
                  tolerance: Duration?) -> JobCancellationToken {
    guard let executor = Task<Never, Never>.currentSchedulingExecutor else {
      fatalError("no scheduling executor is available to resume a continuation")
    }
    return executor.enqueue(resuming: continuation, run: at,
                            tolerance: tolerance, clock: self)
  }
}

// MARK: - Forwarding from executors

/// Returns `true` if `clock` is one that an executor should forward to.
@available(StdlibDeploymentTarget 6.3, *)
internal func _isEnqueueingClock<C: Clock>(_ clock: C) -> Bool {
  return clock is any EnqueueingClock
}

@available(StdlibDeploymentTarget 6.3, *)
private func _open<E: EnqueueingClock, C: Clock>(
  _ enqueueingClock: E,
  _ job: consuming ExecutorJob,
  on executor: some Executor,
  run: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?
) -> JobCancellationToken {
  // `E` is the dynamic type of the clock that `C` was, so these can't fail.
  return enqueueingClock.enqueue(
    job, on: executor,
    run: run as! SchedulingExecutorFireTime<E>,
    tolerance: tolerance as! E.Duration?)
}

@available(StdlibDeploymentTarget 6.3, *)
private func _open<E: EnqueueingClock, C: Clock>(
  _ enqueueingClock: E,
  resuming continuation: consuming TimerContinuation,
  run: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?
) -> JobCancellationToken {
  return enqueueingClock.run(
    resuming: continuation,
    run: run as! SchedulingExecutorFireTime<E>,
    tolerance: tolerance as! E.Duration?)
}

/// Hand a job to an `EnqueueingClock` to be enqueued on `executor`.  The
/// caller must already have checked that `clock` is one.
@available(StdlibDeploymentTarget 6.3, *)
internal func _enqueueUsingClock<C: Clock>(
  _ job: consuming ExecutorJob,
  on executor: some Executor,
  run: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?,
  clock: C
) -> JobCancellationToken {
  let enqueueingClock = clock as! any EnqueueingClock
  return _open(enqueueingClock, job, on: executor,
               run: run, tolerance: tolerance)
}

/// Hand a continuation to an `EnqueueingClock` to be resumed.  The caller
/// must already have checked that `clock` is one.
@available(StdlibDeploymentTarget 6.3, *)
internal func _resumeUsingClock<C: Clock>(
  _ continuation: consuming TimerContinuation,
  run: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?,
  clock: C
) -> JobCancellationToken {
  let enqueueingClock = clock as! any EnqueueingClock
  return _open(enqueueingClock, resuming: continuation,
               run: run, tolerance: tolerance)
}
#endif // !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

@available(StdlibDeploymentTarget 5.7, *)
extension Clock {
  /// Measure the elapsed time to execute a closure.
  ///
  ///       let clock = ContinuousClock()
  ///       let elapsed = clock.measure {
  ///          someWork()
  ///       }
  @available(StdlibDeploymentTarget 5.7, *)
  public func measure(_ work: () throws -> Void) rethrows -> Instant.Duration {
    let start = now
    try work()
    let end = now
    return start.duration(to: end)
  }

  /// Measure the elapsed time to execute an asynchronous closure.
  ///
  ///       let clock = ContinuousClock()
  ///       let elapsed = await clock.measure {
  ///          await someWork()
  ///       }
  @available(StdlibDeploymentTarget 5.7, *)
  @export(implementation)
  public nonisolated(nonsending) func measure(
    _ work: nonisolated(nonsending) () async throws -> Void
  ) async rethrows -> Instant.Duration {
    let start = now
    try await work()
    let end = now
    return start.duration(to: end)
  }

  @available(StdlibDeploymentTarget 5.7, *)
  @export(implementation)
  @available(*, deprecated, message: "Replaced by nonisolated(nonsending) overload")
  public func measure(
    isolation: isolated (any Actor)? = #isolation,
    _ work: () async throws -> Void
  ) async rethrows -> Instant.Duration {
    let start = now
    try await work()
    let end = now
    return start.duration(to: end)
  }

  // Note: hack to stage out @_unsafeInheritExecutor forms of various functions
  // in favor of #isolation. The _unsafeInheritExecutor_ prefix is meaningful
  // to the type checker.
  //
  // This function also doubles as an ABI-compatibility shim predating the
  // introduction of #isolation.
  @available(StdlibDeploymentTarget 5.7, *)
  @_silgen_name("$ss5ClockPsE7measurey8DurationQzyyYaKXEYaKF")
  @_unsafeInheritExecutor // for ABI compatibility
  public func _unsafeInheritExecutor_measure(
    _ work: () async throws -> Void
  ) async rethrows -> Instant.Duration {
    let start = now
    try await work()
    let end = now
    return start.duration(to: end)
  }
}

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(StdlibDeploymentTarget 5.7, *)
extension Clock {
  /// Suspends for the given duration.
  ///
  /// Prefer to use the `sleep(until:tolerance:)` method on `Clock` if you have
  /// access to an absolute instant.
  @available(StdlibDeploymentTarget 5.7, *)
  @export(implementation)
  public func sleep(
    for duration: Instant.Duration,
    tolerance: Instant.Duration? = nil
  ) async throws {
    try await sleep(until: now.advanced(by: duration), tolerance: tolerance)
  }
}
#endif

enum _ClockID: Int32 {
  case continuous = 1
  case suspending = 2
  case walltime = 3
}

/// Identifier for the standard library clocks.
@nonexhaustive
@available(StdlibDeploymentTarget 6.5, *)
public enum SystemClockID: UInt8, Sendable, Hashable {
  case continuous = 1
  case suspending = 2
}

@available(StdlibDeploymentTarget 5.7, *)
@_silgen_name("swift_get_time")
internal func _getTime(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: CInt)

@available(StdlibDeploymentTarget 5.7, *)
@_silgen_name("swift_get_clock_res")
internal func _getClockRes(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: CInt)

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_sleep")
internal func _sleep(
  seconds: Int64,
  nanoseconds: Int64)
