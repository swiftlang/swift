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
/// other clock, and that clock is a `SchedulingClock`, the executor hands the
/// work over to the clock, which is responsible for running it at the right
/// time.
///
/// An executor says whether it schedules against `ContinuousClock` and
/// `SuspendingClock` itself using `supportsScheduling(on:)`.  If it does not,
/// those clocks are scheduled by the default executor (or, if that does not
/// either, by the runtime's global enqueue function), which gives the job to
/// the executor to run when the time comes.  So forwarding to the clock is
/// only for other clocks.
///
/// A clock should implement `schedule(_:at:tolerance:)` by arranging to run the
/// job on some executor at the fire time.  It must not do that by asking an
/// executor to schedule the job against the clock itself, as the executor would
/// simply hand the job straight back; instead it should either schedule against
/// one of the standard clocks (as `ContinuousClock` and `SuspendingClock`
/// schedule against the default executor), or hold the job itself and enqueue
/// it when the time arrives.
///
/// The tokens that these methods return are cancelled through
/// `JobCancellationToken.canceller`.  If a clock keeps the job itself, it
/// should also conform to `JobCanceller` (and so be a class) and name itself as
/// the canceller.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
public protocol SchedulingClock<Duration>: Clock<Duration> {
  /// Run the given job on an unspecified executor at some point after the
  /// fire time.
  ///
  /// Parameters:
  ///
  /// - job:       The job we wish to run.
  /// - at:        The time at which we would like it to run.
  /// - tolerance: The ideal maximum delay we are willing to tolerate.
  ///
  /// Returns a ``JobCancellationToken`` that can be used to cancel the job
  /// before it runs.
  func schedule(_ job: consuming ExecutorJob,
                at: SchedulingExecutorFireTime<Self>,
                tolerance: Duration?) -> JobCancellationToken

  /// Run the given job on the specified executor at some point after the
  /// fire time.
  ///
  /// The default implementation uses `schedule(_:at:tolerance:)` to schedule a
  /// job that does `executor.enqueue(job)`.  If a clock knows that the executor
  /// it has been asked to use is the one it would run jobs on anyway, it can
  /// short-circuit that and use `schedule(_:at:tolerance:)` with the original
  /// job.
  ///
  /// Cancelling the job gives `job` back, as for any other scheduled job.
  /// (The default implementation arranges that even though it has to schedule
  /// a different job on the clock.)
  ///
  /// Returns a ``JobCancellationToken`` that can be used to cancel the job
  /// before it runs.
  func schedule(_ job: consuming ExecutorJob,
                on executor: some Executor,
                at: SchedulingExecutorFireTime<Self>,
                tolerance: Duration?) -> JobCancellationToken
}

@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
extension SchedulingClock {
  public func schedule(_ job: consuming ExecutorJob,
                       on executor: some Executor,
                       at fireTime: SchedulingExecutorFireTime<Self>,
                       tolerance: Duration?) -> JobCancellationToken {
    return _scheduleUsingTrampoline(job, on: executor, at: fireTime,
                                    tolerance: tolerance, clock: self)
  }

  /// A default `sleep` that wakes the task by scheduling the job that resumes
  /// it with the clock, so that a clock need only implement
  /// `schedule(_:at:tolerance:)` to be usable with `Task.sleep()`.  Cancelling
  /// the task takes the timer back out of the clock immediately.
  public func sleep(
    until deadline: Instant, tolerance: Duration?
  ) async throws {
    guard #available(StdlibDeploymentTarget 6.5, *) else {
      fatalError("we shouldn't get here; if we have, availability is broken")
    }

    try Task.checkCancellation()
    try await Task<Never, Never>._sleepScheduling { job in
      self.schedule(job, at: .at(deadline), tolerance: tolerance)
    }
  }
}

/// Shared state between a trampoline job and the thing that can cancel it.
@available(StdlibDeploymentTarget 6.5, *)
private final class _TrampolineState: @unchecked Sendable {
  /// Set (before the trampoline is enqueued) if the job was cancelled, in which
  /// case the trampoline must not enqueue it.
  var cancelled = false
}

/// Cancels a job that has been scheduled on a clock through a trampoline,
/// handing back the original job instead of the trampoline.
@available(StdlibDeploymentTarget 6.5, *)
private final class _TrampolineCanceller: JobCanceller, @unchecked Sendable {
  let original: UnownedJob
  let state: _TrampolineState

  /// The token for the trampoline.  It is set when we are created, and taken
  /// when we are asked to cancel, which happens at most once because that
  /// consumes our token.
  var trampolineToken: JobCancellationToken?

  init(original: UnownedJob, state: _TrampolineState,
       trampolineToken: consuming JobCancellationToken) {
    self.original = original
    self.state = state
    self.trampolineToken = consume trampolineToken
  }

  func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    guard let trampolineToken = trampolineToken.take(),
          let trampoline = trampolineToken.cancel() else {
      return nil
    }

    // The trampoline is a task, which has to be run to be freed, so tell it
    // to do nothing and let it run.  We hand back the job it was carrying
    // instead.
    state.cancelled = true
    Task<Never, Never>.defaultExecutor.enqueue(trampoline)
    return ExecutorJob(original)
  }
}

/// Run `job` on `executor` at some point after the fire time, by scheduling a
/// trampoline job on `clock`, which enqueues `job` on `executor` when it runs.
@available(StdlibDeploymentTarget 6.5, *)
internal func _scheduleUsingTrampoline<Clk: SchedulingClock>(
  _ job: consuming ExecutorJob,
  on executor: some Executor,
  at fireTime: SchedulingExecutorFireTime<Clk>,
  tolerance: Clk.Duration?,
  clock: Clk
) -> JobCancellationToken {
  let original = UnownedJob(job)
  let state = _TrampolineState()

  let flags = taskCreateFlags(
    priority: TaskPriority(ExecutorJob(original).priority),
    isChildTask: false,
    copyTaskLocals: false,
    inheritContext: false,
    enqueueJob: false,
    addPendingGroupTaskUnconditionally: false,
    isDiscardingTask: false,
    isSynchronousStart: false
  )
  let (trampolineTask, _) = Builtin.createAsyncTask(flags) {
    if !state.cancelled {
      executor.enqueue(original)
    }
  }
  let trampoline = ExecutorJob(
    context: Builtin.convertTaskToJob(trampolineTask))

  let trampolineToken = clock.schedule(trampoline, at: fireTime,
                                       tolerance: tolerance)
  let canceller = _TrampolineCanceller(original: original, state: state,
                                       trampolineToken: trampolineToken)
  return unsafe JobCancellationToken(canceller: canceller, opaqueData: [0, 0])
}

/// A canceller for work that was handed to the runtime's global enqueue
/// functions, which can't take a timer back.
@available(StdlibDeploymentTarget 6.5, *)
private final class _NonCancellingCanceller: JobCanceller, @unchecked Sendable {
  static let shared = _NonCancellingCanceller()

  func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    return nil
  }
}

/// Schedule a job against one of the standard library's clocks, with no
/// particular executor in mind.
///
/// We give it to the default executor if that executor schedules against the
/// clock itself; if not, we use the runtime's global enqueue function, which
/// is how an executor written in C (against `ExecutorImpl.h`) schedules, and
/// which handles both of the clocks but can't cancel.  `fallback` gives us the
/// arguments for that function, and is only evaluated if we need them.
@available(StdlibDeploymentTarget 6.5, *)
private func _scheduleOnDefaultExecutor<C: Clock>(
  _ job: consuming ExecutorJob,
  at fireTime: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?,
  clock: C,
  systemClock: SystemClockID,
  fallback: () -> (clockID: Int32, seconds: Int64, nanoseconds: Int64,
                   toleranceSeconds: Int64, toleranceNanoseconds: Int64)
) -> JobCancellationToken {
  if let executor = Task<Never, Never>.defaultExecutor.asSchedulingExecutor,
     executor.supportsScheduling(on: systemClock) {
    return executor.schedule(job, at: fireTime, tolerance: tolerance,
                             clock: clock)
  }

  let args = fallback()
  _enqueueJobGlobalWithDeadline(
    args.seconds, args.nanoseconds,
    args.toleranceSeconds, args.toleranceNanoseconds,
    args.clockID, UnownedJob(job))
  return unsafe JobCancellationToken(canceller: _NonCancellingCanceller.shared,
                                     opaqueData: [0, 0])
}

/// Run a job on `executor` against one of the standard library's clocks:
/// directly, if the executor schedules against the clock itself, and otherwise
/// by way of a trampoline that the default executor schedules.
@available(StdlibDeploymentTarget 6.5, *)
private func _scheduleOnExecutor<C: SchedulingClock>(
  _ job: consuming ExecutorJob,
  on executor: some Executor,
  at fireTime: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?,
  clock: C,
  systemClock: SystemClockID
) -> JobCancellationToken {
  if let scheduler = executor.asSchedulingExecutor,
     scheduler.supportsScheduling(on: systemClock) {
    return scheduler.schedule(job, at: fireTime, tolerance: tolerance,
                              clock: clock)
  }
  return _scheduleUsingTrampoline(job, on: executor, at: fireTime,
                                  tolerance: tolerance, clock: clock)
}

/// The standard library clock that `clock` is, if it is one.
@available(StdlibDeploymentTarget 6.5, *)
internal func _systemClockID<C: Clock>(_ clock: C) -> SystemClockID? {
  if clock is ContinuousClock {
    return .continuous
  } else if clock is SuspendingClock {
    return .suspending
  }
  return nil
}

@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
extension ContinuousClock: SchedulingClock {
  public func schedule(_ job: consuming ExecutorJob,
                       at fireTime: SchedulingExecutorFireTime<ContinuousClock>,
                       tolerance: Duration?) -> JobCancellationToken {
    return _scheduleOnDefaultExecutor(
      job, at: fireTime, tolerance: tolerance, clock: self,
      systemClock: .continuous) {
      let (clockID, seconds, nanoseconds) =
        timestamp(for: fireTime.asInstant(clock: self))
      let (toleranceSeconds, toleranceNanoseconds) = tolerance.map {
        durationComponents(for: $0)
      } ?? (0, -1)
      return (clockID: clockID.rawValue, seconds: seconds,
              nanoseconds: nanoseconds, toleranceSeconds: toleranceSeconds,
              toleranceNanoseconds: toleranceNanoseconds)
    }
  }

  public func schedule(_ job: consuming ExecutorJob,
                       on executor: some Executor,
                       at fireTime: SchedulingExecutorFireTime<ContinuousClock>,
                       tolerance: Duration?) -> JobCancellationToken {
    return _scheduleOnExecutor(job, on: executor, at: fireTime,
                               tolerance: tolerance, clock: self,
                               systemClock: .continuous)
  }
}

@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
extension SuspendingClock: SchedulingClock {
  public func schedule(_ job: consuming ExecutorJob,
                       at fireTime: SchedulingExecutorFireTime<SuspendingClock>,
                       tolerance: Duration?) -> JobCancellationToken {
    return _scheduleOnDefaultExecutor(
      job, at: fireTime, tolerance: tolerance, clock: self,
      systemClock: .suspending) {
      let (clockID, seconds, nanoseconds) =
        timestamp(for: fireTime.asInstant(clock: self))
      let (toleranceSeconds, toleranceNanoseconds) = tolerance.map {
        durationComponents(for: $0)
      } ?? (0, -1)
      return (clockID: clockID.rawValue, seconds: seconds,
              nanoseconds: nanoseconds, toleranceSeconds: toleranceSeconds,
              toleranceNanoseconds: toleranceNanoseconds)
    }
  }

  public func schedule(_ job: consuming ExecutorJob,
                       on executor: some Executor,
                       at fireTime: SchedulingExecutorFireTime<SuspendingClock>,
                       tolerance: Duration?) -> JobCancellationToken {
    return _scheduleOnExecutor(job, on: executor, at: fireTime,
                               tolerance: tolerance, clock: self,
                               systemClock: .suspending)
  }
}

// MARK: - Forwarding from executors

/// Returns `true` if `clock` is one that an executor should forward to.
@available(StdlibDeploymentTarget 6.5, *)
internal func _isSchedulingClock<C: Clock>(_ clock: C) -> Bool {
  return clock is any SchedulingClock
}

@available(StdlibDeploymentTarget 6.5, *)
private func _open<E: SchedulingClock, C: Clock>(
  _ schedulingClock: E,
  _ job: consuming ExecutorJob,
  on executor: some Executor,
  at fireTime: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?
) -> JobCancellationToken {
  // `E` is the dynamic type of the clock that `C` was, so these can't fail.
  return schedulingClock.schedule(
    job, on: executor,
    at: fireTime as! SchedulingExecutorFireTime<E>,
    tolerance: tolerance as! E.Duration?)
}

/// Hand a job to a `SchedulingClock` to be run on `executor`.  The
/// caller must already have checked that `clock` is one.
@available(StdlibDeploymentTarget 6.5, *)
internal func _scheduleUsingClock<C: Clock>(
  _ job: consuming ExecutorJob,
  on executor: some Executor,
  at fireTime: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?,
  clock: C
) -> JobCancellationToken {
  let schedulingClock = clock as! any SchedulingClock
  return _open(schedulingClock, job, on: executor,
               at: fireTime, tolerance: tolerance)
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
