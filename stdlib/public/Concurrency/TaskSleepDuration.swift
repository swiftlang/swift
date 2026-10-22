//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@_unavailableInEmbedded
extension ContinuousClock {
  func timestamp(for instant: Instant)
    -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, nanoseconds) = durationComponents(for: instant._value)
    return (clockID: .continuous, seconds: seconds, nanoseconds: nanoseconds)
  }

  func durationComponents(for duration: Duration)
    -> (seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000
    return (seconds: seconds, nanoseconds: nanoseconds)
  }
}

@_unavailableInEmbedded
extension SuspendingClock {
  func timestamp(for instant: Instant)
    -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, nanoseconds) = durationComponents(for: instant._value)
    return (clockID: .suspending, seconds: seconds, nanoseconds: nanoseconds)
  }

  func durationComponents(for duration: Duration)
    -> (seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000
    return (seconds: seconds, nanoseconds: nanoseconds)
  }
}

fileprivate func timestamp<C: Clock>(for instant: C.Instant, clock: C)
  -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64) {
  #if !$Embedded
  if let continuousClock = clock as? ContinuousClock {
    return continuousClock.timestamp(for: instant as! ContinuousClock.Instant)
  } else if let suspendingClock = clock as? SuspendingClock {
    return suspendingClock.timestamp(for: instant as! SuspendingClock.Instant)
  }
  #endif
  fatalError("unknown clock in fallback path")
}

fileprivate func durationComponents<C: Clock>(for duration: C.Duration, clock: C)
  -> (seconds: Int64, nanoseconds: Int64) {
  #if !$Embedded
  if let continuousClock = clock as? ContinuousClock {
    return continuousClock.durationComponents(for: duration as! ContinuousClock.Duration)
  } else if let suspendingClock = clock as? SuspendingClock {
    return suspendingClock.durationComponents(for: duration as! SuspendingClock.Duration)
  }
  #endif
  fatalError("unknown clock in fallback path")
}

#if !$Embedded
/// A one-shot holder for a `JobCancellationToken`, so that the (escaping,
/// `sending`) cancellation handler of a sleep can consume the token.
@available(StdlibDeploymentTarget 6.3, *)
fileprivate final class _SleepTokenBox: @unchecked Sendable {
  var token: JobCancellationToken?

  init(_ token: consuming JobCancellationToken) {
    self.token = consume token
  }

  /// Take the token.  Only the cancellation handler calls this, and it
  /// runs at most once.
  func take() -> JobCancellationToken? {
    return token.take()
  }
}
#endif

@available(StdlibDeploymentTarget 5.7, *)
@_unavailableInEmbedded
extension Task where Success == Never, Failure == Never {
  @available(StdlibDeploymentTarget 5.7, *)
  @diagnose(UselessAvailabilityCheck, as: ignored)
  internal static func _sleep<C: Clock>(
    until instant: C.Instant,
    tolerance: C.Duration?,
    clock: C
  ) async throws {
    #if !$Embedded
    if #available(StdlibDeploymentTarget 6.5, *) {
      try await _sleep(run: .at(instant), tolerance: tolerance, clock: clock)
      return
    }
    #endif
    try await _sleepWithoutSchedulingExecutor(
      until: instant, tolerance: tolerance, clock: clock)
  }

  #if !$Embedded

  /// Sleep until the given time, or until the task is cancelled.
  ///
  /// We hand the continuation's resume half to a timer job that we schedule
  /// on the current scheduling executor, and keep the token that lets us
  /// cancel it.  If the task is cancelled, we ask the executor for the job
  /// back.  If we get it, it can never run, so we destroy it and resume the
  /// continuation ourselves; if we don't, the timer has already fired (or is
  /// firing) and will resume the continuation normally.  Either way the
  /// continuation is resumed exactly once, and a cancelled sleep doesn't leave
  /// its timer behind in the executor.
  @available(StdlibDeploymentTarget 6.5, *)
  @diagnose(UselessAvailabilityCheck, as: ignored)
  internal static func _sleep<C: Clock>(
    run fireTime: SchedulingExecutorFireTime<C>,
    tolerance: C.Duration?,
    clock: C
  ) async throws {
    guard #available(StdlibDeploymentTarget 6.3, *),
          let executor = Task.currentSchedulingExecutor else {
      try await _sleepWithoutSchedulingExecutor(
        until: fireTime.asInstant(clock: clock),
        tolerance: tolerance,
        clock: clock)
      return
    }

    try await withContinuation(
      of: Void.self, throwing: _Concurrency.CancellationError.self
    ) { (continuation: consuming Continuation<Void, _Concurrency.CancellationError>,
         waiter: consuming ContinuationWaiter<Void, _Concurrency.CancellationError>)
        async throws(_Concurrency.CancellationError) -> Void in
      // The timer job and the cancellation handler both need to be able to
      // resume the continuation, and ownership of the job decides which of
      // them does, so move to the copyable form.
      let rawContinuation = continuation._takeContext()
      let unsafeContinuation = unsafe UnsafeContinuation<Void, _Concurrency.CancellationError>(
        rawContinuation)

      let job = _taskCreateNullaryContinuationJob(
        priority: Int(Task.currentPriority.rawValue),
        continuation: rawContinuation)

      // The cancellation handler is `sending` and can't consume the token
      // directly, so hand it over in a box.
      let box = _SleepTokenBox(
        executor.enqueue(ExecutorJob(context: job),
                         run: fireTime,
                         tolerance: tolerance,
                         clock: clock))

      try await waiter.wait(onCancel: {
        guard let token = box.take(), let job = token.cancel() else {
          // The timer has already started; it will resume the continuation.
          return
        }

        // We got the job back, so it will never run.  Discard it and resume
        // the continuation ourselves.
        job.destroy()
        unsafe unsafeContinuation.resume(
          throwing: _Concurrency.CancellationError())
      })
    }
  }

  #endif // !$Embedded

  /// The fallback for when there is no scheduling executor, which uses the
  /// runtime's global enqueue functions.  Those can't cancel the timer, so
  /// we only notice cancellation once it has fired.
  @available(StdlibDeploymentTarget 5.7, *)
  @diagnose(UselessAvailabilityCheck, as: ignored)
  internal static func _sleepWithoutSchedulingExecutor<C: Clock>(
    until instant: C.Instant,
    tolerance: C.Duration?,
    clock: C
  ) async throws {
    let _: () = await Builtin.withUnsafeContinuation {
      (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(
        priority: Int(Task.currentPriority.rawValue),
        continuation: continuation)

      let (clockID, seconds, nanoseconds) = timestamp(for: instant,
                                                      clock: clock)
      let toleranceSeconds: Int64
      let toleranceNanoseconds: Int64
      if #available(StdlibDeploymentTarget 6.3, *) {
        if let tolerance = tolerance {
          (toleranceSeconds, toleranceNanoseconds)
            = durationComponents(for: tolerance, clock: clock)
        } else {
          toleranceSeconds = 0
          toleranceNanoseconds = -1
        }
      } else {
        fatalError("we shouldn't get here; if we have, availability is broken")
      }

      if #available(StdlibDeploymentTarget 5.9, *) {
        _enqueueJobGlobalWithDeadline(
          seconds, nanoseconds,
          toleranceSeconds, toleranceNanoseconds,
          clockID.rawValue, UnownedJob(context: job))
      } else {
        fatalError("we shouldn't get here; if we have, availability is broken")
      }
    }

    try Task.checkCancellation()
  }

  #if !$Embedded
  /// Suspends the current task until the given deadline within a tolerance.
  ///
  /// If the task is canceled before the time ends, this function throws
  /// `_Concurrency.CancellationError`.
  ///
  /// This function doesn't block the underlying thread.
  ///
  ///       try await Task.sleep(until: .now + .seconds(3))
  ///
  @available(SwiftStdlib 5.7, *)
  public static func sleep<C: Clock>(
    until deadline: C.Instant,
    tolerance: C.Instant.Duration? = nil,
    clock: C = .continuous
  ) async throws {
    try await clock.sleep(until: deadline, tolerance: tolerance)
  }

  /// Suspends the current task for the given duration.
  ///
  /// If the task is canceled before the time ends, this function throws
  /// `_Concurrency.CancellationError`.
  ///
  /// This function doesn't block the underlying thread.
  ///
  ///       try await Task.sleep(for: .seconds(3))
  ///
  @available(SwiftStdlib 5.7, *)
  @export(implementation)
  public static func sleep<C: Clock>(
    for duration: C.Instant.Duration,
    tolerance: C.Instant.Duration? = nil,
    clock: C = .continuous
  ) async throws {
    try await clock.sleep(for: duration, tolerance: tolerance)
  }
  #endif
}
#else
@available(SwiftStdlib 5.7, *)
@available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
extension Task where Success == Never, Failure == Never {
  @available(SwiftStdlib 5.7, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func sleep<C: Clock>(
    until deadline: C.Instant,
    tolerance: C.Instant.Duration? = nil,
    clock: C = .continuous
  ) async throws {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
  @available(SwiftStdlib 5.7, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  @export(implementation)
  public static func sleep<C: Clock>(
    for duration: C.Instant.Duration,
    tolerance: C.Instant.Duration? = nil,
    clock: C = .continuous
  ) async throws {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
}
#endif
