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
/// A flag that is claimed exactly once, by whichever of the timer and the
/// cancellation handler gets there first.
@available(StdlibDeploymentTarget 5.7, *)
@safe
fileprivate final class _SleepClaim: @unchecked Sendable {
  let wordPtr: UnsafeMutablePointer<Builtin.Word>

  init() {
    unsafe wordPtr = .allocate(capacity: 1)
    unsafe Builtin.atomicstore_seqcst_Word(
      wordPtr._rawValue, (0 as Int)._builtinWordValue)
  }

  /// Returns `true` for the first caller only.
  func claim() -> Bool {
    let (_, won) = unsafe Builtin.cmpxchg_seqcst_seqcst_Word(
      wordPtr._rawValue,
      (0 as Int)._builtinWordValue,
      (1 as Int)._builtinWordValue)
    return Bool(_builtinBooleanLiteral: won)
  }

  deinit {
    unsafe wordPtr.deallocate()
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
  /// We hand the continuation's resume half to the current scheduling
  /// executor, which resumes it when the timer fires, and keep the token that
  /// lets us cancel the timer.  If the task is cancelled before the timer
  /// fires, we tell the continuation to throw when it is resumed, and ask the
  /// executor to fire the timer now; if the timer has already fired (or is
  /// firing), it resumes the continuation normally.  Either way the timer is
  /// what resumes the continuation, so it is resumed exactly once, and a
  /// cancelled sleep doesn't leave its timer behind in an executor that can
  /// fire it early.
  @available(StdlibDeploymentTarget 6.5, *)
  @diagnose(UselessAvailabilityCheck, as: ignored)
  internal static func _sleep<C: Clock>(
    run fireTime: SchedulingExecutorFireTime<C>,
    tolerance: C.Duration?,
    clock: C
  ) async throws {
    // An executor may decline to cancel a timer that is about to fire, so
    // make sure a task that is already cancelled doesn't sleep at all.
    try Task.checkCancellation()

    // Use the current scheduling executor, unless it has said that it doesn't
    // schedule against this clock itself, in which case the default executor
    // does it (and failing that, the runtime's global enqueue function).
    var scheduler = Task.currentSchedulingExecutor
    if let systemClock = _systemClockID(clock),
       !(scheduler?.supportsScheduling(on: systemClock) ?? false) {
      if let defaultScheduler = Task.defaultExecutor.asSchedulingExecutor,
         defaultScheduler.supportsScheduling(on: systemClock) {
        scheduler = defaultScheduler
      } else {
        scheduler = nil
      }
    }

    guard let executor = scheduler else {
      try await _sleepWithoutSchedulingExecutor(
        until: fireTime.asInstant(clock: clock),
        tolerance: tolerance,
        clock: clock)
      return
    }

    try await _sleepScheduling { continuation in
      executor.schedule(continuation,
                        at: fireTime,
                        tolerance: tolerance,
                        clock: clock)
    }
  }

  /// Suspend the current task until a timer fires, or until the task is
  /// cancelled.
  ///
  /// `schedule` is handed the continuation that the sleeping task is waiting
  /// on, and must arrange for it to be resumed when the timer fires.  If the
  /// task is cancelled, we use the result to make the continuation throw, and
  /// to fire the timer early.
  @available(StdlibDeploymentTarget 6.5, *)
  @diagnose(UselessAvailabilityCheck, as: ignored)
  internal static func _sleepScheduling(
    _ schedule: (consuming Continuation<Void, _Concurrency.CancellationError>)
      -> ScheduledContinuation<_Concurrency.CancellationError>
  ) async throws {
    try await withContinuation(
      of: Void.self, throwing: _Concurrency.CancellationError.self
    ) { (continuation: consuming Continuation<Void, _Concurrency.CancellationError>,
         waiter: consuming ContinuationWaiter<Void, _Concurrency.CancellationError>)
        async throws(_Concurrency.CancellationError) -> Void in
      try await waiter.wait(for: schedule(continuation))
    }
  }

  #endif // !$Embedded

  /// The fallback for when there is no scheduling executor, which uses the
  /// runtime's global enqueue functions.  Those can't cancel the timer, so
  /// it stays queued until it fires, but cancelling the task still wakes it at
  /// once.
  ///
  /// Those functions can only do anything useful if something provides them:
  /// by default they use the default executor, which is the scheduling executor
  /// that we just failed to find, so this is only of use where the executor
  /// factory supplies its own implementation of
  /// `swift_task_enqueueGlobalWithDeadlineImpl` (or a hook).
  @available(StdlibDeploymentTarget 5.7, *)
  @diagnose(UselessAvailabilityCheck, as: ignored)
  internal static func _sleepWithoutSchedulingExecutor<C: Clock>(
    until instant: C.Instant,
    tolerance: C.Duration?,
    clock: C
  ) async throws {
    // We are the runtime, so this is always true; the check is only here to
    // let us use the declarations below.  (This is not a fallback for an older
    // runtime: an older runtime has its own, older, copy of this code.)
    guard #available(StdlibDeploymentTarget 6.5, *) else {
      fatalError("we shouldn't get here; if we have, availability is broken")
    }

    try Task.checkCancellation()

    let (clockID, seconds, nanoseconds) = timestamp(for: instant, clock: clock)
    let toleranceSeconds: Int64
    let toleranceNanoseconds: Int64
    if let tolerance = tolerance {
      (toleranceSeconds, toleranceNanoseconds)
        = durationComponents(for: tolerance, clock: clock)
    } else {
      toleranceSeconds = 0
      toleranceNanoseconds = -1
    }

    #if !$Embedded
    // The timer can only resume a continuation, and can't be cancelled, so
    // we can't hand it the continuation that the sleeping task is waiting on:
    // if the task were cancelled, the timer would later resume it a second
    // time.  Instead, a helper task waits for the timer, and whichever of the
    // helper and the cancellation handler gets to the claim first resumes the
    // sleeping task.
    let claim = _SleepClaim()
    try await withContinuation(
      of: Void.self, throwing: _Concurrency.CancellationError.self
    ) { (continuation: consuming Continuation<Void, _Concurrency.CancellationError>,
         waiter: consuming ContinuationWaiter<Void, _Concurrency.CancellationError>)
        async throws(_Concurrency.CancellationError) -> Void in
      let sleeper = unsafe UnsafeContinuation<Void, _Concurrency.CancellationError>(
        continuation._takeContext())

      _ = Task<Void, Never>.detached(priority: Task.currentPriority) {
        await Task<Never, Never>._waitForTimer(
          clockID: clockID.rawValue,
          seconds: seconds, nanoseconds: nanoseconds,
          toleranceSeconds: toleranceSeconds,
          toleranceNanoseconds: toleranceNanoseconds)
        if claim.claim() {
          unsafe sleeper.resume()
        }
      }

      try await waiter.wait(onCancel: { _ in
        if claim.claim() {
          unsafe sleeper.resume(throwing: _Concurrency.CancellationError())
        }
      })
    }
    #else
    await _waitForTimer(clockID: clockID.rawValue,
                        seconds: seconds, nanoseconds: nanoseconds,
                        toleranceSeconds: toleranceSeconds,
                        toleranceNanoseconds: toleranceNanoseconds)
    try Task.checkCancellation()
    #endif
  }

  /// Wait until the timer fires, using the runtime's global enqueue functions.
  @available(StdlibDeploymentTarget 5.7, *)
  internal static func _waitForTimer(
    clockID: Int32,
    seconds: Int64, nanoseconds: Int64,
    toleranceSeconds: Int64, toleranceNanoseconds: Int64
  ) async {
    let _: () = await Builtin.withUnsafeContinuation {
      (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(
        priority: Int(Task.currentPriority.rawValue),
        continuation: continuation)

      _enqueueJobGlobalWithDeadline(
        seconds, nanoseconds,
        toleranceSeconds, toleranceNanoseconds,
        clockID, UnownedJob(context: job))
    }
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
