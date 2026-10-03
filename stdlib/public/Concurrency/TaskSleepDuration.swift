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

#if $Embedded
// The witnesses for the Clock requirements that stand in for the dynamic
// casts below. Full specialization turns each of these into a direct call.
@available(StdlibDeploymentTarget 5.7, *)
extension ContinuousClock {
  @available(StdlibDeploymentTarget 5.7, *)
  public func _timestampComponents(for instant: Instant)
    -> (clockID: Int32, seconds: Int64, nanoseconds: Int64)? {
    let stamp = timestamp(for: instant)
    return (clockID: stamp.clockID.rawValue, seconds: stamp.seconds,
            nanoseconds: stamp.nanoseconds)
  }

  @available(StdlibDeploymentTarget 5.7, *)
  public func _durationComponents(for duration: Instant.Duration)
    -> (seconds: Int64, nanoseconds: Int64)? {
    durationComponents(for: duration)
  }
}

@available(StdlibDeploymentTarget 5.7, *)
extension SuspendingClock {
  @available(StdlibDeploymentTarget 5.7, *)
  public func _timestampComponents(for instant: Instant)
    -> (clockID: Int32, seconds: Int64, nanoseconds: Int64)? {
    let stamp = timestamp(for: instant)
    return (clockID: stamp.clockID.rawValue, seconds: stamp.seconds,
            nanoseconds: stamp.nanoseconds)
  }

  @available(StdlibDeploymentTarget 5.7, *)
  public func _durationComponents(for duration: Instant.Duration)
    -> (seconds: Int64, nanoseconds: Int64)? {
    durationComponents(for: duration)
  }
}
#endif

fileprivate func timestamp<C: Clock>(for instant: C.Instant, clock: C)
  -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64) {
  #if $Embedded
  if let components = clock._timestampComponents(for: instant),
     let clockID = _ClockID(rawValue: components.clockID) {
    return (clockID: clockID, seconds: components.seconds,
            nanoseconds: components.nanoseconds)
  }
  #else
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
  #if $Embedded
  if let components = clock._durationComponents(for: duration) {
    return components
  }
  #else
  if let continuousClock = clock as? ContinuousClock {
    return continuousClock.durationComponents(for: duration as! ContinuousClock.Duration)
  } else if let suspendingClock = clock as? SuspendingClock {
    return suspendingClock.durationComponents(for: duration as! SuspendingClock.Duration)
  }
  #endif
  fatalError("unknown clock in fallback path")
}

@available(StdlibDeploymentTarget 5.7, *)
extension Task where Success == Never, Failure == Never {
  @available(StdlibDeploymentTarget 5.7, *)
  @diagnose(UselessAvailabilityCheck, as: ignored)
  internal static func _sleep<C: Clock>(
    until instant: C.Instant,
    tolerance: C.Duration?,
    clock: C
  ) async throws {
    // Create a token which will initially have the value "not started", which
    // means the continuation has neither been created nor completed.
    let token = unsafe UnsafeSleepStateToken()

    do {
      // Install a cancellation handler to resume the continuation by
      // throwing CancellationError.
      try await withTaskCancellationHandler {
        let _: () = try unsafe await withUnsafeThrowingContinuation { continuation in
          while true {
            let state = unsafe token.load()
            switch unsafe state {
            case .notStarted:
              // Try to swap in the continuation word.
              let newState = unsafe SleepState.activeContinuation(continuation)
              if unsafe !token.exchange(expected: state, desired: newState) {
                // Keep trying!
                continue
              }

              // Create a task that resumes the continuation normally if it
              // finishes first. Enqueue it directly with the delay, so it fires
              // when we're done sleeping.
              let sleepTaskFlags = taskCreateFlags(
                priority: nil, isChildTask: false, copyTaskLocals: false,
                inheritContext: false, enqueueJob: false,
                addPendingGroupTaskUnconditionally: false,
                isDiscardingTask: false, isSynchronousStart: false)
              let (sleepTask, _) = Builtin.createAsyncTask(sleepTaskFlags) {
                unsafe onSleepWake(token)
              }

              let job = Builtin.convertTaskToJob(sleepTask)

              if #available(StdlibDeploymentTarget 6.3, *) {
                #if !$Embedded
                if let executor = Task.currentSchedulingExecutor {
                  executor.enqueue(ExecutorJob(context: job),
                                   at: instant,
                                   tolerance: tolerance,
                                   clock: clock)
                  return
                }
                #endif
              } else {
                fatalError("we shouldn't get here; if we have, availability is broken")
              }

              // If there is no current scheduling executor, fall back to
              // calling _enqueueJobGlobalWithDeadline().
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
              return

            case .activeContinuation, .finished:
              fatalError("Impossible to have multiple active continuations")

            case .cancelled:
              fatalError("Impossible to have cancelled before we began")

            case .cancelledBeforeStarted:
              // Finish the continuation normally. We'll throw later, after
              // we clean up.
              unsafe continuation.resume()
              return
          }
        }
        }
      } onCancel: {
        unsafe onSleepCancel(token)
      }

      // Determine whether we got cancelled before we even started.
      let cancelledBeforeStarted: Bool
      switch unsafe token.load() {
      case .notStarted, .activeContinuation, .cancelled:
        fatalError("Invalid state for non-cancelled sleep task")

      case .cancelledBeforeStarted:
        cancelledBeforeStarted = true

      case .finished:
        cancelledBeforeStarted = false
      }

      // We got here without being cancelled, so deallocate the storage for
      // the flag word and continuation.
      unsafe token.deallocate()

      // If we got cancelled before we even started, through the cancellation
      // error now.
      if cancelledBeforeStarted {
        throw _Concurrency.CancellationError()
      }
    } catch {
      // The task was cancelled; propagate the error. The "on wake" task is
      // responsible for deallocating the flag word and continuation, if it's
      // still running.
      throw error
    }
  }

  /// Suspends the current task until the given deadline within a tolerance.
  ///
  /// If the task is canceled before the time ends, this function throws
  /// `CancellationError`.
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
  /// `CancellationError`.
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
