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

  /// Run the given job on an unspecified executor at some point
  /// after the given instant.
  ///
  /// Parameters:
  ///
  /// - job:         The job we wish to run
  /// - at instant:  The time at which we would like it to run.
  /// - tolerance:   The ideal maximum delay we are willing to tolerate.
  ///
  @available(StdlibDeploymentTarget 6.2, *)
  func run(_ job: consuming ExecutorJob,
           at instant: Instant, tolerance: Duration?)

  /// Enqueue the given job on the specified executor at some point after the
  /// given instant.
  ///
  /// The default implementation uses the `run` method to trigger a job that
  /// does `executor.enqueue(job)`.  If a particular `Clock` knows that the
  /// executor it has been asked to use is the same one that it will run jobs
  /// on, it can short-circuit this behaviour and directly use `run` with
  /// the original job.
  ///
  /// Parameters:
  ///
  /// - job:         The job we wish to run
  /// - on executor: The executor on which we would like it to run.
  /// - at instant:  The time at which we would like it to run.
  /// - tolerance:   The ideal maximum delay we are willing to tolerate.
  ///
  @available(StdlibDeploymentTarget 6.2, *)
  func enqueue(_ job: consuming ExecutorJob,
               on executor: some Executor,
               at instant: Instant, tolerance: Duration?)
#endif
}

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extension Clock {
  // The default implementation works by creating a trampoline and calling
  // the run() method.
  @available(StdlibDeploymentTarget 6.2, *)
  public func enqueue(_ job: consuming ExecutorJob,
                      on executor: some Executor,
                      at instant: Instant, tolerance: Duration?) {
    let trampoline = job.createTrampoline(to: executor)
    run(trampoline, at: instant, tolerance: tolerance)
  }

  // Clocks that do not implement run will fatalError() if you try to use
  // them with an executor that does not understand them.
  @available(StdlibDeploymentTarget 6.2, *)
  public func run(_ job: consuming ExecutorJob,
                  at instant: Instant, tolerance: Duration?) {
    fatalError("\(Self.self) does not implement run(_:at:tolerance:).")
  }
}
#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

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
  @_alwaysEmitIntoClient
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
  @_alwaysEmitIntoClient
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

@available(StdlibDeploymentTarget 6.2, *)
@_silgen_name("swift_sleep")
internal func _sleep(
  seconds: Int64,
  nanoseconds: Int64)
