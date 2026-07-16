//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// ==== -----------------------------------------------------------------------
// MARK: withDeadline

/// Executes an operation with the expectation it completes within a specified deadline.
///
/// Use this function to limit the execution time of an asynchronous operation to a specific instant.
/// If the operation completes before the deadline expires, this function returns the result. If the
/// deadline expires first, this function cancels the operation. The `withDeadline` function will
/// return or throw according to how the operation returns or throws as a response to the cancellation.
///
/// The following example demonstrates using a deadline to limit a network request:
///
/// ```swift
/// let clock = ContinuousClock()
/// let deadline = clock.now.advanced(by: .seconds(5))
/// do {
///     let result = try await withDeadline(deadline, clock: clock) {
///         try await fetchDataFromServer()
///     }
///     print("Data received: \(result)")
/// } catch {
///     print("Operation failed")
/// }
/// ```
///
/// ## Behavior
///
/// The function exhibits the following behavior based on deadline and operation completion:
///
/// - If the operation completes successfully before deadline: Returns the operation result.
/// - If the operation throws an error before deadline: Throws the operation error.
/// - If deadline expires and operation completes successfully: Returns the operation result.
/// - If deadline expires and operation throws an error: Throws the operation error.
///
/// ## Cancellation model
///
/// When the deadline expires, `withDeadline` cancels a private cancellation scope
/// wrapping `operation`, not the enclosing task. This means:
///
/// - `Task.isCancelled` observed inside `operation` returns `true` after the
///   deadline fires.
/// - `withTaskCancellationHandler` handlers registered inside `operation`
///   fire (so `Task.sleep`, etc. wake up promptly).
/// - The enclosing task's `Task.isCancelled` is unaffected.
/// - Structured children spawned inside `operation` are not auto-cancelled
///   by the deadline; they observe the scope only through the same
///   local-check mechanism.
///
/// ## Coordinating multiple operations
///
/// Use `withDeadline` when coordinating multiple operations to complete by the same instant:
///
/// ```swift
/// let clock = ContinuousClock()
/// let deadline = clock.now.advanced(by: .seconds(10))
///
/// async let result1 = withDeadline(deadline, clock: clock) {
///     try await fetchUserData()
/// }
/// async let result2 = withDeadline(deadline) {
///     try await fetchPreferences()
/// }
///
/// let (user, prefs) = try await (result1, result2)
/// ```
///
/// This ensures both operations share the same absolute deadline, avoiding duration drift that can occur
/// when timeouts are passed through multiple call layers.
///
/// - Important: This function cancels the operation when the deadline expires, but waits for the
/// operation to return. The function may run longer than the time until the deadline if the operation
/// doesn't respond to cancellation immediately. As usual with tasks in Swift concurrency, operations
/// must cooperatively check for cancellation if they aim to return "early" when cancelled.
///
/// - Parameters:
///   - expiration: The instant by which the operation must complete.
///   - tolerance: The tolerance used for the sleep.
///   - clock: The clock to use for measuring time.
///   - operation: The asynchronous operation to complete before the deadline.
///
/// - Returns: The result of the operation if it completes successfully before or after the deadline expires.
/// - Throws: The error thrown by the operation.
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending)
func withDeadline<Return, Failure, C>(
  _ expiration: C.Instant,
  tolerance: C.Instant.Duration? = nil,
  clock: C = ContinuousClock(),
  operation: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(Failure) -> Return
where Return: ~Copyable, Failure: Error, C: Clock & Identifiable {
  // Push a deadline status record so `Task.hasActiveDeadline` / observers
  // can see the innermost active deadline. Subsumption (a tighter enclosing
  // deadline for the same clock) is handled by the runtime, which returns
  // a nil record in that case.
  let clockID = _encodeClockID(clock)
  let (seconds, attoseconds) = _instantComponents(expiration, clock: clock)
  let deadlineRecord = unsafe Builtin.taskPushDeadline(
    clockID: clockID,
    deadlineSeconds: seconds,
    deadlineAttoseconds: attoseconds)
  defer { unsafe Builtin.taskPopDeadline(record: deadlineRecord) }

  // Try to find an executor that services this clock's timers. If we can't
  // (unknown clock, or the runtime doesn't have a clock executor for it),
  // fall back to running the operation without deadline enforcement -
  // observers can still see the deadline record and cooperatively check it.
  return try await __withTaskCancellationScope { scope throws(Failure) in
    guard let disarm = _armDeadlineTimer(
      scope: scope,
      expiration: expiration,
      tolerance: tolerance,
      clock: clock
    ) else {
      return try await operation()
    }
    defer { disarm() }
    return try await operation()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Timer arming

/// Enqueue a fire-once synchronous job on the clock's current executor that
/// cancels `scope` when the deadline expires. Returns a closure that
/// synchronously disarms the timer (a no-op if it has already fired).
///
/// Returns `nil` if no executor for this clock exists in the current
/// preference chain - the caller should then run the operation without
/// deadline enforcement.
@available(StdlibDeploymentTarget 6.5, *)
private func _armDeadlineTimer<C: Clock & Identifiable>(
  scope: borrowing TaskCancellationScope,
  expiration: C.Instant,
  tolerance: C.Instant.Duration?,
  clock: C
) -> (@Sendable () -> Void)? {
  // Building the timer job doesn't require any generic-over-C witness -
  // it just needs to call `scope.cancel()` when it fires. But we do need
  // an executor typed by `C` to actually enqueue with a `C.Instant`
  // deadline; dispatch on the two system clocks that have known executor
  // protocols today.
  let priority = UInt8(Task.currentPriority.rawValue)
  // Copy the raw scope pointer into an unsafe box so the sending closure
  // doesn't need to move `scope` itself (it's ~Copyable / ~Escapable).
  let scopeRecord = unsafe scope._record
  let timerJob = Builtin.createSynchronousJob(priority: priority) {
    unsafe _taskCancelTaskCancellationScope(record: scopeRecord)
  }

  if C.self == ContinuousClock.self {
    guard let executor = Task.currentContinuousClockExecutor else { return nil }
    let instant = unsafe unsafeBitCast(expiration, to: ContinuousClock.Instant.self)
    let toleranceValue = tolerance.map { unsafe unsafeBitCast($0, to: ContinuousClock.Duration.self) }
    let registration = executor.enqueue(
      ExecutorJob(context: timerJob),
      at: instant,
      tolerance: toleranceValue
    )
    return { executor.cancel(registration) }
  }
  if C.self == SuspendingClock.self {
    guard let executor = Task.currentSuspendingClockExecutor else { return nil }
    let instant = unsafe unsafeBitCast(expiration, to: SuspendingClock.Instant.self)
    let toleranceValue = tolerance.map { unsafe unsafeBitCast($0, to: SuspendingClock.Duration.self) }
    let registration = executor.enqueue(
      ExecutorJob(context: timerJob),
      at: instant,
      tolerance: toleranceValue
    )
    return { executor.cancel(registration) }
  }

  // Unknown clock - drop the timer job (its retain will be released when
  // this scope exits without enqueue happening; without a scheduler owning
  // it, the SynchronousJob leaks its context. That's acceptable while this
  // primitive is SPI - custom-clock users must plumb through
  // Continuous/SuspendingClock for now).
  return nil
}

// ==== -----------------------------------------------------------------------
// MARK: Clock ID encoding

/// Map a `Clock & Identifiable` to the 64-bit clock identifier the deadline
/// status record uses to determine subsumption between nested deadlines on
/// the same clock. For the built-in clocks this reuses the stable
/// `SystemClockID` raw values; for other clocks it falls back to the
/// clock's own `id` hash.
@available(StdlibDeploymentTarget 6.5, *)
@inline(__always)
private func _encodeClockID<C: Clock & Identifiable>(_ clock: C) -> UInt64 {
  if let known = clock.id as? SystemClockID {
    return UInt64(known.rawValue)
  }
  // For custom clocks, hash to a stable-per-process UInt64 that is
  // guaranteed distinct from `SystemClockID` values (which are small
  // positive integers).
  var hasher = Hasher()
  hasher.combine(clock.id)
  return UInt64(bitPattern: Int64(hasher.finalize()))
}

// ==== -----------------------------------------------------------------------
// MARK: Instant decomposition

/// Break a `C.Instant` into its two-component `Swift.Duration` representation
/// (`(seconds, attoseconds)`) relative to the clock's reference point, as
/// required by `Builtin.taskPushDeadline`. For the built-in clocks whose
/// `Instant` wraps a `Swift.Duration` we go through their `_value` accessor
/// directly; for custom clocks we approximate using
/// `clock.now.duration(to: expiration)`.
@available(StdlibDeploymentTarget 6.5, *)
@inline(__always)
private func _instantComponents<C: Clock & Identifiable>(
  _ expiration: C.Instant,
  clock: C
) -> (seconds: Int64, attoseconds: Int64) {
  if C.self == ContinuousClock.self {
    let inst = unsafe unsafeBitCast(expiration, to: ContinuousClock.Instant.self)
    return inst._value.components
  }
  if C.self == SuspendingClock.self {
    let inst = unsafe unsafeBitCast(expiration, to: SuspendingClock.Instant.self)
    return inst._value.components
  }
  // Fallback: represent the deadline as the duration from `now` to
  // `expiration`. This loses the "absolute point in time" semantics but
  // still gives a stable ordering for subsumption comparisons.
  let dur = unsafe unsafeBitCast(clock.now.duration(to: expiration), to: Swift.Duration.self)
  return dur.components
}
