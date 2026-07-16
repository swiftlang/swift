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
  where Return: ~Copyable,
        Failure: Error,
        C: Clock & Identifiable {
  let clockID = _encodeClockID(clock)
  let (seconds, attoseconds) = _instantComponents(expiration, clock: clock)
  let deadlineRecord = unsafe Builtin.taskPushDeadline(
    clockID: clockID,
    deadlineSeconds: seconds,
    deadlineAttoseconds: attoseconds)
  defer { unsafe Builtin.taskPopDeadline(record: deadlineRecord) }

  // Arm a detached timer task on the given clock that will cancel the
  // scope wrapping `operation` once the deadline elapses. The scope is
  // local to `operation`, so `Task.isCancelled` observed by `operation`
  // (or by any `withTaskCancellationHandler` handler it installs) becomes
  // true, but the enclosing task's own cancellation state is unaffected.
  return try await __withTaskCancellationScope { scope throws(Failure) in
    let disarm = _armDeadlineTimer(
      scope: scope,
      expiration: expiration,
      tolerance: tolerance,
      clock: clock
    )
    defer { disarm() }
    return try await operation()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Timer arming

/// Spawn a detached task that sleeps until `expiration` on the given clock,
/// then cancels `scope`. Returns a closure that disarms the timer by
/// cancelling the task; the sleeping task observes cancellation and exits
/// without touching the scope.
///
/// This intentionally uses only stock building blocks (`Task { ... }` and
/// `clock.sleep(until:tolerance:)`) rather than reaching for a
/// clock-specific executor or a bespoke fire-once job: the deadline API is
/// primarily useful for operations that are already async and long-running,
/// so the extra Task allocation per `withDeadline` call is not a concern
/// while this is SPI. It can be revisited once we have a lighter-weight
/// timer primitive.
@available(StdlibDeploymentTarget 6.5, *)
private func _armDeadlineTimer<C: Clock & Identifiable>(
  scope: borrowing TaskCancellationScope,
  expiration: C.Instant,
  tolerance: C.Instant.Duration?,
  clock: C
) -> (@Sendable () -> Void) {
  // The scope handle itself is `~Escapable`, but its underlying record
  // pointer is a plain `UnsafeRawPointer` we can hand to the sending
  // closure. The scope's lifetime is the body of `__withTaskCancellationScope`
  // above us, and the returned disarm closure is called from `defer` inside
  // that body - so the record pointer is guaranteed live for the duration
  // of the timer task.
  let scopeRecord = unsafe scope._record

  let timer = Task.detached {
    do {
      try await clock.sleep(until: expiration, tolerance: tolerance)
    } catch {
      // Timer was cancelled (disarmed) before the deadline elapsed.
      return
    }
    unsafe _taskCancelTaskCancellationScope(record: scopeRecord)
  }
  return { timer.cancel() }
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
