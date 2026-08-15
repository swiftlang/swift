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

#if !$Embedded

import Swift

// ==== -----------------------------------------------------------------------
// MARK: withDeadline

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

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
/// - If deadline expires and operation throws an error: Throws the operation error,
///     potentially a ``CancellationError`` if the operation throws it as a result of the
///     deadline-triggered cancellation.
///
/// When the deadline expires `Task.isCancelled` returns true for the duration of `operation`.
/// This cancellation does not affect the "outer" task in which the deadline operation was started:
///
/// ```
/// try await withDeadline(in: .seconds(2)) {
///   while !Task.isCancelled {
///     try? await Task.sleep(for: .seconds(1))
///   }
///   assert(Task.isCancelled == true)
/// }
/// assert(Task.isCancelled == false) // the outer task is unaffected
/// ```
///
/// This means:
///
/// - `Task.isCancelled` observed inside `operation` returns `true` after the deadline is exceeded.
/// - Child tasks, created using `async let`, or task groups, inside `operation`
///     are cancelled automatically when the deadline is exceeded. This is not
///     optional: such children can never escape `operation`'s scope (it cannot
///     return until they've completed), so they are cancelled along with it -
///     whether they were already running when the deadline expired or are
///     spawned afterwards.
/// - `withTaskCancellationHandler` handlers created within `operation` are triggered as expected.
/// - The enclosing task's `Task.isCancelled` is unaffected.
///
/// When a deadline expires, semantically the scope of the task which is running the `operation`
/// becomes cancelled. This is observable using `Task.isCancelled` and similar APIs, and has
/// the usual effect on child tasks and task cancellation handlers created inside `operation`.
///
/// Task deadlines are a structured concurrency mechanism, and even though a deadline's expiry cancels
/// the operation scope, the `withDeadline` closure will await for the operation to complete.
/// The operation code must cooperatively be checking for cancellation, or make use of cancellation handlers,
/// if it wants to react and return "early".
///
/// The `withDeadline` function may return after the deadline has expired.
/// Similarily, even if the deadline is set in the past, the operation will still always execute -
/// and it is up to the operation (or any of its parts, or child tasks) to determine if it should
/// proceed with its computation or not.
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
/// - Parameters:
///   - expiration: The instant by which the operation must complete.
///   - tolerance: The tolerance used for the sleep. Defaults to `nil`.
///   - clock: The clock to use for measuring time. Defaults to ``ContinuousClock``.
///   - operation: The asynchronous operation to complete before the deadline.
///
/// - Returns: The result of the operation if it completes successfully before or after the deadline expires.
/// - Throws: The error thrown by the operation.
/// - SeeAlso: ``withDeadline(in:tolerance:clock:operation:)``
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending) func withDeadline<Return, Failure, C>(
  _ expiration: C.Instant,
  tolerance: C.Instant.Duration? = nil,
  clock: C = ContinuousClock(),
  operation: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(Failure) -> Return
  where Return: ~Copyable,
        Failure: Error,
        C: Clock & Identifiable {
#if $BuiltinTaskDeadline
  // Fast path: if an outer deadline exists for the same clock
  if let outer = _findNearestDeadline(clock: clock), outer <= expiration {
    return try await operation()
  }

  // Push a deadline record on the current task.
  let record = unsafe Builtin.taskPushDeadline(clock: clock, instant: expiration)
  defer { unsafe Builtin.taskPopDeadline(record: record) }

  return try await __withTaskCancellationScope { scope throws(Failure) in
    // `scope` is ~Escapable/~Copyable and cannot be captured by the
    // escaping `Task.detached` closure below, so we capture its
    // (Copyable, Escapable) raw record pointer instead and reconstruct
    // a transient `TaskCancellationScope` from it at the point of use.
    let scopeRecord = unsafe scope._record

    // If the deadline is already in the past, cancel the scope synchronously
    // so the operation observes `Task.isCancelled == true` at entry instead
    // of racing against a detached timer that would need to run first.
    guard expiration > clock.now else {
      scope.cancel(reason: .deadlineExpired)
      return try await operation()
    }

    // TODO: Replace this by picking the "Clock's executor"
    // TODO: Instead of creating a full task here, we want to enqueue a job
    //       at a deadline that cancels the scope; disarming should attempt
    //       to cancel the job. I.e. this wants to be:
    //          let registration = clockExecutor.enqueue(Job({ scope.cancel(reason: .deadlineExpired) })
    let timer = Task.detached {
      do {
        try await clock.sleep(until: expiration, tolerance: tolerance)
      } catch {
        // Timer was cancelled (disarmed) before the deadline elapsed.
        return
      }
      // Deadline exceeded; cancel the scope with the `deadlineExpired` reason
      // so `Task.checkCancellation()` etc. throw a `CancellationError` whose
      // `reason` reports the deadline expiration instead of `.unspecified`.
      unsafe TaskCancellationScope(record: scopeRecord).cancel(reason: .deadlineExpired)
    }
    defer { timer.cancel() }
    return try await operation()
  }
#else
  fatalError("Swift compiler is incompatible with this SDK version")
#endif
}

// ==== -----------------------------------------------------------------------
// MARK: withDeadline(in:)

/// Executes an operation with the expectation it completes within the given
/// relative timeout, measured against `clock.now` at the point of call.
///
/// This is a shorthand for the instant-based `withDeadline` that constructs
/// the deadline as `clock.now.advanced(by: timeout)`.
///
/// ```swift
/// let value = try await withDeadline(in: .seconds(5)) {
///     try await fetchDataFromServer()
/// }
/// ```
///
/// See ``withDeadline(_:tolerance:clock:operation:)`` for full behavior.
///
/// - Parameters:
///   - timeout: The duration, relative to `clock.now`, by which the
///     operation must complete.
///   - tolerance: The tolerance used for the sleep. Defaults to `nil`.
///   - clock: The clock to use for measuring time. Defaults to ``ContinuousClock``.
///   - operation: The asynchronous operation to complete before the deadline.
///
/// - Returns: The result of the operation.
/// - Throws: The error thrown by the operation.
/// - SeeAlso: ``withDeadline(_:tolerance:clock:operation:)``
@available(StdlibDeploymentTarget 6.5, *)
@export(implementation)
public nonisolated(nonsending) func withDeadline<Return, Failure, C>(
  in timeout: C.Instant.Duration,
  tolerance: C.Instant.Duration? = nil,
  clock: C = ContinuousClock(),
  operation: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(Failure) -> Return
  where Return: ~Copyable,
        Failure: Error,
        C: Clock & Identifiable {
  return try await withDeadline(
    clock.now.advanced(by: timeout),
    tolerance: tolerance,
    clock: clock,
    operation: operation
  )
}
#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

// ==== -----------------------------------------------------------------------
// MARK: Task.hasActiveDeadline

@available(StdlibDeploymentTarget 6.5, *)
extension Task where Success == Never, Failure == Never {
  /// A Boolean value indicating whether a deadline is currently installed on
  /// the current task.
  ///
  /// Returns `true` when the current task is executing inside at least one
  /// `withDeadline` scope (for any clock), and `false` otherwise.
  ///
  /// This operation is cheaper than obtaining a deadline instant by calling
  /// `activeDeadline(for:)` which needs to perform deadline lookups and clock comparisons.
  ///
  /// - Returns: `true` if any `withDeadline` scope is active on the
  ///   current task, `false` otherwise. Also returns `false` when
  ///   called outside of any task context.
  ///
  /// - SeeAlso: ``activeDeadline(for:)``
  @available(StdlibDeploymentTarget 6.5, *)
  @export(implementation)
  public static var hasActiveDeadline: Bool {
    _swift_task_hasActiveDeadline()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: activeDeadline(for:)

@available(StdlibDeploymentTarget 6.5, *)
extension Task where Success == Never, Failure == Never {
  /// Find the nearest active deadline installed on the current task for the
  /// specified clock.
  ///
  /// The returned instant is the earliest deadline whose clock identity
  /// (`clock.id`) matches the argument's - nested `withDeadline` scopes
  /// on the same clock are coalesced, and the nearest one is returned.
  ///
  /// Active deadlines for a *different* clock are ignored.
  ///
  /// Composing multiple `withDeadline` scopes on different clocks
  /// is supported, however their lookups are idependent;
  /// this accessor can only report one deadline for a specific clock at a time.
  ///
  /// - Parameter clock: The clock whose deadlines to search for. Clock
  ///   identity is compared via ``Identifiable/id``; deadlines installed
  ///   with any other clock value are ignored, even if they share the
  ///   same `C.Instant` type.
  ///
  /// - Returns: The nearest `C.Instant` deadline installed for `clock` on
  ///   the current task (or any of its parents, walking outward), or
  ///   `nil` if no `withDeadline` scope for `clock` is active. Returns
  ///   `nil` when called outside of any task context.
  ///
  /// - SeeAlso: ``hasActiveDeadline``
  @available(StdlibDeploymentTarget 6.5, *)
  public static func activeDeadline<C: Clock & Identifiable>(for clock: C = ContinuousClock()) -> C.Instant? {
    // No need to short-circut here with hasDeadline as the `find...` already does so.
    _findNearestDeadline(clock: clock)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Internals

/// Swift-side helper called by the runtime for each deadline record whose
/// `ClockType` metadata pointer-equals the caller's `I`.
///
/// The runtime pointer-equality checks `record->ClockType == I-metadata`
/// before calling this bridge, so `recordClockPointer` is guaranteed to
/// hold a valid `I`. Only `Identifiable` is needed here - the clock-ness
/// of the type doesn't enter the comparison.
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_task_isEqualIdentifiableID")
internal func _task_isEqualIdentifiableID<I: Identifiable>(
  recordClockPointer: UnsafeMutableRawPointer,
  queryClock: I
) -> Bool {
  let stored = unsafe recordClockPointer
    .assumingMemoryBound(to: I.self).pointee
  return stored.id == queryClock.id
}

/// Find the innermost active deadline installed on the current task for
/// the given clock, or nil if none.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
public func _findNearestDeadline<C: Clock & Identifiable>(clock: C) -> C.Instant? {
  guard let matched =
      unsafe _swift_task_findNearestDeadlineForClock(queryClock: clock) else {
    return nil
  }

  // The runtime returns a borrowed +0 pointer into the record's instant
  // slot, which itself points into the installing `withDeadline`'s async
  // frame (structured concurrency keeps that frame live). Load through
  // `assumingMemoryBound` + `.pointee`, which lowers to
  // `initializeWithCopy` on `C.Instant` and produces an owned +1.
  return unsafe UnsafeRawPointer(matched)
    .assumingMemoryBound(to: C.Instant.self).pointee
}

@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_task_findNearestDeadlineForClock")
internal func _swift_task_findNearestDeadlineForClock<C: Clock & Identifiable>(
  queryClock: C
) -> UnsafeMutableRawPointer?


@usableFromInline
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_swift_task_hasActiveDeadline")
internal func _swift_task_hasActiveDeadline() -> Bool

#endif // !$Embedded
