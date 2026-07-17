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
///     potentially a ``CancellationError`` caused by cancellation caused by the expired deadline.
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
///     are cancelled automatically when the deadline is exceeded.
/// - `withTaskCancellationHandler` handlers created within `operation` are triggered es expected.
/// - The enclosing task's `Task.isCancelled` is unaffected.
///
/// When a deadline expires, semantically the scope of the task which is running the `operation`
/// becomes cancelled. This is observable using `Task.isCancelled` and similar APIs, and has
/// the usual effect on child tasks an task cancellation handlers.
///
/// Even though this a deadline's expiry cancels the operation scope, the `withDeadline` block still
/// will await for the operation to complete. This is consistent with Swift's approach to cooperative
/// cancellation and structured concurrency. It does mean however that operation code must be checking
/// for cancellation if it wants to react and return "early".
///
/// The `withDeadline` function may return after the deadline has expired, as there is no guarantee on
/// interrupting the operation's execution. Similarily, even if the deadline is set in the past, the
/// operation will still always execute - and it is up to the operation (or any of its parts, or child tasks)
/// to check e.g. `Task.isCancelled` if it should proceed with its computation or not.
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
///   - tolerance: The tolerance used for the sleep.
///   - clock: The clock to use for measuring time. Defaults to ``ContinuousClock``.
///   - operation: The asynchronous operation to complete before the deadline.
///
/// - Returns: The result of the operation if it completes successfully before or after the deadline expires.
/// - Throws: The error thrown by the operation.
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
  // Fast path: if an outer deadline exists for the same clock
  if let outer = _findNearestDeadline(clock: clock), outer <= expiration {
    return try await operation()
  }

  // Push a deadline record on the current task.
  // We need to borrow the clock and expiration, because we don't know the exact types.
  // The record will initializeWithCopy them into the record's tail allocated storage,
  // and destroy them when the record is popped.
  //
  // FIXME: The shape of this builtin is a workaround,
  //        since stack nesting SIL checking couldn't handle a generic builtin
  //        that would take clock and expiration directly; Something to improve for sure.
  let deadlineRecord = unsafe Builtin.taskPushDeadline(
    clockPtr: Builtin.addressOfBorrow(clock),
    instantPtr: Builtin.addressOfBorrow(expiration),
    clockType: C.self,
    instantType: C.Instant.self)
  defer { withExtendedLifetime(clock) {} }
  defer { withExtendedLifetime(expiration) {} }
  defer { unsafe Builtin.taskPopDeadline(record: deadlineRecord) }

  return try await __withTaskCancellationScope { scope throws(Failure) in
    // Scope handle is ~Escapable; but we're playing lose here until we get
    // the new Job() enqueue at time...
    let scopeRecord = unsafe scope._record

    // TODO: Replace this by picking the "Clock's executor"
    // TODO: Instead of creating a full task here, we want to enqueue a job
    //       at a deadline that cancels the scope; disarming should attempt
    //       to cancel the job. I.e. this wants to be:
    //          let registration = clockExecutor.enqueue(Job({ scope.cancel(reason: .deadlineExceeded) })
    let timer = Task.detached {
      do {
        try await clock.sleep(until: expiration, tolerance: tolerance)
      } catch {
        // Timer was cancelled (disarmed) before the deadline elapsed.
        return
      }
      // Deadline elapsed; cancel the scope with the `deadlineExpired` reason
      // so `Task.checkCancellation()` etc. throw a `CancellationError` whose
      // `reason` reports the deadline expiration instead of `.unspecified`.
      unsafe _taskCancelTaskCancellationScopeWithReason(
        record: scopeRecord,
        reason: UInt(CancellationError.Reason.deadlineExpired.rawValue))
    }
    defer { timer.cancel() }
    return try await operation()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: withDeadline(in:)

/// Executes an operation with the expectation it completes within the given
/// relative timeout, measured against `clock.now` at the point of call.
///
/// This is a shorthand for the instant-based `withDeadline` that constructs
/// the deadline as `clock.now.advanced(by: timeout)` and forwards to the
/// primary entry point; all deadline composition rules (minimum-expiration
/// nesting, subsumption per clock identity) apply exactly as they do there.
///
/// ```swift
/// let value = try await withDeadline(in: .seconds(5)) {
///     try await fetchDataFromServer()
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
///     potentially a ``CancellationError`` caused by cancellation caused by the expired deadline.
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
///     are cancelled automatically when the deadline is exceeded.
/// - `withTaskCancellationHandler` handlers created within `operation` are triggered es expected.
/// - The enclosing task's `Task.isCancelled` is unaffected.
///
/// When a deadline expires, semantically the scope of the task which is running the `operation`
/// becomes cancelled. This is observable using `Task.isCancelled` and similar APIs, and has
/// the usual effect on child tasks an task cancellation handlers.
///
/// Even though this a deadline's expiry cancels the operation scope, the `withDeadline` block still
/// will await for the operation to complete. This is consistent with Swift's approach to cooperative
/// cancellation and structured concurrency. It does mean however that operation code must be checking
/// for cancellation if it wants to react and return "early".
///
/// The `withDeadline` function may return after the deadline has expired, as there is no guarantee on
/// interrupting the operation's execution. Similarily, even if the deadline is set in the past, the
/// operation will still always execute - and it is up to the operation (or any of its parts, or child tasks)
/// to check e.g. `Task.isCancelled` if it should proceed with its computation or not.
///
/// - Parameters:
///   - timeout: The duration, relative to `clock.now`, by which the
///     operation must complete.
///   - tolerance: The tolerance used for the sleep.
///   - clock: The clock to use for measuring time. Defaults to ``ContinuousClock``.
///   - operation: The asynchronous operation to complete before the deadline.
///
/// - Returns: The result of the operation if it completes successfully before or after the deadline expires.
/// - Throws: The error thrown by the operation.
@available(StdlibDeploymentTarget 6.5, *)
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

/// Executes an operation with the expectation it completes within the given
/// relative timeout, measured against `ContinuousClock().now` at the point
/// of call.
///
/// This concrete overload disambiguates call sites like
/// `withDeadline(in: .seconds(5)) { ... }`: because `Swift.Duration` is the
/// `Instant.Duration` of more than one built-in clock, the generic overload
/// alone cannot infer `C` from `timeout` when the `clock:` argument is
/// defaulted.
///
/// ## Behavior
///
/// The function exhibits the following behavior based on deadline and operation completion:
///
/// - If the operation completes successfully before deadline: Returns the operation result.
/// - If the operation throws an error before deadline: Throws the operation error.
/// - If deadline expires and operation completes successfully: Returns the operation result.
/// - If deadline expires and operation throws an error: Throws the operation error,
///     potentially a ``CancellationError`` caused by cancellation caused by the expired deadline.
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
///     are cancelled automatically when the deadline is exceeded.
/// - `withTaskCancellationHandler` handlers created within `operation` are triggered es expected.
/// - The enclosing task's `Task.isCancelled` is unaffected.
///
/// When a deadline expires, semantically the scope of the task which is running the `operation`
/// becomes cancelled. This is observable using `Task.isCancelled` and similar APIs, and has
/// the usual effect on child tasks an task cancellation handlers.
///
/// Even though this a deadline's expiry cancels the operation scope, the `withDeadline` block still
/// will await for the operation to complete. This is consistent with Swift's approach to cooperative
/// cancellation and structured concurrency. It does mean however that operation code must be checking
/// for cancellation if it wants to react and return "early".
///
/// The `withDeadline` function may return after the deadline has expired, as there is no guarantee on
/// interrupting the operation's execution. Similarily, even if the deadline is set in the past, the
/// operation will still always execute - and it is up to the operation (or any of its parts, or child tasks)
/// to check e.g. `Task.isCancelled` if it should proceed with its computation or not.
///
/// - Parameters:
///   - timeout: The duration, relative to `ContinuousClock().now`, by which
///     the operation must complete.
///   - tolerance: The tolerance used for the sleep.
///   - operation: The asynchronous operation to complete before the deadline.
///
/// - Returns: The result of the operation if it completes successfully before or after the deadline expires.
/// - Throws: The error thrown by the operation.
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending)
func withDeadline<Return, Failure>(
  in timeout: ContinuousClock.Instant.Duration,
  tolerance: ContinuousClock.Instant.Duration? = nil,
  operation: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(Failure) -> Return
  where Return: ~Copyable,
        Failure: Error {
  let clock = ContinuousClock()
  return try await withDeadline(
    clock.now.advanced(by: timeout),
    tolerance: tolerance,
    clock: clock,
    operation: operation
  )
}

// ==== -----------------------------------------------------------------------
// MARK: Task.hasActiveDeadline

@available(StdlibDeploymentTarget 6.5, *)
extension Task where Success == Never, Failure == Never {
  /// Whether any deadline is set on the current task.
  ///
  /// Returns `true` when the current task is executing inside at least one
  /// `withDeadline` scope (for any clock), and `false` otherwise. This is
  /// cheap - it only reads the task's status flags and does not walk the
  /// record chain.
  ///
  /// External systems that only need to know "does an outer deadline
  /// govern our behavior" can use this without knowing which specific
  /// clock is in play. To read the actual deadline value use
  /// ``activeDeadline(for:)``.
  ///
  /// - SeeAlso: ``activeDeadline(for:)``
  public static var hasActiveDeadline: Bool {
    _swift_task_hasActiveDeadline()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: activeDeadline(for:)

@available(StdlibDeploymentTarget 6.5, *)
extension Task where Success == Never, Failure == Never {
  /// Find the nearest deadline given the specified clock.
  ///
  /// The returned instant is the earliest deadline whose clock identity
  /// (`clock.id`) matches the argument's - nested `withDeadline` scopes
  /// on the same clock are coalesced to the nearest one.
  ///
  /// Deadlines installed for a *different* clock are ignored (there is no
  /// meaningful cross-clock conversion, so no attempt is made to unify
  /// them). Composing multiple `withDeadline` scopes on different clocks
  /// still works correctly - the nearest deadline for each clock governs
  /// independently - but this accessor can only report on one clock at a
  /// time.
  ///
  /// - SeeAlso: ``hasActiveDeadline``
  public static func activeDeadline<C: Clock & Identifiable>(for clock: C) -> C.Instant? {
    // No need to short-circut here with hasDeadline as the `find...` already does so.
    _findNearestDeadline(clock: clock)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Internals

/// Swift-side helper called by the runtime for each deadline record whose
/// `ClockType` metadata pointer-equals the caller's `C`. Reads the record's
/// inline-stored clock as a `C` value and compares identities via `Identifiable`.
///
/// **Precondition (upheld by the runtime):** the outer walker in
/// `swift_task_findNearestDeadlineForClockImpl` calls this bridge only after
/// pointer-equality checking `record->ClockType == C-metadata`. The trailing
/// storage at `recordClockStorage` was initialized on push via
/// `clockType->vw_initializeWithCopy(..., incomingClock: C)`, so the bytes
/// there are a valid `C`. No dynamic downcast is needed - there is no class
/// instance to downcast; the bytes ARE the value. This matches how
/// `TaskLocal` loads back a stored task-local: identity of the key/type
/// gates the load, no runtime type check is performed.
///
/// C++-side ABI signature (see TaskStatus.cpp):
///
///     extern "C" SWIFT_CC(swift)
///     bool _task_deadline_recordHasSameClock(
///         OpaqueValue *recordClockStorage,
///         OpaqueValue *queryClock,
///         const Metadata *clockType,
///         const WitnessTable *clockWT,
///         const WitnessTable *identifiableWT);
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_task_deadline_recordHasSameClock")
internal func _task_deadline_recordHasSameClock<C: Clock & Identifiable>(
  recordClockStorage: UnsafeMutableRawPointer,
  queryClock: C
) -> Bool {
  // TODO: This should be simplified to be between two Identifiable things. No need to make it clock specific.
  //       AND it should be well typed on both sides, some I, some I, should be fine;
  // TODO: I guess that's why we need to store Metadata and WT in the record, to implement this callout well typed
  let stored = unsafe recordClockStorage
    .assumingMemoryBound(to: C.self).pointee
  return stored.id == queryClock.id
}

/// Query the innermost active deadline installed on the current task for
/// the given clock, or nil if none.
///
/// Intended for the `withDeadline` fast-path and internal tests /
/// diagnostics. Exposed as `@_spi(Concurrency)` so runtime tests can
/// query the active deadline; not part of the general public API.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
public func _findNearestDeadline<C: Clock & Identifiable>(clock: C) -> C.Instant? {
  // Direct-call into the runtime via `@_silgen_name`. Swift's standard
  // generic ABI passes:
  //   - `queryClock`  as `@in_guaranteed C`  (indirect, +0)
  //   - `C`'s type metadata
  //   - `C: Clock` witness table
  //   - `C: Identifiable` witness table
  // which is exactly what `swift_task_findNearestDeadlineForClock`'s
  // C++ signature expects. The WTs reach the Swift bridge
  // `_task_deadline_recordHasSameClock` so it can compare `.id`s.
  //
  // Result: a borrowed +0 pointer into the matched record's tail storage
  // aligned at `C.Instant`, or null.
  guard let matched =
      unsafe _swift_task_findNearestDeadlineForClock(queryClock: clock) else {
    return nil
  }

  // Copy the instant out of the record's storage; the record continues
  // to own it (a subsequent pop will run vw_destroy on it).
  return unsafe UnsafeRawPointer(matched)
    .assumingMemoryBound(to: C.Instant.self).pointee
}

/// Runtime shim declared purely for Swift's generic ABI: the compiler
/// synthesizes the (value, metadata, Clock WT, Identifiable WT) argument
/// tuple and calls straight into the C++ runtime symbol.
///
/// C++-side signature (see Runtime/Concurrency.h and TaskStatus.cpp):
///
///     OpaqueValue *
///     swift_task_findNearestDeadlineForClock(
///         OpaqueValue *queryClock,
///         const Metadata *clockType,
///         const WitnessTable *clockWT,
///         const WitnessTable *identifiableWT);
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_task_findNearestDeadlineForClock")
internal func _swift_task_findNearestDeadlineForClock<C: Clock & Identifiable>(
  queryClock: C
) -> UnsafeMutableRawPointer?


@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_swift_task_hasActiveDeadline")
internal func _swift_task_hasActiveDeadline() -> Bool

#endif // !$Embedded
