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

  // Allocate a `_ClockBox<C>` on the Swift heap that carries both the
  // clock and the deadline instant. The runtime consumes the +1 on push
  // and releases it on pop.
  let box = _ClockBox<C>(clock: clock, deadline: expiration)
  let opaque = unsafe Unmanaged.passRetained(box).toOpaque()
  let native = unsafe Builtin.reinterpretCast(opaque) as Builtin.NativeObject
  let clockTypeRaw = unsafe Builtin.reinterpretCast(C.self) as Builtin.RawPointer
  let deadlineRecord = unsafe Builtin.taskPushDeadline(clockType: clockTypeRaw, box: native)
  defer { unsafe Builtin.taskPopDeadline(record: deadlineRecord) }

  return try await __withTaskCancellationScope { scope throws(Failure) in
    // The scope handle is `~Escapable`, but its underlying record pointer
    // is a plain `UnsafeRawPointer` we can hand to the sending timer
    // closure. The scope's lifetime is this operation, and the timer is
    // disarmed by `defer` before the operation returns, so the record
    // pointer is guaranteed live for the duration of the timer task.
    let scopeRecord = unsafe scope._record

    // TODO: Replace this by picking the "Clock's executor"
    // TODO: Instead of creating a full task here, we want to enqueue a job
    //       at a deadline that cancels the scope; disarming should attempt
    //       to cancel the job.
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
// MARK: Clock box

/// A non-generic base class carrying identity/compare methods that dispatch
/// via Swift's dynamic-dispatch table into the concrete `_ClockBox<C>`.
@available(StdlibDeploymentTarget 6.5, *)
internal class _AnyClockBox {
  init() {}

  /// Whether `other` refers to the same logical clock as `self`.
  ///
  /// The caller (runtime) has already checked that both records have the
  /// same `ClockType` metadata pointer, so both boxes are known to be
  /// `_ClockBox<C>` for the same `C`. Concrete `_ClockBox<C>` overrides
  /// this method with a properly-typed comparison of `clock.id`.
  func hasSameClock(as other: _AnyClockBox) -> Bool {
    // Base implementation reached only if someone instantiates a bare
    // `_AnyClockBox` (should never happen).
    return self === other
  }
}

/// A heap-allocated Swift class that stores both the clock instance and the
/// deadline instant. Retained by the `TaskDeadlineStatusRecord` on push and
/// released on pop.
@available(StdlibDeploymentTarget 6.5, *)
internal final class _ClockBox<C: Clock & Identifiable>: _AnyClockBox {
  let clock: C
  let deadline: C.Instant

  init(clock: C, deadline: C.Instant) {
    self.clock = clock
    self.deadline = deadline
    super.init()
  }

  override func hasSameClock(as other: _AnyClockBox) -> Bool {
    // The runtime guaranteed both records share ClockType metadata, so
    // the downcast succeeds. Use unchecked downcast for the common path.
    guard let typed = other as? _ClockBox<C> else { return false }
    return clock.id == typed.clock.id
  }
}

/// Bridged Swift-side helper called by the runtime for each candidate
/// record while walking the chain looking for a deadline for the query
/// clock. Both arguments are `_AnyClockBox` instances; the dispatch to
/// `hasSameClock(as:)` picks the right `_ClockBox<C>.hasSameClock` via
/// Swift's virtual dispatch, without any metadata plumbing on the C++
/// side.
///
/// C++-side ABI signature (see TaskStatus.cpp):
///
///     extern "C" SWIFT_CC(swift)
///     bool _task_deadline_boxesSameClock(HeapObject *a, HeapObject *b);
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_task_deadline_boxesSameClock")
internal func _task_deadline_boxesSameClock(
  _ a: AnyObject, _ b: AnyObject
) -> Bool {
  guard let boxA = a as? _AnyClockBox, let boxB = b as? _AnyClockBox
  else { return false }
  return boxA.hasSameClock(as: boxB)
}

// ==== -----------------------------------------------------------------------
// MARK: Nearest-deadline SPI

/// Query the innermost active deadline installed on the current task for
/// the given clock, or nil if none.
///
/// Intended for the `withDeadline` fast-path and internal tests /
/// diagnostics. Exposed as `@_spi(Concurrency)` so runtime tests can
/// query the active deadline; not part of the general public API.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
public func _findNearestDeadline<C: Clock & Identifiable>(clock: C) -> C.Instant? {
  // Build a query box with the clock instance and a placeholder deadline;
  // only the identity fields (clock.id via the virtual `hasSameClock`
  // override) are consulted by the bridge, the deadline is unused.
  let queryBox = _ClockBox<C>(clock: clock, deadline: clock.now)
  let queryRaw = unsafe Unmanaged.passUnretained(queryBox).toOpaque()._rawValue
  let clockTypeRaw = unsafe Builtin.reinterpretCast(C.self) as Builtin.RawPointer

  let matchedRaw: Builtin.RawPointer = unsafe Builtin.taskFindNearestDeadlineForClock(
    queryClock: queryRaw,
    clockType: clockTypeRaw,
    // clockWT + identifiableWT are unused by this path; runtime dispatches
    // via the AnyObject-based bridge which does virtual dispatch on the
    // box's Swift class.
    clockWT: unsafe Builtin.inttoptr_Word(0._builtinWordValue),
    identifiableWT: unsafe Builtin.inttoptr_Word(0._builtinWordValue))
  _fixLifetime(queryBox)

  // Null pointer means "no matching record".
  let matchedInt = Int(bitPattern: UnsafeRawPointer(matchedRaw))
  if matchedInt == 0 {
    return nil
  }

  // Runtime handed us +1. Wrap via Unmanaged so ARC releases it once we
  // read the deadline off it. `matchedRaw` points at a `_ClockBox<C>`
  // (ClockType matched pointer-equal and hasSameClock returned true).
  let matchedBox: AnyObject =
    unsafe Unmanaged<AnyObject>.fromOpaque(UnsafeRawPointer(matchedRaw))
      .takeRetainedValue()
  let typed = unsafe unsafeDowncast(matchedBox, to: _ClockBox<C>.self)
  return typed.deadline
}

// ==== -----------------------------------------------------------------------
// MARK: Task.hasActiveDeadline / activeDeadline(for:)

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
// MARK: Task.hasActiveDeadline / activeDeadline(for:)

@available(StdlibDeploymentTarget 6.5, *)
extension Task where Success == Never, Failure == Never {
  /// Find the tightest deadline given the specified clock.
  ///
  /// The returned instant is the earliest deadline whose clock identity
  /// (`clock.id`) matches the argument's - nested `withDeadline` scopes
  /// on the same clock are coalesced to the tightest one.
  ///
  /// Deadlines installed for a *different* clock are ignored (there is no
  /// meaningful cross-clock conversion, so no attempt is made to unify
  /// them). Composing multiple `withDeadline` scopes on different clocks
  /// still works correctly - the tightest deadline for each clock governs
  /// independently - but this accessor can only report on one clock at a
  /// time.
  ///
  /// - SeeAlso: ``hasActiveDeadline``
  public static func activeDeadline<C: Clock & Identifiable>(for clock: C) -> C.Instant? {
    // No need to short-circut here with hasDeadline as the `find...` already does so.
    _findNearestDeadline(clock: clock)
  }
}

@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_swift_task_hasActiveDeadline")
internal func _swift_task_hasActiveDeadline() -> Bool

#endif // !$Embedded
