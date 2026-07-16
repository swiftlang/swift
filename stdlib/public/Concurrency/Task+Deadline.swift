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
  let (seconds, attoseconds) = _instantComponents(expiration, clock: clock)

  // Fast path: if an outer `withDeadline` for this clock is already in effect
  // and its deadline is at or before ours, the outer one governs. Just run
  // `operation` inline - no record push, no cancellation scope, no timer.
  //
  // This matches the invariant the runtime relies on to make
  // `swift_task_findNearestDeadlineForClock` return the tightest record with
  // a single chain walk: because we (and the runtime's own subsumption
  // fast-path) never install a looser deadline for a clock that already has
  // a tighter one, the innermost matching record is always the tightest.
  let incoming = _DeadlineComponents(seconds: seconds, atto: attoseconds)
  if let outer = _findNearestDeadline(clock: clock), outer <= incoming {
    return try await operation()
  }

  // Encode the clock identity in the tagged form the runtime uses: for the
  // built-in system clocks we pass a small integer raw value and no box; for
  // custom clocks we pass 0 as the raw value and a Swift-side `_ClockIDBox`
  // wrapping `clock.id` as `AnyHashable`. The runtime consumes the +1 on
  // `customIDBox` (releasing it either on pop or immediately if the push is
  // subsumed - e.g. because another path installed a tighter deadline for
  // the same clock between our fast-path check and the push).
  let (systemClockRaw, customIDBox) = _encodeClockIdentityForPush(clock)
  let deadlineRecord = unsafe Builtin.taskPushDeadline(
    systemClockRaw: systemClockRaw,
    customIDBox: customIDBox,
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

/// The two-word `Swift.Duration` representation the deadline runtime stores
/// on a `TaskDeadlineStatusRecord`, exposed to Swift as an internal value
/// type so we can define a lexicographic `<=` (and a full `Comparable`
/// conformance) on it directly.
///
/// This is an implementation detail of `withDeadline` / `_findNearestDeadline`
/// and is not part of the public API. It is exposed as `@_spi(Concurrency)`
/// so runtime tests can inspect the innermost active deadline.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
@frozen
public struct _DeadlineComponents: Comparable, Sendable {
  public var seconds: Int64
  public var atto: Int64

  @inlinable
  public init(seconds: Int64, atto: Int64) {
    self.seconds = seconds
    self.atto = atto
  }

  /// Lexicographic ordering on (seconds, atto). Mirrors `compareDeadline` in
  /// TaskStatus.cpp so that the Swift-side subsumption fast-path stays in
  /// sync with what the runtime does.
  @inlinable
  public static func < (
    lhs: _DeadlineComponents,
    rhs: _DeadlineComponents
  ) -> Bool {
    if lhs.seconds != rhs.seconds {
      return lhs.seconds < rhs.seconds
    }
    return lhs.atto < rhs.atto
  }

  @inlinable
  public static func == (
    lhs: _DeadlineComponents,
    rhs: _DeadlineComponents
  ) -> Bool {
    return lhs.seconds == rhs.seconds && lhs.atto == rhs.atto
  }
}

// ==== -----------------------------------------------------------------------
// MARK: withDeadline(in:) shorthand

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
/// - Parameters:
///   - timeout: The duration, relative to `clock.now`, by which the
///     operation must complete.
///   - tolerance: The tolerance used for the sleep.
///   - clock: The clock to use for measuring time.
///   - operation: The asynchronous operation to complete before the deadline.
///
/// - Returns: The result of the operation if it completes successfully before or after the deadline expires.
/// - Throws: The error thrown by the operation.
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending)
func withDeadline<Return, Failure, C>(
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
// MARK: Timer arming

/// Create a timer and return a function to cancel the underlying task/job.
@available(StdlibDeploymentTarget 6.5, *)
private func _armDeadlineTimer<C: Clock & Identifiable>(
  scope: borrowing TaskCancellationScope,
  expiration: C.Instant,
  tolerance: C.Instant.Duration?,
  clock: C
) -> (@Sendable () -> Void) {
  // The scope handle itself is `~Escapable`, but its underlying record
  // pointer is a plain `UnsafeRawPointer` we can hand to the sending
  // closure. The scope's lifetime is the operation of `__withTaskCancellationScope`
  // above us, and the returned disarm closure is called from `defer` inside
  // that operation - so the record pointer is guaranteed live for the duration
  // of the timer task.
  let scopeRecord = unsafe scope._record

  // TODO: Replace this by picking the "Clock's executor"
  // TODO: Instead of creating a full task here, we want to enqueue a job at a deadline,
  //       that cancels the scope; and the returned func from here must attempt to cancel the job.
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
// MARK: Clock identity encoding

/// A heap-allocated box wrapping a custom clock's `id` as an `AnyHashable`.
///
/// Custom-clock deadlines pass a `+1` `_ClockIDBox` to the runtime via
/// `Builtin.taskPushDeadline`; the runtime holds the reference for the
/// lifetime of the installed record and calls back into Swift via
/// `_swift_task_deadlineClockIDsEqual` to compare two records' clocks.
@available(StdlibDeploymentTarget 6.5, *)
internal final class _ClockIDBox {
  let id: AnyHashable
  init(_ id: AnyHashable) { self.id = id }
}

/// Runtime callback: compare the underlying `AnyHashable` values of two
/// `_ClockIDBox`es. Non-box arguments compare as unequal.
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_swift_task_deadlineClockIDsEqual")
internal func _clockIDsEqual(_ a: AnyObject, _ b: AnyObject) -> Bool {
  guard let boxA = a as? _ClockIDBox, let boxB = b as? _ClockIDBox else {
    return false
  }
  return boxA.id == boxB.id
}

/// Map a `Clock & Identifiable` to the tagged form the deadline runtime
/// uses to determine subsumption between nested deadlines on the same
/// clock. Built-in system clocks encode as `(SystemClockID.rawValue, nil)`;
/// custom clocks encode as `(0, +1 _ClockIDBox)`. Exactly one branch
/// produces a non-null box; the runtime consumes the retain on push.
@available(StdlibDeploymentTarget 6.5, *)
@inline(__always)
private func _encodeClockIdentityForPush<C: Clock & Identifiable>(
  _ clock: C
) -> (systemClockRaw: UInt64, customIDBox: Builtin.NativeObject?) {
  if let known = clock.id as? SystemClockID {
    return (UInt64(known.rawValue), nil)
  }
  // Allocate a heap-owned box and hand a +1 to the runtime. `passRetained`
  // performs the retain-and-take-ownership dance; the returned opaque
  // pointer is reinterpreted as a `Builtin.NativeObject` for the builtin.
  let box = _ClockIDBox(AnyHashable(clock.id))
  let opaque = unsafe Unmanaged.passRetained(box).toOpaque()
  let native = unsafe Builtin.reinterpretCast(opaque)
    as Builtin.NativeObject
  return (0, native)
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

// ==== -----------------------------------------------------------------------
// MARK: Nearest-deadline SPI

/// Query the innermost active deadline installed on the current task for
/// the given clock, or nil if none. Returned as the same two-component
/// `Swift.Duration` representation the runtime stores.
///
/// Intended for the `withDeadline` fast-path and internal tests / diagnostics.
/// Exposed as `@_spi(Concurrency)` so runtime tests can query the active
/// deadline; not part of the general public API.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
public func _findNearestDeadline<C: Clock & Identifiable>(
  clock: C
) -> _DeadlineComponents? {
  // For system clocks: pass the raw value and a null pointer.
  // For custom clocks: allocate a *temporary* box that we own for the
  // duration of this call. Unlike `taskPushDeadline` (which consumes the
  // box), `taskFindNearestDeadlineForClock` only borrows it via a raw
  // pointer, so the box is released when this function returns.
  let boxRaw: Builtin.RawPointer
  let clockRaw: UInt64
  let keepAlive: _ClockIDBox?
  if let known = clock.id as? SystemClockID {
    clockRaw = UInt64(known.rawValue)
    boxRaw = unsafe Builtin.inttoptr_Word(0._builtinWordValue)
    keepAlive = nil
  } else {
    let box = _ClockIDBox(AnyHashable(clock.id))
    let opaque = unsafe Unmanaged.passUnretained(box).toOpaque()
    clockRaw = 0
    boxRaw = opaque._rawValue
    keepAlive = box
  }
  let ptr: UnsafeRawPointer = unsafe Builtin.taskFindNearestDeadlineForClock(
    systemClockRaw: clockRaw,
    customIDBox: boxRaw)
  _fixLifetime(keepAlive)

  if Int(bitPattern: ptr) == 0 {
    return nil
  }
  var secs: Int64 = 0
  var atto: Int64 = 0
  unsafe _swift_task_deadlineComponents(record: ptr,
                                        seconds: &secs,
                                        attoseconds: &atto)
  return _DeadlineComponents(seconds: secs, atto: atto)
}

/// Read the (seconds, attoseconds) components off a
/// `TaskDeadlineStatusRecord *` returned by
/// `Builtin.taskFindNearestDeadlineForClock`.
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_swift_task_deadlineComponents")
internal func _swift_task_deadlineComponents(
  record: UnsafeRawPointer,
  seconds: UnsafeMutablePointer<Int64>,
  attoseconds: UnsafeMutablePointer<Int64>)

// ==== -----------------------------------------------------------------------
// MARK: Task.hasActiveDeadline / activeDeadline(for:)

@available(StdlibDeploymentTarget 6.5, *)
extension Task where Success == Never, Failure == Never {
  /// Whether any deadline is currently active on the current task.
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

  /// The tightest active deadline installed on the current task for
  /// `clock`, or `nil` if there is no active deadline for that clock.
  ///
  /// The returned instant is the earliest deadline whose clock identity
  /// (`clock.id`) matches the argument's - nested `withDeadline` scopes
  /// on the same clock are coalesced by the runtime to the tightest one.
  ///
  /// Deadlines installed for a *different* clock are ignored (there is no
  /// meaningful cross-clock conversion, so no attempt is made to unify
  /// them). Composing multiple `withDeadline` scopes on different clocks
  /// still works correctly - the tightest deadline for each clock governs
  /// independently - but this accessor can only report on one clock at a
  /// time.
  ///
  /// - SeeAlso: ``hasActiveDeadline``
  public static func activeDeadline<C: Clock & Identifiable>(
    for clock: C
  ) -> C.Instant? {
    guard let components = _findNearestDeadline(clock: clock) else {
      return nil
    }
    // Reconstruct the C.Instant from the (seconds, atto) components. For
    // the built-in clocks whose Instant wraps a Swift.Duration we can
    // reverse `_instantComponents` via unsafeBitCast; for custom clocks we
    // fall back to `clock.now.advanced(by: duration)` where `duration` is
    // the total duration from the clock's reference point.
    let duration = Swift.Duration(
      secondsComponent: components.seconds,
      attosecondsComponent: components.atto)
    if C.self == ContinuousClock.self {
      let instant = ContinuousClock.Instant(_value: duration)
      return unsafe unsafeBitCast(instant, to: C.Instant.self)
    }
    if C.self == SuspendingClock.self {
      let instant = SuspendingClock.Instant(_value: duration)
      return unsafe unsafeBitCast(instant, to: C.Instant.self)
    }
    // Custom clocks: interpret the stored (seconds, atto) as a duration
    // from `clock.now`, which is how `_instantComponents` derived it for
    // custom clocks in the first place. This does drift when clock.now
    // has advanced since installation; that's a known limitation while
    // custom-clock deadlines fall through the duration-based path.
    let dur = unsafe unsafeBitCast(duration, to: C.Instant.Duration.self)
    return clock.now.advanced(by: dur)
  }
}

@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("_swift_task_hasActiveDeadline")
internal func _swift_task_hasActiveDeadline() -> Bool
