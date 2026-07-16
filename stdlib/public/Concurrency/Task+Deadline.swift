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
  // Encode the clock identity in the tagged form the runtime uses: for the
  // built-in system clocks we pass a small integer raw value and no box; for
  // custom clocks we pass 0 as the raw value and a Swift-side `_ClockIDBox`
  // wrapping `clock.id` as `AnyHashable`. The runtime consumes the +1 on
  // `customIDBox` (releasing it either on pop or immediately if the push is
  // subsumed by an outer deadline for the same clock).
  let (systemClockRaw, customIDBox) = _encodeClockIdentityForPush(clock)
  let (seconds, attoseconds) = _instantComponents(expiration, clock: clock)
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
  // closure. The scope's lifetime is the body of `__withTaskCancellationScope`
  // above us, and the returned disarm closure is called from `defer` inside
  // that body - so the record pointer is guaranteed live for the duration
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
/// Intended for tests / diagnostics: production code should use
/// `withDeadline` directly.
@available(StdlibDeploymentTarget 6.5, *)
public func _findNearestDeadline<C: Clock & Identifiable>(
  clock: C
) -> (seconds: Int64, atto: Int64)? {
  // For system clocks: pass the raw value and no box.
  // For custom clocks: allocate a *temporary* box that we own for the
  // duration of this call. Unlike `taskPushDeadline` (which consumes the
  // box), `taskFindNearestDeadlineForClock` only borrows it, so the box
  // is released when this function returns.
  let ptr: UnsafeRawPointer
  let keepAlive: _ClockIDBox?
  if let known = clock.id as? SystemClockID {
    ptr = unsafe Builtin.taskFindNearestDeadlineForClock(
      systemClockRaw: UInt64(known.rawValue),
      customIDBox: nil)
    keepAlive = nil
  } else {
    let box = _ClockIDBox(AnyHashable(clock.id))
    let opaque = unsafe Unmanaged.passUnretained(box).toOpaque()
    let native = unsafe Builtin.reinterpretCast(opaque)
      as Builtin.NativeObject
    ptr = unsafe Builtin.taskFindNearestDeadlineForClock(
      systemClockRaw: 0,
      customIDBox: native)
    keepAlive = box
  }
  _fixLifetime(keepAlive)

  if unsafe Int(bitPattern: ptr) == 0 {
    return nil
  }
  var secs: Int64 = 0
  var atto: Int64 = 0
  unsafe _swift_task_deadlineComponents(record: ptr,
                                        seconds: &secs,
                                        attoseconds: &atto)
  return (secs, atto)
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
