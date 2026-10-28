//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A mechanism to interface between synchronous and asynchronous code,
/// which enforces that the continuation is resumed exactly once.
///
/// Unlike `CheckedContinuation`, which detects misuse at runtime,
/// `Continuation` uses non-copyable semantics to enforce correct usage.
///
/// The continuation must only ever be resumed **exactly-once**.
/// The compiler will prevent attempts from resuming the continuation more than once.
///
/// If a `Continuation` is destroyed without being
/// resumed, the program traps with a diagnostic message indicating where
/// the continuation was created. Because it is noncopyable, the compiler
/// prevents accidental copies, and the `consuming` resume methods ensure
/// the continuation can only be used once.
///
/// To create a continuation
/// call ``withContinuation(of:throwing:_:)``.
///
/// To resume the task, suspended on a continuation, call ``resume(returning:)-5fa8w``,
/// ``resume(throwing:)``, ``resume(with:)``, or ``resume()``.
///
/// - SeeAlso: ``CheckedContinuation``
@safe
@frozen
@available(SwiftStdlib 6.4, *)
public struct Continuation<Success: ~Copyable, Failure: Error>: ~Copyable, @unchecked Sendable {

  // Implementation note: we're using raw continuation here since UnsafeContinuation
  // did not yet adopt ~Copyable, and doing so is a bit more involved.
  // TODO: Once UnsafeContinuation supports ~Copyable Success, we can use it here.
  @usableFromInline
  let context: Builtin.RawUnsafeContinuation

  @inlinable
  init(_ context: Builtin.RawUnsafeContinuation) {
    self.context = context
  }

  deinit {
    fatalError("Continuation was deinitialized without being resumed.")
  }

  /// Extract the underlying raw continuation and discard `self`
  /// without firing the deinit trap
  @export(implementation)
  consuming func _takeContext() -> Builtin.RawUnsafeContinuation {
    let ctx = self.context
    discard self
    return ctx
  }

  /// Resume the task awaiting the continuation by having it return
  /// from its suspension point
  ///
  /// - Parameter value: The value to return from the continuation
  @export(implementation)
  public consuming func resume(returning value: consuming sending Success) where Failure == Never {
    #if $BuiltinContinuationNonCopyableSuccess
    Builtin.resumeNonThrowingContinuationReturning(context, value)
    discard self // prevent deinit from firing
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  /// Resume the task awaiting the continuation by having it return
  /// from its suspension point
  ///
  /// - Parameter value: The value to return from the continuation
  @export(implementation)
  public consuming func resume(returning value: consuming sending Success) {
    #if $BuiltinContinuationNonCopyableSuccess
    Builtin.resumeThrowingContinuationReturning(context, value)
    discard self // prevent deinit from firing
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  /// Resume the task awaiting the continuation by having it throw an error
  /// from its suspension point
  ///
  /// - Parameter error: The error to throw from the continuation
  @export(implementation)
  public consuming func resume(throwing error: __owned Failure) {
    #if $BuiltinContinuationNonCopyableSuccess
    Builtin.resumeThrowingContinuationThrowing(context, error)
    discard self // prevent deinit from firing
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  /// Resume the task awaiting the continuation by having it either
  /// return or throw an error based on the state of the given
  /// `Result` value
  ///
  /// - Parameter result: A value to either return or throw from the
  ///   continuation
  @export(implementation)
  public consuming func resume(
    with result: consuming sending Result<Success, Failure>
  ) {
    #if $BuiltinContinuationNonCopyableSuccess
    switch consume result {
    case .success(let val):
      Builtin.resumeThrowingContinuationReturning(context, val)
    case .failure(let err):
      Builtin.resumeThrowingContinuationThrowing(context, err)
    }
    discard self // prevent deinit from firing
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  /// Resume the task awaiting the continuation by having it return
  /// from its suspension point
  @export(implementation)
  public consuming func resume() where Success == Void {
    self.resume(returning: ())
  }

}

// ==== -----------------------------------------------------------------------
// MARK: withContinuation

/// Invokes the passed in closure with a non-copyable continuation for the current task.
///
/// The body of the closure executes synchronously on the calling task, and
/// once it returns the calling task is suspended. It is possible to
/// immediately resume the task, or escape the continuation in order to
/// complete it afterwards, which will then resume the suspended task.
///
/// You must invoke the continuation's `resume` method exactly once.
/// The continuation is a noncopyable type, and therefore multiple resume
/// calls are prevented at compile time (as resuming the continuation
/// consumes it). However, if the continuation is dropped without being
/// resumed, the program traps.
///
/// - Parameters:
///   - of: The `Success` type returned by the continuation
///   - throwing: The `Failure` type that may be thrown
///   - body: A closure that takes a `Continuation` parameter
/// - Returns: The value the continuation is resumed with
@export(implementation)
@available(SwiftStdlib 6.4, *)
public nonisolated(nonsending) func withContinuation<Success: ~Copyable, Failure: Error>(
  of: Success.Type = Success.self,
  throwing: Failure.Type,
  _ body: (consuming Continuation<Success, Failure>) -> Void
) async throws(Failure) -> sending Success {
  #if $BuiltinContinuationNonCopyableSuccess
  do {
    return try await Builtin.withUnsafeThrowingContinuation {
      body(Continuation($0))
    }
  } catch {
    throw error as! Failure
  }
  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}

/// Invokes the passed in closure with a non-copyable continuation for the current task.
///
/// The body of the closure executes synchronously on the calling task, and
/// once it returns the calling task is suspended. It is possible to
/// immediately resume the task, or escape the continuation in order to
/// complete it afterwards, which will then resume the suspended task.
///
/// You must invoke the continuation's `resume` method exactly once.
/// The continuation is a noncopyable type, and therefore multiple resume
/// calls are prevented at compile time (as resuming the continuation
/// consumes it). However, if the continuation is dropped without being
/// resumed, the program traps.
///
/// - Parameters:
///   - of: The `Success` type returned by the continuation
///   - body: A closure that takes a `Continuation` parameter
/// - Returns: The value the continuation is resumed with
@export(implementation)
@available(SwiftStdlib 6.4, *)
public nonisolated(nonsending) func withContinuation<Success: ~Copyable>(
  of: Success.Type = Success.self,
  _ body: (consuming Continuation<Success, Never>) -> Void
) async -> sending Success {
  #if $BuiltinContinuationNonCopyableSuccess
  return await Builtin.withUnsafeContinuation {
    body(Continuation($0))
  }
  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}

// ==== -----------------------------------------------------------------------
// MARK: ContinuationWaiter

/// The *wait* half of a continuation, can be used by a task to suspend until
/// the continuation is resumed.
///
/// It must be waited on exactly once and cannot leave the
/// ``withContinuation(of:throwing:_:)`` scope it was passed into, and it traps
/// if it is destroyed without being waited on.
@available(SwiftStdlib 6.5, *)
@safe
@frozen
public struct ContinuationWaiter<Success: ~Copyable, Failure: Error>: ~Copyable, ~Escapable {
  @usableFromInline
  let context: Builtin.RawUnsafeContinuation

  @usableFromInline
  @_lifetime(immortal)
  init(context: Builtin.RawUnsafeContinuation) {
    self.context = context
  }

  /// Extract the underlying raw continuation and discard `self` without firing
  /// the deinit trap.
  @export(implementation)
  consuming func _takeContext() -> Builtin.RawUnsafeContinuation {
    let ctx = self.context
    discard self
    return ctx
  }

  /// Suspend until the paired ``Continuation`` is resumed, then return its value
  /// or throw its error.
  ///
  /// If the continuation was already resumed before `wait()` is called, this
  /// call does not suspend at all and returns the value immediately.
  @export(implementation)
  public nonisolated(nonsending) consuming func wait() async throws(Failure) -> sending Success {
    try await Self._wait(context: _takeContext())
  }

  /// Suspend until the paired ``Continuation`` is resumed, with a cancellation
  /// handler and a priority-escalation handler installed for the duration of
  /// the suspension.
  ///
  /// If the continuation was already resumed before `wait` is called, this call
  /// does not suspend at all and returns the value immediately.
  ///
  /// If the current task is already cancelled, `onCancel` runs before this
  /// function suspends.
  ///
  /// `onPriorityEscalated` is always called once when `wait` is called, with
  /// the task's current priority as both arguments, so that an escalation that
  /// happened before the handler was installed is not missed. It may therefore
  /// be called with a priority the work already runs at. Compare against the
  /// priority the work was enqueued with, and only act if it's higher:
  ///
  /// ```swift
  /// let priority = Task.currentPriority
  /// let job = queue.enqueue(request, priority: priority, resuming: continuation)
  /// return try await waiter.wait(
  ///   onCancel: { reason in queue.cancel(job, reason: reason) },
  ///   onPriorityEscalated: { _, newPriority in
  ///     guard newPriority > priority else { return }
  ///     queue.escalate(job, to: newPriority)
  ///   })
  /// ```
  ///
  /// - Parameters:
  ///   - onCancel: Run with the reason the task was cancelled with when it is
  ///     cancelled while suspended here. It may run concurrently with the
  ///     resumption, and on any thread.
  ///   - onPriorityEscalated: Run once when `wait` is called, and then when the
  ///     task's priority is escalated while suspended here. The first argument
  ///     is the old priority (before escalation), and the second argument is
  ///     the new escalated priority. It may run concurrently with the
  ///     resumption, and on any thread.
  @export(implementation)
  public nonisolated(nonsending) consuming func wait(
    onCancel handleCancellation: sending (CancellationError.Reason) -> Void,
    onPriorityEscalated handlePriorityEscalation:
      @Sendable (TaskPriority, TaskPriority) -> Void
  ) async throws(Failure) -> sending Success {
    // The handlers are wrapped so the runtime can hand them a raw reason and
    // raw priorities.
    try await Self._wait(
      context: _takeContext(),
      onCancel: {
        handleCancellation(CancellationError.Reason(_rawValue: $0) ?? .unspecified)
      },
      onPriorityEscalated: {
        handlePriorityEscalation(
          TaskPriority(rawValue: $0), TaskPriority(rawValue: $1))
      })
  }

  /// Suspend until the paired ``Continuation`` is resumed, with a
  /// cancellation handler installed for the duration of the suspension.
  ///
  /// If the continuation was already resumed before `wait` is called, this
  /// call does not suspend at all and returns the value immediately.
  ///
  /// If the current task is already cancelled, `onCancel` runs before this
  /// function suspends.
  ///
  /// - Parameters:
  ///   - onCancel: Run with the reason the task was cancelled with when it is
  ///     cancelled while suspended here. It may run concurrently with the
  ///     resumption, and on any thread.
  @export(implementation)
  public nonisolated(nonsending) consuming func wait(
    onCancel handleCancellation: sending (CancellationError.Reason) -> Void
  ) async throws(Failure) -> sending Success {
    // The handler is wrapped so the runtime can hand it a raw reason.
    try await Self._wait(
      context: _takeContext(),
      onCancel: {
        handleCancellation(CancellationError.Reason(_rawValue: $0) ?? .unspecified)
      })
  }

  /// Suspend until the paired ``Continuation`` is resumed, with a
  /// priority-escalation handler installed for the duration of the suspension.
  ///
  /// If the continuation was already resumed before `wait` is called, this
  /// call does not suspend at all and returns the value immediately.
  ///
  /// `onPriorityEscalated` is always called once when `wait` is called, with
  /// the task's current priority as both arguments, so that an escalation that
  /// happened before the handler was installed is not missed. It may therefore
  /// be called with a priority the work already runs at. Compare against the
  /// priority the work was enqueued with, and only act if it's higher:
  ///
  /// ```swift
  /// let priority = Task.currentPriority
  /// let job = queue.enqueue(request, priority: priority, resuming: continuation)
  /// return try await waiter.wait(onPriorityEscalated: { _, newPriority in
  ///   guard newPriority > priority else { return }
  ///   queue.escalate(job, to: newPriority)
  /// })
  /// ```
  ///
  /// - Parameters:
  ///   - onPriorityEscalated: Run once when `wait` is called, and then when the
  ///     task's priority is escalated while suspended here. The first argument
  ///     is the old priority (before escalation), and the second argument is
  ///     the new escalated priority. It may run concurrently with the
  ///     resumption, and on any thread.
  @export(implementation)
  public nonisolated(nonsending) consuming func wait(
    onPriorityEscalated handlePriorityEscalation:
      @Sendable (TaskPriority, TaskPriority) -> Void
  ) async throws(Failure) -> sending Success {
    // The escalation handler is wrapped so the runtime can hand it raw
    // priorities.
    try await Self._wait(
      context: _takeContext(),
      onPriorityEscalated: {
        handlePriorityEscalation(
          TaskPriority(rawValue: $0), TaskPriority(rawValue: $1))
      })
  }

  @export(implementation)
  nonisolated(nonsending) static func _wait(
    context: Builtin.RawUnsafeContinuation
  ) async throws(Failure) -> sending Success {
    #if $BuiltinSplitContinuations
    do {
      return try await Builtin.awaitSplitThrowingContinuation(context)
    } catch {
      // Matches the cast Continuation.resume uses.
      throw error as! Failure
    }
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  @export(implementation)
  nonisolated(nonsending) static func _wait(
    context: Builtin.RawUnsafeContinuation,
    onCancel handleCancellation: sending (UInt8) -> Void,
    onPriorityEscalated handlePriorityEscalation:
      @Sendable (UInt8, UInt8) -> Void
  ) async throws(Failure) -> sending Success {
    #if $BuiltinCancellationHandlerWithReason && $BuiltinConcurrencyStackNesting
    let cancellationRecord = unsafe Builtin.taskAddCancellationHandlerWithReason(
      handler: handleCancellation)
    defer { unsafe Builtin.taskRemoveCancellationHandler(record: cancellationRecord) }

    let escalationRecord = unsafe Builtin.taskAddPriorityEscalationHandler(
      handler: handlePriorityEscalation)
    defer {
      unsafe Builtin.taskRemovePriorityEscalationHandler(record: escalationRecord)
    }

    return try await _wait(context: context)
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  @export(implementation)
  nonisolated(nonsending) static func _wait(
    context: Builtin.RawUnsafeContinuation,
    onCancel handleCancellation: sending (UInt8) -> Void
  ) async throws(Failure) -> sending Success {
    #if $BuiltinCancellationHandlerWithReason
    let cancellationRecord = unsafe Builtin.taskAddCancellationHandlerWithReason(
      handler: handleCancellation)
    defer { unsafe Builtin.taskRemoveCancellationHandler(record: cancellationRecord) }

    return try await _wait(context: context)
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  @export(implementation)
  nonisolated(nonsending) static func _wait(
    context: Builtin.RawUnsafeContinuation,
    onPriorityEscalated handlePriorityEscalation:
      @Sendable (UInt8, UInt8) -> Void
  ) async throws(Failure) -> sending Success {
    #if $BuiltinConcurrencyStackNesting
    let escalationRecord = unsafe Builtin.taskAddPriorityEscalationHandler(
      handler: handlePriorityEscalation)
    defer {
      unsafe Builtin.taskRemovePriorityEscalationHandler(record: escalationRecord)
    }

    return try await _wait(context: context)
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  deinit {
    fatalError("ContinuationWaiter deinitialized without being waited on")
  }
}

// ==== -----------------------------------------------------------------------
// MARK: withContinuation

/// Creates a continuation and hands `body` both halves: the *resume* half
/// (a ``Continuation``) to give to whoever performs the work, and the *wait*
/// half (a ``ContinuationWaiter``) for the task to wait on.
///
/// The continuation must be resumed exactly once, and the waiter waited on
/// exactly once, before `body` returns. `body`'s own return and error types
/// are independent of the continuation's: `body` might wait for the
/// continuation's value and then transform it, or catch a thrown `Failure`
/// and rethrow something else -- there's no requirement that what `body`
/// returns or throws relates to `Success`/`Failure` at all.
///
/// - Parameters:
///   - of: The `Success` type the continuation is resumed with.
///   - throwing: The `Failure` type the continuation can be resumed by
///     throwing.
///   - body: Receives both halves of the continuation. Its own result
///     becomes the result of this function.
@available(SwiftStdlib 6.5, *)
@export(implementation)
public nonisolated(nonsending) func withContinuation<
  Success: ~Copyable,
  Failure: Error,
  BodyResult: ~Copyable,
  BodyFailure: Error
>(
  of: Success.Type = Success.self,
  throwing: Failure.Type,
  _ body: nonisolated(nonsending) (
    consuming Continuation<Success, Failure>,
    consuming ContinuationWaiter<Success, Failure>
  ) async throws(BodyFailure) -> BodyResult
) async throws(BodyFailure) -> BodyResult {
  #if $BuiltinSplitContinuations
  // Create the split continuation once and vend both halves from it: the resume
  // half (given to whoever performs the work) and the wait half (waited on by
  // `body`). The continuation is destroyed when this scope exits, after the
  // wait has resolved and moved the value out.
  //
  // The continuation's storage comes from the task allocator, which is a
  // stack, so it must be destroyed after everything allocated after it, such
  // as the buffers for `body`'s result and error.
  let rawContinuation = Builtin.createSplitContinuation(Success.self)
  defer { Builtin.destroySplitContinuation(rawContinuation) }
  let continuation = Continuation<Success, Failure>(rawContinuation)
  let waiter = ContinuationWaiter<Success, Failure>(context: rawContinuation)
  return try await body(consume continuation, consume waiter)
  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}

// ==== -----------------------------------------------------------------------
// MARK: Resuming synchronously on the resumer's executor

@available(SwiftStdlib 6.5, *)
extension Continuation where Success: ~Copyable {
  /// Resume the task awaiting the continuation by having it return normally
  /// from its suspension point, offering the current execution context to run
  /// the resumed task.
  ///
  /// The offered execution context is used only if it belongs to the executors
  /// the awaiting task resumes on. If the context doesn't match, the task is
  /// enqueued on its expected execution context instead.
  ///
  /// If the offer is taken, the resumed task runs synchronously, nested inside
  /// this call, just like a job run with
  /// ``ExecutorJob/runSynchronously(isolatedTo:taskExecutor:)``. This method
  /// must therefore only be called from the context of the offered executors,
  /// at a point where they could run one of their own jobs; typically from
  /// within the executor's implementation, between running jobs.
  ///
  /// Don't offer an executor that the calling code merely happens to be
  /// isolated to, such as `#isolation` in an actor method. Running the resumed
  /// task from there re-enters the actor outside of a suspension point, which
  /// breaks the guarantee that actors are only re-entered at suspension
  /// points, and can cause exclusivity violations.
  ///
  /// This overload names both a serial executor and a task executor, mirroring
  /// ``ExecutorJob/runSynchronously(isolatedTo:taskExecutor:)``. Use
  /// ``resumeSynchronously(returning:isolatedTo:)`` if only a serial executor
  /// is known, or ``resumeSynchronously(returning:on:)`` if only a task
  /// executor is known.
  ///
  /// - Parameters:
  ///   - value: The value to return from the continuation.
  ///   - serialExecutor: The serial executor the resuming thread is running on.
  ///   - taskExecutor: The task executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    returning value: consuming sending Success,
    isolatedTo serialExecutor: UnownedSerialExecutor,
    taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _resumeSynchronously(
      returning: value, serialExecutor._executor, taskExecutor._executor)
  }

  /// Resume the task awaiting this continuation by having it throw the given
  /// error, offering the current execution context to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - error: The error to throw from the continuation.
  ///   - serialExecutor: The serial executor the resuming thread is running on.
  ///   - taskExecutor: The task executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    throwing error: __owned Failure,
    isolatedTo serialExecutor: UnownedSerialExecutor,
    taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _resumeSynchronously(
      throwing: error, serialExecutor._executor, taskExecutor._executor)
  }

  /// Resume the task awaiting this continuation by having it either return or
  /// throw based on the given `Result`, offering the current execution context
  /// to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - result: A value to either return or throw from the continuation.
  ///   - serialExecutor: The serial executor the resuming thread is running on.
  ///   - taskExecutor: The task executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    with result: consuming sending Result<Success, Failure>,
    isolatedTo serialExecutor: UnownedSerialExecutor,
    taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _resumeSynchronously(
      with: result, serialExecutor._executor, taskExecutor._executor)
  }

  /// Resume the task awaiting this continuation by having it return the given
  /// value, offering the current execution context to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - value: The value to return from the continuation.
  ///   - serialExecutor: The serial executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    returning value: consuming sending Success,
    isolatedTo serialExecutor: UnownedSerialExecutor
  ) {
    unsafe _resumeSynchronously(
      returning: value, serialExecutor._executor, _getUndefinedTaskExecutor())
  }

  /// Resume the task awaiting this continuation by having it throw the given
  /// error, offering the current execution context to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - error: The error to throw from the continuation.
  ///   - serialExecutor: The serial executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    throwing error: __owned Failure,
    isolatedTo serialExecutor: UnownedSerialExecutor
  ) {
    unsafe _resumeSynchronously(
      throwing: error, serialExecutor._executor, _getUndefinedTaskExecutor())
  }

  /// Resume the task awaiting this continuation by having it either return or
  /// throw based on the given `Result`, offering the current execution context
  /// to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - result: A value to either return or throw from the continuation.
  ///   - serialExecutor: The serial executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    with result: consuming sending Result<Success, Failure>,
    isolatedTo serialExecutor: UnownedSerialExecutor
  ) {
    unsafe _resumeSynchronously(
      with: result, serialExecutor._executor, _getUndefinedTaskExecutor())
  }

  /// Resume the task awaiting this continuation by having it return the given
  /// value, offering the current execution context to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - value: The value to return from the continuation.
  ///   - taskExecutor: The task executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    returning value: consuming sending Success,
    on taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _resumeSynchronously(
      returning: value, _getGenericSerialExecutor(), taskExecutor._executor)
  }

  /// Resume the task awaiting this continuation by having it throw the given
  /// error, offering the current execution context to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - error: The error to throw from the continuation.
  ///   - taskExecutor: The task executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    throwing error: __owned Failure,
    on taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _resumeSynchronously(
      throwing: error, _getGenericSerialExecutor(), taskExecutor._executor)
  }

  /// Resume the task awaiting this continuation by having it either return or
  /// throw based on the given `Result`, offering the current execution context
  /// to run the resumed task.
  ///
  /// This must only be called from the context of the offered executor. See
  /// ``resumeSynchronously(returning:isolatedTo:taskExecutor:)`` for when the
  /// offered execution context is used to run the resumed task.
  ///
  /// - Parameters:
  ///   - result: A value to either return or throw from the continuation.
  ///   - taskExecutor: The task executor the resuming thread is running on.
  @export(implementation)
  public consuming func resumeSynchronously(
    with result: consuming sending Result<Success, Failure>,
    on taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _resumeSynchronously(
      with: result, _getGenericSerialExecutor(), taskExecutor._executor)
  }

  /// Resume by returning `value`, offering the given executors to run the
  /// resumed task.
  @export(implementation)
  consuming func _resumeSynchronously(
    returning value: consuming sending Success,
    _ serialExecutor: Builtin.Executor,
    _ taskExecutor: Builtin.Executor
  ) {
    unsafe _continuationResumeOnExecutors(
      _takeContext(), value, nil, serialExecutor, taskExecutor)
  }

  /// Resume by throwing `error`, offering the given executors to run the
  /// resumed task.
  @export(implementation)
  consuming func _resumeSynchronously(
    throwing error: __owned Failure,
    _ serialExecutor: Builtin.Executor,
    _ taskExecutor: Builtin.Executor
  ) {
    // There is no value to return, so pass an empty one for the runtime to
    // destroy.
    unsafe _continuationResumeOnExecutors(
      _takeContext(), (), error, serialExecutor, taskExecutor)
  }

  /// Resume by returning or throwing, based on `result`, offering the given
  /// executors to run the resumed task.
  @export(implementation)
  consuming func _resumeSynchronously(
    with result: consuming sending Result<Success, Failure>,
    _ serialExecutor: Builtin.Executor,
    _ taskExecutor: Builtin.Executor
  ) {
    switch consume result {
    case .success(let value):
      _resumeSynchronously(returning: value, serialExecutor, taskExecutor)
    case .failure(let error):
      _resumeSynchronously(throwing: error, serialExecutor, taskExecutor)
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Runtime functions

/// Resume `continuation` by returning `result` if `error` is nil, or by
/// throwing `error` otherwise, offering the given executors to run the resumed
/// task. Takes ownership of both `result` and `error`.
@usableFromInline
@available(SwiftStdlib 6.5, *)
@_silgen_name("swift_continuation_resumeOnExecutors")
internal func _continuationResumeOnExecutors<T: ~Copyable>(
  _ continuation: Builtin.RawUnsafeContinuation,
  _ result: consuming T,
  _ error: __owned (any Error)?,
  _ serialExecutor: Builtin.Executor,
  _ taskExecutor: Builtin.Executor)


// ==== -----------------------------------------------------------------------
// MARK: Convert to CheckedContinuation

@available(SwiftStdlib 6.4, *)
extension CheckedContinuation {
  /// Convert a non-copyable continuation to a ``CheckedContinuation``
  ///
  /// A checked continuation may be escaped into contexts where
  /// the non-copyable semantics would not be able to statically enforce
  /// the resume-once semantics, however the correct use of the
  /// continuation is enforced in some way at runtime.
  @export(implementation)
  public init(
    _ continuation: consuming Continuation<T, E>,
    function: String = #function
  ) {
    unsafe self.init(
      continuation: UnsafeContinuation(continuation._takeContext()),
      function: function)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Convert to UnsafeContinuation

@available(SwiftStdlib 6.4, *)
extension UnsafeContinuation {
  /// Convert a non-copyable continuation to an ``UnsafeContinuation``.
  ///
  /// An unsafe continuation may be escaped into contexts where
  /// the non-copyable semantics would not be able to statically enforce
  /// the resume-once semantics, however the correct use of the
  /// continuation is enforced in some way at runtime.
  @export(implementation)
  public init(
    _ continuation: consuming Continuation<T, E>
  ) {
    unsafe self = UnsafeContinuation(continuation._takeContext())
  }
}

// MARK: - Continuations as jobs

/// The job that resumes the task awaiting a split continuation, which is the
/// continuation itself.
///
/// This is deliberately not available to clients.  The job is part of the
/// continuation's storage, so the continuation must not be resumed any other
/// way while the job might run, nor the job used once the continuation has been
/// resumed.  `SchedulingExecutor.schedule(_:at:clock:)` for a `Continuation`
/// is the safe interface to it.
///
/// A split continuation begins with a job header, so the raw continuation
/// is also a job.  Running the job resumes the continuation normally, so it can
/// be handed to an executor to schedule the resumption, with no other object
/// standing in for it.  This is only meaningful if the continuation's result
/// is `Void`.
@available(StdlibDeploymentTarget 6.5, *)
internal func _jobToResume<Failure: Error>(
  _ continuation: UnsafeContinuation<Void, Failure>
) -> ExecutorJob {
  let job = unsafe ExecutorJob(
    unsafeBitCast(continuation, to: UnownedJob.self))
  precondition(job.kind != .task,
               "only a split continuation can be converted to a job")
  return job
}
