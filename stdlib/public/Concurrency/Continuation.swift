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

// MARK: - ContinuationAwaiter

/// The *await* half of a split continuation, held by the suspending task.
///
/// It is non-copyable and non-escapable, so it must be awaited exactly once and
/// cannot leave the ``withContinuation(of:throwing:_:)`` scope it was vended
/// in, and it traps if it is destroyed without being awaited.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
@safe
@frozen
public struct ContinuationAwaiter<Success: ~Copyable, Failure: Error>: ~Copyable, ~Escapable {
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
  /// If the continuation was already resumed before this is reached, this does
  /// not suspend at all and returns the value directly.
  @export(implementation)
  public nonisolated(nonsending) consuming func wait() async throws(Failure) -> sending Success {
    do {
      return try await Builtin.awaitSplitThrowingContinuation(_takeContext())
    } catch {
      // Matches the cast Continuation.resume uses.
      throw error as! Failure
    }
  }

  /// Suspend until the paired ``Continuation`` is resumed, with an optional
  /// cancellation handler and a priority-escalation handler installed for the
  /// duration of the suspension.
  ///
  /// If the task is already cancelled, `onCancel` runs before this suspends.
  ///
  /// - Parameters:
  ///   - onCancel: Run when the task is cancelled while suspended
  ///     here. It may run concurrently with the resumption, and on any thread.
  ///   - onEscalate: Run with the new priority when the task's priority is
  ///     escalated while suspended here.
  @export(implementation)
  public nonisolated(nonsending) consuming func wait(
    onCancel handleCancellation: sending () -> Void,
    onEscalate handleEscalation: sending (TaskPriority) -> Void
  ) async throws(Failure) -> sending Success {
    // The escalation handler is wrapped so the runtime can hand it raw
    // priorities.
    try await Self._wait(
      context: _takeContext(),
      onCancel: handleCancellation,
      onEscalate: { handleEscalation(TaskPriority(rawValue: $0)) })
  }

  @export(implementation)
  nonisolated(nonsending) static func _wait(
    context: Builtin.RawUnsafeContinuation,
    onCancel handleCancellation: sending () -> Void,
    onEscalate handleEscalation: sending (UInt8) -> Void
  ) async throws(Failure) -> sending Success {
    // Install the handlers with the existing per-handler entry points.
    let cancellationRecord = unsafe _taskAddCancellationHandler(
      handler: handleCancellation)
    defer { unsafe _taskRemoveCancellationHandler(record: cancellationRecord) }

    let escalationRecord = unsafe _taskAddPriorityEscalationHandler(
      handler: { _, newPriority in handleEscalation(newPriority) })
    defer { unsafe _taskRemovePriorityEscalationHandler(record: escalationRecord) }

    do {
      return try await Builtin.awaitSplitThrowingContinuation(context)
    } catch {
      throw error as! Failure
    }
  }

  deinit {
    fatalError("ContinuationAwaiter deinitialized without being awaited")
  }
}

// MARK: - withContinuation

/// Creates a split continuation and hands `body` both halves: the *resume* half
/// (a ``Continuation``) to give to whoever performs the work, and the *await*
/// half (a ``ContinuationAwaiter``) for the task to await.
///
/// The continuation must be resumed exactly once, and the awaiter awaited
/// exactly once, before `body` returns.
///
/// - Parameters:
///   - of: The type the continuation is resumed with.
///   - throwing: The type the continuation can be resumed by throwing.
///   - body: Receives both halves of the continuation.  Returns the value the
///     continuation resumed with, which becomes the result of this function.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
@export(implementation)
public nonisolated(nonsending) func withContinuation<
  Success: ~Copyable,
  Failure: Error
>(
  of: Success.Type = Success.self,
  throwing: Failure.Type,
  _ body: nonisolated(nonsending) (
    consuming Continuation<Success, Failure>,
    consuming ContinuationAwaiter<Success, Failure>
  ) async throws(Failure) -> Success
) async throws(Failure) -> Success {
  // Create the split continuation once and vend both halves from it: the resume
  // half (given to whoever performs the work) and the await half (awaited by
  // `body`). The continuation is destroyed when this scope exits, after the
  // await has resolved and moved the value out.
  let token = Builtin.createSplitContinuation(Success.self)
  let continuation = Continuation<Success, Failure>(token)
  let awaiter = ContinuationAwaiter<Success, Failure>(context: token)
  do {
    let result = try await body(consume continuation, consume awaiter)
    Builtin.destroySplitContinuation(token)
    return result
  } catch {
    Builtin.destroySplitContinuation(token)
    throw error
  }
}

// MARK: - Resuming on the resumer's own thread

@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
extension Continuation where Success: ~Copyable {
  /// Resume the task awaiting this continuation by having it return the given
  /// value, offering the current execution context to run the resumed task.
  ///
  /// The offered execution context is used only if it belongs to the executor
  /// the awaiting task resumes on. If the context doesn't match the task is
  /// enqueued on its expected execution context. Which executor the awaiting
  /// task resumes on is decided only when the continuation is awaited. So the
  /// resumer cannot know in advance whether its thread will be usable so this
  /// is a best effort to resume synchronously with a graceful fallback that
  /// enqueues.
  ///
  /// This overload names only a serial executor, asserting no task executor
  /// preference of its own -- mirroring
  /// ``ExecutorJob/runSynchronously(on:)-(UnownedSerialExecutor)``. Use
  /// ``resume(returning:on:)`` if only a task executor is known, or
  /// ``resume(returning:isolatedTo:taskExecutor:)`` if both are.
  ///
  /// - Parameters:
  ///   - value: The value to return from the continuation.
  ///   - serialExecutor: The serial executor the resuming thread is running on.
  @export(implementation)
  public consuming func resume(
    returning value: consuming sending Success,
    isolatedTo serialExecutor: UnownedSerialExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, serialExecutor._executor, _getUndefinedTaskExecutor())
    Builtin.resumeThrowingContinuationReturning(_takeContext(), value)
  }

  /// Resume the task awaiting this continuation by having it throw the given
  /// error, offering the current thread to run the resumed task inline.
  ///
  /// See ``resume(returning:isolatedTo:)`` for when the thread is taken.
  @export(implementation)
  public consuming func resume(
    throwing error: __owned Failure,
    isolatedTo serialExecutor: UnownedSerialExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, serialExecutor._executor, _getUndefinedTaskExecutor())
    Builtin.resumeThrowingContinuationThrowing(_takeContext(), error)
  }

  /// Resume the task awaiting this continuation by having it either return or
  /// throw, based on the given `Result`, offering the current thread to run the
  /// resumed task inline.
  ///
  /// See ``resume(returning:isolatedTo:)`` for when the thread is taken.
  @export(implementation)
  public consuming func resume(
    with result: consuming sending Result<Success, Failure>,
    isolatedTo serialExecutor: UnownedSerialExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, serialExecutor._executor, _getUndefinedTaskExecutor())
    switch consume result {
    case .success(let value):
      Builtin.resumeThrowingContinuationReturning(_takeContext(), value)
    case .failure(let error):
      Builtin.resumeThrowingContinuationThrowing(_takeContext(), error)
    }
  }

  /// Resume the task awaiting this continuation by having it return the given
  /// value, offering the current thread to run the resumed task inline.
  ///
  /// This overload names only a task executor, asserting no serial executor
  /// (no actor isolation) of its own -- mirroring
  /// ``ExecutorJob/runSynchronously(on:)-(UnownedTaskExecutor)``. The offer is
  /// taken only if the awaiting task *also* resumes with no serial isolation
  /// and the same task executor; any actor isolation on the awaiting task's
  /// side makes this fall back to an enqueue, the same as a mismatch would.
  ///
  /// See ``resume(returning:isolatedTo:)`` for the general explanation of the
  /// best-effort fallback.
  @export(implementation)
  public consuming func resume(
    returning value: consuming sending Success,
    on taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, _getGenericSerialExecutor(), taskExecutor._executor)
    Builtin.resumeThrowingContinuationReturning(_takeContext(), value)
  }

  /// Resume the task awaiting this continuation by having it throw the given
  /// error, offering the current thread to run the resumed task inline.
  ///
  /// See ``resume(returning:on:)`` for when the thread is taken.
  @export(implementation)
  public consuming func resume(
    throwing error: __owned Failure,
    on taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, _getGenericSerialExecutor(), taskExecutor._executor)
    Builtin.resumeThrowingContinuationThrowing(_takeContext(), error)
  }

  /// Resume the task awaiting this continuation by having it either return or
  /// throw, based on the given `Result`, offering the current thread to run the
  /// resumed task inline.
  ///
  /// See ``resume(returning:on:)`` for when the thread is taken.
  @export(implementation)
  public consuming func resume(
    with result: consuming sending Result<Success, Failure>,
    on taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, _getGenericSerialExecutor(), taskExecutor._executor)
    switch consume result {
    case .success(let value):
      Builtin.resumeThrowingContinuationReturning(_takeContext(), value)
    case .failure(let error):
      Builtin.resumeThrowingContinuationThrowing(_takeContext(), error)
    }
  }

  /// Resume the task awaiting this continuation by having it return the given
  /// value, offering the current thread to run the resumed task inline.
  ///
  /// This overload names both a serial executor and a task executor, mirroring
  /// ``ExecutorJob/runSynchronously(isolatedTo:taskExecutor:)``. Use this when
  /// the resuming thread has a task executor preference in addition to its
  /// serial executor; otherwise prefer ``resume(returning:isolatedTo:)``.
  ///
  /// See ``resume(returning:isolatedTo:)`` for the general explanation of the
  /// best-effort fallback.
  @export(implementation)
  public consuming func resume(
    returning value: consuming sending Success,
    isolatedTo serialExecutor: UnownedSerialExecutor,
    taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, serialExecutor._executor, taskExecutor._executor)
    Builtin.resumeThrowingContinuationReturning(_takeContext(), value)
  }

  /// Resume the task awaiting this continuation by having it throw the given
  /// error, offering the current thread to run the resumed task inline.
  ///
  /// See ``resume(returning:isolatedTo:taskExecutor:)`` for when the thread is
  /// taken.
  @export(implementation)
  public consuming func resume(
    throwing error: __owned Failure,
    isolatedTo serialExecutor: UnownedSerialExecutor,
    taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, serialExecutor._executor, taskExecutor._executor)
    Builtin.resumeThrowingContinuationThrowing(_takeContext(), error)
  }

  /// Resume the task awaiting this continuation by having it either return or
  /// throw, based on the given `Result`, offering the current thread to run the
  /// resumed task inline.
  ///
  /// See ``resume(returning:isolatedTo:taskExecutor:)`` for when the thread is
  /// taken.
  @export(implementation)
  public consuming func resume(
    with result: consuming sending Result<Success, Failure>,
    isolatedTo serialExecutor: UnownedSerialExecutor,
    taskExecutor: UnownedTaskExecutor
  ) {
    unsafe _continuationSetResumingExecutors(
      self.context, serialExecutor._executor, taskExecutor._executor)
    switch consume result {
    case .success(let value):
      Builtin.resumeThrowingContinuationReturning(_takeContext(), value)
    case .failure(let error):
      Builtin.resumeThrowingContinuationThrowing(_takeContext(), error)
    }
  }
}

// MARK: - Runtime functions

@usableFromInline
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_continuation_setResumingExecutors")
internal func _continuationSetResumingExecutors(
  _ token: Builtin.RawUnsafeContinuation,
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
