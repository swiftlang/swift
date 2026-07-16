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

/// A continuation that an executor resumes on behalf of a suspended task.
///
/// The difference between an ``ExecutorContinuation`` and ``Continuation`` is
/// that the executor continuations allow synchronous resumption which donates
/// the executors thread to run the resumed task inline.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 9999, *)
@safe
@frozen
public struct ExecutorContinuation<Success: ~Copyable, Failure: Error>: ~Copyable, @unchecked Sendable {
  @usableFromInline
  let context: Builtin.RawUnsafeContinuation

  @usableFromInline
  init(context: Builtin.RawUnsafeContinuation) {
    self.context = context
  }

  deinit {
    fatalError("ExecutorContinuation was deinitialized without being resumed.")
  }

  /// Extract the underlying raw continuation and discard `self`
  /// without firing the deinit trap
  @_alwaysEmitIntoClient
  consuming func _takeContext() -> Builtin.RawUnsafeContinuation {
    let ctx = self.context
    discard self
    return ctx
  }

  /// Resume the task synchronously awaiting the continuation by having it return
  /// from its suspension point.
  ///
  /// - Parameter value: The value to return from the continuation
  @_alwaysEmitIntoClient
  public consuming func resumeSynchronously(
    returning value: consuming sending Success
  ) where Failure == Never {
    Builtin.resumeDetachedContinuationReturning(_takeContext(), value)
  }

  /// Resume the task synchronously awaiting the continuation by having it return
  /// from its suspension point.
  ///
  /// - Parameter value: The value to return from the continuation
  @_alwaysEmitIntoClient
  public consuming func resumeSynchronously(
    returning value: consuming sending Success
  ) {
    Builtin.resumeDetachedThrowingContinuationReturning(_takeContext(), value)
  }

  /// Resume the task synchronously awaiting the continuation by having it throw an error
  /// from its suspension point.
  ///
  /// - Parameter error: The error to throw from the continuation
  @_alwaysEmitIntoClient
  public consuming func resumeSynchronously(throwing error: consuming Failure) {
    Builtin.resumeDetachedThrowingContinuationThrowing(_takeContext(), error)
  }

  /// Resume the task synchronously awaiting the continuation by having it either
  /// return or throw an error based on the state of the given
  /// `Result` value.
  ///
  /// - Parameter result: A value to either return or throw from the
  ///   continuation
  @_alwaysEmitIntoClient
  public consuming func resumeSynchronously(
    with result: consuming sending Result<Success, Failure>
  ) {
    switch consume result {
    case .success(let value):
      Builtin.resumeDetachedThrowingContinuationReturning(_takeContext(), value)
    case .failure(let error):
      Builtin.resumeDetachedThrowingContinuationThrowing(_takeContext(), error)
    }
  }
}

/// Suspends the current task and hands a non-copyable ``ExecutorContinuation``
/// to `body`, which must pass it to an executor's `enqueue` (which mints and
/// returns an ``OperationExecutorRegistration``) so the executor resumes it
/// exactly once.
///
/// Because `enqueue` runs inside `body` before this suspends, the returned
/// registration is available to a cancellation / escalation handler installed
/// around the await without anything being shared back out of `body`.
///
/// `Failure` is inferred from the type of `body`'s parameter (e.g. annotate it
/// `(continuation: consuming ExecutorContinuation<Void, CancellationError>)`).
///
/// - Parameter body: A closure that receives the continuation and hands it to
///   an executor.  Runs synchronously on the calling task before it suspends.
/// - Throws: Whatever the continuation is resumed with via
///   ``ExecutorContinuation/resumeSynchronously(throwing:)``.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 9999, *)
@_alwaysEmitIntoClient
public nonisolated(nonsending) func withExecutorContinuation<Failure: Error>(
  _ body: (consuming ExecutorContinuation<Void, Failure>) -> Void
) async throws(Failure) {
  // Even though create and await are co-located in this frame, we build on the
  // detached primitive so that every ExecutorContinuation token is a context
  // pointer and resume is uniformly by-context.
  let token = Builtin.createDetachedContinuation(Void.self)
  body(ExecutorContinuation<Void, Failure>(context: token))
  do {
    let _: Void = try await Builtin.awaitDetachedThrowingContinuation(token)
  } catch {
    Builtin.destroyDetachedContinuation(token)
    throw error as! Failure
  }
  Builtin.destroyDetachedContinuation(token)
}

// MARK: - Split executor continuation (Design 1)
//
// The *resume* half reuses the existing `ExecutorContinuation` type above; the
// *await* half is `ExecutorContinuationAwaiter`. Both halves share one detached
// continuation token (created by `createDetachedContinuation`): the producer is
// handed to an executor and the awaiter is awaited by the suspending task, so
// create, await, and resume can span different frames and threads.

/// The *await* half of a split executor continuation, held by the suspending
/// task. Non-copyable and non-escapable so it is awaited exactly once and cannot
/// leave the `withExecutorContinuation` scope; traps if dropped without being
/// awaited.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 9999, *)
@safe
@frozen
public struct ExecutorContinuationAwaiter<Success: ~Copyable, Failure: Error>: ~Copyable, ~Escapable {
  @usableFromInline
  let context: Builtin.RawUnsafeContinuation

  @usableFromInline
  @_lifetime(immortal)
  init(context: Builtin.RawUnsafeContinuation) {
    self.context = context
  }

  /// Extract the underlying raw continuation and discard `self` without firing
  /// the deinit trap.
  @_alwaysEmitIntoClient
  consuming func _takeContext() -> Builtin.RawUnsafeContinuation {
    let ctx = self.context
    discard self
    return ctx
  }

  /// Suspend until the paired ``ExecutorContinuation`` is resumed, then return
  /// its value (or throw its error). Consumes the awaiter: awaitable exactly once.
  @_alwaysEmitIntoClient
  public consuming func wait() async throws(Failure) -> sending Success {
    do {
      return try await Builtin.awaitDetachedThrowingContinuation(_takeContext())
    } catch {
      throw error as! Failure
    }
  }

  deinit {
    fatalError("ExecutorContinuationAwaiter deinitialized without being awaited")
  }
}

/// Creates a split executor continuation and hands `body` both halves: the
/// *resume* half (``ExecutorContinuation``) to give to an executor, and the
/// *await* half (``ExecutorContinuationAwaiter``) that the suspending task
/// awaits. The result of `withExecutorContinuation` is the result of `body`.
///
/// - Parameters:
///   - of: The success type `body` returns.
///   - throwing: The error type `body` can throw.
///   - body: Receives both halves of the continuation; enqueues the resume half
///     with an executor and awaits the await half, returning a result.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 9999, *)
@_alwaysEmitIntoClient
public nonisolated(nonsending) func withExecutorContinuation<
  Success: ~Copyable,
  Failure: Error,
  ContinuationSuccess: ~Copyable,
  ContinuationFailure: Error
>(
  of: Success.Type = Success.self,
  throwing: Failure.Type,
  _ body: nonisolated(nonsending) (
    consuming ExecutorContinuation<ContinuationSuccess, ContinuationFailure>,
    consuming ExecutorContinuationAwaiter<ContinuationSuccess, ContinuationFailure>
  ) async throws(Failure) -> Success
) async throws(Failure) -> Success {
  // Create the detached continuation once and vend both halves from the same
  // token: the resume half (given to an executor) and the await half (awaited
  // by `body`).  The context is destroyed when this scope exits, after the
  // await has resolved and moved the value out.
  let token = Builtin.createDetachedContinuation(ContinuationSuccess.self)
  let producer =
    ExecutorContinuation<ContinuationSuccess, ContinuationFailure>(context: token)
  let awaiter =
    ExecutorContinuationAwaiter<ContinuationSuccess, ContinuationFailure>(
      context: token)
  do {
    let result = try await body(consume producer, consume awaiter)
    Builtin.destroyDetachedContinuation(token)
    return result
  } catch {
    Builtin.destroyDetachedContinuation(token)
    throw error
  }
}

