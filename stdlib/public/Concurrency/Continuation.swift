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
/// To resume the task, suspended on a continuation, call ``resume(returning:)``,
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
    unsafe self.context = context
  }

  deinit {
    fatalError("Continuation was deinitialized without being resumed.")
  }

  /// Extract the underlying raw continuation and discard `self`
  /// without firing the deinit trap
  @_alwaysEmitIntoClient
  consuming func _takeContext() -> Builtin.RawUnsafeContinuation {
    let ctx = unsafe self.context
    discard self
    return unsafe ctx
  }

  /// Resume the task awaiting the continuation by having it return
  /// from its suspension point
  ///
  /// - Parameter value: The value to return from the continuation
  @_alwaysEmitIntoClient
  public consuming func resume(returning value: consuming sending Success) where Failure == Never {
    unsafe Builtin.resumeNonThrowingContinuationReturning(context, value)
    discard self // prevent deinit from firing
  }

  /// Resume the task awaiting the continuation by having it return
  /// from its suspension point
  ///
  /// - Parameter value: The value to return from the continuation
  @_alwaysEmitIntoClient
  public consuming func resume(returning value: consuming sending Success) {
    unsafe Builtin.resumeThrowingContinuationReturning(context, value)
    discard self // prevent deinit from firing
  }

  /// Resume the task awaiting the continuation by having it throw an error
  /// from its suspension point
  ///
  /// - Parameter error: The error to throw from the continuation
  @_alwaysEmitIntoClient
  public consuming func resume(throwing error: __owned Failure) {
    unsafe Builtin.resumeThrowingContinuationThrowing(context, error)
    discard self // prevent deinit from firing
  }

  /// Resume the task awaiting the continuation by having it either
  /// return or throw an error based on the state of the given
  /// `Result` value
  ///
  /// - Parameter result: A value to either return or throw from the
  ///   continuation
  @_alwaysEmitIntoClient
  public consuming func resume(
    with result: consuming sending Result<Success, Failure>
  ) {
    switch consume result {
    case .success(let val):
      unsafe Builtin.resumeThrowingContinuationReturning(context, val)
    case .failure(let err):
      unsafe Builtin.resumeThrowingContinuationThrowing(context, err)
    }
    discard self // prevent deinit from firing
  }

  /// Resume the task awaiting the continuation by having it return
  /// from its suspension point
  @_alwaysEmitIntoClient
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
@_alwaysEmitIntoClient
@available(SwiftStdlib 6.4, *)
public nonisolated(nonsending) func withContinuation<Success: ~Copyable, Failure: Error>(
  of: Success.Type = Success.self,
  throwing: Failure.Type,
  _ body: (consuming Continuation<Success, Failure>) -> Void
) async throws(Failure) -> sending Success {
  do {
    return try await Builtin.withUnsafeThrowingContinuation {
      body(unsafe Continuation($0))
    }
  } catch {
    throw error as! Failure
  }
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
@_alwaysEmitIntoClient
@available(SwiftStdlib 6.4, *)
public nonisolated(nonsending) func withContinuation<Success: ~Copyable>(
  of: Success.Type = Success.self,
  _ body: (consuming Continuation<Success, Never>) -> Void
) async -> sending Success {
  return await Builtin.withUnsafeContinuation {
    body(unsafe Continuation($0))
  }
}

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
  @_alwaysEmitIntoClient
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
  @_alwaysEmitIntoClient
  public init(
    _ continuation: consuming Continuation<T, E>
  ) {
    unsafe self = UnsafeContinuation(continuation._takeContext())
  }
}
