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

/// Represents an independently-cancellable region within a task, distinct from whole-task cancellation.
///
/// Use `withTaskCancellationScope(_:)` to run an operation inside a scope.
/// The operation receives the scope and can cancel it, for example from a
/// structured event handler that runs concurrently to the operation.
///
/// A scope is `Sendable`, so you can cancel it and check its cancellation
/// from any thread. A scope is non-escapable, so you can use it only while
/// the operation of its `withTaskCancellationScope` runs.
///
/// ## Cancellation Semantics
/// Cancelling a scope is semantically equivalent to cancelling as-if the scope were its own task.
///
/// The scope's effects on `Task.isCancelled` are strictly contained for the duration
/// of executing the `operation`; the enclosing task's own cancellation state is unchanged.
///
/// Cancellation cascades to nested scopes, to task groups and to structured
/// child tasks (`async let`, child tasks of task groups created inside the
/// scope) created inside the scope. A cancellation shield stops the
/// cancellation of the enclosing task or scopes from reaching a scope inside it.
@safe
@frozen
@available(StdlibDeploymentTarget 6.5, *)
public struct TaskCancellationScope: ~Escapable, @unchecked Sendable {
  /// The record of the scope in the task, or `nil` if the scope was created
  /// outside of a task.
  @usableFromInline
  internal let _record: UnsafeRawPointer?

  @usableFromInline
  @_lifetime(immortal)
  internal init(record: UnsafeRawPointer?) {
    unsafe self._record = record
  }

  /// Cancel this scope.
  ///
  /// Causes `Task.isCancelled` to return `true` for code running inside the
  /// scope's `operation` and runs any `withTaskCancellationHandler` handlers
  /// installed inside the scope. For example, this allows operations like
  /// `Task.sleep(for:)` inside the scope to return early.
  ///
  /// It doesn't cancel the enclosing task or the enclosing scopes, and it
  /// doesn't run the handlers installed outside of the scope.
  ///
  /// Cancellation cascades to nested scopes, to task groups and to structured
  /// child tasks (`async let`, child tasks of task groups created inside the
  /// scope) created inside the scope. This applies both to the ones that
  /// exist when you call `cancel(reason:)` and to the ones created later.
  /// A cancellation shield inside of the scope prevents the cancellation
  /// from reaching the code inside of the shield.
  ///
  /// A scope is only cancelled once. The first cancellation decides the
  /// reason, and later calls to `cancel(reason:)` have no effect.
  ///
  /// If the scope was created outside of a task, this method has no effect.
  ///
  /// - Parameter reason: The ``CancellationError/Reason`` to record on the
  ///   scope; observable via ``Task/cancellationReason`` from code
  ///   running inside the scope, and passed to reason-aware cancellation
  ///   handlers. Defaults to ``CancellationError/Reason/unspecified``.
  @export(implementation)
  public func cancel(reason: CancellationError.Reason = .unspecified) {
    unsafe _taskCancelTaskCancellationScope(
      record: _record, flags: UInt(reason._rawValue))
  }

  /// A Boolean value indicating whether this scope has been cancelled.
  ///
  /// Checks only the cancellation status of this scope. A shield around the
  /// code that reads this property doesn't change the result.
  ///
  /// If the scope is cancelled, `Task.isCancelled` also returns `true` for code running inside the
  /// scope, unless a cancellation shield inside of the scope prevents it.
  ///
  /// - Returns: `true` if this scope has been cancelled (via
  ///   ``cancel(reason:)`` or by the cancellation of an enclosing task or
  ///   scope that reaches it), `false` otherwise. Once `true`, remains `true` for the
  ///   scope's lifetime.
  @export(implementation)
  public var isCancelled: Bool {
    let packed = unsafe _taskCancellationScopeGetIsCancelledWithReason(
      record: _record)
    return packed & 1 != 0
  }

  /// A value indicating the reason this scope has been cancelled.
  ///
  /// Checks only the cancellation status of this scope. A shield around the
  /// code that reads this property doesn't change the result.
  ///
  /// - Returns: A non-`nil` reason if this scope has been cancelled (via
  ///   ``cancel(reason:)`` or by the cancellation of an enclosing task or
  ///   scope that reaches it), `nil` otherwise. Once this returns a reason, it keeps
  ///   returning the same reason for the scope's lifetime.
  @export(implementation)
  public var cancellationReason: CancellationError.Reason? {
    let packed = unsafe _taskCancellationScopeGetIsCancelledWithReason(
      record: _record)
    guard packed & 1 != 0 else { return nil }
    let raw = UInt8(truncatingIfNeeded: packed >> 1)
    return CancellationError.Reason(_rawValue: raw) ?? .unspecified
  }
}

// ==== -----------------------------------------------------------------------
// MARK: withTaskCancellationScope

/// Executes an operation inside a cancellation scope.
///
/// The `operation` closure receives a ``TaskCancellationScope`` which can be
/// used to cancel the scope.
///
/// Cancelling the scope causes `Task.isCancelled` to return `true` for code
/// executing inside `operation`, and triggers any other effects task cancellation does.
///
/// ```swift
/// await withTaskCancellationScope { scope in
///   print(Task.isCancelled) // false
///
///   scope.cancel()
///
///   print(Task.isCancelled) // true
///   try? await Task.sleep(for: .seconds(1)) // returns immediately
/// }
///
/// print(Task.isCancelled) // false, the task itself was never cancelled
/// ```
///
/// The operation runs in the calling task and on the isolation of the caller.
/// This function always runs the operation, even if the caller already
/// observes a cancellation. In that case, the scope starts out cancelled with
/// the same reason. It is up to the operation to check for cancellation.
///
/// ## Cancellation Semantics
/// Cancelling a scope is semantically equivalent to cancelling as-if the scope were its own task.
///
/// The scope's effects on `Task.isCancelled` are strictly contained for the duration
/// of executing the `operation`; the enclosing task's own cancellation state is unchanged.
///
/// Cancellation cascades to nested scopes, to task groups and to structured
/// child tasks (`async let`, child tasks of task groups created inside the
/// scope) created inside the scope.
///
/// - Parameter operation: The work to perform. Receives the scope which can
///   be used to cancel the operation. The scope can't escape `operation`.
/// - Returns: the result returned by the `operation` closure.
/// - Throws: if an error is thrown by the `operation` closure.
@export(implementation)
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending) func withTaskCancellationScope<Return, Failure>(
  _ operation: nonisolated(nonsending) (TaskCancellationScope) async throws(Failure) -> Return
) async throws(Failure) -> Return where Return: ~Copyable, Failure: Error {
#if $BuiltinTaskCancellationScope
  let record = unsafe Builtin.taskCancellationScopePush()
  defer { unsafe Builtin.taskCancellationScopePop(record: record) }
#else
  let record = unsafe _taskPushTaskCancellationScope()
  defer { unsafe _taskPopTaskCancellationScope(record: record) }
#endif
  let scope = unsafe TaskCancellationScope(record: record)
  return try await operation(scope)
}

/// Executes a synchronous operation inside a cancellation scope.
///
/// The `operation` closure receives a ``TaskCancellationScope`` which can be
/// used to cancel the scope. This allows a synchronous function to cancel a
/// part of the work it does, for example a blocking operation that checks
/// `Task.isCancelled`.
///
/// If this function is called outside of a task, the scope has no effect:
/// cancelling it doesn't change `Task.isCancelled` and the scope never
/// reports that it is cancelled.
///
/// See the asynchronous `withTaskCancellationScope(_:)` for the cancellation
/// semantics.
///
/// - Parameter operation: The work to perform. Receives the scope which can
///   be used to cancel the operation. The scope can't escape `operation`.
/// - Returns: the result returned by the `operation` closure.
/// - Throws: if an error is thrown by the `operation` closure.
@export(implementation)
@available(StdlibDeploymentTarget 6.5, *)
public func withTaskCancellationScope<Return, Failure>(
  _ operation: (TaskCancellationScope) throws(Failure) -> Return
) throws(Failure) -> Return where Return: ~Copyable, Failure: Error {
  // Outside of a task there is no task to attach the record to.
  guard unsafe _getCurrentAsyncTask() != nil else {
    return try operation(unsafe TaskCancellationScope(record: nil))
  }
#if $BuiltinTaskCancellationScope
  let record = unsafe Builtin.taskCancellationScopePush()
  defer { unsafe Builtin.taskCancellationScopePop(record: record) }
#else
  let record = unsafe _taskPushTaskCancellationScope()
  defer { unsafe _taskPopTaskCancellationScope(record: record) }
#endif
  let scope = unsafe TaskCancellationScope(record: record)
  return try operation(scope)
}

// ==== -----------------------------------------------------------------------
// MARK: Runtime functions

@usableFromInline
internal typealias TaskCancellationScopeRecordUnsafeRawPointer = UnsafeRawPointer

@usableFromInline
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_task_pushCancellationScope")
internal func _taskPushTaskCancellationScope() -> TaskCancellationScopeRecordUnsafeRawPointer

@usableFromInline
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_task_popCancellationScope")
internal func _taskPopTaskCancellationScope(record: TaskCancellationScopeRecordUnsafeRawPointer)

@usableFromInline
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_task_cancelCancellationScope")
internal func _taskCancelTaskCancellationScope(
  record: TaskCancellationScopeRecordUnsafeRawPointer?,
  flags: UInt)

/// Returns the cancelled flag in bit 0 and the `CancellationError.Reason` raw
/// value in bits 1-3.
@usableFromInline
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_task_cancellationScopeGetIsCancelledWithReason")
internal func _taskCancellationScopeGetIsCancelledWithReason(
  record: TaskCancellationScopeRecordUnsafeRawPointer?) -> UInt
