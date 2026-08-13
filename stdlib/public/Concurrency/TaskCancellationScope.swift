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
/// ## Cancellation Semantics
/// Effects of cancelling a scope are semantically the same as-if cancelling an entire task,
/// in the sense that `Task.isCancelled` and cancellation handlers are triggered within the scope.
///
/// The scope's effects on `Task.isCancelled` are strictly contained for the duration
/// of executing the `operation`; the enclosing task's own cancellation state is unchanged.
///
/// This is primarily designed to be used as an optimization in order to avoid creating new child tasks
/// as a "cancellation scope", and it is a goal to have the cancellation semantics of a scope be equivalent
/// to those of those provided by creating a child task.
@safe
@frozen
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
public struct TaskCancellationScope: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _record: UnsafeRawPointer

  @usableFromInline
  @_lifetime(immortal)
  internal init(record: UnsafeRawPointer) {
    unsafe self._record = record
  }

  /// Cancel this scope.
  ///
  /// Causes `Task.isCancelled` to return `true` for code running inside the
  /// scope's `operation` and fires any `withTaskCancellationHandler` handlers
  /// installed while the scope was active (this is what allows operations
  /// like `Task.sleep(for:)` inside the scope to return early).
  ///
  /// It does _not_ set the enclosing task's own cancellation flag, and it
  /// does _not_ invoke handlers installed outside the scope's dynamic extent.
  ///
  /// Cancellation cascades to nested inner `__withTaskCancellationScope`
  /// records and to structured child tasks (`async let`, `TaskGroup`)
  /// spawned inside the scope. This is not optional: structured children
  /// can never escape the scope that spawned them - the scope's `operation`
  /// cannot return until they've completed - so leaving them uncancelled
  /// while their parent scope is cancelled would be observably wrong. This
  /// applies both to children already running when `cancel()` is called
  /// (cascaded immediately by walking the status-record chain) and to
  /// children spawned afterwards (cancelled at creation, since their parent
  /// task already has a cancelled scope on its chain).
  ///
  /// Multiple calls to `cancel(reason:)` are safe: first-cancel-wins, so
  /// subsequent calls are no-ops and the originally-recorded reason is
  /// preserved.
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
  /// Checks only the cancellation status of this scope, and not the enclosing task.
  ///
  /// If checking from within a task and the scope is cancelled, the task will also report being cancelled.
  ///
  /// - Returns: `true` if this scope has been cancelled (via
  ///   ``cancel(reason:)`` or by an outer scope cascading cancellation
  ///   inward), `false` otherwise. Once `true`, remains `true` for the
  ///   scope's lifetime.
  @export(implementation)
  public var isCancelled: Bool {
    unsafe _taskCancellationScopeIsCancelled(record: _record)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: __withTaskCancellationScope

/// Executes an operation inside a cancellation scope.
///
/// The `operation` closure receives a ``TaskCancellationScope`` handle
/// which can be used to cancel the scope.
///
/// Cancelling the scope causes `Task.isCancelled` to return `true` for code
/// executing inside `operation`, and triggers any other effects task cancellation does.
///
/// ## Cancellation Semantics
/// Cancelling a scope is semantically equivalent to cancelling as-if the scope were its own task.
///
/// The scope's effects on `Task.isCancelled` are strictly contained for the duration
/// of executing the `operation`; the enclosing task's own cancellation state is unchanged.
///
/// Structured children (`async let`, `TaskGroup`) spawned inside `operation`
/// can never escape the scope - `operation` cannot return until they've
/// completed - so cancelling the scope also cancels them, whether they were
/// already running at the time of cancellation or are spawned afterwards.
/// See ``TaskCancellationScope/cancel(reason:)`` for details.
///
/// - Parameter operation: The work to perform. Receives a scope handle that
///   can be used to trigger local cancellation. The handle must not
///   escape `operation`.
/// - Returns: the result returned by the `operation` closure.
/// - Throws: if an error is thrown by the `operation` closure.
@_spi(Concurrency)
@export(implementation)
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending) func __withTaskCancellationScope<Return, Failure>(
  _ operation: nonisolated(nonsending) (borrowing TaskCancellationScope) async throws(Failure) -> Return
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
  record: TaskCancellationScopeRecordUnsafeRawPointer,
  flags: UInt)

@usableFromInline
@available(StdlibDeploymentTarget 6.5, *)
@_silgen_name("swift_task_cancellationScopeIsCancelled")
internal func _taskCancellationScopeIsCancelled(record: TaskCancellationScopeRecordUnsafeRawPointer) -> Bool
