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
// MARK: TaskCancellationScope

/// A handle representing a scoped, independently-cancellable region within a
/// task, distinct from whole-task cancellation.
///
/// Unlike `Task.cancel()`, which flips the enclosing task's own `isCancelled`
/// state, `TaskCancellationScope.cancel()` only affects code running inside the
/// `__withTaskCancellationScope { scope in ... }` body. Structured child tasks
/// spawned inside the scope (via `TaskGroup` or `async let`) are NOT
/// automatically cancelled through this mechanism.
///
/// This type is `~Copyable` and `~Escapable`: the scope handle exists only
/// for the duration of `__withTaskCancellationScope`'s body and cannot leave
/// that body. Cancel from an external task by capturing the scope in a
/// closure whose lifetime the caller has proven does not outlive the body
/// (typically via a synchronous timer job whose disarm-on-scope-exit is
/// enforced by the executor's synchronous `cancel(_:)` contract).
///
/// This is a low-level building block for higher-level primitives such as
/// `withDeadline`; direct client use is discouraged and gated on
/// `@_spi(Concurrency)`.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
@safe
@frozen
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
  /// Setting the scope's cancellation flag causes `Task.isCancelled` to
  /// return `true` for code running inside `__withTaskCancellationScope`'s body
  /// (or its non-child callees), and fires any `withTaskCancellationHandler`
  /// handlers installed while the scope was active - this is what allows
  /// operations like `Task.sleep(for:)` inside the scope to return early.
  ///
  /// It does NOT set the enclosing task's own cancellation flag, and it
  /// does NOT invoke handlers installed outside the scope's dynamic extent.
  ///
  /// Each `CancellationNotificationStatusRecord` handler fires at most
  /// once across scope-cancel and whole-task-cancel combined, so a
  /// subsequent whole-task cancellation will not double-fire handlers that
  /// were already invoked by scope cancellation.
  ///
  /// Multiple calls to `cancel()` are safe; subsequent calls are no-ops.
  public func cancel() {
#if $BuiltinConcurrencyStackNesting
    unsafe Builtin.taskCancellationScopeCancel(record: _record)
#else
    unsafe _taskCancelTaskCancellationScope(record: _record)
#endif
  }
}

// ==== -----------------------------------------------------------------------
// MARK: __withTaskCancellationScope

/// Executes an operation inside a fresh cancellation scope.
///
/// The `body` closure receives a `TaskCancellationScope` handle. Calling
/// `scope.cancel()` (from `body` itself, or from any other concurrency
/// context that has captured the handle for the duration of `body`) causes
/// `Task.isCancelled` to return `true` for code executing inside `body`,
/// and wakes up any `withTaskCancellationHandler`-based operations
/// (including `Task.sleep`) installed inside the scope.
///
/// The scope's effects are strictly local to `body`. The enclosing task's
/// own cancellation state is unchanged, and cancellation does not propagate
/// into structured children (`TaskGroup` / `async let`) spawned inside the
/// scope.
///
/// This is a low-level primitive intended for building higher-level control
/// abstractions such as `withDeadline`. It is gated on `@_spi(Concurrency)`
/// and is not part of the general public API.
///
/// - Parameter body: The operation to perform. Receives a scope handle that
///   can be used to trigger local cancellation. The handle must not
///   escape `body`.
/// - Returns: Whatever `body` returns.
/// - Throws: Whatever `body` throws.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending)
func __withTaskCancellationScope<Return, Failure>(
  _ body: nonisolated(nonsending) (borrowing TaskCancellationScope) async throws(Failure) -> Return
) async throws(Failure) -> Return where Return: ~Copyable, Failure: Error {
#if $BuiltinConcurrencyStackNesting
  let record = unsafe Builtin.taskCancellationScopePush()
  defer { unsafe Builtin.taskCancellationScopePop(record: record) }
#else
  let record = unsafe _taskPushTaskCancellationScope()
  defer { unsafe _taskPopTaskCancellationScope(record: record) }
#endif
  let scope = unsafe TaskCancellationScope(record: record)
  return try await body(scope)
}

// ==== -----------------------------------------------------------------------
// MARK: Runtime functions

@available(StdlibDeploymentTarget 6.5, *)
@usableFromInline
@_silgen_name("swift_task_pushCancellationScope")
internal func _taskPushTaskCancellationScope() -> UnsafeRawPointer /*TaskCancellationScopeRecord*/

@available(StdlibDeploymentTarget 6.5, *)
@usableFromInline
@_silgen_name("swift_task_popCancellationScope")
internal func _taskPopTaskCancellationScope(record: UnsafeRawPointer /*TaskCancellationScopeRecord*/)

@available(StdlibDeploymentTarget 6.5, *)
@usableFromInline
@_silgen_name("swift_task_cancelCancellationScope")
internal func _taskCancelTaskCancellationScope(record: UnsafeRawPointer /*TaskCancellationScopeRecord*/)
