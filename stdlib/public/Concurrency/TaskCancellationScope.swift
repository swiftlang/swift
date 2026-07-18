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
/// state, `TaskCancellationScope.cancel()` only affects code running inside
/// the `__withTaskCancellationScope { scope in ... }` operation. Structured
/// child tasks spawned inside the scope (via `TaskGroup` or `async let`)
/// cascade: they observe `Task.isCancelled == true` just as they would inside
/// a cancelled parent task.
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
  /// spawned inside the scope, mirroring "as-if child task" semantics.
  ///
  /// Multiple calls to `cancel()` are safe; subsequent calls are no-ops.
  public func cancel() {
#if $BuiltinConcurrencyStackNesting
    unsafe Builtin.taskCancellationScopeCancel(record: _record)
#else
    unsafe _taskCancelTaskCancellationScope(record: _record)
#endif
  }

  /// Whether this scope has been cancelled.
  ///
  /// Reads the scope record's own atomic cancellation flag directly, so
  /// this ignores whether the enclosing task has a whole-task cancellation
  /// or any surrounding cancellation shield.
  public var isCancelled: Bool {
    unsafe _taskCancellationScopeIsCancelled(record: _record)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: __withTaskCancellationScope

/// Executes an operation inside a fresh cancellation scope.
///
/// The `operation` closure receives a `TaskCancellationScope` handle. Calling
/// `scope.cancel()` causes `Task.isCancelled` to return `true` for code
/// executing inside `operation`, and wakes up any
/// `withTaskCancellationHandler`-based operations (including `Task.sleep`)
/// installed inside the scope.
///
/// The scope's effects on `Task.isCancelled` are strictly local to
/// `operation`; the enclosing task's own cancellation state is unchanged.
/// Structured children (`TaskGroup` / `async let`) spawned inside `operation`
/// cascade when the scope is cancelled.
///
/// This is a low-level primitive intended for building higher-level control
/// abstractions such as `withDeadline`. It is gated on `@_spi(Concurrency)`
/// and is not part of the general public API.
///
/// - Parameter operation: The work to perform. Receives a scope handle that
///   can be used to trigger local cancellation. The handle must not
///   escape `operation`.
/// - Returns: Whatever `operation` returns.
/// - Throws: Whatever `operation` throws.
@_spi(Concurrency)
@available(StdlibDeploymentTarget 6.5, *)
public nonisolated(nonsending)
func __withTaskCancellationScope<Return, Failure>(
  _ operation: nonisolated(nonsending) (borrowing TaskCancellationScope) async throws(Failure) -> Return
) async throws(Failure) -> Return where Return: ~Copyable, Failure: Error {
#if $BuiltinConcurrencyStackNesting
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

@available(StdlibDeploymentTarget 6.5, *)
@usableFromInline
@_silgen_name("swift_task_cancelCancellationScopeWithReason")
internal func _taskCancelTaskCancellationScopeWithReason(
  record: UnsafeRawPointer /*TaskCancellationScopeRecord*/,
  reason: UInt)

@available(StdlibDeploymentTarget 6.5, *)
@usableFromInline
@_silgen_name("swift_task_cancellationScopeIsCancelled")
internal func _taskCancellationScopeIsCancelled(record: UnsafeRawPointer /*TaskCancellationScopeRecord*/) -> Bool
