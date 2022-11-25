//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


import Swift
@_implementationOnly import _SwiftConcurrencyShims
import Darwin

// ==== TaskPool --------------------------------------------------------------

/// Starts a new scope that can contain a dynamic number of child tasks.
/// 
/// A `TaskPool` is similar to a ``TaskGroup``, however its usage is fairly specialized for
/// submitting work using child tasks, where the results of those child tasks do not need to be collected.
/// 
/// A task pool cannot be iterated over and its child tasks cannot be awaited on explicitly.
/// Task pool tasks are immediately removed from the pool as soon as they complete, 
/// this is the primary difference from a task pool which stores results (and thus retains the results),
/// until they are consumed.
///
/// Similarly to a `TaskGroup` a `TaskPool` awaits all tasks that are submitted to it before returning 
/// from the `withTaskPool` call.
///
/// Task Group Cancellation
/// =======================
///
/// You can cancel a task pool and all of its child tasks
/// by calling the `cancelAll()` method on the task pool,
/// or by canceling the task in which the pool is running.
///
/// If you call `addTask(priority:operation:)` to create a new task in a canceled pool,
/// that task is immediately canceled after creation.
/// Alternatively, you can call `asyncUnlessCancelled(priority:operation:)`,
/// which doesn't create the task if the pool has already been canceled
/// Choosing between these two functions
/// lets you control how to react to cancellation within a pool:
/// some child tasks need to run regardless of cancellation,
/// but other tasks are better not even being created
/// when you know they can't produce useful results.
@available(SwiftStdlib 5.7, *)
@_unsafeInheritExecutor
@inlinable
public func withTaskPool<PoolResult>(
  returning returnType: PoolResult.Type = PoolResult.self,
  body: (inout TaskPool) async throws -> PoolResult
) async rethrows -> PoolResult {
  let _pool = Builtin.createTaskPool(Void.self)
  var pool = TaskPool(pool: _pool)

  // Run the withTaskPool body.
  do {
    let result = try await body(&pool)

    await pool.awaitAllRemainingTasks()
    Builtin.destroyTaskPool(_pool)

    return result
  } catch {
//    pool.cancelAll()
    await pool.awaitAllRemainingTasks()
    Builtin.destroyTaskPool(_pool)

    throw error
  }
}

/// A pool that contains dynamically created child tasks.
///
/// To create a task pool,
/// call the `withTaskPool(returning:body:)` method.
///
/// Don't use a task pool from outside the task where you created it.
/// In most cases,
/// the Swift type system prevents a task pool from escaping like that
/// because adding a child task to a task pool is a mutating operation,
/// and mutation operations can't be performed
/// from a concurrent execution context like a child task.
///
/// For information about the language-level concurrency model that `TaskPool` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
///
@available(SwiftStdlib 5.1, *)
@frozen
public struct TaskPool {

  @usableFromInline
  internal let _pool: Builtin.RawPointer

  // No public initializers
  @inlinable
  init(pool: Builtin.RawPointer) {
    self._pool = pool
  }

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  /// Adds a child task to the pool.
  ///
  /// - Parameters:
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the pool.
  ///   - operation: The operation to execute as part of the task pool.
  @_alwaysEmitIntoClient
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) {
#if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true
    )
#else
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true
    )
#endif

    // Create the task in this pool.
    _ = Builtin.createAsyncTaskInPool(flags, _pool, operation)
#else
    fatalError("Unsupported Swift compiler")
#endif
  }

  /// Adds a child task to the pool, unless the pool has been canceled.
  ///
  /// - Parameters:
  ///   - overridingPriority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the pool.
  ///   - operation: The operation to execute as part of the task pool.
  /// - Returns: `true` if the child task was added to the pool;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) -> Bool {
#if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
    let canAdd = _taskPoolAddPendingTask(pool: _pool, unconditionally: false)

    guard canAdd else {
      // the pool is cancelled and is not accepting any new work
      return false
    }
#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: false
    )
#else
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false
    )
#endif

    // Create the task in this pool.
    _ = Builtin.createAsyncTaskInGroup(flags, _pool, operation)

    return true
#else
    fatalError("Unsupported Swift compiler")
#endif
  }
#else
  @available(SwiftStdlib 5.7, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  /// Adds a child task to the pool.
  ///
  /// - Parameters:
  ///   - operation: The operation to execute as part of the task pool.
  @_alwaysEmitIntoClient
  public mutating func addTask(
    operation: __owned @Sendable @escaping () async -> Void
  ) {
#if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
    let flags = taskCreateFlags(
      priority: nil, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true
    )

    // Create the task in this pool.
    _ = Builtin.createAsyncTaskInGroup(flags, _pool, operation)
#else
    fatalError("Unsupported Swift compiler")
#endif
  }

  @available(SwiftStdlib 5.7, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) -> Bool {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  /// Adds a child task to the pool, unless the pool has been canceled.
  ///
  /// - Parameters:
  ///   - operation: The operation to execute as part of the task pool.
  /// - Returns: `true` if the child task was added to the pool;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    operation: __owned @Sendable @escaping () async -> Void
  ) -> Bool {
#if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
    let canAdd = _taskPoolAddPendingTask(pool: _pool, unconditionally: false)

    guard canAdd else {
      // the pool is cancelled and is not accepting any new work
      return false
    }

    let flags = taskCreateFlags(
      priority: nil, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false
    )

    // Create the task in this pool.
    _ = Builtin.createAsyncTaskInPool(flags, _pool, operation)

    return true
#else
    fatalError("Unsupported Swift compiler")
#endif
  }
#endif

  /// Await all of the remaining tasks on this pool.
  @usableFromInline
  internal mutating func awaitAllRemainingTasks() async {
    while let _: Void = try? await _taskPoolWaitAll(pool: _pool) {}
  }

  /// Wait for all of the pool's remaining tasks to complete.
  @_alwaysEmitIntoClient
  public mutating func waitForAll() async {
    await awaitAllRemainingTasks()
  }

  /// A Boolean value that indicates whether the pool has any remaining tasks.
  ///
  /// At the start of the body of a `withTaskPool(of:returning:body:)` call,
  /// the task pool is always empty.
  /// It`s guaranteed to be empty when returning from that body
  /// because a task pool waits for all child tasks to complete before returning.
  ///
  /// - Returns: `true` if the pool has no pending tasks; otherwise `false`.
  public var isEmpty: Bool {
    _taskPoolIsEmpty(_pool)
  }

  /// Cancel all of the remaining tasks in the pool.
  ///
  /// After cancellation,
  /// any new results from the tasks in this pool
  /// are silently discarded.
  ///
  /// If you add a task to a pool after canceling the pool,
  /// that task is canceled immediately after being added to the pool.
  ///
  /// This method can only be called by the parent task that created the task
  /// pool.
  ///
  /// - SeeAlso: `Task.isCancelled`
  /// - SeeAlso: `TaskPool.isCancelled`
  public func cancelAll() {
    _taskPoolCancelAll(pool: _pool)
  }

  /// A Boolean value that indicates whether the pool was canceled.
  ///
  /// To cancel a pool, call the `TaskPool.cancelAll()` method.
  ///
  /// If the task that's currently running this pool is canceled,
  /// the pool is also implicitly canceled,
  /// which is also reflected in this property's value.
  public var isCancelled: Bool {
    return _taskPoolIsCancelled(pool: _pool)
  }
}

@available(SwiftStdlib 5.7, *)
@available(*, unavailable)
extension TaskPool: Sendable { }

/// ==== -----------------------------------------------------------------------

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_taskPool_destroy")
func _taskPoolDestroy(pool: __owned Builtin.RawPointer)

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_taskPool_addPending")
@usableFromInline
func _taskPoolAddPendingTask(
  pool: Builtin.RawPointer,
  unconditionally: Bool
) -> Bool

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_taskPool_cancelAll")
func _taskPoolCancelAll(pool: Builtin.RawPointer)

/// Checks ONLY if the pool was specifically canceled.
/// The task itself being canceled must be checked separately.
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_taskPool_isCancelled")
func _taskPoolIsCancelled(pool: Builtin.RawPointer) -> Bool

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_taskPool_waitAll")
func _taskPoolWaitAll<T>(pool: Builtin.RawPointer) async throws -> T?

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_hasTaskPoolStatusRecord")
func _taskHasTaskPoolStatusRecord() -> Bool

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_taskPool_isEmpty")
func _taskPoolIsEmpty(
  _ pool: Builtin.RawPointer
) -> Bool
