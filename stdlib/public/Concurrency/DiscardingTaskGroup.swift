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

// ==== DiscardingTaskGroup ---------------------------------------------------

/// Starts a new scope that can contain a dynamic number of child tasks.
///
/// Unlike a ``TaskGroup``, the child tasks as well as their results are
/// discarded as soon as the tasks complete. This prevents the discarding
/// task group from accumulating many results waiting to be consumed, and is
/// best applied in situations where the result of a child task is some form
/// of side-effect.
///
/// A group waits for all of its child tasks
/// to complete before it returns. Even cancelled tasks must run until
/// completion before this function returns.
/// Cancelled child tasks cooperatively react to cancellation and attempt
/// to return as early as possible.
/// After this function returns, the task group is always empty.
///
/// It is not possible to explicitly await completion of child-tasks,
/// however the group will automatically await *all* child task completions
/// before returning from this function:
///
/// ```
/// await withDiscardingTaskGroup { group in
///   group.addTask { /* slow-task */ }
///   // slow-task executes...
/// }
/// // guaranteed that slow-task has completed and the group is empty & destroyed
/// ```
///
/// Task Group Cancellation
/// =======================
///
/// You can cancel a task group and all of its child tasks
/// by calling the ``TaskGroup/cancelAll()`` method on the task group,
/// or by canceling the task in which the group is running.
///
/// If you call `addTask(priority:operation:)` to create a new task in a canceled group,
/// that task is immediately canceled after creation.
/// Alternatively, you can call `asyncUnlessCancelled(priority:operation:)`,
/// which doesn't create the task if the group has already been canceled
/// Choosing between these two functions
/// lets you control how to react to cancellation within a group:
/// some child tasks need to run regardless of cancellation,
/// but other tasks are better not even being created
/// when you know they can't produce useful results.
///
/// Because the tasks you add to a group with this method are nonthrowing,
/// those tasks can't respond to cancellation by throwing `CancellationError`.
/// The tasks must handle cancellation in some other way,
/// such as returning the work completed so far, returning an empty result, or returning `nil`.
/// For tasks that need to handle cancellation by throwing an error,
/// use the `withThrowingDiscardingTaskGroup(returning:body:)` method instead.
///
/// - SeeAlso: ``withThrowingDiscardingTaskGroup(returning:body:)
@available(SwiftStdlib 5.8, *)
@inlinable
@_unsafeInheritExecutor
public func withDiscardingTaskGroup<GroupResult>(
  returning returnType: GroupResult.Type = GroupResult.self,
  body: (inout DiscardingTaskGroup) async -> GroupResult
) async -> GroupResult {
  #if compiler(>=5.5) && $BuiltinCreateTaskGroupWithFlags
  let flags = taskGroupCreateFlags(
    discardResults: true
  )

  let _group = Builtin.createTaskGroupWithFlags(flags, GroupResult.self)
  var group = DiscardingTaskGroup(group: _group)
  defer { Builtin.destroyTaskGroup(_group) }

  let result = await body(&group)

  try! await group.awaitAllRemainingTasks() // try!-safe, cannot throw since this is a non throwing group
  return result
  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}

/// A discarding group that contains dynamically created child tasks.
///
/// To create a discarding task group,
/// call the ``withDiscardingTaskGroup(returning:body:)`` method.
///
/// Don't use a task group from outside the task where you created it.
/// In most cases,
/// the Swift type system prevents a task group from escaping like that
/// because adding a child task to a task group is a mutating operation,
/// and mutation operations can't be performed
/// from a concurrent execution context like a child task.
///
/// ### Task execution order
/// Tasks added to a task group execute concurrently, and may be scheduled in
/// any order.
///
/// ### Discarding behavior
/// A discarding task group eagerly discards and releases its child tasks as
/// soon as they complete. This allows for the efficient releasing of memory used
/// by those tasks, which are not retained for future `next()` calls, as would
/// be the case with a ``TaskGroup``.
///
/// ### Cancellation behavior
/// A task group becomes cancelled in one of two ways: when ``cancelAll()`` is
/// invoked on it, or when the ``Task`` running this task group is cancelled.
///
/// Since a `TaskGroup` is a structured concurrency primitive, cancellation is
/// automatically propagated through all of its child-tasks (and their child
/// tasks).
///
/// A cancelled task group can still keep adding tasks, however they will start
/// being immediately cancelled, and may act accordingly to this. To avoid adding
/// new tasks to an already cancelled task group, use ``addTaskUnlessCancelled(priority:body:)``
/// rather than the plain ``addTask(priority:body:)`` which adds tasks unconditionally.
///
/// For information about the language-level concurrency model that `DiscardingTaskGroup` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
///
/// - SeeAlso: ``TaskGroup``
/// - SeeAlso: ``ThrowingTaskGroup``
/// - SeeAlso: ``ThrowingDiscardingTaskGroup``
@available(SwiftStdlib 5.8, *)
@frozen
public struct DiscardingTaskGroup {

  @usableFromInline
  internal let _group: Builtin.RawPointer

  // No public initializers
  @inlinable
  init(group: Builtin.RawPointer) {
    self._group = group
  }

  /// Await all the remaining tasks on this group.
  ///
  /// - Throws: The first error that was encountered by this group.
  @usableFromInline
  internal mutating func awaitAllRemainingTasks() async throws {
    let _: Void? = try await _taskGroupWaitAll(group: _group, bodyError: nil)
  }

  @_alwaysEmitIntoClient
  #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
  #endif
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) {
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

    // Create the task in this group.
    _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
  }

  @_alwaysEmitIntoClient
  #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
  #endif
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) -> Bool {
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
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

    // Create the task in this group.
    _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)

    return true
  }

  @_alwaysEmitIntoClient
  public mutating func addTask(
    operation: __owned @Sendable @escaping () async -> Void
  ) {
    let flags = taskCreateFlags(
      priority: nil, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true
    )

    // Create the task in this group.
    _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
  }

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
#endif
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    operation: __owned @Sendable @escaping () async -> Void
  ) -> Bool {
#if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    let flags = taskCreateFlags(
      priority: nil, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false
    )

    // Create the task in this group.
    _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)

    return true
#else
    fatalError("Unsupported Swift compiler")
#endif
  }

  public var isEmpty: Bool {
    _taskGroupIsEmpty(_group)
  }

  public func cancelAll() {
    _taskGroupCancelAll(group: _group)
  }

  public var isCancelled: Bool {
    return _taskGroupIsCancelled(group: _group)
  }
}

@available(SwiftStdlib 5.8, *)
@available(*, unavailable)
extension DiscardingTaskGroup: Sendable { }

// ==== ThrowingDiscardingTaskGroup -------------------------------------------

/// Starts a new scope that can contain a dynamic number of child tasks.
///
/// Unlike a ``ThrowingTaskGroup``, the child tasks as well as their results are
/// discarded as soon as the tasks complete. This prevents the discarding
/// task group from accumulating many results waiting to be consumed, and is
/// best applied in situations where the result of a child task is some form
/// of side-effect.
///
/// A group waits for all of its child tasks
/// to complete before it returns. Even cancelled tasks must run until
/// completion before this function returns.
/// Cancelled child tasks cooperatively react to cancellation and attempt
/// to return as early as possible.
/// After this function returns, the task group is always empty.
///
/// It is not possible to explicitly await completion of child-tasks,
/// however the group will automatically await *all* child task completions
/// before returning from this function:
///
/// ```
/// try await withThrowingDiscardingTaskGroup { group in
///   group.addTask { /* slow-task */ }
///   // slow-task executes...
/// }
/// // guaranteed that slow-task has completed and the group is empty & destroyed
/// ```
///
/// Task Group Cancellation
/// =======================
///
/// You can cancel a task group and all of its child tasks
/// by calling the ``TaskGroup/cancelAll()`` method on the task group,
/// or by canceling the task in which the group is running.
///
/// If you call `addTask(priority:operation:)` to create a new task in a canceled group,
/// that task is immediately canceled after creation.
/// Alternatively, you can call `asyncUnlessCancelled(priority:operation:)`,
/// which doesn't create the task if the group has already been canceled
/// Choosing between these two functions
/// lets you control how to react to cancellation within a group:
/// some child tasks need to run regardless of cancellation,
/// but other tasks are better not even being created
/// when you know they can't produce useful results.
///
/// Error Handling and Implicit Cancellation
/// ========================================
///
/// Since it is not possible to explicitly await individual task completions,
/// it is also not possible to "re-throw" an error thrown by one of the child
/// tasks using the same pattern as one would in a ``ThrowingTaskGroup``:
///
/// ```
/// // ThrowingTaskGroup, pattern not applicable to ThrowingDiscardingTaskGroup
/// try await withThrowingTaskGroup { group in
///   group.addTask { try boom() }
///   try await group.next() // re-throws "boom"
/// }
/// ```
///
/// Since discarding task groups don't have access to `next()`, this pattern
/// cannot be used.
/// Instead,
/// a *throwing discarding task group implicitly cancels itself whenever any
/// of its child tasks throws*.
///
/// The *first error* thrown inside such task group
/// is then retained and thrown
/// out of the `withThrowingDiscardingTaskGroup` method when it returns.
///
/// ```
/// try await withThrowingDiscardingTaskGroup() { group in
///   group.addTask { try boom(1) }
///   group.addTask { try boom(2, after: .seconds(5)) }
///   group.addTask { try boom(3, after: .seconds(5)) }
/// }
/// ```
///
///
///
/// Generally, this suits the typical use-cases of a
/// discarding task group well, however, if you wanted to prevent specific
/// errors from cancelling the group
///
///
///
///
/// Throwing an error in one of the child tasks of a task group
/// doesn't immediately cancel the other tasks in that group.
/// However,
/// throwing out of the `body` of the `withThrowingTaskGroup` method does cancel
/// the group, and all of its child tasks.
@available(SwiftStdlib 5.8, *)
@inlinable
@_unsafeInheritExecutor
public func withThrowingDiscardingTaskGroup<GroupResult>(
    returning returnType: GroupResult.Type = GroupResult.self,
    body: (inout ThrowingDiscardingTaskGroup<Error>) async throws -> GroupResult
) async throws -> GroupResult {
  #if compiler(>=5.5) && $BuiltinCreateTaskGroupWithFlags
  let flags = taskGroupCreateFlags(
      discardResults: true
  )

  let _group = Builtin.createTaskGroupWithFlags(flags, GroupResult.self)
  var group = ThrowingDiscardingTaskGroup<Error>(group: _group)
  defer { Builtin.destroyTaskGroup(_group) }

  let result: GroupResult
  do {
    result = try await body(&group)
  } catch {
    group.cancelAll()

    try await group.awaitAllRemainingTasks(bodyError: error)

    throw error
  }

  try await group.awaitAllRemainingTasks(bodyError: nil)

  return result
  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}


/// A throwing discarding group that contains dynamically created child tasks.
///
/// To create a discarding task group,
/// call the ``withDiscardingTaskGroup(returning:body:)`` method.
///
/// Don't use a task group from outside the task where you created it.
/// In most cases,
/// the Swift type system prevents a task group from escaping like that
/// because adding a child task to a task group is a mutating operation,
/// and mutation operations can't be performed
/// from a concurrent execution context like a child task.
///
/// ### Task execution order
/// Tasks added to a task group execute concurrently, and may be scheduled in
/// any order.
///
/// ### Discarding behavior
/// A discarding task group eagerly discards and releases its child tasks as
/// soon as they complete. This allows for the efficient releasing of memory used
/// by those tasks, which are not retained for future `next()` calls, as would
/// be the case with a ``TaskGroup``.
///
/// ### Cancellation behavior
/// A task group becomes cancelled in one of two ways: when ``cancelAll()`` is
/// invoked on it, or when the ``Task`` running this task group is cancelled.
///
/// Since a `TaskGroup` is a structured concurrency primitive, cancellation is
/// automatically propagated through all of its child-tasks (and their child
/// tasks).
///
/// A cancelled task group can still keep adding tasks, however they will start
/// being immediately cancelled, and may act accordingly to this. To avoid adding
/// new tasks to an already cancelled task group, use ``addTaskUnlessCancelled(priority:body:)``
/// rather than the plain ``addTask(priority:body:)`` which adds tasks unconditionally.
///
/// For information about the language-level concurrency model that `DiscardingTaskGroup` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
///
/// - SeeAlso: ``TaskGroup``
/// - SeeAlso: ``ThrowingTaskGroup``
/// - SeeAlso: ``DiscardingTaskGroup``
@available(SwiftStdlib 5.8, *)
@frozen
public struct ThrowingDiscardingTaskGroup<Failure: Error> {

  @usableFromInline
  internal let _group: Builtin.RawPointer

  // No public initializers
  @inlinable
  init(group: Builtin.RawPointer) {
    self._group = group
  }

  /// Await all the remaining tasks on this group.
  @usableFromInline
  internal mutating func awaitAllRemainingTasks(bodyError: Error?) async throws {
    let _: Void? = try await _taskGroupWaitAll(group: _group, bodyError: bodyError)
  }

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
#endif
  @_alwaysEmitIntoClient
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Void
  ) {
#if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true
    )

    // Create the task in this group.
    _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
#else
    fatalError("Unsupported Swift compiler")
#endif
  }

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
#endif
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Void
  ) -> Bool {
#if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false
    )

    // Create the task in this group.
    _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)

    return true
#else
    fatalError("Unsupported Swift compiler")
#endif
  }

  public var isEmpty: Bool {
    _taskGroupIsEmpty(_group)
  }

  public func cancelAll() {
    _taskGroupCancelAll(group: _group)
  }

  public var isCancelled: Bool {
    return _taskGroupIsCancelled(group: _group)
  }
}

@available(SwiftStdlib 5.8, *)
@available(*, unavailable)
extension ThrowingDiscardingTaskGroup: Sendable { }

// ==== -----------------------------------------------------------------------
// MARK: Runtime functions

/// Always returns `nil`.
@available(SwiftStdlib 5.8, *)
@usableFromInline
@discardableResult
@_silgen_name("swift_taskGroup_waitAll")
func _taskGroupWaitAll<T>(
    group: Builtin.RawPointer,
    bodyError: Error?
) async throws -> T?
