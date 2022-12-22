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

@available(SwiftStdlib 5.8, *)
@inlinable
@_unsafeInheritExecutor
public func withDiscardingTaskGroup<GroupResult>(
  returning returnType: GroupResult.Type = GroupResult.self,
  body: (inout DiscardingTaskGroup) async -> GroupResult
) async -> GroupResult {
  #if compiler(>=5.5) && $BuiltinTaskGroupWithArgument
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
    let _: Void? = try await _taskGroupWaitAll(group: _group)
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

@available(SwiftStdlib 5.8, *)
@inlinable
@_unsafeInheritExecutor
public func withThrowingDiscardingTaskGroup<GroupResult>(
    returning returnType: GroupResult.Type = GroupResult.self,
    body: (inout ThrowingDiscardingTaskGroup<Error>) async throws -> GroupResult
) async throws -> GroupResult {
  #if compiler(>=5.5) && $BuiltinTaskGroupWithArgument
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

    try await group.awaitAllRemainingTasks()

    throw error
  }

  try await group.awaitAllRemainingTasks()

  return result
  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}

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
  internal mutating func awaitAllRemainingTasks() async throws {
    let _: Void? = try await _taskGroupWaitAll(group: _group)
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
    group: Builtin.RawPointer
) async throws -> T?

@available(SwiftStdlib 5.8, *) // FIXME: remove
@_silgen_name("swift_taskGroup_isDiscardingResults")
func _taskGroupIsDiscardingResults(group: Builtin.RawPointer) -> Bool