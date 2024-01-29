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

// None of _TaskExecutor APIs are available in task-to-thread concurrency model.
#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

@_unavailableInEmbedded
@available(SwiftStdlib 9999, *)
extension TaskGroup {
  /// Adds a child task to the group and enqueue it on the specified executor.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addTask` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func _addTask(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    guard let taskExecutor else {
      return self.addTask(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }

  /// Adds a child task to the group and enqueue it on the specified executor, unless the group has been canceled.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `_addTaskUnlessCancelled()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func _addTaskUnlessCancelled(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    guard let taskExecutor else {
      return self.addTaskUnlessCancelled(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

// ==== ThrowingTaskGroup ------------------------------------------------------------------------------------------------------

@_unavailableInEmbedded
@available(SwiftStdlib 9999, *)
extension ThrowingTaskGroup {
  /// Adds a child task to the group and enqueue it on the specified executor.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `_addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func _addTask(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    guard let taskExecutor else {
      return self.addTask(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }

  /// Adds a child task to the group and enqueue it on the specified executor, unless the group has been canceled.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func _addTaskUnlessCancelled(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    guard let taskExecutor else {
      return self.addTaskUnlessCancelled(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

// ==== DiscardingTaskGroup ------------------------------------------------------------------------------------------------------

@_unavailableInEmbedded
@available(SwiftStdlib 9999, *)
extension DiscardingTaskGroup {
  /// Adds a child task to the group and enqueue it on the specified executor.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `_addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func _addTask(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) {
    guard let taskExecutor else {
      return self.addTask(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncDiscardingTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncDiscardingTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }

  /// Adds a child task to the group and set it up with the passed in task executor preference,
  /// unless the group has been canceled.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `_addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func _addTaskUnlessCancelled(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) -> Bool {
    guard let taskExecutor else {
      return self.addTaskUnlessCancelled(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncDiscardingTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false, isDiscardingTask: true
    )

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncDiscardingTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

// ==== ThrowingDiscardingTaskGroup ------------------------------------------------------------------------------------------------------

@_unavailableInEmbedded
@available(SwiftStdlib 9999, *)
extension ThrowingDiscardingTaskGroup {
  /// Adds a child task to the group and set it up with the passed in task executor preference.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `_addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func _addTask(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Void
  ) {
    guard let taskExecutor else {
      return self.addTask(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncDiscardingTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncDiscardingTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }

  /// Adds a child task to the group and set it up with the passed in task executor preference,
  /// unless the group has been canceled.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `_addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func _addTaskUnlessCancelled(
    executorPreference taskExecutor: (any _TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Void
  ) -> Bool {
    guard let taskExecutor else {
      return self.addTaskUnlessCancelled(priority: priority, operation: operation)
    }
    #if $BuiltinCreateAsyncDiscardingTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false, isDiscardingTask: true
    )

    let executorBuiltin: Builtin.Executor =
      taskExecutor.asUnownedTaskExecutor().executor

    // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncDiscardingTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

#endif
