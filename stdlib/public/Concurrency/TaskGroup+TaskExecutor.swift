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

// None of TaskExecutor APIs are available in task-to-thread concurrency model.
#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

@available(SwiftStdlib 9999, *)
extension TaskGroup {
  /// Adds a child task to the group and enqueue it on the specified executor.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func addTask(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)

    let executorBuiltin: Builtin.Executor =
      if let taskExecutor {
        taskExecutor.asUnownedTaskExecutor().executor
      } else {
        _getUndefinedTaskExecutor()
      }

//    if let taskExecutor {
//      let executorBuiltin: Builtin.Executor =
//        taskExecutor.asUnownedTaskExecutor().executor
//
      // Create the task in this group with an executor preference.
    _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
//    } else {
//      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
//      // Create the task in this group without executor preference.
//      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
//    }
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
  ///                   invoke `addTaskUnlessCancelled()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    if let taskExecutor {
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
      // Create the task in this group without executor preference.
      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }

    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

// ==== ThrowingTaskGroup ------------------------------------------------------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension ThrowingTaskGroup {
  /// Adds a child task to the group and enqueue it on the specified executor.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func addTask(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)

    if let taskExecutor {
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
      // Create the task in this group without executor preference.
      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }
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
  ///                   invoke `addTaskUnlessCancelled()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    if let taskExecutor {
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
      // Create the task in this group without executor preference.
      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }

    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

// ==== DiscardingTaskGroup ------------------------------------------------------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension DiscardingTaskGroup {
  /// Adds a child task to the group and enqueue it on the specified executor.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func addTask(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Void
  ) {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)

    if let taskExecutor {
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
      // Create the task in this group without executor preference.
      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }
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
  ///                   invoke `addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Void
  ) -> Bool {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: false, isDiscardingTask: true
    )

    if let taskExecutor {
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
      // Create the task in this group without executor preference.
      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }

    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

// ==== ThrowingDiscardingTaskGroup ------------------------------------------------------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension ThrowingDiscardingTaskGroup {
  /// Adds a child task to the group and set it up with the passed in task executor preference.
  ///
  /// - Parameters:
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///                   If `nil` is passed explicitly, tht parent task's executor preference (if any),
  ///                   will be ignored. In order to inherit the parent task's executor preference
  ///                   invoke `addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func addTask(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Void
  ) {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)

    if let taskExecutor {
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
      // Create the task in this group without executor preference.
      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }
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
  ///                   invoke `addTask()` without passing a value to the `taskExecutor` parameter,
  ///                   and it will be inherited automatically.
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    on taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Void
  ) -> Bool {
    #if $BuiltinCreateAsyncTaskInGroupWithExecutor
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: false, isDiscardingTask: true
    )

    if let taskExecutor {
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // FIXME: DISABLE THE TASK PREFERENCE (PASS NIL?)
      // Create the task in this group without executor preference.
      fatalError("NOT YET") // _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }

    return true
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

#endif