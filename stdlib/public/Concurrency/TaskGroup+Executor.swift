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

// ==== TaskGroup ------------------------------------------------------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension TaskGroup {
  // FIXME: SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
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
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskInGroup
    #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)
    #else
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)
    #endif

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

  // FIXME: addTaskUnlessCancelled
}

// ==== ThrowingTaskGroup ------------------------------------------------------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension ThrowingTaskGroup {
  // FIXME: SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
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
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskInGroup
    #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)
    #else
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false)
    #endif

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

  // FIXME: addTaskUnlessCancelled
}

// ==== DiscardingTaskGroup ------------------------------------------------------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension DiscardingTaskGroup {
  // FIXME: SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
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
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskInGroup
    #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)
    #else
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)
    #endif

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

  // FIXME: addTaskUnlessCancelled
}

// ==== ThrowingDiscardingTaskGroup ------------------------------------------------------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension ThrowingDiscardingTaskGroup {
  // FIXME: SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
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
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskInGroup
    #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: false,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)
    #else
    let flags = taskCreateFlags(
      priority: priority, isChildTask: true, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true)
    #endif

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

  // FIXME: addTaskUnlessCancelled
}