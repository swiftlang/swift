//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import SwiftConcurrencyInternalShims

// ==== Task.immediate(Detached) ---------------------------------------------------------

@available(SwiftStdlib 6.2, *)
extension Task { // throwing Failure error type

  /// Create and immediately start running a new task in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to a task created using
  /// the ``Task/init`` initializer.
  ///
  /// - Parameters:
  ///   - name: The high-level human-readable name given for this task
  ///   - priority: The priority of the task.
  ///     Pass `nil` to use the ``Task/basePriority`` of the current task (if there is one).
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to no preference,
  ///      and effectively means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: the operation to be run immediately upon entering the task.
  /// - Returns: A reference to the unstructured task which may be awaited on.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @discardableResult
  public static func immediate(
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_implicitSelfCapture @_inheritActorContext(always) operation: sending @isolated(any) @escaping () async throws(Failure)  -> Success
  ) -> Task<Success, Failure> {

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    // Determine if we're switching isolation dynamically.
    // If not, we can run the task synchronously and therefore MUST NOT "enqueue" it.
    let flagsMustNotCrash: UInt64 = 0
    let canRunSynchronously: Bool =
    if let builtinSerialExecutor {
      _taskIsCurrentExecutor(executor: builtinSerialExecutor, flags: flagsMustNotCrash)
    } else {
      true // if there is no target executor, we can run synchronously
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: !canRunSynchronously,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: true
    )

    var task: Builtin.NativeObject?
    #if $BuiltinCreateAsyncTaskName
    if let name {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
      #else // no $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
      #endif // $BuiltinCreateAsyncTaskOwnedTaskExecutor
    } // let name
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil {
      assert(name == nil)
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        operation: operation).0
    }

    if canRunSynchronously {
      _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)
    }
    return Task<Success, Failure>(task!)
  }
}

@available(SwiftStdlib 6.2, *)
extension Task { // throwing Failure error type

  /// Create and immediately start running a new detached task in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to a task created using
  /// the ``Task/detached`` function.
  ///
  /// - Parameters:
  ///   - name: The high-level human-readable name given for this task
  ///   - priority: The priority of the task.
  ///     Pass `nil` to use the ``Task/basePriority`` of the current task (if there is one).
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to no preference,
  ///      and effectively means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: the operation to be run immediately upon entering the task.
  /// - Returns: A reference to the unstructured task which may be awaited on.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @discardableResult
  public static func immediateDetached(
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_implicitSelfCapture @_inheritActorContext(always) operation: sending @isolated(any) @escaping () async throws(Failure)  -> Success
  ) -> Task<Success, Failure> {

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    // Determine if we're switching isolation dynamically.
    // If not, we can run the task synchronously and therefore MUST NOT "enqueue" it.
    let flagsMustNotCrash: UInt64 = 0
    let canRunSynchronously: Bool =
    if let builtinSerialExecutor {
      _taskIsCurrentExecutor(executor: builtinSerialExecutor, flags: flagsMustNotCrash)
    } else {
      true // if there is no target executor, we can run synchronously
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: !canRunSynchronously,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: true
    )

    var task: Builtin.NativeObject?
    #if $BuiltinCreateAsyncTaskName
    if let name {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
      #else // no $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
      #endif // $BuiltinCreateAsyncTaskOwnedTaskExecutor
    } // let name
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil {
      assert(name == nil)
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        operation: operation).0
    }

    if canRunSynchronously {
      _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)
    }
    return Task<Success, Failure>(task!)
  }
}

@available(SwiftStdlib 6.2, *)
extension TaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``TaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTask` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTask( // in TaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async -> ChildTaskResult
  ) {

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

  }
}

@available(SwiftStdlib 6.2, *)
extension TaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``TaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTaskUnlessCancelled` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTaskUnlessCancelled( // in TaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async -> ChildTaskResult
  ) -> Bool {

    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

    return true // task successfully enqueued
  }
}

@available(SwiftStdlib 6.2, *)
extension ThrowingTaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``ThrowingTaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTask` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTask( // in ThrowingTaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async throws -> ChildTaskResult
  ) {

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

  }
}

@available(SwiftStdlib 6.2, *)
extension ThrowingTaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``ThrowingTaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTaskUnlessCancelled` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTaskUnlessCancelled( // in ThrowingTaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async throws -> ChildTaskResult
  ) -> Bool {

    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

    return true // task successfully enqueued
  }
}

@available(SwiftStdlib 6.2, *)
extension DiscardingTaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``DiscardingTaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTask` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTask( // in DiscardingTaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async -> Void
  ) {

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createDiscardingTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

  }
}

@available(SwiftStdlib 6.2, *)
extension DiscardingTaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``DiscardingTaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTaskUnlessCancelled` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTaskUnlessCancelled( // in DiscardingTaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async -> Void
  ) -> Bool {

    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: true,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createDiscardingTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

    return true // task successfully enqueued
  }
}

@available(SwiftStdlib 6.2, *)
extension ThrowingDiscardingTaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``ThrowingDiscardingTaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTask` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTask( // in ThrowingDiscardingTaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async throws -> Void
  ) {

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createDiscardingTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

  }
}

@available(SwiftStdlib 6.2, *)
extension ThrowingDiscardingTaskGroup {

  /// Add a child task to the group and immediately start running it in the context of the calling thread/task.
  ///
  /// This function _starts_ the created task on the calling context.
  /// The task will continue executing on the caller's context until it suspends,
  /// and after suspension will resume on the adequate executor. For a nonisolated
  /// operation this means running on the global concurrent pool, and on an isolated
  /// operation it means the appropriate executor of that isolation context.
  ///
  /// As indicated by the lack of `async` on this method, this method does _not_
  /// suspend, and instead takes over the calling task's (thread's) execution in
  /// a synchronous manner.
  ///
  /// Other than the execution semantics discussed above, the created task
  /// is semantically equivalent to its basic version which can be
  /// created using ``ThrowingDiscardingTaskGroup/addTask``.
  ///
  /// - Parameters:
  ///   - name: Human readable name of this task.
  ///   - priority: The priority of the operation task.
  ///      Omit this parameter or pass `nil` to inherit the task group's base priority.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to
  ///      calling the `addImmediateTaskUnlessCancelled` method without a preference, and effectively
  ///      means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - operation: The operation to execute as part of the task group.
  /// - Returns: `true` if the child task was added to the group;
  ///   otherwise `false`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func addImmediateTaskUnlessCancelled( // in ThrowingDiscardingTaskGroup
    name: String? = nil,
    priority: TaskPriority? = nil,
    executorPreference taskExecutor: consuming (any TaskExecutor)? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @isolated(any) @escaping () async throws -> Void
  ) -> Bool {

    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: false, // don't enqueue, we'll run it manually
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: true,
      isSynchronousStart: true
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    #if $BuiltinCreateAsyncTaskName
    if let name {
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createDiscardingTask(
            flags: flags,
            initialSerialExecutor: builtinSerialExecutor,
            taskGroup: _group,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0
        }
    }
    #endif // $BuiltinCreateAsyncTaskName

    // Task name was not set, or task name createTask is unavailable
    if task == nil, let taskExecutor {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0
      #endif
    }

    if task == nil {
      task = Builtin.createDiscardingTask(
        flags: flags,
        initialSerialExecutor: builtinSerialExecutor,
        taskGroup: _group,
        operation: operation).0
    }

    // Assert that we did create the task, but there's no need to store it,
    // as it was added to the group itself.
    assert(task != nil, "Expected task to be created!")

    _startTaskImmediately(task!, targetExecutor: builtinSerialExecutor)

    return true // task successfully enqueued
  }
}

// ==== Legacy SPI -------------------------------------------------------------

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY && !SWIFT_CONCURRENCY_EMBEDDED
@available(SwiftStdlib 5.9, *)
extension Task where Failure == Error {

  @_spi(MainActorUtilities)
  @MainActor
  @available(SwiftStdlib 5.9, *)
  @discardableResult
  @available(*, deprecated, renamed: "immediate")
  public static func startOnMainActor(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture _ operation: __owned @Sendable @escaping @MainActor () async throws  -> Success
  ) -> Task<Success, Error> {
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: false,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false
    )

    let (task, _) = Builtin.createAsyncTask(flags, operation)
    _startTaskOnMainActor(task)

    return Task<Success, Error>(task)
  }
}
#endif

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY && !SWIFT_CONCURRENCY_EMBEDDED
@available(SwiftStdlib 5.9, *)
extension Task where Failure == Never {

  @_spi(MainActorUtilities)
  @MainActor
  @available(SwiftStdlib 5.9, *)
  @discardableResult
  @available(*, deprecated, renamed: "immediate")
  public static func startOnMainActor(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture _ operation: __owned @Sendable @escaping @MainActor () async  -> Success
  ) -> Task<Success, Never> {
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: false,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false
    )

    let (task, _) = Builtin.createAsyncTask(flags, operation)
    _startTaskOnMainActor(task)

    return Task<Success, Never>(task)
  }
}
#endif

// Internal Runtime Functions --------------------------------------------------

@_silgen_name("swift_task_startOnMainActor")
internal func _startTaskOnMainActor(_ task: Builtin.NativeObject)

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_immediate")
@usableFromInline
internal func _startTaskImmediately(_ task: Builtin.NativeObject, targetExecutor: Builtin.Executor?)
