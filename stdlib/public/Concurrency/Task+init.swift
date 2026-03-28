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

extension Task where Failure == Never {

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public init(
      name: String? = nil,
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async -> Success
    ) {
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public init(
      name: String? = nil,
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async -> Success
    ) {
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    self._task = task
  }

#else

  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new _unstructured_ top-level task.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - priority: The priority of the operation task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @discardableResult
  public init(
    name: String? = nil,
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async -> Success
  ) {

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

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
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task init
        }
    } // let name
    #endif // $BuiltinCreateAsyncTaskName

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task init
    }

    self._task = task!
  }

#endif

} // extension Task where Failure == Never ...

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public init/*throws*/(
      name: String? = nil,
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async throws -> Success
    ) {
    fatalError("Unavailable in task-to-thread concurrency model.")
  }
}

#elseif $Embedded
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public init/*throws(Failure)*/(
      name: String? = nil,
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async throws(Failure) -> Success
    ) {
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    self._task = task
  }
}

#else
extension Task where Failure == Error { // throwing Failure error type

  /// Runs the given throwing operation asynchronously
  /// as part of a new _unstructured_ top-level task.
  ///
  /// If the `operation` throws an error, it is caught by the `Task` and will be
  /// rethrown only when the task's `value` is awaited. Take care to not accidentally
  /// dismiss errors by not awaiting on the task's resulting value.
  ///
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - priority: The priority of the operation task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @discardableResult
  public init/*throws*/(
    name: String? = nil,
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async throws -> Success
  ) {

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

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
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task init/*throws*/
        }
    } // let name
    #endif // $BuiltinCreateAsyncTaskName

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task init/*throws*/
    }

    self._task = task!
  }

} // extension Task where Failure == Error ...
#endif

extension Task where Failure == Never {

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func detached(
      name: String? = nil,
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> Success
    ) -> Task<Success, Never>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public static func detached(
      name: String? = nil,
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> Success
    ) -> Task<Success, Never>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new _unstructured_ _detached_ top-level task.
  ///
  /// Don't use a detached unstructured task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - priority: The priority of the operation task.
  ///       Omit this parameter or pass `nil` to inherit the enclosing context's base priority.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @discardableResult
  public static func detached(
    name: String? = nil,
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> Success
  ) -> Task<Success, Never>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

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
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task static func detached
        }
    } // let name
    #endif // $BuiltinCreateAsyncTaskName

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task static func detached
    }

    return Task(task!)
  }

#endif

} // extension Task where Failure == Never ...

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func detached/*throws*/(
      name: String? = nil,
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Failure>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }
}

#elseif $Embedded
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public static func detached/*throws(Failure)*/(
      name: String? = nil,
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws(Failure) -> Success
    ) -> Task<Success, Failure>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }
}

#else
extension Task where Failure == Error { // throwing Failure error type

  /// Runs the given throwing operation asynchronously
  /// as part of a new _unstructured_ _detached_ top-level task.
  ///
  /// If the `operation` throws an error, it is caught by the `Task` and will be
  /// rethrown only when the task's `value` is awaited. Take care to not accidentally
  /// dismiss errors by not awaiting on the task's resulting value.
  ///
  /// Don't use a detached unstructured task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - priority: The priority of the operation task.
  ///       Omit this parameter or pass `nil` to inherit the enclosing context's base priority.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @discardableResult
  public static func detached/*throws*/(
    name: String? = nil,
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> Success
  ) -> Task<Success, Failure>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

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
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task static func detached/*throws*/
        }
    } // let name
    #endif // $BuiltinCreateAsyncTaskName

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task static func detached/*throws*/
    }

    return Task(task!)
  }

} // extension Task where Failure == Error ...
#endif

extension Task where Failure == Never {

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public init(
      name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async -> Success
    ) {
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#else

  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new _unstructured_ top-level task.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to no preference,
  ///      and effectively means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - priority: The priority of the operation task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 6.0, *)
  @discardableResult
  public init(
    name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async -> Success
  ) {

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    var task: Builtin.NativeObject?
    #if $BuiltinCreateAsyncTaskName
    if let name {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task init
        }
      #else // no $BuiltinCreateAsyncTaskOwnedTaskExecutor
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutor: executorBuiltin,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task init
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
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0 // Task init
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0 // Task init
      #endif
    }

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      operation: operation).0 // Task init
    }

    self._task = task!
  }

#endif

} // extension Task where Failure == Never ...

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public init/*throws*/(
      name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async throws -> Success
    ) {
    fatalError("Unavailable in task-to-thread concurrency model.")
  }
}

#else
extension Task where Failure == Error { // throwing Failure error type

  /// Runs the given throwing operation asynchronously
  /// as part of a new _unstructured_ top-level task.
  ///
  /// If the `operation` throws an error, it is caught by the `Task` and will be
  /// rethrown only when the task's `value` is awaited. Take care to not accidentally
  /// dismiss errors by not awaiting on the task's resulting value.
  ///
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to no preference,
  ///      and effectively means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - priority: The priority of the operation task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 6.0, *)
  @discardableResult
  public init/*throws*/(
    name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async throws -> Success
  ) {

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    var task: Builtin.NativeObject?
    #if $BuiltinCreateAsyncTaskName
    if let name {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task init/*throws*/
        }
      #else // no $BuiltinCreateAsyncTaskOwnedTaskExecutor
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutor: executorBuiltin,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task init/*throws*/
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
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0 // Task init/*throws*/
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0 // Task init/*throws*/
      #endif
    }

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      operation: operation).0 // Task init/*throws*/
    }

    self._task = task!
  }

} // extension Task where Failure == Error ...
#endif

extension Task where Failure == Never {

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func detached(
      name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async -> Success
    ) -> Task<Success, Never>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#else

  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new _unstructured_ _detached_ top-level task.
  ///
  /// Don't use a detached unstructured task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to no preference,
  ///      and effectively means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - priority: The priority of the operation task.
  ///       Omit this parameter or pass `nil` to inherit the enclosing context's base priority.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 6.0, *)
  @discardableResult
  public static func detached(
    name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async -> Success
  ) -> Task<Success, Never>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    var task: Builtin.NativeObject?
    #if $BuiltinCreateAsyncTaskName
    if let name {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task static func detached
        }
      #else // no $BuiltinCreateAsyncTaskOwnedTaskExecutor
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutor: executorBuiltin,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task static func detached
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
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0 // Task static func detached
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0 // Task static func detached
      #endif
    }

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      operation: operation).0 // Task static func detached
    }

    return Task(task!)
  }

#endif

} // extension Task where Failure == Never ...

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func detached/*throws*/(
      name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async throws -> Success
    ) -> Task<Success, Failure>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }
}

#else
extension Task where Failure == Error { // throwing Failure error type

  /// Runs the given throwing operation asynchronously
  /// as part of a new _unstructured_ _detached_ top-level task.
  ///
  /// If the `operation` throws an error, it is caught by the `Task` and will be
  /// rethrown only when the task's `value` is awaited. Take care to not accidentally
  /// dismiss errors by not awaiting on the task's resulting value.
  ///
  /// Don't use a detached unstructured task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - name: Human readable name of the task.
  ///   - taskExecutor: The task executor that the child task should be started on and keep using.
  ///      Explicitly passing `nil` as the executor preference is equivalent to no preference,
  ///      and effectively means to inherit the outer context's executor preference.
  ///      You can also pass the ``globalConcurrentExecutor`` global executor explicitly.
  ///   - priority: The priority of the operation task.
  ///       Omit this parameter or pass `nil` to inherit the enclosing context's base priority.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 6.0, *)
  @discardableResult
  public static func detached/*throws*/(
    name: String? = nil,
    executorPreference taskExecutor: (any TaskExecutor)?,
    priority: TaskPriority? = nil,
    operation: sending @escaping () async throws -> Success
  ) -> Task<Success, Failure>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    var task: Builtin.NativeObject?
    #if $BuiltinCreateAsyncTaskName
    if let name {
      #if $BuiltinCreateAsyncTaskOwnedTaskExecutor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutorConsuming: taskExecutor,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task static func detached/*throws*/
        }
      #else // no $BuiltinCreateAsyncTaskOwnedTaskExecutor
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor
      task =
        unsafe name.utf8CString.withUnsafeBufferPointer { nameBytes in
          Builtin.createTask(
            flags: flags,
            initialTaskExecutor: executorBuiltin,
            taskName: nameBytes.baseAddress!._rawValue,
            operation: operation).0 // Task static func detached/*throws*/
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
        initialTaskExecutorConsuming: taskExecutor,
        operation: operation).0 // Task static func detached/*throws*/
      #else
      // legacy branch for the non-consuming task executor
      let executorBuiltin: Builtin.Executor =
        taskExecutor.asUnownedTaskExecutor().executor

      task = Builtin.createTask(
        flags: flags,
        initialTaskExecutor: executorBuiltin,
        operation: operation).0 // Task static func detached/*throws*/
      #endif
    }

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      operation: operation).0 // Task static func detached/*throws*/
    }

    return Task(task!)
  }

} // extension Task where Failure == Error ...
#endif

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func detach<Success>(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public func detach<Success>(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`detach` was replaced by `Task.detached` and will be removed shortly.")
  @discardableResult
  public func detach<Success>(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
  ) -> Task<Success, Never>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task func detach<Success>
    }

    return Task(task!)
  }

#endif

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func detach<Success>/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Error>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public func detach<Success>/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Error>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`detach` was replaced by `Task.detached` and will be removed shortly.")
  @discardableResult
  public func detach<Success>/*throws*/(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
  ) -> Task<Success, Error>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task func detach<Success>/*throws*/
    }

    return Task(task!)
  }

#endif

extension Task where Failure == Never {

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func runDetached(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public static func runDetached(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`Task.runDetached` was replaced by `Task.detached` and will be removed shortly.")
  @discardableResult
  public static func runDetached(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
  ) -> Task<Success, Never>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task static func runDetached
    }

    return Task(task!)
  }

#endif

} // extension Task ...

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func runDetached/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Failure>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }
}

#elseif $Embedded
extension Task { // throwing Failure error type
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public static func runDetached/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Failure>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }
}

#else
extension Task where Failure == Error { // throwing Failure error type

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`Task.runDetached` was replaced by `Task.detached` and will be removed shortly.")
  @discardableResult
  public static func runDetached/*throws*/(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
  ) -> Task<Success, Failure>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task static func runDetached/*throws*/
    }

    return Task(task!)
  }

} // extension Task where Failure == Error ...
#endif

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func asyncDetached<Success>(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public func asyncDetached<Success>(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`asyncDetached` was replaced by `Task.detached` and will be removed shortly.")
  @discardableResult
  public func asyncDetached<Success>(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
  ) -> Task<Success, Never>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task func asyncDetached<Success>
    }

    return Task(task!)
  }

#endif

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func asyncDetached<Success>/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Error>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public func asyncDetached<Success>/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Error>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`asyncDetached` was replaced by `Task.detached` and will be removed shortly.")
  @discardableResult
  public func asyncDetached<Success>/*throws*/(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
  ) -> Task<Success, Error>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: false /* detached */,
      inheritContext: false /* detached */,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task func asyncDetached<Success>/*throws*/
    }

    return Task(task!)
  }

#endif

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func async<Success>(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public func async<Success>(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
    ) -> Task<Success, Never>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`async` was replaced by `Task.init` and will be removed shortly.")
  @discardableResult
  public func async<Success>(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async  -> Success
  ) -> Task<Success, Never>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task func async<Success>
    }

    return Task(task!)
  }

#endif

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func async<Success>/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Error>{
    fatalError("Unavailable in task-to-thread concurrency model.")
  }

#elseif $Embedded
  @discardableResult
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public func async<Success>/*throws*/(
      priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
    ) -> Task<Success, Error>{
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let (task, _) = Builtin.createAsyncTask(flags, operation)

    return Task(task)
  }

#else

/// Deprecated, available only for source compatibility reasons.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "`async` was replaced by `Task.init` and will be removed shortly.")
  @discardableResult
  public func async<Success>/*throws*/(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping @isolated(any) () async throws -> Success
  ) -> Task<Success, Error>{

    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: false,
      copyTaskLocals: true,
      inheritContext: true,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false,
      isSynchronousStart: false)

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    var task: Builtin.NativeObject?

    if task == nil {
      // either no task name was set, or names are unsupported
      task = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: operation).0 // Task func async<Success>/*throws*/
    }

    return Task(task!)
  }

#endif

// =====================================================================================================================
// =====================================================================================================================
