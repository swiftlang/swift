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

/// Configure the current task hierarchy's task executor preference to the passed ``_TaskExecutor``,
/// and execute the passed in closure by immediately hopping to that executor.
///
/// ### Task executor preference semantics
/// Task executors influence _where_ nonisolated asynchronous functions, and default actor methods execute.
/// The preferred executor will be used whenever possible, rather than hopping to the global concurrent pool.
///
/// For an in depth discussion of this topic, see ``_TaskExecutor``.
///
/// ### Disabling task executor preference
/// Passing `nil` as executor means disabling any preference preference (if it was set) and the task hierarchy
/// will execute without any executor preference until a different preference is set.
///
/// - Parameters:
///   - executor: the task executor to use as preferred task executor; if `nil` it is interpreted as "no preference"
///   - operation: the operation to execute on the passed executor; if the executor was `nil`, this will execute on the default global concurrent executor.
/// - Returns: the value returned from the `operation` closure
/// - Throws: if the operation closure throws
/// - SeeAlso: `_TaskExecutor`
@_unavailableInEmbedded
@available(SwiftStdlib 9999, *)
@_unsafeInheritExecutor // calling withTaskExecutor MUST NOT perform the "usual" hop to global
public func _withTaskExecutor<T: Sendable>(
  _ executor: (any _TaskExecutor)?,
  operation: @Sendable () async throws -> T
  ) async rethrows -> T {
  let taskExecutorBuiltin: Builtin.Executor =
    if let executor {
      // We need to go through the asUnowned... for serial executors,
      // because they encode certain behavior in the reference bits,
      // so we cannot just cast and assume it'll be correct.
      executor.asUnownedTaskExecutor().executor
    } else {
      // we must push a "no preference" record onto the task
      // because there may be other records issuing a preference already,
      // so by pushing this "no preference" (undefined task executor),
      // we turn off the task executor preference for the scope of `operation`.
      _getUndefinedTaskExecutor()
    }

  let record = _pushTaskExecutorPreference(taskExecutorBuiltin)
  defer {
    _popTaskExecutorPreference(record: record)
  }

  // No need to manually hop to the target executor, because as we execute
  // the operation, its enqueue will respect the attached executor preference.

  return try await operation()
}

/// Task with specified executor ----------------------------------------------

@available(SwiftStdlib 9999, *)
extension Task where Failure == Never {

  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new top-level task on behalf of the current actor.
  ///
  /// This overload allows specifying a preferred ``_TaskExecutor`` on which
  /// the `operation`, as well as all child tasks created from this task will be
  /// executing whenever possible. Refer to ``_TaskExecutor`` for a detailed discussion
  // of the effect of task executors on execution semantics of asynchronous code.
  ///
  /// Use this function when creating asynchronous work
  /// that operates on behalf of the synchronous function that calls it.
  /// Like `Task.detached(priority:operation:)`,
  /// this function creates a separate, top-level task.
  /// Unlike `Task.detached(priority:operation:)`,
  /// the task created by `Task.init(priority:operation:)`
  /// inherits the priority and actor context of the caller,
  /// so the operation is treated more like an asynchronous extension
  /// to the synchronous operation.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a detached task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - executor: the preferred task executor for this task, and any child tasks created by it
  ///   - priority: The priority of the task.
  ///     Pass `nil` to use the priority from `Task.currentPriority`.
  ///   - operation: The operation to perform.
  @discardableResult
  @_alwaysEmitIntoClient
  public init(
    _on executor: any _TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Success
  ) {
    #if $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task.
    let taskExecutorRef = executor.asUnownedTaskExecutor()
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, taskExecutorRef.executor, operation)
    self._task = task
    #else
    fatalError("Unsupported Swift compiler, missing support for BuiltinCreateAsyncTaskWithExecutor")
    #endif
  }
}

@available(SwiftStdlib 9999, *)
extension Task where Failure == Error {
  @discardableResult
  @_alwaysEmitIntoClient
  public init(
    _on executor: any _TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Success
  ) {
    #if $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task.
    let taskExecutorRef = executor.asUnownedTaskExecutor()
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, taskExecutorRef.executor, operation)
    self._task = task
    #else
    fatalError("Unsupported Swift compiler, missing support for $BuiltinCreateAsyncTaskWithExecutor")
    #endif
  }
}

// ==== Detached tasks ---------------------------------------------------------

@available(SwiftStdlib 9999, *)
extension Task where Failure == Never {
  #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func _detached(
    on executor: any _TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Success
  ) -> Task<Success, Failure> {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
  #else
  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new top-level task.
  ///
  /// Don't use a detached task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the detached task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a detached task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - executor: The task executor preference to use for this task.
  ///   - priority: The priority of the task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @discardableResult
  @_alwaysEmitIntoClient
  public static func _detached(
    on executor: any _TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Success
  ) -> Task<Success, Failure> {
    #if $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task.
    let taskExecutorRef = executor.asUnownedTaskExecutor()
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, taskExecutorRef.executor, operation)

    return Task(task)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
  #endif
}

@available(SwiftStdlib 9999, *)
extension Task where Failure == Error {
  #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func _detached(
    on executor: any _TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Success
  ) -> Task<Success, Failure> {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
  #else
  /// Runs the given throwing operation asynchronously
  /// as part of a new top-level task.
  ///
  /// If the operation throws an error, this method propagates that error.
  ///
  /// Don't use a detached task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the detached task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a detached task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - executor: The task executor preference to use for this task.
  ///   - priority: The priority of the task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @discardableResult
  @_alwaysEmitIntoClient
  public static func _detached(
    on executor: any _TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Success
  ) -> Task<Success, Failure> {
    #if $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task.
    // Create the asynchronous task.
    let taskExecutorRef = executor.asUnownedTaskExecutor()
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, taskExecutorRef.executor, operation)

    return Task(task)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
  #endif
}

// ==== Unsafe Current Task ----------------------------------------------------

@available(SwiftStdlib 9999, *)
extension UnsafeCurrentTask {

  /// The current ``_TaskExecutor`` preference, if this task has one configured.
  ///
  /// The executor may be used to compare for equality with an expected executor preference.
  ///
  /// The lifetime of an executor is not guaranteed by an ``UnownedTaskExecutor``,
  /// so accessing it must be handled with great case -- and the program must use other
  /// means to guarantee the executor remains alive while it is in use.
  @available(SwiftStdlib 9999, *)
  public var _unownedTaskExecutor: UnownedTaskExecutor? {
    let ref = _getPreferredTaskExecutor()
    return UnownedTaskExecutor(ref)
  }
}

// ==== Runtime ---------------------------------------------------------------

@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_getPreferredTaskExecutor")
internal func _getPreferredTaskExecutor() -> Builtin.Executor

typealias TaskExecutorPreferenceStatusRecord = UnsafeRawPointer

@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_pushTaskExecutorPreference")
internal func _pushTaskExecutorPreference(_ executor: Builtin.Executor)
  -> TaskExecutorPreferenceStatusRecord

@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_popTaskExecutorPreference")
internal func _popTaskExecutorPreference(
  record: TaskExecutorPreferenceStatusRecord
)

/// Get the "undefined" task executor reference.
///
/// It can be used to compare against, and is semantically equivalent to
/// "no preference".
@available(SwiftStdlib 9999, *)
@usableFromInline
internal func _getUndefinedTaskExecutor() -> Builtin.Executor {
  // Similar to the `_getGenericSerialExecutor` this method relies
  // on the runtime representation of the "undefined" executor
  // to be specifically `{0, 0}` (a null-pointer to an executor and witness
  // table).
  //
  // Rather than call into the runtime to return the
  // `TaskExecutorRef::undefined()`` we this information to bitcast
  // and return it directly.
  unsafeBitCast((UInt(0), UInt(0)), to: Builtin.Executor.self)
}

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
