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

/// Configure the current task hierarchy's task executor preference to the passed ``TaskExecutor``,
/// and execute the passed in closure by immediately hopping to that executor.
///
/// ### Task executor preference semantics
/// Task executors influence _where_ nonisolated asynchronous functions, and default actor methods execute.
/// The preferred executor will be used whenever possible, rather than hopping to the global concurrent pool.
///
/// For an in depth discussion of this topic, see ``TaskExecutor``.
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
/// - SeeAlso: `TaskExecutor`
@_unavailableInEmbedded
@available(SwiftStdlib 9999, *)
@_unsafeInheritExecutor // calling withTaskExecutor MUST NOT perform the "usual" hop to global
public func withTaskExecutor<T: Sendable>(
  _ executor: (any TaskExecutor)?,
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
  /// This overload allows specifying a preferred ``TaskExecutor`` on which
  /// the `operation`, as well as all child tasks created from this task will be
  /// executing whenever possible. Refer to ``TaskExecutor`` for a detailed discussion
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
    on executor: any TaskExecutor,
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
    on executor: any TaskExecutor,
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

@available(SwiftStdlib 9999, *)
extension UnsafeCurrentTask {

  /// The current ``TaskExecutor`` preference, if this task has one configured.
  ///
  /// The executor may be used to compare for equality with an expected executor preference.
  ///
  /// The lifetime of an executor is not guaranteed by an ``UnownedTaskExecutor``,
  /// so accessing it must be handled with great case -- and the program must use other
  /// means to guarantee the executor remains alive while it is in use.
  @available(SwiftStdlib 9999, *)
  public var unownedTaskExecutor: UnownedTaskExecutor? {
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
