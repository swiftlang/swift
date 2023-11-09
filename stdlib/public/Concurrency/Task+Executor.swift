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

/// Code block with specified executor ----------------------------------------

// FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 9999, *)
@_unsafeInheritExecutor // calling withTaskExecutor MUST NOT perform the "usual" hop to global
public func withTaskExecutor<T: Sendable>(
  _ executor: (any TaskExecutor)?,
  operation: @Sendable () async throws -> T
  ) async rethrows -> T {
    let executorBuiltin: Builtin.Executor =
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

    let record = _pushTaskExecutorPreference(executorBuiltin)
    defer {
      _popTaskExecutorPreference(record: record)
    }

//    #if compiler(>=9999) && $BuiltinHopToExecutor
//  if let executor {
    await Builtin.hopToExecutor(executorBuiltin)
//  } else {
//    await Builtin.hopToExecutor(_getGenericExecutor())
//  }
//    #else
//    fatalError("Swift compiler is incompatible with this SDK version")
//    #endif

    return try await operation()
}

/// Task with specified executor ----------------------------------------------

@available(SwiftStdlib 9999, *)
extension Task where Failure == Never {
  // FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  public init(
    on executor: any TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Success
  ) {
    // #if compiler(>=9999) && $BuiltinCreateAsyncTaskWithExecutor // FIXME
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
//    #else
//    fatalError("Unsupported Swift compiler")
//    #endif
  }
}

@available(SwiftStdlib 9999, *)
extension Task where Failure == Error {
  // FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  public init(
    on executor: any TaskExecutor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> Success
  ) {
    #if compiler(>=9999) && $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task.
    let taskExecutorRef = executor.asUnownedTaskExecutor()
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, taskExecutorRef, operation)
    self._task = task
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

@available(SwiftStdlib 9999, *)
extension UnsafeCurrentTask {

//  public var withunownedTaskExecutor: (any TaskExecutor)? {
  public var unownedTaskExecutor: UnownedTaskExecutor? {
    let ref = _getPreferredTaskExecutor()
//    assert(ref is TaskExecutor)
    return UnownedTaskExecutor(ref)
  }
}

// ==== Runtime ---------------------------------------------------------------

// FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_getPreferredTaskExecutor")
public func _getPreferredTaskExecutor() -> Builtin.Executor

@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_getAnyPreferredTaskExecutor")
public func _getAnyPreferredTaskExecutor() -> (any TaskExecutor)?

// FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_pushTaskExecutorPreference")
internal func _pushTaskExecutorPreference(_ executor: Builtin.Executor)
  -> UnsafeRawPointer /*TaskExecutorPreferenceStatusRecord*/

// FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_popTaskExecutorPreference")
internal func _popTaskExecutorPreference(
  record: UnsafeRawPointer /*TaskExecutorPreferenceStatusRecord*/
)

@available(SwiftStdlib 9999, *)
@usableFromInline
@_silgen_name("swift_task_getUndefinedTaskExecutor")
internal func _getUndefinedTaskExecutor() -> Builtin.Executor