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
  _ executor: (any SerialExecutor)?, // FIXME: any Executor
  operation: @Sendable () async throws -> T
  ) async rethrows -> T {
    let executorBuiltin: Builtin.Executor =
        if let executor {
          // We need to go through the asUnowned... for serial executors,
          // because they encode certain behavior in the reference bits,
          // so we cannot just cast and assume it'll be correct.
          executor.asUnownedSerialExecutor().executor
        } else {
          _getGenericExecutor()
        }

    let record = _pushTaskExecutorPreference(executorBuiltin)
    defer {
      _popTaskExecutorPreference(record: record)
    }

    #if compiler(>=9999) && $BuiltinHopToExecutor
    await Builtin.hopToExecutor(executorBuiltin)
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
    if let executor {
      executor.preconditionIsolated() // TODO: remove this once confident
    }

    return try await operation()
}

//// FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
//@available(SwiftStdlib 9999, *)
//@_unsafeInheritExecutor // calling withTaskExecutor MUST NOT perform the "usual" hop to global
//public func withTaskExecutor<T: Sendable>(
//  _ executor: any Executor, // FIXME: any Executor
//  operation: @Sendable () async throws -> T
//  ) async rethrows -> T {
//  let executorBuiltin = Builtin.buildOrdinaryExecutorRef(executor)
//  let record = _pushTaskExecutorPreference(executorBuiltin)
//  defer { _popTaskExecutorPreference(record: record) }
//
//#if compiler(>=5.5) && $BuiltinHopToExecutor
//  await Builtin.hopToExecutor(executorBuiltin)
//#else
//  fatalError("Swift compiler is incompatible with this SDK version")
//#endif
//  // executor.preconditionIsolated() // TODO: remove this once confident
//
//  return try await operation()
//}

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
    #if compiler(>=9999) && $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task.
    if let serialExecutor = executor as? any SerialExecutor {
      // We need to go through the asUnowned... for serial executors,
      // because they encode certain behavior in the reference bits,
      // so we cannot just cast and assume it'll be correct.
      let executorBuiltin = serialExecutor.asUnownedSerialExecutor().executor
      let (task, _) = Builtin.createAsyncTaskWithExecutor(
        flags, executorBuiltin, operation)
      self._task = task
    } else {
      let (task, _) = Builtin.createAsyncTaskWithExecutor(
        flags, unsafeBitCast(executor, to: Builtin.Executor.self), operation)
      self._task = task
    }
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

// ==== Runtime ---------------------------------------------------------------

// FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_getPreferredTaskExecutor")
public func _getPreferredTaskExecutor() -> Builtin.Executor

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
