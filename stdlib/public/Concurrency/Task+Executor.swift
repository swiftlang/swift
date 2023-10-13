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
@_unsafeInheritExecutor // calling withExecutor MUST NOT perform the "usual" hop to global
public func withExecutor<T: Sendable>(
  _ executor: any SerialExecutor, // FIXME: any Executor
  operation: @Sendable () async throws -> T
  ) async rethrows -> T {
  let executorBuiltin = executor.asUnownedSerialExecutor().executor
  let record = _pushTaskExecutorPreference(executorBuiltin)
  defer { _popTaskExecutorPreference(record: record) }

#if compiler(>=5.5) && $BuiltinHopToExecutor
  await Builtin.hopToExecutor(executorBuiltin)
#else
  fatalError("Swift compiler is incompatible with this SDK version")
#endif
  executor.preconditionIsolated() // TODO: remove this once confident

  return try await operation()
}

//// FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
//@available(SwiftStdlib 9999, *)
//@_unsafeInheritExecutor // calling withExecutor MUST NOT perform the "usual" hop to global
//public func withExecutor<T: Sendable>(
//  _ executor: any Executor, // FIXME: any Executor
//  operation: @Sendable () async throws -> T
//  ) async rethrows -> T {
//  let executorBuiltin = executor.asUnownedSerialExecutor().executor
//  let record = _pushTaskExecutorPreference(executorBuiltin)
//  defer { _popTaskExecutorPreference(record: record) }
//
//#if compiler(>=5.5) && $BuiltinHopToExecutor
//  await Builtin.hopToExecutor(executorBuiltin)
//#else
//  fatalError("Swift compiler is incompatible with this SDK version")
//#endif
//  executor.preconditionIsolated() // TODO: remove this once confident
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
    on executor: any Executor,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> Success
  ) {
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskWithExecutor
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

  // FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  public init<A>(
    on actor: A,
    priority: TaskPriority? = nil,
    operation _operation: __owned @Sendable @escaping (isolated A) async -> Success
  ) where A: Actor {
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
        priority: priority, isChildTask: false, copyTaskLocals: true,
        inheritContext: true, enqueueJob: true,
        addPendingGroupTaskUnconditionally: false,
        isDiscardingTask: false)

    typealias IsolatedSignature = (isolated A) async -> Success

    let operation: (@Sendable () async -> Success) = {
      await _operation(actor)
    }

    // Create the asynchronous task.
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, actor.unownedExecutor.executor, operation)

    self._task = task
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

@available(SwiftStdlib 9999, *)
extension GlobalActor {

  // FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  public static func task<Success>(
    priority: TaskPriority? = nil,
    operation _operation: __owned @Sendable @escaping (isolated Self.ActorType) async -> Success
  ) -> Task<Success, Never> {
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    typealias IsolatedSignature = (isolated Self) async -> Success

    let operation: (@Sendable () async -> Success) = {
      await _operation(Self.shared)
    }

    // Create the asynchronous task.
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, Self.sharedUnownedExecutor.executor, operation)

    return Task(task)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }

  // FIXME: do the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  public static func task<Success>(
    priority: TaskPriority? = nil,
    operation _operation: __owned @MainActor @Sendable @escaping () async -> Success
  ) -> Task<Success, Never> where Self == MainActor {
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    typealias IsolatedSignature = (isolated Self) async -> Success

    let operation: (@Sendable () async -> Success) = {
      await _operation()
    }

    // Create the asynchronous task.
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, Self.sharedUnownedExecutor.executor, operation)

    return Task(task)
    #else
    fatalError("Unsupported Swift compiler")
    #endif
  }
}

/// Task on MainActor specializations -----------------------------------------

@available(SwiftStdlib 9999, *)
extension Task where Failure == Never {

  // TODO: rdar://116049628 Would like to express <GA>(@GA () -> Success) where GA: GlobalActor but can't today
  @discardableResult
  @_alwaysEmitIntoClient
  public init(on globalActorType: MainActor.Type,
              priority: TaskPriority? = nil,
              operation _operation: __owned @MainActor @Sendable @escaping () async -> Success) {
    #if compiler(>=5.9) && $BuiltinCreateAsyncTaskWithExecutor
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    let operation: (@Sendable () async -> Success) = {
      await _operation()
    }

    // Create the asynchronous task.
    let (task, _) = Builtin.createAsyncTaskWithExecutor(
      flags, MainActor.sharedUnownedExecutor.executor, operation)

    self._task = task
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
