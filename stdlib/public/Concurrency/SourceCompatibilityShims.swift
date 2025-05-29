//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file provides source compatibility shims to help migrate code
// using earlier versions of the concurrency library to the latest syntax.
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {
  @available(*, deprecated, message: "Task.Priority has been removed; use TaskPriority")
  public typealias Priority = TaskPriority

  @available(*, deprecated, message: "Task.Handle has been removed; use Task")
  public typealias Handle = _Concurrency.Task

  @available(*, deprecated, message: "Task.CancellationError has been removed; use CancellationError")
  @_alwaysEmitIntoClient
  public static func CancellationError() -> _Concurrency.CancellationError {
    return _Concurrency.CancellationError()
  }

  @available(*, deprecated, renamed: "yield()")
  @_alwaysEmitIntoClient
  public static func suspend() async {
    await yield()
  }
}

@available(SwiftStdlib 5.1, *)
extension TaskPriority {
  @available(*, deprecated, message: "unspecified priority will be removed; use nil")
  @_alwaysEmitIntoClient
  public static var unspecified: TaskPriority {
    .init(rawValue: 0x00)
  }

  @available(*, deprecated, message: "userInteractive priority will be removed")
  @_alwaysEmitIntoClient
  public static var userInteractive: TaskPriority {
    .init(rawValue: 0x21)
  }
}

@available(SwiftStdlib 5.1, *)
@_alwaysEmitIntoClient
@available(*, deprecated, renamed: "withTaskCancellationHandler(operation:onCancel:)")
public func withTaskCancellationHandler<T>(
  handler: @Sendable () -> Void,
  operation: () async throws -> T
) async rethrows -> T {
  try await withTaskCancellationHandler(operation: operation, onCancel: handler)
}

// Note: hack to stage out @_unsafeInheritExecutor forms of various functions
// in favor of #isolation. The _unsafeInheritExecutor_ prefix is meaningful
// to the type checker.
@available(SwiftStdlib 5.1, *)
@_alwaysEmitIntoClient
@_unsafeInheritExecutor
@available(*, deprecated, renamed: "withTaskCancellationHandler(operation:onCancel:)")
public func _unsafeInheritExecutor_withTaskCancellationHandler<T>(
  handler: @Sendable () -> Void,
  operation: () async throws -> T
) async rethrows -> T {
  try await withTaskCancellationHandler(operation: operation, onCancel: handler)
}

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {
  @available(*, deprecated, message: "`Task.withCancellationHandler` has been replaced by `withTaskCancellationHandler` and will be removed shortly.")
  @_alwaysEmitIntoClient
  public static func withCancellationHandler<T>(
    handler: @Sendable () -> Void,
    operation: () async throws -> T
  ) async rethrows -> T {
    try await withTaskCancellationHandler(operation: operation, onCancel: handler)
  }
}

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {
  @available(*, deprecated, message: "`Task.Group` was replaced by `ThrowingTaskGroup` and `TaskGroup` and will be removed shortly.")
  public typealias Group<TaskResult: Sendable> = ThrowingTaskGroup<TaskResult, Error>

  @available(*, deprecated, message: "`Task.withGroup` was replaced by `withThrowingTaskGroup` and `withTaskGroup` and will be removed shortly.")
  @_alwaysEmitIntoClient
  public static func withGroup<TaskResult: Sendable, BodyResult>(
    resultType: TaskResult.Type,
    returning returnType: BodyResult.Type = BodyResult.self,
    body: (inout Task.Group<TaskResult>) async throws -> BodyResult
  ) async rethrows -> BodyResult {
    try await withThrowingTaskGroup(of: resultType) { group in
      try await body(&group)
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension Task {
  @available(*, deprecated, message: "get() has been replaced by .value")
  @_alwaysEmitIntoClient
  public func get() async throws -> Success {
    return try await value
  }

  @available(*, deprecated, message: "getResult() has been replaced by .result")
  @_alwaysEmitIntoClient
  public func getResult() async -> Result<Success, Failure>  {
    return await result
  }
}

@available(SwiftStdlib 5.1, *)
extension Task where Failure == Never {
  @available(*, deprecated, message: "get() has been replaced by .value")
  @_alwaysEmitIntoClient
  public func get() async -> Success {
    return await value
  }
}

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.1, *)
extension TaskGroup {
  @available(*, deprecated, renamed: "addTask(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func add(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) async -> Bool {
    return self.addTaskUnlessCancelled(priority: priority) {
      await operation()
    }
  }

  @available(*, deprecated, renamed: "addTask(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawn(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    addTask(priority: priority, operation: operation)
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawnUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(priority: priority, operation: operation)
  }

  @available(*, deprecated, renamed: "addTask(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func async(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    addTask(priority: priority, operation: operation)
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func asyncUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(priority: priority, operation: operation)
  }
}
#else
@available(SwiftStdlib 5.1, *)
extension TaskGroup {
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
  public mutating func add(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) async -> Bool {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(operation:)")
  @_alwaysEmitIntoClient
  public mutating func add(
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) async -> Bool {
    return self.addTaskUnlessCancelled {
      await operation()
    }
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
  public mutating func spawn(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTask(operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawn(
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    addTask(operation: operation)
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
  public mutating func spawnUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawnUnlessCancelled(
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(operation: operation)
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
  public mutating func async(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTask(operation:)")
  @_alwaysEmitIntoClient
  public mutating func async(
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    addTask(operation: operation)
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
  public mutating func asyncUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(operation:)")
  @_alwaysEmitIntoClient
  public mutating func asyncUnlessCancelled(
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(operation: operation)
  }
}
#endif

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.1, *)
extension ThrowingTaskGroup {
  @available(*, deprecated, renamed: "addTask(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func add(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) async -> Bool {
    return self.addTaskUnlessCancelled(priority: priority) {
      try await operation()
    }
  }

  @available(*, deprecated, renamed: "addTask(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawn(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    addTask(priority: priority, operation: operation)
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawnUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(priority: priority, operation: operation)
  }

  @available(*, deprecated, renamed: "addTask(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func async(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    addTask(priority: priority, operation: operation)
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(priority:operation:)")
  @_alwaysEmitIntoClient
  public mutating func asyncUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(priority: priority, operation: operation)
  }
}
#else
@available(SwiftStdlib 5.1, *)
extension ThrowingTaskGroup {
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
  public mutating func add(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) async -> Bool {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(operation:)")
  @_alwaysEmitIntoClient
  public mutating func add(
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) async -> Bool {
    return self.addTaskUnlessCancelled {
      try await operation()
    }
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
  public mutating func spawn(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTask(operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawn(
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    addTask(operation: operation)
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
  public mutating func spawnUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(operation:)")
  @_alwaysEmitIntoClient
  public mutating func spawnUnlessCancelled(
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(operation: operation)
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTask(operation:)")
  public mutating func async(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTask(operation:)")
  @_alwaysEmitIntoClient
  public mutating func async(
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    addTask(operation: operation)
  }

  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model", renamed: "addTaskUnlessCancelled(operation:)")
  public mutating func asyncUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    fatalError("Unavailable in task-to-thread concurrency model")
  }

  @available(*, deprecated, renamed: "addTaskUnlessCancelled(operation:)")
  @_alwaysEmitIntoClient
  public mutating func asyncUnlessCancelled(
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    addTaskUnlessCancelled(operation: operation)
  }
}
#endif

@available(SwiftStdlib 5.1, *)
@available(*, deprecated, message: "please use UnsafeContinuation<..., Error>")
public typealias UnsafeThrowingContinuation<T> = UnsafeContinuation<T, Error>

@available(SwiftStdlib 5.1, *)
@available(*, deprecated, renamed: "UnownedJob")
public typealias PartialAsyncTask = UnownedJob
