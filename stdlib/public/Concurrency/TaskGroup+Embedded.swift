//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// FIXME: This is a workaround for trouble including gyb-generated sources

#if SWIFT_CONCURRENCY_EMBEDDED

@available(SwiftStdlib 5.1, *)
extension TaskGroup {

  @available(SwiftStdlib 5.1, *)
  @_alwaysEmitIntoClient
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> ChildTaskResult
  ) {
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0
  }

  @available(SwiftStdlib 5.1, *)
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> ChildTaskResult
  ) -> Bool {
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: true,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0

    return true
  }
}

@available(SwiftStdlib 5.1, *)
extension ThrowingTaskGroup {

  @available(SwiftStdlib 5.1, *)
  @_alwaysEmitIntoClient
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> ChildTaskResult
  ) {
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: false,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0
  }

  @available(SwiftStdlib 5.1, *)
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> ChildTaskResult
  ) -> Bool {
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: true,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0

    return true
  }
}

@available(SwiftStdlib 5.9, *)
extension DiscardingTaskGroup {

  @available(SwiftStdlib 5.9, *)
  @_alwaysEmitIntoClient
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> Void
  ) {
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0
  }

  @available(SwiftStdlib 5.9, *)
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> Void
  ) -> Bool {
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: true,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0

    return true
  }
}

@available(SwiftStdlib 5.9, *)
extension ThrowingDiscardingTaskGroup {

  @available(SwiftStdlib 5.9, *)
  @_alwaysEmitIntoClient
  public mutating func addTask(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> Void
  ) {
    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: true,
      isDiscardingTask: true,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor
  
    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0
  }

  @available(SwiftStdlib 5.9, *)
  @_alwaysEmitIntoClient
  public mutating func addTaskUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> Void
  ) -> Bool {
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      return false
    }

    let flags = taskCreateFlags(
      priority: priority,
      isChildTask: true,
      copyTaskLocals: false,
      inheritContext: false,
      enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: true,
      isSynchronousStart: false
    )

    let builtinSerialExecutor =
      unsafe Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    _ = Builtin.createTask(
      flags: flags,
      initialSerialExecutor: builtinSerialExecutor,
      taskGroup: _group,
      operation: operation).0

    return true
  }
}

#endif
