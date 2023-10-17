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
  ///   - on:
  ///   - priority: The priority of the operation task.
  ///     Omit this parameter or pass `.unspecified`
  ///     to set the child task's priority to the priority of the group.
  ///   - operation: The operation to execute as part of the task group.
  @_alwaysEmitIntoClient
  public mutating func addTask(
    on executor: (any Executor)?,
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    #if compiler(>=5.5) && $BuiltinCreateAsyncTaskInGroup
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

    if let executor {
      let executorBuiltin: Builtin.Executor =
      if let serialExecutor = executor as? any SerialExecutor {
        // We need to go through the asUnowned... for serial executors,
        // because they encode certain behavior in the reference bits,
        // so we cannot just cast and assume it'll be correct.
        serialExecutor.asUnownedSerialExecutor().executor
      } else {
        _task_executor_getExecutorRef(executor)
      }

      // Create the task in this group with an executor preference.
      _ = Builtin.createAsyncTaskInGroupWithExecutor(flags, _group, executorBuiltin, operation)
    } else {
      // Create the task in this group without executor preference.
      _ = Builtin.createAsyncTaskInGroup(flags, _group, operation)
    }
    #else
    fatalError("Unsupported Swift compiler")
    #endif
    }
  }
