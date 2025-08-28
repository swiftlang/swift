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
@_implementationOnly import SwiftConcurrencyInternalShims

// ==== Task Priority Escalation -----------------------------------------------

@available(SwiftStdlib 6.2, *)
extension Task {
  /// Manually escalate the task `priority` of this task to the `newPriority`.
  ///
  /// - Warning: This API should rarely be used, and instead you can rely on
  ///   structured concurrency and implicit priority escalation which happens
  ///   when a higher priority task awaits on a result of a lower priority task.
  ///
  ///   I.e. using `await` on the target task usually is the correct way to
  ///   escalate the target task to the current priority of the calling task,
  ///   especially because in such setup if the waiting task gets escalated,
  ///   the waited on task would be escalated automatically as well.
  ///
  /// The concurrency runtime is free to interpret and handle escalation
  /// depending on platform characteristics.
  ///
  /// Priority escalation is propagated to child tasks of the waited-on task,
  /// and will trigger any priority escalation handlers, if any were registered.
  ///
  /// Escalation can only *increase* the priority of a task, and
  /// de-escalating priority is not supported.
  ///
  /// This method can be called from any task or thread.
  ///
  /// - Parameters:
  ///   - task: the task which to escalate the priority of
  ///   - newPriority: the new priority the task should continue executing on
  @available(SwiftStdlib 6.2, *)
  public func escalatePriority(to newPriority: TaskPriority) {
    _taskEscalate(self._task, newPriority: newPriority.rawValue)
  }
}

@available(SwiftStdlib 6.2, *)
extension UnsafeCurrentTask {
  /// Escalate the task `priority` of the passed in task to the `newPriority`.
  ///
  /// - Warning: This API should rarely be used, and instead you can rely on
  ///   structured concurrency and implicit priority escalation which happens
  ///   when a higher priority task awaits on a result of a lower priority task.
  ///
  ///   I.e. using `await` on the target task usually is the correct way to
  ///   escalate the target task to the current priority of the calling task,
  ///   especially because in such setup if the waiting task gets escalated,
  ///   the waited on task would be escalated automatically as well.
  ///
  /// The concurrency runtime is free to interpret and handle escalation
  /// depending on platform characteristics.
  ///
  /// Priority escalation is propagated to child tasks of the waited-on task,
  /// and will trigger any priority escalation handlers, if any were registered.
  ///
  /// Escalation can only *increase* the priority of a task, and
  /// de-escalating priority is not supported.
  ///
  /// This method can be called from any task or thread.
  ///
  /// - Parameters:
  ///   - task: the task which to escalate the priority of
  ///   - newPriority: the new priority the task should continue executing on
  @available(SwiftStdlib 6.2, *)
  public func escalatePriority(to newPriority: TaskPriority) {
    unsafe _taskEscalate(self._task, newPriority: newPriority.rawValue)
  }
}

// ==== Task Priority Escalation Handlers --------------------------------------

/// Runs the passed `operation` while registering a task priority escalation handler.
/// The handler will be triggered concurrently to the current task if the current
/// is subject to priority escalation.
///
/// The handler may perform additional actions upon priority escalation,
/// but cannot influence how the escalation influences the task, i.e. the task's
/// priority will be escalated regardless of actions performed in the handler.
///
/// The handler will only trigger if a priority escalation occurs while the
/// operation is in progress.
///
/// If multiple task escalation handlers are nester they will all be triggered.
///
/// Task escalation propagates through structured concurrency child-tasks.
///
/// - Parameters:
///   - operation: the operation during which to listen for priority escalation
///   - handler: handler to invoke, concurrently to `operation`,
///              when priority escalation happens
/// - Returns: the value returned by `operation`
/// - Throws: when the `operation` throws an error
@available(SwiftStdlib 6.2, *)
public func withTaskPriorityEscalationHandler<T, E>(
  operation: () async throws(E) -> T,
  onPriorityEscalated handler: @Sendable (TaskPriority, TaskPriority) -> Void,
  isolation: isolated (any Actor)? = #isolation
) async throws(E) -> T {
  return try await __withTaskPriorityEscalationHandler0(
    operation: operation,
    onPriorityEscalated: {
      handler(TaskPriority(rawValue: $0), TaskPriority(rawValue: $1))
    })
}

// Method necessary in order to avoid the handler0 to be destroyed too eagerly.
@available(SwiftStdlib 6.2, *)
@_alwaysEmitIntoClient
func __withTaskPriorityEscalationHandler0<T, E>(
  operation: () async throws(E) -> T,
  onPriorityEscalated handler0: @Sendable (UInt8, UInt8) -> Void,
  isolation: isolated (any Actor)? = #isolation
) async throws(E) -> T {
  let record = unsafe _taskAddPriorityEscalationHandler(handler: handler0)
  defer { unsafe _taskRemovePriorityEscalationHandler(record: record) }

  return try await operation()
}
