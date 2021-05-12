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

// ==== Task Cancellation ------------------------------------------------------

/// Execute an operation with cancellation handler which will immediately be
/// invoked if the current task is cancelled.
///
/// This differs from the operation cooperatively checking for cancellation
/// and reacting to it in that the cancellation handler is _always_ and
/// _immediately_ invoked when the task is cancelled. For example, even if the
/// operation is running code which never checks for cancellation, a cancellation
/// handler still would run and give us a chance to run some cleanup code.
///
/// Does not check for cancellation, and always executes the passed `operation`.
///
/// This function returns instantly and will never suspend.
@available(SwiftStdlib 5.5, *)
public func withTaskCancellationHandler<T>(
  handler: @Sendable () -> (),
  operation: () async throws -> T
) async rethrows -> T {
  let task = Builtin.getCurrentAsyncTask()

  guard !_taskIsCancelled(task) else {
    // If the current task is already cancelled, run the handler immediately.
    handler()
    return try await operation()
  }

  let record = _taskAddCancellationHandler(handler: handler)
  defer { _taskRemoveCancellationHandler(record: record) }

  return try await operation()
}

@available(SwiftStdlib 5.5, *)
extension Task {

  /// Returns `true` if the task is cancelled, and should stop executing.
  ///
  /// If no current `Task` is available, returns `false`, as outside of a task
  /// context no task cancellation may be observed.
  ///
  /// - SeeAlso: `checkCancellation()`
  public static var isCancelled: Bool {
     withUnsafeCurrentTask { task in
       task?.isCancelled ?? false
     }
  }

  /// Returns `true` if the task is cancelled, and should stop executing.
  ///
  /// - SeeAlso: `checkCancellation()`
  @available(*, deprecated, message: "Storing `Task` instances has been deprecated and will be removed soon. Use the static 'Task.isCancelled' instead.")
  public var isCancelled: Bool {
    withUnsafeCurrentTask { task in
      guard let task = task else {
        return false
      }

      return _taskIsCancelled(task._task)
    }
  }

  /// Check if the task is cancelled and throw an `CancellationError` if it was.
  ///
  /// It is intentional that no information is passed to the task about why it
  /// was cancelled. A task may be cancelled for many reasons, and additional
  /// reasons may accrue / after the initial cancellation (for example, if the
  /// task fails to immediately exit, it may pass a deadline).
  ///
  /// The goal of cancellation is to allow tasks to be cancelled in a
  /// lightweight way, not to be a secondary method of inter-task communication.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  ///
  /// - SeeAlso: `isCancelled()`
  public static func checkCancellation() throws {
    if Task.isCancelled {
      throw CancellationError()
    }
  }

  @available(*, deprecated, message: "`Task.withCancellationHandler` has been replaced by `withTaskCancellationHandler` and will be removed shortly.")
  public static func withCancellationHandler<T>(
    handler: @Sendable () -> (),
    operation: () async throws -> T
  ) async rethrows -> T {
    try await withTaskCancellationHandler(handler: handler, operation: operation)
  }

  /// The default cancellation thrown when a task is cancelled.
  ///
  /// This error is also thrown automatically by `Task.checkCancellation()`,
  /// if the current task has been cancelled.
  public struct CancellationError: Error {
    // no extra information, cancellation is intended to be light-weight
    public init() {}
  }
}

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_addCancellationHandler")
func _taskAddCancellationHandler(handler: @Sendable () -> ()) -> UnsafeRawPointer /*CancellationNotificationStatusRecord*/

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_removeCancellationHandler")
func _taskRemoveCancellationHandler(
  record: UnsafeRawPointer /*CancellationNotificationStatusRecord*/
)
