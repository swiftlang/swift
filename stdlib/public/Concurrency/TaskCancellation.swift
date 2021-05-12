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

/// Execute an operation with a cancellation handler that's immediately
/// invoked if the current task is canceled.
///
/// This differs from the operation cooperatively checking for cancellation
/// and reacting to it in that the cancellation handler is _always_ and
/// _immediately_ invoked when the task is canceled. For example, even if the
/// operation is running code that never checks for cancellation, a cancellation
/// handler still runs and provides a chance to run some cleanup code.
///
/// Doesn't check for cancellation, and always executes the passed `operation`.
///
/// This function returns immediately and never suspends.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {

  /// A Boolean value that indicates whether
  /// the current task should stop executing.
  ///
  /// If there is no current task, the value of this property is `false`.
  ///
  /// - SeeAlso: `checkCancellation()`
  public static var isCancelled: Bool {
     withUnsafeCurrentTask { task in
       task?.isCancelled ?? false
     }
  }

  /// A Boolean value that indicates whether the task should stop executing.
  ///
  /// - SeeAlso: `checkCancellation()`
  @available(*, deprecated, message: "Storing `Task` instances has been deprecated and will be removed soon. Use the static 'Task.isCancelled' instead.")
  public var isCancelled: Bool {
    _taskIsCancelled(_task)
  }

  /// Throws a cancellation error if the current task was canceled.
  ///
  /// The error is always an instance of `Task.CancellationError`.
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

  /// The default error thrown by a canceled task.
  ///
  /// The `Task.checkCancellation()` method throws this error
  /// if the current task has been canceled.
  /// You can throw this error in your cancellation-checking code,
  /// or another more specific error if needed.
  public struct CancellationError: Error {
    // no extra information, cancellation is intended to be light-weight
    public init() {}
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_addCancellationHandler")
func _taskAddCancellationHandler(handler: @Sendable () -> ()) -> UnsafeRawPointer /*CancellationNotificationStatusRecord*/

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_removeCancellationHandler")
func _taskRemoveCancellationHandler(
  record: UnsafeRawPointer /*CancellationNotificationStatusRecord*/
)
