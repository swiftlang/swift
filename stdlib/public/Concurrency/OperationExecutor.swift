//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

/// A registration for an asynchronous operation,
///
/// Used by the ``OperationExecutor`` to identify operations and allow
/// cancellation and priority escalation.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 9999, *)
@frozen
public struct OperationExecutorRegistration: Sendable, Hashable {
  @usableFromInline
  internal var _id: UInt64

  @usableFromInline
  internal init(id: UInt64) {
    self._id = id
  }

  @_alwaysEmitIntoClient
  public static func == (
    lhs: OperationExecutorRegistration,
    rhs: OperationExecutorRegistration
  ) -> Bool {
    lhs._id == rhs._id
  }

  @_alwaysEmitIntoClient
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_id)
  }
}

/// An executor that can run asynchronous operations such as timers and I/O.
///
/// This protocol owns only the *lifecycle* of an asynchronous operation:
///  - Cancelling an operation.
///  - Escalating the priority of an operation.
///
/// ## Example
///
/// A suspending function drives it as:
/// ```
/// return try await withExecutorContinuation(of: Void.self, throwing: (any Error).self) { producer, awaiter in
///   let registration = executor.enqueue(producer, at: deadline, tolerance: tolerance)
///   var awaiterBox = Optional(awaiter)
///   return try await withTaskCancellationHandler {
///     try await withTaskPriorityEscalationHandler {
///       try await awaiterBox.take()!.wait()
///     } onPriorityEscalated: { _, new in executor.escalatePriority(of: registration, to: new) }
///   } onCancel: { executor.cancel(registration) }
/// }
/// ```
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 9999, *)
public protocol OperationExecutor: Executor {
  /// Cancels the asynchronous operation.
  ///
  /// Cancellation is a best effort and the operation might still successfully complete.
  ///
  /// - Parameter registration: The registration returned by `enqueue`.
  @available(SwiftStdlib 9999, *)
  func cancel(_ registration: OperationExecutorRegistration)

  /// Escalates the priority of the asynchronous operation.
  ///
  /// - Parameters:
  ///   - registration: The registration returned by `enqueue`.
  ///   - newPriority: The new priority.
  @available(SwiftStdlib 9999, *)
  func escalatePriority(
    of registration: OperationExecutorRegistration,
    to newPriority: TaskPriority
  )
}
#endif

extension Task where Success == Never, Failure == Never {
  #if !$Embedded
  /// Walk the current executor-preference chain -- the active serial (actor)
  /// executor, then the preferred task executor, then the current task
  /// executor, and finally the default executor -- and return the first
  /// executor for which `cast` succeeds.
  ///
  /// The clocks use this to find the executor that services a sleep: `cast`
  /// tests for the *specific* clock refinement (e.g. ``ContinuousClockExecutor``),
  /// so a custom actor or task executor that implements that clock is used when
  /// the task is running on it -- letting it intercept, or even block, sleeps --
  /// and otherwise the search falls through to the default executor.  Matching
  /// on the concrete clock rather than on ``OperationExecutor`` is what lets an
  /// executor that implements only one clock coexist with the default executor
  /// for the other.
  @available(StdlibDeploymentTarget 9999, *)
  internal static func _currentOperationExecutor<E>(
    _ cast: (any Executor) -> E?
  ) -> E? {
    if let activeExecutor = unsafe _getActiveExecutor().asSerialExecutor(),
       let match = cast(activeExecutor) {
      return match
    }
    if let taskExecutor = unsafe _getPreferredTaskExecutor().asTaskExecutor(),
       let match = cast(taskExecutor) {
      return match
    }
    if let taskExecutor = unsafe _getCurrentTaskExecutor().asTaskExecutor(),
       let match = cast(taskExecutor) {
      return match
    }
    return cast(defaultExecutor)
  }
  #endif
}
