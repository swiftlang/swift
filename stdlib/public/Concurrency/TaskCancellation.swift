////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2020 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

// ==== Task Cancellation ------------------------------------------------------

extension Task {

  /// Returns `true` if the task is cancelled, and should stop executing.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  ///
  /// - SeeAlso: `checkCancellation()`
  /* @instantaneous */
  public static func isCancelled() async -> Bool {
     _taskIsCancelled(Builtin.getCurrentAsyncTask())
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
  /* @instantaneous */
  public static func checkCancellation() async throws {
    if await Task.isCancelled() {
      throw CancellationError()
    }
  }

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
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  /* @instantaneous */
  public static func withCancellationHandler<T>(
    handler: /* @concurrent */ () -> (),
    operation: () async throws -> T
  ) async throws -> T {
      fatalError("\(#function) not implemented yet.")
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

// ==== Task Deadlines ---------------------------------------------------------

extension Task {

  /// Returns the earliest deadline set on the current task.
  ///
  /// If no deadline was set for the task the `Deadline.distantFuture` is returned,
  /// as it is effective in conveying that there still is time remaining and the
  /// deadline is not overdue yet.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  /* @instantaneous */
  public static func currentDeadline() async -> Deadline {
    fatalError("\(#function) not implemented yet.")
  }

  /// Execute a code block with a deadline in `interval`.
  ///
  /// If the current task already has a deadline set that is _prior_
  /// to the newly set deadline with this API, that deadline will remain in effect.
  ///
  /// This allows higher level tasks to set an upper bound on the deadline they
  /// are willing cumulatively willing to wait for the entire task to execute,
  /// regardless of the inner deadlines of the specific child tasks.
  ///
  /// Cancellation is co-operative and must be checked for by the operation, e.g.
  /// by invoking `Task.checkCancellation`, or `Task.isCancelled`.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  ///
  /// - Parameters:
  ///   - interval: interval after which (from `now()`) the operation task should
  ///     be considered cancelled.
  ///   - operation: the operation to execute
  /* @instantaneous */
  public static func withDeadline<T>(
    in interval: _TimeInterval,
    operation: () async throws -> T
  ) async rethrows -> T {
    fatalError("\(#function) not implemented yet.")
  }

  /// Execute a code block with the passed in deadline (unless a shorter deadline is already set).
  ///
  /// If the current task already has a deadline set that is _prior_
  /// to the newly set deadline with this API, that deadline will remain in effect.
  ///
  /// This allows higher level tasks to set an upper bound on the deadline they
  /// are willing cumulatively willing to wait for the entire task to execute,
  /// regardless of the inner deadlines of the specific child tasks.
  ///
  /// Cancellation is co-operative and must be checked for by the operation, e.g.
  /// by invoking `Task.checkCancellation` or `Task.isCancelled`.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  ///
  /// - Parameters:
  ///   - deadline: the point in time after which the operation task should be
  ///     considered cancelled.
  ///   - operation: the operation to execute
  /* @instantaneous */
  public static func withDeadline<T>(
    _ deadline: Deadline,
    operation: () async throws -> T
  ) async rethrows -> T {
    fatalError("\(#function) not implemented yet.")
  }

  /// A deadline is a point in time past-which a task should be considered cancelled.
  ///
  /// Deadlines function the same was as pure cancellation, in the sense that they
  /// are cooperative and require the cancelled (deadline exceeding) task to check
  /// for this as it is performing its execution.
  ///
  /// Generally tasks (or partial tasks) should perform such check before they
  /// start executing, however this is not a strict rule, and some tasks may
  /// choose to be un-cancellable.
  public struct Deadline {
    public typealias WallTime = UInt64 // equivalent to DispatchWallTime
    internal let time: WallTime

    public init(at time: WallTime) {
      self.time = time
    }

    public static var distantFuture: Self {
      .init(at: .max)
    }

    public static func `in`(_ interval: _TimeInterval) -> Self {
      // now() + interval
      fatalError("#\(#function) not implemented yet.")
    }

    /// Returns `true` if the deadline is overdue and deadline should be
    /// considered overdue (or "exceeded").
    ///
    /// If this deadline was related to a `Task`, that task should be considered
    /// cancelled if the deadline is overdue.
    public var isOverdue: Bool {
      !self.hasTimeLeft
    }

    /// Returns `true` if the deadline is still pending with respect to "now".
    public var hasTimeLeft: Bool {
      fatalError("\(#function) not implemented yet.")// self.hasTimeLeft(until: now())
    }

    // TODO: public func hasTimeLeft(until: DispatchWallTime or whichever time type we'll use) -> Bool
    
  }
}
