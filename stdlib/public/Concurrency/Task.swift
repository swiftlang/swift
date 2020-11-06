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

// ==== Task -------------------------------------------------------------------

/// An asynchronous task (just "Task" hereafter) is the analogue of a thread for
/// asynchronous functions. All asynchronous functions run as part of some task.
///
/// A task can only be interacted with by code running "in" the task,
/// by invoking the appropriate context sensitive static functions which operate
/// on the "current" task. Because all such functions are `async` they can only
/// be invoked as part of an existing task, and therefore are guaranteed to be
/// effective.
///
/// A task's execution can be seen as a series of periods where the task was
/// running. Each such period ends at a suspension point or -- finally -- the
/// completion of the task.
///
/// These partial periods towards the task's completion are `PartialAsyncTask`.
/// Partial tasks are generally not interacted with by end-users directly,
/// unless implementing a scheduler.
public enum Task {
}

// ==== Task Priority ----------------------------------------------------------

extension Task {

  /// Returns the current task's priority.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  /* @instantaneous */
  public static func currentPriority() async -> Priority {
    fatalError("\(#function) not implemented yet.")
  }

  /// Task priority may inform decisions an `Executor` makes about how and when
  /// to schedule tasks submitted to it.
  ///
  /// ### Priority scheduling
  /// An executor MAY utilize priority information to attempt running higher
  /// priority tasks first, and then continuing to serve lower priority tasks.
  ///
  /// The exact semantics of how priority is treated are left up to each
  /// platform and `Executor` implementation.
  ///
  /// ### Priority inheritance
  /// Child tasks automatically inherit their parent task's priority.
  ///
  /// Detached tasks (created by `Task.runDetached`) DO NOT inherit task priority,
  /// as they are "detached" from their parent tasks after all.
  ///
  /// ### Priority elevation
  /// In some situations the priority of a task must be elevated (or "escalated", "raised"):
  ///
  /// - if a `Task` running on behalf of an actor, and a new higher-priority
  ///   task is enqueued to the actor, its current task must be temporarily
  ///   elevated to the priority of the enqueued task, in order to allow the new
  ///   task to be processed at--effectively-- the priority it was enqueued with.
  ///   - this DOES NOT affect `Task.currentPriority()`.
  /// - if a task is created with a `Task.Handle`, and a higher-priority task
  ///   calls the `await handle.get()` function the priority of this task must be
  ///   permanently increased until the task completes.
  ///   - this DOES affect `Task.currentPriority()`.
  ///
  /// TODO: Define the details of task priority; It is likely to be a concept
  ///       similar to Darwin Dispatch's QoS; bearing in mind that priority is not as
  ///       much of a thing on other platforms (i.e. server side Linux systems).
  public struct Priority: Comparable {
    public static let `default`: Task.Priority = .init() // TODO: replace with actual values

    // TODO: specifics of implementation are not decided yet
    private let __value: Int = 0

    public static func < (lhs: Self, rhs: Self) -> Bool {
      lhs.__value < rhs.__value
    }
  }
}

// ==== Task Handle ------------------------------------------------------------

extension Task {

  /// A task handle refers to an in-flight `Task`,
  /// allowing for potentially awaiting for its result or canceling it.
  ///
  /// It is not a programming error to drop a handle without awaiting or canceling it,
  /// i.e. the task will run regardless of the handle still being present or not.
  /// Dropping a handle however means losing the ability to await on the task's result
  /// and losing the ability to cancel it.
  @_frozen
  public struct Handle<Success> {
    private let task: Builtin.NativeObject

    /// Wait for the task to complete, returning (or throwing) its result.
    ///
    /// ### Priority
    /// If the task has not completed yet, its priority will be elevated to the
    /// priority of the current task. Note that this may not be as effective as
    /// creating the task with the "right" priority to in the first place.
    ///
    /// ### Cancellation
    /// If the awaited on task gets cancelled externally the `get()` will throw
    /// a cancellation error.
    ///
    /// If the task gets cancelled internally, e.g. by checking for cancellation
    /// and throwing a specific error or using `checkCancellation` the error
    /// thrown out of the task will be re-thrown here.
    public func get() async throws -> Success {
      fatalError("\(#function) not implemented yet.")
    }

    /// Attempt to cancel the task.
    ///
    /// Whether this function has any effect is task-dependent.
    ///
    /// For a task to respect cancellation it must cooperatively check for it
    /// while running. Many tasks will check for cancellation before beginning
    /// their "actual work", however this is not a requirement nor is it guaranteed
    /// how and when tasks check for cancellation in general.
    public func cancel() {
      Builtin.cancelAsyncTask(task)
    }
  }
}

// ==== Detached Tasks ---------------------------------------------------------

extension Task {
  /// Run given `operation` as part of a new top-level task.
  ///
  /// Creating detached tasks should, generally, be avoided in favor of using
  /// `async` functions, `async let` declarations and `await` expressions - as
  /// those benefit from structured, bounded concurrency which is easier to reason
  /// about, as well as automatically inheriting the parent tasks priority,
  /// task-local storage, deadlines, as well as being cancelled automatically
  /// when their parent task is cancelled. Detached tasks do not get any of those
  /// benefits, and thus should only be used when an operation is impossible to
  /// be modelled with child tasks.
  ///
  /// ### Cancellation
  /// A detached task always runs to completion unless it is explicitly cancelled.
  /// Specifically, dropping a detached tasks `Task.Handle` does _not_ automatically
  /// cancel given task.
  ///
  /// Canceling a task must be performed explicitly via `handle.cancel()`.
  ///
  /// - Note: it is generally preferable to use child tasks rather than detached
  ///   tasks. Child tasks automatically carry priorities, task-local state,
  ///   deadlines and have other benefits resulting from the structured
  ///   concurrency concepts that they model. Consider using detached tasks only
  ///   when strictly necessary and impossible to model operations otherwise.
  ///
  /// - Parameters:
  ///   - priority: priority of the task TODO: reword and define more explicitly once we have priorities well-defined
  ///   - operation: the operation to execute
  /// - Returns: handle to the task, allowing to `await handle.get()` on the
  ///     tasks result or `cancel` it.
  public static func runDetached<T>(
    priority: Priority = .default,
    operation: () async -> T
  ) -> Handle<T> {
    fatalError("\(#function) not implemented yet.")
  }

  /// Run given throwing `operation` as part of a new top-level task.
  ///
  /// Creating detached tasks should, generally, be avoided in favor of using
  /// `async` functions, `async let` declarations and `await` expressions - as
  /// those benefit from structured, bounded concurrency which is easier to reason
  /// about, as well as automatically inheriting the parent tasks priority,
  /// task-local storage, deadlines, as well as being cancelled automatically
  /// when their parent task is cancelled. Detached tasks do not get any of those
  /// benefits, and thus should only be used when an operation is impossible to
  /// be modelled with child tasks.
  ///
  /// ### Cancellation
  /// A detached task always runs to completion unless it is explicitly cancelled.
  /// Specifically, dropping a detached tasks `Task.Handle` does _not_ automatically
  /// cancel given task.
  ///
  /// Canceling a task must be performed explicitly via `handle.cancel()`.
  ///
  /// - Note: it is generally preferable to use child tasks rather than detached
  ///   tasks. Child tasks automatically carry priorities, task-local state,
  ///   deadlines and have other benefits resulting from the structured
  ///   concurrency concepts that they model. Consider using detached tasks only
  ///   when strictly necessary and impossible to model operations otherwise.
  ///
  /// - Parameters:
  ///   - priority: priority of the task TODO: reword and define more explicitly once we have priorities well-defined
  ///   - operation: the operation to execute
  /// - Returns: handle to the task, allowing to `await handle.get()` on the
  ///     tasks result or `cancel` it. If the operation fails the handle will
  ///     throw the error the operation has thrown when awaited on.
  public static func runDetached<T>(
    priority: Priority = .default,
    operation: () async throws -> T
  ) -> Handle<T> {
    fatalError("\(#function) not implemented yet.")
  }
}

// ==== Voluntary Suspension -----------------------------------------------------
extension Task {

  /// Suspend until a given point in time.
  ///
  /// ### Cancellation
  /// Does not check for cancellation and suspends the current context until the
  /// given deadline.
  ///
  /// - Parameter until: point in time until which to suspend.
  public static func sleep(until: Deadline) async {
    fatalError("\(#function) not implemented yet.")
  }

  /// Explicitly suspend the current task, potentially giving up execution actor
  /// of current actor/task, allowing other tasks to execute.
  ///
  /// This is not a perfect cure for starvation;
  /// if the task is the highest-priority task in the system, it might go
  /// immediately back to executing.
  public static func yield() async {
    fatalError("\(#function) not implemented yet.")
  }
}

// ==== UnsafeContinuation -----------------------------------------------------

extension Task {
  public struct UnsafeContinuation<T> {
    /// Return a value into the continuation and make the task schedulable.
    ///
    /// The task will never run synchronously, even if the task does not
    /// need to be resumed on a specific executor.
    ///
    /// This is appropriate when the caller is something "busy", like an event
    /// loop, and doesn't want to be potentially delayed by arbitrary work.
    public func resume(returning: T) {
      fatalError("\(#function) not implemented yet.")
    }
  }

  public struct UnsafeThrowingContinuation<T, E: Error> {
    /// Return a value into the continuation and make the task schedulable.
    ///
    /// The task will never run synchronously, even if the task does not
    /// need to be resumed on a specific executor.
    ///
    /// This is appropriate when the caller is something "busy", like an event
    /// loop, and doesn't want to be potentially delayed by arbitrary work.
    public func resume(returning: T) {
      fatalError("\(#function) not implemented yet.")
    }

    /// Resume the continuation with an error and make the task schedulable.
    ///
    /// The task will never run synchronously, even if the task does not
    /// need to be resumed on a specific executor.
    ///
    /// This is appropriate when the caller is something "busy", like an event
    /// loop, and doesn't want to be potentially delayed by arbitrary work.
    public func resume(throwing: E) {
      fatalError("\(#function) not implemented yet.")
    }
  }

  /// The operation functions must resume the continuation *exactly once*.
  ///
  /// The continuation will not begin executing until the operation function returns.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  /* @instantaneous */
  public static func withUnsafeContinuation<T>(
    operation: (UnsafeContinuation<T>) -> Void
  ) async -> T {
    fatalError("\(#function) not implemented yet.")
  }

  /// The operation functions must resume the continuation *exactly once*.
  ///
  /// The continuation will not begin executing until the operation function returns.
  ///
  /// ### Suspension
  /// This function returns instantly and will never suspend.
  /* @instantaneous */
  public static func withUnsafeThrowingContinuation<T>(
    operation: (UnsafeThrowingContinuation<T, Error>) -> Void
  ) async throws -> T {
    fatalError("\(#function) not implemented yet.")
  }
}
