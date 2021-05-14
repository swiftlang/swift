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
/// These partial periods towards the task's completion are
/// individually schedulable as jobs.  Jobs are generally not interacted
/// with by end-users directly, unless implementing a scheduler.
@available(SwiftStdlib 5.5, *)
public struct Task {
  // Task instances should not be used as they could be stored away,
  // and sine some tasks may be task-local allocated such stored away
  // references could point at already destroyed task memory (!).
  //
  // If necessary to obtain a task instance, please use withUnsafeCurrentTask.
}

// ==== Task Priority ----------------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension Task {

  /// Returns the `current` task's priority.
  ///
  /// If no current `Task` is available, queries the system to determine the
  /// priority at which the current function is running. If the system cannot
  /// provide an appropriate priority, returns `Priority.default`.
  ///
  /// - SeeAlso: `Task.Priority`
  /// - SeeAlso: `Task.priority`
  public static var currentPriority: Priority {
    withUnsafeCurrentTask { task in
      // If we are running on behalf of a task, use that task's priority.
      if let task = task {
        return task.priority
      }

      // Otherwise, query the system.
      return Task.Priority(rawValue: _getCurrentThreadPriority()) ?? .default
    }
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
  /// Detached tasks (created by `detach`) DO NOT inherit task priority,
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
  public enum Priority: Int, Comparable {
    // Values must be same as defined by the internal `JobPriority`.
    case userInteractive = 0x21
    case userInitiated   = 0x19
    case `default`       = 0x15
    case utility         = 0x11
    case background      = 0x09

    @available(*, deprecated, message: "unspecified priority will be removed; use nil")
    case unspecified     = 0x00

    public static func < (lhs: Priority, rhs: Priority) -> Bool {
      lhs.rawValue < rhs.rawValue
    }
  }
}

@available(SwiftStdlib 5.5, *)
extension Task.Priority {
  /// Downgrade user-interactive to user-initiated.
  var _downgradeUserInteractive: Task.Priority {
    if self == .userInteractive {
      return .userInitiated
    }

    return self
  }
}

// ==== Task Handle ------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension Task {
  /// A task handle refers to an in-flight `Task`,
  /// allowing for potentially awaiting for its result or Cancelling it.
  ///
  /// It is not a programming error to drop a handle without awaiting or cancelling it,
  /// i.e. the task will run regardless of the handle still being present or not.
  /// Dropping a handle however means losing the ability to await on the task's result
  /// and losing the ability to cancel it.
  ///
  // Implementation notes:
  // A task handle can ONLY be obtained for a detached task, and as such shares
  // no lifetime concerns with regards to holding and storing the `_task` with
  // the `Task` type, which would have also be obtainable for any task, including
  // a potentially task-local allocated one. I.e. it is always safe to store away
  // a Task.Handle, yet the same is not true for the "current task" which may be
  // a async-let created task, at risk of getting destroyed while the reference
  // lingers around.
  public struct Handle<Success, Failure: Error>: Sendable {
    internal let _task: Builtin.NativeObject

    internal init(_ task: Builtin.NativeObject) {
      self._task = task
    }

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
      return try await _taskFutureGetThrowing(_task)
    }

    /// Wait for the task to complete, returning its `Result`.
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
    public func getResult() async -> Result<Success, Failure> {
      do {
        return .success(try await get())
      } catch {
        return .failure(error as! Failure) // as!-safe, guaranteed to be Failure
      }
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
      Builtin.cancelAsyncTask(_task)
    }
  }
}

@available(SwiftStdlib 5.5, *)
extension Task.Handle where Failure == Never {

  /// Wait for the task to complete, returning its result.
  ///
  /// ### Priority
  /// If the task has not completed yet, its priority will be elevated to the
  /// priority of the current task. Note that this may not be as effective as
  /// creating the task with the "right" priority to in the first place.
  ///
  /// ### Cancellation
  /// The task this handle refers to may check for cancellation, however
  /// since it is not-throwing it would have to handle it using some other
  /// way than throwing a `CancellationError`, e.g. it could provide a neutral
  /// value of the `Success` type, or encode that cancellation has occurred in
  /// that type itself.
  public func get() async -> Success {
    return await _taskFutureGet(_task)
  }
  
}

@available(SwiftStdlib 5.5, *)
extension Task.Handle: Hashable {
  public func hash(into hasher: inout Hasher) {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(_task)).hash(into: &hasher)
  }
}

@available(SwiftStdlib 5.5, *)
extension Task.Handle: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(lhs._task)) ==
      UnsafeRawPointer(Builtin.bridgeToRawPointer(rhs._task))
  }
}

// ==== Job Flags --------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension Task {
  /// Flags for schedulable jobs.
  ///
  /// This is a port of the C++ FlagSet.
  struct JobFlags {
    /// Kinds of schedulable jobs.
    enum Kind: Int32 {
      case task = 0
    }

    /// The actual bit representation of these flags.
    var bits: Int32 = 0

    /// The kind of job described by these flags.
    var kind: Kind {
      get {
        Kind(rawValue: bits & 0xFF)!
      }

      set {
        bits = (bits & ~0xFF) | newValue.rawValue
      }
    }

    /// Whether this is an asynchronous task.
    var isAsyncTask: Bool { kind == .task }

    /// The priority given to the job.
    var priority: Priority? {
      get {
        Priority(rawValue: (Int(bits) & 0xFF00) >> 8)
      }

      set {
        bits = (bits & ~0xFF00) | Int32((newValue?.rawValue ?? 0) << 8)
      }
    }

    /// Whether this is a child task.
    var isChildTask: Bool {
      get {
        (bits & (1 << 24)) != 0
      }

      set {
        if newValue {
          bits = bits | 1 << 24
        } else {
          bits = (bits & ~(1 << 24))
        }
      }
    }

    /// Whether this is a future.
    var isFuture: Bool {
      get {
        (bits & (1 << 25)) != 0
      }

      set {
        if newValue {
          bits = bits | 1 << 25
        } else {
          bits = (bits & ~(1 << 25))
        }
      }
    }

    /// Whether this is a group child.
    var isGroupChildTask: Bool {
      get {
        (bits & (1 << 26)) != 0
      }

      set {
        if newValue {
          bits = bits | 1 << 26
        } else {
          bits = (bits & ~(1 << 26))
        }
      }
    }

    /// Whether this is a task created by the 'async' operation, which
    /// conceptually continues the work of the synchronous code that invokes
    /// it.
    var isContinuingAsyncTask: Bool {
      get {
        (bits & (1 << 27)) != 0
      }

      set {
        if newValue {
          bits = bits | 1 << 27
        } else {
          bits = (bits & ~(1 << 27))
        }
      }
    }
  }
}

// ==== Detached Tasks ---------------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension Task {

  @discardableResult
  @available(*, deprecated, message: "`Task.runDetached` was replaced by `detach` and will be removed shortly.")
  public static func runDetached<T>(
    priority: Task.Priority = .unspecified,
    operation: __owned @Sendable @escaping () async throws -> T
  ) -> Task.Handle<T, Error> {
    detach(priority: priority, operation: operation)
  }

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
/// Cancelling a task must be performed explicitly via `handle.cancel()`.
///
/// - Note: it is generally preferable to use child tasks rather than detached
///   tasks. Child tasks automatically carry priorities, task-local state,
///   deadlines and have other benefits resulting from the structured
///   concurrency concepts that they model. Consider using detached tasks only
///   when strictly necessary and impossible to model operations otherwise.
///
/// - Parameters:
///   - priority: priority of the task
///   - executor: the executor on which the detached closure should start
///               executing on.
///   - operation: the operation to execute
/// - Returns: handle to the task, allowing to `await handle.get()` on the
///     tasks result or `cancel` it. If the operation fails the handle will
///     throw the error the operation has thrown when awaited on.
@discardableResult
@available(SwiftStdlib 5.5, *)
public func detach<T>(
  priority: Task.Priority = .unspecified,
  operation: __owned @Sendable @escaping () async -> T
) -> Task.Handle<T, Never> {
  // Set up the job flags for a new task.
  var flags = Task.JobFlags()
  flags.kind = .task
  flags.priority = priority
  flags.isFuture = true

  // Create the asynchronous task future.
  let (task, _) = Builtin.createAsyncTaskFuture(Int(flags.bits), operation)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return Task.Handle<T, Never>(task)
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
/// Cancelling a task must be performed explicitly via `handle.cancel()`.
///
/// - Note: it is generally preferable to use child tasks rather than detached
///   tasks. Child tasks automatically carry priorities, task-local state,
///   deadlines and have other benefits resulting from the structured
///   concurrency concepts that they model. Consider using detached tasks only
///   when strictly necessary and impossible to model operations otherwise.
///
/// - Parameters:
///   - priority: priority of the task
///   - executor: the executor on which the detached closure should start
///               executing on.
///   - operation: the operation to execute
/// - Returns: handle to the task, allowing to `await handle.get()` on the
///     tasks result or `cancel` it. If the operation fails the handle will
///     throw the error the operation has thrown when awaited on.
@discardableResult
@available(SwiftStdlib 5.5, *)
public func detach<T>(
  priority: Task.Priority = .unspecified,
  operation: __owned @Sendable @escaping () async throws -> T
) -> Task.Handle<T, Error> {
  // Set up the job flags for a new task.
  var flags = Task.JobFlags()
  flags.kind = .task
  flags.priority = priority
  flags.isFuture = true

  // Create the asynchronous task future.
  let (task, _) = Builtin.createAsyncTaskFuture(Int(flags.bits), operation)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return Task.Handle<T, Error>(task)
}

@discardableResult
@available(SwiftStdlib 5.5, *)
public func asyncDetached<T>(
  priority: Task.Priority? = nil,
  @_implicitSelfCapture operation: __owned @Sendable @escaping () async -> T
) -> Task.Handle<T, Never> {
  return detach(priority: priority ?? .unspecified, operation: operation)
}

@discardableResult
@available(SwiftStdlib 5.5, *)
public func asyncDetached<T>(
  priority: Task.Priority? = nil,
  @_implicitSelfCapture operation: __owned @Sendable @escaping () async throws -> T
) -> Task.Handle<T, Error> {
  return detach(priority: priority ?? .unspecified, operation: operation)
}

/// ABI stub while we stage in the new signatures
@available(SwiftStdlib 5.5, *)
@usableFromInline
func async(
  priority: Task.Priority,
  @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping () async -> Void
) {
  let adjustedPriority: Task.Priority?
  if priority == .unspecified {
    adjustedPriority = nil
  } else {
    adjustedPriority = priority
  }
  let _: Task.Handle = async(priority: adjustedPriority, operation: operation)
}

/// Run given `operation` as asynchronously in its own top-level task.
///
/// The `async` function should be used when creating asynchronous work
/// that operates on behalf of the synchronous function that calls it.
/// Like `detach`, the async function creates a separate, top-level task.
/// Unlike `detach`, the task creating by `async` inherits the priority and
/// actor context of the caller, so the `operation` is treated more like an
/// asynchronous extension to the synchronous operation. Additionally, `async`
/// does not return a handle to refer to the task.
///
/// - Parameters:
///   - priority: priority of the task. If nil, the priority will come from
///     Task.currentPriority.
///   - operation: the operation to execute
@available(SwiftStdlib 5.5, *)
@discardableResult
public func async<T>(
  priority: Task.Priority? = nil,
  @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping () async -> T
) -> Task.Handle<T, Never> {
  // Set up the job flags for a new task.
  var flags = Task.JobFlags()
  flags.kind = .task
  flags.priority = priority ?? Task.currentPriority._downgradeUserInteractive
  flags.isFuture = true
  flags.isContinuingAsyncTask = true

  // Create the asynchronous task future.
  let (task, _) = Builtin.createAsyncTaskFuture(Int(flags.bits), operation)

  // Copy all task locals to the newly created task.
  // We must copy them rather than point to the current task since the new task
  // is not structured and may out-live the current task.
  //
  // WARNING: This MUST be done BEFORE we enqueue the task,
  // because it acts as-if it was running inside the task and thus does not
  // take any extra steps to synchronize the task-local operations.
  _taskLocalsCopy(to: task)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return Task.Handle(task)
}

/// Run given `operation` as asynchronously in its own top-level task.
///
/// The `async` function should be used when creating asynchronous work
/// that operates on behalf of the synchronous function that calls it.
/// Like `detach`, the async function creates a separate, top-level task.
/// Unlike `detach`, the task creating by `async` inherits the priority and
/// actor context of the caller, so the `operation` is treated more like an
/// asynchronous extension to the synchronous operation. Additionally, `async`
/// does not return a handle to refer to the task.
///
/// - Parameters:
///   - priority: priority of the task. If nil, the priority will come from
///     Task.currentPriority.
///   - operation: the operation to execute
@available(SwiftStdlib 5.5, *)
@discardableResult
public func async<T>(
  priority: Task.Priority? = nil,
  @_inheritActorContext @_implicitSelfCapture operation: __owned @Sendable @escaping () async throws -> T
) -> Task.Handle<T, Error> {
  // Set up the job flags for a new task.
  var flags = Task.JobFlags()
  flags.kind = .task
  flags.priority = priority ?? Task.currentPriority._downgradeUserInteractive
  flags.isFuture = true
  flags.isContinuingAsyncTask = true

  // Create the asynchronous task future.
  let (task, _) = Builtin.createAsyncTaskFuture(Int(flags.bits), operation)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return Task.Handle(task)
}

// ==== Async Sleep ------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension Task {
  /// Suspends the current task for _at least_ the given duration
  /// in nanoseconds.
  ///
  /// This function does _not_ block the underlying thread.
  public static func sleep(_ duration: UInt64) async {
    let currentTask = Builtin.getCurrentAsyncTask()
    let priority = getJobFlags(currentTask).priority ?? Task.currentPriority._downgradeUserInteractive

    return await Builtin.withUnsafeContinuation { (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(priority: priority.rawValue, continuation: continuation)
      _enqueueJobGlobalWithDelay(duration, job)
    }
  }
}

// ==== Voluntary Suspension -----------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension Task {

  /// Explicitly suspend the current task, potentially giving up execution actor
  /// of current actor/task, allowing other tasks to execute.
  ///
  /// This is not a perfect cure for starvation;
  /// if the task is the highest-priority task in the system, it might go
  /// immediately back to executing.
  public static func yield() async {
    let currentTask = Builtin.getCurrentAsyncTask()
    let priority = getJobFlags(currentTask).priority ?? Task.currentPriority._downgradeUserInteractive

    return await Builtin.withUnsafeContinuation { (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(priority: priority.rawValue, continuation: continuation)
      _enqueueJobGlobal(job)
    }
  }
}

// ==== UnsafeCurrentTask ------------------------------------------------------

@available(SwiftStdlib 5.5, *)
extension Task {

  @available(*, deprecated, message: "`Task.unsafeCurrent` was replaced by `withUnsafeCurrentTask { task in ... }`, and will be removed soon.")
  public static var unsafeCurrent: UnsafeCurrentTask? { // TODO: remove as soon as possible
    guard let _task = _getCurrentAsyncTask() else {
      return nil
    }
    // FIXME: This retain seems pretty wrong, however if we don't we WILL crash
    //        with "destroying a task that never completed" in the task's destroy.
    //        How do we solve this properly?
    Builtin.retain(_task)
    return UnsafeCurrentTask(_task)
  }
}

/// Calls the given closure with the with the "current" task in which this
/// function was invoked.
///
/// If invoked from an asynchronous function the task will always be non-nil,
/// as an asynchronous function is always running within some task.
/// However if invoked from a synchronous function the task may be nil,
/// meaning that the function is not executing within a task, i.e. there is no
/// asynchronous context available in the call stack.
///
/// It is generally not safe to escape/store the `UnsafeCurrentTask` for future
/// use, as some operations on it may only be performed from the same task
/// that it is representing.
///
/// It is possible to obtain a `Task` fom the `UnsafeCurrentTask` which is safe
/// to access from other tasks or even store for future reference e.g. equality
/// checks.
@available(SwiftStdlib 5.5, *)
public func withUnsafeCurrentTask<T>(body: (UnsafeCurrentTask?) throws -> T) rethrows -> T {
  guard let _task = _getCurrentAsyncTask() else {
    return try body(nil)
  }

  // FIXME: This retain seems pretty wrong, however if we don't we WILL crash
  //        with "destroying a task that never completed" in the task's destroy.
  //        How do we solve this properly?
  Builtin.retain(_task)

  return try body(UnsafeCurrentTask(_task))
}

/// An *unsafe* 'current' task handle.
///
/// An `UnsafeCurrentTask` should not be stored for "later" access.
///
/// Storing an `UnsafeCurrentTask` has no implication on the task's actual lifecycle.
///
/// The sub-set of APIs of `UnsafeCurrentTask` which also exist on `Task` are
/// generally safe to be invoked from any task/thread.
///
/// All other APIs must not, be called 'from' any other task than the one
/// represented by this handle itself. Doing so may result in undefined behavior,
/// and most certainly will break invariants in other places of the program
/// actively running on this task.
@available(SwiftStdlib 5.5, *)
public struct UnsafeCurrentTask {
  internal let _task: Builtin.NativeObject

  // May only be created by the standard library.
  internal init(_ task: Builtin.NativeObject) {
    self._task = task
  }

  /// Returns `true` if the task is cancelled, and should stop executing.
  ///
  /// - SeeAlso: `checkCancellation()`
  public var isCancelled: Bool {
    _taskIsCancelled(_task)
  }

  /// Returns the `current` task's priority.
  ///
  /// - SeeAlso: `Task.Priority`
  /// - SeeAlso: `Task.currentPriority`
  public var priority: Task.Priority {
    getJobFlags(_task).priority ?? .default
  }

}

@available(SwiftStdlib 5.5, *)
extension UnsafeCurrentTask: Hashable {
  public func hash(into hasher: inout Hasher) {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(_task)).hash(into: &hasher)
  }
}

@available(SwiftStdlib 5.5, *)
extension UnsafeCurrentTask: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(lhs._task)) ==
      UnsafeRawPointer(Builtin.bridgeToRawPointer(rhs._task))
  }
}

// ==== Internal ---------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_getCurrent")
func _getCurrentAsyncTask() -> Builtin.NativeObject?

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_getJobFlags")
func getJobFlags(_ task: Builtin.NativeObject) -> Task.JobFlags

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_enqueueGlobal")
@usableFromInline
func _enqueueJobGlobal(_ task: Builtin.Job)

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_enqueueGlobalWithDelay")
@usableFromInline
func _enqueueJobGlobalWithDelay(_ delay: UInt64, _ task: Builtin.Job)

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_asyncMainDrainQueue")
public func _asyncMainDrainQueue() -> Never

@available(SwiftStdlib 5.5, *)
public func _runAsyncMain(_ asyncFun: @escaping () async throws -> ()) {
#if os(Windows)
  detach {
    do {
      try await asyncFun()
      exit(0)
    } catch {
      _errorInMain(error)
    }
  }
#else
  @MainActor @Sendable
  func _doMain(_ asyncFun: @escaping () async throws -> ()) async {
    do {
      try await asyncFun()
    } catch {
      _errorInMain(error)
    }
  }

  detach {
    await _doMain(asyncFun)
    exit(0)
  }
#endif
  _asyncMainDrainQueue()
}

// FIXME: both of these ought to take their arguments _owned so that
//        we can do a move out of the future in the common case where it's
//        unreferenced
@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_future_wait")
public func _taskFutureGet<T>(_ task: Builtin.NativeObject) async -> T

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_future_wait_throwing")
public func _taskFutureGetThrowing<T>(_ task: Builtin.NativeObject) async throws -> T

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_cancel")
func _taskCancel(_ task: Builtin.NativeObject)

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_isCancelled")
func _taskIsCancelled(_ task: Builtin.NativeObject) -> Bool

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_createNullaryContinuationJob")
func _taskCreateNullaryContinuationJob(priority: Int, continuation: Builtin.RawUnsafeContinuation) -> Builtin.Job

@available(SwiftStdlib 5.5, *)
@usableFromInline
@_silgen_name("swift_task_isCurrentExecutor")
func _taskIsCurrentExecutor(_ executor: Builtin.Executor) -> Bool

@available(SwiftStdlib 5.5, *)
@usableFromInline
@_silgen_name("swift_task_reportUnexpectedExecutor")
func _reportUnexpectedExecutor(_ _filenameStart: Builtin.RawPointer,
                               _ _filenameLength: Builtin.Word,
                               _ _filenameIsASCII: Builtin.Int1,
                               _ _line: Builtin.Word,
                               _ _executor: Builtin.Executor)

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_getCurrentThreadPriority")
func _getCurrentThreadPriority() -> Int

#if _runtime(_ObjC)

/// Intrinsic used by SILGen to launch a task for bridging a Swift async method
/// which was called through its ObjC-exported completion-handler-based API.
@available(SwiftStdlib 5.5, *)
@_alwaysEmitIntoClient
@usableFromInline
internal func _runTaskForBridgedAsyncMethod(_ body: @escaping () async -> Void) {
#if compiler(>=5.5) && $Sendable
  async { await body() }
#endif
}

#endif
