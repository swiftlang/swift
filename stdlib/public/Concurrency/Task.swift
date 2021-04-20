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
/// These partial periods towards the task's completion are `PartialAsyncTask`.
/// Partial tasks are generally not interacted with by end-users directly,
/// unless implementing a scheduler.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct Task {
  internal let _task: Builtin.NativeObject

  // May only be created by the standard library.
  internal init(_ task: Builtin.NativeObject) {
    self._task = task
  }
}

// ==== Current Task -----------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {

  /// Returns 'current' `Task` instance, representing the task from within which
  /// this function was called.
  ///
  /// All functions available on the Task
  public static var current: Task? {
    guard let _task = _getCurrentAsyncTask() else {
      return nil
    }

    // FIXME: This retain seems pretty wrong, however if we don't we WILL crash
    //        with "destroying a task that never completed" in the task's destroy.
    //        How do we solve this properly?
    Builtin.retain(_task)

    return Task(_task)
  }

}

// ==== Task Priority ----------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {

  /// Returns the `current` task's priority.
  ///
  /// If no current `Task` is available, returns `Priority.default`.
  ///
  /// - SeeAlso: `Task.Priority`
  /// - SeeAlso: `Task.priority`
  public static var currentPriority: Priority {
    withUnsafeCurrentTask { task in
      task?.priority ?? Priority.default
    }
  }

  /// Returns the `current` task's priority.
  ///
  /// If no current `Task` is available, returns `Priority.default`.
  ///
  /// - SeeAlso: `Task.Priority`
  /// - SeeAlso: `Task.currentPriority`
  public var priority: Priority {
    getJobFlags(_task).priority
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
    case unspecified     = 0x00

    public static func < (lhs: Priority, rhs: Priority) -> Bool {
      lhs.rawValue < rhs.rawValue
    }
  }
}

// ==== Task Handle ------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {
  /// A task handle refers to an in-flight `Task`,
  /// allowing for potentially awaiting for its result or Cancelling it.
  ///
  /// It is not a programming error to drop a handle without awaiting or cancelling it,
  /// i.e. the task will run regardless of the handle still being present or not.
  /// Dropping a handle however means losing the ability to await on the task's result
  /// and losing the ability to cancel it.
  public struct Handle<Success, Failure: Error>: Sendable {
    internal let _task: Builtin.NativeObject

    internal init(_ task: Builtin.NativeObject) {
      self._task = task
    }

    /// Returns the `Task` that this handle refers to.
    public var task: Task {
      Task(_task)
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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task.Handle: Hashable {
  public func hash(into hasher: inout Hasher) {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(_task)).hash(into: &hasher)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task.Handle: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(lhs._task)) ==
      UnsafeRawPointer(Builtin.bridgeToRawPointer(rhs._task))
  }
}

// ==== Conformances -----------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task: Hashable {
  public func hash(into hasher: inout Hasher) {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(_task)).hash(into: &hasher)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(lhs._task)) ==
      UnsafeRawPointer(Builtin.bridgeToRawPointer(rhs._task))
  }
}

// ==== Job Flags --------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {
  /// Flags for schedulable jobs.
  ///
  /// This is a port of the C++ FlagSet.
  struct JobFlags {
    /// Kinds of schedulable jobs.
    enum Kind: Int {
      case task = 0
    }

    /// The actual bit representation of these flags.
    var bits: Int = 0

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
    var priority: Priority {
      get {
        Priority(rawValue: (bits & 0xFF00) >> 8)!
      }

      set {
        bits = (bits & ~0xFF00) | (newValue.rawValue << 8)
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

  }
}

// ==== Detached Tasks ---------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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
  let (task, _) = Builtin.createAsyncTaskFuture(flags.bits, operation)

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
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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
  let (task, _) = Builtin.createAsyncTaskFuture(flags.bits, operation)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return Task.Handle<T, Error>(task)
}

// ==== Async Handler ----------------------------------------------------------

// TODO: remove this?
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func _runAsyncHandler(operation: @escaping () async -> ()) {
  typealias ConcurrentFunctionType = @Sendable () async -> ()
  detach(
    operation: unsafeBitCast(operation, to: ConcurrentFunctionType.self)
  )
}

// ==== Async Sleep ------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {
  /// Suspends the current task for _at least_ the given duration
  /// in nanoseconds.
  ///
  /// This function does _not_ block the underlying thread.
  public static func sleep(_ duration: UInt64) async {
    // Set up the job flags for a new task.
    var flags = Task.JobFlags()
    flags.kind = .task
    flags.priority = .default
    flags.isFuture = true

    // Create the asynchronous task future.
    let (task, _) = Builtin.createAsyncTaskFuture(flags.bits, {})

    // Enqueue the resulting job.
    _enqueueJobGlobalWithDelay(duration, Builtin.convertTaskToJob(task))

    await Handle<Void, Never>(task).get()
  }
}

// ==== Voluntary Suspension -----------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {

  /// Explicitly suspend the current task, potentially giving up execution actor
  /// of current actor/task, allowing other tasks to execute.
  ///
  /// This is not a perfect cure for starvation;
  /// if the task is the highest-priority task in the system, it might go
  /// immediately back to executing.
  public static func yield() async {
    // Prepare the job flags
    var flags = JobFlags()
    flags.kind = .task
    flags.priority = .default
    flags.isFuture = true

    // Create the asynchronous task future, it will do nothing, but simply serves
    // as a way for us to yield our execution until the executor gets to it and
    // resumes us.
    // TODO: consider if it would be useful for this task to be a child task
    let (task, _) = Builtin.createAsyncTaskFuture(flags.bits, {})

    // Enqueue the resulting job.
    _enqueueJobGlobal(Builtin.convertTaskToJob(task))

    let _ = await Handle<Void, Never>(task).get()
  }
}

// ==== UnsafeCurrentTask ------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Task {

  @available(*, deprecated, message: "`Task.unsafeCurrent` was replaced by `withUnsafeCurrentTask { task in ... }`, and will be removed soon.")
  public static var unsafeCurrent: UnsafeCurrentTask? {
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
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public struct UnsafeCurrentTask {
  internal let _task: Builtin.NativeObject

  // May only be created by the standard library.
  internal init(_ task: Builtin.NativeObject) {
    self._task = task
  }

  /// Returns `Task` representing the same asynchronous context as this 'UnsafeCurrentTask'.
  ///
  /// Operations on `Task` (unlike `UnsafeCurrentTask`) are safe to be called
  /// from any other task (or thread).
  public var task: Task {
    Task(_task)
  }

  /// Returns `true` if the task is cancelled, and should stop executing.
  ///
  /// - SeeAlso: `checkCancellation()`
  public var isCancelled: Bool {
    _taskIsCancelled(_task)
  }

  /// Returns the `current` task's priority.
  ///
  /// If no current `Task` is available, returns `Priority.default`.
  ///
  /// - SeeAlso: `Task.Priority`
  /// - SeeAlso: `Task.currentPriority`
  public var priority: Task.Priority {
    getJobFlags(_task).priority
  }

}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeCurrentTask: Hashable {
  public func hash(into hasher: inout Hasher) {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(_task)).hash(into: &hasher)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeCurrentTask: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(lhs._task)) ==
      UnsafeRawPointer(Builtin.bridgeToRawPointer(rhs._task))
  }
}

// ==== Internal ---------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_getCurrent")
func _getCurrentAsyncTask() -> Builtin.NativeObject?

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_getJobFlags")
func getJobFlags(_ task: Builtin.NativeObject) -> Task.JobFlags

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_enqueueGlobal")
@usableFromInline
func _enqueueJobGlobal(_ task: Builtin.Job)

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_enqueueGlobalWithDelay")
@usableFromInline
func _enqueueJobGlobalWithDelay(_ delay: UInt64, _ task: Builtin.Job)

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_asyncMainDrainQueue")
public func _asyncMainDrainQueue() -> Never

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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
// we can do a move out of the future in the common case where it's
// unreferenced
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_future_wait")
public func _taskFutureGet<T>(_ task: Builtin.NativeObject) async -> T

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_future_wait_throwing")
public func _taskFutureGetThrowing<T>(_ task: Builtin.NativeObject) async throws -> T

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public func _runChildTask<T>(
  operation: @Sendable @escaping () async throws -> T
) async -> Builtin.NativeObject {
  let currentTask = Builtin.getCurrentAsyncTask()

  // Set up the job flags for a new task.
  var flags = Task.JobFlags()
  flags.kind = .task
  flags.priority = getJobFlags(currentTask).priority
  flags.isFuture = true
  flags.isChildTask = true

  // Create the asynchronous task future.
  let (task, _) = Builtin.createAsyncTaskFuture(
      flags.bits, operation)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return task
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_cancel")
func _taskCancel(_ task: Builtin.NativeObject)

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_task_isCancelled")
func _taskIsCancelled(_ task: Builtin.NativeObject) -> Bool

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@usableFromInline
@_silgen_name("swift_task_isCurrentExecutor")
func _taskIsCurrentExecutor(_ executor: Builtin.Executor) -> Bool

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@usableFromInline
@_silgen_name("swift_task_reportUnexpectedExecutor")
func _reportUnexpectedExecutor(_ _filenameStart: Builtin.RawPointer,
                               _ _filenameLength: Builtin.Word,
                               _ _filenameIsASCII: Builtin.Int1,
                               _ _line: Builtin.Word,
                               _ _executor: Builtin.Executor)

#if _runtime(_ObjC)

/// Intrinsic used by SILGen to launch a task for bridging a Swift async method
/// which was called through its ObjC-exported completion-handler-based API.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_alwaysEmitIntoClient
@usableFromInline
internal func _runTaskForBridgedAsyncMethod(_ body: @escaping () async -> Void) {
  // TODO: We can probably do better than detach
  // if we're already running on behalf of a task,
  // if the receiver of the method invocation is itself an Actor, or in other
  // situations.
#if compiler(>=5.5) && $Sendable
  detach { await body() }
#endif
}

#endif
