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
    getJobFlags(Builtin.getCurrentAsyncTask()).priority
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

extension Task {
  /// A task handle refers to an in-flight `Task`,
  /// allowing for potentially awaiting for its result or canceling it.
  ///
  /// It is not a programming error to drop a handle without awaiting or canceling it,
  /// i.e. the task will run regardless of the handle still being present or not.
  /// Dropping a handle however means losing the ability to await on the task's result
  /// and losing the ability to cancel it.
  public struct Handle<Success> {
    let task: Builtin.NativeObject

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
      return await try _taskFutureGetThrowing(task)
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

// ==== Job Flags --------------------------------------------------------------

extension Task {
  /// Flags for schedulable jobs.
  ///
  /// This is a port of the C++ FlagSet.
  struct JobFlags {
    /// Kinds of schedulable jobs.
    enum Kind: Int {
      case task = 0
    };

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

    /// Whether this is a channel.
    var isTaskGroup: Bool {
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

extension Task {
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
  ///   - priority: priority of the task
  ///   - operation: the operation to execute
  /// - Returns: handle to the task, allowing to `await handle.get()` on the
  ///     tasks result or `cancel` it. If the operation fails the handle will
  ///     throw the error the operation has thrown when awaited on.
  public static func runDetached<T>(
    priority: Priority = .default,
    operation: @escaping () async throws -> T
  ) -> Handle<T> {
    // Set up the job flags for a new task.
    var flags = JobFlags()
    flags.kind = .task
    flags.priority = priority
    flags.isFuture = true

    // Create the asynchronous task future.
    let (task, _) = Builtin.createAsyncTaskFuture(flags.bits, nil, operation)

    // Enqueue the resulting job.
    _enqueueJobGlobal(Builtin.convertTaskToJob(task))

    return Handle<T>(task: task)
  }

}

public func _runAsyncHandler(operation: @escaping () async -> ()) {
  _ = Task.runDetached(operation: operation)
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

@_silgen_name("swift_task_getJobFlags")
func getJobFlags(_ task: Builtin.NativeObject) -> Task.JobFlags

@_silgen_name("swift_task_enqueueGlobal")
@usableFromInline
func _enqueueJobGlobal(_ task: Builtin.Job)

@_silgen_name("swift_task_isCancelled")
func isTaskCancelled(_ task: Builtin.NativeObject) -> Bool

@_silgen_name("swift_task_runAndBlockThread")
public func runAsyncAndBlock(_ asyncFun: @escaping () async -> ())

@_silgen_name("swift_task_future_wait")
func _taskFutureWait(
  on task: Builtin.NativeObject
) async -> (hadErrorResult: Bool, storage: UnsafeRawPointer)

public func _taskFutureGet<T>(_ task: Builtin.NativeObject) async -> T {
  let rawResult = await _taskFutureWait(on: task)
  assert(!rawResult.hadErrorResult)

  // Take the value.
  let storagePtr = rawResult.storage.bindMemory(to: T.self, capacity: 1)
  return UnsafeMutablePointer<T>(mutating: storagePtr).pointee
}

public func _taskFutureGetThrowing<T>(
    _ task: Builtin.NativeObject
) async throws -> T {
  let rawResult = await _taskFutureWait(on: task)
  if rawResult.hadErrorResult {
    // Throw the result on error.
    throw unsafeBitCast(rawResult.storage, to: Error.self)
  }

  // Take the value on success
  let storagePtr =
    rawResult.storage.bindMemory(to: T.self, capacity: 1)
  return UnsafeMutablePointer<T>(mutating: storagePtr).pointee
}

public func _runChildTask<T>(
  operation: @escaping () async throws -> T
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
      flags.bits, currentTask, operation)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return task
}

public func _runGroupChildTask<T>(
  overridingPriority priorityOverride: Task.Priority? = nil,
  operation: @escaping () async throws -> T
) async -> Builtin.NativeObject {
  let currentTask = Builtin.getCurrentAsyncTask()

  // Set up the job flags for a new task.
  var flags = Task.JobFlags()
  flags.kind = .task
  flags.priority = priorityOverride ?? getJobFlags(currentTask).priority
  flags.isFuture = true
  flags.isChildTask = true

  // Create the asynchronous task future.
  let (task, _) = Builtin.createAsyncTaskFuture(
      flags.bits, currentTask, operation)

  // Enqueue the resulting job.
  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return task
}

@_silgen_name("swift_task_cancel")
func _taskCancel(_ task: Builtin.NativeObject)

@_silgen_name("swift_task_isCancelled")
func _taskIsCancelled(_ task: Builtin.NativeObject) -> Bool

#if _runtime(_ObjC)

/// Intrinsic used by SILGen to launch a task for bridging a Swift async method
/// which was called through its ObjC-exported completion-handler-based API.
@_alwaysEmitIntoClient
@usableFromInline
internal func _runTaskForBridgedAsyncMethod(_ body: @escaping () async -> Void) {
  // TODO: We can probably do better than Task.runDetached
  // if we're already running on behalf of a task,
  // if the receiver of the method invocation is itself an Actor, or in other
  // situations.
  _ = Task.runDetached { await body() }
}

#endif
