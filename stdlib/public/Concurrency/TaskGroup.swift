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
import Dispatch
@_implementationOnly import _SwiftConcurrencyShims

// ==== Task Group -------------------------------------------------------------

extension Task {

  /// Starts a new task group which provides a scope in which a dynamic number of
  /// tasks may be spawned.
  ///
  /// Tasks added to the group by `group.add()` will automatically be awaited on
  /// when the scope exits. If the group exits by throwing, all added tasks will
  /// be cancelled and their results discarded.
  ///
  /// ### Implicit awaiting
  /// When results of tasks added to the group need to be collected, one can
  /// gather their results using the following pattern:
  ///
  ///     while let result = await group.next() {
  ///       // some accumulation logic (e.g. sum += result)
  ///     }
  ///
  /// ### Thrown errors
  /// When tasks are added to the group using the `group.add` function, they may
  /// immediately begin executing. Even if their results are not collected explicitly
  /// and such task throws, and was not yet cancelled, it may result in the `withGroup`
  /// throwing.
  ///
  /// ### Cancellation
  /// If an error is thrown out of the task group, all of its remaining tasks
  /// will be cancelled and the `withGroup` call will rethrow that error.
  ///
  /// Individual tasks throwing results in their corresponding `try group.next()`
  /// call throwing, giving a chance to handle individual errors or letting the
  /// error be rethrown by the group.
  ///
  /// Postcondition:
  /// Once `withGroup` returns it is guaranteed that the `group` is *empty*.
  ///
  /// This is achieved in the following way:
  /// - if the body returns normally:
  ///   - the group will await any not yet complete tasks,
  ///     - if any of those tasks throws, the remaining tasks will be cancelled,
  ///   - once the `withGroup` returns the group is guaranteed to be empty.
  /// - if the body throws:
  ///   - all tasks remaining in the group will be automatically cancelled.
  // TODO: Do we have to add a different group type to accommodate throwing
  //       tasks without forcing users to use Result?  I can't think of how that
  //       could be propagated out of the callback body reasonably, unless we
  //       commit to doing multi-statement closure typechecking.
  public static func withGroup<TaskResult, BodyResult>(
    resultType: TaskResult.Type,
    returning returnType: BodyResult.Type = BodyResult.self,
    cancelOutstandingTasksOnReturn: Bool = false,
    body: @escaping ((inout Task.Group<TaskResult>) async throws -> BodyResult)
  ) async throws -> BodyResult {
    let drainPendingTasksOnSuccessfulReturn = !cancelOutstandingTasksOnReturn
    let parent = Builtin.getCurrentAsyncTask()

    // Set up the job flags for a new task.
    var groupFlags = JobFlags()
    groupFlags.kind = .task // TODO: .taskGroup?
    groupFlags.priority = await Task.currentPriority()
    groupFlags.isFuture = true
    groupFlags.isChildTask = true

    // 1. Prepare the Group task
    // FIXME: do we have to rather prepare it inside the task we spawn; and yield it back along with the result instead?
    var group = Task.Group<TaskResult>(parentTask: parent)

    let (groupTask, context) =
      Builtin.createAsyncTaskFuture(groupFlags.bits, parent) { () async throws -> BodyResult in
        await try body(&group)
      }
    let groupHandle = Handle<BodyResult>(task: groupTask)

    // 2.0) Run the task!
    DispatchQueue.global(priority: .default).async { // TODO: use executors when they land
      groupHandle.run()
    }

    // 2.1) ensure that if we fail and exit by throwing we will cancel all tasks,
    // if we succeed, there is nothing to cancel anymore so this is noop
    defer { group.cancelAll() }

    // 2.2) Await the group completing it's run ("until the withGroup returns")
    let result = await try groupHandle.get() // if we throw, so be it -- group tasks will be cancelled

// TODO: do drain before exiting
//    if drainPendingTasksOnSuccessfulReturn {
//      // drain all outstanding tasks
//      while await try group.next() != nil {
//        continue // awaiting all remaining tasks
//      }
//    }

    return result
  }

  /// A task group serves as storage for dynamically started tasks.
  ///
  /// Its intended use is with the `Task.withGroup` function.
  /* @unmoveable */
  public struct Group<TaskResult> {
    private let parentTask: Builtin.NativeObject

    // TODO: we want groups to be unordered in completion, the counterpart to streams (Series),
    // as such it feels like we need to keep them like this, because a next() can complete any of them
    // and then we need to remove it from the pending ones
    private var pendingTasks: [Int: Handle<TaskResult>] // TODO: make a dict for out of order completions
    private var nextTaskID: Int = 0

    /// If present, the handle on which the `next()` call is awaiting,
    /// it should be resumed by *any* of the in-flight tasks completing.
    private var nextHandle: Task.Handle<TaskResult>? = nil

    /// No public initializers
    init(parentTask: Builtin.NativeObject) {
      self.parentTask = parentTask
      self.pendingTasks = [:]
    }

    // Swift will statically prevent this type from being copied or moved.
    // For now, that implies that it cannot be used with generics.

    /// Add a child task to the group.
    ///
    /// ### Error handling
    /// Operations are allowed to `throw`, in which case the `await try next()`
    /// invocation corresponding to the failed task will re-throw the given task.
    ///
    /// The `add` function will never (re)-throw exceptions from the `operation`,
    /// the corresponding `next()` call will throw the error when necessary.
    ///
    /// - Parameters:
    ///   - overridingPriority: override priority of the operation task
    ///   - operation: operation to execute and add to the group
    @discardableResult
    public mutating func add(
      overridingPriority: Priority? = nil,
      operation: @escaping () async throws -> TaskResult
    ) async -> Task.Handle<TaskResult> {
      var flags = JobFlags()
      flags.kind = .task // TODO: childTask?
      flags.priority = .default // TODO: priority getting from parent not implemented yet
//      if let overridingPriority = overridingPriority { // TODO: cannot use ?? with async defaultValue
//        flags.priority = overridingPriority
//      } else {
//        flags.priority = await Task.currentPriority() // TODO: self.parent.priority ?
//      }
      flags.isFuture = true

      let (childTask, context) =
        // TODO: passing the parentTask (instead of nil) here makes the program hang here
        Builtin.createAsyncTaskFuture(flags.bits, nil, operation)

      let handle = Handle<TaskResult>(task: childTask)

      // runTask(childTask)
        DispatchQueue.global(priority: .default).async {
          handle.run()
        }

//      _ = DispatchQueue.global(priority: .default).async {
//          print("run dispatch INSIDE: \(childTask)")
//          await try operation()
//        }

      // FIXME: need to store? self.pendingTasks[ObjectIdentifier(childTask)] = childTask

      defer { nextTaskID += 1 }
      self.pendingTasks[nextTaskID] = handle

      return handle
    }

    /// Wait for a child task to complete and return the result it returned,
    /// or else return.
    ///
    /// Order of completions is *not* guaranteed to be same as submission order,
    /// rather the order of `next()` calls completing is by completion order of
    /// the tasks. This differentiates task groups from streams (
    public mutating func next(file: String = #file, line: UInt = #line) async throws -> TaskResult? {
      // FIXME: this implementation is wrong and naive; we instead need to maintain a dict of handles,
      //        and return them as they complete in that order; so likely a queue of "which one completed"
      //        this will allow building "collect first N results" APIs easily;
      //        APIs which need order can implement on top of this, or we provide a different API for it
      let handle = self.pendingTasks.removeValue(forKey: 0) ??
        self.pendingTasks.removeValue(forKey: 1)

      if let handle = handle {
        let got = await try handle.get()
        return got
      } else {
        return nil
      }
    }

    /// Query whether the group has any remaining tasks.
    ///
    /// Task groups are always empty upon entry to the `withGroup` body, and
    /// become empty again when `withGroup` returns (either by awaiting on all
    /// pending tasks or cancelling them).
    ///
    /// - Returns: `true` if the group has no pending tasks, `false` otherwise.
    public var isEmpty: Bool {
      return self.pendingTasks.isEmpty
    }

    /// Cancel all the remaining tasks in the group.
    ///
    /// A cancelled group will not will NOT accept new tasks being added into it.
    ///
    /// Any results, including errors thrown by tasks affected by this
    /// cancellation, are silently discarded.
    ///
    /// - SeeAlso: `Task.addCancellationHandler`
    public mutating func cancelAll(file: String = #file, line: UInt = #line) {
      for (id, handle) in self.pendingTasks {
        handle.cancel()
      }
      self.pendingTasks = [:]
    }
  }
}
