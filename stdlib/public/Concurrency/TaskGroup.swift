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
    body: @escaping ((inout Task.Group<TaskResult>) async throws -> BodyResult)
  ) async throws -> BodyResult {
    let parent = Builtin.getCurrentAsyncTask()

    // Set up the job flags for a new task.
    var groupFlags = JobFlags()
    groupFlags.kind = .task
    groupFlags.priority = getJobFlags(parent).priority
    groupFlags.isChildTask = true
    groupFlags.isTaskGroup = true
    groupFlags.isFuture = true

    let (groupTask, _) =
      Builtin.createAsyncTaskFuture(groupFlags.bits, parent) { () async throws -> BodyResult in
        let task = Builtin.getCurrentAsyncTask()
        var group = Task.Group<TaskResult>(task: task)

        // This defer handles both return/throw cases in the following way:
        // - body throws: all tasks must be cancelled immediately as we re-throw
        // - body returns: as good measure, cancel tasks after draining
        //   (although there should be none left by that time)
        defer { group.cancelAll() }

        let result = await try body(&group)

        /// Drain all remaining tasks by awaiting on them;
        /// Their failures are ignored;
        await group._tearDown()

        return result
      }

    // Enqueue the resulting job.
    _enqueueJobGlobal(Builtin.convertTaskToJob(groupTask))

    return await try Handle<BodyResult>(task: groupTask).get()
  }

  /// A task group serves as storage for dynamically started tasks.
  ///
  /// Its intended use is with the `Task.withGroup` function.
  /* @unmoveable */
  public struct Group<TaskResult> {
    /// Group task into which child tasks offer their results,
    /// and the `next()` function polls those results from.
    private let task: Builtin.NativeObject

    /// No public initializers
    init(task: Builtin.NativeObject) {
      self.task = task
      _swiftRetain(task)
    }

    // Swift will statically prevent this type from being copied or moved.
    // For now, that implies that it cannot be used with generics.

    /// Add a child task to the group.
    ///
    /// ### Error handling
    /// Operations are allowed to `throw`, in which case the `await try next()`
    /// invocation corresponding to the failed task will re-throw the given task.
    ///
    /// The `add` function will never (re-)throw errors from the `operation`.
    /// Instead, the corresponding `next()` call will throw the error when necessary.
    ///
    /// - Parameters:
    ///   - overridingPriority: override priority of the operation task
    ///   - operation: operation to execute and add to the group
    @discardableResult
    public mutating func add(
      overridingPriority priorityOverride: Priority? = nil,
      operation: @escaping () async throws -> TaskResult
    ) async -> Task.Handle<TaskResult> {
      // Increment the number of pending tasks immediately;
      // We don't need to know which specific task is pending, just that pending
      // ones exist, so that next() can know if to wait or return nil.
      _taskGroupAddPendingTask(self.task)

      let childTask =
        await _runGroupChildTask(overridingPriority: priorityOverride, operation: operation)

      return Handle<TaskResult>(task: childTask)
    }

    /// Wait for the a child task that was added to the group to complete,
    /// and return (or rethrow) the value it completed with. If no tasks are
    /// pending in the task group this function returns `nil`, allowing the
    /// following convenient expressions to be written for awaiting for one
    /// or all tasks to complete:
    ///
    /// Await on a single completion:
    ///
    ///     if let first = await try group.next() {
    ///        return first
    ///     }
    ///
    /// Wait and collect all group child task completions:
    ///
    ///     while let first = await try group.next() {
    ///        collected += value
    ///     }
    ///     return collected
    ///
    /// Awaiting on an empty group results in the immediate return of a `nil`
    /// value, without the group task having to suspend.
    ///
    /// ### Ordering
    /// Order of values returned by next() is *completion order*, and not
    /// submission order. I.e. if tasks are added to the group one after another:
    ///
    ///     await group.add { 1 }
    ///     await group.add { 2 }
    ///
    ///     print(await group.next())
    ///     /// Prints "1" OR "2"
    ///
    /// ### Errors
    /// If an operation added to the group throws, that error will be rethrown
    /// by the next() call corresponding to that operation's completion.
    ///
    /// It is possible to directly rethrow such error out of a `withGroup` body
    /// function's body, causing all remaining tasks to be implicitly cancelled.
    public mutating func next() async throws -> TaskResult? {
      let rawResult = await _taskGroupWaitNext(on: self.task)

      if rawResult.hadErrorResult {
        // Throw the result on error.
        let error = unsafeBitCast(rawResult.storage, to: Error.self)
        throw error
      }

      guard let storage = rawResult.storage else {
        // The group was empty, return nil
        return nil
      }

      // Take the value on success.
      let storagePtr =
        storage.bindMemory(to: TaskResult.self, capacity: 1)
      let value = UnsafeMutablePointer<TaskResult>(mutating: storagePtr).pointee
      return value
    }

    /// Query whether the group has any remaining tasks.
    ///
    /// Task groups are always empty upon entry to the `withGroup` body, and
    /// become empty again when `withGroup` returns (either by awaiting on all
    /// pending tasks or cancelling them).
    ///
    /// - Returns: `true` if the group has no pending tasks, `false` otherwise.
    public var isEmpty: Bool {
      _taskGroupIsEmpty(self.task)
    }

    /// Cancel all the remaining tasks in the group.
    ///
    /// A cancelled group will not will NOT accept new tasks being added into it.
    ///
    /// Any results, including errors thrown by tasks affected by this
    /// cancellation, are silently discarded.
    ///
    /// - SeeAlso: `Task.addCancellationHandler`
    /// - SeeAlso: `Task.checkCancelled`
    /// - SeeAlso: `Task.isCancelled`
    public mutating func cancelAll() {
      _taskCancel(self.task) // TODO: do we also have to go over all child tasks and cancel there?
    }
  }
}

/// ==== -----------------------------------------------------------------------

extension Task.Group {
  /// Invoked after a withGroup's body returns, and initiates an orderly
  /// teardown of the task.group. No new tasks are accepted by the group // TODO: don't accept new tasks once body returned to avoid infinitely waiting on accident
  ///
  /// This function waits until all pending tasks have been processed before
  /// returning.
  ///
  /// Failures thrown by remaining tasks are ignored
  ///
  /// Child tasks are NOT cancelled by this function implicitly.
  /// If tasks should be cancelled before returning this must be done by an
  /// explicit `group.cancelAll()` call within the `withGroup`'s function body.
  mutating func _tearDown() async {
    // Drain any not next() awaited tasks if the group wasn't cancelled
    // If any of these tasks were to throw
    //
    // Failures of tasks are ignored.
    while !self.isEmpty {
      _ = await try? self.next()
      // TODO: Should a failure cause a cancellation of the task group?
      //       This looks very much like supervision trees,
      //       where one may have various decisions depending on use cases...
      continue // keep awaiting on all pending tasks
    }
  }
}

/// ==== -----------------------------------------------------------------------

@_silgen_name("swift_retain")
func _swiftRetain(
  _ task: Builtin.NativeObject
)

@_silgen_name("swift_task_group_add_pending")
func _taskGroupAddPendingTask(
  _ groupTask: Builtin.NativeObject
)

@_silgen_name("swift_task_group_offer")
func taskGroupOffer(
  group: Builtin.NativeObject,
  completedTask: Builtin.NativeObject
)

@_silgen_name("swift_task_group_wait_next")
func _taskGroupWaitNext(
  on groupTask: Builtin.NativeObject
) async -> (hadErrorResult: Bool, storage: UnsafeRawPointer?)

enum GroupPollStatus: Int {
  case empty   = 0
  case waiting = 1
  case success = 2
  case error   = 3
}

@_silgen_name("swift_task_group_is_empty")
func _taskGroupIsEmpty(
  _ groupTask: Builtin.NativeObject
) -> Bool
