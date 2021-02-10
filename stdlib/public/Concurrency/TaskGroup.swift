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

import Darwin // FIXME: remove this

func pprint(_ m: String, file: String = #file, line: UInt = #line) {
  fputs("[\(file):\(line)] \(m)\n", stderr)
}

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
//    startingChildTasksOn executor: ExecutorRef? = nil, // TODO: actually respect it
    body: (inout Task.Group<TaskResult>) async throws -> BodyResult
  ) async throws -> BodyResult {
    let task = Builtin.getCurrentAsyncTask()
    let _group = _taskGroupCreate(task: task)
    var group: Task.Group<TaskResult>! = Task.Group(task: task, group: _group)

    // Run the withGroup body.
    do {
      let result = try await body(&group)
      await group._tearDown()
      group = nil
      _taskGroupDestroy(task: task, group: _group)
      return result
    } catch {
      group.cancelAll()
      await group._tearDown()
      group = nil
      _taskGroupDestroy(task: task, group: _group)
      throw error
    }
  }

  /// A task group serves as storage for dynamically started tasks.
  ///
  /// Its intended use is with the `Task.withGroup` function.
  /* @unmoveable */
  public struct Group<TaskResult> {
    private let _task: Builtin.NativeObject
    /// Group task into which child tasks offer their results,
    /// and the `next()` function polls those results from.
    private let _group: Builtin.NativeObject

    /// No public initializers
    init(task: Builtin.NativeObject, group: Builtin.NativeObject) {
      // TODO: this feels slightly off, any other way to avoid the task being too eagerly released?
      _swiftRetain(task) // to avoid the task being destroyed when the group is destroyed

      self._task = task
      self._group = group
    }

    /// Add a child task to the group.
    ///
    /// ### Error handling
    /// Operations are allowed to `throw`, in which case the `try await next()`
    /// invocation corresponding to the failed task will re-throw the given task.
    ///
    /// The `add` function will never (re-)throw errors from the `operation`.
    /// Instead, the corresponding `next()` call will throw the error when necessary.
    ///
    /// - Parameters:
    ///   - overridingPriority: override priority of the operation task
    ///   - operation: operation to execute and add to the group
    /// - Returns:
    ///   - `true` if the operation was added to the group successfully,
    ///     `false` otherwise (e.g. because the group `isCancelled`)
    @discardableResult
    public mutating func add(
      overridingPriority priorityOverride: Priority? = nil,
      operation: @concurrent @escaping () async throws -> TaskResult
    ) async -> Bool {
      let canAdd = _taskGroupAddPendingTask(group: _group)

      guard canAdd else {
        // the group is cancelled and is not accepting any new work
        return false
      }

      // Set up the job flags for a new task.
      var flags = Task.JobFlags()
      flags.kind = .task
      flags.priority = priorityOverride ?? getJobFlags(_task).priority
      flags.isFuture = true
      flags.isChildTask = true
      flags.isGroupChildTask = true

      // Create the asynchronous task future.
      let (childTask, _) = Builtin.createAsyncTaskGroupFuture(
        flags.bits, _task, _group, operation)

      // Enqueue the resulting job.
      _enqueueJobGlobal(Builtin.convertTaskToJob(childTask))

      // TODO: need to store task in the group too

      return true
    }

    /// Wait for the a child task that was added to the group to complete,
    /// and return (or rethrow) the value it completed with. If no tasks are
    /// pending in the task group this function returns `nil`, allowing the
    /// following convenient expressions to be written for awaiting for one
    /// or all tasks to complete:
    ///
    /// Await on a single completion:
    ///
    ///     if let first = try await group.next() {
    ///        return first
    ///     }
    ///
    /// Wait and collect all group child task completions:
    ///
    ///     while let first = try await group.next() {
    ///        collected += value
    ///     }
    ///     return collected
    ///
    /// Awaiting on an empty group results in the immediate return of a `nil`
    /// value, without the group task having to suspend.
    ///
    /// ### Thread-safety
    /// Please note that the `group` object MUST NOT escape into another task.
    /// The `group.next()` MUST be awaited from the task that had originally
    /// created the group. It is not allowed to escape the group reference.
    ///
    /// Note also that this is generally prevented by Swift's type-system,
    /// as the `add` operation is `mutating`, and those may not be performed
    /// from concurrent execution contexts, such as child tasks.
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
      #if NDEBUG
      let callingTask = Builtin.getCurrentAsyncTask() // can't inline into the assert sadly
      assert(unsafeBitCast(callingTask, to: size_t.self) ==
             unsafeBitCast(_task, to: size_t.self),
        """
        group.next() invoked from task other than the task which created the group! \
        This means the group must have illegally escaped the withGroup{} scope.
        """)
      #endif

      let rawResult = await _taskGroupWaitNext(waitingTask: _task, group: _group)

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
      _taskGroupIsEmpty(_group)
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
      _taskGroupCancelAll(task: _task, group: _group)
    }

    // Returns `true` if the group was cancelled, e.g. by `cancelAll`.
    public var isCancelled: Bool {
      return _taskIsCancelled(_task) ||
        _taskGroupIsCancelled(task: _task, group: _group)
    }
  }
}

/// ==== -----------------------------------------------------------------------

extension Task.Group {
  /// Invoked after a withGroup's body exits, and initiates an orderly
  /// teardown of the group.
  ///
  /// This function waits until all pending tasks have been processed before
  /// returning.
  ///
  /// Failures thrown by remaining tasks are ignored.
  ///
  /// Once this function returns the underlying task group has been freed,
  /// and must not be touched ever again.
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
      _ = try? await self.next()
      continue // keep awaiting on all pending tasks
    }
  }
}

/// ==== -----------------------------------------------------------------------

@_silgen_name("swift_retain")
func _swiftRetain(
  _ object: Builtin.NativeObject
)

@_silgen_name("swift_release")
func _swiftRelease(
  _ object: Builtin.NativeObject
)

@_silgen_name("swift_task_group_create")
func _taskGroupCreate(
  task: Builtin.NativeObject
) -> Builtin.NativeObject

@_silgen_name("swift_task_group_destroy")
func _taskGroupDestroy(
  task: Builtin.NativeObject,
  group: __owned Builtin.NativeObject
)

@_silgen_name("swift_task_group_add_pending")
func _taskGroupAddPendingTask(
  group: Builtin.NativeObject
) -> Bool

@_silgen_name("swift_task_group_cancel_all")
func _taskGroupCancelAll(
  task: Builtin.NativeObject,
  group: Builtin.NativeObject
)

/// Checks ONLY if the group was specifically cancelled.
/// The task itself being cancelled must be checked separately.
@_silgen_name("swift_task_group_is_cancelled")
func _taskGroupIsCancelled(
  task: Builtin.NativeObject,
  group: Builtin.NativeObject
) -> Bool

@_silgen_name("swift_task_group_wait_next")
func _taskGroupWaitNext(
  waitingTask: Builtin.NativeObject,
  group: Builtin.NativeObject
) async -> (hadErrorResult: Bool, storage: UnsafeRawPointer?)

enum GroupPollStatus: Int {
  case empty   = 0
  case waiting = 1
  case success = 2
  case error   = 3
}

@_silgen_name("swift_task_group_is_empty")
func _taskGroupIsEmpty(
  _ group: Builtin.NativeObject
) -> Bool
