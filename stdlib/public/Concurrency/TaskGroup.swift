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

// ==== TaskGroup --------------------------------------------------------------

/// Starts a new task group which provides a scope in which a dynamic number of
/// tasks may be spawned.
///
/// When the group returns,
/// it implicitly waits for all spawned tasks to complete.
/// The tasks are canceled only if `cancelAll()` was invoked before returning,
/// if the group's task was canceled.
///
/// After this method returns, the task group is guaranteed to be empty.
///
/// To collect the results of tasks that were added to the group,
/// you can use the following pattern:
///
///     var sum = 0
///     for await result in group {
///         sum += result
///     }
///
/// If you need more control or only a few results,
/// you can use a pattern like the following:
///
///     guard let first = await group.next() {
///         group.cancelAll()
///         return 0
///     }
///     let second = await group.next() ?? 0
///     group.cancelAll()
///     return first + second
///
/// Task Group Cancellation
/// =======================
///
/// Canceling the task in which the group is running
/// also cancels the group and all of its child tasks.
///
/// If you call `spawn(priority:operation:)` to create a new task in a canceled group,
/// that task is immediately canceled after creation.
/// Alternatively, you can call `spawnUnlessCancelled(priority:operation:)`,
/// which doesn't spawn the task if the group has already been canceled
/// Choosing between these two functions
/// lets you control how to react to cancellation within a group:
/// some child tasks need to run regardless of cancellation
/// and others are better not even being spawned
/// knowing they can't produce useful results.
///
/// Because the tasks you add to a group with this method are nonthrowing,
/// those tasks can't respond to cancellation by throwing `CancellationError`.
/// The tasks must handle cancellation in some other way,
/// such as returning the work completed so far, returning an empty result, or returning `nil`.
/// For tasks that need to handle cancellation by throwing an error,
/// use the `withThrowingTaskGroup(of:returning:body:)` method instead.
@available(SwiftStdlib 5.5, *)
@inlinable
public func withTaskGroup<ChildTaskResult, GroupResult>(
  of childTaskResultType: ChildTaskResult.Type,
  returning returnType: GroupResult.Type = GroupResult.self,
  body: (inout TaskGroup<ChildTaskResult>) async -> GroupResult
) async -> GroupResult {
  #if compiler(>=5.5) && $BuiltinTaskGroup

  let _group = Builtin.createTaskGroup(ChildTaskResult.self)
  var group = TaskGroup<ChildTaskResult>(group: _group)

  // Run the withTaskGroup body.
  let result = await body(&group)

  await group.awaitAllRemainingTasks()

  Builtin.destroyTaskGroup(_group)
  return result

  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}

/// Starts a new scope in which a dynamic number of throwing tasks can be spawned.
///
/// When the group returns,
/// it implicitly waits for all spawned tasks to complete.
/// The tasks are canceled only if `cancelAll()` was invoked before returning,
/// if the group's task was canceled,
/// or if the group's body throws an error.
///
/// After this method returns, the task group is guaranteed to be empty.
///
/// To collect the results of tasks that were added to the group,
/// you can use the following pattern:
///
///     var sum = 0
///     for await result in group {
///         sum += result
///     }
///
/// If you need more control or only a few results,
/// you can use a pattern like the following:
///
///     guard let first = await group.next() {
///         group.cancelAll()
///         return 0
///     }
///     let second = await group.next() ?? 0
///     group.cancelAll()
///     return first + second
///
/// Task Group Cancellation
/// =======================
///
/// Canceling the task in which the group is running
/// also cancels the group and all of its child tasks.
///
/// If you call `spawn(priority:operation:)` to create a new task in a canceled group,
/// that task is is immediately canceled after being created.
/// Alternatively, you can call `spawnUnlessCancelled(priority:operation:)`,
/// which doesn't spawn the task if the group has already been canceled
/// Choosing between these two functions
/// lets you control how to react to cancellation within a group:
/// some child tasks need to run regardless of cancellation
/// and others are better not even being spawned
/// knowing they can't produce useful results.
///
/// Throwing an error in one of the tasks of a task group
/// doesn't immediately cancel the other tasks in that group.
/// However,
/// if you call `next()` in the task group and propogate its error,
/// all other tasks are canceled.
/// For example, in the code below,
/// nothing is canceled and the group doesn't throw an error:
///
///     withThrowingTaskGroup { group in
///         group.spawn { throw SomeError() }
///     }
///
/// In contrast, this example throws `SomeError`
/// and cancels all of the tasks in the group:
///
///     withThrowingTaskGroup { group in
///         group.spawn { throw SomeError() }
///         try group.next()
///     }
///
/// An individual task throws its error
/// in the corresponding call to `Group.next()`,
/// which gives you a chance to handle individual error
/// or to let the error be rethrown by the group.
@available(SwiftStdlib 5.5, *)
@inlinable
public func withThrowingTaskGroup<ChildTaskResult, GroupResult>(
  of childTaskResultType: ChildTaskResult.Type,
  returning returnType: GroupResult.Type = GroupResult.self,
  body: (inout ThrowingTaskGroup<ChildTaskResult, Error>) async throws -> GroupResult
) async rethrows -> GroupResult {
  #if compiler(>=5.5) && $BuiltinTaskGroup

  let _group = Builtin.createTaskGroup(ChildTaskResult.self)
  var group = ThrowingTaskGroup<ChildTaskResult, Error>(group: _group)

  do {
    // Run the withTaskGroup body.
    let result = try await body(&group)

    await group.awaitAllRemainingTasks()
    Builtin.destroyTaskGroup(_group)

    return result
  } catch {
    group.cancelAll()

    await group.awaitAllRemainingTasks()
    Builtin.destroyTaskGroup(_group)

    throw error
  }

  #else
  fatalError("Swift compiler is incompatible with this SDK version")
  #endif
}

/// A task group serves as storage for dynamically spawned child tasks.
///
/// To create a task group,
/// call the `withTaskGroup(of:returning:body:)` method.
///
/// A task group must be used only within the task where it was created.
/// In most cases,
/// the Swift type system prevents a task group from escaping like that
/// because adding a child task is a mutating operation,
/// and mutation operations can't be performed
/// from concurrent execution contexts likes child tasks.
@available(SwiftStdlib 5.5, *)
@frozen
public struct TaskGroup<ChildTaskResult> {

  /// Group task into which child tasks offer their results,
  /// and the `next()` function polls those results from.
  @usableFromInline
  internal let _group: Builtin.RawPointer

  // No public initializers
  @inlinable
  init(group: Builtin.RawPointer) {
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
  public mutating func async(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) {
    _ = _taskGroupAddPendingTask(group: _group, unconditionally: true)

    // Set up the job flags for a new task.
    var flags = JobFlags()
    flags.kind = .task
    flags.priority = priority
    flags.isFuture = true
    flags.isChildTask = true
    flags.isGroupChildTask = true

    // Create the asynchronous task future.
    let (childTask, _) = Builtin.createAsyncTaskGroupFuture(
      Int(flags.bits), _group, /*options*/nil, operation)

    // Attach it to the group's task record in the current task.
    _taskGroupAttachChild(group: _group, child: childTask)

    // Enqueue the resulting job.
    _enqueueJobGlobal(Builtin.convertTaskToJob(childTask))
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
  public mutating func asyncUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async -> ChildTaskResult
  ) -> Bool {
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    // Set up the job flags for a new task.
    var flags = JobFlags()
    flags.kind = .task
    flags.priority = priority
    flags.isFuture = true
    flags.isChildTask = true
    flags.isGroupChildTask = true

    // Create the asynchronous task future.
    let (childTask, _) = Builtin.createAsyncTaskGroupFuture(
      Int(flags.bits), _group, /*options*/nil, operation)

    // Attach it to the group's task record in the current task.
    _taskGroupAttachChild(group: _group, child: childTask)

    // Enqueue the resulting job.
    _enqueueJobGlobal(Builtin.convertTaskToJob(childTask))

    return true
  }

  /// Wait for the next child task to complete,
  /// and return the value it returned.
  ///
  /// The values returned by successive calls to this method
  /// appear in the order that the tasks *completed*,
  /// not in the order that those tasks were added to the task group.
  /// For example:
  ///
  ///     group.spawn { 1 }
  ///     group.spawn { 2 }
  ///
  ///     print(await group.next())
  ///     // Prints either "2" or "1".
  ///
  /// If there aren't any pending tasks in the task group,
  /// this method returns `nil`,
  /// which lets you write the following
  /// to wait for a single task to complete:
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
  /// Awaiting on an empty group
  /// immediate returns `nil` without suspending.
  ///
  /// It's also possible to use `for await` to collect results of a task groups:
  ///
  ///     for await try value in group {
  ///         collected += value
  ///     }
  ///
  /// Don't call this method from outside the task
  /// where this task group was created.
  /// In most cases, the Swift type system prevents this mistake;
  /// for example, because the `add(priority:operation:)` method is mutating,
  /// that method can't be called from a concurrent execution context like a child task.
  ///
  /// - Returns: The value returned by the next child task that completes.
  public mutating func next() async -> ChildTaskResult? {
    // try!-safe because this function only exists for Failure == Never,
    // and as such, it is impossible to spawn a throwing child task.
    return try! await _taskGroupWaitNext(group: _group)
  }

  /// Await all of the remaining tasks on this group.
  @usableFromInline
  internal mutating func awaitAllRemainingTasks() async {
    while let _ = await next() {}
  }
  
  /// A Boolean value that indicates whether the group has any remaining tasks.
  ///
  /// At the start of the body of a `withTaskGroup(of:returning:body:)` call,
  /// the task group is always empty.
  /// It is guaranteed to be empty when returning from that body
  /// because a task group waits for all child tasks to complete before returning.
  ///
  /// - Returns: `true` if the group has no pending tasks; otherwise `false`.
  public var isEmpty: Bool {
    _taskGroupIsEmpty(_group)
  }

  /// Cancel all of the remaining tasks in the group.
  ///
  /// After canceling a group, adding a new task to it always fails.
  ///
  /// Any results, including errors thrown by tasks affected by this
  /// cancellation, are silently discarded.
  ///
  /// This function may be called even from within child (or any other) tasks,
  /// and causes the group to be canceled.
  ///
  /// - SeeAlso: `Task.isCancelled`
  /// - SeeAlso: `TaskGroup.isCancelled`
  public func cancelAll() {
    _taskGroupCancelAll(group: _group)
  }

  /// A Boolean value that indicates whether the group was canceled.
  ///
  /// To cancel a group, call the `TaskGroup.cancelAll()` method.
  ///
  /// If the task that's currently running this group is canceled,
  /// the group is also implicitly canceled,
  /// which is also reflected in this property's value.
  public var isCancelled: Bool {
    return _taskGroupIsCancelled(group: _group)
  }
}

// Implementation note:
// We are unable to just™ abstract over Failure == Error / Never because of the
// complicated relationship between `group.spawn` which dictates if `group.next`
// AND the AsyncSequence conformances would be throwing or not.
//
// We would be able to abstract over TaskGroup<..., Failure> equal to Never
// or Error, and specifically only add the `spawn` and `next` functions for
// those two cases. However, we are not able to conform to AsyncSequence "twice"
// depending on if the Failure is Error or Never, as we'll hit:
//    conflicting conformance of 'TaskGroup<ChildTaskResult, Failure>' to protocol
//    'AsyncSequence'; there cannot be more than one conformance, even with
//    different conditional bounds
// So, sadly we're forced to duplicate the entire implementation of TaskGroup
// to TaskGroup and ThrowingTaskGroup.
//
// The throwing task group is parameterized with failure only because of future
// proofing, in case we'd ever have typed errors, however unlikely this may be.
// Today the throwing task group failure is simply automatically bound to `Error`.

/// A task group serves as storage for dynamically spawned,
/// potentially throwing, child tasks.
///
@available(SwiftStdlib 5.5, *)
@frozen
public struct ThrowingTaskGroup<ChildTaskResult, Failure: Error> {

  /// Group task into which child tasks offer their results,
  /// and the `next()` function polls those results from.
  @usableFromInline
  internal let _group: Builtin.RawPointer

  // No public initializers
  @inlinable
  init(group: Builtin.RawPointer) {
    self._group = group
  }

  /// Await all the remaining tasks on this group.
  @usableFromInline
  internal mutating func awaitAllRemainingTasks() async {
    while true {
      do {
        guard let _ = try await next() else {
          return
        }
      } catch {}
    }
  }

  /// Spawn, unconditionally, a child task in the group.
  ///
  /// ### Error handling
  /// Operations are allowed to `throw`, in which case the `try await next()`
  /// invocation corresponding to the failed task will re-throw the given task.
  ///
  /// This method doesn't throw an error, even if the child task does.
  /// Instead, corresponding next call to `TaskGroup.next()` rethrows that error.
  ///
  /// - Parameters:
  ///   - overridingPriority: override priority of the operation task
  ///   - operation: operation to execute and add to the group
  /// - Returns:
  ///   - `true` if the operation was added to the group successfully,
  ///     `false` otherwise (e.g. because the group `isCancelled`)
  public mutating func async(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) {
    // we always add, so no need to check if group was cancelled
    _ = _taskGroupAddPendingTask(group: _group, unconditionally: true)

    // Set up the job flags for a new task.
    var flags = JobFlags()
    flags.kind = .task
    flags.priority = priority
    flags.isFuture = true
    flags.isChildTask = true
    flags.isGroupChildTask = true

    // Create the asynchronous task future.
    let (childTask, _) = Builtin.createAsyncTaskGroupFuture(
      Int(flags.bits), _group, /*options*/nil, operation)

    // Attach it to the group's task record in the current task.
    _taskGroupAttachChild(group: _group, child: childTask)

    // Enqueue the resulting job.
    _enqueueJobGlobal(Builtin.convertTaskToJob(childTask))
  }

  /// Add a child task to the group.
  ///
  /// ### Error handling
  /// Operations are allowed to `throw`, in which case the `try await next()`
  /// invocation corresponding to the failed task will re-throw the given task.
  ///
  /// This method doesn't throw an error, even if the child task does.
  /// Instead, the corresponding call to `TaskGroup.next()` rethrows that error.
  ///
  /// - Parameters:
  ///   - overridingPriority: override priority of the operation task
  ///   - operation: operation to execute and add to the group
  /// - Returns:
  ///   - `true` if the operation was added to the group successfully,
  ///     `false` otherwise (e.g. because the group `isCancelled`)
  public mutating func asyncUnlessCancelled(
    priority: TaskPriority? = nil,
    operation: __owned @Sendable @escaping () async throws -> ChildTaskResult
  ) -> Bool {
    let canAdd = _taskGroupAddPendingTask(group: _group, unconditionally: false)

    guard canAdd else {
      // the group is cancelled and is not accepting any new work
      return false
    }

    // Set up the job flags for a new task.
    var flags = JobFlags()
    flags.kind = .task
    flags.priority = priority
    flags.isFuture = true
    flags.isChildTask = true
    flags.isGroupChildTask = true

    // Create the asynchronous task future.
    let (childTask, _) = Builtin.createAsyncTaskGroupFuture(
      Int(flags.bits), _group, /*options*/nil, operation)

    // Attach it to the group's task record in the current task.
    _taskGroupAttachChild(group: _group, child: childTask)

    // Enqueue the resulting job.
    _enqueueJobGlobal(Builtin.convertTaskToJob(childTask))

    return true
  }

  /// Wait for the next child task to complete,
  /// and return the value it returned or rethrow the error it threw.
  ///
  /// The values returned by successive calls to this method
  /// appear in the order that the tasks *completed*,
  /// not in the order that those tasks were added to the task group.
  /// For example:
  ///
  ///     group.spawn { 1 }
  ///     group.spawn { 2 }
  ///
  ///     await print(group.next())
  ///     // Prints either "2" or "1".
  ///
  /// If there aren't any pending tasks in the task group,
  /// this method returns `nil`,
  /// which lets you write like the following
  /// to wait for a single task to complete:
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
  /// Awaiting on an empty group
  /// immediate returns `nil` without suspending.
  ///
  /// It's also possible to use `for await` to collect results of a task groups:
  ///
  ///     for await try value in group {
  ///         collected += value
  ///     }
  ///
  /// If the next child task throws an error
  /// and you propagate that error from this method
  /// out of the body of a `TaskGroup.withThrowingTaskGroup(of:returning:body:)` call,
  /// then all remaining child tasks in that group are implicitly canceled.
  ///
  /// Don't call this method from outside the task
  /// where this task group was created.
  /// In most cases, the Swift type system prevents this mistake;
  /// for example, because the `add(priority:operation:)` method is mutating,
  /// that method can't be called from a concurrent execution context like a child task.
  ///
  /// - Returns: The value returned by the next child task that completes.
  ///
  /// - Throws: The error thrown by the next child task that completes.
  ///
  /// - SeeAlso: `nextResult()`
  public mutating func next() async throws -> ChildTaskResult? {
    return try await _taskGroupWaitNext(group: _group)
  }

  /// Wait for the next child task to complete,
  /// and return a result containing either
  /// the value that the child task returned or the error that it threw.
  ///
  /// The values returned by successive calls to this method
  /// appear in the order that the tasks *completed*,
  /// not in the order that those tasks were added to the task group.
  /// For example:
  ///
  ///     group.spawn { 1 }
  ///     group.spawn { 2 }
  ///
  ///     guard let result = await group.nextResult() else {
  ///         return  // No task to wait on, which won't happen in this example.
  ///     }
  ///     
  ///     switch result { 
  ///     case .success(let value): print(value)
  ///     case .failure(let error): print("Failure: \(error)")
  ///     }
  ///     // Prints either "2" or "1".
  ///
  /// If the next child task throws an error
  /// and you propagate that error from this method
  /// out of the body of a `ThrowingTaskGroup.withThrowingTaskGroup(of:returning:body:)` call,
  /// then all remaining child tasks in that group are implicitly canceled.
  ///
  /// - Returns: A `Result.success` value
  ///   containing the value that the child task returned,
  ///   or a `Result.failure` value
  ///   containing the error that the child task threw.
  ///
  /// - SeeAlso: `next()`
  public mutating func nextResult() async throws -> Result<ChildTaskResult, Failure>? {
    do {
      guard let success: ChildTaskResult = try await _taskGroupWaitNext(group: _group) else {
        return nil
      }

      return .success(success)
    } catch {
      return .failure(error as! Failure) // as!-safe, because we are only allowed to throw Failure (Error)
    }
  }

  /// A Boolean value that indicates whether the group has any remaining tasks.
  ///
  /// At the start of the body of a `withThrowingTaskGroup(of:returning:body:)` call,
  /// the task group is always empty.
  /// It's guaranteed to be empty when returning from that body
  /// because a task group waits for all child tasks to complete before returning.
  ///
  /// - Returns: `true` if the group has no pending tasks; otherwise `false`.
  public var isEmpty: Bool {
    _taskGroupIsEmpty(_group)
  }

  /// Cancel all of the remaining tasks in the group.
  ///
  /// After canceling a group, adding a new task to it always fails.
  ///
  /// Any results, including errors thrown by tasks affected by this
  /// cancellation, are silently discarded.
  ///
  /// This function may be called even from within child (or any other) tasks,
  /// and causes the group to be canceled.
  ///
  /// - SeeAlso: `Task.isCancelled`
  /// - SeeAlso: `TaskGroup.isCancelled`
  public func cancelAll() {
    _taskGroupCancelAll(group: _group)
  }

  /// A Boolean value that indicates whether the group was canceled.
  ///
  /// To cancel a group, call the `ThrowingTaskGroup.cancelAll()` method.
  ///
  /// If the task that's currently running this group is canceled,
  /// the group is also implicitly canceled,
  /// which is also reflected in this property's value.
  public var isCancelled: Bool {
    return _taskGroupIsCancelled(group: _group)
  }
}

/// ==== TaskGroup: AsyncSequence ----------------------------------------------

@available(SwiftStdlib 5.5, *)
extension TaskGroup: AsyncSequence {
  public typealias AsyncIterator = Iterator
  public typealias Element = ChildTaskResult

  public func makeAsyncIterator() -> Iterator {
    return Iterator(group: self)
  }

  /// A type that provides an iteration interface
  /// over the results of tasks added to the group.
  ///
  /// The elements returned by this iterator
  /// appear in the order that the tasks *completed*,
  /// not in the order that those tasks were added to the task group.
  ///
  /// This iterator terminates after all tasks have completed successfully,
  /// or after any task completes by throwing an error.
  /// However, it's valid to make a new iterator for the task group,
  /// which you can use to continue iterating over the group's results.
  /// For example:
  ///
  ///     group.spawn { 1 }
  ///     group.spawn { throw SomeError }
  ///     group.spawn { 2 }
  ///     
  ///     do { 
  ///         // Assuming the child tasks complete in order, this prints "1"
  ///         // and then throws an error.
  ///         for try await r in group { print(r) }
  ///     } catch {
  ///         // Resolve the error.
  ///     }
  ///     
  ///     // Iterate again.
  ///     for try await r in group { print(r) }
  ///
  /// - SeeAlso: `TaskGroup.next()`
  @available(SwiftStdlib 5.5, *)
  public struct Iterator: AsyncIteratorProtocol {
    public typealias Element = ChildTaskResult

    @usableFromInline
    var group: TaskGroup<ChildTaskResult>

    @usableFromInline
    var finished: Bool = false

    // no public constructors
    init(group: TaskGroup<ChildTaskResult>) {
      self.group = group
    }

    /// Advances to the result of the next child task,
    /// or `nil` if there are no remaining child tasks,
    /// rethrowing an error if the child task threw.
    ///
    /// The elements returned from this method
    /// appear in the order that the tasks *completed*,
    /// not in the order that those tasks were added to the task group.
    /// After this method returns `nil`,
    /// this iterater is guaranteed to never produce more values.
    ///
    /// For more information about the iteration order and semantics,
    /// see `TaskGroup.next()`.
    public mutating func next() async -> Element? {
      guard !finished else { return nil }
      guard let element = await group.next() else {
        finished = true
        return nil
      }
      return element
    }

    public mutating func cancel() {
      finished = true
      group.cancelAll()
    }
  }
}

@available(SwiftStdlib 5.5, *)
extension ThrowingTaskGroup: AsyncSequence {
  public typealias AsyncIterator = Iterator
  public typealias Element = ChildTaskResult

  public func makeAsyncIterator() -> Iterator {
    return Iterator(group: self)
  }

  /// A type that provides an iteration interface
  /// over the results of tasks added to the group.
  ///
  /// The elements returned by this iterator
  /// appear in the order that the tasks *completed*,
  /// not in the order that those tasks were added to the task group.
  ///
  /// This iterator terminates after all tasks have completed successfully,
  /// or after any task completes by throwing an error.
  /// If a task completes by throwing an error,
  /// no further task results are returned.
  ///
  /// - SeeAlso: `ThrowingTaskGroup.next()`
  @available(SwiftStdlib 5.5, *)
  public struct Iterator: AsyncIteratorProtocol {
    public typealias Element = ChildTaskResult

    @usableFromInline
    var group: ThrowingTaskGroup<ChildTaskResult, Failure>

    @usableFromInline
    var finished: Bool = false

    // no public constructors
    init(group: ThrowingTaskGroup<ChildTaskResult, Failure>) {
      self.group = group
    }

    /// Advances to the result of the next child task,
    /// or `nil` if there are no remaining child tasks,
    /// rethrowing an error if the child task threw.
    ///
    /// The elements returned from this method
    /// appear in the order that the tasks *completed*,
    /// not in the order that those tasks were added to the task group.
    /// After this method returns `nil`,
    /// this iterater is guaranteed to never produce more values.
    ///
    /// For more information about the iteration order and semantics,
    /// see `ThrowingTaskGroup.next()` 
    public mutating func next() async throws -> Element? {
      guard !finished else { return nil }
      do {
        guard let element = try await group.next() else {
          finished = true
          return nil
        }
        return element
      } catch {
        finished = true
        throw error
      }
    }

    public mutating func cancel() {
      finished = true
      group.cancelAll()
    }
  }
}

/// ==== -----------------------------------------------------------------------

/// Attach task group child to the group group to the task.
@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_taskGroup_attachChild")
func _taskGroupAttachChild(
  group: Builtin.RawPointer,
  child: Builtin.NativeObject
)

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_taskGroup_destroy")
func _taskGroupDestroy(group: __owned Builtin.RawPointer)

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_taskGroup_addPending")
func _taskGroupAddPendingTask(
  group: Builtin.RawPointer,
  unconditionally: Bool
) -> Bool

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_taskGroup_cancelAll")
func _taskGroupCancelAll(group: Builtin.RawPointer)

/// Checks ONLY if the group was specifically canceled.
/// The task itself being canceled must be checked separately.
@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_taskGroup_isCancelled")
func _taskGroupIsCancelled(group: Builtin.RawPointer) -> Bool

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_taskGroup_wait_next_throwing")
func _taskGroupWaitNext<T>(group: Builtin.RawPointer) async throws -> T?

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_task_hasTaskGroupStatusRecord")
func _taskHasTaskGroupStatusRecord() -> Bool

@available(SwiftStdlib 5.5, *)
enum PollStatus: Int {
  case empty   = 0
  case waiting = 1
  case success = 2
  case error   = 3
}

@available(SwiftStdlib 5.5, *)
@_silgen_name("swift_taskGroup_isEmpty")
func _taskGroupIsEmpty(
  _ group: Builtin.RawPointer
) -> Bool
