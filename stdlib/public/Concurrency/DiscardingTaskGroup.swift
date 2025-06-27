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

// ==== DiscardingTaskGroup ---------------------------------------------------

/// Starts a new scope that can contain a dynamic number of child tasks.
///
/// Unlike a ``TaskGroup``, the child tasks as well as their results are
/// discarded as soon as the tasks complete. This prevents the discarding
/// task group from accumulating many results waiting to be consumed, and is
/// best applied in situations where the result of a child task is some form
/// of side-effect.
///
/// A group *always* waits for all of its child tasks
/// to complete before it returns. Even cancelled tasks must run until
/// completion before this function returns.
/// Cancelled child tasks cooperatively react to cancellation and attempt
/// to return as early as possible.
/// After this function returns, the task group is always empty.
///
/// It is not possible to explicitly await completion of child-tasks,
/// however the group will automatically await *all* child task completions
/// before returning from this function:
///
/// ```
/// await withDiscardingTaskGroup(...) { group in
///   group.addTask { /* slow-task */ }
///   // slow-task executes...
/// }
/// // guaranteed that slow-task has completed and the group is empty & destroyed
/// ```
///
/// Refer to ``TaskGroup`` documentation for detailed discussion of semantics shared between all task groups.
///
/// Task Group Cancellation
/// =======================
///
/// You can cancel a task group and all of its child tasks
/// by calling the ``TaskGroup/cancelAll()`` method on the task group,
/// or by canceling the task in which the group is running.
///
/// If you call `addTask(priority:operation:)` to create a new task in a canceled group,
/// that task is immediately canceled after creation.
/// Alternatively, you can call `asyncUnlessCancelled(priority:operation:)`,
/// which doesn't create the task if the group has already been canceled.
/// Choosing between these two functions
/// lets you control how to react to cancellation within a group:
/// some child tasks need to run regardless of cancellation,
/// but other tasks are better not even being created
/// when you know they can't produce useful results.
///
/// Because the tasks you add to a group with this method are nonthrowing,
/// those tasks can't respond to cancellation by throwing `CancellationError`.
/// The tasks must handle cancellation in some other way,
/// such as returning the work completed so far, returning an empty result, or returning `nil`.
/// For tasks that need to handle cancellation by throwing an error,
/// use the `withThrowingDiscardingTaskGroup(returning:body:)` method instead.
///
/// - SeeAlso: ``TaskGroup``
/// - SeeAlso: ``withThrowingDiscardingTaskGroup(returning:body:)``
@available(SwiftStdlib 5.9, *)
#if !hasFeature(Embedded)
@backDeployed(before: SwiftStdlib 6.0)
#endif
@inlinable
public func withDiscardingTaskGroup<GroupResult>(
  returning returnType: GroupResult.Type = GroupResult.self,
  isolation: isolated (any Actor)? = #isolation,
  body: (inout DiscardingTaskGroup) async -> GroupResult
) async -> GroupResult {
  let flags = taskGroupCreateFlags(
    discardResults: true
  )

  let _group = Builtin.createTaskGroupWithFlags(flags, Void.self)
  var group = DiscardingTaskGroup(group: _group)
  defer { Builtin.destroyTaskGroup(_group) }

  let result = await body(&group)

  try! await group.awaitAllRemainingTasks() // try!-safe, cannot throw since this is a non throwing group

  return result
}

// Note: hack to stage out @_unsafeInheritExecutor forms of various functions
// in favor of #isolation. The _unsafeInheritExecutor_ prefix is meaningful
// to the type checker.
//
// This function also doubles as an ABI-compatibility shim predating the
// introduction of #isolation.
@available(SwiftStdlib 5.9, *)
@_unsafeInheritExecutor // for ABI compatibility
@_silgen_name("$ss23withDiscardingTaskGroup9returning4bodyxxm_xs0bcD0VzYaXEtYalF")
public func _unsafeInheritExecutor_withDiscardingTaskGroup<GroupResult>(
  returning returnType: GroupResult.Type = GroupResult.self,
  body: (inout DiscardingTaskGroup) async -> GroupResult
) async -> GroupResult {
  let flags = taskGroupCreateFlags(
    discardResults: true
  )

  let _group = Builtin.createTaskGroupWithFlags(flags, Void.self)
  var group = DiscardingTaskGroup(group: _group)
  defer { Builtin.destroyTaskGroup(_group) }

  let result = await body(&group)

  try! await group.awaitAllRemainingTasks() // try!-safe, cannot throw since this is a non throwing group

  return result
}

/// A discarding group that contains dynamically created child tasks.
///
/// To create a discarding task group,
/// call the ``withDiscardingTaskGroup(returning:body:)`` method.
///
/// Don't use a task group from outside the task where you created it.
/// In most cases,
/// the Swift type system prevents a task group from escaping like that
/// because adding a child task to a task group is a mutating operation,
/// and mutation operations can't be performed
/// from a concurrent execution context like a child task.
///
/// Refer to ``TaskGroup`` documentation for detailed discussion of semantics shared between all task groups.
///
/// ### Discarding behavior
/// A discarding task group eagerly discards and releases its child tasks as
/// soon as they complete. This allows for the efficient releasing of memory used
/// by those tasks, which are not retained for future `next()` calls, as would
/// be the case with a ``TaskGroup``.
///
/// ### Cancellation behavior
/// A discarding task group becomes cancelled in one of the following ways:
///
/// - when ``cancelAll()`` is invoked on it,
/// - when the ``Task`` running this task group is cancelled.
///
/// Since a `DiscardingTaskGroup` is a structured concurrency primitive, cancellation is
/// automatically propagated through all of its child-tasks (and their child
/// tasks).
///
/// A cancelled task group can still keep adding tasks, however they will start
/// being immediately cancelled, and may act accordingly to this. To avoid adding
/// new tasks to an already cancelled task group, use ``addTaskUnlessCancelled(priority:body:)``
/// rather than the plain ``addTask(priority:body:)`` which adds tasks unconditionally.
///
/// For information about the language-level concurrency model that `DiscardingTaskGroup` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
///
/// - SeeAlso: ``TaskGroup``
/// - SeeAlso: ``ThrowingTaskGroup``
/// - SeeAlso: ``ThrowingDiscardingTaskGroup``
@available(SwiftStdlib 5.9, *)
@frozen
public struct DiscardingTaskGroup {

  @usableFromInline
  internal let _group: Builtin.RawPointer

  // No public initializers
  @inlinable
  init(group: Builtin.RawPointer) {
    self._group = group
  }

  /// Await all the remaining tasks on this group.
  ///
  /// - Throws: The first error that was encountered by this group.
  @usableFromInline
  internal mutating func awaitAllRemainingTasks() async throws {
    let _: Void? = try await _taskGroupWaitAll(group: _group, bodyError: nil)
  }

  /// A Boolean value that indicates whether the group has any remaining tasks.
  ///
  /// At the start of the body of a `withDiscardingTaskGroup(of:returning:body:)` call,
  /// the task group is always empty.
  ///
  /// It's guaranteed to be empty when returning from that body
  /// because a task group waits for all child tasks to complete before returning.
  ///
  /// - Returns: `true` if the group has no pending tasks; otherwise `false`.
  public var isEmpty: Bool {
    _taskGroupIsEmpty(_group)
  }

  /// Cancel all of the remaining tasks in the group.
  ///
  /// If you add a task to a group after canceling the group,
  /// that task is canceled immediately after being added to the group.
  ///
  /// Immediately cancelled child tasks should therefore cooperatively check for and
  /// react  to cancellation, e.g. by throwing an `CancellationError` at their
  /// earliest convenience, or otherwise handling the cancellation.
  ///
  /// There are no restrictions on where you can call this method.
  /// Code inside a child task or even another task can cancel a group,
  /// however one should be very careful to not keep a reference to the
  /// group longer than the `with...TaskGroup(...) { ... }` method body is executing.
  ///
  /// - SeeAlso: `Task.isCancelled`
  /// - SeeAlso: `DiscardingTaskGroup.isCancelled`
  public func cancelAll() {
    _taskGroupCancelAll(group: _group)
  }

  /// A Boolean value that indicates whether the group was canceled.
  ///
  /// To cancel a group, call the `DiscardingTaskGroup.cancelAll()` method.
  ///
  /// If the task that's currently running this group is canceled,
  /// the group is also implicitly canceled,
  /// which is also reflected in this property's value.
  public var isCancelled: Bool {
    return _taskGroupIsCancelled(group: _group)
  }
}

@available(SwiftStdlib 5.9, *)
@available(*, unavailable)
extension DiscardingTaskGroup: Sendable { }

// ==== ThrowingDiscardingTaskGroup -------------------------------------------

/// Starts a new scope that can contain a dynamic number of child tasks.
///
/// Unlike a ``ThrowingTaskGroup``, the child tasks as well as their results are
/// discarded as soon as the tasks complete. This prevents the discarding
/// task group from accumulating many results waiting to be consumed, and is
/// best applied in situations where the result of a child task is some form
/// of side-effect.
///
/// A group *always* waits for all of its child tasks
/// to complete before it returns. Even cancelled tasks must run until
/// completion before this function returns.
/// Cancelled child tasks cooperatively react to cancellation and attempt
/// to return as early as possible.
/// After this function returns, the task group is always empty.
///
/// It is not possible to explicitly await completion of child-tasks,
/// however the group will automatically await *all* child task completions
/// before returning from this function:
///
/// ```
/// try await withThrowingDiscardingTaskGroup(of: Void.self) { group in
///   group.addTask { /* slow-task */ }
///   // slow-task executes...
/// }
/// // guaranteed that slow-task has completed and the group is empty & destroyed
/// ```
///
/// Refer to ``TaskGroup`` documentation for detailed discussion of semantics shared between all task groups.
///
/// Task Group Cancellation
/// =======================
///
/// You can cancel a task group and all of its child tasks
/// by calling the ``TaskGroup/cancelAll()`` method on the task group,
/// or by canceling the task in which the group is running.
///
/// If you call `addTask(priority:operation:)` to create a new task in a canceled group,
/// that task is immediately canceled after creation.
/// Alternatively, you can call `asyncUnlessCancelled(priority:operation:)`,
/// which doesn't create the task if the group has already been canceled.
/// Choosing between these two functions
/// lets you control how to react to cancellation within a group:
/// some child tasks need to run regardless of cancellation,
/// but other tasks are better not even being created
/// when you know they can't produce useful results.
///
/// Error Handling and Implicit Cancellation
/// ========================================
///
/// Since it is not possible to explicitly await individual task completions,
/// it is also not possible to "re-throw" an error thrown by one of the child
/// tasks using the same pattern as one would in a ``ThrowingTaskGroup``:
///
/// ```
/// // ThrowingTaskGroup, pattern not applicable to ThrowingDiscardingTaskGroup
/// try await withThrowingTaskGroup(of: Void.self) { group in
///   group.addTask { try boom() }
///   try await group.next() // re-throws "boom"
/// }
/// ```
///
/// Since discarding task groups don't have access to `next()`, this pattern
/// cannot be used.
/// Instead,
/// a *throwing discarding task group implicitly cancels itself whenever any
/// of its child tasks throws*.
///
/// The *first error* thrown inside such task group
/// is then retained and thrown
/// out of the `withThrowingDiscardingTaskGroup` method when it returns.
///
/// ```
/// try await withThrowingDiscardingTaskGroup { group in
///   group.addTask { try boom(1) }
///   group.addTask { try boom(2, after: .seconds(5)) }
///   group.addTask { try boom(3, after: .seconds(5)) }
/// }
/// ```
///
/// Generally, this suits the typical use cases of a
/// discarding task group well, however, if you want to prevent specific
/// errors from canceling the group you can catch them inside the child
/// task's body like this:
///
/// ```
/// try await withThrowingDiscardingTaskGroup { group in
///   group.addTask {
///     do {
///       try boom(1)
///     } catch is HarmlessError {
///       return
///     }
///   }
///   group.addTask {
///     try boom(2, after: .seconds(5))
///   }
/// }
/// ```
@available(SwiftStdlib 5.9, *)
#if !hasFeature(Embedded)
@backDeployed(before: SwiftStdlib 6.0)
#endif
@inlinable
public func withThrowingDiscardingTaskGroup<GroupResult>(
    returning returnType: GroupResult.Type = GroupResult.self,
    isolation: isolated (any Actor)? = #isolation,
    body: (inout ThrowingDiscardingTaskGroup<Error>) async throws -> GroupResult
) async throws -> GroupResult {
  let flags = taskGroupCreateFlags(
      discardResults: true
  )

  let _group = Builtin.createTaskGroupWithFlags(flags, Void.self)
  var group = ThrowingDiscardingTaskGroup<Error>(group: _group)
  defer { Builtin.destroyTaskGroup(_group) }

  let result: GroupResult
  do {
    result = try await body(&group)
  } catch {
    group.cancelAll()

    try await group.awaitAllRemainingTasks(bodyError: error)

    throw error
  }

  try await group.awaitAllRemainingTasks(bodyError: nil)

  return result
}

@available(SwiftStdlib 5.9, *)
@_unsafeInheritExecutor // for ABI compatibility
@_silgen_name("$ss31withThrowingDiscardingTaskGroup9returning4bodyxxm_xs0bcdE0Vys5Error_pGzYaKXEtYaKlF")
public func _unsafeInheritExecutor_withThrowingDiscardingTaskGroup<GroupResult>(
    returning returnType: GroupResult.Type = GroupResult.self,
    body: (inout ThrowingDiscardingTaskGroup<Error>) async throws -> GroupResult
) async throws -> GroupResult {
  let flags = taskGroupCreateFlags(
      discardResults: true
  )

  let _group = Builtin.createTaskGroupWithFlags(flags, Void.self)
  var group = ThrowingDiscardingTaskGroup<Error>(group: _group)
  defer { Builtin.destroyTaskGroup(_group) }

  let result: GroupResult
  do {
    result = try await body(&group)
  } catch {
    group.cancelAll()

    try await group.awaitAllRemainingTasks(bodyError: error)

    throw error
  }

  try await group.awaitAllRemainingTasks(bodyError: nil)

  return result
}


/// A throwing discarding group that contains dynamically created child tasks.
///
/// To create a discarding task group,
/// call the ``withDiscardingTaskGroup(returning:body:)`` method.
///
/// Don't use a task group from outside the task where you created it.
/// In most cases,
/// the Swift type system prevents a task group from escaping like that
/// because adding a child task to a task group is a mutating operation,
/// and mutation operations can't be performed
/// from a concurrent execution context like a child task.
///
/// Refer to ``TaskGroup`` documentation for detailed discussion of semantics shared between all task groups.
///
/// ### Discarding behavior
/// A discarding task group eagerly discards and releases its child tasks as
/// soon as they complete. This allows for the efficient releasing of memory used
/// by those tasks, which are not retained for future `next()` calls, as would
/// be the case with a ``TaskGroup``.
///
/// ### Cancellation behavior
/// A throwing discarding task group becomes cancelled in one of the following ways:
///
/// - when ``cancelAll()`` is invoked on it,
/// - when an error is thrown out of the `withThrowingDiscardingTaskGroup { ... }` closure,
/// - when the ``Task`` running this task group is cancelled.
///
/// But also, and uniquely in *discarding* task groups:
/// - when *any* of its child tasks throws.
///
/// The group becoming cancelled automatically, and cancelling all of its child tasks,
/// whenever *any* child task throws an error is a behavior unique to discarding task groups,
/// because achieving such semantics is not possible otherwise, due to the missing `next()` method
/// on discarding groups. Accumulating task groups can implement this by manually polling `next()`
/// and deciding to `cancelAll()` when they decide an error should cause the group to become cancelled,
/// however a discarding group cannot poll child tasks for results and therefore assumes that child
/// task throws are an indication of a group wide failure. In order to avoid such behavior,
/// use a ``DiscardingTaskGroup`` instead of a throwing one, or catch specific errors in
/// operations submitted using `addTask`.
///
/// Since a `ThrowingDiscardingTaskGroup` is a structured concurrency primitive, cancellation is
/// automatically propagated through all of its child-tasks (and their child
/// tasks).
///
/// A cancelled task group can still keep adding tasks, however they will start
/// being immediately cancelled, and may act accordingly to this. To avoid adding
/// new tasks to an already cancelled task group, use ``addTaskUnlessCancelled(priority:body:)``
/// rather than the plain ``addTask(priority:body:)`` which adds tasks unconditionally.
///
/// For information about the language-level concurrency model that `DiscardingTaskGroup` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
///
/// - SeeAlso: ``TaskGroup``
/// - SeeAlso: ``ThrowingTaskGroup``
/// - SeeAlso: ``DiscardingTaskGroup``
@available(SwiftStdlib 5.9, *)
@frozen
public struct ThrowingDiscardingTaskGroup<Failure: Error> {

  @usableFromInline
  internal let _group: Builtin.RawPointer

  // No public initializers
  @inlinable
  init(group: Builtin.RawPointer) {
    self._group = group
  }

  /// Await all the remaining tasks on this group.
  @usableFromInline
  internal mutating func awaitAllRemainingTasks(bodyError: Error?) async throws {
    let _: Void? = try await _taskGroupWaitAll(group: _group, bodyError: bodyError)
  }

  /// A Boolean value that indicates whether the group has any remaining tasks.
  ///
  /// At the start of the body of a `withThrowingDiscardingTaskGroup(returning:body:)` call,
  /// the task group is always empty.
  ///
  /// It's guaranteed to be empty when returning from that body
  /// because a task group waits for all child tasks to complete before returning.
  ///
  /// - Returns: `true` if the group has no pending tasks; otherwise `false`.
  public var isEmpty: Bool {
    _taskGroupIsEmpty(_group)
  }

  /// Cancel all of the remaining tasks in the group.
  ///
  /// If you add a task to a group after canceling the group,
  /// that task is canceled immediately after being added to the group.
  ///
  /// Immediately cancelled child tasks should therefore cooperatively check for and
  /// react  to cancellation, e.g. by throwing an `CancellationError` at their
  /// earliest convenience, or otherwise handling the cancellation.
  ///
  /// There are no restrictions on where you can call this method.
  /// Code inside a child task or even another task can cancel a group,
  /// however one should be very careful to not keep a reference to the
  /// group longer than the `with...TaskGroup(...) { ... }` method body is executing.
  ///
  /// - SeeAlso: `Task.isCancelled`
  /// - SeeAlso: `ThrowingDiscardingTaskGroup.isCancelled`
  public func cancelAll() {
    _taskGroupCancelAll(group: _group)
  }

  /// A Boolean value that indicates whether the group was canceled.
  ///
  /// To cancel a group, call the `ThrowingDiscardingTaskGroup.cancelAll()` method.
  ///
  /// If the task that's currently running this group is canceled,
  /// the group is also implicitly canceled,
  /// which is also reflected in this property's value.
  public var isCancelled: Bool {
    return _taskGroupIsCancelled(group: _group)
  }
}

@available(SwiftStdlib 5.9, *)
@available(*, unavailable)
extension ThrowingDiscardingTaskGroup: Sendable { }

// ==== -----------------------------------------------------------------------
// MARK: Runtime functions

/// Always returns `nil`.
@available(SwiftStdlib 5.9, *)
@usableFromInline
@discardableResult
@_silgen_name("swift_taskGroup_waitAll")
func _taskGroupWaitAll<T>(
    group: Builtin.RawPointer,
    bodyError: Error?
) async throws -> T?
