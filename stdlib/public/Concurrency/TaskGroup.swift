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
    body: (inout Task.Group<TaskResult>) async throws -> BodyResult
  ) async rethrows -> BodyResult {
    fatalError("\(#function) not implemented yet.")
  }

  /// A task group serves as storage for dynamically started tasks.
  ///
  /// Its intended use is with the
  /* @unmoveable */
  public struct Group<TaskResult> {
    /// No public initializers
    private init() {}

    // Swift will statically prevent this type from being copied or moved.
    // For now, that implies that it cannot be used with generics.

    /// Add a child task to the group.
    ///
    /// ### Error handling
    /// Operations are allowed to throw.
    ///
    /// in which case the `await try next()`
    /// invocation corresponding to the failed task will re-throw the given task.
    ///
    /// - Parameters:
    ///   - overridingPriority: override priority of the operation task
    ///   - operation: operation to execute and add to the group
    public mutating func add(
      overridingPriority: Priority? = nil,
      operation: () async throws -> TaskResult
    ) async {
      fatalError("\(#function) not implemented yet.")
    }

    /// Add a child task and return a `Task.Handle` that can be used to manage it.
    ///
    /// The task's result is accessible either via the returned `handle` or the
    /// `group.next()` function (as any other `add`-ed task).
    ///
    /// - Parameters:
    ///   - overridingPriority: override priority of the operation task
    ///   - operation: operation to execute and add to the group
    public mutating func addWithHandle(
      overridingPriority: Priority? = nil,
      operation: () async throws -> TaskResult
    ) async -> Handle<TaskResult> {
      fatalError("\(#function) not implemented yet.")
    }

    /// Wait for a child task to complete and return the result it returned,
    /// or else return.
    ///
    ///
    public mutating func next() async throws -> TaskResult? {
      fatalError("\(#function) not implemented yet.")
    }

    /// Query whether the group has any remaining tasks.
    ///
    /// Task groups are always empty upon entry to the `withGroup` body, and
    /// become empty again when `withGroup` returns (either by awaiting on all
    /// pending tasks or cancelling them).
    ///
    /// - Returns: `true` if the group has no pending tasks, `false` otherwise.
    public var isEmpty: Bool {
      fatalError("\(#function) not implemented yet.")
    }

    /// Cancel all the remaining tasks in the group.
    ///
    /// A cancelled group will not will NOT accept new tasks being added into it.
    ///
    /// Any results, including errors thrown by tasks affected by this
    /// cancellation, are silently discarded.
    ///
    /// - SeeAlso: `Task.addCancellationHandler`
    public mutating func cancelAll() {
      fatalError("\(#function) not implemented yet.")
    }
  }
}
