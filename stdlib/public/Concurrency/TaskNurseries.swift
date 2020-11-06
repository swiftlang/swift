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

// ==== Task Nursery -----------------------------------------------------------

extension Task {

  /// Starts a new nursery which provides a scope in which a dynamic number of
  /// tasks may be spawned.
  ///
  /// Tasks added to the nursery by `nursery.add()` will automatically be
  /// awaited on when the scope exits.
  ///
  /// ### Implicit awaiting
  /// When results of tasks added to the nursery need to be collected, one will
  /// gather task's results using the `while let result = await nursery.next() { ... }`
  /// pattern.
  ///
  /// ### Cancellation
  /// If any of the tasks throws the nursery and all of its tasks will be cancelled,
  /// and the error will be re-thrown by `withNursery`.
  ///
  /// Postcondition:
  /// Once `withNursery` returns it is guaranteed that the *nursery* is *empty*.
  ///
  /// This is achieved in the following way:
  /// - if the body returns normally:
  ///   - the nursery will await any not yet complete tasks,
  ///     - if any of those tasks throws, the remaining tasks will be cancelled,
  ///   - once the `withNursery` returns the nursery is guaranteed to be empty.
  /// - if the body throws:
  ///   - all tasks remaining in the nursery will be automatically cancelled.
  ///
  // TODO: Do we have to add a different nursery type to accommodate throwing
  //       tasks without forcing users to use Result?  I can't think of how that
  //       could be propagated out of the callback body reasonably, unless we
  //       commit to doing multi-statement closure typechecking.
  public static func withNursery<TaskResult, BodyResult>(
    resultType: TaskResult.Type,
    returning returnType: BodyResult.Type = BodyResult.self,
    body: (inout Nursery<TaskResult>) async throws -> BodyResult
  ) async rethrows -> BodyResult {
    fatalError("\(#function) not implemented yet.")
  }

  /// A nursery provides a scope within which a dynamic number of tasks may be
  /// started and added to the nursery.
  /* @unmoveable */
  public struct Nursery<TaskResult> {
    /// No public initializers
    private init() {}

    // Swift will statically prevent this type from being copied or moved.
    // For now, that implies that it cannot be used with generics.

    /// Add a child task to the nursery.
    ///
    /// ### Error handling
    /// Operations are allowed to throw.
    ///
    /// in which case the `await try next()`
    /// invocation corresponding to the failed task will re-throw the given task.
    ///
    /// - Parameters:
    ///   - overridingPriority: override priority of the operation task
    ///   - operation: operation to execute and add to the nursery
    public mutating func add(
      overridingPriority: Priority? = nil,
      operation: () async throws -> TaskResult
    ) async {
      fatalError("\(#function) not implemented yet.")
    }

    /// Add a child task and return a `Task.Handle` that can be used to manage it.
    ///
    /// The task's result is accessible either via the returned `handle` or the
    /// `nursery.next()` function (as any other `add`-ed task).
    ///
    /// - Parameters:
    ///   - overridingPriority: override priority of the operation task
    ///   - operation: operation to execute and add to the nursery
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

    /// Query whether the nursery has any remaining tasks.
    ///
    /// Nurseries are always empty upon entry to the `withNursery` body, and
    /// become empty again when `withNursery` returns (either by awaiting on all
    /// pending tasks or cancelling them).
    ///
    /// - Returns: `true` if the nursery has no pending tasks, `false` otherwise.
    public var isEmpty: Bool {
      fatalError("\(#function) not implemented yet.")
    }

    /// Cancel all the remaining tasks in the nursery.
    ///
    /// A cancelled nursery will not will NOT accept new tasks being added into it.
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
