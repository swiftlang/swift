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
/// A task's execution can be seen as a series of periods where the task was
/// running. Each such period ends at a suspension point or -- finally -- the
/// completion of the task.
///
/// These partial periods towards the task's completion are `PartialAsyncTask`.
/// Partial tasks are generally not interacted with by end-users directly,
/// unless implementing a scheduler.
public struct Task {
}

// ==== UnsafeContinuation -----------------------------------------------------

extension Task {
  public struct UnsafeContinuation<T> {
    /// Return a value into the continuation and make the task schedulable.
    ///
    /// The task will never run synchronously, even if the task does not
    /// need to be resumed on a specific executor.
    ///
    /// This is appropriate when the caller is something "busy", like an event
    /// loop, and doesn't want to be potentially delayed by arbitrary work.
    public func resume(returning: T) {
      fatalError("\(#function) not implemented yet.")
    }
  }

  public struct UnsafeThrowingContinuation<T, E: Error> {
    /// Return a value into the continuation and make the task schedulable.
    ///
    /// The task will never run synchronously, even if the task does not
    /// need to be resumed on a specific executor.
    ///
    /// This is appropriate when the caller is something "busy", like an event
    /// loop, and doesn't want to be potentially delayed by arbitrary work.
    public func resume(returning: T) {
      fatalError("\(#function) not implemented yet.")
    }

    /// Resume the continuation with an error and make the task schedulable.
    ///
    /// The task will never run synchronously, even if the task does not
    /// need to be resumed on a specific executor.
    ///
    /// This is appropriate when the caller is something "busy", like an event
    /// loop, and doesn't want to be potentially delayed by arbitrary work.
    public func resume(throwing: E) {
      fatalError("\(#function) not implemented yet.")
    }
  }

  /// The operation functions must resume the continuation *exactly once*.
  ///
  /// The continuation will not begin executing until the operation function returns.
  public static func withUnsafeContinuation<T>(
    operation: (UnsafeContinuation<T>) -> Void
  ) async -> T {
    fatalError("\(#function) not implemented yet.")
  }

  /// The operation functions must resume the continuation *exactly once*.
  ///
  /// The continuation will not begin executing until the operation function returns.
  public static func withUnsafeThrowingContinuation<T>(
    operation: (UnsafeThrowingContinuation<T, Error>) -> Void
  ) async throws -> T {
    fatalError("\(#function) not implemented yet.")
  }
}
