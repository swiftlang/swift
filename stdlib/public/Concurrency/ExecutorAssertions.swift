//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
import SwiftShims

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

// ==== -----------------------------------------------------------------------
// MARK: Precondition executors

@available(SwiftStdlib 5.9, *)
extension SerialExecutor {
  /// Unconditionally if the current task is executing on the expected serial executor,
  /// and if not crash the program offering information about the executor mismatch.
  ///
  /// This function's effect varies depending on the build flag used:
  ///
  /// * In playgrounds and `-Onone` builds (the default for Xcode's Debug
  ///   configuration), stops program execution in a debuggable state after
  ///   printing `message`.
  ///
  /// * In `-O` builds (the default for Xcode's Release configuration), stops
  ///   program execution.
  ///
  /// * In `-Ounchecked` builds, the optimizer may assume that this function is
  ///   never called. Failure to satisfy that assumption is a serious
  ///   programming error.
  @available(SwiftStdlib 5.9, *)
  @_unavailableInEmbedded
  public func preconditionIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    guard _isDebugAssertConfiguration() || _isReleaseAssertConfiguration() else {
      return
    }

    let expectationCheck = _taskIsCurrentExecutor(self.asUnownedSerialExecutor().executor)

    /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
    precondition(expectationCheck,
        // TODO: offer information which executor we actually got
        "Incorrect actor executor assumption; Expected '\(self)' executor. \(message())",
        file: file, line: line) // short-cut so we get the exact same failure reporting semantics
  }
}

@available(SwiftStdlib 5.9, *)
extension Actor {
  /// Unconditionally if the current task is executing on the serial executor of the passed in `actor`,
  /// and if not crash the program offering information about the executor mismatch.
  ///
  /// This function's effect varies depending on the build flag used:
  ///
  /// * In playgrounds and `-Onone` builds (the default for Xcode's Debug
  ///   configuration), stops program execution in a debuggable state after
  ///   printing `message`.
  ///
  /// * In `-O` builds (the default for Xcode's Release configuration), stops
  ///   program execution.
  ///
  /// * In `-Ounchecked` builds, the optimizer may assume that this function is
  ///   never called. Failure to satisfy that assumption is a serious
  ///   programming error.
  @available(SwiftStdlib 5.9, *)
  @_unavailableInEmbedded
  public nonisolated func preconditionIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    guard _isDebugAssertConfiguration() || _isReleaseAssertConfiguration() else {
      return
    }

    let expectationCheck = _taskIsCurrentExecutor(self.unownedExecutor.executor)

    // TODO: offer information which executor we actually got
    precondition(expectationCheck,
        // TODO: figure out a way to get the typed repr out of the unowned executor
        "Incorrect actor executor assumption; Expected '\(self.unownedExecutor)' executor. \(message())",
        file: file, line: line)
  }
}

@available(SwiftStdlib 5.9, *)
extension GlobalActor {
  /// Unconditionally if the current task is executing on the serial executor of the passed in `actor`,
  /// and if not crash the program offering information about the executor mismatch.
  ///
  /// This function's effect varies depending on the build flag used:
  ///
  /// * In playgrounds and `-Onone` builds (the default for Xcode's Debug
  ///   configuration), stops program execution in a debuggable state after
  ///   printing `message`.
  ///
  /// * In `-O` builds (the default for Xcode's Release configuration), stops
  ///   program execution.
  ///
  /// * In `-Ounchecked` builds, the optimizer may assume that this function is
  ///   never called. Failure to satisfy that assumption is a serious
  ///   programming error.
  @available(SwiftStdlib 5.9, *)
  @_unavailableInEmbedded
  public static func preconditionIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    try Self.shared.preconditionIsolated(message(), file: file, line: line)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Assert executors

@available(SwiftStdlib 5.9, *)
extension SerialExecutor {
  /// Performs an executor check in debug builds.
  ///
  /// * In playgrounds and `-Onone` builds (the default for Xcode's Debug
  ///   configuration): If `condition` evaluates to `false`, stop program
  ///   execution in a debuggable state after printing `message`.
  ///
  /// * In `-O` builds (the default for Xcode's Release configuration),
  ///   `condition` is not evaluated, and there are no effects.
  ///
  /// * In `-Ounchecked` builds, `condition` is not evaluated, but the optimizer
  ///   may assume that it *always* evaluates to `true`. Failure to satisfy that
  ///   assumption is a serious programming error.
  @available(SwiftStdlib 5.9, *)
  @_unavailableInEmbedded
  public func assertIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    guard _isDebugAssertConfiguration() else {
      return
    }

    guard _taskIsCurrentExecutor(self.asUnownedSerialExecutor().executor) else {
      // TODO: offer information which executor we actually got
      let msg = "Incorrect actor executor assumption; Expected '\(self)' executor. \(message())"
      /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
      assertionFailure(msg, file: file, line: line)
      return
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension Actor {
  /// Performs an executor check in debug builds.
  ///
  /// * In playgrounds and `-Onone` builds (the default for Xcode's Debug
  ///   configuration): If `condition` evaluates to `false`, stop program
  ///   execution in a debuggable state after printing `message`.
  ///
  /// * In `-O` builds (the default for Xcode's Release configuration),
  ///   `condition` is not evaluated, and there are no effects.
  ///
  /// * In `-Ounchecked` builds, `condition` is not evaluated, but the optimizer
  ///   may assume that it *always* evaluates to `true`. Failure to satisfy that
  ///   assumption is a serious programming error.
  @available(SwiftStdlib 5.9, *)
  @_unavailableInEmbedded
  public nonisolated func assertIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    guard _isDebugAssertConfiguration() else {
      return
    }

    guard _taskIsCurrentExecutor(self.unownedExecutor.executor) else {
      // TODO: offer information which executor we actually got
      // TODO: figure out a way to get the typed repr out of the unowned executor
      let msg = "Incorrect actor executor assumption; Expected '\(self.unownedExecutor)' executor. \(message())"
      /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
      assertionFailure(msg, file: file, line: line) // short-cut so we get the exact same failure reporting semantics
      return
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension GlobalActor {
  /// Performs an executor check in debug builds.
  ///
  /// * In playgrounds and `-Onone` builds (the default for Xcode's Debug
  ///   configuration): If `condition` evaluates to `false`, stop program
  ///   execution in a debuggable state after printing `message`.
  ///
  /// * In `-O` builds (the default for Xcode's Release configuration),
  ///   `condition` is not evaluated, and there are no effects.
  ///
  /// * In `-Ounchecked` builds, `condition` is not evaluated, but the optimizer
  ///   may assume that it *always* evaluates to `true`. Failure to satisfy that
  ///   assumption is a serious programming error.
  @available(SwiftStdlib 5.9, *)
  @_unavailableInEmbedded
  public static func assertIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    try Self.shared.assertIsolated(message(), file: file, line: line)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Assume Executor

@available(SwiftStdlib 5.9, *)
extension Actor {
  /// A safe way to synchronously assume that the current execution context belongs to the passed in actor.
  ///
  /// This API should only be used as last resort, when it is not possible to express the current
  /// execution context definitely belongs to the specified actor in other ways. E.g. one may need to use
  /// this in a delegate style API, where a synchronous method is guaranteed to be called by the
  /// specified actor, however it is not possible to move this method as being declared on the specified actor.
  ///
  /// - Warning: If the current executor is *not* the expected serial executor, this function will crash.
  ///
  /// Note that this check is performed against the passed in actor's serial executor, meaning that
  /// if another actor uses the same serial executor--by using that actor's ``Actor/unownedExecutor``
  /// as its own ``Actor/unownedExecutor``--this check will succeed, as from a concurrency safety
  /// perspective, the serial executor guarantees mutual exclusion of those two actors.
  @available(SwiftStdlib 5.9, *)
  @_unavailableFromAsync(message: "express the closure as an explicit function declared on the specified 'actor' instead")
  @_unavailableInEmbedded
  public nonisolated func assumeIsolated<T>(
      _ operation: (isolated Self) throws -> T,
      file: StaticString = #fileID, line: UInt = #line
  ) rethrows -> T {
    typealias YesActor = (isolated Self) throws -> T
    typealias NoActor = (Self) throws -> T

    /// This is guaranteed to be fatal if the check fails,
    /// as this is our "safe" version of this API.
    let executor: Builtin.Executor = self.unownedExecutor.executor
    guard _taskIsCurrentExecutor(executor) else {
      // TODO: offer information which executor we actually got
      fatalError("Incorrect actor executor assumption; Expected same executor as \(self).", file: file, line: line)
    }

    // To do the unsafe cast, we have to pretend it's @escaping.
    return try withoutActuallyEscaping(operation) {
      (_ fn: @escaping YesActor) throws -> T in
      let rawFn = unsafeBitCast(fn, to: NoActor.self)
      return try rawFn(self)
    }
  }
}

#endif // not SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY