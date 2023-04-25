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
import _Concurrency

// ==== -----------------------------------------------------------------------
// MARK: Precondition APIs

@available(SwiftStdlib 5.9, *)
extension DistributedActor {
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
  public nonisolated func preconditionIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    guard _isDebugAssertConfiguration() || _isReleaseAssertConfiguration() else {
      return
    }

    let unownedExecutor = self.unownedExecutor
    let expectationCheck = _taskIsCurrentExecutor(unownedExecutor._executor)

    // TODO: offer information which executor we actually got
    precondition(expectationCheck,
        // TODO: figure out a way to get the typed repr out of the unowned executor
        "Incorrect actor executor assumption; Expected '\(self.unownedExecutor)' executor. \(message())",
        file: file, line: line)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Assert APIs

@available(SwiftStdlib 5.9, *)
extension DistributedActor {
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
  @_transparent
  public nonisolated func assertIsolated(
      _ message: @autoclosure () -> String = String(),
      file: StaticString = #fileID, line: UInt = #line
  ) {
    guard _isDebugAssertConfiguration() else {
      return
    }

    let unownedExecutor = self.unownedExecutor
    guard _taskIsCurrentExecutor(unownedExecutor._executor) else {
      // TODO: offer information which executor we actually got
      // TODO: figure out a way to get the typed repr out of the unowned executor
      let msg = "Incorrect actor executor assumption; Expected '\(unownedExecutor)' executor. \(message())"
      /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
      assertionFailure(msg, file: file, line: line) // short-cut so we get the exact same failure reporting semantics
      return
    }
  }
}


// ==== -----------------------------------------------------------------------
// MARK: Assume APIs

@available(SwiftStdlib 5.9, *)
extension DistributedActor {

  /// Assume that the current actor is a local distributed actor and that the currently executing context is the same as that actors
  /// serial executor, or crash.
  ///
  /// This method allows developers to *assume and verify* that the currently executing synchronous function
  /// is actually executing on the serial executor that this distributed (local) actor is using.
  ///
  /// If that is the case, the operation is invoked with an `isolated` version of the actoe,
  /// allowing synchronous access to actor local state without hopping through asynchronous boundaries.
  ///
  /// If the current context is not running on the actor's serial executor, or if the actor is a reference to a remote actor,
  /// this method will crash with a fatalError (similar to ``preconditionIsolated()``).
  ///
  /// This method can only be used from synchronous functions, as asynchronous ones should instead
  /// perform normal method call to the actor.
  ///
  /// - Parameters:
  ///   - operation: the operation that will be executed if the current context is executing on the actors serial executor, and the actor is a local reference
  ///   - file: source location where the assume call is made
  ///   - file: source location where the assume call is made
  /// - Returns: the return value of the `operation`
  /// - Throws: rethrows the `Error` thrown by the operation if it threw
  @available(SwiftStdlib 5.9, *)
  @_unavailableFromAsync(message: "express the closure as an explicit function declared on the specified 'distributed actor' instead")
  public nonisolated func assumeIsolated<T>(
      _ operation: (isolated Self) throws -> T,
      file: StaticString = #fileID, line: UInt = #line
  ) rethrows -> T {
    typealias YesActor = (isolated Self) throws -> T
    typealias NoActor = (Self) throws -> T

    guard __isLocalActor(self) else {
      fatalError("Cannot assume to be 'isolated \(Self.self)' since distributed actor '\(self)' is a remote actor reference.")
    }

    let unownedExecutor = self.unownedExecutor
    guard _taskIsCurrentExecutor(unownedExecutor._executor) else {
      // TODO: offer information which executor we actually got when
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

@available(SwiftStdlib 5.1, *)
@usableFromInline
@_silgen_name("swift_task_isCurrentExecutor")
func _taskIsCurrentExecutor(_ executor: Builtin.Executor) -> Bool
