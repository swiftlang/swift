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

// ==== -----------------------------------------------------------------------
// MARK: Precondition executors

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
///
/// - Parameter executor: the expected current executor
@available(SwiftStdlib 5.9, *) // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
public
func preconditionTaskOnExecutor(
    _ executor: some SerialExecutor,
    message: @autoclosure () -> String = String(),
    file: StaticString = #fileID, line: UInt = #line
) {
  guard _isDebugAssertConfiguration() || _isReleaseAssertConfiguration() else {
    return
  }

  let expectationCheck = _taskIsCurrentExecutor(executor.asUnownedSerialExecutor().executor)

  /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
  precondition(expectationCheck,
      // TODO: offer information which executor we actually got
      "Incorrect actor executor assumption; Expected '\(executor)' executor. \(message())",
      file: file, line: line) // short-cut so we get the exact same failure reporting semantics
}

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
///
/// - Parameter actor: the actor whose serial executor we expect to be the current executor
@available(SwiftStdlib 5.9, *) // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
public
func preconditionTaskOnActorExecutor(
    _ actor: some Actor,
    message: @autoclosure () -> String = String(),
    file: StaticString = #fileID, line: UInt = #line
) {
  guard _isDebugAssertConfiguration() || _isReleaseAssertConfiguration() else {
    return
  }

  let expectationCheck = _taskIsCurrentExecutor(actor.unownedExecutor.executor)

  // TODO: offer information which executor we actually got
  precondition(expectationCheck,
      // TODO: figure out a way to get the typed repr out of the unowned executor
      "Incorrect actor executor assumption; Expected '\(actor.unownedExecutor)' executor. \(message())",
      file: file, line: line)
}

// ==== -----------------------------------------------------------------------
// MARK: Assert executors

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
///
/// - Parameter executor: the expected current executor
@available(SwiftStdlib 5.9, *) // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
public
func assertTaskOnExecutor(
    _ executor: some SerialExecutor,
    _ message: @autoclosure () -> String = String(),
    file: StaticString = #fileID, line: UInt = #line
) {
  guard _isDebugAssertConfiguration() else {
    return
  }

  guard _taskIsCurrentExecutor(executor.asUnownedSerialExecutor().executor) else {
    // TODO: offer information which executor we actually got
    let msg = "Incorrect actor executor assumption; Expected '\(executor)' executor. \(message())"
    /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
    assertionFailure(msg, file: file, line: line)
    return
  }
}

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
///
///
/// - Parameter actor: the actor whose serial executor we expect to be the current executor
@available(SwiftStdlib 5.9, *) // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
public
func assertTaskOnActorExecutor(
    _ actor: some Actor,
    _ message: @autoclosure () -> String = String(),
    file: StaticString = #fileID, line: UInt = #line
) {
  guard _isDebugAssertConfiguration() else {
    return
  }

  guard _taskIsCurrentExecutor(actor.unownedExecutor.executor) else {
    // TODO: offer information which executor we actually got
    // TODO: figure out a way to get the typed repr out of the unowned executor
    let msg = "Incorrect actor executor assumption; Expected '\(actor.unownedExecutor)' executor. \(message())"
    /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
    assertionFailure(msg, file: file, line: line) // short-cut so we get the exact same failure reporting semantics
    return
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Assume Executor

/// A safe way to synchronously assume that the current execution context belongs to the MainActor.
///
/// This API should only be used as last resort, when it is not possible to express the current
/// execution context definitely belongs to the main actor in other ways. E.g. one may need to use
/// this in a delegate style API, where a synchronous method is guaranteed to be called by the
/// main actor, however it is not possible to annotate this legacy API with `@MainActor`.
///
/// - Warning: If the current executor is *not* the MainActor's serial executor, this function will crash.
///
/// Note that this check is performed against the MainActor's serial executor, meaning that
/// if another actor uses the same serial executor--by using ``MainActor/sharedUnownedExecutor``
/// as its own ``Actor/unownedExecutor``--this check will succeed, as from a concurrency safety
/// perspective, the serial executor guarantees mutual exclusion of those two actors.
@available(SwiftStdlib 5.9, *) // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
@_unavailableFromAsync(message: "await the call to the @MainActor closure directly")
public
func assumeOnMainActorExecutor<T>(
    _ operation: @MainActor () throws -> T,
    file: StaticString = #fileID, line: UInt = #line
) rethrows -> T {
  typealias YesMainActor = @MainActor () throws -> T
  typealias NoMainActor = () throws -> T

  /// This is guaranteed to be fatal if the check fails,
  /// as this is our "safe" version of this API.
  guard _taskIsCurrentExecutor(Builtin.buildMainActorExecutorRef()) else {
    // TODO: offer information which executor we actually got
    fatalError("Incorrect actor executor assumption; Expected 'MainActor' executor.", file: file, line: line)
  }

  // To do the unsafe cast, we have to pretend it's @escaping.
  return try withoutActuallyEscaping(operation) {
    (_ fn: @escaping YesMainActor) throws -> T in
    let rawFn = unsafeBitCast(fn, to: NoMainActor.self)
    return try rawFn()
  }
}

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
@available(SwiftStdlib 5.9, *) // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
@_unavailableFromAsync(message: "express the closure as an explicit function declared on the specified 'actor' instead")
public
func assumeOnActorExecutor<Act: Actor, T>(
    _ actor: Act,
    _ operation: (isolated Act) throws -> T,
    file: StaticString = #fileID, line: UInt = #line
) rethrows -> T {
  typealias YesActor = (isolated Act) throws -> T
  typealias NoActor = (Act) throws -> T

  /// This is guaranteed to be fatal if the check fails,
  /// as this is our "safe" version of this API.
  let executor: Builtin.Executor = actor.unownedExecutor.executor
  guard _taskIsCurrentExecutor(executor) else {
    // TODO: offer information which executor we actually got
    fatalError("Incorrect actor executor assumption; Expected same executor as \(actor).", file: file, line: line)
  }

  // To do the unsafe cast, we have to pretend it's @escaping.
  return try withoutActuallyEscaping(operation) {
    (_ fn: @escaping YesActor) throws -> T in
    let rawFn = unsafeBitCast(fn, to: NoActor.self)
    return try rawFn(actor)
  }
}

// TODO(ktoso): implement assume for distributed actors as well