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
@available(SwiftStdlib 5.9, *)
public func preconditionOnExecutor(
    of actor: some DistributedActor,
    _ message: @autoclosure () -> String = String(),
    file: StaticString = #fileID, line: UInt = #line
) {
  guard _isDebugAssertConfiguration() || _isReleaseAssertConfiguration() else {
    return
  }

  guard __isLocalActor(actor) else {
    return
  }

  guard let unownedExecutor = actor.localUnownedExecutor else {
    preconditionFailure(
        "Incorrect actor executor assumption; Distributed actor \(actor) is 'local' but has no executor!",
        file: file, line: line)
  }

  let expectationCheck = _taskIsCurrentExecutor(unownedExecutor._executor)

  // TODO: offer information which executor we actually got
  precondition(expectationCheck,
      // TODO: figure out a way to get the typed repr out of the unowned executor
      "Incorrect actor executor assumption; Expected '\(unownedExecutor)' executor. \(message())",
      file: file, line: line)
}

// ==== -----------------------------------------------------------------------
// MARK: Assert APIs

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
@available(SwiftStdlib 5.9, *)
@_transparent
public func assertOnExecutor(
    of actor: some DistributedActor,
    _ message: @autoclosure () -> String = String(),
    file: StaticString = #fileID, line: UInt = #line
) {
  guard _isDebugAssertConfiguration() else {
    return
  }

  guard __isLocalActor(actor) else {
    return
  }

  guard let unownedExecutor = actor.localUnownedExecutor else {
    preconditionFailure(
        "Incorrect actor executor assumption; Distributed actor \(actor) is 'local' but has no executor!",
        file: file, line: line)
  }

  guard _taskIsCurrentExecutor(unownedExecutor._executor) else {
    // TODO: offer information which executor we actually got
    // TODO: figure out a way to get the typed repr out of the unowned executor
    let msg = "Incorrect actor executor assumption; Expected '\(unownedExecutor)' executor. \(message())"
    /// TODO: implement the logic in-place perhaps rather than delegating to precondition()?
    assertionFailure(msg, file: file, line: line) // short-cut so we get the exact same failure reporting semantics
    return
  }
}


// ==== -----------------------------------------------------------------------
// MARK: Assume APIs

@available(SwiftStdlib 5.9, *)
@_unavailableFromAsync(message: "express the closure as an explicit function declared on the specified 'distributed actor' instead")
public func assumeOnLocalDistributedActorExecutor<Act: DistributedActor, T>(
    of actor: Act,
    _ operation: (isolated Act) throws -> T,
    file: StaticString = #fileID, line: UInt = #line
) rethrows -> T {
  typealias YesActor = (isolated Act) throws -> T
  typealias NoActor = (Act) throws -> T

  guard __isLocalActor(actor) else {
    fatalError("Cannot assume to be 'isolated \(Act.self)' since distributed actor '\(actor)' is remote.")
  }

  /// This is guaranteed to be fatal if the check fails,
  /// as this is our "safe" version of this API.
  guard let executor = actor.localUnownedExecutor else {
    fatalError("Distributed local actor MUST have executor, but was nil")
  }
  guard _taskIsCurrentExecutor(executor._executor) else {
    // TODO: offer information which executor we actually got when
    fatalError("Incorrect actor executor assumption; Expected same executor as \(actor).", file: file, line: line)
  }

  // To do the unsafe cast, we have to pretend it's @escaping.
  return try withoutActuallyEscaping(operation) {
    (_ fn: @escaping YesActor) throws -> T in
    let rawFn = unsafeBitCast(fn, to: NoActor.self)
    return try rawFn(actor)
  }
}

@available(SwiftStdlib 5.1, *)
@usableFromInline
@_silgen_name("swift_task_isCurrentExecutor")
func _taskIsCurrentExecutor(_ executor: Builtin.Executor) -> Bool
