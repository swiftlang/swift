//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

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