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
@_spi(ConcurrencyExecutors) import _Concurrency

@available(SwiftStdlib 5.9, *)
@_unavailableFromAsync(message: "express the closure as an explicit function declared on the specified 'distributed actor' instead")
public
func assumeOnLocalDistributedActorExecutor<Act: DistributedActor, T>(
    _ actor: Act,
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
  let executor: Builtin.Executor = actor.unownedExecutor._executor
  guard _taskIsCurrentExecutor(executor) else {
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
