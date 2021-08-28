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

/// Report a call to a _remote function on a distributed (remote) actor,
/// that was not dynamically replaced by some specific ActorTransport library.
@_transparent
public func _missingDistributedActorTransport(
  className: StaticString, functionName: StaticString,
  file: StaticString, line: UInt, column: UInt
) -> Never {
  // This function is marked @_transparent so that it is inlined into the caller
  // (the remote function stub), and, depending on the build configuration,
  // redundant parameter values (#file etc.) are eliminated, and don't leak
  // information about the user's source.
  fatalError(
    """
    Invoked remote placeholder function '\(functionName)' on remote \
    distributed actor of type '\(className)'. Configure an appropriate \
    'ActorTransport' for this actor to resolve this error (e.g. by depending \
    on some specific transport library).
    """, file: file, line: line)
}