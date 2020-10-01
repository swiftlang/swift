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
@_implementationOnly import _SwiftConcurrencyShims

/// Common protocol to which all actor classes conform.
///
/// The \c Actor protocol provides the core functionality of an actor class,
/// which involves enqueuing new partial tasks to be executed at some
/// point. Actor classes implicitly conform to this protocol as part of their
/// primary class definition.
public protocol Actor: AnyObject {
  /// Enqueue a new partial task that will be executed in the actor's context.
  func enqueue(partialTask: PartialAsyncTask)
}

/// A native actor queue, which schedules partial tasks onto a serial queue.
public struct _NativeActorQueue {
  // TODO: This is just a stub for now
}

/// The default type to be used for an actor's queue when an actor does not
/// provide its own implementation of `enqueue(partialTask:)`.
public typealias _DefaultActorQueue = _NativeActorQueue

/// Called to create a new default actor queue instance for a class of the given
/// type.  The implementation will call this within the actor's initializer to
/// initialize the actor queue.
public func _defaultActorQueueCreate(
  _ actorClass: AnyObject.Type
) -> _DefaultActorQueue {
  _DefaultActorQueue()
}

/// Called by the synthesized implementation of enqueue(partialTask:).
///
/// The implementation is provided with the address of the synthesized instance
/// property for the actor queue, so that it need not be at a fixed offset.
public func _defaultActorQueueEnqueuePartialTask(
  actor: AnyObject,
  queue: inout _DefaultActorQueue,
  partialTask: PartialAsyncTask
) {
  // TODO: Implement queueing.
}
