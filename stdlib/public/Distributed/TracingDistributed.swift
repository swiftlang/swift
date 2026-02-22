//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// Check if distributed tracing is currently enabled.
///
/// Use this to avoid expensive operations (like string interpolation) when
/// tracing is disabled.
@available(SwiftStdlib 6.4, *)
@inlinable
public func _distributedTraceIsEnabled() -> Bool {
  return _traceDistributedIsEnabled()
}

@available(SwiftStdlib 6.4, *)
@inlinable
public func _traceDistributedRemoteCall(
  targetActor: some DistributedActor,
  targetIdentifier: String
) {
  guard _distributedTraceIsEnabled() else { return }

  let targetActorID = "\(targetActor.id)"
  unsafe targetActorID.withCString { actorIDPtr in
    unsafe targetIdentifier.withCString { identifierPtr in
      unsafe _traceRemoteCallOutbound(targetActor, actorIDPtr, identifierPtr)
    }
  }
}

@available(SwiftStdlib 6.4, *)
@inlinable
public func _traceDistributedExecuteTarget(
  targetActor: some DistributedActor,
  targetIdentifier: String
) {
  guard _distributedTraceIsEnabled() else { return }

  let targetActorID = "\(targetActor.id)"
  unsafe targetActorID.withCString { actorIDPtr in
    unsafe targetIdentifier.withCString { identifierPtr in
      unsafe _traceExecuteDistributedTarget(targetActor, actorIDPtr, identifierPtr)
    }
  }
}

@available(SwiftStdlib 6.4, *)
@inlinable
public func _traceDistributedInvokeResultHandler(
  targetActor: some DistributedActor,
  targetIdentifier: String,
  error: (any Error)?
) {
  guard _distributedTraceIsEnabled() else { return }

  unsafe targetIdentifier.withCString { identifierPtr in
    unsafe _traceInvokeResultHandler(targetActor, identifierPtr, error == nil)
  }
}

@usableFromInline
@_silgen_name("swift_distributed_trace_is_enabled")
internal func _traceDistributedIsEnabled() -> Bool

@usableFromInline
@_silgen_name("swift_distributed_trace_remote_call_outbound")
internal func _traceRemoteCallOutbound(
  _ targetActor: AnyObject,
  _ targetActorID: UnsafePointer<CChar>?,
  _ targetIdentifier: UnsafePointer<CChar>?
)

@usableFromInline
@_silgen_name("swift_distributed_trace_execute_target")
internal func _traceExecuteDistributedTarget(
  _ targetActor: AnyObject,
  _ targetActorID: UnsafePointer<CChar>?,
  _ targetIdentifier: UnsafePointer<CChar>?
)

@usableFromInline
@_silgen_name("swift_distributed_trace_invoke_result_handler")
internal func _traceInvokeResultHandler(
  _ targetActor: AnyObject,
  _ targetIdentifier: UnsafePointer<CChar>?,
  _ success: Bool
)
