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

// The tracing runtime entry points these functions call were introduced in
// SwiftStdlib 6.5, but the tracing calls themselves are emitted from the
// distributed thunks synthesized in *client* code, and from
// 'executeDistributedTarget' below, neither of which can require 6.5.
//
// The entry points are therefore declared '@available(SwiftStdlib 6.5, *)' and
// each of the functions below is '@_alwaysEmitIntoClient' and performs the
// '#available' check itself. That way callers can invoke them unconditionally
// and tracing simply does nothing when the runtime does not provide it.

/// Check if tracing of the Distributed module is currently enabled.
///
/// Use this to avoid expensive operations (like string interpolation) when
/// tracing is disabled.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _distributedTraceIsEnabled() -> Bool {
  if #available(anyAppleOS 9999, *) {
    return _traceDistributedIsEnabled()
  }
  return false
}

/// Invokes `body` with the qualified type name of `error`, or with nil when the
/// traced operation succeeded.
///
/// Pass `failed` to report a failure when no error value is available, as in a
/// `defer` that cannot see the error propagating out of the operation. `body`
/// then gets an empty name, which the runtime records as a failure without an
/// error type.
///
/// Only the error's *type* is reported. Its description is deliberately not
/// traced, because an error value can carry data from the call it failed on,
/// whereas the type name is safe to log publicly.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
internal func _withDistributedTraceErrorType<Result>(
  _ error: (any Error)?,
  failed: Bool = false,
  _ body: (UnsafePointer<CChar>?) -> Result
) -> Result {
  guard let error else {
    guard failed else { return body(nil) }
    return "".withCString { namePtr in unsafe body(namePtr) }
  }
  return _typeName(type(of: error), qualified: true).withCString { namePtr in
    unsafe body(namePtr)
  }
}

/// Begins an interval measuring the whole outgoing call: from just before
/// `remoteCall` is invoked until it returns, spanning encoding, the transport
/// round trip and decoding the reply.
///
/// The returned span ID must be passed to `_traceDistributedRemoteCallEnd`. It
/// is 0 when tracing is disabled, in which case ending the interval is a no-op.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedRemoteCall(
  targetActor: some DistributedActor,
  targetIdentifier: String
) -> UInt64 {
  guard #available(anyAppleOS 9999, *) else { return 0 }
  guard _distributedTraceIsEnabled() else { return 0 }

  let targetActorID = "\(targetActor.id)"
  return targetActorID.withCString { actorIDPtr in
    unsafe targetIdentifier.withCString { identifierPtr in
      unsafe _traceRemoteCallOutboundBegin(targetActor, actorIDPtr, identifierPtr)
    }
  }
}

/// Ends the interval started by `_traceDistributedRemoteCall`.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedRemoteCallEnd(
  _ spanID: UInt64,
  error: (any Error)? = nil,
  failed: Bool = false
) {
  guard #available(anyAppleOS 9999, *) else { return }
  guard spanID != 0 else { return }
  unsafe _withDistributedTraceErrorType(error, failed: failed) { errorTypePtr in
    unsafe _traceRemoteCallOutboundEnd(spanID, errorTypePtr)
  }
}

/// Begins an interval measuring the encoding of an outgoing invocation:
/// the generic substitutions, arguments, error and return types, up to and
/// including `doneRecording`.
///
/// The returned span ID must be passed to
/// `_traceDistributedEncodeArgumentsEnd`. It is 0 when tracing is disabled, in
/// which case ending the interval is a no-op.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedEncodeArgumentsBegin(
  targetActor: some DistributedActor,
  targetIdentifier: String,
  argumentCount: Int
) -> UInt64 {
  guard #available(anyAppleOS 9999, *) else { return 0 }
  guard _distributedTraceIsEnabled() else { return 0 }

  return targetIdentifier.withCString { identifierPtr in
    unsafe _traceEncodeArgumentsBegin(targetActor, identifierPtr, argumentCount)
  }
}

/// Ends the interval started by `_traceDistributedEncodeArgumentsBegin`.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedEncodeArgumentsEnd(
  _ spanID: UInt64,
  error: (any Error)? = nil,
  failed: Bool = false
) {
  guard #available(anyAppleOS 9999, *) else { return }
  guard spanID != 0 else { return }
  unsafe _withDistributedTraceErrorType(error, failed: failed) { errorTypePtr in
    unsafe _traceEncodeArgumentsEnd(spanID, errorTypePtr)
  }
}

/// Begins an interval measuring the decoding of an incoming invocation:
/// the generic substitutions, witness tables, parameter types and return type.
///
/// The decoding of the individual argument *values* happens inside the
/// distributed accessor (via `decodeNextArgument`) and is therefore not part of
/// this interval.
///
/// The returned span ID must be passed to
/// `_traceDistributedDecodeArgumentsEnd`. It is 0 when tracing is disabled, in
/// which case ending the interval is a no-op.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedDecodeArgumentsBegin(
  targetActor: some DistributedActor,
  targetIdentifier: String
) -> UInt64 {
  guard #available(anyAppleOS 9999, *) else { return 0 }
  guard _distributedTraceIsEnabled() else { return 0 }

  return targetIdentifier.withCString { identifierPtr in
    unsafe _traceDecodeArgumentsBegin(targetActor, identifierPtr)
  }
}

/// Ends the interval started by `_traceDistributedDecodeArgumentsBegin`.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedDecodeArgumentsEnd(
  _ spanID: UInt64,
  argumentCount: Int,
  error: (any Error)? = nil,
  failed: Bool = false
) {
  guard #available(anyAppleOS 9999, *) else { return }
  guard spanID != 0 else { return }
  unsafe _withDistributedTraceErrorType(error, failed: failed) { errorTypePtr in
    unsafe _traceDecodeArgumentsEnd(spanID, argumentCount, errorTypePtr)
  }
}

/// Begins an interval measuring the whole inbound execution of a call, from
/// `executeDistributedTarget` being invoked until it returns, spanning decoding,
/// invoking the target and the result handler.
///
/// The returned span ID must be passed to
/// `_traceDistributedExecuteTargetEnd`. It is 0 when tracing is disabled, in
/// which case ending the interval is a no-op.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedExecuteTarget(
  targetActor: some DistributedActor,
  targetIdentifier: String
) -> UInt64 {
  guard #available(anyAppleOS 9999, *) else { return 0 }
  guard _distributedTraceIsEnabled() else { return 0 }

  let targetActorID = "\(targetActor.id)"
  return targetActorID.withCString { actorIDPtr in
    unsafe targetIdentifier.withCString { identifierPtr in
      unsafe _traceExecuteDistributedTargetBegin(targetActor, actorIDPtr, identifierPtr)
    }
  }
}

/// Ends the interval started by `_traceDistributedExecuteTarget`.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedExecuteTargetEnd(
  _ spanID: UInt64,
  error: (any Error)? = nil,
  failed: Bool = false
) {
  guard #available(anyAppleOS 9999, *) else { return }
  guard spanID != 0 else { return }
  unsafe _withDistributedTraceErrorType(error, failed: failed) { errorTypePtr in
    unsafe _traceExecuteDistributedTargetEnd(spanID, errorTypePtr)
  }
}

/// Begins an interval measuring the execution of the distributed target: from
/// invoking the user's function until control returns and the result handler is
/// about to be invoked.
///
/// This is the "how long did handling this call take" measurement; decoding the
/// invocation is measured separately by
/// `_traceDistributedDecodeArgumentsBegin`.
///
/// The returned span ID must be passed to
/// `_traceDistributedInvokeTargetEnd`. It is 0 when tracing is disabled, in
/// which case ending the interval is a no-op.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedInvokeTargetBegin(
  targetActor: some DistributedActor,
  targetIdentifier: String
) -> UInt64 {
  guard #available(anyAppleOS 9999, *) else { return 0 }
  guard _distributedTraceIsEnabled() else { return 0 }

  return targetIdentifier.withCString { identifierPtr in
    unsafe _traceInvokeTargetBegin(targetActor, identifierPtr)
  }
}

/// Ends the interval started by `_traceDistributedInvokeTargetBegin`.
@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedInvokeTargetEnd(
  _ spanID: UInt64,
  error: (any Error)? = nil,
  failed: Bool = false
) {
  guard #available(anyAppleOS 9999, *) else { return }
  guard spanID != 0 else { return }
  unsafe _withDistributedTraceErrorType(error, failed: failed) { errorTypePtr in
    unsafe _traceInvokeTargetEnd(spanID, errorTypePtr)
  }
}

@available(SwiftStdlib 5.7, *)
@_alwaysEmitIntoClient
public func _traceDistributedInvokeResultHandler(
  targetActor: some DistributedActor,
  targetIdentifier: String,
  error: (any Error)?
) {
  guard #available(anyAppleOS 9999, *) else { return }
  guard _distributedTraceIsEnabled() else { return }

  targetIdentifier.withCString { identifierPtr in
    unsafe _withDistributedTraceErrorType(error) { errorTypePtr in
      unsafe _traceInvokeResultHandler(targetActor, identifierPtr, errorTypePtr)
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Runtime entry points

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_is_enabled")
internal func _traceDistributedIsEnabled() -> Bool

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_remote_call_outbound_begin")
internal func _traceRemoteCallOutboundBegin(
  _ targetActor: AnyObject,
  _ targetActorID: UnsafePointer<CChar>?,
  _ targetIdentifier: UnsafePointer<CChar>?
) -> UInt64

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_remote_call_outbound_end")
internal func _traceRemoteCallOutboundEnd(
  _ spanID: UInt64,
  _ errorType: UnsafePointer<CChar>?
)

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_execute_target_begin")
internal func _traceExecuteDistributedTargetBegin(
  _ targetActor: AnyObject,
  _ targetActorID: UnsafePointer<CChar>?,
  _ targetIdentifier: UnsafePointer<CChar>?
) -> UInt64

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_execute_target_end")
internal func _traceExecuteDistributedTargetEnd(
  _ spanID: UInt64,
  _ errorType: UnsafePointer<CChar>?
)

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_encode_arguments_begin")
internal func _traceEncodeArgumentsBegin(
  _ targetActor: AnyObject,
  _ targetIdentifier: UnsafePointer<CChar>?,
  _ argumentCount: Int
) -> UInt64

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_encode_arguments_end")
internal func _traceEncodeArgumentsEnd(
  _ spanID: UInt64,
  _ errorType: UnsafePointer<CChar>?
)

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_decode_arguments_begin")
internal func _traceDecodeArgumentsBegin(
  _ targetActor: AnyObject,
  _ targetIdentifier: UnsafePointer<CChar>?
) -> UInt64

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_decode_arguments_end")
internal func _traceDecodeArgumentsEnd(
  _ spanID: UInt64,
  _ argumentCount: Int,
  _ errorType: UnsafePointer<CChar>?
)

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_invoke_target_begin")
internal func _traceInvokeTargetBegin(
  _ targetActor: AnyObject,
  _ targetIdentifier: UnsafePointer<CChar>?
) -> UInt64

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_invoke_target_end")
internal func _traceInvokeTargetEnd(
  _ spanID: UInt64,
  _ errorType: UnsafePointer<CChar>?
)

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_distributed_trace_invoke_result_handler")
internal func _traceInvokeResultHandler(
  _ targetActor: AnyObject,
  _ targetIdentifier: UnsafePointer<CChar>?,
  _ errorType: UnsafePointer<CChar>?
)






