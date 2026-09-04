//===--- Tracing.h - Support code for distributed tracing ----------*- C++ -*-//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Support code for tracing events in the distributed runtime
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DISTRIBUTED_TRACING_H
#define SWIFT_DISTRIBUTED_TRACING_H

#include <stddef.h>
#include <stdint.h>

namespace swift {
class AsyncTask;
struct HeapObject;

namespace distributed {
namespace trace {

/// Check if tracing of the Distributed module is enabled.
///
/// This can be used to avoid expensive operations (like string formatting)
/// when tracing is disabled.
bool distributed_trace_is_enabled();

/// ==== Outbound ------------------------------------------------------------------------------------------------------

/// Begins an interval covering the whole outgoing call: from just before
/// `remoteCall` is invoked until it returns (or throws), spanning encoding, the
/// transport round trip and decoding the reply. The nested phases (such as
/// `distributed_encode_arguments`) draw inside it.
///
/// Returns an opaque trace ID which must be passed to
/// `distributed_remote_call_outbound_end`, or 0 if tracing is not enabled.
uint64_t distributed_remote_call_outbound_begin(HeapObject *localTargetActor,
                                                const char *targetActorId,
                                                const char *targetIdentifier);

/// Ends the interval started by `distributed_remote_call_outbound_begin`.
///
/// A `spanId` of 0 is ignored.
void distributed_remote_call_outbound_end(uint64_t spanId, const char *errorType);

/// Begins an interval covering the encoding of an outgoing invocation:
/// the generic substitutions, arguments, error and return types, up to and
/// including `doneRecording`.
///
/// Returns an opaque trace ID which must be passed to
/// `distributed_encode_arguments_end`, or 0 if tracing is not enabled.
///
/// Only the actor pointer is recorded, not its type or ID; the
/// `distributed_remote_call_outbound` interval opened for the same call carries
/// those, and computing them is comparatively expensive.
uint64_t distributed_encode_arguments_begin(HeapObject *localTargetActor,
                                            const char *targetIdentifier,
                                            size_t argumentCount);

/// Ends the interval started by `distributed_encode_arguments_begin`.
///
/// A `spanId` of 0 is ignored.
void distributed_encode_arguments_end(uint64_t spanId, const char *errorType);

/// ==== Inbound -------------------------------------------------------------------------------------------------------

/// Begins an interval covering the whole inbound execution of a call, from
/// `DistributedActorSystem/executeDistributedTarget` being invoked until it
/// returns (or throws), spanning decoding, invoking the target and the result
/// handler. The nested phases (decode, invoke) draw inside it.
///
/// Returns an opaque trace ID which must be passed to
/// `distributed_execute_distributed_target_end`, or 0 if tracing is not enabled.
uint64_t distributed_execute_distributed_target_begin(HeapObject *localTargetActor,
                                                      const char *targetActorId,
                                                      const char *targetIdentifier);

/// Ends the interval started by `distributed_execute_distributed_target_begin`.
///
/// A `spanId` of 0 is ignored.
void distributed_execute_distributed_target_end(uint64_t spanId, const char *errorType);

/// Begins an interval covering the decoding of an incoming invocation:
/// the generic substitutions, witness tables, parameter types and return type.
///
/// Note that the decoding of the individual argument *values* happens inside
/// the distributed accessor (via `decodeNextArgument`) and is therefore not
/// part of this interval.
///
/// Returns an opaque trace ID which must be passed to
/// `distributed_decode_arguments_end`, or 0 if tracing is not enabled.
uint64_t distributed_decode_arguments_begin(HeapObject *localTargetActor,
                                            const char *targetIdentifier);

/// Ends the interval started by `distributed_decode_arguments_begin`.
///
/// A `spanId` of 0 is ignored.
void distributed_decode_arguments_end(uint64_t spanId, size_t argumentCount,
                                      const char *errorType);

/// Begins an interval covering the execution of the distributed target itself:
/// from invoking the user's function until control returns and the result
/// handler is about to be invoked. This is the "how long did handling this call
/// take" measurement, excluding decoding the invocation.
///
/// Returns an opaque trace ID which must be passed to
/// `distributed_invoke_target_end`, or 0 if tracing is not enabled.
uint64_t distributed_invoke_target_begin(HeapObject *localTargetActor,
                                         const char *targetIdentifier);

/// Ends the interval started by `distributed_invoke_target_begin`.
///
/// A `spanId` of 0 is ignored.
void distributed_invoke_target_end(uint64_t spanId, const char *errorType);

/// Emitted when `swift_findAccessibleFunction` has found (or not) a distributed function accessor.
void distributed_find_accessible_function(const char *targetName,
                                          size_t targetNameLength,
                                          const void *accessibleFunctionRecord,
                                          const char *funcName,
                                          const void *genericEnv,
                                          const void *funcPtr);

/// Emitted when a result handler is invoked after execution of a local distributed call target completes.
///
/// This will always be after `distributed_execute_distributed_target`.
void distributed_invoke_result_handler(HeapObject *localActor,
                                       const char *targetIdentifier,
                                       const char *errorType);

} // namespace trace
} // namespace distributed
} // namespace swift

#if SWIFT_STDLIB_TRACING
#include "TracingDistributedSignpost.h"
#else
#include "TracingDistributedStubs.h"
#endif

#endif
