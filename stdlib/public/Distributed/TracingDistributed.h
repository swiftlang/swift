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

/// Check if distributed tracing is enabled.
///
/// This can be used to avoid expensive operations (like string formatting)
/// when tracing is disabled.
bool distributed_trace_is_enabled();

/// ==== Outbound ------------------------------------------------------------------------------------------------------

/// Emitted when a call on a remote reference distributed actor is made and `remoteCall` will be invoked.
void distributed_remote_call_outbound(HeapObject *localTargetActor,
                                      const char *targetActorId,
                                      const char *targetIdentifier);
/// ==== Inbound -------------------------------------------------------------------------------------------------------

/// Emitted when `DistributedActorSystem/executeDistributedTarget` is invoked by a system implementation.
void distributed_execute_distributed_target(HeapObject *localTargetActor,
                                            const char *targetActorId,
                                            const char *targetIdentifier);

/// Emitted when `swift_findAccessibleFunction` has found (or not) a distributed function accessor.
void distributed_find_accessible_function(const char *targetName,
                                          size_t targetNameLength,
                                          const void *func);

/// Emitted when a result handler is invoked after execution of a local distributed call target completes.
///
/// This will always be after `distributed_execute_distributed_target`.
void distributed_invoke_result_handler(HeapObject *localActor,
                                       const char *targetActorId,
                                       const char *targetIdentifier,
                                       bool success);

} // namespace trace
} // namespace distributed
} // namespace swift

#if SWIFT_STDLIB_TRACING
#include "TracingDistributedSignpost.h"
#else
#include "TracingDistributedStubs.h"
#endif

#endif
