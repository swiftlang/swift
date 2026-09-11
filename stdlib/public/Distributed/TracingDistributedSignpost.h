//===--- TracingSignpost.h - Tracing with the signpost API ---------*- C++ -*-//
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
// Distributed tracing implemented with the os_signpost API.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DISTRIBUTED_TRACINGSIGNPOST_H
#define SWIFT_DISTRIBUTED_TRACINGSIGNPOST_H

#include "TracingDistributed.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/TracingCommon.h"
#include <inttypes.h>
#include <os/log.h>
#include <os/signpost.h>

// Compatibility notes:
//
// These signposts can be read by external software that isn't synced with the
// Swift runtime build. Changes here must be considered carefully to avoid
// breaking users of these signposts.
//
// We may:
// * Add new signpost calls with new names. (Keeping in mind that they won't be
//   picked up by software that doesn't know about them.)
// * Remove existing calls if the given event is somehow no longer relevant.
// * Change format strings.
// * Add format string arguments.
//
// We may NOT:
// * Change the order of existing format string arguments.
// * Change event names.
// * Change subsystem names.

#define SWIFT_LOG_DISTRIBUTED_OUTBOUND_REMOTE_CALL_NAME "distributed_outbound_remote_call"
#define SWIFT_LOG_DISTRIBUTED_OUTBOUND_ENCODE_ARGUMENTS_NAME "distributed_outbound_encode_arguments"

#define SWIFT_LOG_DISTRIBUTED_INBOUND_EXECUTE_TARGET_NAME "distributed_inbound_execute_target"
#define SWIFT_LOG_DISTRIBUTED_INBOUND_DECODE_ARGUMENTS_NAME "distributed_inbound_decode_arguments"
#define SWIFT_LOG_DISTRIBUTED_INBOUND_INVOKE_TARGET_NAME "distributed_inbound_invoke_target"
#define SWIFT_LOG_DISTRIBUTED_INBOUND_FIND_ACCESSIBLE_FUNCTION_NAME "distributed_inbound_find_accessible_function"
#define SWIFT_LOG_DISTRIBUTED_INBOUND_INVOKE_RESULT_HANDLER_NAME "distributed_inbound_invoke_result_handler"

namespace swift {
namespace distributed {
namespace trace {

extern os_log_t DistributedRemoteCallsLog;
extern swift::once_t LogsToken;
extern bool TracingEnabled;

void setupLogs(void *unused);

// Check a representative os_signpost function for NULL rather than doing a
// standard availability check, for better performance if the check doesn't get
// optimized out.
#define ENSURE_LOGS(...)                                                       \
  do {                                                                         \
    if (!runtime::trace::tracingReady())                                       \
      return __VA_ARGS__;                                                      \
    swift::once(LogsToken, setupLogs, nullptr);                                \
    if (!TracingEnabled)                                                       \
      return __VA_ARGS__;                                                      \
  } while (0)

// Every function does ENSURE_LOGS() before making any os_signpost calls, so
// we can skip availability checking on all the individual calls.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunguarded-availability"
#pragma clang diagnostic ignored "-Wunguarded-availability-new"

// ==== ------------------------------------------------------------------------
// MARK: Tracing control

inline bool distributed_trace_is_enabled() {
  if (!runtime::trace::tracingReady())
    return false;
  swift::once(LogsToken, setupLogs, nullptr);
  return TracingEnabled && os_signpost_enabled(DistributedRemoteCallsLog);
}

// ==== ------------------------------------------------------------------------
// MARK: Outbound

inline uint64_t distributed_remote_call_outbound_begin(HeapObject *localTargetActor,
                                                       const char *targetActorId,
                                                       const char *targetIdentifier) {
  ENSURE_LOGS(0);

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return 0;

  auto typeName = swift_getTypeName(swift_getObjectType(localTargetActor),
                                    /*qualified=*/true);

  auto id = os_signpost_id_generate(DistributedRemoteCallsLog);
  os_signpost_interval_begin(
      DistributedRemoteCallsLog, id,
      SWIFT_LOG_DISTRIBUTED_OUTBOUND_REMOTE_CALL_NAME,
      "actor=%p"
      " actorType=%{public}.*s"
      " targetActorId=%{public}s"
      " targetFunction=%{public}s",
      localTargetActor,
      (int)typeName.length, typeName.data,
      targetActorId ? targetActorId : "<unknown>",
      targetIdentifier ? targetIdentifier : "<unknown>");
  return id;
}

inline void distributed_remote_call_outbound_end(uint64_t spanId,
                                                 const char *errorType) {
  if (!spanId)
    return;

  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return;

  os_signpost_interval_end(
      DistributedRemoteCallsLog, spanId,
      SWIFT_LOG_DISTRIBUTED_OUTBOUND_REMOTE_CALL_NAME,
      "success=%{bool}d"
      " errorType=%{public}s",
      errorType == nullptr,
      errorType ? errorType : "");
}

inline uint64_t distributed_encode_arguments_begin(HeapObject *localTargetActor,
                                                   const char *targetIdentifier,
                                                   size_t argumentCount) {
  ENSURE_LOGS(0);

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return 0;

  auto id = os_signpost_id_generate(DistributedRemoteCallsLog);
  os_signpost_interval_begin(
      DistributedRemoteCallsLog, id,
      SWIFT_LOG_DISTRIBUTED_OUTBOUND_ENCODE_ARGUMENTS_NAME,
      "actor=%p"
      " targetFunction=%{public}s"
      " argumentCount=%ld",
      localTargetActor,
      targetIdentifier ? targetIdentifier : "<unknown>",
      (long)argumentCount);
  return id;
}

inline void distributed_encode_arguments_end(uint64_t spanId,
                                             const char *errorType) {
  if (!spanId)
    return;

  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return;

  os_signpost_interval_end(
      DistributedRemoteCallsLog, spanId,
      SWIFT_LOG_DISTRIBUTED_OUTBOUND_ENCODE_ARGUMENTS_NAME,
      "success=%{bool}d"
      " errorType=%{public}s",
      errorType == nullptr,
      errorType ? errorType : "");
}

// ==== ------------------------------------------------------------------------
// MARK: Inbound

inline uint64_t distributed_execute_distributed_target_begin(HeapObject *localTargetActor,
                                                             const char *targetActorId,
                                                             const char *targetIdentifier) {
  ENSURE_LOGS(0);

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return 0;

  auto typeName = swift_getTypeName(swift_getObjectType(localTargetActor),
                                    /*qualified=*/true);

  auto id = os_signpost_id_generate(DistributedRemoteCallsLog);
  os_signpost_interval_begin(
      DistributedRemoteCallsLog, id,
      SWIFT_LOG_DISTRIBUTED_INBOUND_EXECUTE_TARGET_NAME,
      "actor=%p"
      " actorType=%{public}.*s"
      " targetActorId=%{public}s"
      " targetFunction=%{public}s",
      localTargetActor,
      (int)typeName.length, typeName.data,
      targetActorId ? targetActorId : "<unknown>",
      targetIdentifier ? targetIdentifier : "<unknown>");
  return id;
}

inline void distributed_execute_distributed_target_end(uint64_t spanId,
                                                       const char *errorType) {
  if (!spanId)
    return;

  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return;

  os_signpost_interval_end(
      DistributedRemoteCallsLog, spanId,
      SWIFT_LOG_DISTRIBUTED_INBOUND_EXECUTE_TARGET_NAME,
      "success=%{bool}d"
      " errorType=%{public}s",
      errorType == nullptr,
      errorType ? errorType : "");
}

inline uint64_t distributed_decode_arguments_begin(HeapObject *localTargetActor,
                                                   const char *targetIdentifier) {
  ENSURE_LOGS(0);

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return 0;

  auto id = os_signpost_id_generate(DistributedRemoteCallsLog);
  os_signpost_interval_begin(
      DistributedRemoteCallsLog, id,
      SWIFT_LOG_DISTRIBUTED_INBOUND_DECODE_ARGUMENTS_NAME,
      "actor=%p"
      " targetFunction=%{public}s",
      localTargetActor,
      targetIdentifier ? targetIdentifier : "<unknown>");
  return id;
}

inline void distributed_decode_arguments_end(uint64_t spanId,
                                             size_t argumentCount,
                                             const char *errorType) {
  if (!spanId)
    return;

  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return;

  os_signpost_interval_end(
      DistributedRemoteCallsLog, spanId,
      SWIFT_LOG_DISTRIBUTED_INBOUND_DECODE_ARGUMENTS_NAME,
      "argumentCount=%ld"
      " success=%{bool}d"
      " errorType=%{public}s",
      (long)argumentCount,
      errorType == nullptr,
      errorType ? errorType : "");
}

inline uint64_t distributed_invoke_target_begin(HeapObject *localTargetActor,
                                                const char *targetIdentifier) {
  ENSURE_LOGS(0);

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return 0;

  auto id = os_signpost_id_generate(DistributedRemoteCallsLog);
  os_signpost_interval_begin(
      DistributedRemoteCallsLog, id,
      SWIFT_LOG_DISTRIBUTED_INBOUND_INVOKE_TARGET_NAME,
      "actor=%p"
      " targetFunction=%{public}s",
      localTargetActor,
      targetIdentifier ? targetIdentifier : "<unknown>");
  return id;
}

inline void distributed_invoke_target_end(uint64_t spanId,
                                          const char *errorType) {
  if (!spanId)
    return;

  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return;

  os_signpost_interval_end(
      DistributedRemoteCallsLog, spanId,
      SWIFT_LOG_DISTRIBUTED_INBOUND_INVOKE_TARGET_NAME,
      "success=%{bool}d"
      " errorType=%{public}s",
      errorType == nullptr,
      errorType ? errorType : "");
}

inline void distributed_find_accessible_function(const char *targetName,
                                                 size_t targetNameLength,
                                                 const void *accessibleFunctionRecord,
                                                 const char *funcName,
                                                 const void *genericEnv,
                                                 const void *funcPtr) {
  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return;

  auto id = os_signpost_id_generate(DistributedRemoteCallsLog);
  os_signpost_event_emit(
      DistributedRemoteCallsLog, id,
      SWIFT_LOG_DISTRIBUTED_INBOUND_FIND_ACCESSIBLE_FUNCTION_NAME,
      "targetName=%{public}.*s"
      " accessibleFunctionRecord=%p"
      " funcName=%{public}s"
      " genericEnv=%p"
      " funcPtr=%p",
      (int)targetNameLength, targetName ? targetName : "<no-name>",
      accessibleFunctionRecord,
      funcName ? funcName : "<not found>",
      genericEnv,
      funcPtr);
}

inline void distributed_invoke_result_handler(HeapObject *localTargetActor,
                                              const char *targetIdentifier,
                                              const char *errorType) {
  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedRemoteCallsLog))
    return;

  auto id = os_signpost_id_generate(DistributedRemoteCallsLog);
  os_signpost_event_emit(
      DistributedRemoteCallsLog, id,
      SWIFT_LOG_DISTRIBUTED_INBOUND_INVOKE_RESULT_HANDLER_NAME,
      "actor=%p"
      " targetFunction=%{public}s"
      " success=%{bool}d"
      " errorType=%{public}s",
      localTargetActor,
      targetIdentifier ? targetIdentifier : "<unknown>",
      errorType == nullptr,
      errorType ? errorType : "");
}

#pragma clang diagnostic pop

} // namespace trace
} // namespace distributed
} // namespace swift

#endif
