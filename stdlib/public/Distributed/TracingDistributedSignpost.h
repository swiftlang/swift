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

#define SWIFT_LOG_DISTRIBUTED_FIND_ACCESSIBLE_FUNCTION_NAME "distributed_find_accessible_function"
#define SWIFT_LOG_DISTRIBUTED_REMOTE_CALL_OUTBOUND_NAME "distributed_remote_call_outbound"
#define SWIFT_LOG_DISTRIBUTED_EXECUTE_TARGET_NAME "distributed_execute_distributed_target"
#define SWIFT_LOG_DISTRIBUTED_RESULT_HANDLER_NAME "distributed_invoke_result_handler"

namespace swift {
namespace distributed {
namespace trace {

extern os_log_t DistributedLog;
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

// ==== Tracing Control ---------------------------------------------------------------------------------------------------

inline bool distributed_trace_is_enabled() {
  if (!runtime::trace::tracingReady())
    return false;
  swift::once(LogsToken, setupLogs, nullptr);
  return TracingEnabled && os_signpost_enabled(DistributedLog);
}

// ==== Outbound -------------------------------------------------------------------------------------------------------

inline void distributed_remote_call_outbound(HeapObject *localTargetActor,
                                             const char *targetActorId,
                                             const char *targetIdentifier) {
ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedLog))
    return;

  auto typeName = swift_getMangledTypeName(swift_getObjectType(localTargetActor));

  auto id = os_signpost_id_generate(DistributedLog);
  os_signpost_event_emit(
      DistributedLog, id, SWIFT_LOG_DISTRIBUTED_REMOTE_CALL_OUTBOUND_NAME,
      "outbound"
      " actor=%p"
      " actorType:%s"
      " targetActorId=%{public}s"
      " targetFunction=%{public}s",
      localTargetActor,
      typeName.data,
      targetActorId ? targetActorId : "<unknown>",
      targetIdentifier ? targetIdentifier : "<unknown>");
}

// ==== Inbound -------------------------------------------------------------------------------------------------------

inline void distributed_execute_distributed_target(HeapObject *localTargetActor,
                                                   const char *targetActorId,
                                                   const char *targetIdentifier) {
  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedLog))
    return;

  auto typeName = swift_getMangledTypeName(swift_getObjectType(localTargetActor));

  auto id = os_signpost_id_generate(DistributedLog);
  os_signpost_event_emit(
      DistributedLog, id, SWIFT_LOG_DISTRIBUTED_EXECUTE_TARGET_NAME,
      "inbound"
      " actor=%p"
      " actorType:%s"
      " targetActorId=%{public}s"
      " targetFunction=%{public}s",
      localTargetActor,
      typeName.data,
      targetActorId ? targetActorId : "<unknown>",
      targetIdentifier ? targetIdentifier : "<unknown>");
}

inline void distributed_find_accessible_function(const char *targetName,
                                                 size_t targetNameLength,
                                                 const void *func) {
  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedLog))
    return;

  auto id = os_signpost_id_generate(DistributedLog);
  os_signpost_event_emit(
      DistributedLog, id, SWIFT_LOG_DISTRIBUTED_FIND_ACCESSIBLE_FUNCTION_NAME,
      "inbound"
      " targetFunction=%{public}.*s"
      " found=%{bool}d"
      " funcPtr=%p",
      (int)targetNameLength,
      targetName ? targetName : "<unknown>",
      func != nullptr,
      func);
}

inline void distributed_invoke_result_handler(HeapObject *localTargetActor,
                                              const char *targetActorId,
                                              const char *targetIdentifier,
                                              bool success) {
  ENSURE_LOGS();

  if (!os_signpost_enabled(DistributedLog))
    return;

  auto id = os_signpost_id_generate(DistributedLog);
  os_signpost_event_emit(
      DistributedLog, id, SWIFT_LOG_DISTRIBUTED_RESULT_HANDLER_NAME,
      "inbound"
      " actor=%p"
      " targetActorId=%{public}s"
      " targetFunction=%{public}s"
      " success=%{bool}d",
      localTargetActor,
      targetActorId ? targetActorId : "<unknown>",
      targetIdentifier ? targetIdentifier : "<unknown>",
      success);
}

#pragma clang diagnostic pop

} // namespace trace
} // namespace distributed
} // namespace swift

#endif
