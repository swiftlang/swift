///===--- TracingDistributed.cpp - Distributed tracing runtime ------------===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https://swift.org/LICENSE.txt for license information
/// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===----------------------------------------------------------------------===///
///
/// Export some tracing functions which need to be called from Swift.
///
///===----------------------------------------------------------------------===///

#include "swift/ABI/HeapObject.h"
#include "TracingDistributed.h"

using namespace swift;

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
bool swift_distributed_trace_is_enabled() {
  return distributed::trace::distributed_trace_is_enabled();
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_remote_call_outbound(
    HeapObject *targetActor,
    const char *targetActorID,
    const char *targetIdentifier) {
  distributed::trace::distributed_remote_call_outbound(
      targetActor, targetActorID, targetIdentifier);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_execute_target(
    HeapObject *targetActor,
    const char *targetActorID,
    const char *targetIdentifier) {
  distributed::trace::distributed_execute_distributed_target(
      targetActor, targetActorID, targetIdentifier);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_invoke_result_handler(
    HeapObject *targetActor,
    const char *targetActorID,
    const char *targetIdentifier,
    bool success) {
  distributed::trace::distributed_invoke_result_handler(
      targetActor, targetActorID, targetIdentifier, success);
}
