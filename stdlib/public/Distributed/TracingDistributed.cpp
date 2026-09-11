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
uint64_t swift_distributed_trace_remote_call_outbound_begin(
    HeapObject *targetActor,
    const char *targetActorID,
    const char *targetIdentifier) {
  return distributed::trace::distributed_remote_call_outbound_begin(
      targetActor, targetActorID, targetIdentifier);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_remote_call_outbound_end(uint64_t spanID,
                                                      const char *errorType) {
  distributed::trace::distributed_remote_call_outbound_end(spanID, errorType);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
uint64_t swift_distributed_trace_encode_arguments_begin(
    HeapObject *targetActor,
    const char *targetIdentifier,
    size_t argumentCount) {
  return distributed::trace::distributed_encode_arguments_begin(
      targetActor, targetIdentifier, argumentCount);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_encode_arguments_end(uint64_t spanID,
                                                  const char *errorType) {
  distributed::trace::distributed_encode_arguments_end(spanID, errorType);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
uint64_t swift_distributed_trace_decode_arguments_begin(
    HeapObject *targetActor,
    const char *targetIdentifier) {
  return distributed::trace::distributed_decode_arguments_begin(
      targetActor, targetIdentifier);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_decode_arguments_end(uint64_t spanID,
                                                  size_t argumentCount,
                                                  const char *errorType) {
  distributed::trace::distributed_decode_arguments_end(spanID, argumentCount,
                                                       errorType);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
uint64_t swift_distributed_trace_execute_target_begin(
    HeapObject *targetActor,
    const char *targetActorID,
    const char *targetIdentifier) {
  return distributed::trace::distributed_execute_distributed_target_begin(
      targetActor, targetActorID, targetIdentifier);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_execute_target_end(uint64_t spanID,
                                                const char *errorType) {
  distributed::trace::distributed_execute_distributed_target_end(spanID,
                                                                 errorType);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
uint64_t swift_distributed_trace_invoke_target_begin(
    HeapObject *targetActor,
    const char *targetIdentifier) {
  return distributed::trace::distributed_invoke_target_begin(
      targetActor, targetIdentifier);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_invoke_target_end(uint64_t spanID,
                                               const char *errorType) {
  distributed::trace::distributed_invoke_target_end(spanID, errorType);
}

SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void swift_distributed_trace_invoke_result_handler(
    HeapObject *targetActor,
    const char *targetIdentifier,
    const char *errorType) {
  distributed::trace::distributed_invoke_result_handler(
      targetActor, targetIdentifier, errorType);
}
