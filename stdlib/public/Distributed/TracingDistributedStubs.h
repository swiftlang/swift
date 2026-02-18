//===--- TracingStubs.h - Default stub implementation of tracing. --*- C++ -*-//
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
// Distributed tracing stubs for OSes without tracing support.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DISTRIBUTED_TRACINGSTUBS_H
#define SWIFT_DISTRIBUTED_TRACINGSTUBS_H

#include "TracingDistributed.h"

namespace swift {
namespace distributed {
namespace trace {

inline bool distributed_trace_is_enabled() { return false; }

inline uint64_t distributed_remote_call_outbound_begin(HeapObject *localTargetActor,
                                                       const char *targetActorID,
                                                       const char *targetIdentifier) {
  return 0;
}

inline void distributed_remote_call_outbound_end(uint64_t spanId,
                                                 const char *errorType) {
}

inline uint64_t distributed_encode_arguments_begin(HeapObject *localTargetActor,
                                                   const char *targetIdentifier,
                                                   size_t argumentCount) {
  return 0;
}

inline void distributed_encode_arguments_end(uint64_t spanId, const char *errorType) {
}

inline uint64_t distributed_decode_arguments_begin(HeapObject *localTargetActor,
                                                   const char *targetIdentifier) {
  return 0;
}

inline void distributed_decode_arguments_end(uint64_t spanId,
                                             size_t argumentCount,
                                             const char *errorType) {
}

inline uint64_t distributed_invoke_target_begin(HeapObject *localTargetActor,
                                                const char *targetIdentifier) {
  return 0;
}

inline void distributed_invoke_target_end(uint64_t spanId, const char *errorType) {
}

inline uint64_t distributed_execute_distributed_target_begin(HeapObject *localTargetActor,
                                                             const char *targetActorID,
                                                             const char *targetIdentifier) {
  return 0;
}

inline void distributed_execute_distributed_target_end(uint64_t spanId,
                                                       const char *errorType) {
}

inline void distributed_find_accessible_function(const char *targetName,
                                                 size_t targetNameLength,
                                                 const void *accessibleFunctionRecord,
                                                 const char *funcName,
                                                 const void *genericEnv,
                                                 const void *funcPtr) {
}

inline void distributed_invoke_result_handler(HeapObject *localTargetActor,
                                              const char *targetIdentifier,
                                              const char *errorType) {
}

} // namespace trace
} // namespace distributed
} // namespace swift

#endif
