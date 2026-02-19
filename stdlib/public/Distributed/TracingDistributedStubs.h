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

inline void distributed_remote_call_outbound(HeapObject *localTargetActor,
                                             const char *targetActorID,
                                             const char *targetIdentifier) {
}

inline void distributed_execute_distributed_target(HeapObject *localTargetActor,
                                                   const char *targetActorID,
                                                   const char *targetIdentifier) {
}

inline void distributed_find_accessible_function(const char *targetName,
                                                 size_t targetNameLength,
                                                 const void *func) {
}

inline void distributed_invoke_result_handler(HeapObject *localTargetActor,
                                                    const char *targetActorID,
                                                    const char *targetIdentifier,
                                                    bool success) {
}

} // namespace trace
} // namespace distributed
} // namespace swift

#endif
