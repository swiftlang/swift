//===--- TracingSignpost.cpp - Tracing with the signpost API -------*- C++ -*-//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Concurrency tracing implemented with the os_signpost API.
//
//===----------------------------------------------------------------------===//

#if SWIFT_STDLIB_CONCURRENCY_TRACING

#include "TracingSignpost.h"
#include <stdio.h>

// Temporary until we work out what categories we should really use.
#ifndef OS_LOG_CATEGORY_DYNAMIC_STACK_TRACING
#define OS_LOG_CATEGORY_DYNAMIC_STACK_TRACING "DynamicStackTracing"
#endif

#define SWIFT_LOG_CONCURRENCY_ACTOR_SUBSYSTEM "com.apple.swift.actor"
#define SWIFT_LOG_CONCURRENCY_TASK_SUBSYSTEM "com.apple.swift.task"
#define SWIFT_LOG_ACTOR_CATEGORY OS_LOG_CATEGORY_DYNAMIC_STACK_TRACING
#define SWIFT_LOG_TASK_CATEGORY OS_LOG_CATEGORY_DYNAMIC_STACK_TRACING

namespace swift {
namespace concurrency {
namespace trace {

os_log_t ActorLog;
os_log_t TaskLog;
swift::once_t LogsToken;

void setupLogs(void *unused) {
  ActorLog = os_log_create(SWIFT_LOG_CONCURRENCY_ACTOR_SUBSYSTEM,
                           SWIFT_LOG_ACTOR_CATEGORY);
  TaskLog = os_log_create(SWIFT_LOG_CONCURRENCY_TASK_SUBSYSTEM,
                          SWIFT_LOG_TASK_CATEGORY);
}

} // namespace trace
} // namespace concurrency
} // namespace swift

#endif
