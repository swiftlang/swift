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

#define SWIFT_LOG_CONCURRENCY_SUBSYSTEM "com.apple.swift.concurrency"
#define SWIFT_LOG_ACTOR_CATEGORY "Actor"
#define SWIFT_LOG_TASK_CATEGORY "Task"

namespace swift {
namespace concurrency {
namespace trace {

os_log_t ActorLog;
os_log_t TaskLog;
swift::once_t LogsToken;
bool TracingEnabled;

void setupLogs(void *unused) {
  if (!swift::runtime::trace::shouldEnableTracing()) {
    TracingEnabled = false;
    return;
  }

  TracingEnabled = true;
  ActorLog = os_log_create(SWIFT_LOG_CONCURRENCY_SUBSYSTEM,
                           SWIFT_LOG_ACTOR_CATEGORY);
  TaskLog = os_log_create(SWIFT_LOG_CONCURRENCY_SUBSYSTEM,
                          SWIFT_LOG_TASK_CATEGORY);
}

} // namespace trace
} // namespace concurrency
} // namespace swift

#endif
