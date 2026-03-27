//===--- TracingSignpost.cpp - Tracing with the signpost API -------*- C++ -*-//
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
// Tracing for Distributed module implemented with the os_signpost API.
//
//===----------------------------------------------------------------------===//

#if SWIFT_STDLIB_TRACING

#include "TracingDistributedSignpost.h"
#include <stdio.h>

#define SWIFT_LOG_DISTRIBUTED_SUBSYSTEM "com.apple.swift.distributed"
#define SWIFT_LOG_DISTRIBUTED_CATEGORY "Distributed"

namespace swift {
namespace distributed {
namespace trace {

os_log_t DistributedLog;
swift::once_t LogsToken;
bool TracingEnabled;

void setupLogs(void *unused) {
  if (!swift::runtime::trace::shouldEnableTracing()) {
    TracingEnabled = false;
    return;
  }

  TracingEnabled = true;
  DistributedLog = os_log_create(SWIFT_LOG_DISTRIBUTED_SUBSYSTEM, SWIFT_LOG_DISTRIBUTED_CATEGORY);
}

} // namespace trace
} // namespace distributed
} // namespace swift

#endif
