//===--- TracingCommon.h - Common code for runtime/Concurrency -----*- C++ -*-//
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
// Support code shared between swiftCore and swift_Concurrency.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TRACING_COMMON_H
#define SWIFT_TRACING_COMMON_H

#if SWIFT_STDLIB_TRACING

#include "swift/Runtime/Config.h"
#include <os/signpost.h>

extern const char *__progname;

namespace swift {
namespace runtime {
namespace trace {

static inline bool shouldEnableTracing() {
  if (!SWIFT_RUNTIME_WEAK_CHECK(os_signpost_enabled))
    return false;
  if (__progname && (strcmp(__progname, "logd") == 0 ||
                     strcmp(__progname, "diagnosticd") == 0 ||
                     strcmp(__progname, "notifyd") == 0 ||
                     strcmp(__progname, "xpcproxy") == 0))
    return false;
  return true;
}

} // namespace trace
} // namespace runtime
} // namespace swift

#endif

#endif // SWIFT_TRACING_H
