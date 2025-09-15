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

#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include <os/signpost.h>

extern "C" const char *__progname;

#if SWIFT_USE_OS_TRACE_LAZY_INIT
extern "C" bool _os_trace_lazy_init_completed_4swift(void);
#endif

namespace swift {
namespace runtime {
namespace trace {

static inline bool shouldEnableTracing() {
  if (!SWIFT_RUNTIME_WEAK_CHECK(os_signpost_enabled))
    return false;
  return true;
}

#if SWIFT_USE_OS_TRACE_LAZY_INIT
#if __has_include(<sys/codesign.h>)
#include <sys/codesign.h>
#else
// SPI
#define CS_OPS_STATUS 0
#define CS_PLATFORM_BINARY 0x04000000
extern "C" int csops(pid_t, unsigned int, void *, size_t);
#endif

#include <unistd.h>

static inline bool isPlatformBinary() {
  unsigned int flags = 0;
  int error = csops(getpid(), CS_OPS_STATUS, &flags, sizeof(flags));
  if (error)
    return true; // Fail safe if the call fails, assume it's a platform binary.
  return (flags & CS_PLATFORM_BINARY) != 0;
}

static inline bool tracingReady() {
  // For non-platform binaries, consider tracing to always be ready. We can
  // safely initiate setup if it isn't.
  bool platformBinary = SWIFT_LAZY_CONSTANT(isPlatformBinary());
  if (!platformBinary)
    return true;

  // For platform binaries, we may be on the path that sets up tracing, and
  // making tracing calls may deadlock in that case. Wait until something else
  // set up tracing before using it.
  if (!_os_trace_lazy_init_completed_4swift())
    return false;

  return true;
}

#else

static inline bool tracingReady() { return true; }

#endif

} // namespace trace
} // namespace runtime
} // namespace swift

#endif

#endif // SWIFT_TRACING_H
