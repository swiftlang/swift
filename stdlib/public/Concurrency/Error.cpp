//===--- Error.cpp - Error handling support code --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Threading/Errors.h"
#include <atomic>
#include <cstdio>
#include <cstring>

#include "Error.h"

// Redeclared from swift/Runtime/Debug.h.
namespace swift {
SWIFT_WEAK_IMPORT SWIFT_RUNTIME_EXPORT
std::atomic<void (*)(uint32_t, const char *, void *)> _swift_willAbort;
}

// swift::fatalError is not exported from libswiftCore and not shared, so define another
// internal function instead.
SWIFT_NORETURN
SWIFT_VFORMAT(2)
void swift::swift_Concurrency_fatalErrorv(uint32_t flags, const char *format,
                                          va_list val) {
  // TODO: copy swift_vasprintf() so we can capture the formatted log message
  const char *log = format;

#if !SWIFT_CONCURRENCY_EMBEDDED
  vfprintf(stderr, format, val);
#else
  vprintf(format, val);
#endif

  if (&_swift_willAbort != nullptr) {
    auto handler = _swift_willAbort.exchange(nullptr, std::memory_order_acq_rel);
    if (SWIFT_UNLIKELY(handler)) {
      (* handler)(flags, log, nullptr);
    }
  }

  abort();
}

SWIFT_NORETURN
SWIFT_FORMAT(2, 3)
void swift::swift_Concurrency_fatalError(uint32_t flags, const char *format,
                                         ...) {
  va_list val;

  va_start(val, format);
  swift_Concurrency_fatalErrorv(flags, format, val);
}
