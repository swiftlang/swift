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
#include <cstdio>

#include "Error.h"

// swift::fatalError is not exported from libswiftCore and not shared, so define another
// internal function instead.
SWIFT_NORETURN
SWIFT_VFORMAT(2)
void swift::swift_Concurrency_fatalErrorv(uint32_t flags, const char *format,
                                          va_list val) {
#if !SWIFT_CONCURRENCY_EMBEDDED
  vfprintf(stderr, format, val);
#else
  vprintf(format, val);
#endif
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
