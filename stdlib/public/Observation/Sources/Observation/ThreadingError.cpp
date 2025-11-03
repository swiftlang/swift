//===--- ThreadingError.cpp - Error handling support code -----------------===//
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

#include "swift/Threading/Errors.h"
#include <cstdio>
#include <cstdlib>

// Handle fatal errors from the threading library
SWIFT_ATTRIBUTE_NORETURN
SWIFT_FORMAT(1, 2)
void swift::threading::fatal(const char *format, ...) {
  va_list val;

  va_start(val, format);
  vfprintf(stderr, format, val);
  abort();
}
