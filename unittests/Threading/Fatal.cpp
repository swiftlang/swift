//===---Fatal.cpp - Stub for the threading tests --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Threading/Errors.h"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>

SWIFT_ATTRIBUTE_NORETURN
SWIFT_FORMAT(1, 2)
void swift::threading::fatal(const char *msg, ...) {
  va_list val;

  va_start(val, msg);
  std::vfprintf(stderr, msg, val);
  va_end(val);

  std::abort();
}
