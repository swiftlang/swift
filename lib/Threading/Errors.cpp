//==--- Errors.cpp - Threading implementation error handling --- -*-C++ -*-===//
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
//
// Provides a fallback definition of swift::threading::fatal().  You may
// care to provide your own definition elsewhere, to tie the threading code's
// error handling into the relevant code.
//
//===----------------------------------------------------------------------===//

#include <cstdarg>
#include <cstdio>
#include <cstdlib>

#include "swift/Threading/Errors.h"

namespace swift {
namespace threading {

SWIFT_ATTRIBUTE_NORETURN
SWIFT_FORMAT(1, 2)
void fatal(const char *msg, ...) {
  std::va_list val;

  va_start(val, msg);
  std::vfprintf(stderr, msg, val);
  va_end(val);

  std::abort();
}

} // namespace threading
} // namespace swift
