//===--- BacktraceUtils.cpp - Private backtracing utilities -----*- C++ -*-===//
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
//
// Defines some async-signal-safe formatting functions.
//
//===----------------------------------------------------------------------===//

#include "BacktracePrivate.h"

namespace swift {
namespace runtime {
namespace backtracing {

// We can't call sprintf() here because we're in a signal handler,
// so we need to be async-signal-safe.
SWIFT_RUNTIME_STDLIB_INTERNAL void
_swift_formatAddress(uintptr_t addr, char buffer[18])
{
  char *ptr = buffer + 18;
  *--ptr = '\0';
  while (ptr > buffer) {
    char digit = '0' + (addr & 0xf);
    if (digit > '9')
      digit += 'a' - '0' - 10;
    *--ptr = digit;
    addr >>= 4;
    if (!addr)
      break;
  }

  // Left-justify in the buffer
  if (ptr > buffer) {
    char *pt2 = buffer;
    while (*ptr)
      *pt2++ = *ptr++;
    *pt2++ = '\0';
  }
}

// See above; we can't use sprintf() here.
SWIFT_RUNTIME_STDLIB_INTERNAL void
_swift_formatUnsigned(unsigned u, char buffer[22])
{
  char *ptr = buffer + 22;
  *--ptr = '\0';
  while (ptr > buffer) {
    char digit = '0' + (u % 10);
    *--ptr = digit;
    u /= 10;
    if (!u)
      break;
  }

  // Left-justify in the buffer
  if (ptr > buffer) {
    char *pt2 = buffer;
    while (*ptr)
      *pt2++ = *ptr++;
    *pt2++ = '\0';
  }
}

} // namespace backtracing
} // namespace runtime
} // namespace swift
