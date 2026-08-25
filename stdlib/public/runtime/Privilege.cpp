//===--- Privilege.cpp - Process privilege checks ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checks for whether the current process holds privileges or protections that
// are relevant to runtime behavior.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Privilege.h"

#ifdef __APPLE__
#include <TargetConditionals.h>
#endif

#ifdef __linux__
#include <sys/auxv.h>
#endif

#if !defined(_WIN32)
#include <unistd.h>
#endif

#if TARGET_OS_OSX || TARGET_OS_MACCATALYST
#if __has_include(<sys/codesign.h>)
#include <sys/codesign.h>
#else
// SPI
#define CS_OPS_STATUS 0
#define CS_GET_TASK_ALLOW  0x00000004
#define CS_RUNTIME         0x00010000
#define CS_PLATFORM_BINARY 0x04000000
#define CS_PLATFORM_PATH   0x08000000
extern "C" int csops(int, unsigned int, void *, size_t);
#endif
#include <stdint.h>
#endif

bool swift::runtime::_swift_isPrivilegedProcess() {
#if defined(__linux__)
  return getauxval(AT_SECURE);
#elif TARGET_OS_OSX || TARGET_OS_IPHONE || defined(__FreeBSD__) ||             \
    defined(__OpenBSD__)
  return issetugid();
#else
  return false;
#endif
}

#if TARGET_OS_OSX || TARGET_OS_MACCATALYST
// Returns false if the flags can't be read, so callers treat a failed query as
// the most restrictive answer.
static bool getCodeSigningFlags(uint32_t &flags) {
  flags = 0;
  return csops(getpid(), CS_OPS_STATUS, &flags, sizeof(flags)) == 0;
}
#endif

bool swift::runtime::_swift_isRestrictedProcess() {
  if (_swift_isPrivilegedProcess())
    return true;

#if TARGET_OS_OSX || TARGET_OS_MACCATALYST
  uint32_t flags;
  if (!getCodeSigningFlags(flags))
    return true;

  return (flags & (CS_PLATFORM_BINARY | CS_PLATFORM_PATH | CS_RUNTIME)) != 0;
#else
  return false;
#endif
}

bool swift::runtime::_swift_isRestrictedProcessForExec() {
  if (_swift_isRestrictedProcess())
    return true;

#if TARGET_OS_OSX || TARGET_OS_MACCATALYST
  uint32_t flags;
  if (!getCodeSigningFlags(flags))
    return true;

  return !(flags & CS_GET_TASK_ALLOW);
#else
  return false;
#endif
}
