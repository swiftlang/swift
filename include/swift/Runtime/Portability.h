//===--- Portability.h ------------------------------------------*- C++ -*-===//
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
// Includes stub APIs that make the portable runtime easier to write.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_PORTABILITY_H
#define SWIFT_RUNTIME_PORTABILITY_H

#include <cstdarg>
#include <cstddef>
#include <cstdio>
#include <cstdlib>

size_t _swift_strlcpy(char *dst, const char *src, size_t maxlen);

// Skip the attribute when included by the compiler.
#ifdef SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
#endif
inline static int swift_asprintf(char **strp, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
#if defined(_WIN32)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
  int len = _vscprintf(fmt, args);
#pragma GCC diagnostic pop
  if (len < 0) {
    va_end(args);
    return -1;
  }
  char *buffer = static_cast<char *>(malloc(len + 1));
  if (!buffer) {
    va_end(args);
    return -1;
  }
  int result = vsprintf(buffer, fmt, args);
  if (result < 0) {
    va_end(args);
    free(buffer);
    return -1;
  }
  *strp = buffer;
#else
  int result = vasprintf(strp, fmt, args);
#endif
  va_end(args);
  return result;
}

#endif
