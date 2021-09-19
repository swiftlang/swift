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
#ifdef __clang__
__attribute((__format__(__printf__, 2, 0)))
#endif
inline static int
swift_vasprintf(char **strp, const char *fmt, va_list args) {
  va_list args_for_len;
  va_copy(args_for_len, args);
  int len = vsnprintf(nullptr, 0, fmt, args_for_len);
  va_end(args_for_len);

  // If we fail for any reason, strp needs to be set to NULL.
  *strp = nullptr;

  if (len < 0)
    return -1;
  char *buffer = reinterpret_cast<char *>(malloc(len + 1));
  if (!buffer)
    return -1;
  int result = vsnprintf(buffer, len + 1, fmt, args);
  if (result < 0) {
    free(buffer);
    return -1;
  }
  *strp = buffer;
  return result;
}

// Skip the attribute when included by the compiler.
#ifdef SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
#endif
#ifdef __clang__
__attribute((__format__(__printf__, 2, 3)))
#endif
inline static int
swift_asprintf(char **strp, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int result = swift_vasprintf(strp, fmt, args);
  va_end(args);
  return result;
}

#endif
