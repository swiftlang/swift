//===--- Errors.cpp - Demangling library error handling ---------*- C++ -*-===//
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
// Provides the demangling library with its own error handling functions.
// This is necessary because it isn't always linked with the runtime, so
// it can't use the runtime's error handling.
//
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Errors.h"
#include <inttypes.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <io.h>
#endif

#if SWIFT_STDLIB_HAS_ASL
#include <asl.h>
#elif defined(__ANDROID__)
#include <android/log.h>
#endif

#if SWIFT_HAVE_CRASHREPORTERCLIENT
#include <atomic>
#include <malloc/malloc.h>

#include "swift/Runtime/Atomic.h"

#include "CrashReporter.h"
#endif // SWIFT_HAVE_CRASHREPORTERCLIENT

// -- Declarations -----------------------------------------------------------

static SWIFT_NORETURN void demangleFatal(uint32_t flags, const char *format,
                                         va_list val);
static void demangleWarn(uint32_t flags, const char *format, va_list val);

static int demangle_vasprintf(char **strp, const char *format, va_list val);

#if SWIFT_HAVE_CRASHREPORTERCLIENT
static int demangle_asprintf(char **strp, const char *format, ...);
#endif

// -- Crash reporter integration ---------------------------------------------

// Report a message to any forthcoming crash log.
static void reportOnCrash(uint32_t flags, const char *message) {
#if SWIFT_HAVE_CRASHREPORTERCLIENT
  char *oldMessage = nullptr;
  char *newMessage = nullptr;

  oldMessage = std::atomic_load_explicit(
    (volatile std::atomic<char *> *)&gCRAnnotations.message,
    SWIFT_MEMORY_ORDER_CONSUME);

  do {
    if (newMessage) {
      free(newMessage);
      newMessage = nullptr;
    }

    if (oldMessage) {
      demangle_asprintf(&newMessage, "%s%s", oldMessage, message);
    } else {
      newMessage = strdup(message);
    }
  } while (!std::atomic_compare_exchange_strong_explicit(
             (volatile std::atomic<char *> *)&gCRAnnotations.message,
             &oldMessage, newMessage,
             std::memory_order_release,
             SWIFT_MEMORY_ORDER_CONSUME));

  if (oldMessage && malloc_size(oldMessage))
    free(oldMessage);
#else
  // empty
#endif // SWIFT_HAVE_CRASHREPORTERCLIENT
}

// -- Utility functions ------------------------------------------------------

// Report a message to system console and stderr.
static void reportNow(uint32_t flags, const char *message) {
#if defined(_WIN32)
#define STDERR_FILENO 2
  _write(STDERR_FILENO, message, strlen(message));
#else
  fputs(message, stderr);
  fflush(stderr);
#endif
#if SWIFT_STDLIB_HAS_ASL
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  asl_log(nullptr, nullptr, ASL_LEVEL_ERR, "%s", message);
#pragma clang diagnostic pop
#elif defined(__ANDROID__) && !defined(__TERMUX__)
  __android_log_print(ANDROID_LOG_FATAL, "SwiftDemangle", "%s", message);
#endif
}

/// Report a fatal error to system console, stderr, and crash logs.
/// Does not crash by itself.
static void reportError(uint32_t flags, const char *message) {
  reportNow(flags, message);
  reportOnCrash(flags, message);
}

/// Not everything has vasprintf()
SWIFT_VFORMAT(2)
static int demangle_vasprintf(char **strp, const char *format, va_list args) {
  va_list args_for_len;

  va_copy(args_for_len, args);
  int len = vsnprintf(nullptr, 0, format, args_for_len);
  va_end(args_for_len);

  *strp = nullptr;

  if (len < 0)
    return -1;

  size_t bufsiz = len + 1;
  char *buffer = reinterpret_cast<char *>(malloc(bufsiz));
  if (!buffer)
    return -1;

  int result = vsnprintf(buffer, bufsiz, format, args);
  if (result < 0) {
    free(buffer);
    return -1;
  }

  *strp = buffer;
  return result;
}

#if SWIFT_HAVE_CRASHREPORTERCLIENT
SWIFT_FORMAT(2,3)
static int demangle_asprintf(char **strp, const char *format, ...) {
  va_list val;

  va_start(val, format);
  int ret = demangle_vasprintf(strp, format, val);
  va_end(val);

  return ret;
}
#endif // SWIFT_HAVE_CRASHREPORTERCLIENT

// -- Implementation ---------------------------------------------------------

SWIFT_VFORMAT(2)
static SWIFT_NORETURN void demangleFatal(uint32_t flags, const char *format,
                                         va_list val) {
  char *message;

  if (demangle_vasprintf(&message, format, val) < 0) {
    reportError(flags, "unable to format fatal error message");
    abort();
  }

  reportError(flags, message);
  abort();
}

SWIFT_VFORMAT(2)
static void demangleWarn(uint32_t flags, const char *format, va_list val) {
  char *message;

  if (demangle_vasprintf(&message, format, val) < 0) {
    reportNow(flags, "unable to format warning message");
    return;
  }

  reportNow(flags, message);
  free(message);
}

// -- Public API -------------------------------------------------------------

namespace swift {
namespace Demangle {
SWIFT_BEGIN_INLINE_NAMESPACE

SWIFT_FORMAT(2, 3)
SWIFT_NORETURN void fatal(uint32_t flags, const char *format, ...) {
  va_list val;

  va_start(val, format);
  fatalv(flags, format, val);
}

SWIFT_FORMAT(2, 3)
void warn(uint32_t flags, const char *format, ...) {
  va_list val;

  va_start(val, format);
  warnv(flags, format, val);
  va_end(val);
}

SWIFT_VFORMAT(2)
SWIFT_NORETURN void fatalv(uint32_t flags, const char *format, va_list val) {
  demangleFatal(flags, format, val);
}

SWIFT_VFORMAT(2)
void warnv(uint32_t flags, const char *format, va_list val) {
  demangleWarn(flags, format, val);
}

SWIFT_END_INLINE_NAMESPACE
} // end namespace Demangle
} // end namespace swift
