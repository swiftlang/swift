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
// This file is the public API of the demangler library.
// Tools which use the demangler library (like lldb) must include this - and
// only this - header file.
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

#if SWIFT_DEMANGLE_USE_RUNTIME_ERRORS
#include "swift/Runtime/Debug.h"
#endif

#if !SWIFT_DEMANGLE_USE_RUNTIME_ERRORS

// -- Declarations -----------------------------------------------------------

static SWIFT_NORETURN void demangleFatal(uint32_t flags, const char *format,
                                         va_list val);
static void demangleWarn(uint32_t flags, const char *format, va_list val);

static int demangle_vasprintf(char **strp, const char *format, va_list val);

#if SWIFT_HAVE_CRASHREPORTERCLIENT
static int demangle_asprintf(char **strp, const char *format, ...);
#endif

// -- Crash reporter integration ---------------------------------------------

#if SWIFT_HAVE_CRASHREPORTERCLIENT
#include <malloc/malloc.h>
#include <pthread.h>

#define CRASHREPORTER_ANNOTATIONS_VERSION 5
#define CRASHREPORTER_ANNOTATIONS_SECTION "__crash_info"

struct crashreporter_annotations_t {
  uint64_t version;          // unsigned long
  uint64_t message;          // char *
  uint64_t signature_string; // char *
  uint64_t backtrace;        // char *
  uint64_t message2;         // char *
  uint64_t thread;           // uint64_t
  uint64_t dialog_mode;      // unsigned int
  uint64_t abort_cause;      // unsigned int
};

// Instead of linking to CrashReporterClient.a (because it complicates the
// build system), define the only symbol from that static archive ourselves.
//
// The layout of this struct is CrashReporter ABI, so there are no ABI concerns
// here.
extern "C" {
SWIFT_LIBRARY_VISIBILITY
struct crashreporter_annotations_t gCRAnnotations __attribute__((
    __section__("__DATA," CRASHREPORTER_ANNOTATIONS_SECTION))) = {
    CRASHREPORTER_ANNOTATIONS_VERSION, 0, 0, 0, 0, 0, 0, 0};
}

static inline void CRSetCrashLogMessage(const char *message) {
  gCRAnnotations.message = reinterpret_cast<uint64_t>(message);
}

static inline const char *CRGetCrashLogMessage() {
  return reinterpret_cast<const char *>(gCRAnnotations.message);
}

// Report a message to any forthcoming crash log.
static void reportOnCrash(uint32_t flags, const char *message) {
  // We must use an "unsafe" mutex in this pathway since the normal "safe"
  // mutex calls fatal when an error is detected and fatal ends up
  // calling us. In other words we could get infinite recursion if the
  // mutex errors.
  static pthread_mutex_t crashlogLock = PTHREAD_MUTEX_INITIALIZER;

  pthread_mutex_lock(&crashlogLock);

  char *oldMessage = const_cast<char *>(CRGetCrashLogMessage());
  char *newMessage;
  if (oldMessage) {
    demangle_asprintf(&newMessage, "%s%s", oldMessage, message);
    if (malloc_size(oldMessage))
      free(oldMessage);
  } else {
    newMessage = strdup(message);
  }

  CRSetCrashLogMessage(newMessage);

  pthread_mutex_unlock(&crashlogLock);
}

#else
static void

reportOnCrash(uint32_t flags, const char *message) {
  // empty
}

#endif // SWIFT_HAVE_CRASHREPORTERCLIENT

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
  asl_log(nullptr, nullptr, ASL_LEVEL_ERR, "%s", message);
#elif defined(__ANDROID__)
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

static void demangleWarn(uint32_t flags, const char *format, va_list val) {
  char *message;

  if (demangle_vasprintf(&message, format, val) < 0) {
    reportNow(flags, "unable to format warning message");
    return;
  }

  reportNow(flags, message);
  free(message);
}

#endif // !SWIFT_DEMANGLE_USE_RUNTIME_ERRORS

// -- Public API -------------------------------------------------------------

namespace swift {
namespace Demangle {
SWIFT_BEGIN_INLINE_NAMESPACE

SWIFT_NORETURN void fatal(uint32_t flags, const char *format, ...) {
  va_list val;

  va_start(val, format);
  fatalv(flags, format, val);
}

void warn(uint32_t flags, const char *format, ...) {
  va_list val;

  va_start(val, format);
  warnv(flags, format, val);
  va_end(val);
}

SWIFT_NORETURN void fatalv(uint32_t flags, const char *format, va_list val) {
#if SWIFT_DEMANGLE_USE_RUNTIME_ERRORS
  swift::fatalErrorv(flags, format, val);
#else
  demangleFatal(flags, format, val);
#endif
}

void warnv(uint32_t flags, const char *format, va_list val) {
#if SWIFT_DEMANGLE_USE_RUNTIME_ERRORS
  swift::warningv(flags, format, val);
#else
  demangleWarn(flags, format, val);
#endif
}

SWIFT_END_INLINE_NAMESPACE
} // end namespace Demangle
} // end namespace swift
