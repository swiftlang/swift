//===--- Errors.cpp - Error reporting utilities ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utilities for reporting errors to stderr, system console, and crash logs.
//
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <pthread.h>
#include <stdarg.h>
#include "swift/Runtime/Debug.h"

#ifdef __APPLE__
#include <asl.h>
#endif

#ifdef SWIFT_HAVE_CRASHREPORTERCLIENT
#include <malloc/malloc.h>

// Instead of linking to CrashReporterClient.a (because it complicates the
// build system), define the only symbol from that static archive ourselves.
//
// The layout of this struct is CrashReporter ABI, so there are no ABI concerns
// here.
extern "C" {
CRASH_REPORTER_CLIENT_HIDDEN
struct crashreporter_annotations_t gCRAnnotations
    __attribute__((section("__DATA," CRASHREPORTER_ANNOTATIONS_SECTION))) = {
        CRASHREPORTER_ANNOTATIONS_VERSION, 0, 0, 0, 0, 0, 0, 0};
}

// Report a message to any forthcoming crash log.
static void
reportOnCrash(const char *message)
{
  static pthread_mutex_t crashlogLock = PTHREAD_MUTEX_INITIALIZER;
  pthread_mutex_lock(&crashlogLock);
  
  char *oldMessage = (char *)CRGetCrashLogMessage();

  char *newMessage;
  if (oldMessage) {
    asprintf(&newMessage, "%s%s", oldMessage, message);
    if (malloc_size(oldMessage)) free(oldMessage);
  } else {
    newMessage = strdup(message);
  }

  CRSetCrashLogMessage(newMessage);

  pthread_mutex_unlock(&crashlogLock);
}

#else

static void
reportOnCrash(const char *message)
{
  // empty
}

#endif


// Report a message to system console and stderr.
static void
reportNow(const char *message)
{
  write(STDERR_FILENO, message, strlen(message));
#ifdef __APPLE__
  asl_log(NULL, NULL, ASL_LEVEL_ERR, "%s", message);
#endif
}

/// Report a fatal error to system console, stderr, and crash logs.
/// Does not crash by itself.
void swift::swift_reportError(const char *message) {
  reportNow(message);
  reportOnCrash(message);
}

// Report a fatal error to system console, stderr, and crash logs, then abort.
LLVM_ATTRIBUTE_NORETURN
void
swift::fatalError(const char *format, ...)
{
  va_list args;
  va_start(args, format);

  char *log;
  vasprintf(&log, format, args);

  swift_reportError(log);
  abort();
}

// Crash when a deleted method is called by accident.
LLVM_ATTRIBUTE_NORETURN
extern "C" void
swift_deletedMethodError() {
  swift::fatalError("fatal error: call of deleted method\n");
}
