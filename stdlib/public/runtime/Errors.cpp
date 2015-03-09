//===--- Errors.cpp - Error reporting utilities ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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
#include "Debug.h"

#ifdef __APPLE__
#include <asl.h>
#endif

#if SWIFT_HAVE_CRASHREPORTERCLIENT

#include <CrashReporterClient.h>
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
        CRASHREPORTER_ANNOTATIONS_VERSION, 0, 0, 0, 0, 0, 0};
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


// Report a fatal error to system console, stderr, and crash logs, then abort.
LLVM_ATTRIBUTE_NORETURN
void
swift::fatalError(const char *format, ...)
{
  va_list args;
  va_start(args, format);

  char *log;
  vasprintf(&log, format, args);

  reportNow(log);
  reportOnCrash(log);

  abort();
}


// Report a fatal error to system console, stderr, and crash logs.
// <prefix>: <message>: file <file>, line <line>\n
// The message may be omitted by passing messageLength=0.
extern "C" void
swift_reportFatalErrorInFile(const char *prefix, intptr_t prefixLength,
                             const char *message, intptr_t messageLength,
                             const char *file, intptr_t fileLength,
                             uintptr_t line) {
  char *log;
  asprintf(&log, "%.*s: %.*s%sfile %.*s, line %zu\n", (int)prefixLength, prefix,
           (int)messageLength, message, (messageLength ? ": " : ""),
           (int)fileLength, file, (size_t)line);

  reportNow(log);
  reportOnCrash(log);

  free(log);
}

// Report a fatal error to system console, stderr, and crash logs.
// <prefix>: <message>: file <file>, line <line>\n
// The message may be omitted by passing messageLength=0.
extern "C" void swift_reportFatalError(const char *prefix,
                                       intptr_t prefixLength,
                                       const char *message,
                                       intptr_t messageLength) {
  char *log;
  asprintf(&log, "%.*s: %.*s\n", (int)prefixLength, prefix,
           (int)messageLength, message);

  reportNow(log);
  reportOnCrash(log);

  free(log);
}

// Report a call to an unimplemented initializer.
// <file>: <line>: <column>: fatal error: use of unimplemented 
// initializer '<initName>' for class 'className'
extern "C" void swift_reportUnimplementedInitializerInFile(
    const char *className, intptr_t classNameLength, const char *initName,
    intptr_t initNameLength, const char *file, intptr_t fileLength,
    uintptr_t line, uintptr_t column) {
  char *log;
  asprintf(&log, "%.*s: %zu: %zu: fatal error: use of unimplemented "
                 "initializer '%.*s' for class '%.*s'\n",
           (int)fileLength, file, (size_t)line, (size_t)column,
           (int)initNameLength, initName, (int)classNameLength, className);

  reportNow(log);
  reportOnCrash(log);

  free(log);
}

// Report a call to an unimplemented initializer.
// fatal error: use of unimplemented initializer '<initName>' for class
// 'className'
extern "C" void swift_reportUnimplementedInitializer(const char *className,
                                                     intptr_t classNameLength,
                                                     const char *initName,
                                                     intptr_t initNameLength) {
  char *log;
  asprintf(&log, "fatal error: use of unimplemented "
                 "initializer '%.*s' for class '%.*s'\n",
           (int)initNameLength, initName, (int)classNameLength, className);

  reportNow(log);
  reportOnCrash(log);

  free(log);
}

// Report a call to a removed method.
LLVM_ATTRIBUTE_NORETURN
extern "C" void
swift_reportMissingMethod() {
  swift::fatalError("fatal error: call of removed method\n");
}
