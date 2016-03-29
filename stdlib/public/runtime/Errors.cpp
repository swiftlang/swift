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
#include "swift/Basic/Demangle.h"
#include <cxxabi.h>

#if !defined(__CYGWIN__)
#include <execinfo.h>
#endif

#include <dlfcn.h>
#include <inttypes.h>
#include <libgen.h>

#ifdef __APPLE__
#include <asl.h>
#endif

namespace FatalErrorFlags {
enum : uint32_t { ReportBacktrace = 1 << 0 };
} // end namespace FatalErrorFlags

#if !defined(__CYGWIN__)
static std::string demangledSymbol(std::string symbol) {
  if (symbol.size() < 2)
    return symbol;
  auto prefix = symbol.substr(0, 2);
  if (prefix == "_Z") {
    int status;
    auto demangled = abi::__cxa_demangle(symbol.c_str(), 0, 0, &status);
    if (demangled != NULL) {
      std::string out = demangled;
      free(demangled);
      return out;
    }
  } else if (prefix == "_T") {
    return swift::Demangle::demangleSymbolAsString(symbol);
  }
  return symbol;
}
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
static void reportOnCrash(uint32_t flags, const char *message) {
  static pthread_mutex_t crashlogLock = PTHREAD_MUTEX_INITIALIZER;
  pthread_mutex_lock(&crashlogLock);

  char *oldMessage = (char *)CRGetCrashLogMessage();

  char *newMessage;
  if (oldMessage) {
    asprintf(&newMessage, "%s%s", oldMessage, message);
    if (malloc_size(oldMessage))
      free(oldMessage);
  } else {
    newMessage = strdup(message);
  }

  CRSetCrashLogMessage(newMessage);

  pthread_mutex_unlock(&crashlogLock);
}

#else

static void reportOnCrash(uint32_t flags, const char *message) {
  // empty
}

#endif

const int STACK_DEPTH = 128;

// Report a message to system console and stderr.
static void reportNow(uint32_t flags, const char *message) {
  write(STDERR_FILENO, message, strlen(message));
#ifdef __APPLE__
  asl_log(NULL, NULL, ASL_LEVEL_ERR, "%s", message);
#endif
#if !defined(__CYGWIN__)
  if (flags & FatalErrorFlags::ReportBacktrace) {
    void **addrs = (void **)malloc(sizeof(void *) * STACK_DEPTH);
    if (addrs == NULL)
      return;
    int symbolCount = backtrace(addrs, STACK_DEPTH);
    if (symbolCount == 0)
      return;
    fputs("Current stack trace:\n", stderr);
    for (int i = 0; i < symbolCount; i++) {
      Dl_info info;
      if (dladdr(addrs[i], &info) == 0)
        return;

      auto symbol =
          info.dli_sname ? demangledSymbol(info.dli_sname) : "<unknown>";

#define SYM_COMMON "%-3d %-35s 0x%" PRIxPTR " %s"

      if (info.dli_saddr == NULL) {
        fprintf(stderr, SYM_COMMON " (0x%" PRIxPTR ")\n", i,
                basename((char *)info.dli_fname), (uintptr_t)info.dli_saddr,
                symbol.c_str(), (intptr_t)addrs[i]);
      } else {
        int offset = (int)((intptr_t)addrs[i] - (intptr_t)info.dli_saddr);
        fprintf(stderr, SYM_COMMON " + %d\n", i,
                basename((char *)info.dli_fname), (uintptr_t)info.dli_saddr,
                symbol.c_str(), offset);
      }
    }
    free(addrs);
  }
#endif
}

/// Report a fatal error to system console, stderr, and crash logs.
/// Does not crash by itself.
void swift::swift_reportError(uint32_t flags, const char *message) {
  reportNow(flags, message);
  reportOnCrash(flags, message);
}

// Report a fatal error to system console, stderr, and crash logs, then abort.
LLVM_ATTRIBUTE_NORETURN
void swift::fatalError(uint32_t flags, const char *format, ...) {
  va_list args;
  va_start(args, format);

  char *log;
  vasprintf(&log, format, args);

  swift_reportError(flags, log);
  abort();
}

// Crash when a deleted method is called by accident.
SWIFT_RUNTIME_EXPORT
LLVM_ATTRIBUTE_NORETURN
extern "C" void swift_deletedMethodError() {
  swift::fatalError(/* flags = */ 0, "fatal error: call of deleted method\n");
}
