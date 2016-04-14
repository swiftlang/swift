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
#if !defined(__CYGWIN__) && !defined(__ANDROID__)
// execinfo.h is not available on Android. Checks in this file ensure that
// fatalError behaves as expected, but without stack traces.
#include <execinfo.h>
#endif

#ifdef __APPLE__
#include <asl.h>
#endif

namespace FatalErrorFlags {
enum: uint32_t {
  ReportBacktrace = 1 << 0
};
} // end namespace FatalErrorFlags

#if !defined(__CYGWIN__) && !defined(__ANDROID__)
LLVM_ATTRIBUTE_ALWAYS_INLINE
static bool
isIdentifier(char c)
{
  return isalnum(c) || c == '_' || c == '$';
}

static bool
demangledLinePrefix(std::string line, std::string prefix,
                    std::string &out,
                    bool (*demangle)(std::string, std::string &))
{
  int symbolStart = -1;
  int symbolEnd = -1;
  
  for (size_t i = 0; i < line.size(); i++) {
    char c = line[i];
    
    bool hasBegun = symbolStart != -1;
    bool isIdentifierChar = isIdentifier(c);
    bool isEndOfSymbol = hasBegun && !isIdentifierChar;
    bool isEndOfLine = i == line.size() - 1;
    
    if (isEndOfLine || isEndOfSymbol) {
      symbolEnd = i;
      break;
    }

    bool canFindPrefix = (line.size() - 2) - i > prefix.size();
    if (!hasBegun && canFindPrefix && !isIdentifierChar &&
        line.substr(i + 1, prefix.size()) == prefix) {
      symbolStart = i + 1;
      continue;
    }
  }
  
  if (symbolStart == -1 || symbolEnd == -1) {
    out = line;
    return false;
  } else {
    auto symbol = line.substr(symbolStart, symbolEnd - symbolStart);
    
    std::string demangled;
    bool success = demangle(symbol, demangled);
    
    if (success) {
      line.replace(symbolStart, symbolEnd - symbolStart, demangled);
    }
    
    out = line;
    
    return success;
  }
}

static std::string
demangledLine(std::string line) {
  std::string res;
  bool success = false;
  auto cppPrefix = "_Z"; // not sure how to check for DARWIN's __Z here.
  success = demangledLinePrefix(line, cppPrefix, res,
                                [](std::string symbol, std::string &out) {
    int status;
    auto demangled = abi::__cxa_demangle(symbol.c_str(), 0, 0, &status);
    if (demangled == NULL || status != 0) {
      out = symbol;
      return false;
    } else {
      out = demangled;
      free(demangled);
      return true;
    }
  });
  if (success) return res;
  success = demangledLinePrefix(line, "_T", res,
                                [](std::string symbol, std::string &out) {
    out = swift::Demangle::demangleSymbolAsString(symbol);
    return true;
  });
  if (success) return res;
  return line;
}

const int STACK_DEPTH = 128;

static char **
reportBacktrace(int *count)
{
  void **addrs = (void **)malloc(sizeof(void *) * STACK_DEPTH);
  if (addrs == NULL) {
    if (count) *count = 0;
    return NULL;
  }
  int symbolCount = backtrace(addrs, STACK_DEPTH);
  if (count) *count = symbolCount;

  char **symbols = backtrace_symbols(addrs, symbolCount);
  free(addrs);
  if (symbols == NULL) {
    if (count) *count = 0;
    return NULL;
  }

  return symbols;
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
static void
reportOnCrash(uint32_t flags, const char *message)
{
  // FIXME: SR-946 - we can't yet switch the following to use swift::Mutex
  //                 since swift::Mutex uses fatalError and we could end
  //                 up back here again ...and again ...and again
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
reportOnCrash(uint32_t flags, const char *message)
{
  // empty
}

#endif


// Report a message to system console and stderr.
static void
reportNow(uint32_t flags, const char *message)
{
  write(STDERR_FILENO, message, strlen(message));
#ifdef __APPLE__
  asl_log(NULL, NULL, ASL_LEVEL_ERR, "%s", message);
#endif
#if !defined(__CYGWIN__) && !defined(__ANDROID__)
  if (flags & FatalErrorFlags::ReportBacktrace) {
    fputs("Current stack trace:\n", stderr);
    int count = 0;
    char **trace = reportBacktrace(&count);
    for (int i = 0; i < count; i++) {
      fprintf(stderr, "%s\n", demangledLine(trace[i]).c_str());
    }
    free(trace);
  }
#endif
}

/// Report a fatal error to system console, stderr, and crash logs.
/// Does not crash by itself.
void swift::swift_reportError(uint32_t flags,
                              const char *message) {
  reportNow(flags, message);
  reportOnCrash(flags, message);
}

// Report a fatal error to system console, stderr, and crash logs, then abort.
LLVM_ATTRIBUTE_NORETURN
void
swift::fatalError(uint32_t flags, const char *format, ...)
{
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
extern "C" void
swift_deletedMethodError() {
  swift::fatalError(/* flags = */ 0,
                    "fatal error: call of deleted method\n");
}
