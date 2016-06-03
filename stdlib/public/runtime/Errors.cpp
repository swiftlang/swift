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

#if defined(__CYGWIN__) || defined(__ANDROID__) || defined(_MSC_VER)
#  define SWIFT_SUPPORTS_BACKTRACE_REPORTING 0
#else
#  define SWIFT_SUPPORTS_BACKTRACE_REPORTING 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#if defined(_MSC_VER)
#include <io.h>
#else
#include <unistd.h>
#endif
#include <stdarg.h>
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#if !defined(_MSC_VER)
#include <cxxabi.h>
#endif
#if SWIFT_SUPPORTS_BACKTRACE_REPORTING

// execinfo.h is not available on Android. Checks in this file ensure that
// fatalError behaves as expected, but without stack traces.
#include <execinfo.h>
// We are only using dlfcn.h in code that is invoked on non cygwin/android
// platforms. So I am putting it here.
#include <dlfcn.h>
#endif

#ifdef __APPLE__
#include <asl.h>
#endif

namespace FatalErrorFlags {
enum: uint32_t {
  ReportBacktrace = 1 << 0
};
} // end namespace FatalErrorFlags

using namespace swift;

#if SWIFT_SUPPORTS_BACKTRACE_REPORTING

static bool getSymbolNameAddr(llvm::StringRef libraryName, Dl_info dlinfo,
                              std::string &symbolName, uintptr_t &addrOut) {

  // If we failed to find a symbol and thus dlinfo->dli_sname is nullptr, we
  // need to use the hex address.
  bool hasUnavailableAddress = dlinfo.dli_sname == nullptr;

  // If the address is unavailable, just use <unavailable> as the symbol
  // name. We do not set addrOut, since addrOut will be set to our ptr address.
  if (hasUnavailableAddress) {
    symbolName += "<unavailable>";
    return false;
  }

  // Ok, now we know that we have some sort of "real" name. Set the outAddr.
  addrOut = uintptr_t(dlinfo.dli_saddr);

  // First lets try to demangle using cxxabi. If this fails, we will try to
  // demangle with swift. We are taking advantage of __cxa_demangle actually
  // providing failure status instead of just returning the original string like
  // swift demangle.
  int status;
  char *demangled = abi::__cxa_demangle(dlinfo.dli_sname, 0, 0, &status);
  if (status == 0) {
    assert(demangled != nullptr && "If __cxa_demangle succeeds, demangled "
                                   "should never be nullptr");
    symbolName += demangled;
    free(demangled);
    return true;
  }
  assert(demangled == nullptr && "If __cxa_demangle fails, demangled should "
                                 "be a nullptr");

  // Otherwise, try to demangle with swift. If swift fails to demangle, it will
  // just pass through the original output.
  symbolName = demangleSymbolAsString(
      dlinfo.dli_sname, strlen(dlinfo.dli_sname),
      Demangle::DemangleOptions::SimplifiedUIDemangleOptions());
  return true;
}

/// This function dumps one line of a stack trace. It is assumed that \p address
/// is the address of the stack frame at index \p index.
static void dumpStackTraceEntry(unsigned index, void *framePC) {
  Dl_info dlinfo;

  // 0 is failure for dladdr. We do not use nullptr since it is an int
  // argument. This violates normal unix patterns. See man page for dladdr on OS
  // X.
  if (0 == dladdr(framePC, &dlinfo)) {
    return;
  }

  // According to the man page of dladdr, if dladdr returns non-zero, then we
  // know that it must have fname, fbase set. Thus, we find the library name
  // here.
  StringRef libraryName = StringRef(dlinfo.dli_fname).rsplit('/').second;

  // Next we get the symbol name that we are going to use in our backtrace.
  std::string symbolName;
  // We initialize symbolAddr to framePC so that if we succeed in finding the
  // symbol, we get the offset in the function and if we fail to find the symbol
  // we just get HexAddr + 0.
  uintptr_t symbolAddr = uintptr_t(framePC);
  bool foundSymbol =
      getSymbolNameAddr(libraryName, dlinfo, symbolName, symbolAddr);

  // We do not use %p here for our pointers since the format is implementation
  // defined. This makes it logically impossible to check the output. Forcing
  // hexadecimal solves this issue.
  static const char *backtraceEntryFormat = "%-4u %-34s 0x%0.16lx %s + %td\n";

  // Then dump the backtrace.
  fprintf(stderr, backtraceEntryFormat, index, libraryName.data(),
          foundSymbol ? symbolAddr : uintptr_t(framePC), symbolName.c_str(),
          ptrdiff_t(uintptr_t(framePC) - symbolAddr));
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
__attribute__((__section__("__DATA," CRASHREPORTER_ANNOTATIONS_SECTION))) = {
    CRASHREPORTER_ANNOTATIONS_VERSION, 0, 0, 0, 0, 0, 0, 0};
}

// Report a message to any forthcoming crash log.
static void
reportOnCrash(uint32_t flags, const char *message)
{
  // We must use an "unsafe" mutex in this pathway since the normal "safe"
  // mutex calls fatalError when an error is detected and fatalError ends up
  // calling us. In other words we could get infinite recursion if the
  // mutex errors.
  static swift::StaticUnsafeMutex crashlogLock;

  crashlogLock.lock();

  char *oldMessage = (char *)CRGetCrashLogMessage();
  char *newMessage;
  if (oldMessage) {
    asprintf(&newMessage, "%s%s", oldMessage, message);
    if (malloc_size(oldMessage)) free(oldMessage);
  } else {
    newMessage = strdup(message);
  }
  
  CRSetCrashLogMessage(newMessage);

  crashlogLock.unlock();
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
#if defined(_MSC_VER)
#define STDERR_FILENO 2
  _write(STDERR_FILENO, message, strlen(message));
#else
  write(STDERR_FILENO, message, strlen(message));
#endif
#ifdef __APPLE__
  asl_log(NULL, NULL, ASL_LEVEL_ERR, "%s", message);
#endif
#if SWIFT_SUPPORTS_BACKTRACE_REPORTING
  if (flags & FatalErrorFlags::ReportBacktrace) {
    fputs("Current stack trace:\n", stderr);
    constexpr unsigned maxSupportedStackDepth = 128;
    void *addrs[maxSupportedStackDepth];

    int symbolCount = backtrace(addrs, maxSupportedStackDepth);
    for (int i = 0; i < symbolCount; ++i) {
      dumpStackTraceEntry(i, addrs[i]);
    }
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

static int swift_vasprintf(char **strp, const char *fmt, va_list ap) {
#if defined(_MSC_VER)
  int len = _vscprintf(fmt, ap);
  if (len < 0)
    return -1;
  char *buffer = reinterpret_cast<char *>(malloc(len + 1));
  if (!buffer)
    return -1;
  int result = vsprintf(*strp, fmt, ap);
  if (result < 0) {
    free(buffer);
    return -1;
  }
  *strp = buffer;
  return result;
#else
  return vasprintf(strp, fmt, ap);
#endif
}

// Report a fatal error to system console, stderr, and crash logs, then abort.
LLVM_ATTRIBUTE_NORETURN
void
swift::fatalError(uint32_t flags, const char *format, ...)
{
  va_list args;
  va_start(args, format);

  char *log;
  swift_vasprintf(&log, format, args);

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
