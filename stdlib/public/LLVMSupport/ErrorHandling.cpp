//===- lib/Support/ErrorHandling.cpp - Callbacks for errors ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines an API used to indicate fatal error conditions.  Non-fatal
// errors (most of them) should be handled through LLVMContext.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/StringRef.h"
#include <cassert>
#include <cstdlib>
#include <cstdio>

#if defined(_MSC_VER)
# include <io.h>
# include <fcntl.h>
#endif

#include <stdarg.h>

#if SWIFT_STDLIB_HAS_ASL
#include <asl.h>
#elif defined(__ANDROID__)
#include <android/log.h>
#endif

namespace {
void error(const char *prefix, const char *msg, const char *file = nullptr, unsigned line = 0) {
  char buffer[1024];

  if (file) {
    snprintf(buffer, sizeof(buffer), "%s%s at %s:%u\n", prefix, msg, file, line);
  } else {
    snprintf(buffer, sizeof(buffer), "%s%s\n", prefix, msg);
  }

#if SWIFT_STDLIB_HAS_ASL
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  asl_log(nullptr, nullptr, ASL_LEVEL_ERR, "%s", buffer);
#pragma clang diagnostic pop
#elif defined(__ANDROID__) && !defined(__TERMUX__)
  __android_log_print(ANDROID_LOG_FATAL, "SwiftRuntime", "%s", buffer);
#elif defined(_WIN32)
#define STDERR_FILENO 2
  _write(STDERR_FILENO, buffer, strlen(buffer));
#else
  fputs(buffer, stderr);
  fflush(stderr);
#endif
}
}

using namespace llvm;

void __swift::__runtime::llvm::report_fatal_error(const char *Reason,
                                                  bool GenCrashDiag) {
  error("LLVM ERROR: ", Reason);
  abort();
}

void __swift::__runtime::llvm::report_fatal_error(const std::string &Reason,
                                                  bool GenCrashDiag) {
  report_fatal_error(Reason.c_str(), GenCrashDiag);
}

void __swift::__runtime::llvm::report_fatal_error(StringRef Reason,
                                                  bool GenCrashDiag) {
  report_fatal_error(Reason.str(), GenCrashDiag);
}

void __swift::__runtime::llvm::report_bad_alloc_error(const char *Reason,
                                                      bool GenCrashDiag) {
  // Don't call the normal error handler. It may allocate memory. Directly write
  // an OOM to stderr and abort.
  error("LLVM ERROR: ", "out of memory");
  abort();
}

void __swift::__runtime::llvm::llvm_unreachable_internal(
    const char *msg, const char *file, unsigned line) {
  // This code intentionally doesn't call the ErrorHandler callback, because
  // llvm_unreachable is intended to be used to indicate "impossible"
  // situations, and not legitimate runtime errors.
  error("", msg ? msg : "UNREACHABLE executed!", file, line);
  abort();
#ifdef LLVM_BUILTIN_UNREACHABLE
  // Windows systems and possibly others don't declare abort() to be noreturn,
  // so use the unreachable builtin to avoid a Clang self-host warning.
  LLVM_BUILTIN_UNREACHABLE;
#endif
}
