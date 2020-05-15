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

#if defined(_MSC_VER)
# include <io.h>
# include <fcntl.h>
#else
# include <stdio.h>
#endif

using namespace llvm;

void __swift::__runtime::llvm::report_fatal_error(const char *Reason,
                                                  bool GenCrashDiag) {
  report_fatal_error(std::string(Reason), GenCrashDiag);
}

void __swift::__runtime::llvm::report_fatal_error(const std::string &Reason,
                                                  bool GenCrashDiag) {
  // Blast the result out to stderr.  We don't try hard to make sure this
  // succeeds (e.g. handling EINTR) and we can't use errs() here because
  // raw ostreams can call report_fatal_error.
  fprintf(stderr, "LLVM ERROR: %s\n", Reason.c_str());
  abort();
}

void __swift::__runtime::llvm::report_fatal_error(StringRef Reason,
                                                  bool GenCrashDiag) {
  report_fatal_error(Reason.str(), GenCrashDiag);
}

void __swift::__runtime::llvm::report_bad_alloc_error(const char *Reason,
                                                      bool GenCrashDiag) {
  // Don't call the normal error handler. It may allocate memory. Directly write
  // an OOM to stderr and abort.
  fprintf(stderr, "LLVM ERROR: out of memory\n");
  abort();
}

void __swift::__runtime::llvm::llvm_unreachable_internal(
    const char *msg, const char *file, unsigned line) {
  // This code intentionally doesn't call the ErrorHandler callback, because
  // llvm_unreachable is intended to be used to indicate "impossible"
  // situations, and not legitimate runtime errors.
  if (msg)
    fprintf(stderr, "%s\n", msg);
  fprintf(stderr, "UNREACHABLE executed");
  if (file)
    fprintf(stderr, " at %s:%u", file, line);
  fprintf(stderr, "!\n");
  abort();
#ifdef LLVM_BUILTIN_UNREACHABLE
  // Windows systems and possibly others don't declare abort() to be noreturn,
  // so use the unreachable builtin to avoid a Clang self-host warning.
  LLVM_BUILTIN_UNREACHABLE;
#endif
}
