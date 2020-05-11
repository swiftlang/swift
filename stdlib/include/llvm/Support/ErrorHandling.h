//===- llvm/Support/ErrorHandling.h - Fatal error handling ------*- C++ -*-===//
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

#ifndef LLVM_SUPPORT_ERRORHANDLING_H
#define LLVM_SUPPORT_ERRORHANDLING_H

#include "llvm/Support/Compiler.h"
#include <string>

inline namespace __swift { inline namespace __runtime {
namespace llvm {
class StringRef;
/// An error handler callback.
typedef void (*fatal_error_handler_t)(void *user_data,
                                      const std::string& reason,
                                      bool gen_crash_diag);

/// Reports a serious error, calling any installed error handler. These
/// functions are intended to be used for error conditions which are outside
/// the control of the compiler (I/O errors, invalid user input, etc.)
///
/// If no error handler is installed the default is to print the message to
/// standard error, followed by a newline.
/// After the error handler is called this function will call abort(), it
/// does not return.
LLVM_ATTRIBUTE_NORETURN void report_fatal_error(const char *reason,
                                                bool gen_crash_diag = true);
LLVM_ATTRIBUTE_NORETURN void report_fatal_error(const std::string &reason,
                                                bool gen_crash_diag = true);
LLVM_ATTRIBUTE_NORETURN void report_fatal_error(StringRef reason,
                                                bool gen_crash_diag = true);

/// Reports a bad alloc error, calling any user defined bad alloc
/// error handler. In contrast to the generic 'report_fatal_error'
/// functions, this function is expected to return, e.g. the user
/// defined error handler throws an exception.
///
/// Note: When throwing an exception in the bad alloc handler, make sure that
/// the following unwind succeeds, e.g. do not trigger additional allocations
/// in the unwind chain.
///
/// If no error handler is installed (default), then a bad_alloc exception
/// is thrown, if LLVM is compiled with exception support, otherwise an
/// assertion is called.
void report_bad_alloc_error(const char *Reason, bool GenCrashDiag = true);

/// This function calls abort(), and prints the optional message to stderr.
/// Use the llvm_unreachable macro (that adds location info), instead of
/// calling this function directly.
LLVM_ATTRIBUTE_NORETURN void
llvm_unreachable_internal(const char *msg = nullptr, const char *file = nullptr,
                          unsigned line = 0);
}
}} // namespace swift::runtime

/// Marks that the current location is not supposed to be reachable.
/// In !NDEBUG builds, prints the message and location info to stderr.
/// In NDEBUG builds, becomes an optimizer hint that the current location
/// is not supposed to be reachable.  On compilers that don't support
/// such hints, prints a reduced message instead.
///
/// Use this instead of assert(0).  It conveys intent more clearly and
/// allows compilers to omit some unnecessary code.
#ifndef NDEBUG
#define llvm_unreachable(msg) \
  __swift::__runtime::llvm::llvm_unreachable_internal(msg, __FILE__, __LINE__)
#elif defined(LLVM_BUILTIN_UNREACHABLE)
#define llvm_unreachable(msg) LLVM_BUILTIN_UNREACHABLE
#else
#define llvm_unreachable(msg) __swift::__runtime::llvm::llvm_unreachable_internal()
#endif

#endif
