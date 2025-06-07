//===--- Assertions.cpp - Assertion macros --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines implementation details of include/swift/Basic/Assertions.h.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallString.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Assertions.h"
#include <iostream>

llvm::cl::opt<bool> AssertContinue(
    "assert-continue", llvm::cl::init(false),
    llvm::cl::desc("Do not stop on an assertion failure"));

llvm::cl::opt<bool> AssertHelp(
    "assert-help", llvm::cl::init(false),
    llvm::cl::desc("Print help for managing assertions"));

int CONDITIONAL_ASSERT_Global_enable_flag =
#ifdef NDEBUG
  0; // Default to `off` in release builds
#else
  1; // Default to `on` in debug builds
#endif

static void ASSERT_help(llvm::raw_ostream &out) {
  static int ASSERT_help_shown = 0;
  if (ASSERT_help_shown) {
    return;
  }
  ASSERT_help_shown = 1;

  if (!AssertHelp) {
    out << "(to display assertion configuration options: -Xllvm -assert-help)\n";
    return;
  }

  out << "\n";
  out << "Control assertion behavior with one or more of the following options:\n\n";
  out << " -Xllvm -assert-continue\n";
  out << "     Continue after any failed assertion\n\n";
}

[[noreturn]]
static inline void _abortWithMessage(llvm::StringRef message);

void ASSERT_failure(const char *expr, const char *filename, int line, const char *func) {
  // Find the last component of `filename`
  // Needed on Windows MSVC, which lacks __FILE_NAME__
  // so we have to use __FILE__ instead:
  for (const char *p = filename; *p != '\0'; p++) {
    if ((p[0] == '/' || p[0] == '\\')
       && p[1] != '/' && p[1] != '\\' && p[1] != '\0') {
      filename = p + 1;
    }
  }

  llvm::SmallString<0> message;
  llvm::raw_svector_ostream out(message);

  out << "Assertion failed: "
      << "(" << expr << "), "
      << "function " << func << " at "
      << filename << ":"
      << line << ".\n";

  ASSERT_help(out);

  if (AssertContinue) {
    llvm::errs() << message;
    llvm::errs() << "Continuing after failed assertion (-Xllvm -assert-continue)\n";
    return;
  }

  _abortWithMessage(message);
}

// This has to be callable in the same way as the macro version,
// so we can't put it inside a namespace.
#undef CONDITIONAL_ASSERT_enabled
int CONDITIONAL_ASSERT_enabled() {
  return (CONDITIONAL_ASSERT_Global_enable_flag != 0);
}

// MARK: ABORT

namespace {
/// Similar to PrettyStackTraceString, but formats multi-line strings for
/// the stack trace.
class PrettyStackTraceMultilineString : public llvm::PrettyStackTraceEntry {
  llvm::StringRef Str;

public:
  PrettyStackTraceMultilineString(llvm::StringRef str) : Str(str) {}
  void print(llvm::raw_ostream &OS) const override {
    // For each line, add a leading character and indentation to better match
    // the formatting of the stack trace.
    for (auto c : Str.rtrim('\n')) {
      OS << c;
      if (c == '\n')
        OS << "| \t";
    }
    OS << '\n';
  }
};
} // end anonymous namespace

static void _abortWithMessage(llvm::StringRef message) {
  // Use a pretty stack trace to ensure the message gets picked up the
  // crash reporter.
  PrettyStackTraceMultilineString trace(message);

  // Also dump to stderr in case pretty backtracing is disabled, and to
  // allow the message to be seen while attached with a debugger.
  llvm::errs() << message << '\n';

  abort();
}

void _ABORT(const char *file, int line, const char *func,
            llvm::function_ref<void(llvm::raw_ostream &)> message) {
  llvm::SmallString<0> errorStr;
  llvm::raw_svector_ostream out(errorStr);
  out << "Abort: " << "function " << func << " at "
      << file << ":" << line << "\n";
  message(out);

  _abortWithMessage(errorStr);
}

void _ABORT(const char *file, int line, const char *func,
            llvm::StringRef message) {
  _ABORT(file, line, func, [&](auto &out) { out << message; });
}
