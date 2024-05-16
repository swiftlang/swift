//===--- Assertions.cpp - Swift Version Number -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines custom assertion support functions
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/CommandLine.h"
#include "swift/Basic/Assertions.h"
#undef NDEBUG
#include <cassert>
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
  0; // TODO: Default to `on` in debug builds
#endif

void ASSERT_failure(const char *expr, const char *file, int line, const char *func) {
  // Only print the last component of `file`
  const char *f = file;
  for (const char *p = file; *p != '\0'; p++) {
    if ((p[0] == '/' || p[0] == '\\')
	&& p[1] != '/' && p[1] != '\\' && p[1] != '\0') {
      f = p + 1;
    }
  }

  if (AssertHelp) {
    ASSERT_help();
  } else {
    std::cerr << "Assertion help:  -Xllvm -assert-help" << std::endl;
  }


  // Format here matches that used by `assert` on macOS:
  std::cerr
    << "Assertion failed: "
    << "(" << expr << "), "
    << "function " << func << ", "
    << "file " << f << ", "
    << "line " << line << "."
    << std::endl;

  if (AssertContinue) {
    std::cerr << "Continuing after failed assertion (-Xllvm -assert-continue)" << std::endl;
    return;
  }

  abort();
}

void ASSERT_help() {
  static int ASSERT_help_shown = 0;
  if (ASSERT_help_shown) {
    return;
  }
  ASSERT_help_shown = 1;

  std::cerr << std::endl;
  std::cerr << "Control assertion behavior with one or more of the following options:" << std::endl;
  std::cerr << std::endl;
  std::cerr << " -Xllvm -assert-continue" << std::endl;
  std::cerr << "     Continue after any failed assertion" << std::endl;
  std::cerr << std::endl;
}

// This has to be callable in the same way as the macro version,
// so we can't put it inside a namespace.
#undef CONDITIONAL_ASSERT_enabled
int CONDITIONAL_ASSERT_enabled() {
  return (CONDITIONAL_ASSERT_Global_enable_flag != 0);
}

