//===-- ArgParsingTest.h ----------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef ARG_PARSING_TEST_H
#define ARG_PARSING_TEST_H

#include "swift/Frontend/Frontend.h"
#include "gtest/gtest.h"

using Args = std::vector<std::string>;

class ArgParsingTest : public ::testing::Test {
  swift::CompilerInvocation invocation;
  swift::SourceManager sourceMgr;
  swift::DiagnosticEngine diags;

protected:
  std::optional<std::string> langMode;

public:
  ArgParsingTest();

  void parseArgs(const Args &args);

  swift::LangOptions &getLangOptions();
};

template <typename T>
struct ArgParsingTestCase final {
  Args args;
  T expectedResult;

  ArgParsingTestCase(Args args, T expectedResult)
      : args(std::move(args)), expectedResult(std::move(expectedResult)) {}
};

// MARK: - Printers

void PrintTo(const Args &, std::ostream *);

template <typename T>
void PrintTo(const ArgParsingTestCase<T> &value, std::ostream *os) {
  PrintTo(value.args, os);
}

#endif // ARG_PARSING_TEST_H
