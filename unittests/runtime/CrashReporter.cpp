//===--- CrashReporter.cpp - Crash log message tests ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/CrashReporter.h"
#include "gtest/gtest.h"
#include <string>

using namespace swift;

// Build a string of repeated numbered lines at least `size` bytes long.
static std::string linesOfLength(size_t size) {
  std::string result;
  while (result.size() < size)
    result += "0123456789\n";
  return result;
}

TEST(CrashReporterTest, appendToEmpty) {
  char *result = appendToCrashLogMessage(nullptr, "hello\n");
  EXPECT_STREQ("hello\n", result);
  free(result);
}

TEST(CrashReporterTest, appendConcatenates) {
  char *result = appendToCrashLogMessage("first\n", "second\n");
  EXPECT_STREQ("first\nsecond\n", result);
  free(result);
}

TEST(CrashReporterTest, appendKeepsNewMessageWhole) {
  std::string message = linesOfLength(crashLogMessageLimit * 3);
  char *result = appendToCrashLogMessage(linesOfLength(100).c_str(),
                                         message.c_str());
  EXPECT_EQ(message, std::string(result));
  free(result);
}

TEST(CrashReporterTest, appendTruncatesHistory) {
  std::string history = linesOfLength(crashLogMessageLimit * 2);
  char *result = appendToCrashLogMessage(history.c_str(), "newest\n");
  std::string resulting = result;
  free(result);

  EXPECT_LE(resulting.size(), crashLogMessageLimit);
  EXPECT_EQ("newest\n", resulting.substr(resulting.size() - 7));

  // The retained history starts at a line boundary rather than mid-line.
  EXPECT_EQ("0123456789\n", resulting.substr(0, 11));
}

TEST(CrashReporterTest, appendDropsHistoryWhenMessageFillsLimit) {
  std::string history = linesOfLength(crashLogMessageLimit);
  std::string message(crashLogMessageLimit, 'x');
  char *result = appendToCrashLogMessage(history.c_str(), message.c_str());
  EXPECT_EQ(message, std::string(result));
  free(result);
}
